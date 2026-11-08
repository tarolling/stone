//! A Model Context Protocol server, which gives coding agents what the language server knows.
//!
//! `stone-lsp --mcp` runs [`serve`] over stdin and stdout: newline-delimited JSON-RPC 2.0, as
//! MCP's stdio transport defines it, answering `initialize`, `ping`, `tools/list`, and
//! `tools/call`. The tools (see [`tools`]) read programs from disk, analyze them exactly as the
//! language server analyzes an open document, and answer in text: positions as `file:line:col`
//! with the source line, diagnostics as `stone check` writes them.
//!
//! For example, the line `{"jsonrpc":"2.0","id":1,"method":"ping"}` gets the reply
//! `{"id":1,"jsonrpc":"2.0","result":{}}`.

use std::io::{self, BufRead, Write};
use std::path::PathBuf;
use std::time::Duration;

use serde_json::{Value, json};

use crate::features::Builtins;

pub mod tools;

#[cfg(test)]
mod tests;

/// The newest protocol version the server speaks, which it offers to a client that asks for one
/// it does not know.
pub const PROTOCOL_VERSION: &str = "2025-06-18";

/// Every protocol version the server can answer as, since the tools it uses are the same in each.
const PROTOCOL_VERSIONS: [&str; 3] = [PROTOCOL_VERSION, "2025-03-26", "2024-11-05"];

/// What the server tells an agent about itself when it connects.
const INSTRUCTIONS: &str = "\
Tools for stone, a small Python-like language with Rust-like semantics (files end in .stn). \
Before writing or changing stone code, call stone_guide once to learn the language. After every \
edit, call stone_diagnostics on the file or directory. Use stone_hover, stone_definition, and \
stone_references to understand code, stone_rename to rename a symbol across every file, \
stone_docs to look up a builtin, and stone_check_snippet or stone_run_snippet to try code that \
is not in a file. Positions are a 1-based line plus either the symbol's text on that line or a \
1-based column.";

const PARSE_ERROR: i64 = -32700;
const INVALID_REQUEST: i64 = -32600;
const METHOD_NOT_FOUND: i64 = -32601;
const INVALID_PARAMS: i64 = -32602;

/// What every tool call needs: the directory relative paths start from, where the builtins
/// reference was written, if it could be, and how much a snippet may run.
pub struct Context {
    pub cwd: PathBuf,
    pub builtins: Option<Builtins>,
    /// Loop turns and calls a snippet may take, [`tools::SNIPPET_FUEL`] by default.
    pub fuel: u64,
    /// How long to wait for a snippet, [`tools::SNIPPET_TIMEOUT`] by default.
    pub timeout: Duration,
}

impl Context {
    /// Makes a context for paths relative to `cwd`, writing the builtins reference so that a
    /// builtin's definition has a file to point at.
    pub fn new(cwd: PathBuf) -> Self {
        Context {
            cwd: crate::uri::plain(&cwd.canonicalize().unwrap_or(cwd)),
            builtins: crate::write_builtins_reference().map(Builtins::new),
            fuel: tools::SNIPPET_FUEL,
            timeout: tools::SNIPPET_TIMEOUT,
        }
    }
}

/// Serves MCP over `reader` and `writer` until the reader ends, from the process's working
/// directory.
pub fn serve(reader: impl BufRead, writer: impl Write) -> io::Result<()> {
    let cwd = std::env::current_dir()?;
    serve_in(&Context::new(cwd), reader, writer)
}

/// Serves MCP over `reader` and `writer` with `context`, writing one reply per line for each
/// request and none for a notification.
pub fn serve_in(context: &Context, reader: impl BufRead, mut writer: impl Write) -> io::Result<()> {
    for line in reader.lines() {
        let line = line?;
        if line.trim().is_empty() {
            continue;
        }
        let reply = match serde_json::from_str::<Value>(&line) {
            Ok(message) => answer(context, &message),
            Err(error) => Some(failure(&Value::Null, PARSE_ERROR, &error.to_string())),
        };
        if let Some(reply) = reply {
            writeln!(writer, "{reply}")?;
            writer.flush()?;
        }
    }
    Ok(())
}

/// Returns the reply to one message, or `None` for a notification or a response, which get none.
fn answer(context: &Context, message: &Value) -> Option<Value> {
    let Some(method) = message.get("method").and_then(Value::as_str) else {
        // a response to something the server never asks, or not a message at all
        return match message.get("id") {
            Some(_) if message.get("result").is_some() || message.get("error").is_some() => None,
            _ => Some(failure(
                &Value::Null,
                INVALID_REQUEST,
                "not a JSON-RPC request",
            )),
        };
    };
    let id = message.get("id")?;
    let params = message.get("params").cloned().unwrap_or(json!({}));
    let result = match method {
        "initialize" => Ok(initialize(&params)),
        "ping" => Ok(json!({})),
        "tools/list" => Ok(json!({ "tools": tools::list() })),
        "tools/call" => call(context, &params),
        _ => Err((METHOD_NOT_FOUND, format!("unknown method '{method}'"))),
    };
    Some(match result {
        Ok(result) => json!({ "jsonrpc": "2.0", "id": id, "result": result }),
        Err((code, message)) => failure(id, code, &message),
    })
}

/// Answers `initialize`, with the client's protocol version if the server speaks it.
fn initialize(params: &Value) -> Value {
    let asked = params.get("protocolVersion").and_then(Value::as_str);
    let version = asked
        .filter(|v| PROTOCOL_VERSIONS.contains(v))
        .unwrap_or(PROTOCOL_VERSION);
    json!({
        "protocolVersion": version,
        "capabilities": { "tools": { "listChanged": false } },
        "serverInfo": {
            "name": "stone",
            "title": "stone",
            "version": env!("CARGO_PKG_VERSION"),
        },
        "instructions": INSTRUCTIONS,
    })
}

/// Answers `tools/call`: an unknown tool is a protocol error, and a tool that fails, even for a
/// bad argument, is a result marked `isError`, which the agent sees and can correct.
fn call(context: &Context, params: &Value) -> Result<Value, (i64, String)> {
    let name = params
        .get("name")
        .and_then(Value::as_str)
        .ok_or((INVALID_PARAMS, "tools/call needs a 'name'".to_string()))?;
    let arguments = params.get("arguments").cloned().unwrap_or(json!({}));
    let result = tools::call(name, &arguments, context)
        .ok_or_else(|| (INVALID_PARAMS, format!("unknown tool '{name}'")))?;
    Ok(json!({
        "content": [{ "type": "text", "text": result.text }],
        "isError": result.is_error,
    }))
}

fn failure(id: &Value, code: i64, message: &str) -> Value {
    json!({ "jsonrpc": "2.0", "id": id, "error": { "code": code, "message": message } })
}
