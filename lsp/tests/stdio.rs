//! Runs the `stone-lsp` executable and talks to it over stdio, as an editor does.

use std::io::{BufRead, BufReader, Write};
use std::process::{Command, Stdio};

use lsp_server::{Message, Notification, Request, RequestId};
use serde_json::json;

#[test]
fn serves_diagnostics_over_stdio_and_exits_cleanly() {
    let mut child = Command::new(env!("CARGO_BIN_EXE_stone-lsp"))
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("stone-lsp should start");
    let mut stdin = child.stdin.take().unwrap();
    let mut stdout = BufReader::new(child.stdout.take().unwrap());
    let mut send = |message: Message| message.write(&mut stdin).unwrap();
    let mut receive = || {
        Message::read(&mut stdout)
            .unwrap()
            .expect("server should reply")
    };

    send(
        Request::new(
            RequestId::from(1),
            "initialize".to_string(),
            json!({ "capabilities": {} }),
        )
        .into(),
    );
    let Message::Response(initialized) = receive() else {
        panic!("expected the initialize response");
    };
    assert_eq!(
        initialized.response_result.unwrap()["serverInfo"]["name"],
        "stone-lsp"
    );
    send(Notification::new("initialized".to_string(), json!({})).into());

    send(
        Notification::new(
            "textDocument/didOpen".to_string(),
            json!({
                "textDocument": {
                    "uri": "file:///bad.stn",
                    "languageId": "stone",
                    "version": 1,
                    "text": "if 1\n    x = 1\n"
                }
            }),
        )
        .into(),
    );
    let Message::Notification(published) = receive() else {
        panic!("expected diagnostics");
    };
    assert_eq!(
        published.params["diagnostics"][0]["message"],
        "expected ':', found end of line"
    );

    send(Request::new(RequestId::from(2), "shutdown".to_string(), json!(null)).into());
    assert!(matches!(receive(), Message::Response(_)));
    send(Notification::new("exit".to_string(), json!(null)).into());
    drop(stdin);
    assert!(child.wait().unwrap().success());
}

#[test]
fn serves_mcp_tools_over_stdio_with_the_mcp_flag() {
    let dir = std::env::temp_dir().join(format!("stone-lsp-mcp-stdio-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(
        dir.join("main.stn"),
        "fn add(a: int, b: int) -> int:\n    ret a + b\nprint(add(1, 2))\n",
    )
    .unwrap();
    let mut child = Command::new(env!("CARGO_BIN_EXE_stone-lsp"))
        .arg("--mcp")
        .current_dir(&dir)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("stone-lsp --mcp should start");
    let mut stdin = child.stdin.take().unwrap();
    let mut stdout = BufReader::new(child.stdout.take().unwrap());
    let mut ask = |message: serde_json::Value| -> serde_json::Value {
        writeln!(stdin, "{message}").unwrap();
        let mut line = String::new();
        stdout.read_line(&mut line).unwrap();
        serde_json::from_str(&line).unwrap()
    };

    let initialized = ask(json!({
        "jsonrpc": "2.0", "id": 1, "method": "initialize",
        "params": { "protocolVersion": "2025-06-18", "capabilities": {} },
    }));
    assert_eq!(initialized["result"]["serverInfo"]["name"], "stone");
    let hover = ask(json!({
        "jsonrpc": "2.0", "id": 2, "method": "tools/call",
        "params": {
            "name": "stone_hover",
            "arguments": { "path": "main.stn", "line": 3, "symbol": "add" },
        },
    }));
    let text = hover["result"]["content"][0]["text"].as_str().unwrap();
    assert!(text.contains("fn add(a: int, b: int) -> int"), "{text}");

    drop(ask);
    drop(stdin);
    assert!(child.wait().unwrap().success());
    let _ = std::fs::remove_dir_all(&dir);
}
