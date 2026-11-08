//! The `stone-lsp` executable, which serves the Language Server Protocol over stdin and stdout,
//! or with `--mcp`, the Model Context Protocol for coding agents.
//!
//! Editors start it as a child process, for example through the VS Code extension in
//! `editors/vscode`, and agents such as Claude Code start `stone-lsp --mcp` (see `stone_lsp::mcp`).

use lsp_server::Connection;

const USAGE: &str = "\
usage: stone-lsp [--mcp]

Serves the Language Server Protocol for stone over stdin and stdout, as editors start it.

  --mcp      serve the Model Context Protocol instead, for coding agents
  --version  print the version
  --help     print this help
";

fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let args: Vec<String> = std::env::args().skip(1).collect();
    match args
        .iter()
        .map(String::as_str)
        .collect::<Vec<_>>()
        .as_slice()
    {
        [] => {}
        ["--mcp"] => {
            stone_lsp::mcp::serve(std::io::stdin().lock(), std::io::stdout().lock())?;
            return Ok(());
        }
        ["--version"] => {
            println!("stone-lsp {}", env!("CARGO_PKG_VERSION"));
            return Ok(());
        }
        ["--help"] | ["-h"] => {
            print!("{USAGE}");
            return Ok(());
        }
        _ => {
            eprint!("{USAGE}");
            std::process::exit(2);
        }
    }
    let (connection, io_threads) = Connection::stdio();
    stone_lsp::run(connection)?;
    io_threads.join()?;
    Ok(())
}
