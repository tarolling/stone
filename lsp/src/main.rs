//! The `stone-lsp` executable, which serves the Language Server Protocol over stdin and stdout.
//!
//! Editors start it as a child process, for example through the VS Code extension in
//! `editors/vscode`.

use lsp_server::Connection;

fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let (connection, io_threads) = Connection::stdio();
    stone_lsp::run(connection)?;
    io_threads.join()?;
    Ok(())
}
