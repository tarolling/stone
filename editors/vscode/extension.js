// Starts stone-lsp for .st files and connects it to VS Code.

const vscode = require("vscode");
const { LanguageClient } = require("vscode-languageclient/node");

/** @type {LanguageClient | undefined} */
let client;

/**
 * Starts the language server, which VS Code does when a stone file is first opened.
 *
 * The server runs as a child process speaking the Language Server Protocol over stdio. Its path
 * comes from the `stone.server.path` setting, which defaults to `stone-lsp` on PATH.
 */
async function activate() {
  const command = vscode.workspace
    .getConfiguration("stone")
    .get("server.path", "stone-lsp");
  client = new LanguageClient(
    "stone",
    "stone",
    { command },
    { documentSelector: [{ language: "stone" }] },
  );
  await client.start();
}

/** Stops the language server when VS Code shuts the extension down. */
function deactivate() {
  return client?.stop();
}

module.exports = { activate, deactivate };
