// Starts stone-lsp for .st files and connects it to VS Code.

const vscode = require("vscode");
const { LanguageClient } = require("vscode-languageclient/node");
const { serverCommand } = require("./server");

/** @type {LanguageClient | undefined} */
let client;

/**
 * Starts the language server, which VS Code does when a stone file is first opened.
 *
 * The server runs as a child process speaking the Language Server Protocol over stdio. It is the
 * `stone.server.path` setting if set, else the server bundled with the extension, else
 * `stone-lsp` on PATH (see `serverCommand`).
 *
 * @param {vscode.ExtensionContext} context
 */
async function activate(context) {
  const configured = vscode.workspace.getConfiguration("stone").get("server.path", "");
  const command = serverCommand(configured, context.extensionPath);
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
