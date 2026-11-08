// Starts stone-lsp for .stn files and connects it to VS Code.

const vscode = require("vscode");
const { LanguageClient } = require("vscode-languageclient/node");
const { registerMcpServer } = require("./mcp");
const { serverCommand } = require("./server");

/** @type {LanguageClient | undefined} */
let client;

/**
 * Starts the language server, which VS Code does when a stone file is first opened.
 *
 * The server runs as a child process speaking the Language Server Protocol over stdio. It is the
 * `stone.server.path` setting if set, else the server bundled with the extension, else
 * `stone-lsp` on PATH (see `serverCommand`). The same executable is offered to VS Code's agents as
 * an MCP server, `stone-lsp --mcp` (see `registerMcpServer`).
 *
 * @param {vscode.ExtensionContext} context
 */
async function activate(context) {
  const command = () => {
    const configured = vscode.workspace.getConfiguration("stone").get("server.path", "");
    return serverCommand(configured, context.extensionPath);
  };
  // agents in VS Code 1.101 and later can call the same server's tools over MCP
  const mcp = registerMcpServer(vscode, command, context.extension.packageJSON.version);
  if (mcp) {
    context.subscriptions.push(mcp);
  }
  client = new LanguageClient(
    "stone",
    "stone",
    { command: command() },
    { documentSelector: [{ language: "stone" }] },
  );
  await client.start();
}

/** Stops the language server when VS Code shuts the extension down. */
function deactivate() {
  return client?.stop();
}

module.exports = { activate, deactivate };
