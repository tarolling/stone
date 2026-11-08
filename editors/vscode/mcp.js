// Offers stone-lsp's MCP server to VS Code's agents, such as Copilot in agent mode.

/**
 * Registers `<command> --mcp` as the "stone" MCP server if this VS Code can offer MCP servers
 * (1.101 and later), returning the registration to dispose of, or `undefined` if it cannot.
 *
 * The server is started in the first workspace folder, so tools resolve relative paths from
 * there. `command` is called each time VS Code asks, so a changed `stone.server.path` setting
 * applies the next time the server starts. For example, with `command` returning `stone-lsp`,
 * VS Code runs `stone-lsp --mcp`.
 *
 * @param {typeof import("vscode")} vscode the VS Code API, or a stand-in for testing
 * @param {() => string} command returns the stone-lsp executable to run
 * @param {string} version the extension's version, which tells VS Code when the server changed
 * @returns {{ dispose(): void } | undefined}
 */
function registerMcpServer(vscode, command, version) {
  if (typeof vscode.lm?.registerMcpServerDefinitionProvider !== "function") {
    return undefined;
  }
  return vscode.lm.registerMcpServerDefinitionProvider("stone", {
    provideMcpServerDefinitions() {
      const server = new vscode.McpStdioServerDefinition("stone", command(), ["--mcp"], {}, version);
      const folder = vscode.workspace.workspaceFolders?.[0];
      if (folder) {
        server.cwd = folder.uri;
      }
      return [server];
    },
  });
}

module.exports = { registerMcpServer };
