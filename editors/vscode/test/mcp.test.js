// Tests for offering the MCP server to VS Code. Run with `npm test`.

const assert = require("node:assert/strict");
const { test } = require("node:test");
const { registerMcpServer } = require("../mcp");

/** Returns a stand-in for the parts of the VS Code API that `registerMcpServer` uses. */
function fakeVscode(folders) {
  const registered = [];
  class McpStdioServerDefinition {
    constructor(label, command, args, env, version) {
      Object.assign(this, { label, command, args, env, version });
    }
  }
  return {
    registered,
    McpStdioServerDefinition,
    workspace: { workspaceFolders: folders },
    lm: {
      registerMcpServerDefinitionProvider(id, provider) {
        registered.push({ id, provider });
        return { dispose() {} };
      },
    },
  };
}

test("registers stone-lsp --mcp, started in the first workspace folder", () => {
  const vscode = fakeVscode([{ uri: "file:///work" }, { uri: "file:///other" }]);
  assert.ok(registerMcpServer(vscode, () => "/bin/stone-lsp", "1.2.3"));
  assert.equal(vscode.registered.length, 1);
  assert.equal(vscode.registered[0].id, "stone");
  const [server] = vscode.registered[0].provider.provideMcpServerDefinitions();
  assert.equal(server.label, "stone");
  assert.equal(server.command, "/bin/stone-lsp");
  assert.deepEqual(server.args, ["--mcp"]);
  assert.equal(server.version, "1.2.3");
  assert.equal(server.cwd, "file:///work");
});

test("asks for the command each time, so a changed setting applies", () => {
  const vscode = fakeVscode(undefined);
  let command = "stone-lsp";
  registerMcpServer(vscode, () => command, "1.2.3");
  command = "/opt/stone-lsp";
  const [server] = vscode.registered[0].provider.provideMcpServerDefinitions();
  assert.equal(server.command, "/opt/stone-lsp");
  assert.equal(server.cwd, undefined);
});

test("does nothing on a VS Code without MCP servers", () => {
  assert.equal(registerMcpServer({ lm: {} }, () => "stone-lsp", "1.2.3"), undefined);
  assert.equal(registerMcpServer({}, () => "stone-lsp", "1.2.3"), undefined);
});
