// Tests for choosing the stone-lsp executable. Run with `npm test`.

const assert = require("node:assert/strict");
const path = require("node:path");
const { test } = require("node:test");
const { serverCommand } = require("../server");

const extension = path.join("ext", "stone");
const bundled = path.join(extension, "server", process.platform === "win32" ? "stone-lsp.exe" : "stone-lsp");

test("the stone.server.path setting wins over a bundled server", () => {
  assert.equal(serverCommand("/opt/stone-lsp", extension, () => true), "/opt/stone-lsp");
});

test("a bundled server is used when the setting is empty", () => {
  assert.equal(serverCommand("", extension, (file) => file === bundled), bundled);
});

test("falls back to stone-lsp on PATH without a bundled server", () => {
  assert.equal(serverCommand("", extension, () => false), "stone-lsp");
  assert.equal(serverCommand(undefined, extension, () => false), "stone-lsp");
});
