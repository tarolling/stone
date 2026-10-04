// Chooses which stone-lsp executable the extension runs.

const fs = require("node:fs");
const path = require("node:path");

/**
 * Returns the stone-lsp command to run.
 *
 * A non-empty `stone.server.path` setting always wins. Otherwise the server bundled in the
 * extension's `server/` directory is used if the platform-specific package shipped one, and
 * failing that, `stone-lsp` on PATH. For example, `serverCommand("", "/ext")` returns
 * `/ext/server/stone-lsp` when that file exists and `stone-lsp` when it does not.
 *
 * @param {string | undefined} configured the `stone.server.path` setting
 * @param {string} extensionPath the directory the extension is installed in
 * @param {(file: string) => boolean} exists checks whether a file exists
 * @returns {string}
 */
function serverCommand(configured, extensionPath, exists = fs.existsSync) {
  if (configured) {
    return configured;
  }
  const name = process.platform === "win32" ? "stone-lsp.exe" : "stone-lsp";
  const bundled = path.join(extensionPath, "server", name);
  return exists(bundled) ? bundled : "stone-lsp";
}

module.exports = { serverCommand };
