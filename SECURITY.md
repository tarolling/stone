# Security policy

## Supported versions

stone is before 1.0, so only the latest release gets security fixes. A fix ships in a new
release rather than as a patch to an older one, and `stone update` installs it.

| Component | Supported |
| --- | --- |
| `stone`, latest release | Yes |
| `stone`, older releases | No |
| VS Code extension and `stone-lsp`, latest release | Yes |
| VS Code extension and `stone-lsp`, older releases | No |

## What counts as a vulnerability

stone promises memory safety, so these are security issues, not only bugs:

- A program the checker accepts that, once compiled with `stone build`, reads or writes memory it
  should not, such as an out-of-bounds access that is not caught, a use after free, or a double
  free.
- Input to `stone run`, `stone build`, `stone check`, or the language server that makes them run
  code or touch files they should not, beyond the program itself and its build output.
- Weaknesses in how stone is installed or updated: `install.sh`, `stone update`, or the release
  archives and their checksums.
- `stone clean` or `stone uninstall` deleting anything other than what they document.

A crash, panic, or wrong output with no security impact is an ordinary bug. Please report it as a
[GitHub issue](https://github.com/tarolling/stone/issues).

## Reporting a vulnerability

Please do not report vulnerabilities in public issues, pull requests, or discussions.

Report them privately through
[GitHub's private vulnerability reporting](https://github.com/tarolling/stone/security/advisories/new).
Include:

- the stone version (`stone --version`) and the target (such as `x86_64-linux` or
  `aarch64-macos`),
- a program or steps that reproduce the problem, and
- what an attacker could do with it.

## What to expect

stone is maintained by one person in their spare time, so these are goals, not guarantees:

- An acknowledgment within 7 days.
- An update at least every 14 days while the report is being investigated.
- If the report is accepted, a fixed release, a GitHub security advisory crediting you (unless
  you would rather not be named), and an entry under `### Fixed` in the changelog.
- If the report is declined, an explanation of why. If it turns out to be an ordinary bug, it may
  be moved to a public issue with your agreement.

Please keep the details private until a fixed release is out, or until 90 days have passed,
whichever comes first.
