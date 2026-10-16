<!--
  Please check the needed checkboxes ([ ] -> [x]).
  Leave the comments as they are: they do not show on GitHub.

  Please keep each pull request to one change,
  provide a general description of the changes,
  and remember it's up to you to convince us to land it.

  Security fixes: report the vulnerability privately first, as SECURITY.md describes.

  We are excited about pull requests.
  Thank you!
-->

# Pull Request

## Initial Checklist

* [ ] I read the [contributing guide](https://github.com/tarolling/stone/blob/main/CONTRIBUTING.md) (and [the philosophy](https://github.com/tarolling/stone/blob/main/docs/philosophy.md), for a language change)
* [ ] I agree to follow the [Code of Conduct](https://github.com/tarolling/stone/blob/main/CODE_OF_CONDUCT.md)
* [ ] I searched issues and couldn't find anything or linked relevant results below
* [ ] I wrote the tests first and saw them fail, including a golden program in `tests/programs/` for new language behavior (or tests are not needed)
* [ ] `stone run` and `stone build` agree, and `SKIPS` in `tests/programs.rs` is still empty
* [ ] `cargo fmt`, `cargo clippy --all-targets -- -D warnings`, and `cargo test` pass
* [ ] I added an entry under `## [Unreleased]` in `CHANGELOG.md` (or `editors/vscode/CHANGELOG.md`)
* [ ] I updated the docs, and `docs/grammar/` for a syntax change (or that's not needed)
* [ ] I followed the [style guide](https://github.com/tarolling/stone/blob/main/docs/development/style.md)

## Related Issue(s)

<!-- such as "Closes #12", or "none" -->

TODO

## Description of Changes

<!-- what changes, and why -->

TODO

## Testing

<!-- how you tested it: the tests you added, and the targets you ran them on -->

TODO
