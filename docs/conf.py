"""Sphinx configuration for the stone documentation site."""

import re
import sys
from pathlib import Path

DOCS = Path(__file__).parent
sys.path.insert(0, str(DOCS / "_ext"))


def cargo_version() -> str:
    """Returns the version in the root Cargo.toml, such as "0.1.0", so the docs never drift."""
    manifest = (DOCS.parent / "Cargo.toml").read_text()
    return re.search(r'^version = "(.+)"$', manifest, re.MULTILINE).group(1)


project = "stone"
author = "tarolling"
copyright = "tarolling"
release = cargo_version()
version = release

extensions = [
    "myst_parser",
    "sphinx_copybutton",
    "stone_lexer",
]
myst_enable_extensions = ["colon_fence", "deflist"]
myst_heading_anchors = 3
source_suffix = {".md": "markdown"}
exclude_patterns = ["_build", "grammar", "examples", "ARCHITECTURE.md", "requirements.txt"]
highlight_language = "stone"

html_theme = "furo"
html_title = f"stone {release}"
html_logo = "_static/logo.svg"
html_favicon = "_static/logo.svg"
html_theme_options = {
    "source_repository": "https://github.com/tarolling/stone/",
    "source_branch": "main",
    "source_directory": "docs/",
    "footer_icons": [
        {
            "name": "GitHub",
            "url": "https://github.com/tarolling/stone",
            "html": (
                '<svg stroke="currentColor" fill="currentColor" stroke-width="0" '
                'viewBox="0 0 16 16"><path fill-rule="evenodd" d="M8 0C3.58 0 0 3.58 0 8c0 '
                "3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53"
                "-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 "
                "1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 "
                "0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 "
                "2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 "
                "1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 "
                '2.2 0 .21.15.46.55.38A8.013 8.013 0 0016 8c0-4.42-3.58-8-8-8z"></path></svg>'
            ),
            "class": "",
        },
    ],
}
