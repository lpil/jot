# jot

A parser for [Djot][djot], a markdown-like language.

[djot]: https://djot.net/

[![Package Version](https://img.shields.io/hexpm/v/jot)](https://hex.pm/packages/jot)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/jot/)

```sh
gleam add jot@8
```

```gleam
import jot

const document = "
# Hello, Joe!

This is a [Djot][djot] document.

[djot]: https://www.djot.net/
"

pub fn main() {
  // Turn the document into HTML
  let html = jot.to_html(document)

  // Alternatively, parse the document to an AST
  let ast = jot.parse(document)
}
```

Further documentation can be found at <https://hexdocs.pm/jot>.

## Status

This project is a work in progress. So far it supports:

- [x] Autolinks (email and URL)
- [x] Block attributes
- [x] Block quotes
- [x] Code blocks
- [x] Content escaping
- [x] Div
- [x] Emphasis and strong
- [x] Footnotes
- [x] Headings
- [x] Images (with attributes support)
- [x] Inline code
- [x] Inserts, deletes, and marks.
- [x] Links (inline and reference, with attributes support)
- [x] Manual line breaks
- [x] Maths (inline and display)
- [x] Non-breaking spaces
- [x] Ordered lists with the `1. 2. 3.` syntax
- [x] Paragraphs
- [x] Raw blocks
- [x] Smart replacing of `...` with ellipsis
- [x] Smart replacing of hyphens with dashes
- [x] Span with attributes
- [x] Thematic breaks
- [x] Unordered lists
