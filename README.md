# jot

A parser for [Djot][djot], a markdown-like language.

[djot]: https://djot.net/

[![Package Version](https://img.shields.io/hexpm/v/jot)](https://hex.pm/packages/jot)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/jot/)

```sh
gleam add jot
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

- [x] Headings
- [x] Paragraphs
- [x] Links (inline and reference)
- [x] Images
- [x] Code blocks
- [x] Inline code
- [x] Emphasis and strong
- [x] Block attributes
- [x] Manual line breaks
