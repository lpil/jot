import gleam/map.{type Map}
import gleam/int
import gleam/list
import gleam/string

pub type Document {
  Document(content: List(Container), references: Map(String, String))
}

pub type Container {
  Paragraph(List(Inline))
  Heading(level: Int, attributes: Attrs, content: List(Inline))
}

pub type Inline {
  Text(String)
}

type Attrs =
  List(#(String, String))

type Chars =
  List(String)

type Refs =
  Map(String, String)

// TODO: document
pub fn to_html(djot: String) -> String {
  djot
  |> parse
  |> document_to_html
}

// TODO: document
pub fn parse(djot: String) -> Document {
  djot
  |> string.replace("\r\n", "\n")
  |> string.to_graphemes
  |> parse_document(map.new(), [])
}

fn drop_lines(in: Chars) -> Chars {
  case in {
    [] -> []
    ["\n", ..rest] -> drop_lines(rest)
    [c, ..rest] -> [c, ..rest]
  }
}

fn drop_spaces(in: Chars) -> Chars {
  case in {
    [] -> []
    [" ", ..rest] -> drop_spaces(rest)
    [c, ..rest] -> [c, ..rest]
  }
}

fn parse_document(in: Chars, refs: Refs, ast: List(Container)) -> Document {
  let in = drop_lines(in)
  case in {
    [] -> Document(list.reverse(ast), refs)
    ["#", ..rest] -> {
      let #(heading, refs, in) = parse_heading(rest, refs)
      parse_document(in, refs, [heading, ..ast])
    }
    [c, ..rest] -> {
      let #(paragraph, in) = parse_paragraph(rest, c)
      parse_document(in, refs, [paragraph, ..ast])
    }
  }
}

fn parse_heading(in: Chars, refs: Refs) -> #(Container, Refs, Chars) {
  let #(level, in) = heading_level(in, 1)
  let in = drop_spaces(in)
  let #(inline_in, in) = take_heading_chars(in, [])
  let inline = parse_inline(inline_in, "", [])
  let text = take_inline_text(inline, "")
  let id = text
  let refs = map.insert(refs, id, "#" <> id)
  let heading = Heading(level, [#("id", id)], inline)
  #(heading, refs, in)
}

fn take_heading_chars(in: Chars, acc: Chars) -> #(Chars, Chars) {
  case in {
    [] | ["\n"] -> #(list.reverse(acc), [])
    ["\n", "\n", ..rest] -> #(list.reverse(acc), rest)
    [c, ..rest] -> take_heading_chars(rest, [c, ..acc])
  }
}

fn parse_inline(in: Chars, text: String, acc: List(Inline)) -> List(Inline) {
  case in {
    [] if text == "" -> list.reverse(acc)
    [] -> parse_inline([], "", [Text(text), ..acc])
    ["\n", ..rest] -> parse_inline(rest, text, acc)
    [c, ..rest] -> parse_inline(rest, text <> c, acc)
  }
}

fn heading_level(in: Chars, level: Int) -> #(Int, Chars) {
  case in {
    ["#", ..rest] -> heading_level(rest, level + 1)
    ["\n", ..rest] -> #(level, rest)
    [c, ..rest] -> #(level, [c, ..rest])
    [] -> #(level, [])
  }
}

fn take_inline_text(inlines: List(Inline), acc: String) -> String {
  case inlines {
    [] -> acc
    [Text(text), ..rest] -> take_inline_text(rest, acc <> text)
    [_, ..rest] -> take_inline_text(rest, acc)
  }
}

fn parse_paragraph(in: Chars, acc: String) -> #(Container, Chars) {
  case in {
    [] | ["\n"] -> {
      let paragraph = Paragraph([Text(string.trim_right(acc))])
      #(paragraph, [])
    }
    ["\n", "\n", ..rest] -> {
      let paragraph = Paragraph([Text(string.trim_right(acc))])
      #(paragraph, rest)
    }
    [c, ..rest] -> parse_paragraph(rest, acc <> c)
  }
}

// TODO: document
pub fn document_to_html(document: Document) -> String {
  containers_to_html(document.content, document.references, "")
}

fn containers_to_html(
  containers: List(Container),
  refs: Refs,
  html: String,
) -> String {
  case containers {
    [] -> html
    [container, ..rest] -> {
      let html = container_to_html(html, container, refs)
      containers_to_html(rest, refs, html)
    }
  }
}

fn container_to_html(html: String, container: Container, refs: Refs) -> String {
  case container {
    Paragraph(inlines) -> {
      html
      |> open_tag("p", [])
      |> inlines_to_html(inlines, refs)
      |> close_tag("p")
    }

    Heading(level, attributes, inlines) -> {
      let tag = "h" <> int.to_string(level)
      html
      |> open_tag(tag, attributes)
      |> inlines_to_html(inlines, refs)
      |> close_tag(tag)
    }
  } <> "\n"
}

fn open_tag(html: String, tag: String, attributes: Attrs) -> String {
  let html = html <> "<" <> tag
  attributes_to_html(html, attributes) <> ">"
}

fn close_tag(html: String, tag: String) -> String {
  html <> "</" <> tag <> ">"
}

fn inlines_to_html(html: String, inlines: List(Inline), refs: Refs) -> String {
  case inlines {
    [] -> html
    [inline, ..rest] -> {
      html
      |> inline_to_html(inline, refs)
      |> inlines_to_html(rest, refs)
    }
  }
}

fn inline_to_html(html: String, inline: Inline, _refs: Refs) -> String {
  case inline {
    Text(text) -> html <> text
  }
}

fn attributes_to_html(html: String, attributes: Attrs) -> String {
  case attributes {
    [] -> html
    [#(key, value), ..rest] -> {
      let html = html <> " " <> key <> "=\"" <> value <> "\""
      attributes_to_html(html, rest)
    }
  }
}
