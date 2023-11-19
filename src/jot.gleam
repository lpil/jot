import gleam/map.{type Map}
import gleam/int
import gleam/list
import gleam/string
import gleam/option.{type Option, None, Some}

pub type Document {
  Document(content: List(Container), references: Map(String, String))
}

pub type Container {
  Paragraph(List(Inline))
  Heading(level: Int, attributes: Attrs, content: List(Inline))
}

pub type Inline {
  Text(String)
  Link(content: List(Inline), destination: Destination)
}

pub type Destination {
  Reference(String)
  Url(String)
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
  let in = drop_spaces(in)
  case in {
    [] -> Document(list.reverse(ast), refs)
    ["#", ..rest] -> {
      let #(heading, refs, in) = parse_heading(rest, refs)
      parse_document(in, refs, [heading, ..ast])
    }
    _ -> {
      let #(paragraph, in) = parse_paragraph(in)
      parse_document(in, refs, [paragraph, ..ast])
    }
  }
}

fn parse_heading(in: Chars, refs: Refs) -> #(Container, Refs, Chars) {
  case heading_level(in, 1) {
    Some(#(level, in)) -> {
      let in = drop_spaces(in)
      let #(inline_in, in) = take_heading_chars(in, level, [])
      let inline = parse_inline(inline_in, "", [])
      let text = take_inline_text(inline, "")
      let #(refs, attrs) = case id_sanitise(text) {
        "" -> #(refs, [])
        id -> #(map.insert(refs, id, "#" <> id), [#("id", id)])
      }
      let heading = Heading(level, attrs, inline)
      #(heading, refs, in)
    }

    None -> {
      let #(p, in) = parse_paragraph(["#", ..in])
      #(p, refs, in)
    }
  }
}

fn id_sanitise(content: String) -> String {
  content
  |> string.to_graphemes
  |> list.filter(id_char)
  |> id_escape("")
}

fn id_char(char: String) -> Bool {
  case char {
    "#" | "?" | "!" -> False
    _ -> True
  }
}

fn id_escape(content: Chars, acc: String) -> String {
  case content {
    [] -> acc

    [" ", ..rest] | ["\n", ..rest] if rest == [] -> acc
    [" ", ..rest] | ["\n", ..rest] if acc == "" -> id_escape(rest, acc)

    [" ", ..rest] | ["\n", ..rest] -> id_escape(rest, acc <> "-")

    [c, ..rest] -> id_escape(rest, acc <> c)
  }
}

fn take_heading_chars(in: Chars, level: Int, acc: Chars) -> #(Chars, Chars) {
  case in {
    [] | ["\n"] -> #(list.reverse(acc), [])
    ["\n", "\n", ..in] -> #(list.reverse(acc), in)
    ["\n", "#", ..rest] -> {
      case take_heading_chars_newline_hash(rest, level - 1, ["\n", ..acc]) {
        Some(#(acc, in)) -> take_heading_chars(in, level, acc)
        None -> #(list.reverse(acc), in)
      }
    }
    [c, ..in] -> take_heading_chars(in, level, [c, ..acc])
  }
}

fn take_heading_chars_newline_hash(
  in: Chars,
  level: Int,
  acc: Chars,
) -> Option(#(Chars, Chars)) {
  case in {
    _ if level < 0 -> None
    [] if level > 0 -> None

    [] if level == 0 -> Some(#(acc, []))
    [" ", ..in] if level == 0 -> Some(#(acc, in))

    ["#", ..rest] -> take_heading_chars_newline_hash(rest, level - 1, acc)

    [_, ..] -> None
  }
}

fn parse_inline(in: Chars, text: String, acc: List(Inline)) -> List(Inline) {
  case in {
    [] if text == "" -> list.reverse(acc)
    [] -> parse_inline([], "", [Text(text), ..acc])
    ["[", ..rest] -> {
      let #(container, in) = parse_link(rest, [])
      parse_inline(in, "", [container, ..acc])
    }
    [c, ..rest] -> parse_inline(rest, text <> c, acc)
  }
}

fn parse_link(in: Chars, acc: List(Inline)) -> #(Inline, Chars) {
  todo
}

fn heading_level(in: Chars, level: Int) -> Option(#(Int, Chars)) {
  case in {
    ["#", ..rest] -> heading_level(rest, level + 1)

    [] if level > 0 -> Some(#(level, []))
    [" ", ..rest] | ["\n", ..rest] if level != 0 -> Some(#(level, rest))

    _ -> None
  }
}

fn take_inline_text(inlines: List(Inline), acc: String) -> String {
  case inlines {
    [] -> acc
    [first, ..rest] ->
      case first {
        Text(text) -> take_inline_text(rest, acc <> text)
        Link(nested, _) -> {
          let acc = take_inline_text(nested, acc)
          take_inline_text(rest, acc)
        }
      }
  }
}

fn parse_paragraph(in: Chars) -> #(Container, Chars) {
  let #(inline_in, in) = take_paragraph_chars(in, [])
  let inline = parse_inline(inline_in, "", [])
  #(Paragraph(inline), in)
}

fn take_paragraph_chars(in: Chars, acc: Chars) -> #(Chars, Chars) {
  case in {
    [] | ["\n"] -> #(list.reverse(acc), [])
    ["\n", "\n", ..rest] -> #(list.reverse(acc), rest)
    [c, ..rest] -> take_paragraph_chars(rest, [c, ..acc])
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
      |> string.trim_right
    }
  }
}

fn inline_to_html(html: String, inline: Inline, refs: Refs) -> String {
  case inline {
    Text(text) -> html <> text
    Link(text, destination) -> {
      html
      |> open_tag("a", [#("href", destination_to_html(destination, refs))])
      |> inlines_to_html(text, refs)
      |> close_tag("a")
    }
  }
}

fn destination_to_html(destination: Destination, refs: Refs) -> String {
  case destination {
    Reference(id) ->
      case map.get(refs, id) {
        Ok(url) -> url
        Error(Nil) -> ""
      }
    Url(url) -> url
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
