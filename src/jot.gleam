import gleam/list
import gleam/string

pub type Inline {
  Text(String)
}

pub type Container {
  Paragraph(List(Inline))
}

type Chars =
  List(String)

// TODO: document
pub fn to_html(djot: String) -> String {
  djot
  |> to_ast
  |> ast_to_html
}

// TODO: document
pub fn to_ast(djot: String) -> List(Container) {
  djot
  |> string.replace("\r\n", "\n")
  |> string.to_graphemes
  |> parse([])
}

fn trim_lines(in: Chars) -> Chars {
  case in {
    [] -> []
    ["\n", ..rest] -> trim_lines(rest)
    [c, ..rest] -> [c, ..rest]
  }
}

fn parse(in: Chars, ast: List(Container)) -> List(Container) {
  let in = trim_lines(in)
  case in {
    [] -> list.reverse(ast)
    [c, ..rest] -> {
      let #(paragraph, in) = parse_paragraph(rest, c)
      parse(in, [paragraph, ..ast])
    }
  }
}

fn parse_paragraph(in: Chars, acc: String) -> #(Container, Chars) {
  case in {
    [] | ["\n"] -> #(Paragraph([Text(string.trim_right(acc))]), [])
    ["\n", "\n", ..rest] -> #(Paragraph([Text(string.trim_right(acc))]), rest)

    [c, ..rest] -> parse_paragraph(rest, acc <> c)
  }
}

pub fn ast_to_html(ast: List(Container)) -> String {
  containers_to_html(ast, "")
}

fn containers_to_html(containers: List(Container), html: String) -> String {
  list.fold(containers, html, fn(a, b) { container_to_html(b, a) })
}

fn container_to_html(container: Container, html: String) -> String {
  case container {
    Paragraph(inlines) -> {
      let html = html <> "<p>"
      let html = inlines_to_html(inlines, html)
      html <> "</p>"
    }
  } <> "\n"
}

fn inlines_to_html(inlines: List(Inline), html: String) -> String {
  list.fold(inlines, html, fn(a, b) { inline_to_html(b, a) })
}

fn inline_to_html(inline: Inline, html: String) -> String {
  case inline {
    Text(text) -> html <> text
  }
}
