// TODO: collapse adjacent text nodes

import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

pub type Document {
  Document(
    content: List(Container),
    references: Dict(String, String),
    footnotes: Dict(String, List(Container)),
  )
}

fn add_attribute(
  attributes: Dict(String, String),
  key: String,
  value: String,
) -> Dict(String, String) {
  case key {
    "class" ->
      dict.upsert(attributes, key, fn(previous) {
        case previous {
          None -> value
          Some(previous) -> previous <> " " <> value
        }
      })
    _ -> dict.insert(attributes, key, value)
  }
}

pub type Container {
  ThematicBreak
  Paragraph(attributes: Dict(String, String), List(Inline))
  Heading(attributes: Dict(String, String), level: Int, content: List(Inline))
  Codeblock(
    attributes: Dict(String, String),
    language: Option(String),
    content: String,
  )
}

pub type Inline {
  Linebreak
  Text(String)
  Link(content: List(Inline), destination: Destination)
  Image(content: List(Inline), destination: Destination)
  Emphasis(content: List(Inline))
  Strong(content: List(Inline))
  Footnote(reference: String)
  Code(content: String)
}

pub type Destination {
  Reference(String)
  Url(String)
}

type Chars =
  List(String)

type Refs {
  Refs(urls: Dict(String, String), footnotes: Dict(String, List(Container)))
}

/// Convert a string of Djot into a string of HTML.
///
/// If you want to have more control over the HTML generated you can use the
/// `parse` function to convert Djot to a tree of records instead. You can then
/// traverse this tree and turn it into HTML yourself.
///
pub fn to_html(djot: String) -> String {
  djot
  |> parse
  |> document_to_html
}

/// Convert a string of Djot into a tree of records.
///
/// This may be useful when you want more control over the HTML to be converted
/// to, or you wish to convert Djot to some other format.
///
pub fn parse(djot: String) -> Document {
  djot
  |> string.replace("\r\n", "\n")
  |> string.to_graphemes
  |> parse_document(Refs(dict.new(), dict.new()), [], dict.new())
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

fn count_drop_spaces(in: Chars, count: Int) -> #(Chars, Int) {
  case in {
    [] -> #([], count)
    [" ", ..rest] -> count_drop_spaces(rest, count + 1)
    [c, ..rest] -> #([c, ..rest], count)
  }
}

fn parse_document(
  in: Chars,
  refs: Refs,
  ast: List(Container),
  attrs: Dict(String, String),
) -> Document {
  let in = drop_lines(in)
  let #(in, spaces_count) = count_drop_spaces(in, 0)

  let unknown_handler = fn(in, refs, ast, attrs) {
    case in {
      ["[", "^", ..in2] -> {
        case parse_footnote_def(in2, "^") {
          None -> {
            let #(paragraph, in) = parse_paragraph(in, attrs)
            parse_document(in, refs, [paragraph, ..ast], dict.new())
          }
          Some(#(id, footnote, in)) -> {
            let refs =
              Refs(..refs, footnotes: dict.insert(refs.footnotes, id, footnote))
            parse_document(in, refs, ast, dict.new())
          }
        }
      }
      ["[", ..in2] -> {
        case parse_ref_def(in2, "") {
          None -> {
            let #(paragraph, in) = parse_paragraph(in, attrs)
            parse_document(in, refs, [paragraph, ..ast], dict.new())
          }
          Some(#(id, url, in)) -> {
            let refs = Refs(..refs, urls: dict.insert(refs.urls, id, url))
            parse_document(in, refs, ast, dict.new())
          }
        }
      }
      _ -> {
        let #(paragraph, in) = parse_paragraph(in, attrs)
        parse_document(in, refs, [paragraph, ..ast], dict.new())
      }
    }
  }

  parse_block_segment(
    in,
    refs,
    ast,
    attrs,
    spaces_count,
    parse_document,
    fn(ast, refs, _) {
      Document(
        content: list.reverse(ast),
        references: refs.urls,
        footnotes: refs.footnotes,
      )
    },
    unknown_handler,
  )
}

fn parse_block(
  in: Chars,
  refs: Refs,
  ast: List(Container),
  attrs: Dict(String, String),
  required_spaces: Int,
) -> #(List(Container), Chars) {
  let in = drop_lines(in)
  let #(in, spaces_count) = count_drop_spaces(in, 0)

  use <- bool.lazy_guard(spaces_count < required_spaces, fn() {
    #(list.reverse(ast), in)
  })
  parse_block_indent_unchecked(
    in,
    refs,
    ast,
    attrs,
    required_spaces,
    spaces_count,
  )
}

fn parse_block_indent_unchecked(
  in: Chars,
  refs: Refs,
  ast: List(Container),
  attrs: Dict(String, String),
  required_spaces required_spaces: Int,
  indentation indentation: Int,
) {
  case in {
    // if there is a newline at the very beginning of the block, instantly start checking indentation 
    // to make sure it isn't an empty block
    ["\n", ..] -> parse_block(in, refs, ast, attrs, required_spaces)
    _ -> {
      let unknown_handler = fn(in, refs, ast, attrs) {
        let #(paragraph, in) = parse_paragraph(in, attrs)
        parse_block(in, refs, [paragraph, ..ast], dict.new(), required_spaces)
      }

      parse_block_segment(
        in,
        refs,
        ast,
        attrs,
        indentation,
        fn(in, refs, ast, attrs) {
          parse_block(in, refs, ast, attrs, required_spaces)
        },
        fn(ast, _refs, rest) { #(list.reverse(ast), rest) },
        unknown_handler,
      )
    }
  }
}

fn parse_block_segment(
  in: Chars,
  refs: Refs,
  ast: List(Container),
  attrs: Dict(String, String),
  indentation: Int,
  parser: fn(Chars, Refs, List(Container), Dict(String, String)) -> a,
  ast_handler: fn(List(Container), Refs, Chars) -> a,
  unknown_handler: fn(Chars, Refs, List(Container), Dict(String, String)) -> a,
) -> a {
  case in {
    [] -> ast_handler(ast, refs, [])
    ["{", ..in2] ->
      case parse_attributes(in2, attrs) {
        None -> {
          let #(paragraph, in) = parse_paragraph(in, attrs)
          parser(in, refs, [paragraph, ..ast], dict.new())
        }
        Some(#(attrs, in)) -> parser(in, refs, ast, attrs)
      }

    ["#", ..in] -> {
      let #(heading, refs, in) = parse_heading(in, refs, attrs)
      parser(in, refs, [heading, ..ast], dict.new())
    }

    ["~" as delim, ..in2] | ["`" as delim, ..in2] -> {
      case parse_codeblock(in2, attrs, delim, indentation) {
        None -> {
          let #(paragraph, in) = parse_paragraph(in, attrs)
          parser(in, refs, [paragraph, ..ast], dict.new())
        }
        Some(#(codeblock, in)) ->
          parser(in, refs, [codeblock, ..ast], dict.new())
      }
    }

    ["-", ..in2] | ["*", ..in2] -> {
      case parse_thematic_break(1, in2) {
        None -> {
          let #(paragraph, in) = parse_paragraph(in, attrs)
          parser(in, refs, [paragraph, ..ast], dict.new())
        }
        Some(#(thematic_break, in)) -> {
          parser(in, refs, [thematic_break, ..ast], dict.new())
        }
      }
    }
    _ -> unknown_handler(in, refs, ast, attrs)
  }
}

fn parse_thematic_break(count: Int, in: Chars) -> Option(#(Container, Chars)) {
  case in {
    [] | ["\n", ..] ->
      case count >= 3 {
        True -> Some(#(ThematicBreak, in))
        False -> None
      }
    [" ", ..rest] | ["\t", ..rest] -> parse_thematic_break(count, rest)
    ["-", ..rest] | ["*", ..rest] -> parse_thematic_break(count + 1, rest)
    _ -> None
  }
}

fn parse_codeblock(
  in: Chars,
  attrs: Dict(String, String),
  delim: String,
  indentation: Int,
) -> Option(#(Container, Chars)) {
  use #(language, count, in) <- option.then(parse_codeblock_start(in, delim, 1))
  let #(content, in) =
    parse_codeblock_content(in, delim, count, indentation, "")
  Some(#(Codeblock(attrs, language, content), in))
}

fn parse_codeblock_start(
  in: Chars,
  delim: String,
  count: Int,
) -> Option(#(Option(String), Int, Chars)) {
  case in {
    [c, ..in] if c == delim -> parse_codeblock_start(in, delim, count + 1)
    ["\n", ..in] if count >= 3 -> Some(#(None, count, in))
    [_, ..] if count >= 3 -> {
      let in = drop_spaces(in)
      use #(language, in) <- option.map(parse_codeblock_language(in, ""))
      #(language, count, in)
    }
    _ -> None
  }
}

fn parse_codeblock_content(
  in: Chars,
  delim: String,
  count: Int,
  indentation: Int,
  acc: String,
) -> #(String, Chars) {
  case parse_codeblock_end(in, delim, count) {
    None -> {
      let #(acc, in) = slurp_verbatim_line(in, indentation, acc)
      parse_codeblock_content(in, delim, count, indentation, acc)
    }
    Some(#(in)) -> #(acc, in)
  }
}

fn slurp_verbatim_line(
  in: Chars,
  indentation: Int,
  acc: String,
) -> #(String, Chars) {
  case in {
    [] -> #(acc, [])
    // if the codeblock itself is indented, we ignore spaces up to the level of the indent
    [" ", ..in] if indentation > 0 ->
      slurp_verbatim_line(in, indentation - 1, acc)
    ["\n", ..in] -> #(acc <> "\n", in)
    [c, ..in] -> slurp_verbatim_line(in, 0, acc <> c)
  }
}

fn parse_codeblock_end(in: Chars, delim: String, count: Int) -> Option(#(Chars)) {
  case in {
    ["\n", ..in] if count == 0 -> Some(#(in))
    _ if count == 0 -> Some(#(in))

    // if the codeblock is indented (ex: in a footnote block), we need to accept an indented end marker
    [" ", ..in] -> parse_codeblock_end(in, delim, count)
    [c, ..in] if c == delim -> parse_codeblock_end(in, delim, count - 1)

    [] -> Some(#(in))
    _ -> None
  }
}

fn parse_codeblock_language(
  in: Chars,
  language: String,
) -> Option(#(Option(String), Chars)) {
  case in {
    // A language specifier cannot contain a backtick
    ["`", ..] -> None

    [] -> Some(#(None, in))
    ["\n", ..in] if language == "" -> Some(#(None, in))
    ["\n", ..in] -> Some(#(Some(language), in))
    [c, ..in] -> parse_codeblock_language(in, language <> c)
  }
}

fn parse_ref_def(in: Chars, id: String) -> Option(#(String, String, Chars)) {
  case in {
    ["]", ":", ..in] -> parse_ref_value(in, id, "")
    [] | ["]", ..] | ["\n", ..] -> None
    [c, ..in] -> parse_ref_def(in, id <> c)
  }
}

fn parse_ref_value(
  in: Chars,
  id: String,
  url: String,
) -> Option(#(String, String, Chars)) {
  case in {
    [] -> Some(#(id, string.trim(url), []))
    ["\n", " ", ..in] -> parse_ref_value(drop_spaces(in), id, url)
    ["\n", ..in] -> Some(#(id, string.trim(url), in))
    [c, ..in] -> parse_ref_value(in, id, url <> c)
  }
}

fn parse_footnote_def(
  in: Chars,
  id: String,
) -> Option(#(String, List(Container), Chars)) {
  case in {
    ["]", ":", ..in] -> {
      let in = drop_spaces(in)
      let #(block, rest) =
        parse_block_indent_unchecked(
          in,
          Refs(dict.new(), dict.new()),
          [],
          dict.new(),
          required_spaces: 1,
          indentation: 0,
        )
      Some(#(id, block, rest))
    }
    [] | ["]", ..] | ["\n", ..] -> None
    [c, ..in] -> parse_footnote_def(in, id <> c)
  }
}

fn parse_attributes(
  in: Chars,
  attrs: Dict(String, String),
) -> Option(#(Dict(String, String), Chars)) {
  let in = drop_spaces(in)
  case in {
    [] -> None
    ["}", ..in] -> parse_attributes_end(in, attrs)
    ["#", ..in] -> {
      case parse_attributes_id_or_class(in, "") {
        Some(#(id, in)) -> parse_attributes(in, add_attribute(attrs, "id", id))
        None -> None
      }
    }
    [".", ..in] -> {
      case parse_attributes_id_or_class(in, "") {
        Some(#(c, in)) -> parse_attributes(in, add_attribute(attrs, "class", c))
        None -> None
      }
    }
    _ -> {
      case parse_attribute(in, "") {
        Some(#(k, v, in)) -> parse_attributes(in, add_attribute(attrs, k, v))
        None -> None
      }
    }
  }
}

fn parse_attribute(in: Chars, key: String) -> Option(#(String, String, Chars)) {
  case in {
    [] | [" ", ..] -> None
    ["=", "\"", ..in] -> parse_attribute_quoted_value(in, key, "")
    ["=", ..in] -> parse_attribute_value(in, key, "")
    [c, ..in] -> parse_attribute(in, key <> c)
  }
}

fn parse_attribute_value(
  in: Chars,
  key: String,
  value: String,
) -> Option(#(String, String, Chars)) {
  case in {
    [] -> None
    [" ", ..in] -> Some(#(key, value, in))
    ["}", ..] -> Some(#(key, value, in))
    [c, ..in] -> parse_attribute_value(in, key, value <> c)
  }
}

fn parse_attribute_quoted_value(
  in: Chars,
  key: String,
  value: String,
) -> Option(#(String, String, Chars)) {
  case in {
    [] -> None
    ["\"", ..in] -> Some(#(key, value, in))
    [c, ..in] -> parse_attribute_quoted_value(in, key, value <> c)
  }
}

fn parse_attributes_id_or_class(
  in: Chars,
  id: String,
) -> Option(#(String, Chars)) {
  case in {
    [] | ["}", ..] | [" ", ..] -> Some(#(id, in))
    ["#", ..] | [".", ..] | ["=", ..] -> None
    // TODO: in future this will be permitted as attributes can be over multiple lines
    ["\n", ..] -> None
    [c, ..in] -> parse_attributes_id_or_class(in, id <> c)
  }
}

fn parse_attributes_end(
  in: Chars,
  attrs: Dict(String, String),
) -> Option(#(Dict(String, String), Chars)) {
  case in {
    [] -> Some(#(attrs, []))
    ["\n", ..in] -> Some(#(attrs, in))
    [" ", ..in] -> parse_attributes_end(in, attrs)
    [_, ..] -> None
  }
}

fn parse_heading(
  in: Chars,
  refs: Refs,
  attrs: Dict(String, String),
) -> #(Container, Refs, Chars) {
  case heading_level(in, 1) {
    Some(#(level, in)) -> {
      let in = drop_spaces(in)
      let #(inline_in, in) = take_heading_chars(in, level, [])
      let inline = parse_inline(inline_in, "", [])
      let text = take_inline_text(inline, "")
      let #(refs, attrs) = case id_sanitise(text) {
        "" -> #(refs, attrs)
        id -> {
          case dict.get(refs.urls, id) {
            Ok(_) -> #(refs, attrs)
            Error(_) -> {
              let refs =
                Refs(..refs, urls: dict.insert(refs.urls, id, "#" <> id))
              let attrs = add_attribute(attrs, "id", id)
              #(refs, attrs)
            }
          }
        }
      }
      let heading = Heading(attrs, level, inline)
      #(heading, refs, in)
    }

    None -> {
      let #(p, in) = parse_paragraph(["#", ..in], attrs)
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
    "#" | "?" | "!" | "," -> False
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

    _ -> None
  }
}

fn parse_inline(in: Chars, text: String, acc: List(Inline)) -> List(Inline) {
  case in {
    [] if text == "" -> list.reverse(acc)
    [] -> parse_inline([], "", [Text(text), ..acc])

    // Escapes
    ["\\", c, ..rest] -> {
      case c {
        "\n" -> {
          parse_inline(rest, "", [Linebreak, Text(text), ..acc])
        }
        " " -> {
          parse_inline(rest, text <> "&nbsp;", acc)
        }
        "!"
        | "\""
        | "#"
        | "$"
        | "%"
        | "&"
        | "'"
        | "("
        | ")"
        | "*"
        | "+"
        | ","
        | "-"
        | "."
        | "/"
        | ":"
        | ";"
        | "<"
        | "="
        | ">"
        | "?"
        | "@"
        | "["
        | "\\"
        | "]"
        | "^"
        | "_"
        | "`"
        | "{"
        | "|"
        | "}"
        | "~" -> {
          parse_inline(rest, text <> c, acc)
        }
        _ -> parse_inline(list.append([c], rest), text <> "\\", acc)
      }
    }

    // Emphasis and strong
    ["_", c, ..rest] if c != " " && c != "\t" && c != "\n" -> {
      let rest = [c, ..rest]
      case parse_emphasis(rest, "_") {
        None -> parse_inline(rest, text <> "_", acc)
        Some(#(inner, in)) ->
          parse_inline(in, "", [Emphasis(inner), Text(text), ..acc])
      }
    }
    ["*", c, ..rest] if c != " " && c != "\t" && c != "\n" -> {
      let rest = [c, ..rest]
      case parse_emphasis(rest, "*") {
        None -> parse_inline(rest, text <> "*", acc)
        Some(#(inner, in)) ->
          parse_inline(in, "", [Strong(inner), Text(text), ..acc])
      }
    }

    ["[", "^", ..rest] -> {
      case parse_footnote(rest, "^") {
        None -> parse_inline(rest, text <> "[", acc)
        Some(#(link, in)) -> parse_inline(in, "", [link, Text(text), ..acc])
      }
    }

    // Link and image
    ["[", ..rest] -> {
      case parse_link(rest, Link) {
        None -> parse_inline(rest, text <> "[", acc)
        Some(#(link, in)) -> parse_inline(in, "", [link, Text(text), ..acc])
      }
    }
    ["!", "[", ..rest] -> {
      case parse_link(rest, Image) {
        None -> parse_inline(rest, text <> "![", acc)
        Some(#(image, in)) -> parse_inline(in, "", [image, Text(text), ..acc])
      }
    }

    // Code
    ["`", ..rest] -> {
      let #(code, in) = parse_code(rest, 1)
      parse_inline(in, "", [code, Text(text), ..acc])
    }

    ["\n", ..rest] ->
      drop_spaces(rest)
      |> parse_inline(text <> "\n", acc)
    [c, ..rest] -> parse_inline(rest, text <> c, acc)
  }
}

fn parse_code(in: Chars, count: Int) -> #(Inline, Chars) {
  case in {
    ["`", ..in] -> parse_code(in, count + 1)
    _ -> {
      let #(content, in) = parse_code_content(in, count, "")

      // If the string has a single space at the end then a backtick we are
      // supposed to not include that space. This is so inline code can start
      // with a backtick.
      let content = case string.starts_with(content, " `") {
        True -> string.trim_left(content)
        False -> content
      }
      let content = case string.ends_with(content, "` ") {
        True -> string.trim_right(content)
        False -> content
      }
      #(Code(content), in)
    }
  }
}

fn parse_code_content(
  in: Chars,
  count: Int,
  content: String,
) -> #(String, Chars) {
  case in {
    [] -> #(content, in)
    ["`", ..in] -> {
      let #(done, content, in) = parse_code_end(in, count, 1, content)
      case done {
        True -> #(content, in)
        False -> parse_code_content(in, count, content)
      }
    }
    [c, ..in] -> parse_code_content(in, count, content <> c)
  }
}

fn parse_code_end(
  in: Chars,
  limit: Int,
  count: Int,
  content: String,
) -> #(Bool, String, Chars) {
  case in {
    [] -> #(True, content, in)
    ["`", ..in] -> parse_code_end(in, limit, count + 1, content)
    [_, ..] if limit == count -> #(True, content, in)
    [_, ..] -> #(False, content <> string.repeat("`", count), in)
  }
}

fn parse_emphasis(in: Chars, close: String) -> Option(#(List(Inline), Chars)) {
  case take_emphasis_chars(in, close, []) {
    None -> None

    Some(#(inline_in, in)) -> {
      let inline = parse_inline(inline_in, "", [])
      Some(#(inline, in))
    }
  }
}

fn take_emphasis_chars(
  in: Chars,
  close: String,
  acc: Chars,
) -> Option(#(Chars, Chars)) {
  case in {
    [] -> None

    // Inline code overrides emphasis
    ["`", ..] -> None

    // The close is not a close if it is preceeded by whitespace
    ["\t", c, ..in] if c == close ->
      take_emphasis_chars(in, close, [" ", c, ..acc])
    ["\n", c, ..in] if c == close ->
      take_emphasis_chars(in, close, [" ", c, ..acc])
    [" ", c, ..in] if c == close ->
      take_emphasis_chars(in, close, [" ", c, ..acc])

    [c, ..in] if c == close -> {
      case list.reverse(acc) {
        [] -> None
        acc -> Some(#(acc, in))
      }
    }
    [c, ..rest] -> take_emphasis_chars(rest, close, [c, ..acc])
  }
}

fn parse_link(
  in: Chars,
  to_inline: fn(List(Inline), Destination) -> Inline,
) -> Option(#(Inline, Chars)) {
  case take_link_chars(in, []) {
    // This wasn't a link, it was just a `[` in the text
    None -> None

    Some(#(inline_in, ref, in)) -> {
      let inline = parse_inline(inline_in, "", [])
      let ref = case ref {
        Reference("") -> Reference(take_inline_text(inline, ""))
        ref -> ref
      }
      Some(#(to_inline(inline, ref), in))
    }
  }
}

fn take_link_chars(
  in: Chars,
  inline_in: Chars,
) -> Option(#(Chars, Destination, Chars)) {
  case in {
    // This wasn't a link, it was just a `[..]` in the text
    [] -> None

    ["]", "[", ..in] -> {
      let inline_in = list.reverse(inline_in)
      take_link_chars_destination(in, False, inline_in, "")
    }
    ["]", "(", ..in] -> {
      let inline_in = list.reverse(inline_in)
      take_link_chars_destination(in, True, inline_in, "")
    }
    [c, ..rest] -> take_link_chars(rest, [c, ..inline_in])
  }
}

fn take_link_chars_destination(
  in: Chars,
  is_url: Bool,
  inline_in: Chars,
  acc: String,
) -> Option(#(Chars, Destination, Chars)) {
  case in {
    [] -> None

    [")", ..in] if is_url -> Some(#(inline_in, Url(acc), in))
    ["]", ..in] if !is_url -> Some(#(inline_in, Reference(acc), in))

    ["\n", ..rest] if is_url ->
      take_link_chars_destination(rest, is_url, inline_in, acc)
    ["\n", ..rest] if !is_url ->
      take_link_chars_destination(rest, is_url, inline_in, acc <> " ")
    [c, ..rest] ->
      take_link_chars_destination(rest, is_url, inline_in, acc <> c)
  }
}

fn parse_footnote(in: Chars, acc: String) -> Option(#(Inline, Chars)) {
  case in {
    // This wasn't a link, it was just a `[` in the text
    [] -> None

    ["]", ..rest] -> {
      Some(#(Footnote(acc), rest))
    }
    [c, ..rest] -> {
      parse_footnote(rest, acc <> c)
    }
  }
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
        Text(text) | Code(text) -> take_inline_text(rest, acc <> text)
        Strong(inlines) | Emphasis(inlines) ->
          take_inline_text(list.append(inlines, rest), acc)
        Link(nested, _) | Image(nested, _) -> {
          let acc = take_inline_text(nested, acc)
          take_inline_text(rest, acc)
        }
        Linebreak | Footnote(_) -> {
          take_inline_text(rest, acc)
        }
      }
  }
}

fn parse_paragraph(
  in: Chars,
  attrs: Dict(String, String),
) -> #(Container, Chars) {
  let #(inline_in, in) = take_paragraph_chars(in, [])
  let inline = parse_inline(inline_in, "", [])
  #(Paragraph(attrs, inline), in)
}

fn take_paragraph_chars(in: Chars, acc: Chars) -> #(Chars, Chars) {
  case in {
    [] | ["\n"] -> #(list.reverse(acc), [])
    ["\n", "\n", ..rest] -> #(list.reverse(acc), rest)
    [c, ..rest] -> take_paragraph_chars(rest, [c, ..acc])
  }
}

/// Convert a document tree into a string of HTML.
///
pub fn document_to_html(document: Document) -> String {
  let generated_html =
    containers_to_html(
      document.content,
      Refs(document.references, document.footnotes),
      GeneratedHtml("", []),
    )

  // only create the footnotes section if it is needed
  use <- bool.guard(
    list.is_empty(generated_html.used_footnotes),
    generated_html.html,
  )

  let footnotes_section_html =
    generated_html
    |> open_tag("section", dict.from_list([#("role", "doc-endnotes")]))
    |> append_to_html("\n")
    |> open_tag("hr", dict.new())
    |> append_to_html("\n")
    |> open_tag("ol", dict.new())
    |> append_to_html("\n")

  let add_link = fn(html, footnote_number) {
    html
    |> open_tag_ordered_attributes("a", [
      #("href", "#fnref" <> footnote_number),
      #("role", "doc-backlink"),
    ])
    |> append_to_html("↩︎")
    |> close_tag("a")
  }

  let footnote_to_html = fn(footnote: #(Int, String), footnote_number: String) {
    dict.get(document.footnotes, footnote.1)
    |> result.map(fn(footnote) {
      containers_to_html_with_last_paragraph(
        footnote,
        Refs(document.references, document.footnotes),
        GeneratedHtml("", []),
        add_link(_, footnote_number),
      )
    })
    |> result.lazy_unwrap(fn() {
      GeneratedHtml("", [])
      |> open_tag_ordered_attributes("p", [])
      |> add_link(footnote_number)
      |> close_tag("p")
    })
  }

  let html_with_footnotes =
    list.fold(
      list.reverse(footnotes_section_html.used_footnotes),
      footnotes_section_html,
      fn(html, footnote) {
        let footnote_number = int.to_string(footnote.0)
        let footnote_html = footnote_to_html(footnote, footnote_number)

        html
        |> open_tag("li", dict.from_list([#("id", "fn" <> footnote_number)]))
        |> append_to_html("\n")
        |> append_to_html(footnote_html.html)
        |> append_to_html("\n")
        |> close_tag("li")
        |> append_to_html("\n")
      },
    )

  {
    html_with_footnotes
    |> close_tag("ol")
    |> append_to_html("\n")
    |> close_tag("section")
    |> append_to_html("\n")
  }.html
}

type Footnotes =
  List(#(Int, String))

type GeneratedHtml {
  GeneratedHtml(html: String, used_footnotes: Footnotes)
}

fn containers_to_html_with_last_paragraph(
  containers: List(Container),
  refs: Refs,
  html: GeneratedHtml,
  apply: fn(GeneratedHtml) -> GeneratedHtml,
) -> GeneratedHtml {
  case containers {
    [] -> html
    [container] -> {
      case container {
        Paragraph(attrs, inlines) ->
          html
          |> open_tag("p", attrs)
          |> inlines_to_html(inlines, refs)
          |> apply()
          |> close_tag("p")
        _ ->
          container_to_html(html, container, refs)
          |> open_tag("p", dict.new())
          |> apply()
          |> close_tag("p")
      }
    }
    [container, ..rest] -> {
      let html = container_to_html(html, container, refs)
      containers_to_html_with_last_paragraph(rest, refs, html, apply)
    }
  }
}

fn containers_to_html(
  containers: List(Container),
  refs: Refs,
  html: GeneratedHtml,
) -> GeneratedHtml {
  case containers {
    [] -> html
    [container, ..rest] -> {
      let html = container_to_html(html, container, refs)
      containers_to_html(rest, refs, html)
    }
  }
}

fn container_to_html(
  html: GeneratedHtml,
  container: Container,
  refs: Refs,
) -> GeneratedHtml {
  let new_html = case container {
    ThematicBreak -> html |> open_tag("hr", dict.new())

    Paragraph(attrs, inlines) -> {
      html
      |> open_tag("p", attrs)
      |> inlines_to_html(inlines, refs)
      |> close_tag("p")
    }

    Codeblock(attrs, language, content) -> {
      let code_attrs = case language {
        Some(lang) -> add_attribute(attrs, "class", "language-" <> lang)
        None -> attrs
      }
      html
      |> open_tag("pre", dict.new())
      |> open_tag("code", code_attrs)
      |> append_to_html(content)
      |> close_tag("code")
      |> close_tag("pre")
    }

    Heading(attrs, level, inlines) -> {
      let tag = "h" <> int.to_string(level)
      html
      |> open_tag(tag, attrs)
      |> inlines_to_html(inlines, refs)
      |> close_tag(tag)
    }
  }
  append_to_html(new_html, "\n")
}

fn append_to_html(original_html: GeneratedHtml, str: String) -> GeneratedHtml {
  GeneratedHtml(..original_html, html: original_html.html <> str)
}

fn open_tag(
  initial_html: GeneratedHtml,
  tag: String,
  attributes: Dict(String, String),
) -> GeneratedHtml {
  let html = initial_html.html <> "<" <> tag
  GeneratedHtml(
    ..initial_html,
    html: attributes_to_html(html, attributes) <> ">",
  )
}

// Some of the tests require a specific order of attributes for them to pass (unlike most which are alphabetical)
// This function allows you to provide a specific order, which open_tag cannot guarantee as dict.Dict has no set order.
fn open_tag_ordered_attributes(
  initial_html: GeneratedHtml,
  tag: String,
  attributes: List(#(String, String)),
) -> GeneratedHtml {
  let html = initial_html.html <> "<" <> tag
  GeneratedHtml(
    ..initial_html,
    html: ordered_attributes_to_html(attributes, html) <> ">",
  )
}

fn close_tag(initial_html: GeneratedHtml, tag: String) -> GeneratedHtml {
  GeneratedHtml(..initial_html, html: initial_html.html <> "</" <> tag <> ">")
}

fn inlines_to_html(
  html: GeneratedHtml,
  inlines: List(Inline),
  refs: Refs,
) -> GeneratedHtml {
  case inlines {
    [] -> html
    [inline, ..rest] -> {
      let html =
        html
        |> inline_to_html(inline, refs)
        |> inlines_to_html(rest, refs)

      GeneratedHtml(..html, html: string.trim_right(html.html))
    }
  }
}

fn inline_to_html(
  html: GeneratedHtml,
  inline: Inline,
  refs: Refs,
) -> GeneratedHtml {
  case inline {
    Linebreak -> {
      html
      |> open_tag("br", dict.new())
      |> append_to_html("\n")
    }
    Text(text) -> {
      append_to_html(html, text)
    }
    Strong(inlines) -> {
      html
      |> open_tag("strong", dict.new())
      |> inlines_to_html(inlines, refs)
      |> close_tag("strong")
    }
    Emphasis(inlines) -> {
      html
      |> open_tag("em", dict.new())
      |> inlines_to_html(inlines, refs)
      |> close_tag("em")
    }
    Link(text, destination) -> {
      html
      |> open_tag("a", destination_attribute("href", destination, refs))
      |> inlines_to_html(text, refs)
      |> close_tag("a")
    }
    Image(text, destination) -> {
      html
      |> open_tag(
        "img",
        destination_attribute("src", destination, refs)
          |> dict.insert("alt", take_inline_text(text, "")),
      )
    }
    Code(content) -> {
      html
      |> open_tag("code", dict.new())
      |> append_to_html(content)
      |> close_tag("code")
    }
    Footnote(reference) -> {
      let #(footnote_number, new_used_footnotes) =
        find_footnote_number(
          html.used_footnotes,
          reference,
          html.used_footnotes,
        )
      let footnote_attrs = [
        #("id", "fnref" <> footnote_number),
        #("href", "#fn" <> footnote_number),
        #("role", "doc-noteref"),
      ]

      let updated_html =
        html
        |> open_tag_ordered_attributes("a", footnote_attrs)
        |> append_to_html("<sup>" <> footnote_number <> "</sup>")
        |> close_tag("a")

      GeneratedHtml(..updated_html, used_footnotes: new_used_footnotes)
    }
  }
}

fn find_footnote_number(
  footnotes_to_check: Footnotes,
  reference: String,
  used_footnotes: Footnotes,
) -> #(String, Footnotes) {
  case footnotes_to_check {
    [] -> {
      let next_number =
        {
          used_footnotes
          |> list.first()
          |> result.map(fn(f) { f.0 })
          |> result.unwrap(0)
        }
        + 1

      #(int.to_string(next_number), [
        #(next_number, reference),
        ..used_footnotes
      ])
    }
    [#(index, ref), ..] if reference == ref -> {
      #(int.to_string(index), used_footnotes)
    }
    [_, ..rest] -> find_footnote_number(rest, reference, used_footnotes)
  }
}

fn destination_attribute(
  key: String,
  destination: Destination,
  refs: Refs,
) -> Dict(String, String) {
  let dict = dict.new()
  case destination {
    Url(url) -> dict.insert(dict, key, url)
    Reference(id) ->
      case dict.get(refs.urls, id) {
        Ok(url) -> dict.insert(dict, key, url)
        _ -> dict
      }
  }
}

fn attributes_to_html(html: String, attributes: Dict(String, String)) -> String {
  attributes
  |> dict.to_list
  |> list.sort(fn(a, b) { string.compare(a.0, b.0) })
  |> ordered_attributes_to_html(html)
}

fn ordered_attributes_to_html(
  attributes: List(#(String, String)),
  html: String,
) -> String {
  list.fold(attributes, html, fn(html, pair) {
    html <> " " <> pair.0 <> "=\"" <> pair.1 <> "\""
  })
}
