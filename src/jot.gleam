// TODO: collapse adjacent text nodes

import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import houdini
import splitter.{type Splitter}

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
  Paragraph(attributes: Dict(String, String), content: List(Statement))
  Heading(attributes: Dict(String, String), level: Int, content: List(Inline))
  Codeblock(
    attributes: Dict(String, String),
    language: Option(String),
    content: String,
  )
  RawBlock(content: String)
  BulletList(layout: ListLayout, style: String, items: List(List(Container)))
}

pub type Statement {
  Statement(inlines: List(Inline))
}

pub type Inline {
  Linebreak
  NonBreakingSpace
  Text(String)
  Link(content: List(Inline), destination: Destination)
  Image(content: List(Inline), destination: Destination)
  Emphasis(content: List(Inline))
  Strong(content: List(Inline))
  Footnote(reference: String)
  Code(content: String)
  MathInline(content: String)
  MathDisplay(content: String)
}

pub type ListLayout {
  Tight
  Loose
}

pub type Destination {
  Reference(String)
  Url(String)
}

type Refs {
  Refs(urls: Dict(String, String), footnotes: Dict(String, List(Container)))
}

/// Convert a string of Djot into a string of HTML.
///
/// If you want to have more control over the HTML generated you can use the
/// `parse` function to convert Djot to a tree of records instead. You can then
/// traverse this tree and turn it into HTML yourself.
///
/// # Security
///
/// This does not escape the content of raw blocks! If you use this with
/// user-input you likely need to escape raw blocks to prevent
/// cross-site-scripting (XSS) attacks.
///
pub fn to_html(djot: String) -> String {
  djot
  |> parse
  |> document_to_html
}

type Splitters {
  Splitters(
    verbatim_line_end: Splitter,
    codeblock_language: Splitter,
    inline: Splitter,
    link_destination: Splitter,
    math_end: Splitter,
  )
}

/// Convert a string of Djot into a tree of records.
///
/// This may be useful when you want more control over the HTML to be converted
/// to, or you wish to convert Djot to some other format.
///
pub fn parse(djot: String) -> Document {
  let splitters =
    Splitters(
      verbatim_line_end: splitter.new([" ", "\n"]),
      codeblock_language: splitter.new(["`", "\n"]),
      inline: splitter.new([
        "\\", "_", "*", "[^", "[", "![", "$$`", "$`", "`", "\n", ".", "!", "?",
      ]),
      link_destination: splitter.new([")", "]", "\n"]),
      math_end: splitter.new(["`"]),
    )
  let refs = Refs(dict.new(), dict.new())

  let #(ast, Refs(urls, footnotes), _) =
    djot
    |> string.replace("\r\n", "\n")
    |> parse_document_content(refs, splitters, [], dict.new())

  Document(ast, urls, footnotes)
}

fn drop_lines(in: String) -> String {
  case in {
    "\n" <> rest -> drop_lines(rest)
    other -> other
  }
}

fn drop_spaces(in: String) -> String {
  case in {
    " " <> rest -> drop_spaces(rest)
    other -> other
  }
}

fn count_drop_spaces(in: String, count: Int) -> #(String, Int) {
  case in {
    " " <> rest -> count_drop_spaces(rest, count + 1)
    other -> #(other, count)
  }
}

fn parse_document_content(
  in: String,
  refs: Refs,
  splitters: Splitters,
  ast: List(Container),
  attrs: Dict(String, String),
) -> #(List(Container), Refs, String) {
  let in = drop_lines(in)
  let #(in, spaces_count) = count_drop_spaces(in, 0)

  let #(in, refs, container, attrs) =
    parse_container(in, refs, splitters, attrs, spaces_count)
  let ast = case container {
    None -> ast
    Some(container) -> [container, ..ast]
  }
  case in {
    "" -> #(list.reverse(ast), refs, in)
    _ -> parse_document_content(in, refs, splitters, ast, attrs)
  }
}

/// Parse a block of Djot that ends once the content is no longer indented
/// to a certain level.
/// For example:
///
/// ```djot
/// Here's the reference.[^ref]
///
/// [^ref]: This footnote is a block with two paragraphs.
///
///   This is part of the block because it is indented past the start of `[^ref]`
///
/// But this would not be parsed as part of the block because it has no indentation
/// ```
fn parse_block(
  in: String,
  refs: Refs,
  splitters: Splitters,
  ast: List(Container),
  attrs: Dict(String, String),
  required_spaces: Int,
) -> #(List(Container), Refs, String) {
  let in = drop_lines(in)
  let #(in, indentation) = count_drop_spaces(in, 0)

  case indentation < required_spaces {
    True -> #(list.reverse(ast), refs, in)
    False -> {
      let #(in, refs, container, attrs) =
        parse_container(in, refs, splitters, attrs, indentation)
      let ast = case container {
        None -> ast
        Some(container) -> [container, ..ast]
      }
      case in {
        "" -> #(list.reverse(ast), refs, in)
        _ -> parse_block(in, refs, splitters, ast, attrs, required_spaces)
      }
    }
  }
}

/// This function allows us to parse the contents of a block after we know
/// that the *first* container meets indentation requirements, but we want to
/// ensure that once this container is parsed, future containers meet the
/// indentation requirements
fn parse_block_after_indent_checked(
  in: String,
  refs: Refs,
  splitters: Splitters,
  ast: List(Container),
  attrs: Dict(String, String),
  required_spaces required_spaces: Int,
  indentation indentation: Int,
) -> #(List(Container), Refs, String) {
  let #(in, refs, container, attrs) =
    parse_container(in, refs, splitters, attrs, indentation)
  let ast = case container {
    None -> ast
    Some(container) -> [container, ..ast]
  }
  case in {
    "" -> #(list.reverse(ast), refs, in)
    _ -> parse_block(in, refs, splitters, ast, attrs, required_spaces)
  }
}

fn parse_container(
  in: String,
  refs: Refs,
  splitters: Splitters,
  attrs: Dict(String, String),
  indentation: Int,
) -> #(String, Refs, Option(Container), Dict(String, String)) {
  case in {
    "" -> #(in, refs, None, dict.new())

    "{" <> in2 ->
      case parse_attributes(in2, attrs) {
        None -> {
          let #(paragraph, in) = parse_paragraph(in, attrs, splitters)
          #(in, refs, Some(paragraph), dict.new())
        }
        Some(#(attrs, in)) -> #(in, refs, None, attrs)
      }

    "#" <> in -> {
      let #(heading, refs, in) = parse_heading(in, refs, splitters, attrs)
      #(in, refs, Some(heading), dict.new())
    }

    "~" as delim <> in2 | "`" as delim <> in2 -> {
      case parse_codeblock(in2, attrs, delim, indentation, splitters) {
        None -> {
          let #(paragraph, in) = parse_paragraph(in, attrs, splitters)
          #(in, refs, Some(paragraph), dict.new())
        }
        Some(#(codeblock, in)) -> #(in, refs, Some(codeblock), dict.new())
      }
    }

    "-" as style <> in2 | "*" as style <> in2 -> {
      case parse_thematic_break(1, in2), in2 {
        None, " " <> in2 | None, "\n" <> in2 -> {
          let #(list, in) =
            parse_bullet_list(in2, refs, attrs, style, Tight, [], splitters)
          #(in, refs, Some(list), dict.new())
        }
        None, _ -> {
          let #(paragraph, in) = parse_paragraph(in, attrs, splitters)
          #(in, refs, Some(paragraph), dict.new())
        }
        Some(#(thematic_break, in)), _ -> {
          #(in, refs, Some(thematic_break), dict.new())
        }
      }
    }

    "[^" <> in2 -> {
      case parse_footnote_def(in2, refs, splitters, "^") {
        None -> {
          let #(paragraph, in) = parse_paragraph(in, attrs, splitters)
          #(in, refs, Some(paragraph), dict.new())
        }
        Some(#(id, footnote, refs, in)) -> {
          let refs =
            Refs(..refs, footnotes: dict.insert(refs.footnotes, id, footnote))
          #(in, refs, None, dict.new())
        }
      }
    }

    "[" <> in2 -> {
      case parse_ref_def(in2, "") {
        None -> {
          let #(paragraph, in) = parse_paragraph(in, attrs, splitters)
          #(in, refs, Some(paragraph), dict.new())
        }
        Some(#(id, url, in)) -> {
          let refs = Refs(..refs, urls: dict.insert(refs.urls, id, url))
          #(in, refs, None, dict.new())
        }
      }
    }

    _ -> {
      let #(paragraph, in) = parse_paragraph(in, attrs, splitters)
      #(in, refs, Some(paragraph), dict.new())
    }
  }
}

fn parse_thematic_break(count: Int, in: String) -> Option(#(Container, String)) {
  case in {
    "" | "\n" <> _ if count >= 3 -> Some(#(ThematicBreak, in))
    " " <> rest | "\t" <> rest -> parse_thematic_break(count, rest)
    "-" <> rest | "*" <> rest -> parse_thematic_break(count + 1, rest)
    _ -> None
  }
}

fn parse_codeblock(
  in: String,
  attrs: Dict(String, String),
  delim: String,
  indentation: Int,
  splitters: Splitters,
) -> Option(#(Container, String)) {
  let out = parse_codeblock_start(in, splitters, delim, 1)
  use #(language, count, in) <- option.then(out)
  let #(content, in) =
    parse_codeblock_content(in, delim, count, indentation, "", splitters)
  case language {
    Some("=html") -> Some(#(RawBlock(string.trim_end(content)), in))
    _ -> Some(#(Codeblock(attrs, language, content), in))
  }
}

fn parse_codeblock_start(
  in: String,
  splitters: Splitters,
  delim: String,
  count: Int,
) -> Option(#(Option(String), Int, String)) {
  case in {
    "`" as c <> in | "~" as c <> in if c == delim ->
      parse_codeblock_start(in, splitters, delim, count + 1)

    "\n" <> in if count >= 3 -> Some(#(None, count, in))

    "" -> None
    _non_empty if count >= 3 -> {
      let in = drop_spaces(in)
      use #(language, in) <- option.map(parse_codeblock_language(
        in,
        splitters,
        "",
      ))
      #(language, count, in)
    }

    _ -> None
  }
}

fn parse_codeblock_content(
  in: String,
  delim: String,
  count: Int,
  indentation: Int,
  acc: String,
  splitters: Splitters,
) -> #(String, String) {
  case parse_codeblock_end(in, delim, count) {
    None -> {
      let #(acc, in) = slurp_verbatim_line(in, indentation, acc, splitters)
      parse_codeblock_content(in, delim, count, indentation, acc, splitters)
    }
    Some(in) -> #(acc, in)
  }
}

fn slurp_verbatim_line(
  in: String,
  indentation: Int,
  acc: String,
  splitters: Splitters,
) -> #(String, String) {
  case splitter.split(splitters.verbatim_line_end, in) {
    #(before, "\n", in) -> #(acc <> before <> "\n", in)
    #("", " ", in) if indentation > 0 ->
      slurp_verbatim_line(in, indentation - 1, acc, splitters)
    #(before, split, in) ->
      slurp_verbatim_line(in, indentation, acc <> before <> split, splitters)
  }
}

fn parse_codeblock_end(in: String, delim: String, count: Int) -> Option(String) {
  case in {
    "\n" <> in if count == 0 -> Some(in)
    _ if count == 0 -> Some(in)

    // if the codeblock is indented (ex: in a footnote block), we need to accept an indented end marker
    " " <> in -> parse_codeblock_end(in, delim, count)

    _ ->
      case string.pop_grapheme(in) {
        Ok(#(c, in)) if c == delim -> parse_codeblock_end(in, delim, count - 1)
        Ok(_) -> None
        Error(_) -> Some(in)
      }
  }
}

fn parse_codeblock_language(
  in: String,
  splitters: Splitters,
  language: String,
) -> Option(#(Option(String), String)) {
  case splitter.split(splitters.codeblock_language, in) {
    // A language specifier cannot contain a backtick
    #(_, "`", _) -> None
    #(a, "\n", _) if a == "" && language == "" -> Some(#(None, in))
    #(a, "\n", in) -> Some(#(Some(language <> a), in))
    _ -> Some(#(None, in))
  }
}

fn parse_ref_def(in: String, id: String) -> Option(#(String, String, String)) {
  case in {
    "]:" <> in -> parse_ref_value(in, id, "")
    "" | "]" <> _ | "\n" <> _ -> None
    _ ->
      case string.pop_grapheme(in) {
        Ok(#(c, in)) -> parse_ref_def(in, id <> c)
        Error(_) -> None
      }
  }
}

fn parse_ref_value(
  in: String,
  id: String,
  url: String,
) -> Option(#(String, String, String)) {
  case in {
    "\n " <> in -> parse_ref_value(drop_spaces(in), id, url)
    "\n" <> in -> Some(#(id, string.trim(url), in))
    _ ->
      case string.pop_grapheme(in) {
        Ok(#(c, in)) -> parse_ref_value(in, id, url <> c)
        Error(_) -> Some(#(id, string.trim(url), ""))
      }
  }
}

fn parse_footnote_def(
  in: String,
  refs: Refs,
  splitters: Splitters,
  id: String,
) -> Option(#(String, List(Container), Refs, String)) {
  case in {
    "]:" <> in -> {
      let #(in, spaces_count) = count_drop_spaces(in, 0)
      // Because this is the beginning of the block, we don't have to make sure
      // it is properly indented, so we might be able to skip that process.
      let block_parser = case in {
        // However, if there is a new line directly following the beginning of the block,
        // we need to check the indentation to be sure that it is not an empty block
        "\n" <> _ -> parse_block
        _ -> fn(in, refs, splitters, ast, attrs, required_spaces) {
          parse_block_after_indent_checked(
            in,
            refs,
            splitters,
            ast,
            attrs,
            required_spaces,
            indentation: 4 + string.length(id) + spaces_count,
          )
        }
      }
      let #(block, refs, rest) =
        block_parser(in, refs, splitters, [], dict.new(), 1)
      Some(#(id, block, refs, rest))
    }
    "" | "]" <> _ | "\n" <> _ -> None
    _ ->
      case string.pop_grapheme(in) {
        Ok(#(c, in)) -> parse_footnote_def(in, refs, splitters, id <> c)
        Error(_) -> None
      }
  }
}

fn parse_attributes(
  in: String,
  attrs: Dict(String, String),
) -> Option(#(Dict(String, String), String)) {
  let in = drop_spaces(in)
  case in {
    "" -> None
    "}" <> in -> parse_attributes_end(in, attrs)
    "#" <> in -> {
      case parse_attributes_id_or_class(in, "") {
        Some(#(id, in)) -> parse_attributes(in, add_attribute(attrs, "id", id))
        None -> None
      }
    }
    "." <> in -> {
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

fn parse_attribute(in: String, key: String) -> Option(#(String, String, String)) {
  case in {
    "" | " " <> _ -> None
    "=\"" <> in -> parse_attribute_quoted_value(in, key, "")
    "=" <> in -> parse_attribute_value(in, key, "")
    _ ->
      case string.pop_grapheme(in) {
        Ok(#(c, in)) -> parse_attribute(in, key <> c)
        Error(_) -> None
      }
  }
}

fn parse_attribute_value(
  in: String,
  key: String,
  value: String,
) -> Option(#(String, String, String)) {
  case in {
    "" -> None
    " " <> in -> Some(#(key, value, in))
    "}" <> _ -> Some(#(key, value, in))
    _ ->
      case string.pop_grapheme(in) {
        Ok(#(c, in)) -> parse_attribute_value(in, key, value <> c)
        Error(_) -> None
      }
  }
}

fn parse_attribute_quoted_value(
  in: String,
  key: String,
  value: String,
) -> Option(#(String, String, String)) {
  case in {
    "" -> None
    "\"" <> in -> Some(#(key, value, in))
    _ ->
      case string.pop_grapheme(in) {
        Ok(#(c, in)) -> parse_attribute_quoted_value(in, key, value <> c)
        Error(_) -> None
      }
  }
}

fn parse_attributes_id_or_class(
  in: String,
  id: String,
) -> Option(#(String, String)) {
  case in {
    "" | "}" <> _ | " " <> _ -> Some(#(id, in))
    "#" <> _ | "." <> _ | "=" <> _ -> None
    // TODO: in future this will be permitted as attributes can be over multiple lines
    "\n" <> _ -> None
    _ ->
      case string.pop_grapheme(in) {
        Ok(#(c, in)) -> parse_attributes_id_or_class(in, id <> c)
        Error(_) -> Some(#(id, in))
      }
  }
}

fn parse_attributes_end(
  in: String,
  attrs: Dict(String, String),
) -> Option(#(Dict(String, String), String)) {
  case in {
    "" -> Some(#(attrs, ""))
    "\n" <> in -> Some(#(attrs, in))
    " " <> in -> parse_attributes_end(in, attrs)
    _ -> None
  }
}

fn parse_heading(
  in: String,
  refs: Refs,
  splitters: Splitters,
  attrs: Dict(String, String),
) -> #(Container, Refs, String) {
  case heading_level(in, 1) {
    Some(#(level, in)) -> {
      let in = drop_spaces(in)
      let #(inline_in, in) = take_heading_chars(in, level, "")
      let #(inline, inline_in_remaining) =
        parse_inline(inline_in, splitters, "", [])
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
      #(heading, refs, inline_in_remaining <> in)
    }

    None -> {
      let #(p, in) = parse_paragraph("#" <> in, attrs, splitters)
      #(p, refs, in)
    }
  }
}

fn id_sanitise(content: String) -> String {
  content
  |> string.replace("#", "")
  |> string.replace("?", "")
  |> string.replace("!", "")
  |> string.replace(",", "")
  |> string.trim
  |> string.replace(" ", "-")
  |> string.replace("\n", "-")
}

fn take_heading_chars(in: String, level: Int, acc: String) -> #(String, String) {
  case in {
    "" | "\n" -> #(acc, "")
    "\n\n" <> in -> #(acc, in)
    "\n#" <> rest -> {
      case take_heading_chars_newline_hash(rest, level - 1, acc <> "\n") {
        Some(#(acc, in)) -> take_heading_chars(in, level, acc)
        None -> #(acc, in)
      }
    }
    _ ->
      case string.pop_grapheme(in) {
        Ok(#(c, in)) -> take_heading_chars(in, level, acc <> c)
        Error(_) -> #(acc, "")
      }
  }
}

fn take_heading_chars_newline_hash(
  in: String,
  level: Int,
  acc: String,
) -> Option(#(String, String)) {
  case in {
    _ if level < 0 -> None
    "" if level > 0 -> None

    "" if level == 0 -> Some(#(acc, ""))
    " " <> in if level == 0 -> Some(#(acc, in))

    "#" <> rest -> take_heading_chars_newline_hash(rest, level - 1, acc)

    _ -> None
  }
}

fn parse_inline(
  in: String,
  splitters: Splitters,
  text: String,
  acc: List(Inline),
) -> #(List(Inline), String) {
  case splitter.split(splitters.inline, in) {
    // End of the input
    #(text2, "", "") ->
      case text <> text2 {
        "" -> #(list.reverse(acc), "")
        text -> #(list.reverse([Text(text), ..acc]), "")
      }

    // // Escapes
    #(a, "\\", in) -> {
      let text = text <> a
      case in {
        "!" as e <> in
        | "\"" as e <> in
        | "#" as e <> in
        | "$" as e <> in
        | "%" as e <> in
        | "&" as e <> in
        | "'" as e <> in
        | "(" as e <> in
        | ")" as e <> in
        | "*" as e <> in
        | "+" as e <> in
        | "," as e <> in
        | "-" as e <> in
        | "." as e <> in
        | "/" as e <> in
        | ":" as e <> in
        | ";" as e <> in
        | "<" as e <> in
        | "=" as e <> in
        | ">" as e <> in
        | "?" as e <> in
        | "@" as e <> in
        | "[" as e <> in
        | "\\" as e <> in
        | "]" as e <> in
        | "^" as e <> in
        | "_" as e <> in
        | "`" as e <> in
        | "{" as e <> in
        | "|" as e <> in
        | "}" as e <> in
        | "~" as e <> in -> parse_inline(in, splitters, text <> e, acc)

        "\n" <> in ->
          parse_inline(in, splitters, "", [Linebreak, Text(text), ..acc])

        " " <> in ->
          parse_inline(in, splitters, "", [NonBreakingSpace, Text(text), ..acc])

        _other -> parse_inline(in, splitters, text <> "\\", acc)
      }
    }

    #(a, "_" as start, in) | #(a, "*" as start, in) -> {
      let text = text <> a
      case in {
        " " as b <> in | "\t" as b <> in | "\n" as b <> in ->
          parse_inline(in, splitters, text <> start <> b, acc)
        _ ->
          case parse_emphasis(in, splitters, start) {
            None -> parse_inline(in, splitters, text <> start, acc)
            Some(#(inner, in)) -> {
              let item = case start {
                "*" -> Strong(inner)
                _ -> Emphasis(inner)
              }
              parse_inline(in, splitters, "", [item, Text(text), ..acc])
            }
          }
      }
    }

    #(a, "[^", rest) -> {
      let text = text <> a
      case parse_footnote(rest, "^") {
        None -> parse_inline(rest, splitters, text <> "[^", acc)
        // if this is actually a definition instead of a reference, return early
        // This applies in situations such as the following:
        // ```
        // [^footnote]: very long footnote[^another-footnote]
        // [^another-footnote]: bla bla[^another-footnote]
        // ```
        Some(#(_footnote, ":" <> _)) if text != "" -> #(
          list.reverse([Text(text), ..acc]),
          in,
        )
        Some(#(_footnote, ":" <> _)) -> #(list.reverse(acc), in)
        Some(#(footnote, in)) ->
          parse_inline(in, splitters, "", [footnote, Text(text), ..acc])
      }
    }

    // Link and image
    #(a, "[", in) -> {
      let text = text <> a
      case parse_link(in, splitters, Link) {
        None -> parse_inline(in, splitters, text <> "[", acc)
        Some(#(link, in)) ->
          parse_inline(in, splitters, "", [link, Text(text), ..acc])
      }
    }

    #(a, "![", in) -> {
      let text = text <> a
      case parse_link(in, splitters, Image) {
        None -> parse_inline(in, splitters, text <> "![", acc)
        Some(#(image, in)) ->
          parse_inline(in, splitters, "", [image, Text(text), ..acc])
      }
    }

    // Code
    #(a, "`", in) -> {
      let text = text <> a
      let #(code, in) = parse_code(in, 1)
      parse_inline(in, splitters, "", [code, Text(text), ..acc])
    }

    #(a, "\n", in) -> {
      let text = text <> a
      drop_spaces(in)
      |> parse_inline(splitters, text <> "\n", acc)
    }

    // Math (inline)
    #(a, "$`", in) -> {
      let text = text <> a
      case parse_math(in, splitters, False) {
        None -> parse_inline(in, splitters, text <> "$`", acc)
        Some(#(math, in)) ->
          parse_inline(in, splitters, "", [math, Text(text), ..acc])
      }
    }

    // Math (display)
    #(a, "$$`", in) -> {
      let text = text <> a
      case parse_math(in, splitters, True) {
        None -> parse_inline(in, splitters, text <> "$$`", acc)
        Some(#(math, in)) ->
          parse_inline(in, splitters, "", [math, Text(text), ..acc])
      }
    }

    #(text2, text3, in) ->
      case text <> text2 <> text3 {
        "" -> #(list.reverse(acc), in)
        text -> #(list.reverse([Text(text), ..acc]), in)
      }
  }
}

fn parse_math(
  in: String,
  splitters: Splitters,
  display: Bool,
) -> Option(#(Inline, String)) {
  case splitter.split(splitters.math_end, in) {
    #(_, "", "") -> None
    #(latex, _, rest) -> {
      let math = case display {
        True -> MathDisplay(latex)
        False -> MathInline(latex)
      }

      Some(#(math, rest))
    }
  }
}

fn parse_code(in: String, count: Int) -> #(Inline, String) {
  case in {
    "`" <> in -> parse_code(in, count + 1)
    _ -> {
      let #(content, in) = parse_code_content(in, count, "")

      // If the string has a single space at the end then a backtick we are
      // supposed to not include that space. This is so inline code can start
      // with a backtick.
      let content = case string.starts_with(content, " `") {
        True -> string.trim_start(content)
        False -> content
      }
      let content = case string.ends_with(content, "` ") {
        True -> string.trim_end(content)
        False -> content
      }
      #(Code(content), in)
    }
  }
}

fn parse_code_content(
  in: String,
  count: Int,
  content: String,
) -> #(String, String) {
  case in {
    "" -> #(content, in)
    "`" <> in -> {
      let #(done, content, in) = parse_code_end(in, count, 1, content)
      case done {
        True -> #(content, in)
        False -> parse_code_content(in, count, content)
      }
    }

    _ ->
      case string.pop_grapheme(in) {
        Ok(#(c, in)) -> parse_code_content(in, count, content <> c)
        Error(_) -> #(content, in)
      }
  }
}

fn parse_code_end(
  in: String,
  limit: Int,
  count: Int,
  content: String,
) -> #(Bool, String, String) {
  case in {
    "" -> #(True, content, in)
    "`" <> in -> parse_code_end(in, limit, count + 1, content)
    _ if limit == count -> #(True, content, in)
    _ -> #(False, content <> string.repeat("`", count), in)
  }
}

fn parse_emphasis(
  in: String,
  splitters: Splitters,
  close: String,
) -> Option(#(List(Inline), String)) {
  case take_emphasis_chars(in, close, "") {
    None -> None

    Some(#(inline_in, in)) -> {
      let #(inline, inline_in_remaining) =
        parse_inline(inline_in, splitters, "", [])
      Some(#(inline, inline_in_remaining <> in))
    }
  }
}

fn take_emphasis_chars(
  in: String,
  close: String,
  acc: String,
) -> Option(#(String, String)) {
  case in {
    "" -> None

    // Inline code overrides emphasis
    "`" <> _ -> None

    // The close is not a close if it is preceeded by whitespace
    "\t" as ws <> in | "\n" as ws <> in | " " as ws <> in ->
      case string.pop_grapheme(in) {
        Ok(#(c, in)) if c == close ->
          take_emphasis_chars(in, close, acc <> ws <> c)
        _ -> take_emphasis_chars(in, close, acc <> ws)
      }

    _ ->
      case string.pop_grapheme(in) {
        Ok(#(c, __)) if c == close && acc == "" -> None
        Ok(#(c, in)) if c == close -> Some(#(acc, in))
        Ok(#(c, in)) -> take_emphasis_chars(in, close, acc <> c)
        Error(_) -> None
      }
  }
}

fn parse_link(
  in: String,
  splitters: Splitters,
  to_inline: fn(List(Inline), Destination) -> Inline,
) -> Option(#(Inline, String)) {
  case take_link_chars(in, "", splitters) {
    // This wasn't a link, it was just a `[` in the text
    None -> None

    Some(#(inline_in, ref, in)) -> {
      let #(inline, inline_in_remaining) =
        parse_inline(inline_in, splitters, "", [])
      let ref = case ref {
        Reference("") -> Reference(take_inline_text(inline, ""))
        ref -> ref
      }
      Some(#(to_inline(inline, ref), inline_in_remaining <> in))
    }
  }
}

fn take_link_chars(
  in: String,
  inline_in: String,
  splitters: Splitters,
) -> Option(#(String, Destination, String)) {
  case string.split_once(in, "]") {
    Ok(#(before, "[" <> in)) ->
      take_link_chars_destination(in, False, inline_in <> before, splitters, "")

    Ok(#(before, "(" <> in)) ->
      take_link_chars_destination(in, True, inline_in <> before, splitters, "")

    Ok(#(before, in)) -> take_link_chars(in, inline_in <> before, splitters)

    // This wasn't a link, it was just a `[..]` in the text
    Error(_) -> None
  }
}

fn take_link_chars_destination(
  in: String,
  is_url: Bool,
  inline_in: String,
  splitters: Splitters,
  acc: String,
) -> Option(#(String, Destination, String)) {
  case splitter.split(splitters.link_destination, in) {
    #(a, ")", in) if is_url -> Some(#(inline_in, Url(acc <> a), in))
    #(a, "]", in) if !is_url -> Some(#(inline_in, Reference(acc <> a), in))

    #(a, "\n", rest) if is_url ->
      take_link_chars_destination(rest, is_url, inline_in, splitters, acc <> a)
    #(a, "\n", rest) if !is_url ->
      take_link_chars_destination(
        rest,
        is_url,
        inline_in,
        splitters,
        acc <> a <> " ",
      )

    _ -> None
  }
}

fn parse_footnote(in: String, acc: String) -> Option(#(Inline, String)) {
  case in {
    // This wasn't a footnote, it was just a `[^` in the text
    "" -> None

    "]" <> rest -> {
      Some(#(Footnote(acc), rest))
    }
    _ ->
      case string.pop_grapheme(in) {
        Ok(#(c, rest)) -> parse_footnote(rest, acc <> c)
        // This wasn't a footnote, it was just a `[^` in the text
        Error(_) -> None
      }
  }
}

fn heading_level(in: String, level: Int) -> Option(#(Int, String)) {
  case in {
    "#" <> rest -> heading_level(rest, level + 1)

    "" if level > 0 -> Some(#(level, ""))
    " " <> rest | "\n" <> rest if level != 0 -> Some(#(level, rest))

    _ -> None
  }
}

fn take_inline_text(inlines: List(Inline), acc: String) -> String {
  case inlines {
    [] -> acc
    [first, ..rest] ->
      case first {
        NonBreakingSpace -> take_inline_text(rest, acc <> " ")
        Text(text) | Code(text) | MathInline(text) | MathDisplay(text) ->
          take_inline_text(rest, acc <> text)
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
  in: String,
  attrs: Dict(String, String),
  splitters: Splitters,
) -> #(Container, String) {
  let #(inline_in, in) = take_paragraph_chars(in)

  let #(statements, inline_in_remaining) =
    do_parse_paragraph_statements(inline_in, attrs, splitters, [])
  #(
    Paragraph(
      attrs,
      statements
        |> list.reverse,
    ),
    inline_in_remaining <> in,
  )
}

fn do_parse_paragraph_statements(
  inline_in: String,
  attrs: Dict(String, String),
  splitters,
  statements: List(Statement),
) {
  let #(inline, inline_in_remaining) =
    parse_inline(inline_in, splitters, "", [])
  case inline, inline_in_remaining {
    _, "" | _, "\n" -> #([Statement(inline), ..statements], inline_in_remaining)
    [], _ -> #(statements, inline_in_remaining)
    _, _ ->
      do_parse_paragraph_statements(inline_in_remaining, attrs, splitters, [
        Statement(inline),
        ..statements
      ])
  }
}

fn parse_bullet_list(
  in: String,
  refs: Refs,
  attrs: Dict(String, String),
  style: String,
  layout: ListLayout,
  items: List(List(Container)),
  splitters: Splitters,
) -> #(Container, String) {
  let #(inline_in, in, end) = take_list_item_chars(in, "", style)
  let item = parse_list_item(inline_in, refs, attrs, splitters, [])
  let items = [item, ..items]
  case end {
    True -> #(BulletList(layout:, style:, items: list.reverse(items)), in)
    False -> parse_bullet_list(in, refs, attrs, style, layout, items, splitters)
  }
}

fn parse_list_item(
  in: String,
  refs: Refs,
  attrs: Dict(String, String),
  splitters: Splitters,
  children: List(Container),
) -> List(Container) {
  let #(in, refs, container, attrs) =
    parse_container(in, refs, splitters, attrs, 0)
  let children = case container {
    None -> children
    Some(container) -> [container, ..children]
  }
  case in {
    "" -> list.reverse(children)
    _ -> parse_list_item(in, refs, attrs, splitters, children)
  }
}

fn take_list_item_chars(
  in: String,
  acc: String,
  style: String,
) -> #(String, String, Bool) {
  let #(in, acc) = case string.split_once(in, "\n") {
    Ok(#(content, in)) -> #(in, acc <> content)
    Error(_) -> #("", acc <> in)
  }

  case in {
    " " <> in -> take_list_item_chars(in, acc <> "\n ", style)
    "- " <> in if style == "-" -> #(acc, in, False)
    "\n- " <> in if style == "-" -> #(acc, in, False)
    "* " <> in if style == "*" -> #(acc, in, False)
    "\n* " <> in if style == "*" -> #(acc, in, False)
    _ -> #(acc, in, True)
  }
}

fn take_paragraph_chars(in: String) -> #(String, String) {
  case string.split_once(in, "\n\n") {
    Ok(#(content, in)) -> #(content, in)
    Error(Nil) ->
      case string.ends_with(in, "\n") {
        True -> #(string.drop_end(in, 1), "")
        False -> #(in, "")
      }
  }
}

/// Convert a document tree into a string of HTML.
///
/// See `to_html` for further documentation.
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

  let html_with_footnotes =
    create_footnotes(
      document,
      list.reverse(footnotes_section_html.used_footnotes),
      footnotes_section_html,
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
        Paragraph(attrs, statements) ->
          html
          |> open_tag("p", attrs)
          |> inlines_to_html(statements_to_inlines(statements), refs, TrimLast)
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

    Paragraph(attrs, statements) -> {
      html
      |> open_tag("p", attrs)
      |> inlines_to_html(statements_to_inlines(statements), refs, TrimLast)
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
      |> append_to_html(houdini.escape(content))
      |> close_tag("code")
      |> close_tag("pre")
    }

    Heading(attrs, level, inlines) -> {
      let tag = "h" <> int.to_string(level)
      html
      |> open_tag(tag, attrs)
      |> inlines_to_html(inlines, refs, TrimLast)
      |> close_tag(tag)
    }

    RawBlock(content) -> GeneratedHtml(..html, html: html.html <> content)

    BulletList(layout:, style: _, items:) -> {
      html
      |> open_tag("ul", dict.new())
      |> append_to_html("\n")
      |> list_items_to_html(layout, items, refs)
      |> close_tag("ul")
    }
  }
  append_to_html(new_html, "\n")
}

fn create_footnotes(
  document: Document,
  used_footnotes: List(#(Int, String)),
  html_acc: GeneratedHtml,
) {
  let footnote_to_html = fn(
    html: GeneratedHtml,
    footnote: String,
    footnote_number: String,
  ) {
    dict.get(document.footnotes, footnote)
    |> result.then(fn(footnote) {
      // Even if the footnote is empty, we need to still make sure a backlink is generated
      case list.is_empty(footnote) {
        True -> Error(Nil)
        False -> Ok(footnote)
      }
    })
    |> result.map(fn(footnote) {
      containers_to_html_with_last_paragraph(
        footnote,
        Refs(document.references, document.footnotes),
        html,
        add_footnote_link(_, footnote_number),
      )
    })
    |> result.lazy_unwrap(fn() {
      html
      |> open_tag_ordered_attributes("p", [])
      |> add_footnote_link(footnote_number)
      |> close_tag("p")
    })
  }

  case used_footnotes {
    [] -> html_acc
    [#(footnote_number, footnote), ..other_footnotes] -> {
      let footnote_number = int.to_string(footnote_number)

      let html =
        html_acc
        |> open_tag("li", dict.from_list([#("id", "fn" <> footnote_number)]))
        |> append_to_html("\n")
        |> footnote_to_html(footnote, footnote_number)
        |> append_to_html("\n")
        |> close_tag("li")
        |> append_to_html("\n")

      let new_used_footnotes =
        list.append(get_new_footnotes(html_acc, html, []), other_footnotes)
      create_footnotes(document, new_used_footnotes, html)
    }
  }
}

fn add_footnote_link(html: GeneratedHtml, footnote_number: String) {
  html
  |> open_tag_ordered_attributes("a", [
    #("href", "#fnref" <> footnote_number),
    #("role", "doc-backlink"),
  ])
  |> append_to_html("↩︎")
  |> close_tag("a")
}

fn get_new_footnotes(
  original_html: GeneratedHtml,
  new_html: GeneratedHtml,
  acc: List(#(Int, String)),
) {
  case original_html.used_footnotes, new_html.used_footnotes {
    [original, ..], [new, ..] if original == new -> acc
    _, [new, ..rest] ->
      get_new_footnotes(
        original_html,
        GeneratedHtml(..new_html, used_footnotes: rest),
        [new, ..acc],
      )
    _, _ -> acc
  }
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

type Trim {
  NoTrim
  TrimLast
}

fn list_items_to_html(
  html: GeneratedHtml,
  layout: ListLayout,
  items: List(List(Container)),
  refs: Refs,
) -> GeneratedHtml {
  case items {
    [] -> html

    [[Paragraph(_, statements)], ..rest] if layout == Tight -> {
      html
      |> open_tag("li", dict.new())
      |> append_to_html("\n")
      |> inlines_to_html(statements_to_inlines(statements), refs, TrimLast)
      |> append_to_html("\n")
      |> close_tag("li")
      |> append_to_html("\n")
      |> list_items_to_html(layout, rest, refs)
    }

    [item, ..rest] -> {
      html
      |> open_tag("li", dict.new())
      |> append_to_html("\n")
      |> containers_to_html(item, refs, _)
      |> append_to_html("\n")
      |> close_tag("li")
      |> append_to_html("\n")
      |> list_items_to_html(layout, rest, refs)
    }
  }
}

fn statements_to_inlines(statements: List(Statement)) -> List(Inline) {
  list.map(statements, fn(s: Statement) { s.inlines }) |> list.flatten
}

fn inlines_to_html(
  html: GeneratedHtml,
  inlines: List(Inline),
  refs: Refs,
  trim: Trim,
) -> GeneratedHtml {
  case inlines {
    [] -> html

    [inline] if trim == TrimLast -> {
      html
      |> inline_to_html(inline, refs, trim)
    }

    [inline, ..rest] -> {
      html
      |> inline_to_html(inline, refs, NoTrim)
      |> inlines_to_html(rest, refs, trim)
    }
  }
}

fn inline_to_html(
  html: GeneratedHtml,
  inline: Inline,
  refs: Refs,
  trim: Trim,
) -> GeneratedHtml {
  case inline {
    MathInline(latex) -> {
      let math_class = dict.from_list([#("class", "math inline")])

      let latex = "\\(" <> houdini.escape(latex) <> "\\)"

      html
      |> open_tag("span", math_class)
      |> append_to_html(latex)
      |> close_tag("span")
    }
    MathDisplay(latex) -> {
      let math_class = dict.from_list([#("class", "math display")])

      let latex = "\\[" <> houdini.escape(latex) <> "\\]"

      html
      |> open_tag("span", math_class)
      |> append_to_html(latex)
      |> close_tag("span")
    }
    NonBreakingSpace -> {
      html |> append_to_html("&nbsp;")
    }
    Linebreak -> {
      html
      |> open_tag("br", dict.new())
      |> append_to_html("\n")
    }
    Text(text) -> {
      let text = houdini.escape(text)
      case trim {
        NoTrim -> append_to_html(html, text)
        TrimLast -> append_to_html(html, string.trim_end(text))
      }
    }
    Strong(inlines) -> {
      html
      |> open_tag("strong", dict.new())
      |> inlines_to_html(inlines, refs, trim)
      |> close_tag("strong")
    }
    Emphasis(inlines) -> {
      html
      |> open_tag("em", dict.new())
      |> inlines_to_html(inlines, refs, trim)
      |> close_tag("em")
    }
    Link(text, destination) -> {
      html
      |> open_tag("a", destination_attribute("href", destination, refs))
      |> inlines_to_html(text, refs, trim)
      |> close_tag("a")
    }
    Image(text, destination) -> {
      html
      |> open_tag(
        "img",
        destination_attribute("src", destination, refs)
          |> dict.insert("alt", houdini.escape(take_inline_text(text, ""))),
      )
    }
    Code(content) -> {
      let content = houdini.escape(content)
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
    Url(url) -> dict.insert(dict, key, houdini.escape(url))
    Reference(id) ->
      case dict.get(refs.urls, id) {
        Ok(url) -> dict.insert(dict, key, houdini.escape(url))
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
