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
    reference_attributes: Dict(String, Dict(String, String)),
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
  Paragraph(attributes: Dict(String, String), content: List(Inline))
  Heading(attributes: Dict(String, String), level: Int, content: List(Inline))
  Codeblock(
    attributes: Dict(String, String),
    language: Option(String),
    content: String,
  )
  RawBlock(content: String)
  BulletList(
    layout: ListLayout,
    style: BulletStyle,
    items: List(List(Container)),
  )
  OrderedList(
    layout: ListLayout,
    punctuation: OrdinalPunctuation,
    ordinal: OrdinalStyle,
    start: Int,
    items: List(List(Container)),
  )
  BlockQuote(attributes: Dict(String, String), items: List(Container))
  Div(attributes: Dict(String, String), items: List(Container))
}

pub type BulletStyle {
  BulletDash
  BulletStar
  BulletPlus
}

pub type OrdinalPunctuation {
  FullStop
  SingleParen
  DoubleParen
}

pub type OrdinalStyle {
  NumericOrdinal
  LowerAlphaOrdinal
  UpperAlphaOrdinal
}

type ListStyle {
  Bullet(BulletStyle)
  Ordered(start: Int, punctuation: OrdinalPunctuation, style: OrdinalStyle)
}

pub type Inline {
  Linebreak
  NonBreakingSpace
  Text(String)
  Link(
    attributes: Dict(String, String),
    content: List(Inline),
    destination: Destination,
  )
  Image(
    attributes: Dict(String, String),
    content: List(Inline),
    destination: Destination,
  )
  Span(attributes: Dict(String, String), content: List(Inline))
  Emphasis(content: List(Inline))
  Strong(content: List(Inline))
  Delete(content: List(Inline))
  Insert(content: List(Inline))
  Mark(content: List(Inline))
  Footnote(reference: String)
  Code(content: String)
  MathInline(content: String)
  MathDisplay(content: String)
  Symbol(content: String)
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
  Refs(
    urls: Dict(String, String),
    url_attributes: Dict(String, Dict(String, String)),
    headings: Dict(String, Int),
    footnotes: Dict(String, List(Container)),
  )
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
        "\\", "_", "*", "[^", "[", "![", "$$`", "$`", "`", "\n", "--", "...",
        "<", "{-", "{+", "{=", "{", ":",
      ]),
      link_destination: splitter.new([")", "]", "\n"]),
      math_end: splitter.new(["`"]),
    )
  let refs = Refs(dict.new(), dict.new(), dict.new(), dict.new())

  let #(ast, Refs(urls:, url_attributes:, footnotes:, headings:), _) =
    djot
    |> string.replace("\r\n", "\n")
    |> parse_document_content(refs, splitters, [], dict.new())

  let urls =
    dict.fold(headings, urls, fn(urls, id, count) {
      int_fold_down_zero_inclusive(count, urls, fn(urls, i) {
        let key = case i {
          0 -> id
          _ -> id <> "-" <> int.to_string(i)
        }
        case dict.has_key(urls, key) {
          True -> urls
          False -> dict.insert(urls, key, "#" <> key)
        }
      })
    })

  Document(ast, urls, url_attributes, footnotes)
}

fn int_fold_down_zero_inclusive(
  int: Int,
  acc: acc,
  reduce: fn(acc, Int) -> acc,
) -> acc {
  case int < 0 {
    True -> acc
    False -> int_fold_down_zero_inclusive(int - 1, reduce(acc, int), reduce)
  }
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

fn count_drop_hyphens(in: String, count: Int) -> #(Int, String) {
  case in {
    "-" <> rest -> count_drop_hyphens(rest, count + 1)
    _ -> #(count, in)
  }
}

/// Given the length of a sequence of `-` this turns it in a series of em/en
/// dashes.
fn dash_sequence(hyphens: Int) -> String {
  case hyphens % 3, hyphens % 2 {
    0, _ -> string.repeat("—", hyphens / 3)
    _, 0 -> string.repeat("–", hyphens / 2)
    _, _ -> {
      // Thank you Yoshie for figuring this out!!
      let ems = int.max(0, { hyphens - 2 } / 3)
      let hyphens = hyphens - ems * 3

      string.repeat("—", ems)
      <> string.repeat("–", hyphens / 2)
      <> string.repeat("-", hyphens % 2)
    }
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
    parse_container(in, refs, splitters, attrs, spaces_count, None)
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
        parse_container(in, refs, splitters, attrs, indentation, None)
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
    parse_container(in, refs, splitters, attrs, indentation, None)
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
  div_close_size: Option(Int),
) -> #(String, Refs, Option(Container), Dict(String, String)) {
  case in {
    "" -> #(in, refs, None, dict.new())

    "{" <> in2 -> {
      case parse_attributes(in2, attrs) {
        None -> {
          let #(paragraph, in) =
            parse_paragraph(in, attrs, splitters, div_close_size)
          #(in, refs, Some(paragraph), dict.new())
        }
        Some(#(attrs, in)) -> #(in, refs, None, attrs)
      }
    }

    "#" <> in -> {
      let #(heading, refs, in) =
        parse_heading(in, refs, splitters, attrs, div_close_size)
      #(in, refs, Some(heading), dict.new())
    }

    "~" as delim <> in2 | "`" as delim <> in2 -> {
      case parse_codeblock(in2, attrs, delim, indentation, splitters) {
        None -> {
          let #(paragraph, in) =
            parse_paragraph(in, attrs, splitters, div_close_size)
          #(in, refs, Some(paragraph), dict.new())
        }
        Some(#(codeblock, in)) -> #(in, refs, Some(codeblock), dict.new())
      }
    }

    "> " <> _ | ">\n" <> _ -> {
      let #(block_quote, in) =
        parse_block_quote(in, refs, attrs, splitters, div_close_size)
      #(in, refs, Some(block_quote), dict.new())
    }

    "-" as style <> in2 | "*" as style <> in2 | "+" as style <> in2 -> {
      case parse_thematic_break(1, in2), in2 {
        None, " " <> in2 | None, "\n" <> in2 -> {
          let bullet_style = case style {
            "-" -> BulletDash
            "*" -> BulletStar
            _ -> BulletPlus
          }
          let style = Bullet(bullet_style)
          let #(list, in) =
            parse_list(in2, refs, attrs, style, Tight, [], splitters)
          #(in, refs, Some(list), dict.new())
        }
        None, _ -> {
          let #(paragraph, in) =
            parse_paragraph(in, attrs, splitters, div_close_size)
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
          let #(paragraph, in) =
            parse_paragraph(in, attrs, splitters, div_close_size)
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
          let #(paragraph, in) =
            parse_paragraph(in, attrs, splitters, div_close_size)
          #(in, refs, Some(paragraph), dict.new())
        }
        Some(#(id, url, in)) -> {
          let url_attributes = case dict.is_empty(attrs) {
            True -> refs.url_attributes
            False -> dict.insert(refs.url_attributes, id, attrs)
          }
          let urls = dict.insert(refs.urls, id, url)
          let refs = Refs(..refs, urls:, url_attributes:)
          #(in, refs, None, dict.new())
        }
      }
    }

    ":::" <> in2 -> {
      case parse_div(in2, refs, attrs, splitters) {
        None -> {
          let #(paragraph, in) =
            parse_paragraph(in, attrs, splitters, div_close_size)
          #(in, refs, Some(paragraph), dict.new())
        }
        Some(#(in, attrs, content)) -> {
          let div = Some(Div(attrs, content))
          #(in, refs, div, dict.new())
        }
      }
    }

    "(" <> rest -> {
      case parse_maybe_list(rest, refs, attrs, splitters, True) {
        Some(#(in, refs, list)) -> #(in, refs, Some(list), dict.new())
        None -> {
          let #(paragraph, in) =
            parse_paragraph(in, attrs, splitters, div_close_size)
          #(in, refs, Some(paragraph), dict.new())
        }
      }
    }

    _ -> {
      case parse_maybe_list(in, refs, attrs, splitters, False) {
        Some(#(in, refs, list)) -> #(in, refs, Some(list), dict.new())
        None -> {
          let #(paragraph, in) =
            parse_paragraph(in, attrs, splitters, div_close_size)
          #(in, refs, Some(paragraph), dict.new())
        }
      }
    }
  }
}

fn parse_maybe_list(
  in: String,
  refs: Refs,
  attrs: Dict(String, String),
  splitters: Splitters,
  paren: Bool,
) -> Option(#(String, Refs, Container)) {
  case in {
    "0" <> _
    | "1" <> _
    | "2" <> _
    | "3" <> _
    | "4" <> _
    | "5" <> _
    | "6" <> _
    | "7" <> _
    | "8" <> _
    | "9" <> _ -> {
      case parse_number_list(in, 0, paren) {
        Some(#(punctuation, style, start, in)) -> {
          let style = Ordered(start:, punctuation:, style:)
          let #(list, in) =
            parse_list(in, refs, attrs, style, Tight, [], splitters)
          Some(#(in, refs, list))
        }
        None -> None
      }
    }

    "a" <> _
    | "b" <> _
    | "c" <> _
    | "d" <> _
    | "e" <> _
    | "f" <> _
    | "g" <> _
    | "h" <> _
    | "i" <> _
    | "j" <> _
    | "k" <> _
    | "l" <> _
    | "m" <> _
    | "n" <> _
    | "o" <> _
    | "p" <> _
    | "q" <> _
    | "r" <> _
    | "s" <> _
    | "t" <> _
    | "u" <> _
    | "v" <> _
    | "w" <> _
    | "x" <> _
    | "y" <> _
    | "z" <> _ ->
      case parse_lower_list(in, 0, paren) {
        Some(#(punctuation, style, start, in)) -> {
          let style = Ordered(start:, punctuation:, style:)
          let #(list, in) =
            parse_list(in, refs, attrs, style, Tight, [], splitters)
          Some(#(in, refs, list))
        }
        None -> None
      }

    "A" <> _
    | "B" <> _
    | "C" <> _
    | "D" <> _
    | "E" <> _
    | "F" <> _
    | "G" <> _
    | "H" <> _
    | "I" <> _
    | "J" <> _
    | "K" <> _
    | "L" <> _
    | "M" <> _
    | "N" <> _
    | "O" <> _
    | "P" <> _
    | "Q" <> _
    | "R" <> _
    | "S" <> _
    | "T" <> _
    | "U" <> _
    | "V" <> _
    | "W" <> _
    | "X" <> _
    | "Y" <> _
    | "Z" <> _ ->
      case parse_upper_list(in, 0, paren) {
        Some(#(punctuation, style, start, in)) -> {
          let style = Ordered(start:, punctuation:, style:)
          let #(list, in) =
            parse_list(in, refs, attrs, style, Tight, [], splitters)
          Some(#(in, refs, list))
        }
        None -> None
      }

    _ -> None
  }
}

/// Parse a div.
fn parse_div(
  in: String,
  refs: Refs,
  attrs: Dict(String, String),
  splitters: Splitters,
) -> _ {
  let #(size, in2) = count_div_fence_size(in, 3)
  let class = parse_div_class(in2)

  use #(class, rest) <- option.then(class)
  let attrs = case class {
    "" -> attrs
    class -> add_attribute(attrs, "class", class)
  }
  let #(rest, content) =
    parse_div_content(rest, refs, dict.new(), size, splitters, [])
  Some(#(rest, attrs, content))
}

fn parse_div_content(
  in: String,
  refs: Refs,
  attrs: Dict(String, String),
  fence_size: Int,
  splitters: Splitters,
  children: List(Container),
) -> #(String, List(Container)) {
  case check_first_line_suitable_div_end(in, fence_size) {
    Some(in2) -> {
      #(in2, list.reverse(children))
    }
    None -> {
      let #(in, refs, container, attrs) =
        parse_container(in, refs, splitters, attrs, 0, Some(fence_size))

      let children = case container {
        None -> children
        Some(container) -> [container, ..children]
      }

      case in {
        "" -> #(in, list.reverse(children))
        _ -> parse_div_content(in, refs, attrs, fence_size, splitters, children)
      }
    }
  }
}

/// Checks if current line is a suitable terminator for
/// a div fence of a particular size.
///
/// Returns the rest of the input if it is.
fn check_first_line_suitable_div_end(
  in: String,
  fence_size: Int,
) -> Option(String) {
  let #(line, rest) = slurp_to_line_end(in)

  case check_line_suitable_div_end(line, fence_size) {
    False -> None
    True -> Some(rest)
  }
}

fn check_line_suitable_div_end(line: String, fence_size: Int) -> Bool {
  let candidate_fence_size =
    line
    |> string.trim
    |> count_div_terminator_fence_size(0)

  case candidate_fence_size {
    Some(candidate_fence_size) -> candidate_fence_size >= fence_size
    None -> False
  }
}

/// Counts the size of a div fence. Used to count pretrimmed lines which may
/// contain a valid terminating fence. Valid pretrimmed fences contain only
/// colons `:`.
///
/// Returns Some(`size`) for a valid fence and None for an invalid fence.
fn count_div_terminator_fence_size(line: String, count: Int) -> Option(Int) {
  case line {
    "" -> Some(count)
    ":" <> rest -> count_div_terminator_fence_size(rest, count + 1)
    _ -> None
  }
}

/// Counts the size of a div fence. Used in initial parsing of a div once a
/// minimum fence structure has been seen: `:::`.
///
/// Returns the size of the fence seen with the remainder of the unused input
/// stream.
fn count_div_fence_size(in: String, count: Int) -> #(Int, String) {
  case in {
    ":" <> rest -> count_div_fence_size(rest, count + 1)
    _ -> #(count, in)
  }
}

/// Parse the class name for a div. Returns Some if a classname is present.
/// Returns Some if no classname is present. Returns None if the text is an
/// invalid classname.
fn parse_div_class(in: String) -> Option(#(String, String)) {
  let #(line, rest) = slurp_to_line_end(in)
  let line = string.trim(line)

  let has_prohibited = list.any([" ", "\t"], string.contains(line, _))

  case has_prohibited {
    False -> Some(#(line, rest))
    True -> None
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
    _ -> Some(#(attrs, in))
  }
}

fn parse_block_quote(
  in: String,
  refs: Refs,
  attrs: Dict(String, String),
  splitters: Splitters,
  div_close_size: Option(Int),
) -> #(Container, String) {
  let #(reversed_lines, in) = take_block_quote_chars(in, [], div_close_size)
  let items = case list.reverse(reversed_lines) {
    [] -> []
    lines -> {
      let content = string.join(lines, "\n")
      parse_block_quote_items(content, refs, dict.new(), splitters, [])
    }
  }

  #(BlockQuote(attrs, items), in)
}

fn take_block_quote_chars(
  in: String,
  lines: List(String),
  div_close_size: Option(Int),
) -> #(List(String), String) {
  case in {
    // An empty line marks the end of the block quote.
    "\n" <> in -> #(lines, in)
    ">" -> #(["", ..lines], "")
    ">\n" <> in ->
      case lines {
        // Empty lines at the beginning of the block quote are ignored.
        [] -> take_block_quote_chars(in, [], div_close_size)
        _ -> take_block_quote_chars(in, ["", ..lines], div_close_size)
      }

    "> " <> in -> {
      case string.split_once(in, "\n") {
        Ok(#(line, in)) ->
          take_block_quote_chars(in, [line, ..lines], div_close_size)
        Error(_) -> #([in, ..lines], "")
      }
    }
    in -> take_block_quote_stop_on_div_close(in, lines, div_close_size)
  }
}

fn take_block_quote_stop_on_div_close(
  in: String,
  lines: List(String),
  div_close_size: Option(Int),
) {
  let #(line, rest) = slurp_to_line_end(in)

  case div_close_size {
    None ->
      case rest {
        "" -> #([line, ..lines], "")
        _ -> take_block_quote_chars(in, [line, ..lines], div_close_size)
      }
    Some(size) ->
      case check_line_suitable_div_end(line, size) {
        True -> #(lines, in)
        False ->
          case rest {
            "" -> #([line, ..lines], "")
            _ -> take_block_quote_chars(in, [line, ..lines], div_close_size)
          }
      }
  }
}

fn parse_block_quote_items(
  in: String,
  refs: Refs,
  attrs: Dict(String, String),
  splitters: Splitters,
  children: List(Container),
) -> List(Container) {
  let #(in, refs, container, attrs) =
    parse_container(in, refs, splitters, attrs, 0, None)
  let children = case container {
    None -> children
    Some(container) -> [container, ..children]
  }
  case in {
    "" -> list.reverse(children)
    _ -> parse_block_quote_items(in, refs, attrs, splitters, children)
  }
}

fn parse_heading(
  in: String,
  refs: Refs,
  splitters: Splitters,
  attrs: Dict(String, String),
  div_close_size: Option(Int),
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
          case dict.get(refs.headings, id) {
            // This is not the first heading seen with this content. We must
            // add a suffix to the id to make it unique.
            Ok(i) -> {
              let i = i + 1
              let refs =
                Refs(..refs, headings: dict.insert(refs.headings, id, i))
              let id = id <> "-" <> int.to_string(i)
              let attrs = add_attribute(attrs, "id", id)
              #(refs, attrs)
            }

            // This is the first heading seen with this content
            Error(_) -> {
              let refs =
                Refs(..refs, headings: dict.insert(refs.headings, id, 0))
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
      let #(p, in) =
        parse_paragraph("#" <> in, attrs, splitters, div_close_size)
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

    #(before, "...", in) -> {
      let text = text <> before <> "…"
      parse_inline(in, splitters, text, acc)
    }

    #(before, "--", in) -> {
      let #(count, in) = count_drop_hyphens(in, 2)
      let text = text <> before <> dash_sequence(count)
      parse_inline(in, splitters, text, acc)
    }

    // // Escapes
    #(before, "\\", in) -> {
      let text = text <> before
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
      case parse_link_or_recover(in, splitters, Link, "[") {
        Error(#(failed_text, remaining)) ->
          parse_inline(remaining, splitters, text <> failed_text, acc)
        Ok(#(link, remaining)) ->
          parse_inline(remaining, splitters, "", [link, Text(text), ..acc])
      }
    }

    #(a, "![", in) -> {
      let text = text <> a
      case parse_link_or_recover(in, splitters, Image, "![") {
        Error(#(failed_text, remaining)) ->
          parse_inline(remaining, splitters, text <> failed_text, acc)
        Ok(#(image, remaining)) ->
          parse_inline(remaining, splitters, "", [image, Text(text), ..acc])
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

    // Autolinks
    #(a, "<", in) -> {
      let text = text <> a
      case parse_autolink(in) {
        None -> parse_inline(in, splitters, text <> "<", acc)
        Some(#(link, in)) ->
          parse_inline(in, splitters, "", [link, Text(text), ..acc])
      }
    }

    // Delete
    #(a, "{-", in) -> {
      let text = text <> a
      case parse_insert_delete_mark(in, splitters, "-}") {
        None -> parse_inline(in, splitters, text <> "{-", acc)
        Some(#(inner, in)) ->
          parse_inline(in, splitters, "", [Delete(inner), Text(text), ..acc])
      }
    }

    // Insert
    #(a, "{+", in) -> {
      let text = text <> a
      case parse_insert_delete_mark(in, splitters, "+}") {
        None -> parse_inline(in, splitters, text <> "{+", acc)
        Some(#(inner, in)) ->
          parse_inline(in, splitters, "", [Insert(inner), Text(text), ..acc])
      }
    }

    // Mark
    #(a, "{=", in) -> {
      let text = text <> a
      case parse_insert_delete_mark(in, splitters, "=}") {
        None -> parse_inline(in, splitters, text <> "{=", acc)
        Some(#(inner, in)) ->
          parse_inline(in, splitters, "", [Mark(inner), Text(text), ..acc])
      }
    }

    // Standalone attributes (they are discarded)
    #(a, "{", in) -> {
      let text = text <> a
      case parse_attributes(in, dict.new()) {
        None -> parse_inline(in, splitters, text <> "{", acc)
        Some(#(_attrs, in)) ->
          parse_inline(in, splitters, "", [Text(text), ..acc])
      }
    }

    // Symbols
    #(a, ":", in) -> {
      let text = text <> a
      case parse_symbol(in) {
        None -> parse_inline(in, splitters, text <> ":", acc)
        Some(#(symbol, in)) ->
          parse_inline(in, splitters, "", [symbol, Text(text), ..acc])
      }
    }

    #(text2, text3, in) ->
      case text <> text2 <> text3 {
        "" -> #(list.reverse(acc), in)
        text -> #(list.reverse([Text(text), ..acc]), in)
      }
  }
}

fn parse_autolink(in: String) -> Option(#(Inline, String)) {
  case string.split_once(in, ">") {
    Error(_) -> None
    Ok(#(url, rest)) -> {
      // Check if it looks like an email or URL
      case string.contains(url, "@") {
        True -> {
          // Email autolink
          let href = "mailto:" <> url
          Some(#(Link(dict.new(), [Text(url)], Url(href)), rest))
        }
        False -> {
          // URL autolink - check if it has a scheme
          case string.contains(url, "://") || string.starts_with(url, "//") {
            True -> Some(#(Link(dict.new(), [Text(url)], Url(url)), rest))
            False -> None
          }
        }
      }
    }
  }
}

fn parse_symbol(in: String) -> Option(#(Inline, String)) {
  case take_symbol_chars(in, "") {
    Some(#(text, rest)) -> Some(#(Symbol(text), rest))
    _ -> None
  }
}

// The Djot syntax reference says "Surrounding a word with : signs creates
// a “symbol,”  which by default is just rendered literally. "a word" is
// not defined in that document. Per djoths, a symbol's name matches
// the regexp [-_+a-zA-Z09]+, see:
// https://github.com/jgm/djoths/blob/83dbadd6aa325ff23f0e4144221b0df2c64becc7/src/Djot/Inlines.hs#L213-L221
fn take_symbol_chars(in: String, acc: String) -> Option(#(String, String)) {
  case in {
    "" -> None
    ":" <> _ if acc == "" -> None
    ":" <> rest -> Some(#(acc, rest))
    "a" as c <> rest
    | "b" as c <> rest
    | "c" as c <> rest
    | "d" as c <> rest
    | "e" as c <> rest
    | "f" as c <> rest
    | "g" as c <> rest
    | "h" as c <> rest
    | "i" as c <> rest
    | "j" as c <> rest
    | "k" as c <> rest
    | "l" as c <> rest
    | "m" as c <> rest
    | "n" as c <> rest
    | "o" as c <> rest
    | "p" as c <> rest
    | "q" as c <> rest
    | "r" as c <> rest
    | "s" as c <> rest
    | "t" as c <> rest
    | "u" as c <> rest
    | "v" as c <> rest
    | "w" as c <> rest
    | "x" as c <> rest
    | "y" as c <> rest
    | "z" as c <> rest
    | "A" as c <> rest
    | "B" as c <> rest
    | "C" as c <> rest
    | "D" as c <> rest
    | "E" as c <> rest
    | "F" as c <> rest
    | "G" as c <> rest
    | "H" as c <> rest
    | "I" as c <> rest
    | "J" as c <> rest
    | "K" as c <> rest
    | "L" as c <> rest
    | "M" as c <> rest
    | "N" as c <> rest
    | "O" as c <> rest
    | "P" as c <> rest
    | "Q" as c <> rest
    | "R" as c <> rest
    | "S" as c <> rest
    | "T" as c <> rest
    | "U" as c <> rest
    | "V" as c <> rest
    | "W" as c <> rest
    | "X" as c <> rest
    | "Y" as c <> rest
    | "Z" as c <> rest
    | "0" as c <> rest
    | "1" as c <> rest
    | "2" as c <> rest
    | "3" as c <> rest
    | "4" as c <> rest
    | "5" as c <> rest
    | "6" as c <> rest
    | "7" as c <> rest
    | "8" as c <> rest
    | "9" as c <> rest
    | "_" as c <> rest
    | "-" as c <> rest
    | "+" as c <> rest -> take_symbol_chars(rest, acc <> c)
    _ -> None
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

fn parse_insert_delete_mark(
  in: String,
  splitters: Splitters,
  close: String,
) -> Option(#(List(Inline), String)) {
  case string.split_once(in, close) {
    Error(_) -> None
    Ok(#(inline_in, rest)) -> {
      let #(inline, inline_in_remaining) =
        parse_inline(inline_in, splitters, "", [])
      Some(#(inline, inline_in_remaining <> rest))
    }
  }
}

fn parse_link_or_recover(
  in: String,
  splitters: Splitters,
  to_inline: fn(Dict(String, String), List(Inline), Destination) -> Inline,
  opening: String,
) -> Result(#(Inline, String), #(String, String)) {
  case parse_link(in, splitters, to_inline) {
    Some(#(inline, remaining)) -> Ok(#(inline, remaining))
    None -> {
      // Link parsing failed - consume until we find a clear boundary
      // to avoid partial parsing of emphasis markers inside
      let #(consumed, remaining) = consume_until_space_or_newline(in, "")
      Error(#(opening <> consumed, remaining))
    }
  }
}

fn consume_until_space_or_newline(in: String, acc: String) -> #(String, String) {
  case in {
    "" -> #(acc, "")
    " " <> _ -> #(acc, in)
    "\n" <> _ -> #(acc, in)
    _ ->
      case string.pop_grapheme(in) {
        Ok(#(c, rest)) -> consume_until_space_or_newline(rest, acc <> c)
        Error(_) -> #(acc, "")
      }
  }
}

fn parse_link(
  in: String,
  splitters: Splitters,
  to_inline: fn(Dict(String, String), List(Inline), Destination) -> Inline,
) -> Option(#(Inline, String)) {
  case take_link_chars_or_span(in, "", splitters) {
    // This wasn't a link/span, it was just a `[` in the text
    None -> None

    // Span with attributes [text]{attrs}
    Some(#(inline_in, None, in)) -> {
      let #(inline, inline_in_remaining) =
        parse_inline(inline_in, splitters, "", [])
      // Check for attributes after ]
      case in {
        "{" <> rest ->
          case parse_attributes(rest, dict.new()) {
            Some(#(attrs, in)) ->
              Some(#(Span(attrs, inline), inline_in_remaining <> in))
            None -> None
          }
        _ -> None
      }
    }

    // Link/Image with destination
    Some(#(inline_in, Some(ref), in)) -> {
      let #(inline, inline_in_remaining) =
        parse_inline(inline_in, splitters, "", [])
      let ref = case ref {
        Reference("") -> Reference(take_inline_text(inline, ""))
        ref -> ref
      }
      // Check for attributes after the link destination
      let #(attrs, in) = case in {
        "{" <> rest ->
          case parse_attributes(rest, dict.new()) {
            Some(#(attrs, in)) -> #(attrs, in)
            None -> #(dict.new(), in)
          }
        _ -> #(dict.new(), in)
      }
      Some(#(to_inline(attrs, inline, ref), inline_in_remaining <> in))
    }
  }
}

fn take_link_chars_or_span(
  in: String,
  inline_in: String,
  splitters: Splitters,
) -> Option(#(String, Option(Destination), String)) {
  take_link_chars_or_span_depth(in, inline_in, splitters, 0)
}

fn take_link_chars_or_span_depth(
  in: String,
  inline_in: String,
  splitters: Splitters,
  depth: Int,
) -> Option(#(String, Option(Destination), String)) {
  case in {
    "" -> None

    "![" <> rest ->
      // Nested image - increase depth
      take_link_chars_or_span_depth(
        rest,
        inline_in <> "![",
        splitters,
        depth + 1,
      )

    "[" <> rest ->
      // Nested opening bracket - increase depth
      take_link_chars_or_span_depth(
        rest,
        inline_in <> "[",
        splitters,
        depth + 1,
      )

    "]" <> rest if depth > 0 ->
      // This ] closes a nested bracket
      take_link_chars_or_span_depth(
        rest,
        inline_in <> "]",
        splitters,
        depth - 1,
      )

    "][" <> rest if depth == 0 -> {
      // This is ][  - reference link
      case take_link_chars_destination(rest, False, inline_in, splitters, "") {
        Some(#(inline_in, dest, in)) -> Some(#(inline_in, Some(dest), in))
        None -> None
      }
    }

    "](" <> rest if depth == 0 -> {
      // This is ](  - inline link
      case take_link_chars_destination(rest, True, inline_in, splitters, "") {
        Some(#(inline_in, dest, in)) -> Some(#(inline_in, Some(dest), in))
        None -> None
      }
    }

    "]{" <> rest if depth == 0 ->
      // This is ]{  - span with attributes
      Some(#(inline_in, None, "{" <> rest))

    "]" <> _ if depth == 0 ->
      // Just ] without continuation - not a link
      None

    _ -> {
      // Consume one character and continue
      case string.pop_grapheme(in) {
        Ok(#(c, rest)) ->
          take_link_chars_or_span_depth(rest, inline_in <> c, splitters, depth)
        Error(_) -> None
      }
    }
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
        Text(text)
        | Code(text)
        | MathInline(text)
        | MathDisplay(text)
        | Symbol(text) -> take_inline_text(rest, acc <> text)
        Strong(inlines)
        | Emphasis(inlines)
        | Delete(inlines)
        | Insert(inlines)
        | Mark(inlines) -> take_inline_text(list.append(inlines, rest), acc)
        Link(_, nested, _) | Image(_, nested, _) | Span(_, nested) -> {
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
  div_close_size: Option(Int),
) -> #(Container, String) {
  let #(inline_in, in) = take_paragraph_chars(in, div_close_size)
  let #(inline, inline_in_remaining) =
    parse_inline(inline_in, splitters, "", [])
  #(Paragraph(attrs, inline), inline_in_remaining <> in)
}

fn parse_list(
  in: String,
  refs: Refs,
  attrs: Dict(String, String),
  style: ListStyle,
  layout: ListLayout,
  items: List(List(Container)),
  splitters: Splitters,
) -> #(Container, String) {
  let #(inline_in, in, layout) = take_list_item_chars(in, "", style, layout)
  let item = parse_list_item(inline_in, refs, attrs, splitters, [])
  let items = [item, ..items]
  case continue_list(in, style) {
    Some(in) -> parse_list(in, refs, attrs, style, layout, items, splitters)
    None -> {
      let items = list.reverse(items)
      let container = case style {
        Bullet(style) -> BulletList(layout:, style:, items:)
        Ordered(start:, punctuation:, style: ordinal) ->
          OrderedList(layout:, punctuation:, ordinal:, start:, items:)
      }
      #(container, in)
    }
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
    parse_container(in, refs, splitters, attrs, 0, None)
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
  style: ListStyle,
  layout: ListLayout,
) -> #(String, String, ListLayout) {
  let #(line, in) = case string.split_once(in, "\n") {
    Ok(split) -> split
    Error(_) -> #(in, "")
  }
  let acc = acc <> line

  case in {
    "" -> #(acc, "", layout)
    " " <> _ -> take_list_item_chars(in, acc <> "\n", style, layout)

    // A blank line followed by indented content, meaning this is
    // content for the current list item.
    "\n " <> rest -> {
      let #(rest, indent) = count_drop_spaces(rest, 1)
      let layout = case parse_list_marker(rest) {
        Some(_) -> layout
        None -> Loose
      }
      let acc = acc <> "\n\n"
      take_list_item_chars_indented(rest, acc, style, layout, indent)
    }

    // A blank line followed by un-indented content, so the end of this
    // current list item.
    "\n" <> in -> {
      let layout = case continue_list(in, style) {
        Some(_) -> Loose
        None -> layout
      }
      #(acc, in, layout)
    }

    _ -> {
      case parse_list_marker(in) {
        Some(_) -> #(acc, in, layout)
        None -> take_list_item_chars(in, acc <> "\n", style, layout)
      }
    }
  }
}

fn parse_list_marker(in: String) -> Option(#(ListStyle, String)) {
  case in {
    "- " <> in | "-\n" <> in -> Some(#(Bullet(BulletDash), in))
    "* " <> in | "*\n" <> in -> Some(#(Bullet(BulletStar), in))
    "+ " <> in | "+\n" <> in -> Some(#(Bullet(BulletPlus), in))
    "(" <> in -> parse_list_marker_maybe_paren(in, True)
    _ -> parse_list_marker_maybe_paren(in, False)
  }
}

fn parse_list_marker_maybe_paren(
  in: String,
  paren: Bool,
) -> Option(#(ListStyle, String)) {
  case in {
    "0" <> _
    | "1" <> _
    | "2" <> _
    | "3" <> _
    | "4" <> _
    | "5" <> _
    | "6" <> _
    | "7" <> _
    | "8" <> _
    | "9" <> _ ->
      case parse_number_list(in, 0, paren) {
        Some(#(punctuation, style, start, in)) ->
          Some(#(Ordered(start:, style:, punctuation:), in))
        None -> None
      }

    "a" <> _
    | "b" <> _
    | "c" <> _
    | "d" <> _
    | "e" <> _
    | "f" <> _
    | "g" <> _
    | "h" <> _
    | "i" <> _
    | "j" <> _
    | "k" <> _
    | "l" <> _
    | "m" <> _
    | "n" <> _
    | "o" <> _
    | "p" <> _
    | "q" <> _
    | "r" <> _
    | "s" <> _
    | "t" <> _
    | "u" <> _
    | "v" <> _
    | "w" <> _
    | "x" <> _
    | "y" <> _
    | "z" <> _ ->
      case parse_lower_list(in, 0, paren) {
        Some(#(punctuation, style, start, in)) ->
          Some(#(Ordered(start:, style:, punctuation:), in))
        None -> None
      }

    "A" <> _
    | "B" <> _
    | "C" <> _
    | "D" <> _
    | "E" <> _
    | "F" <> _
    | "G" <> _
    | "H" <> _
    | "I" <> _
    | "J" <> _
    | "K" <> _
    | "L" <> _
    | "M" <> _
    | "N" <> _
    | "O" <> _
    | "P" <> _
    | "Q" <> _
    | "R" <> _
    | "S" <> _
    | "T" <> _
    | "U" <> _
    | "V" <> _
    | "W" <> _
    | "X" <> _
    | "Y" <> _
    | "Z" <> _ ->
      case parse_upper_list(in, 0, paren) {
        Some(#(punctuation, style, start, in)) ->
          Some(#(Ordered(start:, style:, punctuation:), in))
        None -> None
      }

    _ -> None
  }
}

fn take_list_item_chars_indented(
  in: String,
  acc: String,
  style: ListStyle,
  layout: ListLayout,
  indent: Int,
) -> #(String, String, ListLayout) {
  let in = drop_n_spaces(in, indent)
  let #(line, in) = case string.split_once(in, "\n") {
    Ok(split) -> split
    Error(_) -> #(in, "")
  }
  let acc = acc <> line

  case in {
    "" -> #(acc, "", layout)

    " " <> _ ->
      take_list_item_chars_indented(in, acc <> "\n", style, layout, indent)

    "\n " <> rest -> {
      let layout = case parse_list_marker(drop_spaces(rest)) {
        Some(_) -> layout
        None -> Loose
      }
      let acc = acc <> "\n\n"
      let in = string.drop_start(in, 1)
      take_list_item_chars_indented(in, acc, style, layout, indent)
    }

    // A blank line followed by un-indented content, so this is the end of this
    // current list item.
    "\n" <> rest2 -> #(acc, rest2, layout)

    _ -> {
      case continue_list(in, style) {
        Some(_) -> #(acc, in, layout)
        None ->
          take_list_item_chars_indented(in, acc <> "\n", style, layout, indent)
      }
    }
  }
}

fn continue_list(in: String, style: ListStyle) -> Option(String) {
  case parse_list_marker(in) {
    Some(#(next, in)) ->
      case style, next {
        Ordered(punctuation: p1, style: s1, start: _),
          Ordered(punctuation: p2, style: s2, start: _)
          if p1 == p2 && s1 == s2
        -> Some(in)

        _, _ if style == next -> Some(in)
        _, _ -> None
      }
    None -> None
  }
}

fn drop_n_spaces(in: String, count: Int) -> String {
  case in {
    _ if count == 0 -> in
    " " <> rest -> drop_n_spaces(rest, count - 1)
    _ -> in
  }
}

fn parse_number_list(
  in: String,
  num: Int,
  // Whether the ordinal started with a paren
  paren: Bool,
) -> Option(#(OrdinalPunctuation, OrdinalStyle, Int, String)) {
  case in {
    "0" <> rest -> parse_number_list(rest, num * 10 + 0, paren)
    "1" <> rest -> parse_number_list(rest, num * 10 + 1, paren)
    "2" <> rest -> parse_number_list(rest, num * 10 + 2, paren)
    "3" <> rest -> parse_number_list(rest, num * 10 + 3, paren)
    "4" <> rest -> parse_number_list(rest, num * 10 + 4, paren)
    "5" <> rest -> parse_number_list(rest, num * 10 + 5, paren)
    "6" <> rest -> parse_number_list(rest, num * 10 + 6, paren)
    "7" <> rest -> parse_number_list(rest, num * 10 + 7, paren)
    "8" <> rest -> parse_number_list(rest, num * 10 + 8, paren)
    "9" <> rest -> parse_number_list(rest, num * 10 + 9, paren)
    ". " <> rest | ".\n" <> rest -> Some(#(FullStop, NumericOrdinal, num, rest))
    ") " <> rest | ")\n" <> rest -> {
      let punctuation = case paren {
        True -> DoubleParen
        False -> SingleParen
      }
      Some(#(punctuation, NumericOrdinal, num, rest))
    }
    _ -> None
  }
}

fn parse_lower_list(
  in: String,
  num: Int,
  // Whether the ordinal started with a paren
  paren: Bool,
) -> Option(#(OrdinalPunctuation, OrdinalStyle, Int, String)) {
  case in {
    "a" <> in -> parse_lower_list(in, num * 26 + 1, paren)
    "b" <> in -> parse_lower_list(in, num * 26 + 2, paren)
    "c" <> in -> parse_lower_list(in, num * 26 + 3, paren)
    "d" <> in -> parse_lower_list(in, num * 26 + 4, paren)
    "e" <> in -> parse_lower_list(in, num * 26 + 5, paren)
    "f" <> in -> parse_lower_list(in, num * 26 + 6, paren)
    "g" <> in -> parse_lower_list(in, num * 26 + 7, paren)
    "h" <> in -> parse_lower_list(in, num * 26 + 8, paren)
    "i" <> in -> parse_lower_list(in, num * 26 + 9, paren)
    "j" <> in -> parse_lower_list(in, num * 26 + 10, paren)
    "k" <> in -> parse_lower_list(in, num * 26 + 11, paren)
    "l" <> in -> parse_lower_list(in, num * 26 + 12, paren)
    "m" <> in -> parse_lower_list(in, num * 26 + 13, paren)
    "n" <> in -> parse_lower_list(in, num * 26 + 14, paren)
    "o" <> in -> parse_lower_list(in, num * 26 + 15, paren)
    "p" <> in -> parse_lower_list(in, num * 26 + 16, paren)
    "q" <> in -> parse_lower_list(in, num * 26 + 17, paren)
    "r" <> in -> parse_lower_list(in, num * 26 + 18, paren)
    "s" <> in -> parse_lower_list(in, num * 26 + 19, paren)
    "t" <> in -> parse_lower_list(in, num * 26 + 20, paren)
    "u" <> in -> parse_lower_list(in, num * 26 + 21, paren)
    "v" <> in -> parse_lower_list(in, num * 26 + 22, paren)
    "w" <> in -> parse_lower_list(in, num * 26 + 23, paren)
    "x" <> in -> parse_lower_list(in, num * 26 + 24, paren)
    "y" <> in -> parse_lower_list(in, num * 26 + 25, paren)
    "z" <> in -> parse_lower_list(in, num * 26 + 26, paren)

    ". " <> rest | ".\n" <> rest if !paren ->
      Some(#(FullStop, LowerAlphaOrdinal, num, rest))

    ") " <> rest | ")\n" <> rest -> {
      let punctuation = case paren {
        True -> DoubleParen
        False -> SingleParen
      }
      Some(#(punctuation, LowerAlphaOrdinal, num, rest))
    }
    _ -> None
  }
}

fn parse_upper_list(
  in: String,
  num: Int,
  // Whether the ordinal started with a paren
  paren: Bool,
) -> Option(#(OrdinalPunctuation, OrdinalStyle, Int, String)) {
  case in {
    "A" <> in -> parse_upper_list(in, num * 26 + 1, paren)
    "B" <> in -> parse_upper_list(in, num * 26 + 2, paren)
    "C" <> in -> parse_upper_list(in, num * 26 + 3, paren)
    "D" <> in -> parse_upper_list(in, num * 26 + 4, paren)
    "E" <> in -> parse_upper_list(in, num * 26 + 5, paren)
    "F" <> in -> parse_upper_list(in, num * 26 + 6, paren)
    "G" <> in -> parse_upper_list(in, num * 26 + 7, paren)
    "H" <> in -> parse_upper_list(in, num * 26 + 8, paren)
    "I" <> in -> parse_upper_list(in, num * 26 + 9, paren)
    "J" <> in -> parse_upper_list(in, num * 26 + 10, paren)
    "K" <> in -> parse_upper_list(in, num * 26 + 11, paren)
    "L" <> in -> parse_upper_list(in, num * 26 + 12, paren)
    "M" <> in -> parse_upper_list(in, num * 26 + 13, paren)
    "N" <> in -> parse_upper_list(in, num * 26 + 14, paren)
    "O" <> in -> parse_upper_list(in, num * 26 + 15, paren)
    "P" <> in -> parse_upper_list(in, num * 26 + 16, paren)
    "Q" <> in -> parse_upper_list(in, num * 26 + 17, paren)
    "R" <> in -> parse_upper_list(in, num * 26 + 18, paren)
    "S" <> in -> parse_upper_list(in, num * 26 + 19, paren)
    "T" <> in -> parse_upper_list(in, num * 26 + 20, paren)
    "U" <> in -> parse_upper_list(in, num * 26 + 21, paren)
    "V" <> in -> parse_upper_list(in, num * 26 + 22, paren)
    "W" <> in -> parse_upper_list(in, num * 26 + 23, paren)
    "X" <> in -> parse_upper_list(in, num * 26 + 24, paren)
    "Y" <> in -> parse_upper_list(in, num * 26 + 25, paren)
    "Z" <> in -> parse_upper_list(in, num * 26 + 26, paren)

    ". " <> rest | ".\n" <> rest if !paren ->
      Some(#(FullStop, UpperAlphaOrdinal, num, rest))

    ") " <> rest | ")\n" <> rest -> {
      let punctuation = case paren {
        True -> DoubleParen
        False -> SingleParen
      }
      Some(#(punctuation, UpperAlphaOrdinal, num, rest))
    }
    _ -> None
  }
}

fn take_paragraph_chars(
  in: String,
  div_close_size: Option(Int),
) -> #(String, String) {
  let #(paragraph, in) = case string.split_once(in, "\n\n") {
    Ok(#(content, in)) -> #(content, in)
    Error(_) ->
      case string.ends_with(in, "\n") {
        True -> #(string.drop_end(in, 1), "")
        False -> #(in, "")
      }
  }

  case div_close_size {
    Some(size) -> {
      let #(split_paragraph, paragraph_in) =
        search_paragraph_for_div_end(paragraph, [], size)

      case split_paragraph, paragraph_in {
        "", "" -> #(paragraph, in)
        _, "" -> #(split_paragraph, in)
        _, _ -> #(split_paragraph, paragraph_in <> "\n\n" <> in)
      }
    }
    None -> #(paragraph, in)
  }
}

/// Search a stretch of paragraph characters for valid div terminator. A valid
/// div terminator is a line containing leading and trailing whitespace with an
/// uninterrupted fence of colons `:`. The fence must be at least `size` long.
fn search_paragraph_for_div_end(
  in: String,
  acc: List(String),
  size: Int,
) -> #(String, String) {
  let #(line, rest) = slurp_to_line_end(in)
  case check_line_suitable_div_end(line, size) {
    True -> #(acc |> list.reverse |> string.join("\n"), in)
    False -> {
      case rest {
        "" -> #([line, ..acc] |> list.reverse |> string.join("\n"), "")
        rest -> search_paragraph_for_div_end(rest, [line, ..acc], size)
      }
    }
  }
}

/// Split at \n. If a newline is not present, then the remaining characters
/// will be returned as if there where a newline as the final character.
fn slurp_to_line_end(in: String) -> #(String, String) {
  case string.split_once(in, "\n") {
    Ok(split) -> split
    Error(Nil) -> #(in, "")
  }
}

type RenderRefs {
  RenderRefs(
    urls: Dict(String, String),
    reference_attributes: Dict(String, Dict(String, String)),
    footnotes: Dict(String, List(Container)),
  )
}

/// Convert a document tree into a string of HTML.
///
/// See `to_html` for further documentation.
///
pub fn document_to_html(document: Document) -> String {
  let generated_html =
    containers_to_html(
      document.content,
      RenderRefs(
        urls: document.references,
        reference_attributes: document.reference_attributes,
        footnotes: document.footnotes,
      ),
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
  refs: RenderRefs,
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
          |> inlines_to_html(inlines, refs, TrimLast)
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
  refs: RenderRefs,
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
  refs: RenderRefs,
) -> GeneratedHtml {
  let new_html = case container {
    ThematicBreak -> html |> open_tag("hr", dict.new())

    Paragraph(attrs, inlines) -> {
      html
      |> open_tag("p", attrs)
      |> inlines_to_html(inlines, refs, TrimLast)
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

    OrderedList(layout:, punctuation: _, ordinal:, start:, items:) -> {
      let attrs = case start {
        1 -> dict.new()
        _ -> dict.from_list([#("start", int.to_string(start))])
      }
      let attrs = case ordinal {
        NumericOrdinal -> attrs
        LowerAlphaOrdinal -> dict.insert(attrs, "type", "a")
        UpperAlphaOrdinal -> dict.insert(attrs, "type", "A")
      }
      html
      |> open_tag("ol", attrs)
      |> append_to_html("\n")
      |> list_items_to_html(layout, items, refs)
      |> close_tag("ol")
    }

    BlockQuote(attrs, items) ->
      html
      |> open_tag("blockquote", attrs)
      |> append_to_html("\n")
      |> containers_to_html(items, refs, _)
      |> close_tag("blockquote")

    Div(attributes:, items:) ->
      html
      |> open_tag("div", attributes)
      |> append_to_html("\n")
      |> containers_to_html(items, refs, _)
      |> close_tag("div")
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
    |> result.try(fn(footnote) {
      // Even if the footnote is empty, we need to still make sure a backlink is generated
      case list.is_empty(footnote) {
        True -> Error(Nil)
        False -> Ok(footnote)
      }
    })
    |> result.map(fn(footnote) {
      containers_to_html_with_last_paragraph(
        footnote,
        RenderRefs(
          document.references,
          document.reference_attributes,
          document.footnotes,
        ),
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
  refs: RenderRefs,
) -> GeneratedHtml {
  case items {
    [] -> html

    [[Paragraph(_, inlines)], ..rest] if layout == Tight -> {
      html
      |> open_tag("li", dict.new())
      |> append_to_html("\n")
      |> inlines_to_html(inlines, refs, TrimLast)
      |> append_to_html("\n")
      |> close_tag("li")
      |> append_to_html("\n")
      |> list_items_to_html(layout, rest, refs)
    }

    [[Paragraph(_, inlines), nested_list, ..item_rest], ..rest]
      if layout == Tight
    -> {
      html
      |> open_tag("li", dict.new())
      |> append_to_html("\n")
      |> inlines_to_html(inlines, refs, TrimLast)
      |> append_to_html("\n")
      |> containers_to_html([nested_list, ..item_rest], refs, _)
      |> close_tag("li")
      |> append_to_html("\n")
      |> list_items_to_html(layout, rest, refs)
    }

    [item, ..rest] -> {
      html
      |> open_tag("li", dict.new())
      |> append_to_html("\n")
      |> containers_to_html(item, refs, _)
      |> close_tag("li")
      |> append_to_html("\n")
      |> list_items_to_html(layout, rest, refs)
    }
  }
}

fn inlines_to_html(
  html: GeneratedHtml,
  inlines: List(Inline),
  refs: RenderRefs,
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
  refs: RenderRefs,
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
    Delete(inlines) -> {
      html
      |> open_tag("del", dict.new())
      |> inlines_to_html(inlines, refs, NoTrim)
      |> close_tag("del")
    }
    Insert(inlines) -> {
      html
      |> open_tag("ins", dict.new())
      |> inlines_to_html(inlines, refs, NoTrim)
      |> close_tag("ins")
    }
    Mark(inlines) -> {
      html
      |> open_tag("mark", dict.new())
      |> inlines_to_html(inlines, refs, NoTrim)
      |> close_tag("mark")
    }
    Link(attributes, text, destination) -> {
      // Merge: reference attrs <- href <- inline attrs
      let ref_attrs = get_reference_attributes(destination, refs)
      let attrs =
        ref_attrs
        |> dict.merge(destination_attribute("href", destination, refs))
        |> dict.merge(attributes)
      html
      |> open_tag("a", attrs)
      |> inlines_to_html(text, refs, trim)
      |> close_tag("a")
    }
    Image(attributes, text, destination) -> {
      // Merge: reference attrs <- src/alt <- inline attrs
      let ref_attrs = get_reference_attributes(destination, refs)
      let attrs =
        ref_attrs
        |> dict.merge(destination_attribute("src", destination, refs))
        |> dict.insert("alt", houdini.escape(take_inline_text(text, "")))
        |> dict.merge(attributes)
      html
      |> open_tag("img", attrs)
    }
    Symbol(content) -> {
      let attrs =
        dict.new()
        |> add_attribute("class", "symbol")
      html
      |> open_tag("span", attrs)
      |> append_to_html(content)
      |> close_tag("span")
    }
    Span(attributes, inlines) -> {
      html
      |> open_tag("span", attributes)
      |> inlines_to_html(inlines, refs, trim)
      |> close_tag("span")
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

fn get_reference_attributes(
  destination: Destination,
  refs: RenderRefs,
) -> Dict(String, String) {
  case destination {
    Url(_) -> dict.new()
    Reference(id) ->
      dict.get(refs.reference_attributes, id)
      |> result.unwrap(dict.new())
  }
}

fn destination_attribute(
  key: String,
  destination: Destination,
  refs: RenderRefs,
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

/// Get the text from within a container.
///
/// Raw blocks, footnotes, and the ordinals and bullets from lists are not
/// included.
///
pub fn inner_text(container: Container) -> String {
  case container {
    RawBlock(..) | ThematicBreak -> ""

    Codeblock(content:, ..) -> content

    Paragraph(content:, ..) | Heading(content:, ..) ->
      list.fold(content, "", inline_text)

    BlockQuote(items:, ..) | Div(items:, ..) ->
      list.map(items, inner_text) |> string.join("\n\n")

    BulletList(items:, ..) | OrderedList(items:, ..) ->
      items |> list.flat_map(list.map(_, inner_text)) |> string.join("\n\n")
  }
}

fn inline_text(accumulator: String, item: Inline) -> String {
  case item {
    Footnote(..) | Image(..) -> accumulator

    Linebreak -> accumulator <> "\n\n"
    NonBreakingSpace -> accumulator <> " "

    Code(content:)
    | MathInline(content:)
    | MathDisplay(content:)
    | Symbol(content:)
    | Text(content) -> accumulator <> content

    Link(content:, ..)
    | Span(content:, ..)
    | Emphasis(content:)
    | Strong(content:)
    | Delete(content:)
    | Insert(content:)
    | Mark(content:) -> list.fold(content, accumulator, inline_text)
  }
}
