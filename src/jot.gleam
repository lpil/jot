import gleam/int
import gleam/list
import gleam/string
import gleam/iterator.{type Iterator, Done, Next}

// TODO: document
pub fn to_html(djot: String) -> String {
  djot
  |> string.trim_left
  |> string.to_graphemes
  |> iterator.from_list
  |> iterator.map(string.replace(_, "\r\n", "\n"))
  |> iter
  |> events_to_html("")
}

type Chars =
  Iterator(String)

type Events =
  Iterator(Event)

pub type Event {
  ParagraphStart
  ParagraphEnd
  HeadingStart(level: Int)
  HeadingEnd(level: Int)
  Text(String)
}

fn iter(in: Chars) -> Events {
  case iterator.step(in) {
    Done -> iterator.empty()
    Next("\n", in) -> iter(in)
    Next("#", in) -> iter_heading(in)
    Next(g, in) -> start_paragraph(in, g)
  }
}

fn start_paragraph(in: Chars, g: String) -> Events {
  yield(ParagraphStart, iter_paragraph(in, g))
}

fn iter_paragraph(in: Chars, acc: String) -> Events {
  case iterator.step(in) {
    Done if acc == "" -> iterator.single(ParagraphEnd)
    Done -> yield_text(acc, iter_paragraph(in, ""))

    Next("\n", in) -> {
      case iterator.step(in) {
        Next("\r\n", in2) | Next("\n", in2) ->
          yield_text(acc, yield(ParagraphEnd, iter(in2)))

        Next(_, _) -> iter_paragraph(in, acc <> "\n")
        Done -> iter_paragraph(in, acc)
      }
    }

    Next(g, in) -> iter_paragraph(in, acc <> g)
  }
}

fn iter_heading(in: Chars) -> Events {
  let #(level, in) = count_heading_hashes(in, 1)
  yield(HeadingStart(level), iter_heading_text(in, level, ""))
}

fn iter_heading_text(in: Chars, level: Int, acc: String) -> Events {
  case iterator.step(in) {
    Next("\n", in) ->
      case iterator.step(in) {
        // Two newlines. The heading is over.
        Next("\n", in2) -> yield_text(acc, yield(HeadingEnd(level), iter(in2)))

        // One newline followed by some text.
        // This could be more content, or a different heading
        Next(_, _) -> {
          let in = drop_spaces(in)
          let #(count, in) = count_heading_hashes(in, 0)
          case count {
            // Text within same heading
            0 -> iter_heading_text(in, level, acc <> "\n")

            // The same heading, with preceeding hashes of the same length
            _ if count == level -> iter_heading_text(in, level, acc <> "\n")

            // Some other number of hashes. This is a new heading.
            c -> {
              yield_all(
                [Text(acc), HeadingEnd(level), HeadingStart(c)],
                iter_heading_text(in, c, ""),
              )
            }
          }
        }

        Done -> iter_heading_text(in, level, acc)
      }
    Next(g, in) -> iter_heading_text(in, level, acc <> g)
    Done -> yield_all([Text(acc), HeadingEnd(level)], iterator.empty())
  }
}

fn yield_all(first: List(Event), then: Events) -> Events {
  list.fold_right(first, then, fn(a, b) { yield(b, a) })
}

fn yield(first: Event, then: Events) -> Events {
  case first {
    Text(t) ->
      case string.trim(t) {
        "" -> then
        text ->
          iterator.single(Text(text))
          |> iterator.append(then)
      }

    _ ->
      iterator.single(first)
      |> iterator.append(then)
  }
}

fn yield_text(first: String, then: Events) -> Events {
  yield(Text(first), then)
}

fn events_to_html(events: Iterator(Event), acc: String) -> String {
  case iterator.step(events) {
    Done -> acc
    Next(event, events) ->
      case event {
        ParagraphEnd -> events_to_html(events, acc <> "</p>\n")
        ParagraphStart -> events_to_html(events, acc <> "<p>")

        HeadingStart(level) -> {
          let plain =
            plain_text_until(events, HeadingEnd(level))
            |> string.replace(" ", "-")
            |> string.replace("\n", "-")
            |> string.replace("\"", "&quot;")
          let acc = acc <> "<h" <> int.to_string(level)
          let acc = case plain {
            "" -> acc
            _ -> acc <> " id=\"" <> plain <> "\""
          }
          let acc = acc <> ">"
          events_to_html(events, acc)
        }
        HeadingEnd(level) -> {
          let level = int.to_string(level)
          events_to_html(events, acc <> "</h" <> level <> ">\n")
        }

        Text(text) -> events_to_html(events, acc <> text)
      }
  }
}

fn drop_spaces(iter: Chars) -> Chars {
  case iterator.step(iter) {
    Next(" ", iter2) -> drop_spaces(iter2)
    Next("\t", iter2) -> drop_spaces(iter2)
    Next(_, _) -> iter
    Done -> iter
  }
}

fn count_heading_hashes(iter: Chars, count: Int) -> #(Int, Chars) {
  case iterator.step(iter) {
    Next("#", iter2) -> count_heading_hashes(iter2, count + 1)
    Next(" ", iter2) -> #(count, iter2)
    Next("\n", iter2) -> #(count, iter2)
    Next(_, _) -> #(0, iter)
    Done -> #(count, iter)
  }
}

fn plain_text_until(events: Events, end: Event) -> String {
  events
  |> iterator.take_while(fn(e) { e != end })
  |> iterator.flat_map(fn(e) {
    case e {
      Text(t) -> iterator.single(t)
      _ -> iterator.empty()
    }
  })
  |> iterator.fold("", fn(a, b) { a <> b })
}
