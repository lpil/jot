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
    Next("#", in) -> iter_heading(in, "#", 1)
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

fn iter_heading(in: Chars, acc: String, level: Int) -> Events {
  case iterator.step(in) {
    Next("#", in) -> iter_heading(in, acc <> "#", level + 1)
    Next(" ", in) ->
      yield(HeadingStart(level), iter_heading_text(in, level, ""))
    Done -> start_paragraph(in, "#" <> acc)
  }
}

fn iter_heading_text(in: Chars, level: Int, acc: String) -> Events {
  case iterator.step(in) {
    Next("\n", in) ->
      case iterator.step(in) {
        Next("\n", in2) -> yield_text(acc, yield(HeadingEnd(level), iter(in2)))

        Next(_, _) -> iter_heading_text(in, level, acc <> "\n")
        Done -> iter_heading_text(in, level, acc)
      }
    Next(g, in) -> iter_heading_text(in, level, acc <> g)
    Done -> yield_text(acc, yield(HeadingEnd(level), iterator.empty()))
  }
}

fn yield(first: Event, then: Events) -> Events {
  iterator.single(first)
  |> iterator.append(then)
}

fn yield_text(first: String, then: Events) -> Events {
  case string.trim(first) {
    "" -> then
    text -> yield(Text(text), then)
  }
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
            |> string.replace("\"", "&quot;")
          let level = int.to_string(level)
          let acc = acc <> "<h" <> level <> " id=\"" <> plain <> "\">"
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
  |> string.to_graphemes
  |> list.filter_map(fn(g) {
    case g {
      "\n" -> Error(Nil)
      _ -> Ok(g)
    }
  })
  |> string.concat
}
