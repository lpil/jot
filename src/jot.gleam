import gleam/string
import gleam/iterator.{type Iterator, Done, Next}

// TODO: document
pub fn to_html(djot: String) -> String {
  djot
  |> string.trim_left
  |> string.to_graphemes
  |> iterator.from_list
  |> iter
  |> events_to_html
}

type Chars =
  Iterator(String)

type Events =
  Iterator(Event)

pub type Event {
  ParagraphStart
  ParagraphEnd
  Text(String)
}

fn iter(in: Chars) -> Events {
  case iterator.step(in) {
    Done -> iterator.empty()
    Next("\n", in) | Next("\r\n", in) -> iter(in)
    Next(g, in) -> yield(ParagraphStart, iter_paragraph(in, g))
  }
}

fn iter_paragraph(in: Chars, acc: String) -> Events {
  case iterator.step(in) {
    Done if acc == "" -> iterator.single(ParagraphEnd)
    Done -> yield_text(acc, iter_paragraph(in, ""))

    Next("\r\n", in) | Next("\n", in) -> {
      case iterator.step(in) {
        Next("\r\n", in2) | Next("\n", in2) ->
          yield_text(acc, yield(ParagraphEnd, iter(in2)))

        Next(_, _) -> iter_paragraph(in, acc <> "\n")
        Done -> iter_paragraph(in, acc)
      }
    }

    Next(g, in) -> {
      iter_paragraph(in, acc <> g)
    }
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

fn events_to_html(events: Iterator(Event)) -> String {
  events
  |> iterator.map(event_to_html)
  |> iterator.fold("", string.append)
}

fn event_to_html(event: Event) -> String {
  case event {
    ParagraphStart -> "<p>"
    Text(text) -> text
    ParagraphEnd -> "</p>\n"
  }
}
