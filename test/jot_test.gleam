import gleam/io
import gleam/list
import gleeunit
import jot
import jot_test/support

pub fn main() {
  gleeunit.main()
}

pub fn integration_test() {
  let tests = support.load_example_test_cases()
  use test <- list.each(tests)

  let result = jot.to_html(test.djot)
  case result == test.html {
    True -> io.print_error(".")
    False -> {
      io.print_error("F")
      io.print_error("\n\nTest failed: " <> test.file)
      io.print_error("\n\nInput:\n" <> test.djot)
      io.print_error("\nExpected:\n" <> test.html)
      io.print_error("\nActual:\n" <> result)
      io.print_error("\n")
      panic as "integration test failed"
    }
  }
}
