import gleam/io
import gleam/list
import gleam/string
import gleeunit
import jot
import jot_test/support

pub fn main() {
  gleeunit.main()
}

pub fn integration_test() {
  let tests = support.load_example_test_cases()
  use testcase <- list.each(tests)

  let result = jot.to_html(testcase.djot)
  case result == testcase.html {
    True -> io.print_error(".")
    False -> {
      io.print_error("F")
      io.print_error("\n\nTest failed: " <> testcase.file)
      io.print_error("\n\nInput:\n" <> testcase.djot)
      io.print_error("\nExpected:\n" <> string.inspect(testcase.html))
      io.print_error("\nActual:\n" <> string.inspect(result))
      io.print_error("\n")
      panic as "integration test failed"
    }
  }
}
