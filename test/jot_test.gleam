import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import gleam_community/ansi
import gleave
import gleeunit
import jot
import jot_test/support

pub fn main() {
  integration_tests()
  gleeunit.main()
}

fn integration_tests() {
  let tests = support.load_example_test_cases()
  case tests |> list.map(run_testcase) |> result.partition {
    #(_, []) -> Nil
    #(_, errors) -> {
      let count = int.to_string(list.length(errors))
      io.println_error("\n\n" <> count <> " tests failed")
      gleave.exit(127)
    }
  }
}

fn run_testcase(testcase: support.Example) -> Result(Nil, Nil) {
  let result = jot.to_html(testcase.djot)
  case result == testcase.html {
    True -> Ok(io.print_error(ansi.green(".")))
    False -> {
      io.print_error(ansi.red("F"))
      io.print_error(ansi.red("\n\nTest failed: " <> testcase.file))
      io.print_error(ansi.gray("\nInput:\n") <> testcase.djot)
      io.print_error(ansi.gray("\nWant:\n") <> string.inspect(testcase.html))
      io.print_error(ansi.gray("\nGot:\n") <> string.inspect(result))
      io.print_error("\n")
      // panic
      Error(Nil)
    }
  }
}
