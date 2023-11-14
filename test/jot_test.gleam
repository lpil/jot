import gleam/io
import gleeunit
import jot_test/support

pub fn main() {
  gleeunit.main()
}

pub fn integration_test() {
  let tests = support.load_example_test_cases()

  io.debug(tests)
}
