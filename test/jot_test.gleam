import gleam/list
import gleam/int
import jot
import jot_test/support
import testbldr
import testbldr/pieces as testpieces

pub fn main() {
  testbldr.demonstrate(
    that: "Implemented cases pass",
    with: {
      use tests <- list.map(support.load_example_test_cases())
      use i, tst <- list.index_map(tests)
      case tst.html == jot.to_html(tst.djot) {
        True -> {
          use <- testbldr.named(".")
          testpieces.Pass
        }
        False -> {
          use <- testbldr.named(int.to_string(i + 1) <> " in " <> tst.file)
          let failure_msg =
            "Input djot: " <> tst.djot <> "\n" <> "Expected html: " <> tst.html <> "\n" <> "Produced html: " <> jot.to_html(
              tst.djot,
            ) <> "\n"
          testpieces.Fail(failure_msg)
        }
      }
    }
    |> list.flatten,
  )
}
