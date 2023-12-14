import gleam/list
import gleam/int
import jot
import jot_test/support
import testbldr
import testbldr/should

pub fn main() {
  testbldr.demonstrate(
    that: "Implemented cases pass",
    with: {
      use tests <- list.map(support.load_example_test_cases())
      use i, tst <- list.index_map(tests)
      use <- testbldr.named(int.to_string(i + 1) <> " in " <> tst.file)
      tst.html
      |> should.equal(jot.to_html(tst.djot))
    }
    |> list.flatten,
  )
}
