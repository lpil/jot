import jot

pub fn main() {
  let djot =
    "Hello, `world!` This is cool! Dr\\. friend? Very neat.
Hello other fren?

  Hi"
  let ast = jot.parse(djot)
  echo ast
}
