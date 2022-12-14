# Glisp

A tree-walking Lisp interpreter written in [Gleam](https://gleam.run)!

It features:
- [x] Ints with `+`, `-`, `*`, and `/`.
- [x] Bools with `not`, `and`, and `or`.
- [x] Comparison with `=`.
- [x] Lists with `empty`, `cons`, `car`, `cdr`.
- [x] Closures with `lambda`.
- [x] Global variables with `define`.
- [x] Local variables with `let`.
- [x] Flow control with `if`.

## Usage

```gleam
import glisp

pub fn main() {
  glisp.eval("(+ 1 2 3)") //=> "6"
}
```

# Caveats

I wrote this on a transatlantic flight on a day that involved no sleep so expect
bugs and other curiosities.
