# Glisp

A tree-walking Lisp interpreter written in [Gleam](https://gleam.run).

It features:
- [x] Integers with `+`, `-`, `*`, and `/`.
- [x] Lists with `cons`, `car`, `cdr`.
- [ ] Closures with `lambda`.
- [x] Global variables with `define`.
- [ ] Local variables with `let`.

## Usage

```gleam
import glisp

pub fn main() {
  glisp.eval("(+ 1 2 3)") # => "6"
}
```

# Caveats

I wrote this on a transatlantic flight on a day that involved no sleep so expect
bugs and other curiosities.
