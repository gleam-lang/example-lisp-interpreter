# Glisp

A tree-walking Lisp interpreter written in [Gleam](https://gleam.run).

It features:
- Integers and basic arithmetic.
- Lists, `cons`.
- Immutable variable definitions.
- Lexical scoping.
- S-expression printing.

## Usage

```gleam
import glisp

pub fn main() {
  glisp.eval("(+ 1 2 3)")
}
```

# Caveats

I wrote this on a transatlantic flight on a day that involved no sleep so expect
bugs and other curiosities.
