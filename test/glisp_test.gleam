import gleeunit
import glisp

pub fn main() {
  gleeunit.main()
}

pub fn no_code_test() {
  assert Ok(glisp.Nil) = glisp.eval("")
}

pub fn empty_list_test() {
  assert Ok(glisp.Nil) = glisp.eval("()")
}

pub fn int_test() {
  assert Ok(glisp.Int(124)) = glisp.eval("124")
}

pub fn negative_int_test() {
  assert Ok(glisp.Int(124)) = glisp.eval("124")
}

pub fn multiple_expressions_test() {
  assert Ok(glisp.Int(4)) = glisp.eval("1 2 3 4")
}

pub fn add_two_test() {
  assert Ok(glisp.Int(3)) = glisp.eval("(+ 1 2)")
}

pub fn add_many_test() {
  assert Ok(glisp.Int(73)) = glisp.eval("(+ 1 2 100 -7 -23)")
}

pub fn add_nothing_test() {
  assert Ok(glisp.Int(0)) = glisp.eval("(+)")
}

pub fn subtract_test() {
  assert Ok(glisp.Int(-4)) = glisp.eval("(- 1 2 3)")
}

pub fn subtract_nothing_test() {
  assert Ok(glisp.Int(0)) = glisp.eval("(-)")
}

pub fn multiply_test() {
  assert Ok(glisp.Int(6)) = glisp.eval("(* 1 2 3)")
}

pub fn multiply_nothing_test() {
  assert Ok(glisp.Int(1)) = glisp.eval("(*)")
}

pub fn divide_test() {
  assert Ok(glisp.Int(2)) = glisp.eval("(/ 20 2 5)")
}

pub fn divide_nothing_test() {
  assert Ok(glisp.Int(1)) = glisp.eval("(/)")
}

pub fn multiple_spaces_test() {
  assert Ok(glisp.Int(3)) = glisp.eval("  (    +     1   2   ) ")
}

pub fn nested_expressions_test() {
  assert Ok(glisp.Int(7)) = glisp.eval("(+ 1 2 (* 2 2))")
}

pub fn def_test() {
  assert Ok(glisp.Int(1)) = glisp.eval("(define x 1) x")
}

pub fn redefine_test() {
  assert Ok(glisp.Int(2)) = glisp.eval("(define x 1) (define x 2) x")
}

pub fn var_in_expression_test() {
  assert Ok(glisp.Int(3)) = glisp.eval("(define x 1) (define x (+ x 2)) x")
}

pub fn def_sequence_body_test() {
  assert Error(glisp.MalformedDefinition) = glisp.eval("(define x 1 2 3) x")
}

pub fn empty_test() {
  assert Ok(glisp.List([])) = glisp.eval("empty")
}

pub fn cons_test() {
  assert Ok(glisp.List([])) = glisp.eval("empty")
}
