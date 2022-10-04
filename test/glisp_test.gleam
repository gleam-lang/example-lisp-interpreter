import gleeunit
import glisp

pub fn main() {
  gleeunit.main()
}

pub fn no_code_test() {
  assert Error(glisp.UnexpectedEndOfFile) = glisp.eval("")
}

pub fn close_paren_test() {
  assert Error(glisp.UnexpectedCloseParen) = glisp.eval("(+ 1 2))")
}

pub fn empty_list_test() {
  assert Error(glisp.MissingProcedure) = glisp.eval("()")
}

pub fn int_test() {
  assert Ok("124") = glisp.eval("124")
}

pub fn negative_int_test() {
  assert Ok("-124") = glisp.eval("-124")
}

pub fn multiple_expressions_test() {
  assert Ok("4") = glisp.eval("1 2 3 4")
}

pub fn add_two_test() {
  assert Ok("3") = glisp.eval("(+ 1 2)")
}

pub fn add_many_test() {
  assert Ok("73") = glisp.eval("(+ 1 2 100 -7 -23)")
}

pub fn add_nothing_test() {
  assert Ok("0") = glisp.eval("(+)")
}

pub fn subtract_test() {
  assert Ok("-4") = glisp.eval("(- 1 2 3)")
}

pub fn subtract_nothing_test() {
  assert Ok("0") = glisp.eval("(-)")
}

pub fn multiply_test() {
  assert Ok("6") = glisp.eval("(* 1 2 3)")
}

pub fn multiply_nothing_test() {
  assert Ok("1") = glisp.eval("(*)")
}

pub fn divide_test() {
  assert Ok("2") = glisp.eval("(/ 20 2 5)")
}

pub fn divide_nothing_test() {
  assert Ok("1") = glisp.eval("(/)")
}

pub fn multiple_spaces_test() {
  assert Ok("3") = glisp.eval("  (    +     1   2   ) ")
}

pub fn nested_expressions_test() {
  assert Ok("7") = glisp.eval("(+ 1 2 (* 2 2))")
}

pub fn def_test() {
  assert Ok("1") = glisp.eval("(define x 1) x")
}

pub fn redefine_test() {
  assert Ok("2") = glisp.eval("(define x 1) (define x 2) x")
}

pub fn var_in_expression_test() {
  assert Ok("3") = glisp.eval("(define x 1) (define x (+ x 2)) x")
}

pub fn def_sequence_body_test() {
  assert Error(glisp.IncorrectArity(2, 4)) = glisp.eval("(define x 1 2 3) x")
}

pub fn empty_test() {
  assert Ok("'()") = glisp.eval("empty")
}

pub fn cons_1_test() {
  assert Ok("'(1)") = glisp.eval("(cons 1 empty)")
}

pub fn cons_2_test() {
  assert Ok("'(1 2)") = glisp.eval("(cons 1 (cons 2 empty))")
}

pub fn car_test() {
  assert Ok("1") = glisp.eval("(car (cons 1 (cons 2 empty)))")
}

pub fn cdr_test() {
  assert Ok("'(2)") = glisp.eval("(cdr (cons 1 (cons 2 empty)))")
}

pub fn false_test() {
  assert Ok("false") = glisp.eval("false")
}

pub fn true_test() {
  assert Ok("true") = glisp.eval("true")
}
