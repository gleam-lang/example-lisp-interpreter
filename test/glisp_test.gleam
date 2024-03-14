import gleeunit
import glisp

pub fn main() {
  gleeunit.main()
}

pub fn no_code_test() {
  let assert Error(glisp.UnexpectedEndOfFile) = glisp.eval("")
}

pub fn close_paren_test() {
  let assert Error(glisp.UnexpectedCloseParen) = glisp.eval("(+ 1 2))")
}

pub fn empty_list_test() {
  let assert Error(glisp.MissingProcedure) = glisp.eval("()")
}

pub fn int_test() {
  let assert Ok("124") = glisp.eval("124")
}

pub fn negative_int_test() {
  let assert Ok("-124") = glisp.eval("-124")
}

pub fn multiple_expressions_test() {
  let assert Ok("4") = glisp.eval("1 2 3 4")
}

pub fn add_two_test() {
  let assert Ok("3") = glisp.eval("(+ 1 2)")
}

pub fn add_many_test() {
  let assert Ok("73") = glisp.eval("(+ 1 2 100 -7 -23)")
}

pub fn add_nothing_test() {
  let assert Ok("0") = glisp.eval("(+)")
}

pub fn subtract_test() {
  let assert Ok("-4") = glisp.eval("(- 1 2 3)")
}

pub fn subtract_nothing_test() {
  let assert Ok("0") = glisp.eval("(-)")
}

pub fn multiply_test() {
  let assert Ok("6") = glisp.eval("(* 1 2 3)")
}

pub fn multiply_nothing_test() {
  let assert Ok("1") = glisp.eval("(*)")
}

pub fn divide_test() {
  let assert Ok("2") = glisp.eval("(/ 20 2 5)")
}

pub fn divide_nothing_test() {
  let assert Ok("1") = glisp.eval("(/)")
}

pub fn multiple_spaces_test() {
  let assert Ok("3") = glisp.eval("  (    +     1   2   ) ")
}

pub fn nested_expressions_test() {
  let assert Ok("7") = glisp.eval("(+ 1 2 (* 2 2))")
}

pub fn def_test() {
  let assert Ok("1") = glisp.eval("(define x 1) x")
}

pub fn redefine_test() {
  let assert Ok("2") = glisp.eval("(define x 1) (define x 2) x")
}

pub fn var_in_expression_test() {
  let assert Ok("3") = glisp.eval("(define x 1) (define x (+ x 2)) x")
}

pub fn def_sequence_body_test() {
  let assert Error(glisp.IncorrectArity(2, 4)) =
    glisp.eval("(define x 1 2 3) x")
}

pub fn empty_test() {
  let assert Ok("'()") = glisp.eval("empty")
}

pub fn cons_1_test() {
  let assert Ok("'(1)") = glisp.eval("(cons 1 empty)")
}

pub fn cons_2_test() {
  let assert Ok("'(1 2)") = glisp.eval("(cons 1 (cons 2 empty))")
}

pub fn car_test() {
  let assert Ok("1") = glisp.eval("(car (cons 1 (cons 2 empty)))")
}

pub fn cdr_test() {
  let assert Ok("'(2)") = glisp.eval("(cdr (cons 1 (cons 2 empty)))")
}

pub fn false_test() {
  let assert Ok("false") = glisp.eval("false")
}

pub fn true_test() {
  let assert Ok("true") = glisp.eval("true")
}

pub fn not_true_test() {
  let assert Ok("false") = glisp.eval("(not true)")
}

pub fn not_false_test() {
  let assert Ok("true") = glisp.eval("(not false)")
}

pub fn and_empty_test() {
  let assert Ok("true") = glisp.eval("(and)")
}

pub fn and_true_test() {
  let assert Ok("true") = glisp.eval("(and true true true true)")
}

pub fn and_false_test() {
  let assert Ok("false") = glisp.eval("(and true true true false)")
}

pub fn or_empty_test() {
  let assert Ok("false") = glisp.eval("(or)")
}

pub fn or_true_test() {
  let assert Ok("true") = glisp.eval("(or false false false true)")
}

pub fn or_false_test() {
  let assert Ok("false") = glisp.eval("(or false false false false)")
}

pub fn eq_true_int_test() {
  let assert Ok("true") = glisp.eval("(= 1 1)")
}

pub fn eq_false_int_test() {
  let assert Ok("false") = glisp.eval("(= 2 1)")
}

pub fn eq_true_list_test() {
  let assert Ok("true") = glisp.eval("(= (cons 1 empty) (cons 1 empty))")
}

pub fn eq_false_list_test() {
  let assert Ok("false") = glisp.eval("(= empty (cons 1 empty))")
}

pub fn eq_proc_list_test() {
  let assert Ok("false") = glisp.eval("(= cons cons)")
}

pub fn eq_other_test() {
  let assert Ok("false") = glisp.eval("(= 1 cons)")
}

pub fn let_test() {
  let assert Ok("3") = glisp.eval("(let ((x 1) (y (+ x 2))) y)")
}

pub fn let_nested_test() {
  // Here x is defined in both lets. The inner one does not leak
  let assert Ok("1") =
    glisp.eval(
      "(let ((x 1) 
             (y (let ((x 2)) x)))
        x)",
    )
}

pub fn procedure_printing_test() {
  let assert Ok("#<procedure>") = glisp.eval("let")
}

pub fn if_true_test() {
  let assert Ok("2") = glisp.eval("(if (= 1 1) (+ 1 1) (+ 2 2))")
}

pub fn if_false_test() {
  let assert Ok("4") = glisp.eval("(if (= 1 2) (+ 1 1) (+ 2 2))")
}

pub fn closure_test() {
  let assert Ok("1") =
    glisp.eval(
      "
(define id (lambda (x) x))
(id 1)
",
    )
}

pub fn closure_closes_over_scope_at_time_of_definition_test() {
  let assert Ok("1") =
    glisp.eval(
      "
(define fn (let ((x 1)
                 (return-x (lambda () x)))
            return-x))
(let ((x 2)) (fn))
",
    )
}
