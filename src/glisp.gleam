//// A Lisp interpreter in Gleam
//// It covers a subset of the language, including:
//// - Integers
//// - Booleans
//// - Lists
//// - Procedures
//// - Closures
//// - The `let` special form for defining local variables
//// - The `define` special form for defining global variables
//// - The `lambda` special form for defining procedures
//// - The `if` special form for conditional execution
//// - The `cons` built-in procedure to working with lists
//// - The `car` built-in procedure to working with lists
//// - The `cdr` built-in procedure to working with lists
//// - The `=` built-in procedure for comparing values
//// - The `not` built-in procedure for negating a boolean
//// - The `and` built-in procedure for combining booleans
//// - The `or` built-in procedure for combining booleans
//// - The `+` built-in procedure that adds integers
//// - The `-` built-in procedure that subtracts integers
//// - The `*` built-in procedure that multiplies integers
//// - The `/` built-in procedure that divides integers
//// - The `empty` built-in procedure that returns an empty list

// -- Imports --
import gleam/int
import gleam/list
import gleam/pair
import gleam/result
import gleam/string
import gleam/dict.{type Dict}

// -- Types --

/// Expression represents a value in the language.
/// In lisps everything is an expression.
pub type Expression {
  List(List(Expression))
  Bool(Bool)
  Int(Int)
  Atom(String)
  Procedure(procedure: Procedure)
  Closure(arguments: List(String), body: List(Expression), scope: Scope)
}

/// Error represents a failure in the language.
/// Different types of errors are represented as variants.
pub type Error {
  UnknownValue(String)
  MissingProcedure
  IncorrectArity(expected: Int, got: Int)
  TypeError(expected: String, got: String, value: Expression)
  EmptyList
  UnexpectedEndOfFile
  UnexpectedCloseParen
}

/// Scope represents a mapping of names to values.
pub type Scope =
  Dict(String, Expression)

/// State represents the state of the interpreter.
/// It contains the global scope and the local scope.
/// The local scope is used for local variables and 
/// the global scope is used for global variables.
pub type State {
  State(global_scope: Scope, local_scope: Scope)
}

/// A type alias for the result of evaluating an expression.
type Evaluated =
  Result(#(Expression, State), Error)

/// A type alias for the result of parsing an expression.
type Parsed =
  Result(#(Expression, String), Error)

/// A type alias for a procedure.
type Procedure =
  fn(List(Expression), State) -> Evaluated

/// Eval function takes a string as an input and returns a result 
/// after evaluating the input.
pub fn eval(source: String) -> Result(String, Error) {
  source
  |> parse([])
  |> result.then(evaluate(_, empty, new_state()))
  |> result.map(pair.first)
  |> result.map(print)
}

/// A constants that represents an empty list.
const empty = List([])

// -- Parsing --

/// Parse function takes a string as an input and a list of expressions and 
/// returns a result after parsing the input.
fn parse(
  source: String,
  expressions: List(Expression),
) -> Result(List(Expression), Error) {
  use #(expression, rest) <- result.try(parse_expression(source))
  let expressions = [expression, ..expressions]
  case string.trim_left(rest) {
    "" -> Ok(list.reverse(expressions))
    _ -> parse(rest, expressions)
  }
}

/// Parse_expression function takes a string as a source and returns a result
/// after parsing the expressions included in the source.
/// Also checks for unexpected end of file and unexpected close parenthesis.
fn parse_expression(source: String) -> Parsed {
  let source = string.trim_left(source)
  case source {
    "" -> Error(UnexpectedEndOfFile)
    ")" <> _ -> Error(UnexpectedCloseParen)
    "(" <> source -> parse_list(source)
    source -> parse_atom(source)
  }
}

/// Parse_list function takes a string as a source and returns a result
fn parse_list(source: String) -> Parsed {
  tail_recursive_parse_list(source, [])
}

/// Tail_recursive_parse_list function takes a string as a source and
/// a list of expressions. It returns a result after parsing the list.
/// It is tail recursive and also checks for unexpected end of file.
fn tail_recursive_parse_list(
  source: String,
  elements: List(Expression),
) -> Parsed {
  let source = string.trim_left(source)
  case source {
    "" -> Error(UnexpectedEndOfFile)
    ")" <> rest -> Ok(#(List(list.reverse(elements)), rest))
    _ -> {
      use #(expression, rest) <- result.try(parse_expression(source))
      tail_recursive_parse_list(rest, [expression, ..elements])
    }
  }
}

/// Parse_atom function takes a string as a source and returns a result
/// Atoms can be integers, booleans or symbols.
/// This function also checks for unexpected end of file and 
/// unexpected close parenthesis.
fn parse_atom(source: String) -> Parsed {
  let #(content, rest) = parse_atom_content(source, "")
  case content, rest {
    "", "" -> Error(UnexpectedEndOfFile)
    "", ")" <> _ -> Error(UnexpectedCloseParen)
    "true", _ -> Ok(#(Bool(True), rest))
    "false", _ -> Ok(#(Bool(False), rest))
    _, _ -> {
      let atom =
        int.parse(content)
        |> result.map(Int)
        |> result.unwrap(Atom(content))
      Ok(#(atom, rest))
    }
  }
}

/// Parse_atom_content function parses the content of an atom.
/// It takes a string as source and an atom and returns a tuple.
fn parse_atom_content(source: String, atom: String) -> #(String, String) {
  let #(char, rest) =
    string.pop_grapheme(source)
    |> result.unwrap(#("", ""))
  case char {
    "" -> #(atom, source)
    ")" -> #(atom, source)
    " " -> #(atom, rest)
    char -> parse_atom_content(rest, atom <> char)
  }
}

// -- State --

/// New_state function creates a new state for the interpreter.
/// It initializes the global scope with the built-in procedures.
/// It also initializes the local scope with an empty dictionary.
fn new_state() -> State {
  let global_scope =
    dict.from_list([
      #("+", make_int_operator(fn(a, b) { a + b }, 0)),
      #("-", make_int_operator(fn(a, b) { a - b }, 0)),
      #("*", make_int_operator(fn(a, b) { a * b }, 1)),
      #("/", make_int_operator(fn(a, b) { a / b }, 1)),
      #("empty", empty),
      #("cons", Procedure(cons_builtin)),
      #("car", Procedure(car_builtin)),
      #("cdr", Procedure(cdr_builtin)),
      #("let", Procedure(let_builtin)),
      #("=", Procedure(eq_builtin)),
      #("not", Procedure(not_builtin)),
      #("and", Procedure(and_builtin)),
      #("or", Procedure(or_builtin)),
      #("if", Procedure(if_builtin)),
      #("define", Procedure(define_builtin)),
      #("lambda", Procedure(lambda_builtin)),
    ])
  let local_scope = dict.new()
  State(global_scope: global_scope, local_scope: local_scope)
}

// -- Evaluation --

/// Evaluate function takes a list of expressions, an accumulator and a state
/// and returns a result after evaluating the expressions.
fn evaluate(
  expressions: List(Expression),
  accumulator: Expression,
  state: State,
) -> Evaluated {
  case expressions {
    [] -> Ok(#(accumulator, state))
    [expression, ..expressions] -> {
      use #(evaluated, state) <- result.try(evaluate_expression(
        expression,
        state,
      ))
      evaluate(expressions, evaluated, state)
    }
  }
}

/// Evaluate_expression function takes an expression and a state and returns
/// a result after evaluating the expression.
fn evaluate_expression(expression: Expression, state: State) -> Evaluated {
  case expression {
    Bool(_) | Int(_) | Procedure(_) | Closure(..) -> Ok(#(expression, state))
    List(expressions) -> evaluate_list(expressions, state)
    Atom(atom) -> {
      use value <- result.try(evaluate_atom(atom, state))
      Ok(#(value, state))
    }
  }
}

/// Evaluate_expressions function takes a list of expressions, an evaluated list
/// and a state and returns a result after evaluating the expressions.
fn evaluate_expressions(
  expressions: List(Expression),
  evaluated: List(Expression),
  state: State,
) -> Result(#(List(Expression), State), Error) {
  case expressions {
    [] -> Ok(#(list.reverse(evaluated), state))
    [expression, ..rest] -> {
      use #(expression, state) <- result.try(evaluate_expression(
        expression,
        state,
      ))
      evaluate_expressions(rest, [expression, ..evaluated], state)
    }
  }
}

/// Evaluate_list function takes a list of expressions and a state and returns
/// a result after evaluating the list.
/// Also checks for missing procedure.
fn evaluate_list(list: List(Expression), state) -> Evaluated {
  case list {
    [] -> Error(MissingProcedure)
    [procedure, ..arguments] -> {
      use #(procedure, state) <- result.try(evaluate_expression(
        procedure,
        state,
      ))
      call(procedure, arguments, state)
    }
  }
}

/// Call function takes a callable, a list of arguments and a state and returns
/// a result after calling the callable with the arguments.
/// Also checks for type error.
fn call(
  callable: Expression,
  arguments: List(Expression),
  state: State,
) -> Evaluated {
  case callable {
    Procedure(procedure) -> procedure(arguments, state)
    Closure(parameters, body, environment) ->
      call_closure(parameters, body, environment, arguments, state)
    _ -> type_error("Procedure", callable)
  }
}

/// Call_closure function takes a list of parameters, a list of expressions, a
/// dictionary of environment, a list of arguments and a state and returns a
fn call_closure(
  parameters: List(String),
  body: List(Expression),
  environment: Scope,
  arguments: List(Expression),
  state: State,
) -> Evaluated {
  let original_locals = state.local_scope
  let state = set_locals(state, environment)
  use state <- result.try(evaluate_lambda_arguments(
    parameters,
    arguments,
    state,
    0,
  ))
  use #(result, state) <- result.try(evaluate(body, empty, state))
  Ok(#(result, set_locals(state, original_locals)))
}

/// Evaluate_lambda_arguments function takes a list of parameters, a list of
/// arguments, a state and a count and returns a result after evaluating the
/// lambda arguments.
/// Also checks for incorrect arity.
fn evaluate_lambda_arguments(
  parameters: List(String),
  arguments: List(Expression),
  state: State,
  count: Int,
) -> Result(State, Error) {
  case parameters, arguments {
    [], [] -> Ok(state)
    [parameter, ..parameters], [argument, ..arguments] -> {
      use #(argument, state) <- result.try(evaluate_expression(argument, state))
      let state = insert_local(state, parameter, argument)
      evaluate_lambda_arguments(parameters, arguments, state, count + 1)
    }
    [], rest -> Error(IncorrectArity(count, count + list.length(rest)))
    rest, [] -> Error(IncorrectArity(count + list.length(rest), count))
  }
}

/// Set_locals function takes a state and a scope and returns a new state with
/// the local scope set to the given scope.
fn set_locals(state: State, locals: Scope) -> State {
  State(..state, local_scope: locals)
}

/// Insert_local function takes a state, a name and a value and returns a new
/// state with the local scope updated with the given name and value.
fn insert_local(state: State, name: String, value: Expression) -> State {
  State(..state, local_scope: dict.insert(state.local_scope, name, value))
}

/// Insert_global function takes a state, a name and a value and returns a new
/// state with the global scope updated with the given name and value.
fn insert_global(state: State, name: String, value: Expression) -> State {
  State(..state, global_scope: dict.insert(state.global_scope, name, value))
}

// -- Built-in procedures --

/// Define_builtin function takes a list of expressions and a state and returns
/// a result after defining a global variable.
/// Also checks for incorrect arity.
fn define_builtin(arguments: List(Expression), state: State) -> Evaluated {
  case arguments {
    [name, value] -> {
      use name <- result.try(expect_atom(name))
      use #(value, state) <- result.try(evaluate_expression(value, state))
      Ok(#(empty, insert_global(state, name, value)))
    }
    _ -> Error(IncorrectArity(2, list.length(arguments)))
  }
}

/// Lambda_builtin function takes a list of expressions and a state and returns
/// a result after defining a procedure.
/// Also checks for incorrect arity.
fn lambda_builtin(arguments: List(Expression), state: State) -> Evaluated {
  case arguments {
    [parameters, ..body] -> {
      use parameters <- result.try(expect_list(parameters))
      use parameters <- result.try(list.try_map(parameters, expect_atom))
      Ok(#(Closure(parameters, body, state.local_scope), state))
    }
    _ -> Error(IncorrectArity(2, list.length(arguments)))
  }
}

/// Evaluate_atom function takes an atom and a state and returns a result after
/// evaluating the atom.
/// Also checks for unknown value.
fn evaluate_atom(atom: String, state: State) -> Result(Expression, Error) {
  dict.get(state.local_scope, atom)
  |> result.lazy_or(fn() { dict.get(state.global_scope, atom) })
  |> result.replace_error(UnknownValue(atom))
}

/// Make_int_operator function is responsible for creating a procedure that
/// takes a list of integers and returns a result after reducing the list
/// Used for the built-in procedures +, -, * and /
fn make_int_operator(reducer: fn(Int, Int) -> Int, initial: Int) -> Expression {
  let procedure = fn(values, state) {
    use #(values, state) <- result.try(evaluate_expressions(values, [], state))
    use ints <- result.try(list.try_map(values, expect_int))
    let result =
      ints
      |> list.reduce(reducer)
      |> result.unwrap(initial)
      |> Int
    Ok(#(result, state))
  }
  Procedure(procedure)
}

/// Cons_builtin function takes a list of expressions and a state and returns a
/// result after consing the expressions.
/// Also checks for incorrect arity.
fn cons_builtin(values: List(Expression), state: State) -> Evaluated {
  use #(values, state) <- result.try(evaluate_expressions(values, [], state))
  case values {
    [head, tail] -> {
      use tail <- result.try(expect_list(tail))
      Ok(#(List([head, ..tail]), state))
    }
    _ -> Error(IncorrectArity(2, list.length(values)))
  }
}

/// Car_builtin function takes a list of expressions and a state and returns a
/// result after getting the first element of the list.
/// Returns an error if the list is empty.
fn car_builtin(expressions: List(Expression), state: State) -> Evaluated {
  use expression <- result.try(expect_1(expressions))
  use #(value, state) <- result.try(evaluate_expression(expression, state))
  use list <- result.try(expect_list(value))
  case list {
    [] -> Error(EmptyList)
    [head, ..] -> Ok(#(head, state))
  }
}

/// Cdr_builtin function takes a list of expressions and a state and returns a
/// result after getting the rest of the list.
/// Returns an error if the list is empty.
fn cdr_builtin(expressions: List(Expression), state: State) -> Evaluated {
  use expression <- result.try(expect_1(expressions))
  use #(value, state) <- result.try(evaluate_expression(expression, state))
  use list <- result.try(expect_list(value))
  case list {
    [] -> Error(EmptyList)
    [_, ..tail] -> Ok(#(List(tail), state))
  }
}

/// Let_builtin function takes a list of expressions and a state and returns a
/// result after defining a local variable.
fn let_builtin(expressions: List(Expression), state: State) -> Evaluated {
  let original_locals = state.local_scope
  use #(bindings, value) <- result.try(expect_2(expressions))
  use bindings <- result.try(expect_list(bindings))
  use state <- result.try(list.try_fold(bindings, state, evaluate_binding))
  use #(value, state) <- result.try(evaluate_expression(value, state))
  Ok(#(value, set_locals(state, original_locals)))
}

/// Evaluate_binding function takes a state and a binding and returns a result 
/// after evaluating the binding.
fn evaluate_binding(state: State, binding: Expression) -> Result(State, Error) {
  use binding <- result.try(expect_list(binding))
  use #(name, value) <- result.try(expect_2(binding))
  use name <- result.try(expect_atom(name))
  use #(value, state) <- result.try(evaluate_expression(value, state))
  Ok(insert_local(state, name, value))
}

/// Eq_builtin function takes a list of expressions and a state and returns a
/// result after comparing the expressions.
fn eq_builtin(expressions: List(Expression), state: State) -> Evaluated {
  use #(a, b) <- result.try(expect_2(expressions))
  use #(a, state) <- result.try(evaluate_expression(a, state))
  use #(b, state) <- result.try(evaluate_expression(b, state))
  Ok(#(Bool(compare(a, b)), state))
}

/// Compare function takes two expressions and returns a boolean after comparing
/// the expressions.
/// It compares integers and booleans and returns true if they are equal.
/// It compares lists by comparing each element of the list.
/// It returns false for all other types of expressions.
fn compare(a: Expression, b: Expression) -> Bool {
  case a, b {
    Int(a), Int(b) -> a == b
    Bool(a), Bool(b) -> a == b
    List(a), List(b) -> compare_lists(a, b)
    _, _ -> False
  }
}

/// Compare_lists function takes two lists of expressions and returns a boolean
/// after comparing the lists.
fn compare_lists(a: List(Expression), b: List(Expression)) -> Bool {
  case a, b {
    [], [] -> True
    [x, ..xs], [y, ..ys] -> compare(x, y) && compare_lists(xs, ys)
    _, _ -> False
  }
}

/// Not_builtin function takes a list of expressions and a state and returns a
/// result after negating the expression.
fn not_builtin(expressions: List(Expression), state: State) -> Evaluated {
  use expression <- result.try(expect_1(expressions))
  use #(value, state) <- result.try(evaluate_expression(expression, state))
  use bool <- result.try(expect_bool(value))
  Ok(#(Bool(!bool), state))
}

/// And_builtin function takes a list of expressions and a state and returns a
/// result after combining the expressions with the and operator.
/// It returns true if all the expressions are true, otherwise it returns false.
fn and_builtin(expressions: List(Expression), state: State) -> Evaluated {
  case expressions {
    [] -> Ok(#(Bool(True), state))
    [expression, ..rest] -> {
      use #(value, state) <- result.try(evaluate_expression(expression, state))
      use bool <- result.try(expect_bool(value))
      case bool {
        True -> and_builtin(rest, state)
        False -> Ok(#(Bool(False), state))
      }
    }
  }
}

/// Or_builtin function takes a list of expressions and a state and returns a
/// result after combining the expressions with the or operator.
/// It returns true if any of the expressions are true, otherwise it returns
/// false.
fn or_builtin(expressions: List(Expression), state: State) -> Evaluated {
  case expressions {
    [] -> Ok(#(Bool(False), state))
    [expression, ..rest] -> {
      use #(value, state) <- result.try(evaluate_expression(expression, state))
      use bool <- result.try(expect_bool(value))
      case bool {
        True -> Ok(#(Bool(True), state))
        False -> or_builtin(rest, state)
      }
    }
  }
}

/// If_builtin function takes a list of expressions and a state and returns a
/// result after evaluating the expressions.
/// It evaluates the first expression if the condition is true, otherwise it
/// evaluates the second expression.
fn if_builtin(expressions: List(Expression), state: State) -> Evaluated {
  use #(condition, then, after) <- result.try(expect_3(expressions))
  use #(condition, state) <- result.try(evaluate_expression(condition, state))
  use bool <- result.try(expect_bool(condition))
  case bool {
    True -> evaluate_expression(then, state)
    False -> evaluate_expression(after, state)
  }
}

// -- Error handling --

/// type_error function takes an expected type and a value and returns a result
/// after creating a type error.
/// It is used to create a type error when the expected type and the value do
/// not match.
fn type_error(expected: String, value: Expression) -> Result(anything, Error) {
  Error(TypeError(expected: expected, got: type_name(value), value: value))
}

/// arity_error function takes an expected arity and a list of expressions and
/// returns a result after creating an arity error.
/// It is used to create an arity error when the expected arity and the length
/// 
fn arity_error(expected: Int, got: List(a)) -> Result(anything, Error) {
  Error(IncorrectArity(expected: expected, got: list.length(got)))
}

// -- Helpers --

/// Expect_int function takes an expression and returns a result after checking
/// if the expression is an integer. If not, it returns a type error.
fn expect_int(value: Expression) -> Result(Int, Error) {
  case value {
    Int(i) -> Ok(i)
    _ -> type_error("Int", value)
  }
}

/// Expect_atom function takes an expression and returns a result after checking
/// if the expression is an atom. If not, it returns a type error.
fn expect_atom(value: Expression) -> Result(String, Error) {
  case value {
    Atom(name) -> Ok(name)
    _ -> type_error("Atom", value)
  }
}

/// Expect_list function takes an expression and returns a result after checking
/// if the expression is a list. If not, it returns a type error.
fn expect_list(value: Expression) -> Result(List(Expression), Error) {
  case value {
    List(name) -> Ok(name)
    _ -> type_error("List", value)
  }
}

/// Expect_bool function takes an expression and returns a result after checking
/// if the expression is a boolean. If not, it returns a type error.
fn expect_bool(value: Expression) -> Result(Bool, Error) {
  case value {
    Bool(x) -> Ok(x)
    _ -> type_error("Bool", value)
  }
}

/// Expect_1 function takes a list of expressions and returns a result after
/// checking if the list contains exactly one expression. If not, it returns an
/// arity error.
fn expect_1(expressions: List(Expression)) -> Result(Expression, Error) {
  case expressions {
    [x] -> Ok(x)
    _ -> arity_error(1, expressions)
  }
}

/// Expect_2 function takes a list of expressions and returns a result after
/// checking if the list contains exactly two expressions. If not, it returns
/// an arity error.
fn expect_2(
  expressions: List(Expression),
) -> Result(#(Expression, Expression), Error) {
  case expressions {
    [x, y] -> Ok(#(x, y))
    _ -> arity_error(2, expressions)
  }
}

/// Expect_3 function takes a list of expressions and returns a result after
/// checking if the list contains exactly three expressions. If not, it returns
/// an arity error.
fn expect_3(
  expressions: List(Expression),
) -> Result(#(Expression, Expression, Expression), Error) {
  case expressions {
    [x, y, z] -> Ok(#(x, y, z))
    _ -> arity_error(3, expressions)
  }
}

/// Type_name function takes an expression and returns a string representing the
/// type of the expression.
fn type_name(value: Expression) -> String {
  case value {
    Int(_) -> "Int"
    Bool(_) -> "Bool"
    List(_) -> "List"
    Procedure(_) -> "Procedure"
    Closure(..) -> "Closure"
    Atom(_) -> "Atom"
  }
}

/// Print function takes an expression and prints a string representing the 
/// expression.
fn print(value: Expression) -> String {
  case value {
    Int(i) -> int.to_string(i)
    Bool(True) -> "true"
    Bool(False) -> "false"
    List(xs) -> "'(" <> string.join(list.map(xs, print), " ") <> ")"
    Procedure(_) -> "#<procedure>"
    Closure(..) -> "#<closure>"
    Atom(x) -> "'" <> x
  }
}
