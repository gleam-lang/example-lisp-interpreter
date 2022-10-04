import gleam/int
import gleam/list
import gleam/pair
import gleam/result
import gleam/string
import gleam/map.{Map}

pub type Expression {
  List(List(Expression))
  Bool(Bool)
  Int(Int)
  Atom(String)
  Procedure(procedure: Procedure)
}

pub type Error {
  UnknownValue(String)
  MissingProcedure
  IncorrectArity(expected: Int, got: Int)
  TypeError(expected: String, got: String, value: Expression)
  EmptyList
  UnexpectedEndOfFile
  UnexpectedCloseParen
}

pub type Scope =
  Map(String, Expression)

pub type State {
  State(global_scope: Scope, local_scope: Scope)
}

type Evaluated =
  Result(#(Expression, State), Error)

type Parsed =
  Result(#(Expression, String), Error)

type Procedure =
  fn(List(Expression), State) -> Evaluated

pub fn eval(source: String) -> Result(String, Error) {
  source
  |> parse([])
  |> result.then(evaluate(_, empty, new_state()))
  |> result.map(pair.first)
  |> result.map(print)
}

const empty = List([])

fn parse(
  source: String,
  expressions: List(Expression),
) -> Result(List(Expression), Error) {
  try #(expression, rest) = parse_expression(source)
  let expressions = [expression, ..expressions]
  case string.trim_left(rest) {
    "" -> Ok(list.reverse(expressions))
    _ -> parse(rest, expressions)
  }
}

fn parse_expression(source: String) -> Parsed {
  let source = string.trim_left(source)
  case source {
    "" -> Error(UnexpectedEndOfFile)
    ")" <> _ -> Error(UnexpectedCloseParen)
    "(" <> source -> parse_list(source)
    source -> parse_atom(source)
  }
}

fn parse_list(source: String) -> Parsed {
  tail_recursive_parse_list(source, [])
}

fn tail_recursive_parse_list(
  source: String,
  elements: List(Expression),
) -> Parsed {
  let source = string.trim_left(source)
  case source {
    "" -> Error(UnexpectedEndOfFile)
    ")" <> rest -> Ok(#(List(list.reverse(elements)), rest))
    _ -> {
      try #(expression, rest) = parse_expression(source)
      tail_recursive_parse_list(rest, [expression, ..elements])
    }
  }
}

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

fn new_state() -> State {
  let global_scope =
    map.from_list([
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
    ])
  let local_scope = map.new()
  State(global_scope: global_scope, local_scope: local_scope)
}

fn evaluate(
  expressions: List(Expression),
  accumulator: Expression,
  state: State,
) -> Evaluated {
  case expressions {
    [] -> Ok(#(accumulator, state))
    [expression, ..expressions] -> {
      try #(evaluated, state) = evaluate_expression(expression, state)
      evaluate(expressions, evaluated, state)
    }
  }
}

fn evaluate_expression(expression: Expression, state: State) -> Evaluated {
  case expression {
    Bool(_) | Int(_) | Procedure(..) -> Ok(#(expression, state))
    List(expressions) -> evaluate_list(expressions, state)
    Atom(atom) -> {
      try value = evaluate_atom(atom, state)
      Ok(#(value, state))
    }
  }
}

fn evaluate_expressions(
  expressions: List(Expression),
  evaluated: List(Expression),
  state: State,
) -> Result(#(List(Expression), State), Error) {
  case expressions {
    [] -> Ok(#(list.reverse(evaluated), state))
    [expression, ..rest] -> {
      try #(expression, state) = evaluate_expression(expression, state)
      evaluate_expressions(rest, [expression, ..evaluated], state)
    }
  }
}

fn evaluate_list(list: List(Expression), state) -> Evaluated {
  case list {
    [] -> Error(MissingProcedure)
    [procedure, ..arguments] -> {
      try #(procedure, state) = evaluate_expression(procedure, state)
      call(procedure, arguments, state)
    }
  }
}

fn call(
  callable: Expression,
  arguments: List(Expression),
  state: State,
) -> Evaluated {
  case callable {
    Procedure(procedure) -> procedure(arguments, state)
    _ -> type_error("procedure", callable)
  }
}

fn set_locals(state: State, locals: Scope) -> State {
  State(..state, local_scope: locals)
}

fn insert_local(state: State, name: String, value: Expression) -> State {
  State(..state, local_scope: map.insert(state.local_scope, name, value))
}

fn insert_global(state: State, name: String, value: Expression) -> State {
  State(..state, global_scope: map.insert(state.global_scope, name, value))
}

fn define_builtin(arguments: List(Expression), state: State) -> Evaluated {
  case arguments {
    [name, value] -> {
      try name = expect_atom(name)
      try #(value, state) = evaluate_expression(value, state)
      Ok(#(empty, insert_global(state, name, value)))
    }
    _ -> Error(IncorrectArity(2, list.length(arguments)))
  }
}

fn evaluate_atom(atom: String, state: State) -> Result(Expression, Error) {
  map.get(state.local_scope, atom)
  |> result.lazy_or(fn() { map.get(state.global_scope, atom) })
  |> result.replace_error(UnknownValue(atom))
}

fn make_int_operator(reducer: fn(Int, Int) -> Int, initial: Int) -> Expression {
  let procedure = fn(values, state) {
    try #(values, state) = evaluate_expressions(values, [], state)
    try ints = list.try_map(values, expect_int)
    let result =
      ints
      |> list.reduce(reducer)
      |> result.unwrap(initial)
      |> Int
    Ok(#(result, state))
  }
  Procedure(procedure)
}

fn cons_builtin(values: List(Expression), state: State) -> Evaluated {
  try #(values, state) = evaluate_expressions(values, [], state)
  case values {
    [head, tail] -> {
      try tail = expect_list(tail)
      Ok(#(List([head, ..tail]), state))
    }
    _ -> Error(IncorrectArity(2, list.length(values)))
  }
}

fn car_builtin(expressions: List(Expression), state: State) -> Evaluated {
  try expression = expect_1(expressions)
  try #(value, state) = evaluate_expression(expression, state)
  try list = expect_list(value)
  case list {
    [] -> Error(EmptyList)
    [head, ..] -> Ok(#(head, state))
  }
}

fn cdr_builtin(expressions: List(Expression), state: State) -> Evaluated {
  try expression = expect_1(expressions)
  try #(value, state) = evaluate_expression(expression, state)
  try list = expect_list(value)
  case list {
    [] -> Error(EmptyList)
    [_, ..tail] -> Ok(#(List(tail), state))
  }
}

fn let_builtin(expressions: List(Expression), state: State) -> Evaluated {
  let original_locals = state.local_scope
  try #(bindings, value) = expect_2(expressions)
  try bindings = expect_list(bindings)
  try state = list.try_fold(bindings, state, evaluate_binding)
  try #(value, state) = evaluate_expression(value, state)
  Ok(#(value, set_locals(state, original_locals)))
}

fn evaluate_binding(state: State, binding: Expression) -> Result(State, Error) {
  try binding = expect_list(binding)
  try #(name, value) = expect_2(binding)
  try name = expect_atom(name)
  try #(value, state) = evaluate_expression(value, state)
  Ok(insert_local(state, name, value))
}

fn eq_builtin(expressions: List(Expression), state: State) -> Evaluated {
  try #(a, b) = expect_2(expressions)
  try #(a, state) = evaluate_expression(a, state)
  try #(b, state) = evaluate_expression(b, state)
  Ok(#(Bool(compare(a, b)), state))
}

fn compare(a: Expression, b: Expression) -> Bool {
  case a, b {
    Int(a), Int(b) -> a == b
    Bool(a), Bool(b) -> a == b
    List(a), List(b) -> compare_lists(a, b)
    _, _ -> False
  }
}

fn compare_lists(a: List(Expression), b: List(Expression)) -> Bool {
  case a, b {
    [], [] -> True
    [x, ..xs], [y, ..ys] -> compare(x, y) && compare_lists(xs, ys)
    _, _ -> False
  }
}

fn not_builtin(expressions: List(Expression), state: State) -> Evaluated {
  try expression = expect_1(expressions)
  try #(value, state) = evaluate_expression(expression, state)
  try bool = expect_bool(value)
  Ok(#(Bool(!bool), state))
}

fn and_builtin(expressions: List(Expression), state: State) -> Evaluated {
  case expressions {
    [] -> Ok(#(Bool(True), state))
    [expression, ..rest] -> {
      try #(value, state) = evaluate_expression(expression, state)
      try bool = expect_bool(value)
      case bool {
        True -> and_builtin(rest, state)
        False -> Ok(#(Bool(False), state))
      }
    }
  }
}

fn or_builtin(expressions: List(Expression), state: State) -> Evaluated {
  case expressions {
    [] -> Ok(#(Bool(False), state))
    [expression, ..rest] -> {
      try #(value, state) = evaluate_expression(expression, state)
      try bool = expect_bool(value)
      case bool {
        True -> Ok(#(Bool(True), state))
        False -> or_builtin(rest, state)
      }
    }
  }
}

fn if_builtin(expressions: List(Expression), state: State) -> Evaluated {
  try #(condition, then, else) = expect_3(expressions)
  try #(condition, state) = evaluate_expression(condition, state)
  try bool = expect_bool(condition)
  case bool {
    True -> evaluate_expression(then, state)
    False -> evaluate_expression(else, state)
  }
}

fn type_error(expected: String, value: Expression) -> Result(anything, Error) {
  Error(TypeError(expected: expected, got: type_name(value), value: value))
}

fn arity_error(expected: Int, got: List(a)) -> Result(anything, Error) {
  Error(IncorrectArity(expected: expected, got: list.length(got)))
}

fn expect_int(value: Expression) -> Result(Int, Error) {
  case value {
    Int(i) -> Ok(i)
    _ -> type_error("Int", value)
  }
}

fn expect_atom(value: Expression) -> Result(String, Error) {
  case value {
    Atom(name) -> Ok(name)
    _ -> type_error("Atom", value)
  }
}

fn expect_list(value: Expression) -> Result(List(Expression), Error) {
  case value {
    List(name) -> Ok(name)
    _ -> type_error("List", value)
  }
}

fn expect_bool(value: Expression) -> Result(Bool, Error) {
  case value {
    Bool(x) -> Ok(x)
    _ -> type_error("Bool", value)
  }
}

fn expect_1(expressions: List(Expression)) -> Result(Expression, Error) {
  case expressions {
    [x] -> Ok(x)
    _ -> arity_error(1, expressions)
  }
}

fn expect_2(
  expressions: List(Expression),
) -> Result(#(Expression, Expression), Error) {
  case expressions {
    [x, y] -> Ok(#(x, y))
    _ -> arity_error(2, expressions)
  }
}

fn expect_3(
  expressions: List(Expression),
) -> Result(#(Expression, Expression, Expression), Error) {
  case expressions {
    [x, y, z] -> Ok(#(x, y, z))
    _ -> arity_error(3, expressions)
  }
}

fn type_name(value: Expression) -> String {
  case value {
    Int(_) -> "Int"
    Bool(_) -> "Bool"
    List(_) -> "List"
    Procedure(_) -> "Procedure"
    Atom(_) -> "Atom"
  }
}

fn print(value: Expression) -> String {
  case value {
    Int(i) -> int.to_string(i)
    Bool(True) -> "true"
    Bool(False) -> "false"
    List(xs) -> "'(" <> string.join(list.map(xs, print), " ") <> ")"
    Procedure(_) -> "#<procedure>"
    Atom(x) -> "'" <> x
  }
}
