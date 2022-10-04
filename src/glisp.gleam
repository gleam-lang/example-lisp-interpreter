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
  Procedure(procedure: Procedure, scope: Scope)
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
    map.new()
    |> map.insert("+", make_int_operator(fn(a, b) { a + b }, 0))
    |> map.insert("-", make_int_operator(fn(a, b) { a - b }, 0))
    |> map.insert("*", make_int_operator(fn(a, b) { a * b }, 1))
    |> map.insert("/", make_int_operator(fn(a, b) { a / b }, 1))
    |> map.insert("empty", empty)
    |> map.insert("cons", Procedure(cons_builtin, map.new()))
    |> map.insert("car", Procedure(car_builtin, map.new()))
    |> map.insert("cdr", Procedure(cdr_builtin, map.new()))
    |> map.insert("define", Procedure(define_builtin, map.new()))
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
  state1: State,
) -> Evaluated {
  case callable {
    Procedure(procedure, scope) -> {
      try #(result, state2) = procedure(arguments, set_locals(state1, scope))
      let state = set_locals(state2, state1.local_scope)
      Ok(#(result, state))
    }
    _ -> type_error("procedure", callable)
  }
}

fn set_locals(state: State, locals: Scope) -> State {
  State(..state, local_scope: locals)
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
  Procedure(procedure, map.new())
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

fn car_builtin(values: List(Expression), state: State) -> Evaluated {
  try expression = expect_1(values)
  try #(value, state) = evaluate_expression(expression, state)
  try list = expect_list(value)
  case list {
    [] -> Error(EmptyList)
    [head, ..] -> Ok(#(head, state))
  }
}

fn cdr_builtin(values: List(Expression), state: State) -> Evaluated {
  try expression = expect_1(values)
  try #(value, state) = evaluate_expression(expression, state)
  try list = expect_list(value)
  case list {
    [] -> Error(EmptyList)
    [_, ..tail] -> Ok(#(List(tail), state))
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

fn expect_1(expressions: List(Expression)) -> Result(Expression, Error) {
  case expressions {
    [x] -> Ok(x)
    _ -> arity_error(1, expressions)
  }
}

fn type_name(value: Expression) -> String {
  case value {
    Int(_) -> "Int"
    Bool(_) -> "Bool"
    List(_) -> "List"
    Procedure(_, _) -> "Procedure"
    Atom(_) -> "Atom"
  }
}

fn print(value: Expression) -> String {
  case value {
    Int(i) -> int.to_string(i)
    Bool(True) -> "true"
    Bool(False) -> "false"
    List(xs) -> "'(" <> string.join(list.map(xs, print), " ") <> ")"
    Procedure(_, _) -> "#procedure"
    Atom(x) -> "'" <> x
  }
}
