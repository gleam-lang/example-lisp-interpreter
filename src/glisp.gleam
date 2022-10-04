import gleam/int
import gleam/list
import gleam/pair
import gleam/result
import gleam/string
import gleam/map.{Map}

pub type Expression {
  Nil
  List(List(Expression))
  Int(Int)
  Atom(String)
  Function(function: Function, scope: Scope)
}

pub type Error {
  Unknown(String)
  IncorrectArity(expected: Int, got: Int)
  TypeError(expected: String, got: String, value: Expression)
}

pub type Scope =
  Map(String, Expression)

pub type State {
  State(global_scope: Scope, local_scope: Scope)
}

type Evaluated =
  Result(#(Expression, State), Error)

type Function =
  fn(List(Expression), State) -> Evaluated

pub fn eval(source: String) -> Result(Expression, Error) {
  source
  |> parse([])
  |> evaluate(Nil, new_state())
  |> result.map(pair.first)
}

fn parse(source: String, expressions: List(Expression)) -> List(Expression) {
  let #(expression, rest) = parse_element(source)
  let expressions = [expression, ..expressions]
  case rest {
    "" -> list.reverse(expressions)
    _ -> parse(rest, expressions)
  }
}

fn parse_element(source: String) -> #(Expression, String) {
  let source = string.trim(source)
  case source {
    "" -> #(Nil, "")
    ")" <> rest -> #(Nil, rest)
    "(" <> source -> parse_list(source)
    source -> parse_atom(source)
  }
}

fn parse_list(source: String) -> #(Expression, String) {
  tail_recursive_parse_list(source, [])
}

fn tail_recursive_parse_list(
  source: String,
  elements: List(Expression),
) -> #(Expression, String) {
  let #(expression, rest) = parse_element(source)
  case expression {
    Nil -> #(List(list.reverse(elements)), rest)
    _ -> tail_recursive_parse_list(rest, [expression, ..elements])
  }
}

fn parse_atom(source: String) -> #(Expression, String) {
  let #(content, rest) = parse_atom_content(source, "")
  let atom = case int.parse(content) {
    Ok(i) -> Int(i)
    _ -> Atom(content)
  }

  #(atom, rest)
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
    |> map.insert("empty", List([]))
    |> map.insert("cons", Function(cons_builtin, map.new()))
    |> map.insert("define", Function(define_builtin, map.new()))
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
    Nil | Int(_) | Function(..) -> Ok(#(expression, state))

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
    [] -> Ok(#(Nil, state))
    [function, ..arguments] -> {
      try #(function, state) = evaluate_expression(function, state)
      call(function, arguments, state)
    }
  }
}

fn call(
  callable: Expression,
  arguments: List(Expression),
  state1: State,
) -> Evaluated {
  case callable {
    Function(function, scope) -> {
      try #(result, state2) = function(arguments, set_locals(state1, scope))
      let state = set_locals(state2, state1.local_scope)
      Ok(#(result, state))
    }
    _ -> type_error("Function", callable)
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
      Ok(#(Nil, insert_global(state, name, value)))
    }
    _ -> Error(IncorrectArity(2, list.length(arguments)))
  }
}

fn evaluate_atom(atom: String, state: State) -> Result(Expression, Error) {
  map.get(state.local_scope, atom)
  |> result.lazy_or(fn() { map.get(state.global_scope, atom) })
  |> result.replace_error(Unknown(atom))
}

fn make_int_operator(reducer: fn(Int, Int) -> Int, initial: Int) -> Expression {
  let function = fn(values, state) {
    try #(values, state) = evaluate_expressions(values, [], state)
    try ints = list.try_map(values, expect_int)
    let result =
      ints
      |> list.reduce(reducer)
      |> result.unwrap(initial)
      |> Int
    Ok(#(result, state))
  }
  Function(function, map.new())
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

fn type_error(expected: String, value: Expression) -> Result(anything, Error) {
  Error(TypeError(expected: expected, got: type_name(value), value: value))
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

fn type_name(value: Expression) -> String {
  case value {
    Nil -> "Nil"
    Int(_) -> "Int"
    List(_) -> "List"
    Function(_, _) -> "Function"
    Atom(_) -> "Atom"
  }
}
