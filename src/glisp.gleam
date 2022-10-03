import gleam/int
import gleam/list
import gleam/pair
import gleam/result
import gleam/string
import gleam/map.{Map}
import gleam/io

pub type Expression {
  Nil
  List(List(Expression))
  Int(Int)
  Atom(String)
  // TODO: this needs to hold a reference to the scope it closes over
  Function(Function)
  Definition
}

pub type Error {
  Unknown(String)
  MalformedDefinition
  TypeError(expected: String, got: String, value: Expression)
}

pub type State {
  State(scope: Map(String, Expression))
}

type Evaluated =
  Result(#(Expression, State), Error)

type Function =
  fn(List(Expression), State) -> Result(#(Expression, State), Error)

pub fn eval(source: String) -> Result(Expression, Error) {
  source
  |> parse([])
  |> evaluate(Nil, State(scope: map.new()))
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
    Nil -> Ok(#(Nil, state))
    Definition -> Ok(#(Definition, state))

    Int(int) -> Ok(#(Int(int), state))
    Function(function) -> Ok(#(Function(function), state))

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
  state: State,
) -> Evaluated {
  io.debug(callable)
  case callable {
    Function(function) -> function(arguments, state)
    Definition -> define(arguments, state)
    _ -> type_error("Function", callable)
  }
}

fn define(arguments: List(Expression), state: State) -> Evaluated {
  case arguments {
    [] -> Error(MalformedDefinition)
    [first, ..rest] -> {
      try name = expect_atom(first)
      try #(value, _body_scope_state) = evaluate(rest, Nil, state)
      let state = State(scope: map.insert(state.scope, name, value))
      Ok(#(Nil, state))
    }
  }
}

fn evaluate_atom(atom: String, state: State) -> Result(Expression, Error) {
  case atom {
    "def" -> Ok(Definition)
    "+" -> Ok(int_function(fn(a, b) { a + b }, 0))
    "-" -> Ok(int_function(fn(a, b) { a - b }, 0))
    "*" -> Ok(int_function(fn(a, b) { a * b }, 1))
    "/" -> Ok(int_function(fn(a, b) { a / b }, 1))
    _ ->
      case map.get(state.scope, atom) {
        Ok(found) -> Ok(found)
        Error(_) -> Error(Unknown(atom))
      }
  }
}

fn int_function(reducer: fn(Int, Int) -> Int, initial: Int) -> Expression {
  Function(fn(values, state) {
    try #(values, state) = evaluate_expressions(values, [], state)
    try arguments = list.try_map(values, expect_int)
    let value =
      arguments
      |> list.reduce(reducer)
      |> result.unwrap(initial)
      |> Int
    Ok(#(value, state))
  })
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

fn type_name(value: Expression) -> String {
  case value {
    Nil -> "Nil"
    Int(_) -> "Int"
    List(_) -> "List"
    Function(_) -> "Function"
    Definition | Atom(_) -> "Atom"
  }
}
