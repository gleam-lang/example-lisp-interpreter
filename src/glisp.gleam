import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub type Expression {
  Nil
  List(List(Expression))
  Int(Int)
  Atom(String)
}

pub type Error {
  ParseError(String)
  UnknownFunction(Expression)
  TypeError(expected: String, got: String, value: Expression)
}

type Parsed =
  Result(#(Expression, String), Error)

type Evaluated =
  Result(Expression, Error)

pub fn run(source: String) -> Evaluated {
  try #(expression, _rest) = parse(source)
  evaluate(expression)
}

fn parse(source: String) -> Parsed {
  Ok(parse_element(source))
}

fn parse_element(source: String) -> #(Expression, String) {
  let source = string.trim(source)
  case source {
    "" | ")" <> _ -> #(Nil, "")
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
    ")" -> #(atom, source)
    " " -> #(atom, rest)
    char -> parse_atom_content(rest, atom <> char)
  }
}

fn evaluate(expression: Expression) -> Evaluated {
  case expression {
    Nil -> Ok(Nil)
    Int(int) -> Ok(Int(int))
    Atom(atom) -> Ok(Atom(atom))
    List(expressions) -> evaluate_list(expressions)
  }
}

fn evaluate_list(list: List(Expression)) -> Evaluated {
  case list {
    [] -> Ok(Nil)
    [function, ..arguments] -> {
      try function = evaluate(function)
      call(function, arguments)
    }
  }
}

fn call(function: Expression, arguments: List(Expression)) -> Evaluated {
  try function = function_procedure(function)
  function(arguments)
}

fn function_procedure(
  function: Expression,
) -> Result(fn(List(Expression)) -> Evaluated, Error) {
  case function {
    Atom("+") -> Ok(add)
    _ -> Error(UnknownFunction(function))
  }
}

fn add(values: List(Expression)) -> Evaluated {
  try arguments = list.try_map(values, expect_int)
  arguments
  |> list.fold(0, fn(a, b) { a + b })
  |> Int
  |> Ok
}

fn expect_int(value: Expression) -> Result(Int, Error) {
  let error = fn(type_) {
    Error(TypeError(expected: "Int", got: type_, value: value))
  }
  case value {
    Int(i) -> Ok(i)
    Nil -> error("Nil")
    List(_) -> error("List")
    Atom(_) -> error("Atom")
  }
}
