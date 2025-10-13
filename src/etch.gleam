import consts.{esc}
import cursor.{hide, set_position}
import gleam/erlang/process.{type Subject}
import gleam/io.{print}
import gleam/list
import gleam/string.{to_utf_codepoints, utf_codepoint}
import screen.{clear, enter_alternative, exit_alternative, println}
import text.{
  blinking, bold, dim, inverse, italic, red, reset, underline, with_black,
  with_blue, with_cyan, with_default, with_green, with_magenta, with_on_black,
  with_red, with_yellow, yellow,
}

@external(erlang, "raw", "enter_raw")
pub fn enter_raw() -> Nil

@external(erlang, "io", "get_chars")
fn get_chars(chars: String, n: Int) -> String

@external(erlang, "erlang", "halt")
fn halt(n: Int) -> Nil

fn initialize() {
  enter_raw()
  enter_alternative()
  clear()
  // hide()
  set_position(0, 0)
}

pub fn main() {
  initialize()

  "Initialized!"
  |> bold
  |> dim
  |> italic
  |> blinking
  |> inverse
  |> underline
  |> println

  let x =
    "This one is red "
    |> bold
    |> italic
    |> red
    <> "and is with black background"
    |> with_on_black
    |> bold
    |> italic
    |> red
    <> " "
    <> "so we can easily apply different styles"
    |> dim
    |> inverse
    |> underline
    |> yellow
  println(x)

  "Hi from GLEAM! YAAAAAAY!"
  |> string.to_graphemes
  |> list.index_map(make_rainbow)
  |> string.join("")
  // |> with_on_black
  |> reset
  |> println

  "Reset" |> println

  let event = init_event_loop()
  loop(event)
}

pub fn init_event_loop() -> Subject(String) {
  let subj = process.new_subject()
  process.spawn(fn() { input_loop(subj) })
  subj
}

fn input_loop(subj: Subject(String)) {
  let char = get_chars("", 1024)
  process.send(subj, char)
  input_loop(subj)
}

fn loop(event: Subject(String)) {
  process.sleep(8)
  handle_input(event)
  loop(event)
}

fn handle_input(msg: Subject(String)) {
  let assert Ok(etx) = utf_codepoint(3)
  let etx = string.from_utf_codepoints([etx])
  case process.receive(msg, 1) {
    Ok("q") -> exit()
    Ok("\r") -> println("")
    Ok(s) if s == etx -> exit()
    Ok(s) -> print(s)
    Error(_) -> Nil
  }
}

pub fn exit() {
  exit_alternative()
  halt(0)
}

fn make_rainbow(s: String, i: Int) -> String {
  case i % 6 {
    0 -> with_red(s)
    1 -> with_yellow(s)
    2 -> with_green(s)
    3 -> with_cyan(s)
    4 -> with_blue(s)
    5 -> with_magenta(s)
    _ -> panic as "Unreachable"
  }
}
