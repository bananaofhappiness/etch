import consts.{esc}
import cursor.{hide, set_position}
import gleam/erlang/process.{type Subject}
import gleam/io.{print, println}
import gleam/list
import gleam/string.{to_utf_codepoints, utf_codepoint}
import screen.{clear, enter_alternative, exit_alternative}
import text.{
  blinking, bold, dim, inverse, italic, red, reset, underline, with_bg_black,
  with_black, with_blue, with_cyan, with_default, with_green, with_magenta,
  with_red, with_yellow,
}

@external(erlang, "raw", "enter_raw")
pub fn enter_raw() -> Nil

@external(erlang, "io", "get_chars")
fn get_chars(chars: String, n: Int) -> String

@external(erlang, "erlang", "halt")
fn halt(n: Int) -> Nil

pub fn initialize() {
  enter_raw()
  enter_alternative()
  clear()
  hide()
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

  "This one is red"
  |> bold
  |> italic
  |> red
  |> println

  "Hi from GLEAM!"
  |> string.to_graphemes
  |> list.index_map(make_rainbow)
  |> string.join("")
  |> with_bg_black
  |> reset
  |> println

  "Reset" |> println

  let event = init_event_loop()
  loop(event)
}

pub fn init_event_loop() -> Subject(String) {
  let subj = process.new_subject()
  let pid = process.self
  process.register(pid, process.Anm)
  process.spawn(fn() { input_loop(subj) })
  subj
}

fn input_loop(subj: Subject(String)) {
  let char = get_chars("", 1024)
  process.send(subj, char)
  input_loop(subj)
}

pub fn loop(event: Subject(String)) {
  process.sleep(8)
  let msg = process.receive(event, 1)
  case msg {
    Ok(s) -> {
      handle_input(s)
      loop(event)
    }
    Error(_) -> loop(event)
  }
}

fn handle_input(s: String) {
  case s {
    "q" -> {
      exit()
    }
    _ -> {
      print(s)
    }
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
    _ -> todo
  }
}
