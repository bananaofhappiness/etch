import gleam/erlang/process.{type Pid, type Subject}
import gleam/io.{print}

@external(erlang, "raw", "enter_raw")
pub fn enter_raw() -> Nil

@external(erlang, "io", "get_chars")
fn get_chars(chars: String, n: Int) -> String

@external(erlang, "erlang", "halt")
fn halt(n: Int) -> Nil

pub fn setup() {
  enter_raw()
  print("\u{001b}[?1049h")
  print("\u{001b}[?25l")
}

pub fn main() {
  let subj = process.new_subject()
  let pid = process.self()

  setup()
  process.spawn(fn() { input_loop(subj) })
  loop(subj)
  print("\u{001b}[?1049l")
}

pub fn input_loop(subj: Subject(String)) {
  let char = get_chars("", 1024)
  case char {
    _ -> {
      process.send(subj, char)
      input_loop(subj)
    }
  }
}

pub fn loop(sbj: Subject(String)) {
  let msg = process.receive(sbj, 1)
  case msg {
    Ok(s) -> {
      print(s)
      loop(sbj)
    }
    Error(_) -> loop(sbj)
  }
}
