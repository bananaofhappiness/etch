import command.{
  EnterAlternateScreen, EnterRaw, LeaveAlternateScreen, MoveCursor,
  MoveToNextLine, Print, PrintReset, Println, PrintlnReset,
}
import cursor.{hide, set_position}
import gleam/erlang/process.{type Subject}
import gleam/io.{print}
import gleam/list
import gleam/string.{utf_codepoint}
import screen.{clear, enter_alternative, leave_alternative}
import stdout.{Queue, execute, flush, println, queue}
import style.{
  blinking, bold, dim, inverse, italic, on, reset, underline, with, with_on,
}

@external(erlang, "io", "get_chars")
fn get_chars(chars: String, n: Int) -> String

@external(erlang, "erlang", "halt")
fn halt(n: Int) -> Nil

pub fn main() {
  execute([EnterRaw, EnterAlternateScreen])
  let q = Queue([])
  let q = q |> queue([EnterAlternateScreen, MoveCursor(0, 0)])

  let x =
    "Initialized!"
    |> bold
    |> dim
    |> italic
    |> blinking
    |> inverse
    |> underline

  let y =
    "This one is grey "
    |> bold
    |> italic
    |> with(style.Grey)
    <> "and this is dark turquoise"
    |> bold
    |> italic
    |> on(style.AnsiValue(44))
    <> " "
    <> "so we can easily apply different styles"
    |> dim
    |> inverse
    |> underline

  let z =
    "Hi from GLEAM! YAAAAAY!"
    |> string.to_graphemes
    |> list.index_map(make_rainbow)
    |> string.join("")
  // |> on(style.AnsiValue(239))

  let a = "Reset"

  let q =
    q
    |> queue([
      PrintlnReset(x),
      PrintReset(y),
      MoveToNextLine(1),
      Println(z),
      PrintReset(""),
      Print(a),
      MoveToNextLine(1),
    ])
  flush(q)
  // echo q.commands

  execute([
    PrintReset(
      "This one is CadetBlue with HotPink3 background"
      |> with_on(style.AnsiValue(72), style.AnsiValue(168)),
    ),
    MoveToNextLine(1),
    Print("ok?"),
  ])

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
  execute([LeaveAlternateScreen])
  halt(0)
}

fn make_rainbow(s: String, i: Int) -> String {
  case i % 7 {
    0 -> with(s, style.Red)
    1 -> with(s, style.AnsiValue(208))
    2 -> with(s, style.Yellow)
    3 -> with(s, style.Green)
    4 -> with(s, style.AnsiValue(14))
    5 -> with(s, style.Blue)
    6 -> with(s, style.Magenta)
    _ -> panic as "Unreachable"
  }
}
