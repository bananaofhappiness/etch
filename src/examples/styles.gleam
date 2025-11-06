import command.{
  Clear, DisableLineWrap, EnableLineWrap, EnterAlternateScreen, EnterRaw,
  HideCursor, LeaveAlternateScreen, MoveDown, MoveLeft, MoveRight, MoveTo,
  MoveToColumn, MoveToNextLine, MoveToPreviousLine, MoveToRow, MoveUp, Print,
  PrintReset, Println, PrintlnReset, ResetColors, SavePosition, ScrollDown,
  ScrollUp, SetCursorStyle, SetSize, SetTitle, ShowCursor,
}
import cursor
import esc.{esc}
import event.{init_event_loop}
import gleam/erlang/process.{type Subject}
import gleam/io.{print}
import gleam/list
import gleam/string.{utf_codepoint}
import stdout.{Queue, execute, flush, println, queue}
import style.{
  blinking, bold, dim, inverse, italic, on, reset_colors, underline, with,
  with_on,
}
import terminal

@external(erlang, "erlang", "halt")
fn halt(n: Int) -> Nil

pub fn main() {
  let q = Queue([])
  let q =
    q
    |> queue([
      Clear(terminal.All),
      MoveTo(0, 0),
      // command.EnableLineWrap,
    ])

  let x =
    "Initialized! This one is dim, italic, blinking, underlined with inversed colors!"
    |> dim
    |> italic
    |> blinking
    |> inverse
    |> underline

  let y =
    "This one is magenta "
    |> bold
    |> italic
    |> with(style.Magenta)
    <> "and this is dark turquoise. "
    |> bold
    |> italic
    |> on(style.AnsiValue(44))
    <> "As you can see, we can easily apply different styles"
    |> dim
    |> inverse
    |> underline

  let z =
    "Hi from GLEAM! YAAAAAY! "
    |> string.to_graphemes
    |> list.index_map(make_rainbow)
    |> string.join("")
    |> reset_colors()
    <> "We defined "
    <> "`make_rainbow`"
    |> style.attributes([style.Italic, style.Bold, style.Underline])
    |> style.reset_attributes()
    <> " function and made this text "
    <> "RAINBOW!"
    |> string.to_graphemes
    |> list.index_map(make_rainbow)
    |> string.join("")
  // |> on(style.AnsiValue(239))

  let a = "And now the colors are reset"

  let q =
    q
    |> queue([
      PrintlnReset(x),
      PrintReset(y),
      MoveToNextLine(1),
      Println(z),
      ResetColors,
      Print(a),
      MoveToNextLine(1),
    ])
  flush(q)

  execute([
    PrintReset(
      "We can use ANSI color values. This one is DarkSlateGray3 with HotPink3 background"
      |> with_on(style.AnsiValue(116), style.AnsiValue(168)),
    ),
    MoveToNextLine(1),
    Println(
      "And the colors are reset again, because `with_on` guarantees that colors are reset after applying them.",
    ),
  ])
}

fn loop(event_rx: Subject(String)) {
  process.sleep(8)
  handle_input(event_rx)
  loop(event_rx)
}

fn handle_input(rx: Subject(String)) {
  let assert Ok(etx) = utf_codepoint(3)
  let etx = string.from_utf_codepoints([etx])
  // let assert Ok(bs) = utf_codepoint(8)
  // let bs = string.from_utf_codepoints([bs])
  let up = esc <> "[1A"
  case process.receive(rx, 1) {
    Ok("q") -> exit()
    Ok("w") -> execute([EnableLineWrap])
    Ok("f") -> execute([DisableLineWrap])
    Ok("\r") -> println("")
    Ok(s) if s == up -> println("up")
    // Ok(s) if s == bs -> execute([MoveLeft(1)])
    // Ok("b") ->
    //   execute([
    //     SetCursorStyle(cursor.BlinkingBlock),
    //   ])
    // Ok("s") ->
    //   execute([
    //     SetCursorStyle(cursor.SteadyBlock),
    //   ])
    Ok("x") -> print(cursor.restore_position())
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
    0 -> with(s, style.Rgb(233, 51, 35))
    1 -> with(s, style.Rgb(241, 161, 57))
    2 -> with(s, style.AnsiValue(220))
    3 -> with(s, style.AnsiValue(40))
    4 -> with(s, style.Rgb(74, 163, 228))
    5 -> with(s, style.Blue)
    6 -> with(s, style.Magenta)
    _ -> panic as "Unreachable"
  }
}
