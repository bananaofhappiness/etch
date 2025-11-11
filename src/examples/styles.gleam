import command.{
  Clear, LeaveAlternateScreen, MoveTo, MoveToNextLine, Print, PrintReset,
  Println, PrintlnReset, ResetColors,
}

import gleam/list
import gleam/string
import stdout.{Queue, execute, flush, queue}
import style.{
  blinking, bold, dim, inverse, italic, on, reset_colors, underline, with,
  with_on,
}
import terminal

@external(erlang, "erlang", "halt")
fn halt(n: Int) -> Nil

fn main() {
  let q = Queue([])
  let q =
    q
    |> queue([
      Clear(terminal.All),
      MoveTo(0, 0),
      command.EnableLineWrap,
    ])

  let x =
    "Hi! This one is dim, italic, blinking, underlined with inversed colors! (your terminal may not support some modifiers)."
    |> dim
    |> italic
    |> blinking
    |> inverse
    |> underline

  let y =
    "`command.PrintlnReset()` resets our color. Now we can use different styles. This one is magenta "
    |> bold
    |> italic
    |> with(style.Magenta)
    <> "and this is dark turquoise. "
    |> bold
    |> italic
    |> on(style.AnsiValue(44))
    <> "As you can see, we can easily apply different styles."
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

  let a =
    "And now the colors are reset after we call `command.ResetColors`. Try commenting it and this text will turn red, because we never reset the color after using `make_rainbow`."

  let q =
    q
    |> queue([
      PrintlnReset(x),
      PrintReset(y),
      MoveToNextLine(1),
      Println(z),
      ResetColors,
      Print(a),
      MoveToNextLine(2),
    ])
  flush(q)

  execute([
    PrintReset(
      "We can use ANSI color values. This one is DarkSlateGray3 with HotPink3 background."
      |> with_on(style.AnsiValue(116), style.AnsiValue(168)),
    ),
    MoveToNextLine(1),
    Println(
      "And the colors are reset again, because `with_on` guarantees that colors are reset after applying them.",
    ),
  ])
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
