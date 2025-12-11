import etch/command.{
  Clear, MoveTo, MoveToNextLine, Print, PrintReset, Println, PrintlnReset,
  ResetStyle,
}
import etch/stdout.{Queue, execute, flush, queue}
import etch/style.{
  attributes, blinking, bold, dim, inverse, italic, on, reset_attributes,
  reset_color, underline, with, with_on,
}
import etch/terminal
import gleam/list
import gleam/string

pub fn main() {
  let q = Queue([])
  let q =
    q
    |> queue([
      Clear(terminal.All),
      MoveTo(0, 0),
      command.EnableLineWrap,
    ])

  let x =
    "Hi! This one is dim, italic, blinking, underlined with inversed colors! (your terminal may not support some attributes)."
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
    <> "and this is on dark turquoise. "
    |> bold
    |> italic
    |> on(style.AnsiValue(44))
    <> "As you can see, we can easily apply different styles."
    |> dim
    |> inverse
    |> underline

  let z =
    "Hi from ETCH! YAAAAAY! "
    |> make_rainbow()
    |> reset_color()
    <> "We defined "
    <> "`make_rainbow`"
    |> attributes([style.Italic, style.Bold, style.Underline])
    |> reset_attributes()
    <> " function and made this text "
    <> "RAINBOW!"
    |> make_rainbow()

  let a =
    "And now the colors are reset after we call `command.ResetColors`. Try commenting it and this text will turn red, because we never reset the color after using `make_rainbow`."

  let q =
    q
    |> queue([
      PrintlnReset(x),
      PrintReset(y),
      MoveToNextLine(1),
      Println(z),
      ResetStyle,
      Print(a),
      MoveToNextLine(2),
    ])
  flush(q)

  execute([
    Println(
      "We can use ANSI color values. This one is DarkSlateGray3 with HotPink3 background."
      |> with_on(style.AnsiValue(116), style.AnsiValue(168)),
    ),
    MoveToNextLine(1),
    Println(
      "The colors are not reset again, because `with_on` does not reset them after applying.",
    ),
  ])
}

fn make_rainbow(s: String) -> String {
  s
  |> string.to_graphemes
  |> list.index_map(fn(s, i) {
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
  })
  |> string.join("")
}
