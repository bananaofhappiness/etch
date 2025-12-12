import etch/command
import etch/stdout
import etch/style.{
  attributes, on, on_green, red, reset_color, underline, with, with_on,
}
import etch/terminal
import gleam/list
import gleam/string

pub fn main() {
  stdout.execute([command.Clear(terminal.All), command.MoveTo(0, 0)])
  let line1 = "This example shows how to apply some style to a text."
  let line2 =
    "We can use"
    <> "`with`" |> with(style.BrightRed)
    <> ","
    <> "`on`" |> on(style.BrightGreen)
    <> "and "
    <> "`with_on`." |> with_on(style.Magenta, style.BrightYellow)
  let line3 =
    "(Note that if you want to set both foreground and background, `with_on` is slightly faster than `with` and `on`)."
  let line4 =
    "And we can also apply attributes using `attributes`. This line is bold, italic and with inversed colors."
    |> attributes([style.Bold, style.Italic, style.Inverse])

  stdout.execute([
    command.Println(line1),
    command.Println(line2),
    command.Println(line3),
    command.PrintlnReset(line4),
  ])

  let line5 =
    "As you can see, none of these functions reset colors or attributes. To reset it, we called PrintlnReset command at the previous line."
  stdout.execute([command.Println(line5)])
  let line6 =
    "If we call commands like "
    <> "`red`" |> red()
    <> ", "
    <> "`on_green`" |> on_green()
    <> ", "
    <> "`underline`" |> underline()
    <> ", they reset their color/attribute after calling them."
  stdout.execute([command.Println(line6)])

  let line7 =
    "Commands that do not reset applied style are still useful."
    <> "We can define some function that utilizes this feature. Like "
    <> "`make_rainbow`" |> make_rainbow()
    <> " It's a neat function!"
  let line8 = "HI FROM ETCH! YAAAAAAAAAAAAAY!" |> make_rainbow()
  stdout.execute([command.Println(line7), command.Println(line8)])
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
  |> reset_color()
}
