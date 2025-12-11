import etch/command
import etch/event
import gleam/option.{None, Some}
import gleam/string
import etch/stdout.{execute}
import etch/style
import etch/terminal.{window_size}

pub fn main() {
  execute([command.HideCursor])
  let #(x, y) = window_size()
  draw_centered_text(x, y)
  event.init_event_server()
  loop()
}

fn loop() {
  handle_input()
  loop()
}

fn handle_input() {
  case event.read() {
    Some(Ok(event.Resize(x, y))) -> {
      draw_centered_text(x, y)
    }
    Some(_) -> Nil
    None -> Nil
  }
}

fn draw_centered_text(x: Int, y: Int) {
  let s = "      Hello from Etch!      "
  let len = string.length(s)
  // `with`, `with_on` and `on` do not reset colors so make sure to reset it manually.
  // using `with_on` if you want to set both foreground and background colors is slightly
  // faster than using `with` and `on` separately.
  let s =
    style.with_on(s, style.Black, style.AnsiValue(40)) |> style.reset_color
  // But here I don't do it because calling `style.magenta()` at the end automatically resets the color.
  let red = "█" |> string.repeat(len) |> style.with(style.Rgb(233, 51, 35))
  let orange = "█" |> string.repeat(len) |> style.with(style.Rgb(241, 161, 57))
  let yellow = "█" |> string.repeat(len) |> style.with(style.AnsiValue(226))
  let light_blue =
    "█"
    |> string.repeat(len)
    |> style.with(style.Rgb(74, 163, 228))
  let blue = "█" |> string.repeat(len) |> style.blue()
  let purple = "█" |> string.repeat(len) |> style.magenta()
  let x_offset = len / 2
  let x = x / 2 - x_offset
  execute([
    command.Clear(terminal.All),
    command.MoveTo(x, y / 2 - 3),
    command.Print(red),
    command.MoveTo(x, y / 2 - 2),
    command.Print(orange),
    command.MoveTo(x, y / 2 - 1),
    command.Print(yellow),
    command.MoveTo(x, y / 2),
    command.Print(s),
    command.MoveTo(x, y / 2 + 1),
    command.Print(light_blue),
    command.MoveTo(x, y / 2 + 2),
    command.Print(blue),
    command.MoveTo(x, y / 2 + 3),
    command.Print(purple),
  ])
}
