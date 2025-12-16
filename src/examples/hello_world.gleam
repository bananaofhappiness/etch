//// This example shows how to print a hello world on a rainbow background
//// that is always centered (it waits for the `Resize` event and updates
//// the background accordingly).

import etch/command

import etch/event.{type Event, type EventError}
import etch/stdout
import etch/style
import etch/terminal
import gleam/javascript/promise
import gleam/option.{type Option, None, Some}
import gleam/string

pub fn main() {
  // First, we hide cursors
  stdout.execute([command.HideCursor])
  // We get windows size so we can center our rainbow background
  let #(x, y) = terminal.window_size()
  draw_centered_text(x, y)
  // make sure to init event server before listening to events.
  event.init_event_server()
  loop()
}

@target(erlang)
fn loop() {
  handle_events()
  loop()
}

@target(javascript)
fn loop() {
  use event <- promise.await(event.read())
  handle_events(event)
  loop()
}

@target(erlang)
fn handle_events() {
  // We call `event.read()` to wait for available input.
  // It blocks program execution until an event is received.
  // This is exactly what we need, because the project has no logic
  // running constantly in the background. We only need to update the screen
  // when its size changes.
  case event.read() {
    // We only need to check for `Resize` event in this program.
    Some(Ok(event.Resize(x, y))) -> {
      draw_centered_text(x, y)
    }
    Some(_) -> Nil
    None -> Nil
  }
}

@target(javascript)
fn handle_events(event: Option(Result(Event, EventError))) {
  // We call `event.read()` to wait for available input.
  // It blocks program execution until an event is received.
  // This is exactly what we need, because the project has no logic
  // running constantly in the background. We only need to update the screen
  // when its size changes.
  case event {
    // We only need to check for `Resize` event in this program.
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
  // But here we don't reset color because calling `style.magenta()`
  // at the end automatically resets the color.
  let red = "█" |> string.repeat(len) |> style.with(style.Rgb(233, 51, 35))
  let orange = "█" |> string.repeat(len) |> style.with(style.Rgb(241, 161, 57))
  let yellow = "█" |> string.repeat(len) |> style.with(style.AnsiValue(226))
  let light_blue =
    "█"
    |> string.repeat(len)
    |> style.with(style.Rgb(74, 163, 228))
  let blue = "█" |> string.repeat(len) |> style.blue()
  // This `style.magenta()` resets color. If you don't know if the function resets color
  // after applying it, read its documentation.
  let purple = "█" |> string.repeat(len) |> style.magenta()

  let x_offset = len / 2
  let x = x / 2 - x_offset

  // It should be clear from the names of the commands what it does
  // but to make sure you get it, I'll explain you.
  // First, we clear the terminal, then move to the first line (red)
  // which is 3 lines above the center line, then we print it.
  // repeat it for every line.
  stdout.execute([
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
