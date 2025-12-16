import etch/event.{Char, Key}
import etch/terminal
import gleam/erlang/process
import gleam/io
import gleam/javascript/promise
import gleam/option.{Some}

pub fn main() {
  // terminal.enter_raw()
  event.init_event_server()
  loop()
}

fn loop() {
  use event <- promise.await(event.read())

  handle_event(event)

  loop()
}

fn handle_event(event) {
  echo event
}
