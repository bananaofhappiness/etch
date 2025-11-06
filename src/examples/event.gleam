import command
import event.{type Event, Key, Mouse, init_event_loop}
import gleam/erlang/process.{type Subject}
import gleam/io
import stdout
import terminal

pub fn main() {
  stdout.execute([
    command.Clear(terminal.All),
    command.EnableMouseCapture,
    command.EnterRaw,
  ])
  let rx = init_event_loop()
  loop(rx)
}

fn loop(rx: Subject(Event)) -> a {
  handle_input(rx)
  loop(rx)
}

fn handle_input(rx: Subject(Event)) {
  case process.receive(rx, 1) {
    Ok(Key(s)) ->
      stdout.execute([
        command.Clear(terminal.All),
        command.MoveTo(0, 10),
        command.Print(s),
      ])
    Ok(Mouse(m)) -> {
      echo m
      Nil
    }
    Ok(_) -> Nil
    Error(_) -> Nil
  }
}
