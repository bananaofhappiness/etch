import command
import event.{type Event, Exit, Key, Mouse, Resize, init_event_loop}
import gleam/erlang/process.{type Subject}
import gleam/int
import stdout
import terminal

pub fn main() {
  stdout.execute([
    command.Clear(terminal.All),
    command.EnableMouseCapture,
    command.HideCursor,
    command.EnterRaw,
  ])
  let rx = init_event_loop()
  loop(rx)
}

fn loop(rx: Subject(Event)) -> a {
  process.sleep(16)
  handle_input(rx)
  // receive_sigwinch()
  loop(rx)
}

fn handle_input(rx: Subject(Event)) {
  case process.receive(rx, 1) {
    Ok(Key(s)) ->
      stdout.execute([
        command.EnterAlternateScreen,
        command.Clear(terminal.All),
        command.MoveTo(0, 0),
        command.Print("Got key event: \"" <> s <> "\""),
      ])
    Ok(Mouse(m)) -> {
      let kind = m.kind
      let row = m.row
      let column = m.column
      let modifiers = m.modifiers
      stdout.execute([
        command.Clear(terminal.All),
        command.MoveTo(0, 0),
        command.Println("Got mouse event"),
        command.Println("Kind: " <> kind_to_string(kind)),
        command.Println("Row: " <> int.to_string(row)),
        command.Println("Column: " <> int.to_string(column)),
        command.Println("Modifiers: " <> modifiers_to_string(modifiers)),
      ])
    }
    Ok(Resize(c, r)) -> {
      stdout.execute([
        command.Clear(terminal.All),
        command.MoveTo(0, 0),
        command.Println("Window resized. Current size: "),
        command.Println("Columns: " <> int.to_string(c)),
        command.Println("Rows: " <> int.to_string(r)),
      ])
    }
    Ok(Exit) -> {
      // stdout.execute([command.LeaveAlternateScreen])
      Nil
    }
    Ok(_) -> Nil
    Error(_) -> Nil
  }
}

fn modifiers_to_string(modifiers: event.Modifiers) -> String {
  let shift = case modifiers.shift {
    True -> "Shift "
    False -> ""
  }
  let control = case modifiers.control {
    True -> "Control "
    False -> ""
  }
  let alt = case modifiers.alt {
    True -> "Alt "
    False -> ""
  }
  shift <> control <> alt
}

fn kind_to_string(kind: event.MouseEventKind) -> String {
  case kind {
    event.Down(button) -> "Pressed " <> button_to_string(button)
    event.Up(button) -> "Released " <> button_to_string(button)
    event.Drag(button) -> "Drag " <> button_to_string(button)
    event.Moved -> "Moved"
    event.ScrollDown -> "ScrollDown"
    event.ScrollUp -> "ScrollUp"
    event.ScrollLeft -> "ScrollLeft"
    event.ScrollRight -> "ScrollRight"
  }
}

fn button_to_string(button: event.MouseButton) -> String {
  case button {
    event.Left -> "Left Mouse Button"
    event.Right -> "Right Mouse Button"
    event.Middle -> "Middle Mouse Button"
  }
}
