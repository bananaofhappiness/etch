import command
import event.{Key, Mouse, Resize, init_event_server}
import gleam/int
import gleam/option.{None, Some}
import stdout
import terminal

fn main() {
  stdout.execute([
    command.EnterRaw,
    command.Clear(terminal.All),
    command.EnableMouseCapture,
    command.HideCursor,
  ])
  init_event_server()
  loop()
}

fn loop() -> a {
  // process.sleep(16)
  handle_input()
  loop()
}

fn handle_input() {
  case event.read() {
    Some(Key(s)) ->
      stdout.execute([
        command.MoveTo(0, 0),
        command.Clear(terminal.FromCursorDown),
        command.Print("Got key event: \"" <> s <> "\""),
      ])
    Some(Mouse(m)) -> {
      let kind = m.kind
      let row = m.row
      let column = m.column
      let modifiers = m.modifiers
      stdout.execute([
        command.MoveTo(0, 0),
        command.Clear(terminal.FromCursorDown),
        command.Println("Got mouse event"),
        command.Println("Kind: " <> kind_to_string(kind)),
        command.Println("Row: " <> int.to_string(row)),
        command.Println("Column: " <> int.to_string(column)),
        command.Println("Modifiers: " <> modifiers_to_string(modifiers)),
      ])
    }
    Some(Resize(c, r)) -> {
      stdout.execute([
        command.MoveTo(0, 0),
        command.Clear(terminal.FromCursorDown),
        command.Println("Window resized. Current size: "),
        command.Println("Columns: " <> int.to_string(c)),
        command.Println("Rows: " <> int.to_string(r)),
      ])
    }
    Some(ev) -> {
      echo ev
      Nil
    }
    None -> Nil
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
