import command
import event.{
  Char, FocusGained, FocusLost, Key, Mouse, Resize, init_event_server,
}
import gleam/int
import gleam/option.{None, Some}
import stdout
import terminal

@external(erlang, "erlang", "halt")
fn halt(n: Int) -> Nil

pub fn main() {
  stdout.execute([
    command.EnableMouseCapture,
    command.EnterRaw,
    command.Clear(terminal.All),
    command.HideCursor,
    command.EnableFocusChange,
    command.PushKeyboardEnhancementFlags([
      event.DisambiguateEscapeCode,
      event.ReportAlternateKeys,
      event.ReportEventTypes,
      event.ReportAllKeysAsEscapeCode,
    ]),
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
    Some(Ok(Mouse(m))) -> {
      stdout.execute([
        command.MoveTo(0, 0),
        command.Clear(terminal.FromCursorDown),
        command.Println("Got mouse event"),
        command.Println("Kind: " <> mouse_event_kind_to_string(m.kind)),
        command.Println("Row: " <> int.to_string(m.row)),
        command.Println("Column: " <> int.to_string(m.column)),
        command.Println("Modifiers: " <> modifiers_to_string(m.modifiers)),
      ])
    }
    Some(Ok(Resize(c, r))) ->
      stdout.execute([
        command.MoveTo(0, 0),
        command.Clear(terminal.FromCursorDown),
        command.Println("Window resized. Current size: "),
        command.Println("Columns: " <> int.to_string(c)),
        command.Println("Rows: " <> int.to_string(r)),
      ])
    Some(Ok(FocusGained)) ->
      stdout.execute([
        command.MoveTo(0, 0),
        command.Clear(terminal.FromCursorDown),
        command.Println("Focus gained."),
      ])
    Some(Ok(FocusLost)) ->
      stdout.execute([
        command.MoveTo(0, 0),
        command.Clear(terminal.FromCursorDown),
        command.Println("Focus lost."),
      ])
    Some(Ok(Key(s))) -> {
      case s.code {
        Char("Q") -> halt(0)
        _ -> {
          stdout.execute([
            command.MoveTo(0, 0),
            command.Clear(terminal.FromCursorDown),
            command.Println(
              "Got key event: \"" <> event.to_string(s.code) <> "\"",
            ),
            command.Println("Modifiers: " <> modifiers_to_string(s.modifiers)),
            command.Println("Kind: " <> key_event_kind_to_string(s.kind)),
            command.Println("State: " <> key_event_state_to_string(s.state)),
          ])
          Nil
        }
      }
    }
    Some(Error(_)) -> Nil
    None -> Nil
  }
}

fn key_event_state_to_string(key_event_state: event.KeyEventState) -> String {
  let capslock = case key_event_state.capslock {
    True -> "Capslock "
    False -> ""
  }
  let keypad = case key_event_state.keypad {
    True -> "Keypad "
    False -> ""
  }
  let numlock = case key_event_state.numlock {
    True -> "Numlock "
    False -> ""
  }
  capslock <> keypad <> numlock
}

fn key_event_kind_to_string(key_event_kind: event.KeyEventKind) -> String {
  case key_event_kind {
    event.Press -> "Press"
    event.Release -> "Release"
    event.Repeat -> "Repeat"
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

fn mouse_event_kind_to_string(kind: event.MouseEventKind) -> String {
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
