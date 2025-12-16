//// This example shows how to handle user inputs and events.

@target(javascript)
import etch/command
@target(javascript)
import etch/cursor
@target(javascript)
import etch/event.{
  type Event, type EventError, Char, FocusGained, FocusLost, Key, Mouse, Resize,
  init_event_server,
}
@target(javascript)
import etch/stdout
@target(javascript)
import etch/terminal
@target(javascript)
import gleam/int
@target(javascript)
import gleam/javascript/promise
@target(javascript)
import gleam/option.{type Option, None, Some}

@external(javascript, "./tools.js", "exit")
fn exit(n: Int) -> Nil

@target(javascript)
pub fn main() {
  stdout.execute([
    command.EnableMouseCapture,
    // Raw modeDisables terminal input/output processing so the program
    // receives each keystroke immediately as raw bytes (no echo, line buffering, or special handling).
    command.EnterRaw,
    command.Clear(terminal.All),
    command.SetCursorStyle(cursor.SteadyBar),
    command.EnableFocusChange,
    command.PushKeyboardEnhancementFlags([
      event.DisambiguateEscapeCode,
      event.ReportAlternateKeys,
      event.ReportEventTypes,
      event.ReportAllKeysAsEscapeCode,
      event.ReportAssociatedText,
    ]),
  ])
  // Make sure you init event server before handling user input and events.
  init_event_server()
  loop()
}

@target(javascript)
fn loop() {
  use event <- promise.await(event.read())
  handle_input(event)
  loop()
}

@target(javascript)
fn handle_input(event: Option(Result(Event, EventError))) {
  // We call `event.read()` to wait for available input.
  // It blocks program execution until an event is received.
  // This is exactly what we need, because the project has no logic
  // running constantly in the background. We only need to update the screen
  // when its size changes.

  case event {
    // the rest of the code speaks for itself.
    Some(Ok(Mouse(m))) -> {
      use _ <- promise.new()
      stdout.execute([
        command.MoveTo(0, 0),
        command.Clear(terminal.FromCursorDown),
        command.Println("Press Q to exit"),
        command.Println("Press R to get current cursor position"),
        command.Println("Press F to get keyboard enhancement flags\n"),
        command.Println("Got mouse event"),
        command.Println("Kind: " <> mouse_event_kind_to_string(m.kind)),
        command.Println("Row: " <> int.to_string(m.row)),
        command.Println("Column: " <> int.to_string(m.column)),
        command.Println("Modifiers: " <> modifiers_to_string(m.modifiers)),
      ])
    }
    Some(Ok(Resize(c, r))) -> {
      use _ <- promise.new()
      stdout.execute([
        command.MoveTo(0, 0),
        command.Clear(terminal.FromCursorDown),
        command.Println("Press Q to exit"),
        command.Println("Press R to get current cursor position"),
        command.Println("Press F to get keyboard enhancement flags\n"),
        command.Println("Window resized. Current size: "),
        command.Println("Columns: " <> int.to_string(c)),
        command.Println("Rows: " <> int.to_string(r)),
      ])
    }
    Some(Ok(FocusGained)) -> {
      use _ <- promise.new()
      stdout.execute([
        command.MoveTo(0, 0),
        command.Clear(terminal.FromCursorDown),
        command.Println("Press Q to exit"),
        command.Println("Press R to get current cursor position"),
        command.Println("Press F to get keyboard enhancement flags\n"),
        command.Println("Focus gained."),
      ])
    }
    Some(Ok(FocusLost)) -> {
      use _ <- promise.new()
      stdout.execute([
        command.MoveTo(0, 0),
        command.Clear(terminal.FromCursorDown),
        command.Println("Press Q to exit"),
        command.Println("Press R to get current cursor position"),
        command.Println("Press F to get keyboard enhancement flags\n"),
        command.Println("Focus lost."),
      ])
    }
    Some(Ok(Key(s))) -> {
      case s.code {
        Char("Q") -> {
          use _ <- promise.new()
          exit(0)
        }
        Char("R") if s.kind == event.Press -> {
          use pos <- promise.await(event.get_cursor_position())
          promise.resolve(pos)
          case pos {
            Ok(#(x, y)) -> {
              use _ <- promise.new()
              let x = int.to_string(x)
              let y = int.to_string(y)
              stdout.execute([
                command.Println("Cursor position:" <> x <> "," <> y),
              ])
            }
            Error(event.FailedToParseEvent(e)) -> {
              use _ <- promise.new()
              stdout.execute([
                command.Println(e),
              ])
            }
          }
        }
        Char("R") -> {
          use _ <- promise.new()
          Nil
        }
        Char("F") -> {
          use _ <- promise.new()
          case event.get_keyboard_enhancement_flags() {
            Ok(f) -> {
              stdout.execute([
                command.MoveTo(0, 0),
                command.Clear(terminal.FromCursorDown),
                command.Println("Press Q to exit"),
                command.Println("Press R to get current cursor position"),
                command.Println("Press F to get keyboard enhancement flags\n"),
                command.Println("Flags: " <> flags_to_string(f, "")),
              ])
            }
            Error(event.FailedToParseEvent(e)) -> {
              stdout.execute([
                command.Println(e),
              ])
            }
          }
        }
        _ -> {
          use _ <- promise.new()
          stdout.execute([
            command.MoveTo(0, 0),
            command.Clear(terminal.FromCursorDown),
            command.Println("Press Q to exit"),
            command.Println("Press R to get current cursor position"),
            command.Println("Press F to get keyboard enhancement flags\n"),
            command.Println(
              "Got key event: \"" <> event.to_string(s.code) <> "\"",
            ),
            command.Println("Modifiers: " <> modifiers_to_string(s.modifiers)),
            command.Println("Kind: " <> key_event_kind_to_string(s.kind)),
            command.Println("State: " <> key_event_state_to_string(s.state)),
            command.Println("Text: " <> s.text),
          ])
        }
      }
    }
    Some(Error(_)) -> {
      use _ <- promise.new()
      Nil
    }
    None -> {
      use _ <- promise.new()
      Nil
    }
  }
  promise.resolve(event)
}

// Functions below are use to display events data, nothing interesting.

@target(javascript)
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

@target(javascript)
fn key_event_kind_to_string(key_event_kind: event.KeyEventKind) -> String {
  case key_event_kind {
    event.Press -> "Press"
    event.Release -> "Release"
    event.Repeat -> "Repeat"
  }
}

@target(javascript)
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

@target(javascript)
fn flags_to_string(
  l: List(event.KeyboardEnhancementFlag),
  acc: String,
) -> String {
  case l {
    [] -> acc
    [f, ..rest] -> {
      let f = case f {
        event.DisambiguateEscapeCode -> "DisambiguateEscapeCode"
        event.ReportAllKeysAsEscapeCode -> "ReportAllKeysAsEscapeCode"
        event.ReportAlternateKeys -> "ReportAlternateKeys"
        event.ReportAssociatedText -> "ReportAssociatedText"
        event.ReportEventTypes -> "ReportEventTypes"
      }
      let acc = acc <> f <> " "
      flags_to_string(rest, acc)
    }
  }
}

@target(javascript)
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

@target(javascript)
fn button_to_string(button: event.MouseButton) -> String {
  case button {
    event.Left -> "Left Mouse Button"
    event.Right -> "Right Mouse Button"
    event.Middle -> "Middle Mouse Button"
  }
}
