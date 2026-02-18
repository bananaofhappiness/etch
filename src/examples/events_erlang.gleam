//// This example shows how to handle user inputs and events.

@target(erlang)
import etch/command
@target(erlang)
import etch/cursor
@target(erlang)
import etch/event.{
  Char, Esc, FocusGained, FocusLost, Key, Mouse, Resize, init_event_server,
}
@target(erlang)
import etch/stdout
@target(erlang)
import etch/terminal
@target(erlang)
import gleam/int
@target(erlang)
import gleam/option.{None, Some}

@target(erlang)
@external(erlang, "erlang", "halt")
fn halt(n: Int) -> Nil

@target(javascript)
pub fn main() {
  panic as "This is a placeholder so that `gleam publish` does not complain about empty module."
}

@target(erlang)
pub fn main() {
  // Raw mode disables terminal input/output processing so the program
  // receives each keystroke immediately as raw bytes (no echo, line buffering, or special handling).
  terminal.enter_raw()
  stdout.execute([
    command.EnableMouseCapture,
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

@target(erlang)
fn loop() {
  handle_input()
  loop()
}

@target(erlang)
const default_text = "Press Escape to exit
Press R to get current cursor position
Press F to get keyboard enhancement flags\n"

@target(erlang)
fn handle_input() {
  // We call `event.read()` to wait for available input.
  // It blocks program execution until an event is received.
  // This is exactly what we need, because the project has no logic
  // running constantly in the background. We only need to update the screen
  // when its size changes.
  let event = event.read()
  case event {
    // the rest of the code speaks for itself.
    Some(Ok(Mouse(m))) -> {
      stdout.execute([
        command.MoveTo(0, 0),
        command.Clear(terminal.FromCursorDown),
        command.Println(default_text),
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
        command.Println(default_text),
        command.Println("Window resized. Current size: "),
        command.Println("Columns: " <> int.to_string(c)),
        command.Println("Rows: " <> int.to_string(r)),
      ])
    Some(Ok(FocusGained)) ->
      stdout.execute([
        command.MoveTo(0, 0),
        command.Clear(terminal.FromCursorDown),
        command.Println(default_text),
        command.Println("Focus gained."),
      ])
    Some(Ok(FocusLost)) ->
      stdout.execute([
        command.MoveTo(0, 0),
        command.Clear(terminal.FromCursorDown),
        command.Println(default_text),
        command.Println("Focus lost."),
      ])
    Some(Ok(Key(s))) -> {
      case s.code {
        Esc -> {
          stdout.execute([
            command.DisableMouseCapture,
            command.Clear(terminal.All),
            command.SetCursorStyle(cursor.SteadyBar),
            command.DisableFocusChange,
            command.PopKeyboardEnhancementFlags,
          ])
          halt(0)
        }
        Char("R") if s.kind == event.Press -> {
          case event.get_cursor_position() {
            Ok(#(x, y)) -> {
              let x = int.to_string(x)
              let y = int.to_string(y)
              stdout.execute([
                command.Println("Cursor position:" <> x <> "," <> y),
              ])
            }
            Error(event.FailedToParseEvent(e)) -> {
              stdout.execute([
                command.Println(e),
              ])
            }
          }
          Nil
        }
        Char("R") -> Nil
        Char("F") -> {
          case event.get_keyboard_enhancement_flags() {
            Ok(f) -> {
              stdout.execute([
                command.MoveTo(0, 0),
                command.Clear(terminal.FromCursorDown),
                command.Println(default_text),
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
          stdout.execute([
            command.MoveTo(0, 0),
            command.Clear(terminal.FromCursorDown),
            command.Println(default_text),
            command.Println(
              "Got key event: \"" <> event.to_string(s.code) <> "\"",
            ),
            command.Println("Modifiers: " <> modifiers_to_string(s.modifiers)),
            command.Println("Kind: " <> key_event_kind_to_string(s.kind)),
            command.Println("State: " <> key_event_state_to_string(s.state)),
            command.Println("Text: " <> s.text),
          ])
          stdout.execute([
            command.Println("\nDebug information:"),
          ])
          echo event
          Nil
        }
      }
    }
    Some(Error(_)) -> Nil
    None -> Nil
  }
}

// Functions below are use to display events data, nothing interesting.

@target(erlang)
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

@target(erlang)
fn key_event_kind_to_string(key_event_kind: event.KeyEventKind) -> String {
  case key_event_kind {
    event.Press -> "Press"
    event.Release -> "Release"
    event.Repeat -> "Repeat"
  }
}

@target(erlang)
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

@target(erlang)
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

@target(erlang)
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

@target(erlang)
fn button_to_string(button: event.MouseButton) -> String {
  case button {
    event.Left -> "Left Mouse Button"
    event.Right -> "Right Mouse Button"
    event.Middle -> "Middle Mouse Button"
  }
}
