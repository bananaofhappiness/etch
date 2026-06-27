//// This example shows how to handle user inputs and events.

import etch/command
import etch/cursor
import etch/erlang/input
import etch/erlang/tty
import etch/event.{Char, Esc, FocusGained, FocusLost, Key, Mouse, Resize}
import etch/stdout
import etch/terminal
import gleam/int
import gleam/option.{None, Some}

@external(erlang, "erlang", "halt")
fn exit(n: Int) -> Nil

pub fn main() {
  // Raw mode disables terminal input/output processing so the program
  // receives each keystroke immediately as raw bytes (no echo, line buffering, or special handling).
  let assert Ok(_) = tty.enter_raw()
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
  loop()
}

fn loop() {
  handle_input()
  loop()
}

const default_text1 = "Press Escape to exit"

const default_text2 = "Press R to get current cursor position"

const default_text3 = "Press F to get keyboard enhancement flags\n"

fn handle_input() {
  // We call `input.read()` to wait for available input.
  // It blocks program execution until an event is received.
  // This is exactly what we need, because the project has no logic
  // running constantly in the background.
  let event = input.read()
  case event {
    // the rest of the code speaks for itself.
    Some(Ok(Mouse(m))) -> {
      stdout.execute([
        command.MoveTo(0, 0),
        command.Clear(terminal.FromCursorDown),
        command.Println(default_text1),
        command.Println(default_text2),
        command.Println(default_text3),
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
        command.Println(default_text1),
        command.Println(default_text2),
        command.Println(default_text3),
        command.Println("Window resized. Current size: "),
        command.Println("Columns: " <> int.to_string(c)),
        command.Println("Rows: " <> int.to_string(r)),
      ])
    Some(Ok(FocusGained)) ->
      stdout.execute([
        command.MoveTo(0, 0),
        command.Clear(terminal.FromCursorDown),
        command.Println(default_text1),
        command.Println(default_text2),
        command.Println(default_text3),
        command.Println("Focus gained."),
      ])
    Some(Ok(FocusLost)) ->
      stdout.execute([
        command.MoveTo(0, 0),
        command.Clear(terminal.FromCursorDown),
        command.Println(default_text1),
        command.Println(default_text2),
        command.Println(default_text3),
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
          exit(0)
        }
        Char("R") if s.kind == event.Press -> {
          case input.get_cursor_position() {
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
          case input.get_keyboard_enhancement_flags() {
            Ok(f) -> {
              stdout.execute([
                command.MoveTo(0, 0),
                command.Clear(terminal.FromCursorDown),
                command.Println(default_text1),
                command.Println(default_text2),
                command.Println(default_text3),
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
            command.Println(default_text1),
            command.Println(default_text2),
            command.Println(default_text3),
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
    Some(Error(_)) -> Nil
    None -> Nil
  }
}

// Functions below are use to display events data, nothing interesting.

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
