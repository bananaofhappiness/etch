import esc.{csi}
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/result
import gleam/string

pub type MouseButton {
  Left
  Right
  Middle
}

pub type Modifiers {
  Modifiers(
    shift: Bool,
    control: Bool,
    alt: Bool,
    super: Bool,
    hyper: Bool,
    meta: Bool,
  )
}

pub type MouseEventKind {
  Down(MouseButton)
  Up(MouseButton)
  Drag(MouseButton)
  Moved
  ScrollDown
  ScrollUp
  ScrollLeft
  ScrollRight
}

pub type MouseEvent {
  MouseEvent(kind: MouseEventKind, column: Int, row: Int, modifiers: Modifiers)
}

pub type Event {
  FocusGained
  FocusLost
  Key(String)
  Mouse(MouseEvent)
  Resize(Int, Int)
  Exit
}

@external(erlang, "io", "get_chars")
fn get_chars(chars: String, n: Int) -> String

@external(erlang, "terminal_ffi", "enable_os_signals")
fn enable_os_signals(rx: Subject(Event)) -> Nil

pub fn init_event_loop() -> Subject(Event) {
  let subject = process.new_subject()
  process.spawn(fn() { input_loop(subject) })
  enable_os_signals(subject)
  subject
}

fn input_loop(subj: Subject(Event)) {
  let char = get_chars("", 1024)
  let event = case char {
    "\u{001b}[<" <> s -> parse_mouse_capture(s)
    s -> Key(s)
  }
  process.send(subj, event)
  input_loop(subj)
}

pub fn enable_mouse_capture() {
  csi
  <> "?1000h"
  <> csi
  <> "?1002h"
  <> csi
  <> "?1003h"
  <> csi
  <> "?1015h"
  <> csi
  <> "?1006h"
}

pub fn disable_mouse_capture() {
  csi
  <> "?1000l"
  <> csi
  <> "?1002l"
  <> csi
  <> "?1003l"
  <> csi
  <> "?1015l"
  <> csi
  <> "?1006l"
}

fn parse_mouse_capture(s: String) -> Event {
  let parts = string.split(s, ";")
  let #(code, column, row) = case parts {
    [code, column, row, ..] -> #(code, column, row)
    [_, _] -> panic as "Unreachable"
    [_] -> panic as "Unreachable"
    [] -> panic as "Unreachable"
  }

  let code = int.parse(code) |> result.unwrap(0)
  let column = int.parse(column) |> result.unwrap(0)
  let is_release = string.ends_with(row, "m")
  let row = string.drop_end(row, 1) |> int.parse() |> result.unwrap(0)

  let button_number =
    int.bitwise_and(code, 0b0000_0011)
    |> int.bitwise_or(
      int.bitwise_and(code, 0b1100_0000) |> int.bitwise_shift_right(4),
    )
  let dragging = int.bitwise_and(code, 0b0010_0000) == 0b0010_0000

  let modifiers = parse_modifiers(code)
  let button = case button_number {
    0 -> Left
    1 -> Middle
    _ -> Right
  }

  let kind = case button_number, is_release, dragging {
    4, _, _ -> ScrollUp
    5, _, _ -> ScrollDown
    6, _, _ -> ScrollLeft
    7, _, _ -> ScrollRight
    c, _, True if c == 3 || c == 4 || c == 5 -> Moved
    _, False, False -> Down(button)
    _, True, False -> Up(button)
    _, False, True -> Drag(button)
    _, _, _ -> panic as "Unsupported"
  }

  Mouse(MouseEvent(kind: kind, modifiers: modifiers, column: column, row: row))
}

fn parse_modifiers(code: Int) -> Modifiers {
  // TODO: DISAMBIGUATE_ESCAPE_CODES
  Modifiers(
    shift: int.bitwise_and(code, 4) != 0,
    control: int.bitwise_and(code, 8) != 0,
    alt: int.bitwise_and(code, 16) != 0,
    super: False,
    hyper: False,
    meta: False,
    // super: int.bitwise_and(code, super_mask) != 0,
  // hyper: int.bitwise_and(code, hyper_mask) != 0,
  // meta: int.bitwise_and(code, meta_mask) != 0,
  )
}

pub fn enable_focus_change() {
  csi <> "?1004h"
}

pub fn disable_focus_change() {
  csi <> "?1004l"
}
