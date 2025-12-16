//// This module provides functionality to capture different events like
//// pressing keys, mouse movement etc.
//// In order to be able to read events, [`init_event_server`](event.html#init_event_server)
//// must be called at the start of the application. See `events` example.
//// ```gleam
//// pub fn main()
////   stdout.execute([
////     command.EnterRaw,
////   ])
////   init_event_server()
////   loop()
////
//// fn loop() {
////   handle_input()
////   loop()
//// }
////
//// fn handle_input() {
////   case event.read() {
////     Some(Ok(Mouse(m))) -> {
////       stdout.execute([
////         command.Println("Got mouse event"),
////       ])
////     }
////     Some(Ok(Key(s))) -> {
////         stdout.execute([
////           command.Println(
////             "Got key event: \"" <> event.to_string(s.code) <> "\"",
////           ),
////         ])
////         }
////       }
////     }
////     Some(Error(_)) -> Nil
////     None -> Nil
////   }
//// }
//// ```

import etch/internal/consts.{csi}
import gleam/int
import gleam/javascript/array.{type Array}
import gleam/javascript/promise.{type Promise}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result.{try}
import gleam/string

/// Event error.
pub type EventError {
  FailedToParseEvent(String)
}

/// Keyboard enhancement flag.
/// See https://sw.kovidgoyal.net/kitty/keyboard-protocol/#progressive-enhancement
pub type KeyboardEnhancementFlag {
  DisambiguateEscapeCode
  ReportEventTypes
  ReportAlternateKeys
  ReportAllKeysAsEscapeCode
  ReportAssociatedText
}

/// Modifiers.
pub type Modifiers {
  /// Modifiers.
  Modifiers(
    shift: Bool,
    alt: Bool,
    control: Bool,
    super: Bool,
    hyper: Bool,
    meta: Bool,
  )
}

/// Mouse button.
pub type MouseButton {
  Left
  Right
  Middle
}

/// Mouse event kind.
pub type MouseEventKind {
  /// Down (pressing).
  Down(MouseButton)
  /// Up (releasing).
  Up(MouseButton)
  /// Drag (moving cursor while holding a button).
  Drag(MouseButton)
  /// Moved cursor.
  Moved
  /// Scroll down.
  ScrollDown
  /// Scroll up.
  ScrollUp
  /// Scroll left.
  ScrollLeft
  /// Scroll right.
  ScrollRight
}

/// Mouse event.
pub type MouseEvent {
  /// Mouse event.
  MouseEvent(kind: MouseEventKind, column: Int, row: Int, modifiers: Modifiers)
}

/// Media key code.
pub type MediaKeyCode {
  Play
  Pause
  PlayPause
  Reverse
  Stop
  FastForward
  Rewind
  TrackNext
  TrackPrevious
  Record
  LowerVolume
  RaiseVolume
  MuteVolume
}

/// Modifier key code.
pub type ModifierKeyCode {
  LeftShift
  LeftControl
  LeftAlt
  LeftSuper
  LeftHyper
  LeftMeta
  RightShift
  RightControl
  RightAlt
  RightSuper
  RightHyper
  RightMeta
  IsoLevel3Shift
  IsoLevel5Shift
}

/// Key code (key pressed).
pub type KeyCode {
  Char(String)
  UpArrow
  LeftArrow
  DownArrow
  RightArrow
  Home
  End
  PageDown
  Insert
  Delete
  PageUp
  KeypadBegin
  Enter
  Backspace
  Esc
  Media(MediaKeyCode)
  Modifier(ModifierKeyCode)
  F(n: Int)
  CapsLock
  ScrollLock
  NumLock
  PrintScreen
  PauseKeyCode
  Tab
  Backtab
  Menu
}

/// Key event kind.
pub type KeyEventKind {
  Press
  Repeat
  Release
}

/// Key event state.
pub type KeyEventState {
  KeyEventState(keypad: Bool, capslock: Bool, numlock: Bool)
}

/// Key event.
pub type KeyEvent {
  KeyEvent(
    code: KeyCode,
    modifiers: Modifiers,
    kind: KeyEventKind,
    state: KeyEventState,
    text: String,
  )
}

/// Event
pub type Event {
  FocusGained
  FocusLost
  Key(KeyEvent)
  Mouse(MouseEvent)
  Resize(Int, Int)
}

@target(erlang)
@external(erlang, "io", "get_chars")
pub fn get_chars(chars: String, n: Int) -> String

@target(javascript)
@external(javascript, "../input/input_ffi.mjs", "get_chars")
pub fn get_chars(chars: String, n: Int) -> Promise(Array(Int))

@target(erlang)
@external(erlang, "event_ffi", "start_link")
fn start_link() -> Nil

@external(erlang, "event_ffi", "push")
@external(javascript, "../input/input_ffi.mjs", "push")
fn push(event: Result(Event, EventError)) -> Nil

/// Checks if there is an [`Event`](event.html#Event) available.
/// Returns None if no events were received within the timeout.
/// See also [`read`](event.html#read).
@external(erlang, "event_ffi", "poll")
@external(javascript, "../input/input_ffi.mjs", "poll")
pub fn poll(timeout: Int) -> Promise(Option(Result(Event, EventError)))

@target(erlang)
/// Checks if there is an [`Event`](event.html#Event) available.
/// Waits forever for an available event.
/// See also [`poll`](event.html#poll).
@external(erlang, "event_ffi", "read")
pub fn read() -> Option(Result(Event, EventError))

@target(javascript)
@external(javascript, "../input/input_ffi.mjs", "read")
pub fn read() -> Promise(Option(Result(Event, EventError)))

@target(erlang)
/// Initializes the event server responsible for listening for events.
pub fn init_event_server() {
  start_link()
  process.spawn(fn() { input_loop() })
}

@target(javascript)
/// Initializes the event server responsible for listening for events.
pub fn init_event_server() {
  input_loop()
}

@target(erlang)
fn input_loop() {
  let str = get_chars("", 1024)
  let str = string.to_graphemes(str)
  let events = parse_events(str, "", [], False) |> list.reverse
  push_events(events)
  input_loop()
}

@target(javascript)
fn input_loop() {
  use bytes <- promise.await(get_chars("", 1024))
  let bytes =
    array.to_list(bytes)
    |> list.map(fn(n) {
      let code =
        string.utf_codepoint(n)
        |> result.lazy_unwrap(fn() {
          let assert Ok(fallback) = string.utf_codepoint(65)
          fallback
        })
      string.from_utf_codepoints([code])
    })
  let events = parse_events(bytes, "", [], False) |> list.reverse
  push_events(events)

  input_loop()
}

fn push_events(events: List(Result(Event, EventError))) {
  case events {
    [] -> Nil
    [e, ..rest] -> {
      push(e)
      push_events(rest)
    }
  }
}

@internal
pub fn parse_events(
  str: List(String),
  esc_acc: String,
  list_acc: List(Result(Event, EventError)),
  in_escape_sequence: Bool,
) -> List(Result(Event, EventError)) {
  case str, in_escape_sequence {
    ["\u{001b}", "[", ..rest], False -> {
      parse_events(rest, "", list_acc, True)
    }
    ["\u{001b}", "[", ..rest], True -> {
      let list_acc = [
        Error(FailedToParseEvent("Unterminated escape sequence")),
        ..list_acc
      ]
      parse_events(rest, "", list_acc, True)
    }
    [c, ..rest], True -> {
      let assert Ok(x) = string.to_utf_codepoints(c) |> list.first()
      let x = string.utf_codepoint_to_int(x)
      case x {
        x if x >= 64 && x <= 126 -> {
          let event = handle_escape_code(esc_acc <> c)
          let list_acc = [event, ..list_acc]
          parse_events(rest, "", list_acc, False)
        }
        _ -> {
          parse_events(rest, esc_acc <> c, list_acc, True)
        }
      }
    }
    [s, ..rest], False -> {
      let list_acc = [Ok(Key(default_key_event(Char(s)))), ..list_acc]
      parse_events(rest, esc_acc <> s, list_acc, False)
    }
    [], _ -> {
      list.reverse(list_acc)
    }
  }
}

fn default_key_event(key_code: KeyCode) -> KeyEvent {
  KeyEvent(
    code: key_code,
    kind: Press,
    modifiers: Modifiers(False, False, False, False, False, False),
    state: KeyEventState(False, False, False),
    text: "",
  )
}

/// Convert [`KeyCode`](event.html#KeyCode) to a string.
pub fn to_string(key_code: KeyCode) -> String {
  case key_code {
    Char(s) -> s
    DownArrow -> "down arrow"
    End -> "end"
    Home -> "home"
    LeftArrow -> "left arrow"
    RightArrow -> "right arrow"
    UpArrow -> "up arrow"
    F(n) -> "F" <> int.to_string(n)
    PageDown -> "page down"
    PageUp -> "page up"
    Insert -> "insert"
    Delete -> "delete"
    CapsLock -> "capslock"
    Enter -> "enter"
    KeypadBegin -> "keypad begin"
    Media(m) ->
      case m {
        FastForward -> "fast forward"
        LowerVolume -> "lower volume"
        MuteVolume -> "mute volume"
        Pause -> "pause"
        Play -> "play"
        PlayPause -> "play-pause"
        RaiseVolume -> "raise volume"
        Record -> "record"
        Reverse -> "reverse"
        Rewind -> "rewind"
        Stop -> "stop"
        TrackNext -> "track next"
        TrackPrevious -> "track previous"
      }
    Menu -> "menu"
    Modifier(m) ->
      case m {
        IsoLevel3Shift -> "iso level-3 shift"
        IsoLevel5Shift -> "iso level-5 shift"
        LeftAlt -> "left alt"
        LeftControl -> "left control"
        LeftHyper -> "left hyper"
        LeftMeta -> "left meta"
        LeftShift -> "left shift"
        LeftSuper -> "left super"
        RightAlt -> "right alt"
        RightControl -> "right control"
        RightHyper -> "right hyper"
        RightMeta -> "right meta"
        RightShift -> "right shift"
        RightSuper -> "right super"
      }
    NumLock -> "num lock"
    PauseKeyCode -> "pause"
    PrintScreen -> "print screen"
    ScrollLock -> "scroll lock"
    Esc -> "escape"
    Backspace -> "backspace"
    Backtab -> "backtab"
    Tab -> "tab"
  }
}

@internal
pub fn handle_escape_code(s: String) -> Result(Event, EventError) {
  case s {
    "A" -> Ok(Key(default_key_event(UpArrow)))
    "B" -> Ok(Key(default_key_event(DownArrow)))
    "C" -> Ok(Key(default_key_event(RightArrow)))
    "D" -> Ok(Key(default_key_event(LeftArrow)))
    "H" -> Ok(Key(default_key_event(Home)))
    "F" -> Ok(Key(default_key_event(End)))
    "O" -> Ok(FocusLost)
    "I" -> Ok(FocusGained)
    "M" <> code -> parse_normal_mouse(code)
    "<" <> code -> parse_sgr_mouse(code)
    ";" <> code -> parse_modifier_key_code(code)
    "P" -> Ok(Key(default_key_event(F(1))))
    "Q" -> Ok(Key(default_key_event(F(2))))
    "S" -> Ok(Key(default_key_event(F(4))))
    "?" <> _s -> {
      // case string.last(s) {
      //   // Ok("u") -> parse_keyboard_enhancement_flags(s)
      //   // Ok("s") -> parse_primary_device_attributes(s)
      //   _ -> Error(FailedToParseEvent("Failed to parse escape code"))
      // }
      Error(FailedToParseEvent("Failed to parse escape code"))
    }
    s ->
      case starts_with_number(s) {
        True -> {
          case string.last(s) {
            Ok("M") -> parse_rxvt_mouse(s)
            Ok("~") -> parse_special_key_code(s)
            Ok("u") -> parse_u_encoded_key_code(s)
            Ok(l) if l != "R" -> parse_modifier_key_code(s)
            _ -> Error(FailedToParseEvent("Unsupported numbered escape code"))
          }
        }
        False -> {
          Error(FailedToParseEvent("Failed to parse escape code"))
        }
      }
  }
}

@target(erlang)
/// Returns cursor position.
/// Do not call this in a loop. You still can call it in a loop, if some condition is true
/// (like when user pressed a key you return the position), but don't call it
/// every loop iteration.
pub fn get_cursor_position() -> Result(#(Int, Int), EventError) {
  io.print(csi <> "6n")
  let pos = get_chars("", 32)
  case pos {
    "\u{001b}[" <> s -> {
      case string.last(s) {
        Ok("R") -> parse_cursor_position(s)
        _ -> Error(FailedToParseEvent("Could not get cursor position"))
      }
    }
    _ -> Error(FailedToParseEvent("Could not get cursor position"))
  }
}

@target(javascript)
@external(javascript, "../input/input_ffi.mjs", "get_cursor_position")
pub fn get_cursor_position() -> Promise(Result(#(Int, Int), EventError))

@internal
pub fn parse_cursor_position(s: String) -> Result(#(Int, Int), EventError) {
  let code = string.drop_end(s, 1)
  let split = string.split(code, ";")
  let res = case split {
    [x, y] -> Ok(#(x, y))
    _ -> Error(FailedToParseEvent("Failed to parse cursor position"))
  }
  use #(x, y) <- try(res)
  let x = int.parse(x) |> result.unwrap(0)
  let y = int.parse(y) |> result.unwrap(0)
  Ok(#(x, y))
}

@internal
pub fn parse_u_encoded_key_code(code: String) -> Result(Event, EventError) {
  let code = string.drop_end(code, 1)
  let split = string.split(code, ";")

  let res = case split {
    [code, modifiers, text] -> Ok(#(code, modifiers, Some(text)))
    [code, modifiers] -> Ok(#(code, modifiers, None))
    [code] -> Ok(#(code, "0", None))
    _ ->
      Error(FailedToParseEvent("Failed to parse u encoded code (CSI <..> u)"))
  }
  use #(code, modifiers, text) <- try(res)
  let text = case text {
    Some(text) -> {
      let assert Ok(text) =
        int.parse(text)
        |> result.unwrap(0)
        |> string.utf_codepoint()
      string.from_utf_codepoints([text])
    }
    None -> ""
  }

  let code_parts = string.split(code, ":")
  let res = case code_parts {
    [unicode, alternate_code] -> Ok(#(unicode, Some(alternate_code)))
    [unicode] -> Ok(#(unicode, None))
    _ -> Error(FailedToParseEvent("Failed to parse u encoded code"))
  }
  use #(code, alternate_code) <- try(res)

  use #(modifier_mask, kind_mask) <- try(parse_modifier_and_kind(modifiers))
  let #(modifiers, kind, state_from_modifier) = #(
    parse_modifiers(modifier_mask),
    parse_kind(kind_mask),
    parse_modifier_to_state(modifier_mask),
  )
  let #(keycode, state_from_keycode) = case
    translate_functional_key_code(code)
  {
    Some(#(keycode, state)) -> #(keycode, state)
    None -> {
      let assert Ok(c) =
        int.parse(code)
        |> result.unwrap(0)
        |> string.utf_codepoint()
      let c = string.from_utf_codepoints([c])
      let keycode = case c {
        "\r" -> Enter
        "\n" -> Enter
        "\u{001b}" -> Esc
        "\u{0007}" -> Backspace
        "\t" if modifiers.shift -> Backtab
        "\t" -> Tab
        c -> Char(c)
      }
      #(keycode, KeyEventState(False, False, False))
    }
  }

  let modifiers = case keycode {
    Modifier(x) if x == LeftAlt || x == RightAlt ->
      Modifiers(..modifiers, alt: True)
    Modifier(x) if x == LeftShift || x == RightShift ->
      Modifiers(..modifiers, shift: True)
    Modifier(x) if x == LeftControl || x == RightControl ->
      Modifiers(..modifiers, control: True)
    Modifier(x) if x == LeftSuper || x == RightSuper ->
      Modifiers(..modifiers, super: True)
    Modifier(x) if x == LeftHyper || x == RightHyper ->
      Modifiers(..modifiers, hyper: True)
    Modifier(x) if x == LeftMeta || x == RightMeta ->
      Modifiers(..modifiers, meta: True)
    _ -> modifiers
  }

  let keycode = case modifiers.shift {
    False -> keycode
    True ->
      case alternate_code {
        Some(c) -> {
          let assert Ok(c) =
            int.parse(c)
            |> result.unwrap(0)
            |> string.utf_codepoint()
          let c = string.from_utf_codepoints([c])
          Char(c)
        }
        None -> keycode
      }
  }
  let state =
    KeyEventState(
      capslock: state_from_modifier.capslock || state_from_keycode.capslock,
      keypad: state_from_modifier.keypad || state_from_keycode.keypad,
      numlock: state_from_modifier.numlock || state_from_keycode.numlock,
    )
  Ok(Key(KeyEvent(keycode, modifiers, kind, state, text)))
}

@internal
pub fn translate_functional_key_code(
  code: String,
) -> Option(#(KeyCode, KeyEventState)) {
  let keycode = case code {
    "57399" -> Some(Char("0"))
    "57400" -> Some(Char("1"))
    "57401" -> Some(Char("2"))
    "57402" -> Some(Char("3"))
    "57403" -> Some(Char("4"))
    "57404" -> Some(Char("5"))
    "57405" -> Some(Char("6"))
    "57406" -> Some(Char("7"))
    "57407" -> Some(Char("8"))
    "57408" -> Some(Char("9"))
    "57409" -> Some(Char("."))
    "57410" -> Some(Char("/"))
    "57411" -> Some(Char("*"))
    "57412" -> Some(Char("-"))
    "57413" -> Some(Char("+"))
    "57414" -> Some(Enter)
    "57415" -> Some(Char("="))
    "57416" -> Some(Char(","))
    "57417" -> Some(LeftArrow)
    "57418" -> Some(RightArrow)
    "57419" -> Some(UpArrow)
    "57420" -> Some(DownArrow)
    "57421" -> Some(PageUp)
    "57422" -> Some(PageDown)
    "57423" -> Some(Home)
    "57424" -> Some(End)
    "57425" -> Some(Insert)
    "57426" -> Some(Delete)
    "57427" -> Some(KeypadBegin)
    _ -> None
  }
  case keycode {
    Some(c) -> Some(#(c, KeyEventState(False, False, True)))
    None -> {
      let keycode = case code {
        "57358" -> Some(CapsLock)
        "57359" -> Some(ScrollLock)
        "57360" -> Some(NumLock)
        "57361" -> Some(PrintScreen)
        "57362" -> Some(PauseKeyCode)
        "57363" -> Some(Menu)
        "57376" -> Some(F(13))
        "57377" -> Some(F(14))
        "57378" -> Some(F(15))
        "57379" -> Some(F(16))
        "57380" -> Some(F(17))
        "57381" -> Some(F(18))
        "57382" -> Some(F(19))
        "57383" -> Some(F(20))
        "57384" -> Some(F(21))
        "57385" -> Some(F(22))
        "57386" -> Some(F(23))
        "57387" -> Some(F(24))
        "57388" -> Some(F(25))
        "57389" -> Some(F(26))
        "57390" -> Some(F(27))
        "57391" -> Some(F(28))
        "57392" -> Some(F(29))
        "57393" -> Some(F(30))
        "57394" -> Some(F(31))
        "57395" -> Some(F(32))
        "57396" -> Some(F(33))
        "57397" -> Some(F(34))
        "57398" -> Some(F(35))
        "57428" -> Some(Media(Play))
        "57429" -> Some(Media(Pause))
        "57430" -> Some(Media(PlayPause))
        "57431" -> Some(Media(Reverse))
        "57432" -> Some(Media(Stop))
        "57433" -> Some(Media(FastForward))
        "57434" -> Some(Media(Rewind))
        "57435" -> Some(Media(TrackNext))
        "57436" -> Some(Media(TrackPrevious))
        "57437" -> Some(Media(Record))
        "57438" -> Some(Media(LowerVolume))
        "57439" -> Some(Media(RaiseVolume))
        "57440" -> Some(Media(MuteVolume))
        "57441" -> Some(Modifier(LeftShift))
        "57442" -> Some(Modifier(LeftControl))
        "57443" -> Some(Modifier(LeftAlt))
        "57444" -> Some(Modifier(LeftSuper))
        "57445" -> Some(Modifier(LeftHyper))
        "57446" -> Some(Modifier(LeftMeta))
        "57447" -> Some(Modifier(RightShift))
        "57448" -> Some(Modifier(RightControl))
        "57449" -> Some(Modifier(RightAlt))
        "57450" -> Some(Modifier(RightSuper))
        "57451" -> Some(Modifier(RightHyper))
        "57452" -> Some(Modifier(RightMeta))
        "57453" -> Some(Modifier(IsoLevel3Shift))
        "57454" -> Some(Modifier(IsoLevel5Shift))
        _ -> None
      }
      case keycode {
        None -> None
        Some(c) -> Some(#(c, KeyEventState(False, False, False)))
      }
    }
  }
}

@internal
pub fn parse_special_key_code(code: String) -> Result(Event, EventError) {
  let code = string.drop_end(code, 1)
  let split = string.split(code, ";")
  let res = case split {
    [key, modifier] -> Ok(#(key, modifier))
    [key] -> Ok(#(key, "0"))
    _ ->
      Error(FailedToParseEvent("Failed to parse special key code (CSI <..> ~)"))
  }
  use #(key, modifier) <- try(res)
  use #(modifier_mask, kind_mask) <- try(parse_modifier_and_kind(modifier))
  let #(modifiers, kind, state) = #(
    parse_modifiers(modifier_mask),
    parse_kind(kind_mask),
    parse_modifier_to_state(modifier_mask),
  )
  let key = int.parse(key) |> result.unwrap(1)
  let key = case key {
    1 | 7 -> Ok(Home)
    2 -> Ok(Insert)
    3 -> Ok(Delete)
    4 | 8 -> Ok(End)
    5 -> Ok(PageUp)
    6 -> Ok(PageDown)
    k -> {
      case k {
        k if k >= 11 && k <= 15 -> Ok(F(k - 10))
        k if k >= 17 && k <= 21 -> Ok(F(k - 11))
        k if k >= 23 && k <= 26 -> Ok(F(k - 12))
        28 | 29 -> Ok(F(k - 15))
        k if k >= 31 && k <= 34 -> Ok(F(k - 17))
        _ ->
          Error(FailedToParseEvent(
            "Failed to parse special key code (CSI <..> ~)",
          ))
      }
    }
  }
  use key <- try(key)
  Ok(
    Key(KeyEvent(
      code: key,
      kind: kind,
      modifiers: modifiers,
      state: state,
      text: "",
    )),
  )
}

@internal
pub fn parse_modifier_to_state(modifier_mask: Int) -> KeyEventState {
  let state = KeyEventState(keypad: False, numlock: False, capslock: False)
  let mask = case modifier_mask - 1 {
    n if n < 0 -> 0
    n -> n
  }
  let state = case int.bitwise_and(mask, 64) != 0 {
    True -> KeyEventState(..state, capslock: True)
    False -> state
  }
  let state = case int.bitwise_and(mask, 128) != 0 {
    True -> KeyEventState(..state, numlock: True)
    False -> state
  }
  state
}

@internal
pub fn parse_rxvt_mouse(s: String) -> Result(Event, EventError) {
  let s = string.drop_end(s, 1)
  let split = string.split(s, ";")
  let res = case split {
    [code, column, row] -> Ok(#(code, column, row))
    _ -> Error(FailedToParseEvent("Failed to parse sgr mouse code"))
  }
  use #(code, column, row) <- try(res)

  let column = int.parse(column) |> result.unwrap(0)
  let row = int.parse(row) |> result.unwrap(0)
  let column = column - 1
  let row = row - 1

  use #(modifiers, kind) <- try(parse_cb(code))
  Ok(
    Mouse(MouseEvent(kind: kind, modifiers: modifiers, column: column, row: row)),
  )
}

@internal
pub fn starts_with_number(s: String) -> Bool {
  string.first(s) |> result.unwrap("") |> int.parse() |> result.is_ok()
}

@target(erlang)
/// Get keyboard enhancement flags. See https://sw.kovidgoyal.net/kitty/keyboard-protocol/#progressive-enhancement
/// Be careful when calling this function inside a loop where you are also listening for events.
/// Some events may be lost.
pub fn get_keyboard_enhancement_flags() -> Result(
  List(KeyboardEnhancementFlag),
  EventError,
) {
  io.print(csi <> "?u")
  let flags = get_chars("", 32)
  case flags {
    "\u{001b}[?" <> s -> {
      case string.last(s) {
        Ok("u") -> Ok(parse_keyboard_enhancement_flags(s))
        _ -> Error(FailedToParseEvent("Could not get enhancment flags"))
      }
    }
    _ -> Error(FailedToParseEvent("Could not get cursor position"))
  }
}

@target(javascript)
pub fn get_keyboard_enhancement_flags() -> Result(
  List(KeyboardEnhancementFlag),
  EventError,
) {
  // io.print(csi <> "?u")
  // let flags = get_chars("", 32) |> array.to_list()
  let flags = []
  case flags {
    [27, 91, ..rest] -> {
      case list.last(rest) {
        //u
        Ok(117) -> {
          let s =
            rest
            |> list.map(fn(n) {
              let code =
                string.utf_codepoint(n)
                |> result.lazy_unwrap(fn() {
                  let assert Ok(fallback) = string.utf_codepoint(48)
                  fallback
                })
              code
            })
            |> string.from_utf_codepoints()
          Ok(parse_keyboard_enhancement_flags(s))
        }
        _ -> Error(FailedToParseEvent("Could not get enhancment flags"))
      }
    }
    _ -> Error(FailedToParseEvent("Could not get enhancment flags"))
  }
}

@internal
pub fn parse_keyboard_enhancement_flags(
  code: String,
) -> List(KeyboardEnhancementFlag) {
  let code = string.drop_end(code, 1) |> int.parse() |> result.unwrap(0)
  let list = []

  let list = case int.bitwise_and(code, 1) != 0 {
    True -> [DisambiguateEscapeCode, ..list]
    False -> list
  }
  let list = case int.bitwise_and(code, 2) != 0 {
    True -> [ReportEventTypes, ..list]
    False -> list
  }
  let list = case int.bitwise_and(code, 4) != 0 {
    True -> [ReportAlternateKeys, ..list]
    False -> list
  }
  let list = case int.bitwise_and(code, 8) != 0 {
    True -> [ReportAllKeysAsEscapeCode, ..list]
    False -> list
  }
  let list = case int.bitwise_and(code, 16) != 0 {
    True -> [ReportAssociatedText, ..list]
    False -> list
  }
  list
}

@internal
pub fn parse_modifier_key_code(code: String) -> Result(Event, EventError) {
  let key = string.last(code) |> result.unwrap("fallback")
  let code = string.drop_end(code, 1)

  let split = string.split(code, ";")
  let res = case split {
    [_, modifiers] -> Ok(modifiers)
    _ ->
      Error(FailedToParseEvent("Failed to parse special key code (CSI <..> ~)"))
  }
  use modifiers <- try(res)

  use #(modifier_mask, kind_mask) <- try(parse_modifier_and_kind(modifiers))

  let #(modifiers, kind) = #(
    parse_modifiers(modifier_mask),
    parse_kind(kind_mask),
  )

  let res = case key {
    "A" -> Ok(UpArrow)
    "B" -> Ok(DownArrow)
    "C" -> Ok(RightArrow)
    "D" -> Ok(LeftArrow)
    "F" -> Ok(End)
    "H" -> Ok(Home)
    "P" -> Ok(F(1))
    "Q" -> Ok(F(2))
    "R" -> Ok(F(3))
    "S" -> Ok(F(4))
    _ -> Error(FailedToParseEvent("Failed to parse modifier key code"))
  }
  use key <- try(res)

  Ok(
    Key(KeyEvent(
      code: key,
      kind: kind,
      modifiers: modifiers,
      state: KeyEventState(False, False, False),
      text: "",
    )),
  )
}

@internal
pub fn parse_kind(kind: Int) -> KeyEventKind {
  case kind {
    1 -> Press
    2 -> Repeat
    3 -> Release
    _ -> Press
  }
}

@internal
pub fn parse_modifier_and_kind(code: String) -> Result(#(Int, Int), EventError) {
  let split = string.split(code, ":")
  case split {
    [modifier_mask, kind_mask] -> {
      let modifier_mask = int.parse(modifier_mask) |> result.unwrap(0)
      let kind_mask = int.parse(kind_mask) |> result.unwrap(0)
      Ok(#(modifier_mask, kind_mask))
    }
    [modifier_mask] -> {
      let modifier_mask = int.parse(modifier_mask) |> result.unwrap(0)
      Ok(#(modifier_mask, 0))
    }
    _ -> Error(FailedToParseEvent("Failed to parse modifiers"))
  }
}

/// Enables mouse capture.
/// It is prefered not to use this directly. See [`EnableMouseCapture`](command.html#EnableMouseCapture).
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

/// Disables mouse capture.
/// It is prefered not to use this directly. See [`DisableMouseCapture`](command.html#DisableMouseCapture).
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

@internal
pub fn parse_normal_mouse(code: String) -> Result(Event, EventError) {
  let res = case string.to_graphemes(code) {
    [cb, cx, cy] -> Ok(#(cb, cx, cy))
    _ -> Error(FailedToParseEvent("Failed to parse normal mouse code"))
  }
  use #(cb, cx, cy) <- try(res)
  let column = int.parse(cx) |> result.unwrap(0)
  let row = int.parse(cy) |> result.unwrap(0)
  use #(modifiers, kind) <- try(parse_cb(cb))

  Ok(
    Mouse(MouseEvent(kind: kind, modifiers: modifiers, column: column, row: row)),
  )
}

@internal
pub fn parse_sgr_mouse(s: String) -> Result(Event, EventError) {
  let split = string.split(s, ";")
  let res = case split {
    [code, column, row] -> Ok(#(code, column, row))
    _ -> Error(FailedToParseEvent("Failed to parse sgr mouse code"))
  }
  use #(code, column, row) <- try(res)

  let column = int.parse(column) |> result.unwrap(0)
  let is_release = string.ends_with(row, "m")
  let row = string.drop_end(row, 1) |> int.parse() |> result.unwrap(0)

  use #(modifiers, kind) <- try(parse_cb(code))
  let kind = case is_release {
    True ->
      case kind {
        Down(b) -> Up(b)
        other -> other
      }
    False -> {
      kind
    }
  }

  Ok(
    Mouse(MouseEvent(kind: kind, modifiers: modifiers, column: column, row: row)),
  )
}

@internal
pub fn parse_cb(
  code: String,
) -> Result(#(Modifiers, MouseEventKind), EventError) {
  let code = int.parse(code) |> result.unwrap(0)

  let button_number =
    int.bitwise_and(code, 0b0000_0011)
    |> int.bitwise_or(
      int.bitwise_and(code, 0b1100_0000) |> int.bitwise_shift_right(4),
    )
  let dragging = int.bitwise_and(code, 0b0010_0000) == 0b0010_0000

  let modifiers =
    Modifiers(
      shift: int.bitwise_and(code, 4) != 0,
      alt: int.bitwise_and(code, 8) != 0,
      control: int.bitwise_and(code, 16) != 0,
      super: int.bitwise_and(code, 32) != 0,
      hyper: int.bitwise_and(code, 64) != 0,
      meta: int.bitwise_and(code, 128) != 0,
    )
  let button = case button_number {
    0 -> Left
    1 -> Middle
    _ -> Right
  }

  let res = case button_number, dragging {
    0, False -> Ok(Down(Left))
    1, False -> Ok(Down(Middle))
    2, False -> Ok(Down(Right))
    0, True -> Ok(Drag(Left))
    1, True -> Ok(Drag(Middle))
    2, True -> Ok(Drag(Right))
    3, False -> Ok(Up(Left))
    4, False -> Ok(ScrollUp)
    5, False -> Ok(ScrollDown)
    6, False -> Ok(ScrollLeft)
    7, False -> Ok(ScrollRight)
    c, True if c == 3 || c == 4 || c == 5 -> Ok(Moved)
    _, True -> Ok(Drag(button))
    _, _ -> Error(FailedToParseEvent("Failed to parse sgr mouse code"))
  }
  use kind <- try(res)
  Ok(#(modifiers, kind))
}

@internal
pub fn parse_modifiers(code: Int) -> Modifiers {
  let mask = case code - 1 {
    x if x < 0 -> 0
    x -> x
  }
  Modifiers(
    shift: int.bitwise_and(mask, 1) != 0,
    alt: int.bitwise_and(mask, 2) != 0,
    control: int.bitwise_and(mask, 4) != 0,
    super: int.bitwise_and(mask, 8) != 0,
    hyper: int.bitwise_and(mask, 16) != 0,
    meta: int.bitwise_and(mask, 32) != 0,
  )
}

/// Enables focus change.
/// It is prefered not to use this directly. See [`EnableFocusChange`](command.html#EnableFocusChange).
pub fn enable_focus_change() {
  csi <> "?1004h"
}

/// Disables focus change.
/// It is prefered not to use this directly. See [`DisableFocusChange`](command.html#DisableFocusChange).
pub fn disable_focus_change() {
  csi <> "?1004l"
}

/// Pushes keyboard enhancement flags.
/// It is prefered not to use this directly. See [`PushKeyboardEnhancementFlags`](command.html#PushKeyboardEnhancementFlags).
pub fn push_keyboard_enhancement_flags(flags: List(KeyboardEnhancementFlag)) {
  push_keyboard_enhancement_flags_inner(flags, 0)
}

fn push_keyboard_enhancement_flags_inner(
  flags: List(KeyboardEnhancementFlag),
  acc: Int,
) {
  case flags {
    [DisambiguateEscapeCode, ..rest] ->
      push_keyboard_enhancement_flags_inner(rest, acc + 1)
    [ReportEventTypes, ..rest] ->
      push_keyboard_enhancement_flags_inner(rest, acc + 2)
    [ReportAlternateKeys, ..rest] ->
      push_keyboard_enhancement_flags_inner(rest, acc + 4)
    [ReportAllKeysAsEscapeCode, ..rest] ->
      push_keyboard_enhancement_flags_inner(rest, acc + 8)
    [ReportAssociatedText, ..rest] ->
      push_keyboard_enhancement_flags_inner(rest, acc + 16)
    [] -> {
      csi <> ">" <> int.to_string(acc) <> "u"
    }
  }
}

/// Pops keyboard enhancement flags.
/// It is prefered not to use this directly. See [`PopKeyboardEnhancementFlags`](command.html#PopKeyboardEnhancementFlags).
pub fn pop_keyboard_enhancement_flags() {
  csi <> "<1u"
}
