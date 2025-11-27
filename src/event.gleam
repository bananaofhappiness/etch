import esc.{csi}
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result.{try}
import gleam/string
import gleam/string_tree

pub type EventError {
  FailedToParseEvent(String)
}

pub type KeyboardEnhancementFlag {
  DisambiguateEscapeCode
  ReportEventTypes
  ReportAlternateKeys
  ReportAllKeysAsEscapeCode
  // ReportAssociatedText
}

pub type Modifiers {
  Modifiers(
    shift: Bool,
    alt: Bool,
    control: Bool,
    super: Bool,
    hyper: Bool,
    meta: Bool,
  )
}

pub type MouseButton {
  Left
  Right
  Middle
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

pub type KeyEventKind {
  Press
  Repeat
  Release
}

pub type KeyEventState {
  KeyEventState(keypad: Bool, capslock: Bool, numlock: Bool)
}

pub type KeyEvent {
  KeyEvent(
    code: KeyCode,
    modifiers: Modifiers,
    kind: KeyEventKind,
    state: KeyEventState,
    // text:
  )
}

pub type Event {
  FocusGained
  FocusLost
  Key(KeyEvent)
  Mouse(MouseEvent)
  Resize(Int, Int)
}

@external(erlang, "io", "get_chars")
fn get_chars(chars: String, n: Int) -> String

@external(erlang, "event_ffi", "start_link")
fn start_link() -> Nil

@external(erlang, "event_ffi", "push")
fn push(event: Result(Event, EventError)) -> Nil

@external(erlang, "event_ffi", "poll")
pub fn poll(timeout: Int) -> Option(Result(Event, EventError))

@external(erlang, "event_ffi", "read")
pub fn read() -> Option(Result(Event, EventError))

pub fn init_event_server() {
  start_link()
  process.spawn(fn() { input_loop() })
}

fn input_loop() {
  let char = get_chars("", 1024)
  let event = case char {
    "\u{001b}[" <> s -> {
      handle_escape_code(s)
    }
    s -> Ok(Key(default_key_event(Char(s))))
  }
  push(event)
  input_loop()
}

fn default_key_event(key_code: KeyCode) -> KeyEvent {
  KeyEvent(
    code: key_code,
    kind: Press,
    modifiers: Modifiers(False, False, False, False, False, False),
    state: KeyEventState(False, False, False),
  )
}

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

fn handle_escape_code(s: String) -> Result(Event, EventError) {
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
    "?" <> s -> {
      case string.last(s) {
        Ok("u") -> parse_keyboard_enhancement_flags(s)
        Ok("s") -> parse_primary_device_attributes(s)
        _ -> Error(FailedToParseEvent("Failed to parse escape code"))
      }
    }
    s ->
      case starts_with_number(s) {
        True -> {
          case string.last(s) {
            Ok("M") -> parse_rxvt_mouse(s)
            Ok("~") -> parse_special_key_code(s)
            Ok("u") -> parse_u_encoded_key_code(s)
            Ok("R") -> parse_cursor_position(s)
            Ok(_) -> parse_modifier_key_code(s)
            _ -> Error(FailedToParseEvent("Unsupported numbered escape code"))
          }
        }
        False -> {
          Error(FailedToParseEvent("Failed to parse escape code"))
        }
      }
  }
}

fn parse_cursor_position(s: String) -> Result(Event, EventError) {
  // let split = string.split(s, ";")
  todo as "Todo cursor position parsing"
}

fn parse_u_encoded_key_code(code: String) -> Result(Event, EventError) {
  let code = string.drop_end(code, 1)
  let split = string.split(code, ";")

  let res = case split {
    [code, modifiers, text] -> Ok(#(code, modifiers, Some(text)))
    [code, modifiers] -> Ok(#(code, modifiers, None))
    _ ->
      Error(FailedToParseEvent("Failed to parse u encoded code (CSI <..> u)"))
  }
  use #(code, modifiers, _text) <- try(res)

  let code_parts = string.split(code, ":")
  let res = case code_parts {
    [unicode, alternate_code] -> Ok(#(unicode, Some(alternate_code)))
    [unicode] -> Ok(#(unicode, None))
    _ -> Error(FailedToParseEvent("Failed to parse u encoded code"))
  }
  use #(code, alternate_code) <- try(res)

  let #(modifier_mask, kind_mask) = parse_modifier_and_kind(modifiers)
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
      let keycode = case code {
        "\r" -> Enter
        "\n" -> Enter
        "\u{001b}" -> Esc
        "\u{0007}" -> Backspace
        "\t" if modifiers.shift -> Backtab
        "\t" -> Tab
        c -> {
          let assert Ok(c) =
            int.parse(c)
            |> result.unwrap(0)
            |> string.utf_codepoint()
          let c = string.from_utf_codepoints([c])
          Char(c)
        }
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
  Ok(Key(KeyEvent(keycode, modifiers, kind, state)))
}

fn translate_functional_key_code(
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
    Some(c) ->
      Some(#(c, KeyEventState(capslock: False, numlock: False, keypad: True)))
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

fn parse_special_key_code(code: String) -> Result(Event, EventError) {
  let code = string.drop_end(code, 1)
  let split = string.split(code, ";")
  let res = case split {
    [a, b] -> Ok(#(a, b))
    _ ->
      Error(FailedToParseEvent("Failed to parse special key code (CSI <..> ~)"))
  }
  use #(key, modifier) <- try(res)
  let #(modifier_mask, kind_mask) = parse_modifier_and_kind(modifier)
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
  Ok(Key(KeyEvent(code: key, kind: kind, modifiers: modifiers, state: state)))
}

fn parse_modifier_to_state(modifier_mask: Int) -> KeyEventState {
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

fn parse_rxvt_mouse(s: String) -> Result(Event, EventError) {
  todo
}

fn starts_with_number(s: String) -> Bool {
  string.first(s) |> result.unwrap("") |> int.parse() |> result.is_ok()
}

fn parse_primary_device_attributes(s: String) -> Result(Event, EventError) {
  todo as "Introduce internal events (if it is really needed)"
}

fn parse_keyboard_enhancement_flags(s: String) -> Result(Event, EventError) {
  todo as "Introduce internal events (if it is really needed)"
}

fn parse_modifier_key_code(code: String) -> Result(Event, EventError) {
  let key = string.last(code) |> result.unwrap("1")
  let code = string.drop_end(code, 1)
  let #(modifier_mask, kind_mask) = parse_modifier_and_kind(code)
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
    )),
  )
}

fn parse_kind(kind: Int) -> KeyEventKind {
  case kind {
    1 -> Press
    2 -> Repeat
    3 -> Release
    _ -> Press
  }
}

fn parse_modifier_and_kind(code: String) -> #(Int, Int) {
  let split = string.split(code, ":")
  case split {
    [modifier_mask, kind_mask] -> {
      let modifier_mask = int.parse(modifier_mask) |> result.unwrap(0)
      let kind_mask = int.parse(kind_mask) |> result.unwrap(0)
      #(modifier_mask, kind_mask)
    }
    [modifier_mask] -> {
      let modifier_mask = int.parse(modifier_mask) |> result.unwrap(0)
      #(modifier_mask, 0)
    }
    _ -> panic as "Unreachable"
  }
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

fn parse_normal_mouse(code: String) -> Result(Event, EventError) {
  let res = case string.to_graphemes(code) {
    [cb, cx, cy] -> Ok(#(cb, cx, cy))
    _ -> Error(FailedToParseEvent("Failed to parse normal mouse code"))
  }
  use #(cb, cx, cy) <- try(res)
  let column = int.parse(cx) |> result.unwrap(0)
  let row = int.parse(cy) |> result.unwrap(0)
  use #(modifiers, kind) <- try(parse_cb(code))

  Ok(
    Mouse(MouseEvent(kind: kind, modifiers: modifiers, column: column, row: row)),
  )
}

fn parse_sgr_mouse(s: String) -> Result(Event, EventError) {
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

fn parse_cb(code: String) -> Result(#(Modifiers, MouseEventKind), EventError) {
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
      super: False,
      hyper: False,
      meta: False,
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

fn parse_modifiers(code: Int) -> Modifiers {
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

pub fn enable_focus_change() {
  csi <> "?1004h"
}

pub fn disable_focus_change() {
  csi <> "?1004l"
}

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
    // [ReportAssociatedText, ..rest] ->
    //   push_keyboard_enhancement_flags_inner(rest, acc + 16)
    [] -> {
      csi <> ">" <> int.to_string(acc) <> "u"
    }
  }
}

pub fn pop_keyboard_enhancement_flags() {
  csi <> "<1u"
}
