import event.{
  Char, DisambiguateEscapeCode, Down, DownArrow, Drag, End, Enter, F,
  FailedToParseEvent, FocusGained, FocusLost, Home, Key, KeyEvent, KeyEventState,
  Left, LeftArrow, LeftShift, Media, Middle, Modifier, Modifiers, Mouse,
  MouseEvent, Moved, Play, Press, Release, Repeat, ReportEventTypes, Right, Up,
  UpArrow, handle_escape_code, parse_cb, parse_cursor_position, parse_events,
  parse_keyboard_enhancement_flags, parse_kind, parse_modifier_and_kind,
  parse_modifier_key_code, parse_modifier_to_state, parse_modifiers,
  parse_normal_mouse, parse_rxvt_mouse, parse_sgr_mouse, parse_special_key_code,
  parse_u_encoded_key_code, starts_with_number, translate_functional_key_code,
}
import gleam/option.{None, Some}
import gleeunit

pub fn main() -> Nil {
  gleeunit.main()
}

// Test parse_events function
pub fn parse_events_test() {
  // Test basic character input
  let result = parse_events(["a", "b", "c"], "", [], False)
  let expected = [
    Ok(
      Key(KeyEvent(
        Char("a"),
        Modifiers(
          shift: False,
          alt: False,
          control: False,
          super: False,
          hyper: False,
          meta: False,
        ),
        Press,
        KeyEventState(False, False, False),
        "",
      )),
    ),
    Ok(
      Key(KeyEvent(
        Char("b"),
        Modifiers(
          shift: False,
          alt: False,
          control: False,
          super: False,
          hyper: False,
          meta: False,
        ),
        Press,
        KeyEventState(False, False, False),
        "",
      )),
    ),
    Ok(
      Key(KeyEvent(
        Char("c"),
        Modifiers(
          shift: False,
          alt: False,
          control: False,
          super: False,
          hyper: False,
          meta: False,
        ),
        Press,
        KeyEventState(False, False, False),
        "",
      )),
    ),
  ]
  assert result == expected

  // Test escape sequence
  let result = parse_events(["\u{001b}", "[", "A"], "", [], False)
  let expected = [
    Ok(
      Key(KeyEvent(
        UpArrow,
        Modifiers(
          shift: False,
          alt: False,
          control: False,
          super: False,
          hyper: False,
          meta: False,
        ),
        Press,
        KeyEventState(False, False, False),
        "",
      )),
    ),
  ]
  assert result == expected
}

// Test handle_escape_code function
pub fn handle_escape_code_test() {
  // Test arrow keys
  assert handle_escape_code("A")
    == Ok(
      Key(KeyEvent(
        UpArrow,
        Modifiers(
          shift: False,
          alt: False,
          control: False,
          super: False,
          hyper: False,
          meta: False,
        ),
        Press,
        KeyEventState(False, False, False),
        "",
      )),
    )
  assert handle_escape_code("B")
    == Ok(
      Key(KeyEvent(
        DownArrow,
        Modifiers(
          shift: False,
          alt: False,
          control: False,
          super: False,
          hyper: False,
          meta: False,
        ),
        Press,
        KeyEventState(False, False, False),
        "",
      )),
    )
  assert handle_escape_code("P")
    == Ok(
      Key(KeyEvent(
        F(1),
        Modifiers(
          shift: False,
          alt: False,
          control: False,
          super: False,
          hyper: False,
          meta: False,
        ),
        Press,
        KeyEventState(False, False, False),
        "",
      )),
    )
  assert handle_escape_code("1;30S")
    == Ok(
      Key(KeyEvent(
        F(4),
        Modifiers(True, False, True, True, True, False),
        Press,
        KeyEventState(False, False, False),
        "",
      )),
    )

  // Test focus events
  assert handle_escape_code("O") == Ok(FocusLost)
  assert handle_escape_code("I") == Ok(FocusGained)

  // Test unsupported escape code
  assert handle_escape_code("X")
    == Error(FailedToParseEvent("Failed to parse escape code"))
}

// Test parse_cursor_position function
pub fn parse_cursor_position_test() {
  assert parse_cursor_position("10;20R") == Ok(#(10, 20))
  assert parse_cursor_position("1;1R") == Ok(#(1, 1))
  assert parse_cursor_position("0;0R") == Ok(#(0, 0))
}

// Test parse_u_encoded_key_code function
pub fn parse_u_encoded_key_code_test() {
  // Test basic u-encoded key code
  let result = parse_u_encoded_key_code("97;1;97u")
  let expected =
    Ok(
      Key(KeyEvent(
        Char("a"),
        Modifiers(
          shift: False,
          alt: False,
          control: False,
          super: False,
          hyper: False,
          meta: False,
        ),
        Press,
        KeyEventState(False, False, False),
        "a",
      )),
    )
  assert result == expected

  // Test with modifiers
  let result = parse_u_encoded_key_code("97;2;65u")
  let expected =
    Ok(
      Key(KeyEvent(
        Char("a"),
        Modifiers(
          shift: True,
          alt: False,
          control: False,
          super: False,
          hyper: False,
          meta: False,
        ),
        Press,
        KeyEventState(False, False, False),
        "A",
      )),
    )
  assert result == expected
}

// Test translate_functional_key_code function
pub fn translate_functional_key_code_test() {
  // Test function key
  assert translate_functional_key_code("57414")
    == Some(#(Enter, KeyEventState(False, False, True)))
  assert translate_functional_key_code("57417")
    == Some(#(LeftArrow, KeyEventState(False, False, True)))

  // Test media keys
  assert translate_functional_key_code("57428")
    == Some(#(Media(Play), KeyEventState(False, False, False)))

  // Test modifier keys
  assert translate_functional_key_code("57441")
    == Some(#(Modifier(LeftShift), KeyEventState(False, False, False)))

  // Test unknown code
  assert translate_functional_key_code("99999") == None
}

// Test parse_special_key_code function
pub fn parse_special_key_code_test() {
  // Test home/end keys
  assert parse_special_key_code("1~")
    == Ok(
      Key(KeyEvent(
        Home,
        Modifiers(
          shift: False,
          alt: False,
          control: False,
          super: False,
          hyper: False,
          meta: False,
        ),
        Press,
        KeyEventState(False, False, False),
        "",
      )),
    )
  assert parse_special_key_code("4~")
    == Ok(
      Key(KeyEvent(
        End,
        Modifiers(
          shift: False,
          alt: False,
          control: False,
          super: False,
          hyper: False,
          meta: False,
        ),
        Press,
        KeyEventState(False, False, False),
        "",
      )),
    )

  // Test function keys with modifiers
  assert parse_special_key_code("11;6~")
    == Ok(
      Key(KeyEvent(
        F(1),
        Modifiers(
          shift: True,
          alt: False,
          control: True,
          super: False,
          hyper: False,
          meta: False,
        ),
        Press,
        KeyEventState(False, False, False),
        "",
      )),
    )
}

// Test parse_modifier_to_state function
pub fn parse_modifier_to_state_test() {
  assert parse_modifier_to_state(0) == KeyEventState(False, False, False)
  assert parse_modifier_to_state(65)
    == KeyEventState(capslock: True, numlock: False, keypad: False)
  assert parse_modifier_to_state(129)
    == KeyEventState(capslock: False, numlock: True, keypad: False)
}

// Test parse_rxvt_mouse function
pub fn parse_rxvt_mouse_test() {
  let result = parse_rxvt_mouse("32;10;20M")
  let expected =
    Ok(
      Mouse(MouseEvent(
        Drag(Left),
        9,
        19,
        Modifiers(
          shift: False,
          alt: False,
          control: False,
          super: True,
          hyper: False,
          meta: False,
        ),
      )),
    )
  assert result == expected
}

// Test starts_with_number function
pub fn starts_with_number_test() {
  assert starts_with_number("123abc") == True
  assert starts_with_number("abc123") == False
  assert starts_with_number("0test") == True
  assert starts_with_number("") == False
}

// Test parse_keyboard_enhancement_flags function
pub fn parse_keyboard_enhancement_flags_test() {
  assert parse_keyboard_enhancement_flags("1u") == [DisambiguateEscapeCode]
  assert parse_keyboard_enhancement_flags("3u")
    == [ReportEventTypes, DisambiguateEscapeCode]
  assert parse_keyboard_enhancement_flags("0u") == []
}

// Test parse_modifier_key_code function
pub fn parse_modifier_key_code_test() {
  let result = parse_modifier_key_code("1;2A")
  let expected =
    Ok(
      Key(KeyEvent(
        UpArrow,
        Modifiers(
          shift: True,
          alt: False,
          control: False,
          super: False,
          hyper: False,
          meta: False,
        ),
        Press,
        KeyEventState(False, False, False),
        "",
      )),
    )
  assert result == expected
}

// Test parse_kind function
pub fn parse_kind_test() {
  assert parse_kind(1) == Press
  assert parse_kind(2) == Repeat
  assert parse_kind(3) == Release
  assert parse_kind(99) == Press
}

// Test parse_modifier_and_kind function
pub fn parse_modifier_and_kind_test() {
  assert parse_modifier_and_kind("1:2") == Ok(#(1, 2))
  assert parse_modifier_and_kind("5") == Ok(#(5, 0))
}

// Test parse_normal_mouse function
pub fn parse_normal_mouse_test() {
  let result = parse_normal_mouse("212")
  let expected =
    Ok(
      Mouse(MouseEvent(
        Down(Right),
        1,
        2,
        Modifiers(
          shift: False,
          alt: False,
          control: False,
          super: False,
          hyper: False,
          meta: False,
        ),
      )),
    )
  assert result == expected
}

// Test parse_sgr_mouse function
pub fn parse_sgr_mouse_test() {
  let result = parse_sgr_mouse("9;10;20m")
  let expected =
    Ok(
      Mouse(MouseEvent(
        Up(Middle),
        10,
        20,
        Modifiers(
          shift: False,
          alt: True,
          control: False,
          super: False,
          hyper: False,
          meta: False,
        ),
      )),
    )
  assert result == expected
}

// Test parse_cb function
pub fn parse_cb_test() {
  let expected =
    Ok(#(
      Modifiers(
        shift: False,
        alt: False,
        control: False,
        super: False,
        hyper: False,
        meta: False,
      ),
      Down(Left),
    ))
  assert parse_cb("0") == expected

  let expected =
    Ok(#(
      Modifiers(
        shift: False,
        alt: False,
        control: False,
        super: True,
        hyper: False,
        meta: False,
      ),
      Drag(Left),
    ))
  assert parse_cb("32") == expected

  let expected =
    Ok(#(
      Modifiers(
        shift: False,
        alt: False,
        control: False,
        super: True,
        hyper: True,
        meta: False,
      ),
      Moved,
    ))
  assert parse_cb("96") == expected
}

// Test parse_modifiers function
pub fn parse_modifiers_test() {
  assert parse_modifiers(0)
    == Modifiers(False, False, False, False, False, False)
  assert parse_modifiers(1)
    == Modifiers(False, False, False, False, False, False)
  assert parse_modifiers(2)
    == Modifiers(True, False, False, False, False, False)
}
