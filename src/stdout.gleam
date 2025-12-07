//// This module provides a way to queue and flush commands.
//// ```gleam
//// import stdout.{Queue, queue, flush}
////
//// let q = Queue([])
//// queue(q, [
////   EnterRaw,
////   EnterAlternativeScreen,
////   HideCursor,
////   Println("Hello from Etch"),
//// ])
//// flush(q)
//// ```

import command.{
  type Command, Clear, DisableFocusChange, DisableLineWrap, DisableMouseCapture,
  EnableFocusChange, EnableLineWrap, EnableMouseCapture, EnterAlternateScreen,
  EnterRaw, HideCursor, LeaveAlternateScreen, MoveDown, MoveLeft, MoveRight,
  MoveTo, MoveToColumn, MoveToNextLine, MoveToPreviousLine, MoveToRow, MoveUp,
  PopKeyboardEnhancementFlags, Print, PrintReset, Println, PrintlnReset,
  PushKeyboardEnhancementFlags, ResetAttributes, ResetBackground, ResetColor,
  ResetForeground, ResetStyle, RestorePosition, SavePosition, ScrollDown,
  ScrollUp, SetAttributes, SetBackgroundColor, SetCursorStyle,
  SetForegroundAndBackgroundColors, SetForegroundColor, SetSize, SetStyle,
  SetTitle, ShowCursor,
}
import cursor.{
  hide, move_down, move_left, move_right, move_to, move_to_column,
  move_to_next_line, move_to_previous_line, move_to_row, move_up,
  restore_position, save_position, set_cursor_style, show,
}
import event.{
  disable_focus_change, disable_mouse_capture, enable_focus_change,
  enable_mouse_capture, pop_keyboard_enhancement_flags,
  push_keyboard_enhancement_flags,
}
import gleam/io
import gleam/list
import gleam/string_tree.{type StringTree, append} as stree
import style.{
  attributes, on, reset_attributes, reset_background, reset_color,
  reset_foreground, reset_style, set_style, with, with_on,
}
import terminal.{
  disable_line_wrap, enable_line_wrap, enter_alternative, enter_raw,
  leave_alternative, scroll_down, scroll_up, set_size, set_title,
}

const csi = "\u{001b}["

/// Queue for the [`Commands`](#command.html#Command) to flush.
pub type Queue {
  Queue(commands: List(Command))
}

/// Adds [`Commands`](#command.html#Command)
/// to the [`Queue`](stdout.html#Command).
pub fn queue(queue: Queue, commands: List(Command)) -> Queue {
  Queue(commands: list.append(queue.commands, commands))
}

/// Flushes the [`Queue`](stdout.html#Command).
pub fn flush(queue: Queue) -> Queue {
  let tree = stree.new()
  flush_inner(queue.commands, tree)
  Queue(commands: [])
}

/// Flushes [`Commands`](#command.html#Command) without queueing them beforehand.
pub fn execute(commands: List(Command)) {
  let tree = stree.new()
  flush_inner(commands, tree)
}

fn flush_inner(commands: List(Command), tree: StringTree) -> Nil {
  case commands {
    [] -> io.print(tree |> stree.to_string)
    [Print(str), ..rest] -> flush_inner(rest, tree |> append(str))
    [PrintReset(str), ..rest] ->
      flush_inner(rest, append(tree, str <> csi <> "0m"))
    [Println(str), ..rest] ->
      flush_inner(rest, append(tree, str <> move_to_next_line(1)))
    [PrintlnReset(str), ..rest] ->
      flush_inner(
        rest,
        append(tree, str <> move_to_next_line(1) <> csi <> "0m"),
      )

    // Cursor
    [MoveUp(n), ..rest] -> flush_inner(rest, append(tree, move_up(n)))
    [MoveDown(n), ..rest] -> flush_inner(rest, append(tree, move_down(n)))
    [MoveLeft(n), ..rest] -> flush_inner(rest, append(tree, move_left(n)))
    [MoveRight(n), ..rest] -> flush_inner(rest, append(tree, move_right(n)))
    [MoveToNextLine(n), ..rest] ->
      flush_inner(rest, tree |> append(move_to_next_line(n)))
    [MoveToPreviousLine(n), ..rest] ->
      flush_inner(rest, tree |> append(move_to_previous_line(n)))
    [MoveToColumn(n), ..rest] ->
      flush_inner(rest, tree |> append(move_to_column(n)))
    [MoveToRow(n), ..rest] -> flush_inner(rest, append(tree, move_to_row(n)))
    [MoveTo(x, y), ..rest] -> flush_inner(rest, append(tree, move_to(x, y)))
    [SavePosition, ..rest] -> flush_inner(rest, append(tree, save_position()))
    [RestorePosition, ..rest] ->
      flush_inner(rest, tree |> append(restore_position()))
    [ShowCursor, ..rest] -> flush_inner(rest, append(tree, show()))
    [HideCursor, ..rest] -> flush_inner(rest, append(tree, hide()))
    [SetCursorStyle(s), ..rest] ->
      flush_inner(rest, append(tree, set_cursor_style(s)))

    // terminal
    [ScrollDown(n), ..rest] -> flush_inner(rest, append(tree, scroll_down(n)))
    [ScrollUp(n), ..rest] -> flush_inner(rest, append(tree, scroll_up(n)))
    [Clear(t), ..rest] -> flush_inner(rest, append(tree, terminal.clear(t)))
    [SetSize(x, y), ..rest] -> flush_inner(rest, append(tree, set_size(x, y)))
    [SetTitle(s), ..rest] -> flush_inner(rest, append(tree, set_title(s)))
    [EnableLineWrap, ..rest] ->
      flush_inner(rest, append(tree, enable_line_wrap()))
    [DisableLineWrap, ..rest] ->
      flush_inner(rest, append(tree, disable_line_wrap()))
    [EnterRaw, ..rest] -> {
      enter_raw()
      flush_inner(rest, tree)
    }
    [EnterAlternateScreen, ..rest] ->
      flush_inner(rest, append(tree, enter_alternative()))
    [LeaveAlternateScreen, ..rest] ->
      flush_inner(rest, append(tree, leave_alternative()))

    // event
    [EnableMouseCapture, ..rest] ->
      flush_inner(rest, append(tree, enable_mouse_capture()))
    [DisableMouseCapture, ..rest] ->
      flush_inner(rest, append(tree, disable_mouse_capture()))
    [DisableFocusChange, ..rest] ->
      flush_inner(rest, append(tree, disable_focus_change()))
    [EnableFocusChange, ..rest] ->
      flush_inner(rest, append(tree, enable_focus_change()))

    // TODO: KeyboardEnhancementFlags
    // [PushKeyboardEnhancementFlags(_), ..rest] -> flush_inner(rest, tree)
    // [PopKeyboardEnhancementFlags, ..rest] -> flush_inner(rest, tree)
    [PushKeyboardEnhancementFlags(f), ..rest] ->
      flush_inner(rest, append(tree, push_keyboard_enhancement_flags(f)))
    [PopKeyboardEnhancementFlags, ..rest] ->
      flush_inner(rest, tree |> append(pop_keyboard_enhancement_flags()))

    // style
    [SetForegroundColor(c), ..rest] ->
      flush_inner(rest, append(tree, with("", c)))
    [SetBackgroundColor(c), ..rest] ->
      flush_inner(rest, append(tree, on("", c)))
    [SetForegroundAndBackgroundColors(fg, bg), ..rest] ->
      flush_inner(rest, append(tree, with_on("", fg, bg)))

    [SetStyle(s), ..rest] -> flush_inner(rest, append(tree, set_style(s)))
    [ResetStyle, ..rest] -> flush_inner(rest, append(tree, reset_style("")))
    [ResetColor, ..rest] -> flush_inner(rest, append(tree, reset_color("")))
    [ResetForeground, ..rest] ->
      flush_inner(rest, append(tree, reset_foreground("")))
    [ResetBackground, ..rest] ->
      flush_inner(rest, append(tree, reset_background("")))
    [ResetAttributes, ..rest] ->
      flush_inner(rest, append(tree, reset_attributes("")))
    [SetAttributes(attrs), ..rest] ->
      flush_inner(rest, append(tree, attributes("", attrs)))
  }
}
