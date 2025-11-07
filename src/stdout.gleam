import command.{
  type Command, Clear, DisableLineWrap, DisableMouseCapture, EnableLineWrap,
  EnableMouseCapture, EnterAlternateScreen, EnterRaw, HideCursor,
  LeaveAlternateScreen, MoveDown, MoveLeft, MoveRight, MoveTo, MoveToColumn,
  MoveToNextLine, MoveToPreviousLine, MoveToRow, MoveUp, Print, PrintReset,
  Println, PrintlnReset, ResetAttributes, ResetColors, RestorePosition,
  SavePosition, ScrollDown, ScrollUp, SetAttributes, SetBackgroundColor,
  SetCursorStyle, SetForegroundAndBackgroundColors, SetForegroundColor, SetSize,
  SetTitle, ShowCursor,
}
import cursor.{
  hide, move_down, move_left, move_right, move_to, move_to_column,
  move_to_next_line, move_to_previous_line, move_to_row, move_up,
  restore_position, save_position, set_cursor_style, show,
}
import esc.{csi}
import event.{disable_mouse_capture, enable_mouse_capture}
import gleam/io
import gleam/list
import gleam/string_tree.{type StringTree, append} as stree
import style.{attributes, on, reset_attributes, reset_colors, with, with_on}
import terminal.{
  disable_line_wrap, enable_line_wrap, enter_alternative, enter_raw,
  leave_alternative, scroll_down, scroll_up, set_size, set_title,
}

pub type Queue {
  Queue(commands: List(Command))
}

pub fn queue(queue: Queue, commands: List(Command)) -> Queue {
  Queue(commands: list.append(queue.commands, commands))
}

pub fn flush(queue: Queue) -> Queue {
  let tree = stree.new()
  flush_inner(queue.commands, tree)
  Queue(commands: [])
}

fn flush_inner(commands: List(Command), tree: StringTree) -> Nil {
  case commands {
    [] -> io.print(tree |> stree.to_string)
    [Print(str), ..rest] -> flush_inner(rest, tree |> append(str))
    [PrintReset(str), ..rest] ->
      flush_inner(rest, tree |> append(str) |> append(csi <> "0m"))
    [Println(str), ..rest] ->
      flush_inner(rest, tree |> append(str) |> append(move_to_next_line(1)))
    [PrintlnReset(str), ..rest] ->
      flush_inner(
        rest,
        tree
          |> append(str)
          |> append(move_to_next_line(1) <> csi <> "0m"),
      )

    // Cursor
    [MoveUp(n), ..rest] -> flush_inner(rest, tree |> append(move_up(n)))
    [MoveDown(n), ..rest] -> flush_inner(rest, tree |> append(move_down(n)))
    [MoveLeft(n), ..rest] -> flush_inner(rest, tree |> append(move_left(n)))
    [MoveRight(n), ..rest] -> flush_inner(rest, tree |> append(move_right(n)))
    [MoveToNextLine(n), ..rest] ->
      flush_inner(rest, tree |> append(move_to_next_line(n)))
    [MoveToPreviousLine(n), ..rest] ->
      flush_inner(rest, tree |> append(move_to_previous_line(n)))
    [MoveToColumn(n), ..rest] ->
      flush_inner(rest, tree |> append(move_to_column(n)))
    [MoveToRow(n), ..rest] -> flush_inner(rest, tree |> append(move_to_row(n)))
    [MoveTo(x, y), ..rest] -> flush_inner(rest, tree |> append(move_to(x, y)))
    [SavePosition, ..rest] -> flush_inner(rest, tree |> append(save_position()))
    [RestorePosition, ..rest] ->
      flush_inner(rest, tree |> append(restore_position()))
    [ShowCursor, ..rest] -> flush_inner(rest, tree |> append(show()))
    [HideCursor, ..rest] -> flush_inner(rest, tree |> append(hide()))
    [SetCursorStyle(s), ..rest] ->
      flush_inner(rest, tree |> append(set_cursor_style(s)))

    // terminal
    [ScrollDown(n), ..rest] -> flush_inner(rest, tree |> append(scroll_down(n)))
    [ScrollUp(n), ..rest] -> flush_inner(rest, tree |> append(scroll_up(n)))
    [Clear(t), ..rest] -> flush_inner(rest, tree |> append(terminal.clear(t)))
    [SetSize(x, y), ..rest] -> flush_inner(rest, tree |> append(set_size(x, y)))
    [SetTitle(s), ..rest] -> flush_inner(rest, tree |> append(set_title(s)))
    [EnableLineWrap, ..rest] ->
      flush_inner(rest, tree |> append(enable_line_wrap()))
    [DisableLineWrap, ..rest] ->
      flush_inner(rest, tree |> append(disable_line_wrap()))
    [EnterRaw, ..rest] -> {
      enter_raw()
      flush_inner(rest, tree)
    }
    [EnterAlternateScreen, ..rest] ->
      flush_inner(rest, tree |> append(enter_alternative()))
    [LeaveAlternateScreen, ..rest] ->
      flush_inner(rest, tree |> append(leave_alternative()))
    [EnableMouseCapture, ..rest] ->
      flush_inner(rest, tree |> append(enable_mouse_capture()))
    [DisableMouseCapture, ..rest] ->
      flush_inner(rest, tree |> append(disable_mouse_capture()))

    // style
    [SetForegroundColor(c), ..rest] ->
      flush_inner(rest, tree |> append(with("", c)))
    [SetBackgroundColor(c), ..rest] ->
      flush_inner(rest, tree |> append(on("", c)))
    [SetForegroundAndBackgroundColors(fg, bg), ..rest] ->
      flush_inner(rest, tree |> append(with_on("", fg, bg)))
    [ResetColors, ..rest] -> flush_inner(rest, tree |> append(reset_colors("")))
    [ResetAttributes, ..rest] ->
      flush_inner(rest, tree |> append(reset_attributes("")))
    [SetAttributes(attrs), ..rest] ->
      flush_inner(rest, tree |> append(attributes("", attrs)))
  }
}

pub fn execute(commands: List(Command)) {
  let tree = stree.new()
  flush_inner(commands, tree)
}

pub fn println(s: String) {
  io.print(csi <> "1E" <> s)
}
