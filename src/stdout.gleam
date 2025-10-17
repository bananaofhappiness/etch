import command.{
  type Command, Clear, EnterAlternateScreen, EnterRaw, HideCursor,
  LeaveAlternateScreen, MoveDown, MoveLeft, MoveRight, MoveTo, MoveToColumn,
  MoveToNextLine, MoveToPreviousLine, MoveToRow, MoveUp, Print, PrintReset,
  Println, PrintlnReset, SetCursorStyle, SetSize, ShowCursor,
}
import cursor.{
  hide, move_down, move_left, move_right, move_to, move_to_column,
  move_to_next_line, move_to_previous_line, move_to_row, move_up,
  set_cursor_style, show,
}
import esc.{esc}
import gleam/io
import gleam/list
import gleam/string_tree.{type StringTree, append} as stree
import terminal

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
      flush_inner(rest, tree |> append(str) |> append(esc("[0m")))
    [Println(str), ..rest] ->
      flush_inner(rest, tree |> append(str) |> append(move_to_next_line(1)))
    [PrintlnReset(str), ..rest] ->
      flush_inner(
        rest,
        tree
          |> append(str)
          |> append(move_to_next_line(1) <> esc("[0m")),
      )

    // Cursor
    [MoveUp(n), ..rest] -> {
      flush_inner(rest, tree |> append(move_up(n)))
    }
    [MoveDown(n), ..rest] -> {
      flush_inner(rest, tree |> append(move_down(n)))
    }
    [MoveLeft(n), ..rest] -> {
      flush_inner(rest, tree |> append(move_left(n)))
    }
    [MoveRight(n), ..rest] -> {
      flush_inner(rest, tree |> append(move_right(n)))
    }
    [MoveToNextLine(n), ..rest] -> {
      flush_inner(rest, tree |> append(move_to_next_line(n)))
    }
    [MoveToPreviousLine(n), ..rest] -> {
      flush_inner(rest, tree |> append(move_to_previous_line(n)))
    }
    [MoveToColumn(n), ..rest] -> {
      flush_inner(rest, tree |> append(move_to_column(n)))
    }
    [MoveToRow(n), ..rest] -> {
      flush_inner(rest, tree |> append(move_to_row(n)))
    }
    [MoveTo(x, y), ..rest] -> flush_inner(rest, tree |> append(move_to(x, y)))
    [ShowCursor, ..rest] -> flush_inner(rest, tree |> append(show()))
    [HideCursor, ..rest] -> flush_inner(rest, tree |> append(hide()))
    [SetCursorStyle(s), ..rest] ->
      flush_inner(rest, tree |> append(set_cursor_style(s)))

    // terminal
    [Clear(t), ..rest] -> flush_inner(rest, tree |> append(terminal.clear(t)))
    [SetSize(x, y), ..rest] ->
      flush_inner(rest, tree |> append(terminal.set_size(x, y)))
    [EnterRaw, ..rest] -> {
      terminal.enter_raw()
      flush_inner(rest, tree)
    }
    [EnterAlternateScreen, ..rest] ->
      flush_inner(rest, tree |> append(terminal.enter_alternative()))
    [LeaveAlternateScreen, ..rest] ->
      flush_inner(rest, tree |> append(terminal.leave_alternative()))
  }
}

pub fn execute(commands: List(Command)) {
  let tree = stree.new()
  flush_inner(commands, tree)
}

pub fn println(s: String) {
  io.print(esc("[1E") <> s)
}
