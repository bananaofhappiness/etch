import command.{
  type Command, DisableBlinking, EnableBlinking, EnterAlternateScreen, EnterRaw,
  HideCursor, LeaveAlternateScreen, MoveDown, MoveLeft, MoveRight, MoveTo,
  MoveToColumn, MoveToNextLine, MoveToPreviousLine, MoveToRow, MoveUp, Print,
  PrintReset, Println, PrintlnReset, SetCursorStyle, ShowCursor,
}
import cursor.{
  move_down, move_left, move_right, move_to, move_to_column, move_to_next_line,
  move_to_previous_line, move_to_row, move_up,
}
import esc.{esc}
import gleam/io
import gleam/list
import gleam/string_tree.{type StringTree, append} as stree
import screen

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
          |> append(move_too_next_line(1) <> esc("[0m")),
      )
    // Cursor
    [MoveUp(n), ..rest] -> {
      todo
    }
    [MoveDown(n), ..rest] -> {
      todo
    }
    [MoveLeft(n), ..rest] -> {
      todo
    }
    [MoveRight(n), ..rest] -> {
      todo
    }
    [MoveToNextLine(n), ..rest] -> {
      todo
    }
    [MoveToPreviousLine(n), ..rest] -> {
      todo
    }
    [MoveToColumn(n), ..rest] -> {
      todo
    }
    [MoveToRow(n), ..rest] -> {
      todo
    }
    [MoveTo(x, y), ..rest] ->
      flush_inner(rest, tree |> append(set_position(x, y)))
    [MoveToNextLine(n), ..rest] ->
      flush_inner(rest, tree |> append(move_to_next_line(n)))
    [EnterRaw, ..rest] -> {
      screen.enter_raw()
      flush_inner(rest, tree)
    }
    [EnterAlternateScreen, ..rest] ->
      flush_inner(rest, tree |> append(screen.enter_alternative()))
    [LeaveAlternateScreen, ..rest] ->
      flush_inner(rest, tree |> append(screen.leave_alternative()))
    [ShowCursor, ..rest] -> flush_inner(rest, tree |> append(show()))
    [HideCursor, ..rest] -> flush_inner(rest, tree |> append(hide()))
  }
}

pub fn execute(commands: List(Command)) {
  let tree = stree.new()
  flush_inner(commands, tree)
}

pub fn println(s: String) {
  io.print(esc("[1E") <> s)
}
