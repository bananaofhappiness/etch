import command.{
  type Command, EnterAlternateScreen, EnterRaw, HideCursor, LeaveAlternateScreen,
  MoveCursor, MoveToNextLine, Print, PrintReset, Println, PrintlnReset,
  ShowCursor,
}
import cursor
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
      flush_inner(
        rest,
        tree |> append(str) |> append(cursor.move_to_next_line(1)),
      )
    [PrintlnReset(str), ..rest] ->
      flush_inner(
        rest,
        tree
          |> append(str)
          |> append(cursor.move_to_next_line(1) <> esc("[0m")),
      )
    [MoveCursor(x, y), ..rest] ->
      flush_inner(rest, tree |> append(cursor.set_position(x, y)))
    [MoveToNextLine(n), ..rest] ->
      flush_inner(rest, tree |> append(cursor.move_to_next_line(n)))
    [EnterRaw, ..rest] -> {
      screen.enter_raw()
      flush_inner(rest, tree)
    }
    [EnterAlternateScreen, ..rest] ->
      flush_inner(rest, tree |> append(screen.enter_alternative()))
    [LeaveAlternateScreen, ..rest] ->
      flush_inner(rest, tree |> append(screen.leave_alternative()))
    [ShowCursor, ..rest] -> flush_inner(rest, tree |> append(cursor.show()))
    [HideCursor, ..rest] -> flush_inner(rest, tree |> append(cursor.hide()))
  }
}

pub fn execute(commands: List(Command)) {
  let tree = stree.new()
  flush_inner(commands, tree)
}

pub fn println(s: String) {
  io.print(esc("[1E") <> s)
}
