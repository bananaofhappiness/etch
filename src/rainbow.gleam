import command.{PrintlnReset}
import gleam/erlang/process
import gleam/list
import gleam/string_tree as stree
import stdout.{Queue, queue}
import style.{with}
import terminal

pub fn run(n: Int) {
  process.sleep(400)
  let #(x, y) = terminal.window_size()
  let x_string =
    list.range(0, x)
    |> list.map(make_rainbow(_, n))
    |> list.fold(stree.new(), fn(tree, s) { stree.append(tree, s) })
    |> stree.to_string
  let q = Queue([command.Clear(terminal.All)])
  let q = queue(q, list.repeat(PrintlnReset(x_string), y))
  stdout.flush(q)
  run(n + 1)
}

fn make_rainbow(i: Int, offset: Int) -> String {
  case { i + offset } % 7 {
    0 -> with("█", style.Red)
    1 -> with("█", style.AnsiValue(208))
    2 -> with("█", style.Yellow)
    3 -> with("█", style.Green)
    4 -> with("█", style.AnsiValue(117))
    5 -> with("█", style.Blue)
    6 -> with("█", style.Magenta)
    _ -> panic as "Unreachable"
  }
}
