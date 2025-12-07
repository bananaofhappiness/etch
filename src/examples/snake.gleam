import command
import event.{Char, DownArrow, Key, LeftArrow, RightArrow, UpArrow}
import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import gleam/string_tree as stree
import stdout
import style
import terminal

@external(erlang, "erlang", "halt")
fn halt(n: Int) -> Nil

type Direction {
  Up
  Down
  Left
  Right
}

type State {
  State(
    grid: Dict(Int, Int),
    snake: List(Int),
    rows: Int,
    columns: Int,
    direction: Direction,
    score: Int,
  )
}

pub fn main() {
  stdout.execute([
    command.EnterRaw,
    command.EnterAlternateScreen,
    command.Clear(terminal.All),
    command.HideCursor,
    command.DisableLineWrap,
  ])

  let #(columns, rows) = terminal.window_size()
  let #(columns, rows) = #(columns - 2, rows - 2)
  let grid = make_grid(columns, rows)
  let snake_center = case int.is_even(rows) {
    True -> rows * columns / 2 + columns / 2
    False -> rows * columns / 2
  }
  let state =
    State(
      grid,
      [snake_center, snake_center - 1, snake_center - 2],
      rows,
      columns,
      Right,
      0,
    )
  let state = spawn_fruit(state)
  event.init_event_server()
  loop(state)
}

fn make_grid(columns: Int, rows: Int) -> Dict(Int, Int) {
  let x =
    list.range(0, columns * rows)
    |> list.zip(list.repeat(0, columns * rows))
  let d = dict.from_list(x)
  case int.is_even(rows) {
    True -> dict.insert(d, { rows * columns / 2 } + columns / 2, 1)
    False -> dict.insert(d, rows * columns / 2, 1)
  }
}

fn loop(state: State) {
  process.sleep(200)
  let state = handle_input(state)
  let state = update_state(state)
  draw(state)
  loop(state)
}

fn update_state(state: State) -> State {
  case state.direction {
    Up -> move_up(state)
    Down -> move_down(state)
    Left -> move_left(state)
    Right -> move_right(state)
  }
}

fn spawn_fruit(state: State) -> State {
  let f = int.random(state.rows * state.columns)
  case dict.get(state.grid, f) {
    Ok(1) -> spawn_fruit(state)
    Ok(0) -> {
      let grid = dict.insert(state.grid, f, 2)
      State(..state, grid: grid)
    }
    _ -> panic as "Unreachable"
  }
}

fn handle_input(state: State) -> State {
  case event.poll(1), state.direction {
    Some(Ok(Key(e))), Down if e.code == Char("w") || e.code == UpArrow -> state

    Some(Ok(Key(e))), _ if e.code == Char("w") || e.code == UpArrow ->
      State(..state, direction: Up)

    Some(Ok(Key(e))), Right if e.code == Char("a") || e.code == LeftArrow ->
      state
    Some(Ok(Key(e))), _ if e.code == Char("a") || e.code == LeftArrow ->
      State(..state, direction: Left)

    Some(Ok(Key(e))), Up if e.code == Char("s") || e.code == DownArrow -> state
    Some(Ok(Key(e))), _ if e.code == Char("s") || e.code == DownArrow ->
      State(..state, direction: Down)

    Some(Ok(Key(e))), Left if e.code == Char("d") || e.code == RightArrow ->
      state
    Some(Ok(Key(e))), _ if e.code == Char("d") || e.code == RightArrow ->
      State(..state, direction: Right)

    Some(_), _ -> state
    None, _ -> state
  }
}

fn move_right(state: State) -> State {
  let assert Ok(head) = list.first(state.snake)
  let new_head = case head {
    n if n % state.columns == state.columns - 1 -> {
      lose(state)
      0
    }
    n -> n + 1
  }
  handle_new_head(state, new_head)
}

fn move_down(state: State) -> State {
  let assert Ok(head) = list.first(state.snake)
  let new_head = case head + state.columns {
    n if n > state.columns * state.rows -> {
      lose(state)
      0
    }
    n -> n
  }
  handle_new_head(state, new_head)
}

fn move_left(state: State) -> State {
  let assert Ok(head) = list.first(state.snake)
  let new_head = case head {
    n if n % state.columns == 0 -> {
      lose(state)
      0
    }
    n -> n - 1
  }
  handle_new_head(state, new_head)
}

fn move_up(state: State) -> State {
  let assert Ok(head) = list.first(state.snake)
  let new_head = case head - state.columns {
    n if n < 0 -> {
      lose(state)
      0
    }
    n -> n
  }
  handle_new_head(state, new_head)
}

fn remove_last_snake_block(state: State, new_head: Int) -> State {
  let snake = [new_head, ..state.snake] |> list.reverse()
  let #(last, snake) = case snake {
    [last, ..rest] -> #(last, rest)
    _ -> panic as "Unreachable"
  }
  let grid = dict.insert(state.grid, last, 0)
  let grid = dict.insert(grid, new_head, 1)
  let snake = list.reverse(snake)
  State(..state, snake: snake, grid: grid)
}

fn handle_new_head(state: State, new_head: Int) -> State {
  case dict.get(state.grid, new_head) {
    Ok(1) -> {
      lose(state)
      state
    }
    Ok(2) -> {
      let state = State(..state, score: state.score + 1)
      let _ = case state.score + 3 == state.rows * state.columns {
        True -> win(state)
        False -> Nil
      }
      let snake = [new_head, ..state.snake]
      let grid = dict.insert(state.grid, new_head, 1)
      let state = State(..state, grid: grid, snake: snake)
      spawn_fruit(state)
    }
    Ok(0) -> {
      remove_last_snake_block(state, new_head)
    }
    _ -> panic as "Unreachable"
  }
}

fn draw(state: State) {
  // convert dict to list and sort it. must be sorted so it prints correctly.
  let l = dict.to_list(state.grid)
  let l = list.sort(l, fn(x, y) { int.compare(x.0, y.0) })

  let score_string = "Score: " <> int.to_string(state.score)
  let score_string_len = string.length(score_string)
  let upper_border =
    "┌"
    <> score_string
    <> string.repeat("─", state.columns - score_string_len)
    <> "┐"

  // make 2 accumulators, one for list of commands, the other one is for string acc.
  // string represents a single row
  // command is Println
  // notice that we store pos as Int, not #(Int, Int). It is easier but we must
  // convert Int to #(x,y) by using % division
  let q = stdout.Queue([command.MoveTo(0, 0)])
  let strings =
    list.fold(l, #([], stree.new()), fn(q, cell) {
      case cell {
        // if we hit the end, add command to print the row with right border
        #(pos, 0) if pos % state.columns == state.columns - 1 -> {
          #([stree.append(q.1, " │") |> stree.to_string, ..q.0], stree.new())
        }
        #(pos, 1) if pos % state.columns == state.columns - 1 -> {
          #([stree.append(q.1, "@│") |> stree.to_string, ..q.0], stree.new())
        }
        #(pos, 2) if pos % state.columns == state.columns - 1 -> {
          #([stree.append(q.1, "$│") |> stree.to_string, ..q.0], stree.new())
        }
        // if we are at the start, print the cell with left border
        #(pos, 0) if pos % state.columns == 0 -> {
          #(q.0, stree.append(q.1, "│ "))
        }
        #(pos, 1) if pos % state.columns == 0 -> {
          #(q.0, stree.append(q.1, "│@"))
        }
        #(pos, 2) if pos % state.columns == 0 -> {
          #(q.0, stree.append(q.1, "│$"))
        }
        // if we are not at the start, just print the symbol
        #(_, 0) -> #(q.0, stree.append(q.1, " "))

        #(_, 1) -> #(q.0, stree.append(q.1, "@"))

        #(_, 2) -> #(q.0, stree.append(q.1, "$"))

        #(_, _) -> {
          panic as "Unreachable"
        }
      }
    })
  let lower_border = "└" <> string.repeat("─", state.columns) <> "┘"
  let strings = [lower_border, ..strings.0]
  let strings = list.reverse(strings) |> list.prepend(upper_border)
  let q1 = colorize(strings)
  let q = stdout.queue(q, q1)
  // let q = stdout.queue(q, q1)
  let q = stdout.queue(q, [command.ResetStyle])
  stdout.flush(q)
}

fn colorize(strings: List(String)) -> List(command.Command) {
  list.index_map(strings, fn(str, i) {
    command.Println(colorize_line(str, i % 2))
  })
}

fn colorize_line(str: String, offset: Int) -> String {
  string.to_graphemes(str)
  |> list.index_map(fn(ch, i) {
    let bg = case { i + offset } % 2 {
      0 -> style.AnsiValue(254)
      1 -> style.AnsiValue(188)
      _ -> panic as "Unreachable"
    }
    case ch {
      "@" -> style.with_on(ch, style.BrightGreen, bg)
      "$" -> style.with_on(ch, style.Red, bg)
      _ -> style.with_on(ch, style.Default, bg)
    }
  })
  |> string.join("")
}

fn print_centered_colored_block(
  state: State,
  s: String,
  c: style.Color,
) -> List(command.Command) {
  let len = string.length(s)
  let s = "│" <> s <> "│"
  let x = state.columns
  let y = state.rows / 2
  let x_offset = len / 2 - 1
  let x = x / 2 - x_offset
  let upper_border = "┌" <> string.repeat("─", len) <> "┐"
  let lower_border = "└" <> string.repeat("─", len) <> "┘"
  [
    command.SetForegroundColor(c),
    command.MoveTo(x, y - 1),
    command.Print(upper_border),
    command.MoveTo(x, y),
    command.Print(s),
    command.MoveTo(x, y + 1),
    command.Print(lower_border),
    command.ResetStyle,
  ]
}

fn lose(state: State) {
  stdout.execute(print_centered_colored_block(state, "You Lose", style.Red))
  process.sleep(2000)
  stdout.execute([command.Clear(terminal.All), command.LeaveAlternateScreen])
  halt(1)
}

fn win(state: State) {
  stdout.execute(print_centered_colored_block(state, "You Win", style.Green))
  process.sleep(2000)
  stdout.execute([command.Clear(terminal.All), command.LeaveAlternateScreen])
  halt(0)
}
