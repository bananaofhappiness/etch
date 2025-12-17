//// Simple snake implementation.

@target(javascript)
import etch/command
@target(javascript)
import etch/event.{
  type Event, type EventError, Char, DownArrow, Key, LeftArrow, RightArrow,
  UpArrow,
}
@target(javascript)
import etch/stdout
@target(javascript)
import etch/style
@target(javascript)
import etch/terminal
@target(javascript)
import gleam/dict.{type Dict}
@target(javascript)
import gleam/int
@target(javascript)
import gleam/javascript/promise.{type Promise}
@target(javascript)
import gleam/list
@target(javascript)
import gleam/option.{type Option, None, Some}
@target(javascript)
import gleam/string
@target(javascript)
import gleam/string_tree as stree

@target(javascript)
@external(javascript, "./tools.js", "exit")
fn exit(n: Int) -> Nil

@target(javascript)
@external(javascript, "./tools.js", "sleep")
fn sleep(n: Int) -> Promise(Nil)

@target(javascript)
/// Direction of snake's movement.
type Direction {
  Up
  Down
  Left
  Right
}

@target(javascript)
type GameOver {
  Win
  Lose
}

@target(javascript)
/// State of the game.
type State {
  State(
    /// Grid with position of each element .
    /// key is position, value is type (1=snake,2=fruit,0=empty space).
    /// Notice that we store pos as Int, not #(Int, Int).
    grid: Dict(Int, Int),
    /// Position/indices of snake's body.
    snake: List(Int),
    /// Number of rows.
    rows: Int,
    /// Number of columns.
    columns: Int,
    /// Direction of snake's movement.
    direction: Direction,
    /// Player's score.
    score: Int,
    game_over: Option(GameOver),
  )
}

@target(javascript)
pub fn main() {
  stdout.execute([
    // enter raw mode to get inputs immediately
    command.EnterRaw,
    // enter alternate screeen to not affect main buffer.
    command.EnterAlternateScreen,
    command.Clear(terminal.All),
    command.HideCursor,
    command.DisableLineWrap,
  ])

  let #(columns, rows) = terminal.window_size()
  // game's grid is a bit smaller than the terminal window because we have borders too.
  // upper and lower borders take 2 tiles from upper and lower parts of the terminal,
  // so do right and left. so we substract 2.
  let #(columns, rows) = #(columns - 2, rows - 2)
  let grid = make_grid(columns, rows)
  // spawn snake at the center of the grid.
  let snake_center = case int.is_even(rows) {
    // if true we add + columns / 2 so the snake does not spawn at the edge of the grid.
    True -> rows * columns / 2 + columns / 2
    False -> rows * columns / 2
  }
  let state =
    State(
      grid,
      // snake starts with a body of 3 cells.
      [snake_center, snake_center - 1, snake_center - 2],
      rows,
      columns,
      Right,
      0,
      None,
    )
  let state = spawn_fruit(state)
  event.init_event_server()
  loop(state)
}

@target(javascript)
fn make_grid(columns: Int, rows: Int) -> Dict(Int, Int) {
  let x =
    list.range(0, columns * rows)
    |> list.zip(list.repeat(0, columns * rows))
  let d = dict.from_list(x)
  // add snake to the grid (value=1)
  case int.is_even(rows) {
    True -> dict.insert(d, { rows * columns / 2 } + columns / 2, 1)
    False -> dict.insert(d, rows * columns / 2, 1)
  }
}

@target(javascript)
fn loop(state: State) {
  // don't forget to add sleep in your loop.
  // not only it makes the game playable (snake doesnt move so fast)
  // but also reduces CPU usage by a lot.
  // (constant loops with no latency between them are super expensive).
  use _ <- promise.await(sleep(200))
  // we handle input first and then update state accordingly.
  use event <- promise.await(event.poll(1))
  let state = handle_input(event, state)
  let state = update_state(state)
  case state.game_over {
    Some(Win) -> win(state)
    Some(Lose) -> lose(state)
    None -> {
      draw(state)
      loop(state)
    }
  }
}

@target(javascript)
fn handle_input(event: Option(Result(Event, EventError)), state: State) -> State {
  // `poll(n)` waits n ms for an event. if there were no events, it returns None.
  case event, state.direction {
    // if the snake is moving downwards and we press w or up arrow, do nothing.
    Some(Ok(Key(k))), Down if k.code == Char("w") || k.code == UpArrow -> state
    // otherwise change state's direction to Up
    Some(Ok(Key(k))), _ if k.code == Char("w") || k.code == UpArrow ->
      State(..state, direction: Up)

    // if the snake is moving right and we press a or left arrow, do nothing.
    Some(Ok(Key(k))), Right if k.code == Char("a") || k.code == LeftArrow ->
      state
    // otherwise change state's direction to Left
    Some(Ok(Key(k))), _ if k.code == Char("a") || k.code == LeftArrow ->
      State(..state, direction: Left)

    // if the snake is moving upwards and we press s or down arrow, do nothing.
    Some(Ok(Key(k))), Up if k.code == Char("s") || k.code == DownArrow -> state
    // otherwise change state's direction to Down
    Some(Ok(Key(k))), _ if k.code == Char("s") || k.code == DownArrow ->
      State(..state, direction: Down)

    // if the snake is moving left and we press d or right arrow, do nothing.
    Some(Ok(Key(k))), Left if k.code == Char("d") || k.code == RightArrow ->
      state
    // otherwise change state's direction to Right
    Some(Ok(Key(k))), _ if k.code == Char("d") || k.code == RightArrow ->
      State(..state, direction: Right)

    Some(_), _ -> state
    None, _ -> state
  }
}

@target(javascript)
fn update_state(state: State) -> State {
  case state.direction {
    Up -> move_up(state)
    Down -> move_down(state)
    Left -> move_left(state)
    Right -> move_right(state)
  }
}

@target(javascript)
fn move_right(state: State) -> State {
  let assert Ok(head) = list.first(state.snake)
  // if snake hits the right border, the game is over.
  // note that the terminal window is a larger than the playing area.
  let #(state, new_head) = case head {
    n if n % state.columns == state.columns - 1 -> {
      #(State(..state, game_over: Some(Lose)), 0)
    }
    n -> #(state, n + 1)
  }
  handle_new_head(state, new_head)
}

@target(javascript)
fn move_down(state: State) -> State {
  let assert Ok(head) = list.first(state.snake)
  // if snake hits the lower border, the game is over.
  // note that the terminal window is a larger than the playing area.
  let #(state, new_head) = case head + state.columns {
    n if n > state.columns * state.rows -> {
      #(State(..state, game_over: Some(Lose)), 0)
    }
    n -> #(state, n)
  }
  handle_new_head(state, new_head)
}

@target(javascript)
fn move_left(state: State) -> State {
  // if snake hits the left border, the game is over.
  // note that the terminal window is a larger than the playing area.
  let assert Ok(head) = list.first(state.snake)
  let #(state, new_head) = case head {
    n if n % state.columns == 0 -> {
      #(State(..state, game_over: Some(Lose)), 0)
    }
    n -> #(state, n - 1)
  }
  handle_new_head(state, new_head)
}

@target(javascript)
fn move_up(state: State) -> State {
  let assert Ok(head) = list.first(state.snake)
  // if snake hits the upper border, the game is over.
  // note that the terminal window is a larger than the playing area.
  let #(state, new_head) = case head - state.columns {
    n if n < 0 -> {
      #(State(..state, game_over: Some(Lose)), 0)
    }
    n -> #(state, n)
  }
  handle_new_head(state, new_head)
}

@target(javascript)
fn handle_new_head(state: State, new_head: Int) -> State {
  case dict.get(state.grid, new_head) {
    // if new head land on the snake's body, the game is over.
    Ok(1) -> {
      State(..state, game_over: Some(Lose))
    }
    // if new head land on a fruit
    Ok(2) -> {
      // add 1 to the score.
      let state = State(..state, score: state.score + 1)
      // if the snake covers the whole grid, player wins.
      // (we add 3 because we start with 3 body cells).
      let state = case state.score + 3 == state.rows * state.columns {
        True -> {
          State(..state, game_over: Some(Win))
        }
        False -> state
      }
      // add new head to the snake making snake 1 cell larger.
      let snake = [new_head, ..state.snake]
      // add new head to the grid
      let grid = dict.insert(state.grid, new_head, 1)
      // update state's grid and snake
      let state = State(..state, grid: grid, snake: snake)
      case state.game_over {
        None -> spawn_fruit(state)
        _ -> state
      }
    }
    // otherwise just move the snake.
    Ok(0) -> {
      remove_last_snake_block(state, new_head)
    }
    _ -> panic as "Unreachable"
  }
}

@target(javascript)
fn spawn_fruit(state: State) -> State {
  // generate random value on the grid.
  let f = int.random(state.rows * state.columns)
  case dict.get(state.grid, f) {
    // if the cell is occupied by snake's body, try spawning fruit again.
    Ok(1) -> spawn_fruit(state)
    // otherwise spawn it.
    Ok(0) -> {
      let grid = dict.insert(state.grid, f, 2)
      State(..state, grid: grid)
    }
    _ -> panic as "Unreachable"
  }
}

@target(javascript)
fn remove_last_snake_block(state: State, new_head: Int) -> State {
  // i don't know the better way to remove the last element of a list
  // but to reverse it and then use pattern matching to split it to last element an the rest
  // and take only the rest.
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

@target(javascript)
fn draw(state: State) {
  // convert dict to list and sort it. must be sorted so it prints correctly.
  let l = dict.to_list(state.grid)
  let l = list.sort(l, fn(x, y) { int.compare(x.0, y.0) })

  // make 2 accumulators, one for list of strings (lines), the other one is for string acc.
  // string represents a single row
  // notice that we store pos as Int, not #(Int, Int).
  // we must convert Int to #(x,y) by using % division.
  let q = stdout.Queue([command.MoveTo(0, 0)])
  let strings =
    list.fold(l, #([], stree.new()), fn(s, cell) {
      case cell {
        // if we hit the end, add command to print the row with right border
        #(pos, 0) if pos % state.columns == state.columns - 1 -> {
          // add the accumulated string + " │" to the queue and make a new accumulator.
          #([stree.append(s.1, " │") |> stree.to_string, ..s.0], stree.new())
        }
        // the same logic but when snake is near the border.
        #(pos, 1) if pos % state.columns == state.columns - 1 -> {
          #([stree.append(s.1, "@│") |> stree.to_string, ..s.0], stree.new())
        }
        // same with fruits
        #(pos, 2) if pos % state.columns == state.columns - 1 -> {
          #([stree.append(s.1, "$│") |> stree.to_string, ..s.0], stree.new())
        }
        // if we are at the start, print the cell with left border
        #(pos, 0) if pos % state.columns == 0 -> {
          // add "│ " to the accumulated string
          #(s.0, stree.append(s.1, "│ "))
        }
        // if snake is near the border
        #(pos, 1) if pos % state.columns == 0 -> {
          #(s.0, stree.append(s.1, "│@"))
        }
        // same with a fruit
        #(pos, 2) if pos % state.columns == 0 -> {
          #(s.0, stree.append(s.1, "│$"))
        }
        // if we are not at the start, just print the symbol
        #(_, 0) -> #(s.0, stree.append(s.1, " "))

        #(_, 1) -> #(s.0, stree.append(s.1, "@"))

        #(_, 2) -> #(s.0, stree.append(s.1, "$"))

        #(_, _) -> {
          panic as "Unreachable"
        }
      }
    })

  // add upper and lower borders.
  let score_string = "Score: " <> int.to_string(state.score)
  let score_string_len = string.length(score_string)
  let upper_border =
    "┌"
    <> score_string
    <> string.repeat("─", state.columns - score_string_len)
    <> "┐"
  let lower_border = "└" <> string.repeat("─", state.columns) <> "┘"
  let strings = [lower_border, ..strings.0]
  let strings = list.reverse(strings) |> list.prepend(upper_border)

  let q1 = colorize(strings)
  let q = stdout.queue(q, q1)
  let q = stdout.queue(q, [command.ResetStyle])
  stdout.flush(q)
}

@target(javascript)
fn colorize(strings: List(String)) -> List(command.Command) {
  list.index_map(strings, fn(str, i) {
    command.Println(colorize_line(str, i % 2))
  })
}

@target(javascript)
fn colorize_line(str: String, offset: Int) -> String {
  string.to_graphemes(str)
  |> list.index_map(fn(ch, i) {
    // make a chessboard pattern.
    let bg = case { i + offset } % 2 {
      0 -> style.AnsiValue(254)
      1 -> style.AnsiValue(188)
      _ -> panic as "Unreachable"
    }
    // make the snake green and fruits red
    case ch {
      "@" -> style.with_on(ch, style.BrightGreen, bg)
      "$" -> style.with_on(ch, style.Red, bg)
      _ -> style.with_on(ch, style.Default, bg)
    }
  })
  |> string.join("")
}

@target(javascript)
fn lose(state: State) {
  stdout.execute(print_centered_colored_block(state, "You Lose", style.Red))
  use _ <- promise.tap(sleep(2000))
  stdout.execute([command.Clear(terminal.All), command.LeaveAlternateScreen])
  exit(1)
}

@target(javascript)
fn win(state: State) {
  stdout.execute(print_centered_colored_block(state, "You Win", style.Green))
  use _ <- promise.tap(sleep(2000))
  stdout.execute([command.Clear(terminal.All), command.LeaveAlternateScreen])
  exit(0)
}

@target(javascript)
fn print_centered_colored_block(
  state: State,
  s: String,
  c: style.Color,
) -> List(command.Command) {
  // the same logic as in the hello_world example
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
