// add etch locally so you can use internal public functions
import argv
import etch/command
import etch/event.{
  type Event, type EventError, Char, Key, KeyEvent, KeyEventState, Left,
  Modifiers, Mouse, MouseEvent, Press, Up, parse_events,
}
import etch/stdout
import etch/style.{attributes, with, with_on}
import etch/terminal
import gleam/dict.{type Dict}

@target(erlang)
import gleam/erlang/process
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string

@external(erlang, "erlang", "timestamp")
@external(javascript, "./time.mjs", "now")
fn now() -> t

@external(erlang, "timer", "now_diff")
@external(javascript, "./time.mjs", "now_diff")
fn now_diff(x: t, y: t) -> Float

@target(javascript)
@external(javascript, "./process.mjs", "sleep")
fn sleep(ms: Int) -> Nil

@target(erlang)
fn sleep(ms: Int) -> Nil {
  process.sleep(ms)
}

@target(erlang)
@external(erlang, "file", "write_file")
fn write_file(path: String, contents: String) -> Result(Nil, err)

@target(javascript)
@external(javascript, "./process.mjs", "write_file")
fn write_file(path: String, contents: String) -> Result(Nil, err)

fn key_event(char: String) -> Event {
  Key(KeyEvent(
    code: Char(char),
    modifiers: Modifiers(
      shift: False,
      alt: False,
      control: False,
      super: False,
      hyper: False,
      meta: False,
    ),
    kind: Press,
    state: KeyEventState(keypad: False, capslock: False, numlock: False),
    text: "",
  ))
}

fn expected_events_base() -> List(Result(Event, EventError)) {
  [
    Ok(
      Mouse(MouseEvent(
        kind: Up(Left),
        column: 0,
        row: 0,
        modifiers: Modifiers(
          shift: False,
          alt: False,
          control: False,
          super: False,
          hyper: False,
          meta: False,
        ),
      )),
    ),
    Ok(key_event("h")),
    Ok(key_event("e")),
    Ok(key_event("l")),
    Ok(key_event("l")),
    Ok(key_event("o")),
    Ok(key_event("w")),
    Ok(key_event("o")),
    Ok(key_event("r")),
    Ok(key_event("l")),
    Ok(key_event("d")),
  ]
}

const events_number = 250

const redraw_whole_screen_repetitions = 100_000

const redraw_1_line_repetitions = 100_000

const redraw_1_symbol_repetitions = 100_000

const style_apply_text_repetitions = 100_000

const style_combine_repetitions = 100_000

const style_large_block_repetitions = 100_000

const cursor_hide_show_repetitions = 100_000

const cursor_move_random_repetitions = 100_000

const cursor_save_restore_repetitions = 100_000

const terminal_clear_repetitions = 100_000

const terminal_line_wrap_repetitions = 100_000

const terminal_alternative_screen_repetitions = 100_000

const terminal_scroll_repetitions = 100_000

const terminal_set_size_repetitions = 100_000

const terminal_set_title_repetitions = 100_000

const terminal_window_size_repetitions = 100_000

const terminal_is_raw_mode_repetitions = 100_000

const execute_batch_repetitions = 100_000

const execute_multiple_repetitions = 100_000

type Res {
  Res(samples: List(Float), total_time: Float)
}

type Benchmark(a) {
  Benchmark(name: String, iterations: Int, thunk: fn() -> a)
}

pub fn main() {
  let #(target, runtime) = case argv.load().arguments {
    [target, name] -> #(target, name)
    _ -> panic as "Missing target and runtime arguments"
  }
  event.init_event_server()
  run_benches(fn() { sleep(1000) }, target, runtime)
}

fn run_benches(pause: fn() -> Nil, target: String, runtime: String) {
  stdout.execute([command.EnterAlternateScreen])

  let #(x, y) = terminal.window_size()
  let results =
    [
      Benchmark("redraw_whole_screen", redraw_whole_screen_repetitions, fn() {
        redraw_whole_screen(x, y)
      }),
      Benchmark("redraw_1_line", redraw_1_line_repetitions, fn() {
        redraw_1_line(x, y)
      }),
      Benchmark("redraw_1_symbol", redraw_1_symbol_repetitions, fn() {
        redraw_1_symbol(x, y)
      }),
      Benchmark("handle_large_event", events_number, fn() {
        handle_large_event(events_number)
      }),
      Benchmark("handle_events", events_number, fn() { handle_many_events() }),
      Benchmark(
        "redraw_and_handle_events",
        redraw_whole_screen_repetitions,
        fn() { redraw_and_handle_events(x, y) },
      ),
      Benchmark("style_apply_text", style_apply_text_repetitions, fn() {
        style_apply_text()
      }),
      Benchmark("style_combine", style_combine_repetitions, fn() {
        style_combine()
      }),
      Benchmark("style_large_block", style_large_block_repetitions, fn() {
        style_large_block()
      }),
      Benchmark("cursor_hide_show", cursor_hide_show_repetitions, fn() {
        cursor_hide_show()
      }),
      Benchmark("cursor_move_random", cursor_move_random_repetitions, fn() {
        cursor_move_random()
      }),
      Benchmark("cursor_save_restore", cursor_save_restore_repetitions, fn() {
        cursor_save_restore()
      }),
      Benchmark("terminal_clear", terminal_clear_repetitions, fn() {
        terminal_clear()
      }),
      Benchmark(
        "terminal_disable_line_wrap",
        terminal_line_wrap_repetitions,
        fn() { terminal_disable_line_wrap() },
      ),
      Benchmark(
        "terminal_enable_line_wrap",
        terminal_line_wrap_repetitions,
        fn() { terminal_enable_line_wrap() },
      ),
      Benchmark(
        "terminal_enter_alternative",
        terminal_alternative_screen_repetitions,
        fn() { terminal_enter_alternative() },
      ),
      Benchmark(
        "terminal_leave_alternative",
        terminal_alternative_screen_repetitions,
        fn() { terminal_leave_alternative() },
      ),
      Benchmark("terminal_scroll_down", terminal_scroll_repetitions, fn() {
        terminal_scroll_down()
      }),
      Benchmark("terminal_scroll_up", terminal_scroll_repetitions, fn() {
        terminal_scroll_up()
      }),
      Benchmark("terminal_set_size", terminal_set_size_repetitions, fn() {
        terminal_set_size()
      }),
      Benchmark("terminal_set_title", terminal_set_title_repetitions, fn() {
        terminal_set_title()
      }),
      Benchmark("terminal_window_size", terminal_window_size_repetitions, fn() {
        terminal_window_size()
      }),
      Benchmark("terminal_is_raw_mode", terminal_is_raw_mode_repetitions, fn() {
        terminal_is_raw_mode()
      }),
      Benchmark("execute_batch", execute_batch_repetitions, fn() {
        execute_batch()
      }),
      Benchmark("execute_multiple", execute_multiple_repetitions, fn() {
        execute_multiple()
      }),
    ]
    |> run_all_benches(pause)

  stdout.execute([command.LeaveAlternateScreen])
  print_results(results)
  write_csv(results, target, runtime, x, y)
}

fn run_all_benches(
  cases: List(Benchmark(a)),
  pause: fn() -> Nil,
) -> Dict(String, Res) {
  list.fold(cases, dict.new(), fn(results, bench_case) {
    io.println("running benchmark " <> bench_case.name)
    pause()
    run_bench_case(results, bench_case)
  })
}

fn run_bench_case(
  results: Dict(String, Res),
  bench_case: Benchmark(a),
) -> Dict(String, Res) {
  let Benchmark(name, iterations, thunk) = bench_case
  let t1 = now()
  let samples = run_thunk_iterations(iterations, thunk, [])
  let t2 = now()
  dict.insert(results, name, Res(samples, now_diff(t2, t1)))
}

fn run_thunk_iterations(
  n: Int,
  thunk: fn() -> a,
  samples: List(Float),
) -> List(Float) {
  case n {
    0 -> samples
    _ -> {
      case n % 1000 == 0 {
        True -> {
          let t1 = now()
          thunk()
          let t2 = now()
          let samples = [now_diff(t2, t1), ..samples]
          run_thunk_iterations(n - 1, thunk, samples)
        }
        False -> {
          thunk()
          run_thunk_iterations(n - 1, thunk, samples)
        }
      }
    }
  }
}

fn redraw_whole_screen(x: Int, y: Int) {
  let str = { "█" |> string.repeat(x) <> "\n" } |> string.repeat(y)

  stdout.execute([
    command.Clear(terminal.All),
    command.MoveTo(0, 0),
    command.Print(str),
  ])
}

fn redraw_1_symbol(x, y) {
  let n = int.random(x * y)
  stdout.execute([
    command.MoveTo(n % x, n / x),
    command.Print("█"),
  ])
}

fn redraw_1_line(x, y) {
  let y = int.random(y)
  stdout.execute([
    command.MoveTo(0, y),
    command.Println("█" |> string.repeat(x)),
  ])
}

fn handle_large_event(events_number: Int) {
  let expected =
    expected_events_base()
    |> list.repeat(events_number)
    |> list.flatten()
  let input = "\u{001b}[<0;0;0mhelloworld" |> string.repeat(events_number)
  handle_events(input, expected)
}

fn handle_many_events() {
  let input = "\u{001b}[<0;0;0mhelloworld"
  handle_events(input, expected_events_base())
}

fn redraw_and_handle_events(x: Int, y: Int) {
  let expected = expected_events_base() |> list.repeat(25) |> list.flatten()
  let input = "\u{001b}[<0;0;0mhelloworld" |> string.repeat(25)

  case int.random(5) == 0 {
    True -> {
      redraw_whole_screen(x, y)
      handle_events(input, expected)
    }
    False -> {
      redraw_whole_screen(x, y)
    }
  }
}

fn style_apply_text() {
  let text = "benchmark text"
  let _ = with(text, style.Red)
  Nil
}

fn style_combine() {
  let text = "benchmark text"
  let attrs = [style.Bold, style.Italic, style.Underline]
  let _ = text |> with_on(style.Red, style.Black) |> attributes(attrs)
  Nil
}

fn style_large_block() {
  let text = "benchmark line" |> string.repeat(100)
  let _ = with_on(text, style.BrightYellow, style.Blue)
  Nil
}

fn cursor_hide_show() {
  stdout.execute([command.HideCursor, command.ShowCursor])
}

fn cursor_move_random() {
  let x = int.random(200)
  let y = int.random(50)
  stdout.execute([command.MoveTo(x, y)])
}

fn cursor_save_restore() {
  stdout.execute([command.SavePosition, command.RestorePosition])
}

fn terminal_clear() {
  let _ = terminal.clear(terminal.All)
  Nil
}

fn terminal_disable_line_wrap() {
  let _ = terminal.disable_line_wrap()
  Nil
}

fn terminal_enable_line_wrap() {
  let _ = terminal.enable_line_wrap()
  Nil
}

fn terminal_enter_alternative() {
  let _ = terminal.enter_alternative()
  Nil
}

fn terminal_leave_alternative() {
  let _ = terminal.leave_alternative()
  Nil
}

fn terminal_scroll_down() {
  let _ = terminal.scroll_down(1)
  Nil
}

fn terminal_scroll_up() {
  let _ = terminal.scroll_up(1)
  Nil
}

fn terminal_set_size() {
  let _ = terminal.set_size(80, 24)
  Nil
}

fn terminal_set_title() {
  let _ = terminal.set_title("benchmark")
  Nil
}

fn terminal_window_size() {
  let _ = terminal.window_size()
  Nil
}

fn terminal_is_raw_mode() {
  let _ = terminal.is_raw_mode()
  Nil
}

fn execute_batch() {
  stdout.execute([
    command.MoveTo(0, 0),
    command.MoveTo(1, 1),
    command.MoveTo(2, 2),
    command.MoveTo(3, 3),
    command.MoveTo(4, 4),
  ])
}

fn execute_multiple() {
  stdout.execute([command.MoveTo(0, 0)])
  stdout.execute([command.MoveTo(1, 1)])
  stdout.execute([command.MoveTo(2, 2)])
  stdout.execute([command.MoveTo(3, 3)])
  stdout.execute([command.MoveTo(4, 4)])
}

fn print_results(results: Dict(String, Res)) {
  io.println(
    string.pad_end("name", 30, " ")
    <> string.pad_end("p50", 30, " ")
    <> string.pad_end("p95", 30, " ")
    <> string.pad_end("p99", 30, " ")
    <> string.pad_end("total_time", 30, " "),
  )
  dict.each(results, fn(name, results) {
    let total_time = get_total_time_string(results.total_time)
    let p50 = get_p(results.samples, list.length(results.samples), 50.0)
    let p95 = get_p(results.samples, list.length(results.samples), 95.0)
    let p99 = get_p(results.samples, list.length(results.samples), 99.0)
    io.println(
      string.pad_end(name, 30, " ")
      <> string.pad_end(float.to_string(p50), 30, " ")
      <> string.pad_end(float.to_string(p95), 30, " ")
      <> string.pad_end(float.to_string(p99), 30, " ")
      <> string.pad_end(total_time, 30, " "),
    )
  })
}

fn handle_events(input: String, expected: List(Result(Event, EventError))) {
  let str = string.to_graphemes(input)
  let events = parse_events(str, "", [], False)
  case events == expected {
    True -> Nil
    False -> panic
  }
}

@target(erlang)
fn get_total_time_string(t) -> String {
  float.round(t) |> int.to_string()
}

@target(javascript)
fn get_total_time_string(t) -> String {
  float.to_string(t)
}

fn get_p(samples, len: Int, p: Float) {
  let samples = list.sort(samples, float.compare)
  let len = int.to_float(len)
  let r = { p /. 100.0 } *. { len -. 1.0 }
  // if integer
  let eps = 0.000000001 *. float.max(1.0, float.absolute_value(r))
  case float.absolute_value(r -. int.to_float(float.round(r))) <=. eps {
    True -> r
    False -> {
      let k = float.floor(r)
      let d = r -. k
      let k = float.round(k)
      let kth_elemnt =
        list.drop(samples, k) |> list.first() |> result.unwrap(0.0)
      let k_plus_one_elemnt =
        list.drop(samples, k + 1) |> list.first() |> result.unwrap(0.0)
      kth_elemnt +. d *. { k_plus_one_elemnt -. kth_elemnt }
    }
  }
}

fn write_csv(
  results: Dict(String, Res),
  target: String,
  runtime: String,
  terminal_width: Int,
  terminal_height: Int,
) {
  let header =
    "target,runtime,terminal_width,terminal_height,name,p50,p95,p99,total_time\n"
  let terminal_width = int.to_string(terminal_width)
  let terminal_height = int.to_string(terminal_height)
  let rows =
    dict.to_list(results)
    |> list.map(fn(pair) {
      let #(name, res) = pair
      let total_time = get_total_time_string(res.total_time)
      let p50 = get_p(res.samples, list.length(res.samples), 50.0)
      let p95 = get_p(res.samples, list.length(res.samples), 95.0)
      let p99 = get_p(res.samples, list.length(res.samples), 99.0)
      target
      <> ","
      <> runtime
      <> ","
      <> terminal_width
      <> ","
      <> terminal_height
      <> ","
      <> name
      <> ","
      <> float.to_string(p50)
      <> ","
      <> float.to_string(p95)
      <> ","
      <> float.to_string(p99)
      <> ","
      <> total_time
      <> "\n"
    })
    |> string.concat()

  let path =
    "results_"
    <> target
    <> "_"
    <> runtime
    <> "_"
    <> terminal_width
    <> "_"
    <> terminal_height
    <> ".csv"
  let contents = header <> rows
  let _ = write_file(path, contents)
  io.println("Results written to " <> path)
}
