import gleam/erlang/process.{type Pid, type Subject}
import gleam/io.{println}

// Добавляем импорты
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom

// ... в секции @external

// Новая функция для изменения настроек ввода
@external(erlang, "io", "setopts")
fn set_io_opts(opts: List(#(String, String))) -> Nil

// @external(erlang, "io", "setopts")
// fn set_io_opts(opts: String) -> Nil

@external(erlang, "io", "getopts")
fn getopts() -> Nil

@external(erlang, "raw", "enter_raw")
pub fn enter_raw() -> Nil

@external(erlang, "raw", "reader_loop")
pub fn reader_loop(pid: Pid) -> Nil

@external(erlang, "raw", "start_reader")
pub fn start_reader() -> Nil

@external(erlang, "io", "get_line")
pub fn get_line(prompt: String) -> String

@external(erlang, "erlang", "halt")
pub fn exit(code: Int) -> Int

@external(erlang, "raw", "read_char")
fn read_char() -> String

@external(erlang, "io", "get_chars")
fn get_chars(str: String, x: Int) -> String

@external(erlang, "io", "put_chars")
fn put_chars(chars: String) -> Nil

@external(erlang, "io", "fwrite")
fn fwrite(str: String) -> Nil

@external(erlang, "io", "getn")
fn getn(n: Int) -> String

pub fn setup() {
  enter_raw()
  // put_chars("\u{001b}[?1049h")
  // --- ВОТ КЛЮЧЕВОЕ ИЗМЕНЕНИЕ ---
  // Готовим опции для Erlang: [{binary, true}]
  let opts = [#("encoding", "unicode"), #("binary", "true")]
  let x = getopts()
  echo x
  // let opts = "binary"
  // Устанавливаем режим для стандартного ввода ('user')
  set_io_opts(opts)
  // --------------------------------
}

pub fn main() {
  let pid = process.self()
  let subj = process.new_subject()
  // start_reader()
  // loop(subj)
  let x = 0
  setup()
  process.spawn(fn() { input_loop(subj) })
  loop(subj)
  // process.spawn(fn() { loop(subj) })
  // input_loop(subj)
  put_chars("\u{001b}[?1049l")
}

fn loop(subj: Subject(String)) {
  let msg = process.receive_forever(subj)
  fwrite(msg)
  fwrite("recieved?\n")
  loop(subj)
}

fn input_loop(subj: Subject(String)) {
  process.send(subj, "this is a fucking 1st msg")
  let input = get_chars("", 1)
  // let input = read_char()
  // let input = get_line("")
  process.send(subj, input)
  input_loop(subj)
}
