import gleam/io

@external(erlang, "raw", "enter_raw")
pub fn enter_raw() -> Nil

// @external(erlang, "raw", "loop")
// pub fn loop(pos: Int) -> Nil

@external(erlang, "io", "put_chars")
fn put_chars(chars: String) -> Nil

@external(erlang, "io", "get_chars")
fn get_chars(chars: String, n: Int) -> String

@external(erlang, "io", "get_line")
fn get_line(chars: String) -> String

pub fn setup() {
  enter_raw()
  put_chars("\u{001b}[?1049h")
  put_chars("\u{001b}[?25l")
}

pub fn main() {
  setup()
  loop()
  put_chars("\u{001b}[?1049l")
}

pub fn loop() {
  // put_chars("fuck2")
  let chars = get_chars("", 1)
  // put_chars("fuck3")
  case chars {
    "q" -> Nil
    _ -> {
      // put_chars(chars)
      io.print(chars)
      loop()
    }
  }
}
