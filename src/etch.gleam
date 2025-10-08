import gleam/io.{print}

@external(erlang, "raw", "enter_raw")
pub fn enter_raw() -> Nil

@external(erlang, "io", "get_chars")
fn get_chars(chars: String, n: Int) -> String

pub fn setup() {
  enter_raw()
  print("\u{001b}[?1049h")
  print("\u{001b}[?25l")
}

pub fn main() {
  setup()
  loop()
  print("\u{001b}[?1049l")
}

pub fn loop() {
  let chars = get_chars("", 1024)
  case chars {
    "q" -> Nil
    _ -> {
      // put_chars(chars)
      print(chars)
      loop()
    }
  }
}
