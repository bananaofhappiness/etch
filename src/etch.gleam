import gleam/io.{println}

@external(erlang, "raw", "enter_raw")
pub fn enter_raw() -> Nil

@external(erlang, "io", "get_line")
pub fn get_line(prompt: String) -> String

@external(erlang, "erlang", "halt")
pub fn exit(code: Int) -> Int

pub fn main() {
  enter_raw()
  main()
  let x = get_line("")
  case x {
    "^C" -> exit(0)
    _ -> exit(1)
  }
}
