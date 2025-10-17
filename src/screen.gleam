import esc.{esc}
import gleam/io

@external(erlang, "raw", "enter_raw")
pub fn enter_raw() -> Nil

pub fn clear() {
  io.print(esc("[2J"))
}

pub fn enter_alternative() -> String {
  esc("[?1049h")
}

pub fn leave_alternative() -> String {
  esc("[?1049l")
}
