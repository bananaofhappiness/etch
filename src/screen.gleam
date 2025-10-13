import consts.{esc}
import gleam/io

pub fn clear() {
  io.print(esc <> "[2J")
}

pub fn enter_alternative() {
  io.print(esc <> "[?1049h")
}

pub fn exit_alternative() {
  io.print(esc <> "[?1049l")
}

pub fn println(s: String) {
  io.print(esc <> "[1E" <> s)
}
