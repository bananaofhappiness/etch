import consts.{esc}
import gleam/io.{print}

pub fn clear() {
  print(esc <> "[2J")
}

pub fn enter_alternative() {
  print(esc <> "[?1049h")
}

pub fn exit_alternative() {
  print(esc <> "[?1049l")
}
