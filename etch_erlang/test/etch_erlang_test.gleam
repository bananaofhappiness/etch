import etch/erlang/tty.{
  CouldNotGetWindowSize, enter_raw, exit_raw, is_raw_mode, window_size,
}
import gleeunit

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn enter_and_exit_raw_test() -> Nil {
  let _ = enter_raw()
  assert is_raw_mode()
  let _ = exit_raw()
  assert !is_raw_mode()
}

pub fn get_window_size_test() -> Nil {
  let res = window_size()
  case res {
    Ok(#(x, y)) -> {
      assert x > 0
      assert y > 0
    }
    Error(error) -> {
      assert error == CouldNotGetWindowSize
    }
  }
}
