import etch/erlang/tty.{
  CouldNotGetWindowSize, FailedToEnterRawMode, FailedToExitRawMode, enter_raw,
  exit_raw, is_raw_mode, window_size,
}
import gleeunit

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn enter_and_exit_raw_test() -> Nil {
  let enter_result = enter_raw()
  case enter_result {
    Ok(ok) -> {
      assert ok == Nil
      assert is_raw_mode()
    }
    Error(error) -> {
      assert error == FailedToEnterRawMode
    }
  }
  let exit_result = exit_raw()
  case exit_result {
    Ok(ok) -> {
      assert ok == Nil
      assert !is_raw_mode()
    }
    Error(error) -> {
      assert error == FailedToExitRawMode
    }
  }
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
