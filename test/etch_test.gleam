import etch/terminal
import gleeunit

pub fn main() -> Nil {
  gleeunit.main()
}

@target(javascript)
pub fn enter_and_exit_raw_test() -> Nil {
  let enter_result = terminal.enter_raw()
  case enter_result {
    Ok(ok) -> {
      assert ok == Nil
      assert terminal.is_raw_mode()
    }
    Error(error) -> {
      assert error == terminal.FailedToEnterRawMode
    }
  }
  let exit_result = terminal.exit_raw()
  case exit_result {
    Ok(ok) -> {
      assert ok == Nil
      assert !terminal.is_raw_mode()
    }
    Error(error) -> {
      assert error == terminal.FailedToExitRawMode
    }
  }
}

@target(erlang)
pub fn enter_and_exit_raw_test() -> Nil {
  let _ = terminal.enter_raw()
  assert terminal.is_raw_mode()
  let _ = terminal.exit_raw()
  assert !terminal.is_raw_mode()
}

pub fn get_window_size_test() -> Nil {
  let res = terminal.window_size()
  case res {
    Ok(#(x, y)) -> {
      assert x > 0
      assert y > 0
    }
    Error(error) -> {
      assert error == terminal.CouldNotGetWindowSize
    }
  }
}
