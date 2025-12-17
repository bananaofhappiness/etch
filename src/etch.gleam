// import etch/event.{Char, Key}
// import etch/terminal
// import gleam/erlang/process
// import gleam/io
// import gleam/javascript/promise
// import gleam/option.{Some}

// pub fn main() {
//   terminal.enter_raw()
//   event.init_event_server()
//   // event.enable_mouse_capture()
//   loop()
// }

// fn loop() {
//   // use event <- promise.await(event.read())
//   handle_input()
//   // echo event
//   loop()
// }

// fn handle_input() {
//   case event.read() {
//     Some(Ok(Key(c))) if c.code == Char("x") -> {
//       terminal.exit_raw()
//     }
//     r -> {
//       echo r
//       Nil
//     }
//   }
// }
