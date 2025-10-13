pub type Command

pub type Queue {
  Queue(commands: List(Command))
}

pub fn new() -> Queue {
  Queue([])
}

pub fn queue(queue: Queue, command: Command) -> Queue {
  Queue(commands: [command, ..queue.commands])
}
// pub fn flush(queue: Queue) -> Nil {
//   let commands = list.reverse(queue.commands)
// }
