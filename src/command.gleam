pub type Command {
  Print(s: String)
  PrintReset(s: String)
  Println(s: String)
  PrintlnReset(s: String)
  MoveCursor(x: Int, y: Int)
  MoveToNextLine(n: Int)
  EnterRaw
  EnterAlternateScreen
  LeaveAlternateScreen
  ShowCursor
  HideCursor
}
