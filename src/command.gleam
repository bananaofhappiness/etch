pub type Command {
  Print(s: String)
  PrintReset(s: String)
  Println(s: String)
  PrintlnReset(s: String)

  //Cursor
  MoveUp(n: Int)
  MoveDown(n: Int)
  MoveLeft(n: Int)
  MoveRight(n: Int)
  MoveToNextLine(n: Int)
  MoveToPreviousLine(n: Int)
  MoveToColumn(n: Int)
  MoveToRow(n: Int)
  MoveTo(x: Int, y: Int)
  // SavePosition
  // RestorePosition
  ShowCursor
  HideCursor
  EnableBlinking
  DisableBlinking
  SetCursorStyle

  //Screen
  EnterRaw
  EnterAlternateScreen
  LeaveAlternateScreen
}
