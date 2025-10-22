import cursor.{type CursorStyle}
import terminal.{type ClearType}

pub type Command {
  // Printing
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
  SavePosition
  RestorePosition
  ShowCursor
  HideCursor
  SetCursorStyle(s: CursorStyle)

  //Screen
  Clear(t: ClearType)
  SetSize(x: Int, y: Int)
  EnterRaw
  EnterAlternateScreen
  LeaveAlternateScreen
}
