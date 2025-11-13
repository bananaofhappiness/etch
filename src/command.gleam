import cursor.{type CursorStyle}
import event.{type KeyboardEnhancementFlag}
import style.{type Attribute, type Color}
import terminal.{type ClearType}

pub type Command {
  // Printing
  Print(s: String)
  PrintReset(s: String)
  Println(s: String)
  PrintlnReset(s: String)

  // Cursor
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

  // Terminal
  ScrollUp(n: Int)
  ScrollDown(n: Int)
  Clear(t: ClearType)
  SetSize(x: Int, y: Int)
  SetTitle(s: String)
  DisableLineWrap
  EnableLineWrap
  EnterRaw
  EnterAlternateScreen
  LeaveAlternateScreen

  // Event
  EnableMouseCapture
  DisableMouseCapture
  PushKeyboardEnhancementFlags(List(KeyboardEnhancementFlag))
  PopKeyboardEnhancementFlags
  EnableFocusChange
  DisableFocusChange

  // Style
  SetForegroundColor(c: Color)
  SetBackgroundColor(c: Color)
  SetForegroundAndBackgroundColors(fg: Color, bg: Color)
  ResetColors
  SetAttributes(attrs: List(Attribute))
  ResetAttributes
}
