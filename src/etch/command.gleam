//// This module provides commads to be queued and flushed.
//// ```gleam
//// import stdout.{Queue, queue, flush}
////
//// let q = Queue([])
//// queue(q, [
////   EnterAlternativeScreen,
////   HideCursor,
////   Println("Hello from Etch"),
//// ])
//// flush(q)
//// ```

import etch/cursor.{type CursorStyle}
import etch/event.{type KeyboardEnhancementFlag}
import etch/style.{type Attribute, type Color, type Style}
import etch/terminal.{type ClearType}

/// Commands can be queued and flushed afterwards.
/// See [`Queue`](stdout.html#Queue), [`flush`](stdout.html#flush)
/// and [`execute`](stdout.html#execute) functions.
pub type Command {
  // Printing
  /// Prints text.
  Print(s: String)
  /// Prints text and resets its [`Colors`](style.html#Color)
  /// and [`Attributes`](style.html#Attribute).
  PrintReset(s: String)
  /// Prints text and moves the caret to the beginning of a new line.
  Println(s: String)
  /// Prints text, resets its [`Colors`](style.html#Color)
  /// and [`Attributes`](style.html#Attribute)
  /// and moves the caret to the beginning of a new line.
  PrintlnReset(s: String)

  // Cursor
  /// Moves the cursor n lines up.
  MoveUp(n: Int)
  /// Moves the cursor n lines down.
  MoveDown(n: Int)
  /// Moves the cursor n characters left.
  MoveLeft(n: Int)
  /// Moves the cursor n characters right.
  MoveRight(n: Int)
  /// Moves the cursor n lines down and moves the caret
  /// to the beginning of the line.
  MoveToNextLine(n: Int)
  /// Moves the cursor n lines up and moves the caret
  /// to the beginning of the line.
  MoveToPreviousLine(n: Int)
  /// Moves the cursor to the given column while the row remains the same.
  MoveToColumn(n: Int)
  /// Moves the cursor to the given row while the column remains the same.
  MoveToRow(n: Int)
  /// Moves the cursor to the given row and column.
  MoveTo(column: Int, row: Int)
  /// Saves cursor position.
  SavePosition
  /// Restores cursor position.
  RestorePosition
  /// Shows cursor.
  ShowCursor
  /// Hides cursor.
  HideCursor
  /// Sets cursor style. See [`CursorStyle`](cursor.html#CursorStyle).
  SetCursorStyle(style: CursorStyle)

  // Terminal
  /// Scrolls the terminal n lines up.
  ScrollUp(n: Int)
  /// Scrolls the terminal n lines down.
  ScrollDown(n: Int)
  /// Clears the terminal. See [`ClearType`](terminal.html#ClearType).
  Clear(clear_type: ClearType)
  /// Sets terminal size.
  SetSize(columns: Int, rows: Int)
  /// Sets terminal title.
  SetTitle(title: String)
  /// Disables line wrap.
  DisableLineWrap
  /// Enables line wrap.
  EnableLineWrap
  /// Enters the alternate screen. Working in the alternate screen
  /// will not affect the main screen buffer. A good example of using an
  /// alternate screen is Vim.
  EnterAlternateScreen
  /// Leaves the alternate screen. Working in the alternate screen
  /// will not affect the main screen buffer. A good example of using an
  /// alternate screen is Vim.
  LeaveAlternateScreen

  // Event
  /// Enables mouse capture. See [`event`](event.html)
  EnableMouseCapture
  /// Disables mouse capture. See [`event`](event.html)
  DisableMouseCapture
  /// Pushes keyboard enhancement flags.
  /// See https://sw.kovidgoyal.net/kitty/keyboard-protocol/#progressive-enhancement
  PushKeyboardEnhancementFlags(flags: List(KeyboardEnhancementFlag))
  /// Pops keyboard enhancement flags.
  /// See https://sw.kovidgoyal.net/kitty/keyboard-protocol/#progressive-enhancement
  PopKeyboardEnhancementFlags
  /// Enables focus change. See [`event`](event.html)
  EnableFocusChange
  /// Disables focus change. See [`event`](event.html)
  DisableFocusChange

  // Style
  /// Sets foreground [`Color`](style.html#Color).
  /// Using [`SetForegroundAndBackgroundColors`](command.html#SetForegroundAndBackgroundColors)
  /// is prefered to set both the background and foreground colors.
  SetForegroundColor(fg: Color)
  /// Sets background [`Color`](style.html#Color).
  /// Using [`SetForegroundAndBackgroundColors`](command.html#SetForegroundAndBackgroundColors)
  /// is prefered to set both the background and foreground colors.
  SetBackgroundColor(bg: Color)
  /// Sets foreground and background [`Colors`](style.html#Color).
  /// Using this is prefered to set both the background and foreground colors.
  SetForegroundAndBackgroundColors(fg: Color, bg: Color)
  /// Sets [`Style`](style.html#Style)
  /// Example:
  /// ```gleam
  /// execute([
  ///   command.SetStyle(Style(fg: style.Red, bg: style.Black, attributes: [style.Underline])),
  ///   command.Print("my string"),
  ///   command.ResetStyle,
  /// ])
  SetStyle(style: Style)
  /// Resets [`Attributes`](style.html#Attribute) and
  /// foreground and background [`Colors`](style.html#Color).
  /// Example:
  /// ```gleam
  /// execute([
  ///   command.SetStyle(Style(fg: style.Red, bg: style.Black, attributes: [style.Underline])),
  ///   command.Print("my string"),
  ///   command.ResetStyle,
  /// ])
  ResetStyle
  /// Resets foreground and background [`Colors`](style.html#Color).
  ResetColor
  /// Resets foreground [`Colors`](style.html#Color).
  ResetForeground
  /// Resets foreground [`Colors`](style.html#Color).
  ResetBackground
  /// Sets [`Attributes`](style.html#Attribute).
  SetAttributes(attrs: List(Attribute))
  /// Resets [`Attributes`](style.html#Attribute).
  ResetAttributes
}
