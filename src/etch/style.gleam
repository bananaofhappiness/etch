//// This module provides utilities for styling terminal text.

import etch/internal/consts.{csi}
import gleam/int

/// A type that represents a color that can be set as foreground or background.
pub type Color {
  /// Default terminal color.
  Default
  /// Black color.
  Black
  /// Grey color.
  Grey
  /// Red color.
  Red
  /// Bright red color.
  BrightRed
  /// Green color.
  Green
  /// Bright green color.
  BrightGreen
  /// Yellow color.
  Yellow
  /// Bright yellow color.
  BrightYellow
  /// Blue color.
  Blue
  /// Bright blue color.
  BrightBlue
  /// Magenta color.
  Magenta
  /// Bright magenta color.
  BrightMagenta
  /// Cyan color.
  Cyan
  /// Bright cyan color.
  BrightCyan
  /// White color.
  White
  /// Bright white color.
  BrightWhite
  /// Bright grey color.
  BrightGrey
  /// Ansi color (256 colors). See https://www.ditig.com/256-colors-cheat-sheet.
  AnsiValue(v: Int)
  /// RGB color.
  Rgb(r: Int, g: Int, b: Int)
}

/// A type that represents text attributes like bold, italic, etc.
pub type Attribute {
  /// Bold text.
  Bold
  /// Dim text.
  Dim
  /// Underline text.
  Underline
  /// Italic text.
  Italic
  /// Blinking text.
  Blinking
  /// Inverse (swaps foreground and background [`Colors`](style.html#Color)).
  Inverse
}

/// Allows to save style (foreground and background [`Colors`](style.html#Color))
/// and [`Attributes`](style.html#Attribute))
/// and use it later by calling [with_style](style.html#with_style).
pub type Style {
  Style(
    /// A background [`Color`](style.html#Color).
    bg: Color,
    /// A foreground [`Color`](style.html#Color).
    fg: Color,
    /// [`Attributes`](style.html#Attribute).
    attributes: List(Attribute),
  )
}

/// Returns default [`Style`](style.html#Style) with terminal's default
/// foreground and background [`Colors`](style.html#Color))
/// and no [`Attributes`](style.html#Attribute).
pub fn default_style() -> Style {
  Style(bg: Default, fg: Default, attributes: [])
}

/// Applies style to a string.
pub fn with_style(s: String, style: Style) {
  csi
  <> get_fg(style.fg)
  <> ";"
  <> get_bg(style.bg)
  <> ";"
  <> get_attributes(style.attributes, "")
  <> "m"
  <> s
}

fn get_fg(c: Color) -> String {
  case c {
    Black -> "30"
    Red -> "31"
    BrightRed -> "91"
    Green -> "32"
    BrightGreen -> "92"
    Yellow -> "33"
    BrightYellow -> "93"
    Blue -> "34"
    BrightBlue -> "94"
    Magenta -> "35"
    BrightMagenta -> "95"
    Cyan -> "36"
    BrightCyan -> "96"
    White -> "37"
    BrightWhite -> "97"
    BrightGrey -> "38;5;7"
    Grey -> "90"
    AnsiValue(v) -> "38;5;" <> int.to_string(v)
    Rgb(r, g, b) ->
      "38;2;"
      <> int.to_string(r)
      <> ";"
      <> int.to_string(g)
      <> ";"
      <> int.to_string(b)
    Default -> "39"
  }
}

fn get_bg(c: Color) -> String {
  case c {
    Black -> "40"
    Red -> "41"
    BrightRed -> "101"
    Green -> "42"
    BrightGreen -> "102"
    Yellow -> "43"
    BrightYellow -> "103"
    Blue -> "44"
    BrightBlue -> "104"
    Magenta -> "45"
    BrightMagenta -> "105"
    Cyan -> "46"
    BrightCyan -> "106"
    White -> "47"
    BrightWhite -> "107"
    BrightGrey -> "48;5;7"
    Grey -> "100"
    AnsiValue(v) -> "48;5;" <> int.to_string(v)
    Rgb(r, g, b) ->
      "48;2;"
      <> int.to_string(r)
      <> ";"
      <> int.to_string(g)
      <> ";"
      <> int.to_string(b)
    Default -> "49"
  }
}

/// Sets the foreground color of a string. It does not reset the color after applying it.
/// Also see [`on`](style.html#on) to set the background color.
/// Using [`with_on`](style.html#with_on) is prefered to set both the background and foreground colors.
pub fn with(s: String, c: Color) -> String {
  csi <> get_fg(c) <> "m" <> s
}

/// Sets the foreground color of a string. It does not reset the color after applying it.
/// Also see [`with`](style.html#with) to set the foreground color.
/// Using [`with_on`](style.html#with_on) is prefered to set both the background and foreground colors.
pub fn on(s: String, c: Color) -> String {
  csi <> get_bg(c) <> "m" <> s
}

/// Sets both the foreground and background colors. It is the prefered way to set them both for one string.
/// Also see [`with`](style.html#with) to set the foreground color and [`on`](style.html#on) to set the background color.
pub fn with_on(s: String, fg: Color, bg: Color) -> String {
  csi <> get_fg(fg) <> ";" <> get_bg(bg) <> "m" <> s
}

/// Sets [`Attributes`](style.html#Attribute) of a string.
pub fn attributes(s: String, a: List(Attribute)) -> String {
  csi <> get_attributes(a, "") <> "m" <> s
}

fn get_attributes(a: List(Attribute), acc: String) -> String {
  case a {
    [] -> ""
    [attr] ->
      case attr {
        Bold -> acc <> "1"
        Dim -> acc <> "2"
        Italic -> acc <> "3"
        Underline -> acc <> "4"
        Blinking -> acc <> "5"
        Inverse -> acc <> "7"
      }
    [attr, ..rest] -> {
      let acc =
        case attr {
          Bold -> acc <> "1"
          Dim -> acc <> "2"
          Italic -> acc <> "3"
          Underline -> acc <> "4"
          Blinking -> acc <> "5"
          Inverse -> acc <> "7"
        }
        <> ";"
      get_attributes(rest, acc)
    }
  }
}

/// Makes the string bold and resets this attribute afterwards.
pub fn bold(s: String) -> String {
  csi <> "1m" <> s <> csi <> "22m"
}

/// Makes the string dim and resets this attribute afterwards.
pub fn dim(s: String) -> String {
  csi <> "2m" <> s <> csi <> "22m"
}

/// Makes the string italic and resets this attribute afterwards.
pub fn italic(s: String) -> String {
  csi <> "3m" <> s <> csi <> "23m"
}

/// Makes the string underline and resets this attribute afterwards.
pub fn underline(s: String) -> String {
  csi <> "4m" <> s <> csi <> "24m"
}

/// Makes the string blinking and resets this attribute afterwards.
pub fn blinking(s: String) -> String {
  csi <> "5m" <> s <> csi <> "25m"
}

/// Makes the string inverse (swaps background and foreground colors)
/// and resets this attribute afterwards. See [`Color`](style.html#Color).
pub fn inverse(s: String) -> String {
  csi <> "7m" <> s <> csi <> "27m"
}

/// Resets style (foreground, background and attributes). See [`Style`](style.html#Style).
pub fn reset_style(s: String) -> String {
  s <> csi <> "39;49;22;23;24;25;27m"
}

/// Sets style. See [`Style`](style.html#Style).
pub fn set_style(s: Style) -> String {
  with_style("", s)
}

/// Resets color of the string. See [`Color`](style.html#Color).
pub fn reset_color(s: String) -> String {
  s <> csi <> "39;49m"
}

/// Resets foreground of the string. See [`Color`](style.html#Color).
pub fn reset_foreground(s: String) -> String {
  s <> csi <> "39m"
}

/// Resets background of the string. See [`Color`](style.html#Color).
pub fn reset_background(s: String) -> String {
  s <> csi <> "49m"
}

/// Resets [`Attributes`](style.html#Attribute) of the string.
pub fn reset_attributes(s: String) -> String {
  s <> csi <> "22;23;24;25;27m"
}

/// Sets the foreground [`Color`](style.html#Color) to black and
/// resets the color afterwards.
pub fn black(s: String) -> String {
  csi <> "30m" <> s <> csi <> "39m"
}

/// Sets the foreground [`Color`](style.html#Color) to red and
/// resets the color afterwards.
pub fn red(s: String) -> String {
  csi <> "31m" <> s <> csi <> "39m"
}

/// Sets the foreground [`Color`](style.html#Color) to bright red and
/// resets the color afterwards.
pub fn bright_red(s: String) -> String {
  csi <> "91m" <> s <> csi <> "39m"
}

/// Sets the foreground [`Color`](style.html#Color) to green and
/// resets the color afterwards.
pub fn green(s: String) -> String {
  csi <> "32m" <> s <> csi <> "39m"
}

/// Sets the foreground [`Color`](style.html#Color) to bright green and
/// resets the color afterwards.
pub fn bright_green(s: String) -> String {
  csi <> "92m" <> s <> csi <> "39m"
}

/// Sets the foreground [`Color`](style.html#Color) to yellow and
/// resets the color afterwards.
pub fn yellow(s: String) -> String {
  csi <> "33m" <> s <> csi <> "39m"
}

/// Sets the foreground [`Color`](style.html#Color) to bright yellow and
/// resets the color afterwards.
pub fn bright_yellow(s: String) -> String {
  csi <> "93m" <> s <> csi <> "39m"
}

/// Sets the foreground [`Color`](style.html#Color) to blue and
/// resets the color afterwards.
pub fn blue(s: String) -> String {
  csi <> "34m" <> s <> csi <> "39m"
}

/// Sets the foreground [`Color`](style.html#Color) to bright blue and
/// resets the color afterwards.
pub fn bright_blue(s: String) -> String {
  csi <> "94m" <> s <> csi <> "39m"
}

/// Sets the foreground [`Color`](style.html#Color) to magenta and
/// resets the color afterwards.
pub fn magenta(s: String) -> String {
  csi <> "35m" <> s <> csi <> "39m"
}

/// Sets the foreground [`Color`](style.html#Color) to bright magenta and
/// resets the color afterwards.
pub fn bright_magenta(s: String) -> String {
  csi <> "95m" <> s <> csi <> "39m"
}

/// Sets the foreground [`Color`](style.html#Color) to cyan and
/// resets the color afterwards.
pub fn cyan(s: String) -> String {
  csi <> "36m" <> s <> csi <> "39m"
}

/// Sets the foreground [`Color`](style.html#Color) to bright cyan and
/// resets the color afterwards.
pub fn bright_cyan(s: String) -> String {
  csi <> "96m" <> s <> csi <> "39m"
}

/// Sets the foreground [`Color`](style.html#Color) to white and
/// resets the color afterwards.
pub fn white(s: String) -> String {
  csi <> "37m" <> s <> csi <> "39m"
}

/// Sets the foreground [`Color`](style.html#Color) to bright white and
/// resets the color afterwards.
pub fn bright_white(s: String) -> String {
  csi <> "97m" <> s <> csi <> "39m"
}

/// Sets the foreground [`Color`](style.html#Color) to bright grey and
/// resets the color afterwards.
pub fn bright_grey(s: String) -> String {
  csi <> "38;5;7m" <> s <> csi <> "39m"
}

/// Sets the foreground [`Color`](style.html#Color) to grey and
/// resets the color afterwards.
pub fn grey(s: String) -> String {
  csi <> "90m" <> s <> csi <> "39m"
}

/// Sets the background [`Color`](style.html#Color) to black
/// and resets it afterwards.
pub fn on_black(s: String) -> String {
  csi <> "40m" <> s <> csi <> "49m"
}

/// Sets the background [`Color`](style.html#Color) to red
/// and resets it afterwards.
pub fn on_red(s: String) -> String {
  csi <> "41m" <> s <> csi <> "49m"
}

/// Sets the background [`Color`](style.html#Color) to bright red
/// and resets it afterwards.
pub fn on_bright_red(s: String) -> String {
  csi <> "101m" <> s <> csi <> "49m"
}

/// Sets the background [`Color`](style.html#Color) to green
/// and resets it afterwards.
pub fn on_green(s: String) -> String {
  csi <> "42m" <> s <> csi <> "49m"
}

/// Sets the background [`Color`](style.html#Color) to bright green
/// and resets it afterwards.
pub fn on_bright_green(s: String) -> String {
  csi <> "102m" <> s <> csi <> "49m"
}

/// Sets the background [`Color`](style.html#Color) to yellow
/// and resets it afterwards.
pub fn on_yellow(s: String) -> String {
  csi <> "43m" <> s <> csi <> "49m"
}

/// Sets the background [`Color`](style.html#Color) to bright yellow
/// and resets it afterwards.
pub fn on_bright_yellow(s: String) -> String {
  csi <> "103m" <> s <> csi <> "49m"
}

/// Sets the background [`Color`](style.html#Color) to blue
/// and resets it afterwards.
pub fn on_blue(s: String) -> String {
  csi <> "44m" <> s <> csi <> "49m"
}

/// Sets the background [`Color`](style.html#Color) to bright blue
/// and resets it afterwards.
pub fn on_bright_blue(s: String) -> String {
  csi <> "104m" <> s <> csi <> "49m"
}

/// Sets the background [`Color`](style.html#Color) to magenta
/// and resets it afterwards.
pub fn on_magenta(s: String) -> String {
  csi <> "45m" <> s <> csi <> "49m"
}

/// Sets the background [`Color`](style.html#Color) to bright magenta
/// and resets it afterwards.
pub fn on_bright_magenta(s: String) -> String {
  csi <> "105m" <> s <> csi <> "49m"
}

/// Sets the background [`Color`](style.html#Color) to cyan
/// and resets it afterwards.
pub fn on_cyan(s: String) -> String {
  csi <> "46m" <> s <> csi <> "49m"
}

/// Sets the background [`Color`](style.html#Color) to bright cyan
/// and resets it afterwards.
pub fn on_bright_cyan(s: String) -> String {
  csi <> "106m" <> s <> csi <> "49m"
}

/// Sets the background [`Color`](style.html#Color) to white
/// and resets it afterwards.
pub fn on_white(s: String) -> String {
  csi <> "47m" <> s <> csi <> "49m"
}

/// Sets the background [`Color`](style.html#Color) to bright white
/// and resets it afterwards.
pub fn on_bright_white(s: String) -> String {
  csi <> "107m" <> s <> csi <> "49m"
}

/// Sets the background [`Color`](style.html#Color) to bright grey
/// and resets it afterwards.
pub fn on_bright_grey(s: String) -> String {
  csi <> "48;5;7m" <> s <> csi <> "49m"
}

/// Sets the background [`Color`](style.html#Color) to grey
/// and resets it afterwards.
pub fn on_grey(s: String) -> String {
  csi <> "100m" <> s <> csi <> "49m"
}

/// Sets the foreground [`Color`](style.html#Color) to the given ANSI value
/// and resets it afterwards.
pub fn ansi(s: String, value: Int) -> String {
  csi <> get_fg(AnsiValue(value)) <> "m" <> s <> csi <> "39m"
}

/// Sets the background [`Color`](style.html#Color) to the given ANSI value
/// and resets it afterwards.
pub fn on_ansi(s: String, value: Int) -> String {
  csi <> get_bg(AnsiValue(value)) <> "m" <> s <> csi <> "49m"
}

/// Sets the foreground [`Color`](style.html#Color) to the given RBG color
/// and resets it afterwards.
pub fn rbg(s: String, r: Int, g: Int, b: Int) -> String {
  csi <> get_fg(Rgb(r, g, b)) <> "m" <> s <> csi <> "39m"
}

/// Sets the background [`Color`](style.html#Color) to the given RBG color
/// and resets it afterwards.
pub fn on_rbg(s: String, r: Int, g: Int, b: Int) -> String {
  csi <> get_bg(Rgb(r, g, b)) <> "m" <> s <> csi <> "49m"
}
