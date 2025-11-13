import esc.{csi}
import gleam/int

pub type Color {
  Reset
  Black
  Grey
  Red
  BrightRed
  Green
  BrightGreen
  Yellow
  BrightYellow
  Blue
  BrightBlue
  Magenta
  BrightMagenta
  Cyan
  BrightCyan
  White
  BrightWhite
  BrightGrey
  AnsiValue(v: Int)
  Rgb(r: Int, g: Int, b: Int)
}

pub type Attribute {
  Bold
  Dim
  Underline
  Italic
  Blinking
  Inverse
  // ResetAll
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
    Reset -> panic as "Unreachable"
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
    Reset -> panic as "Unreachable"
  }
}

pub fn with(s: String, c: Color) -> String {
  case c {
    Reset -> {
      s <> csi <> "0m"
    }
    _ -> csi <> get_fg(c) <> "m" <> s
  }
}

pub fn on(s: String, c: Color) -> String {
  case c {
    Reset -> {
      s <> csi <> "0m"
    }
    _ -> csi <> get_bg(c) <> "m" <> s
  }
}

pub fn with_on(s: String, fg: Color, bg: Color) -> String {
  case fg, bg {
    Reset, Reset -> {
      s <> csi <> "0m"
    }
    fg, Reset -> {
      csi <> get_fg(fg) <> "m" <> s <> csi <> "0m"
    }
    Reset, bg -> {
      csi <> get_bg(bg) <> "m" <> s <> csi <> "0m"
    }
    fg, bg -> csi <> get_fg(fg) <> ";" <> get_bg(bg) <> "m" <> s
  }
}

pub fn attributes(s: String, a: List(Attribute)) -> String {
  get_attributes(a, "") <> s
}

fn get_attributes(a: List(Attribute), acc: String) -> String {
  case a {
    [] -> ""
    [attr] ->
      case attr {
        Bold -> csi <> acc <> "1m"
        Dim -> csi <> acc <> "2m"
        Italic -> csi <> acc <> "3m"
        Underline -> csi <> acc <> "4m"
        Blinking -> csi <> acc <> "5m"
        Inverse -> csi <> acc <> "7m"
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

pub fn bold(s: String) -> String {
  csi <> "1m" <> s <> csi <> "22m"
}

pub fn dim(s: String) -> String {
  csi <> "2m" <> s <> csi <> "22m"
}

pub fn italic(s: String) -> String {
  csi <> "3m" <> s <> csi <> "23m"
}

pub fn underline(s: String) -> String {
  csi <> "4m" <> s <> csi <> "24m"
}

pub fn blinking(s: String) -> String {
  csi <> "5m" <> s <> csi <> "25m"
}

pub fn inverse(s: String) -> String {
  csi <> "7m" <> s <> csi <> "27m"
}

pub fn reset_colors(s: String) -> String {
  s <> csi <> "0m"
}

pub fn reset_attributes(s: String) -> String {
  s <> csi <> "22;23;24;25;27m"
}

pub fn black(s: String) -> String {
  csi <> "30m" <> s <> csi <> "0m"
}

pub fn red(s: String) -> String {
  csi <> "31m" <> s <> csi <> "0m"
}

pub fn bright_red(s: String) -> String {
  csi <> "91m" <> s <> csi <> "0m"
}

pub fn green(s: String) -> String {
  csi <> "32m" <> s <> csi <> "0m"
}

pub fn bright_green(s: String) -> String {
  csi <> "92m" <> s <> csi <> "0m"
}

pub fn yellow(s: String) -> String {
  csi <> "33m" <> s <> csi <> "0m"
}

pub fn bright_yellow(s: String) -> String {
  csi <> "93m" <> s <> csi <> "0m"
}

pub fn blue(s: String) -> String {
  csi <> "34m" <> s <> csi <> "0m"
}

pub fn bright_blue(s: String) -> String {
  csi <> "94m" <> s <> csi <> "0m"
}

pub fn magenta(s: String) -> String {
  csi <> "35m" <> s <> csi <> "0m"
}

pub fn bright_magenta(s: String) -> String {
  csi <> "95m" <> s <> csi <> "0m"
}

pub fn cyan(s: String) -> String {
  csi <> "36m" <> s <> csi <> "0m"
}

pub fn bright_cyan(s: String) -> String {
  csi <> "96m" <> s <> csi <> "0m"
}

pub fn white(s: String) -> String {
  csi <> "37m" <> s <> csi <> "0m"
}

pub fn bright_white(s: String) -> String {
  csi <> "97m" <> s <> csi <> "0m"
}

pub fn bright_grey(s: String) -> String {
  csi <> "38;5;7m" <> s <> csi <> "0m"
}

pub fn grey(s: String) -> String {
  csi <> "90m" <> s <> csi <> "0m"
}

pub fn on_black(s: String) -> String {
  csi <> "40m" <> s <> csi <> "0m"
}

pub fn on_red(s: String) -> String {
  csi <> "41m" <> s <> csi <> "0m"
}

pub fn on_bright_red(s: String) -> String {
  csi <> "101m" <> s <> csi <> "0m"
}

pub fn on_green(s: String) -> String {
  csi <> "42m" <> s <> csi <> "0m"
}

pub fn on_bright_green(s: String) -> String {
  csi <> "102m" <> s <> csi <> "0m"
}

pub fn on_yellow(s: String) -> String {
  csi <> "43m" <> s <> csi <> "0m"
}

pub fn on_bright_yellow(s: String) -> String {
  csi <> "103m" <> s <> csi <> "0m"
}

pub fn on_blue(s: String) -> String {
  csi <> "44m" <> s <> csi <> "0m"
}

pub fn on_bright_blue(s: String) -> String {
  csi <> "104m" <> s <> csi <> "0m"
}

pub fn on_magenta(s: String) -> String {
  csi <> "45m" <> s <> csi <> "0m"
}

pub fn on_bright_magenta(s: String) -> String {
  csi <> "105m" <> s <> csi <> "0m"
}

pub fn on_cyan(s: String) -> String {
  csi <> "46m" <> s <> csi <> "0m"
}

pub fn on_bright_cyan(s: String) -> String {
  csi <> "106m" <> s <> csi <> "0m"
}

pub fn on_white(s: String) -> String {
  csi <> "47m" <> s <> csi <> "0m"
}

pub fn on_bright_white(s: String) -> String {
  csi <> "107m" <> s <> csi <> "0m"
}

pub fn on_bright_grey(s: String) -> String {
  csi <> "48;5;7m" <> s <> csi <> "0m"
}

pub fn on_grey(s: String) -> String {
  csi <> "100m" <> s <> csi <> "0m"
}
