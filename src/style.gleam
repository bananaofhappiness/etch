import esc.{esc}
import gleam/int
import gleam/list

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
      "[38;2;"
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
      "[48;2;"
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
      s <> esc("[0m")
    }
    _ -> esc("[") <> get_fg(c) <> "m" <> s
  }
}

pub fn on(s: String, c: Color) -> String {
  case c {
    Reset -> {
      s <> esc("[0m")
    }
    _ -> esc("[") <> get_bg(c) <> "m" <> s
  }
}

pub fn with_on(s: String, fg: Color, bg: Color) -> String {
  case fg, bg {
    Reset, Reset -> {
      s <> esc("[0m")
    }
    fg, Reset -> {
      esc("[") <> get_fg(fg) <> "m" <> s <> esc("[0m")
    }
    Reset, bg -> {
      esc("[") <> get_bg(bg) <> "m" <> s <> esc("[0m")
    }
    fg, bg -> esc("[") <> get_fg(fg) <> ";" <> get_bg(bg) <> "m" <> s
  }
}

pub fn attributes(s: String, a: List(Attribute)) -> String {
  esc("[") <> get_attributes(a) <> "m" <> s
}

fn get_attributes(a: List(Attribute)) -> String {
  list.fold(a, "", fn(s, a) {
    case a {
      Bold -> s <> "1"
      Dim -> s <> "2"
      Italic -> s <> "3"
      Underline -> s <> "4"
      Blinking -> s <> "5"
      Inverse -> s <> "7"
    }
  })
}

pub fn bold(s: String) -> String {
  esc("[1m") <> s <> esc("[22m")
}

pub fn dim(s: String) -> String {
  esc("[2m") <> s <> esc("[22m")
}

pub fn italic(s: String) -> String {
  esc("[3m") <> s <> esc("[23m")
}

pub fn underline(s: String) -> String {
  esc("[4m") <> s <> esc("[24m")
}

pub fn blinking(s: String) -> String {
  esc("[5m") <> s <> esc("[25m")
}

pub fn inverse(s: String) -> String {
  esc("[7m") <> s <> esc("[27m")
}

pub fn reset(s: String) -> String {
  s <> esc("[0m")
}

pub fn black(s: String) -> String {
  esc("30m") <> s <> esc("[0m")
}

pub fn red(s: String) -> String {
  esc("31m") <> s <> esc("[0m")
}

pub fn bright_red(s: String) -> String {
  esc("91m") <> s <> esc("[0m")
}

pub fn green(s: String) -> String {
  esc("32m") <> s <> esc("[0m")
}

pub fn bright_green(s: String) -> String {
  esc("92m") <> s <> esc("[0m")
}

pub fn yellow(s: String) -> String {
  esc("33m") <> s <> esc("[0m")
}

pub fn bright_yellow(s: String) -> String {
  esc("93m") <> s <> esc("[0m")
}

pub fn blue(s: String) -> String {
  esc("34m") <> s <> esc("[0m")
}

pub fn bright_blue(s: String) -> String {
  esc("94m") <> s <> esc("[0m")
}

pub fn magenta(s: String) -> String {
  esc("35m") <> s <> esc("[0m")
}

pub fn bright_magenta(s: String) -> String {
  esc("95m") <> s <> esc("[0m")
}

pub fn cyan(s: String) -> String {
  esc("36m") <> s <> esc("[0m")
}

pub fn bright_cyan(s: String) -> String {
  esc("96m") <> s <> esc("[0m")
}

pub fn white(s: String) -> String {
  esc("37m") <> s <> esc("[0m")
}

pub fn bright_white(s: String) -> String {
  esc("97m") <> s <> esc("[0m")
}

pub fn bright_grey(s: String) -> String {
  esc("38;5;7m") <> s <> esc("[0m")
}

pub fn grey(s: String) -> String {
  esc("90m") <> s <> esc("[0m")
}

pub fn on_black(s: String) -> String {
  esc("40") <> s <> esc("[0m")
}

pub fn on_red(s: String) -> String {
  esc("41") <> s <> esc("[0m")
}

pub fn on_bright_red(s: String) -> String {
  esc("101") <> s <> esc("[0m")
}

pub fn on_green(s: String) -> String {
  esc("42") <> s <> esc("[0m")
}

pub fn on_bright_green(s: String) -> String {
  esc("102") <> s <> esc("[0m")
}

pub fn on_yellow(s: String) -> String {
  esc("43") <> s <> esc("[0m")
}

pub fn on_bright_yellow(s: String) -> String {
  esc("103") <> s <> esc("[0m")
}

pub fn on_blue(s: String) -> String {
  esc("44") <> s <> esc("[0m")
}

pub fn on_bright_blue(s: String) -> String {
  esc("104") <> s <> esc("[0m")
}

pub fn on_magenta(s: String) -> String {
  esc("45") <> s <> esc("[0m")
}

pub fn on_bright_magenta(s: String) -> String {
  esc("105") <> s <> esc("[0m")
}

pub fn on_cyan(s: String) -> String {
  esc("46") <> s <> esc("[0m")
}

pub fn on_bright_cyan(s: String) -> String {
  esc("106") <> s <> esc("[0m")
}

pub fn on_white(s: String) -> String {
  esc("47") <> s <> esc("[0m")
}

pub fn on_bright_white(s: String) -> String {
  esc("107") <> s <> esc("[0m")
}

pub fn on_bright_grey(s: String) -> String {
  esc("48;5;7") <> s <> esc("[0m")
}

pub fn on_grey(s: String) -> String {
  esc("100") <> s <> esc("[0m")
}
