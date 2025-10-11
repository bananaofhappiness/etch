import consts.{esc}

pub fn bold(s: String) -> String {
  esc <> "[1m" <> s <> esc <> "[22m"
}

pub fn dim(s: String) -> String {
  esc <> "[2m" <> s <> esc <> "[22m"
}

pub fn italic(s: String) -> String {
  esc <> "[3m" <> s <> esc <> "[23m"
}

pub fn underline(s: String) -> String {
  esc <> "[4m" <> s <> esc <> "[24m"
}

pub fn blinking(s: String) -> String {
  esc <> "[5m" <> s <> esc <> "[25m"
}

pub fn inverse(s: String) -> String {
  esc <> "[7m" <> s <> esc <> "[27m"
}

pub fn with_black(s: String) -> String {
  esc <> "[30m" <> s
  // <> esc <> "[0m"
}

pub fn with_red(s: String) -> String {
  esc <> "[31m" <> s
  // <> esc <> "[0m"
}

pub fn with_green(s: String) -> String {
  esc <> "[32m" <> s
  // <> esc <> "[0m"
}

pub fn with_yellow(s: String) -> String {
  esc <> "[33m" <> s
  // <> esc <> "[0m"
}

pub fn with_blue(s: String) -> String {
  esc <> "[34m" <> s
  // <> esc <> "[0m"
}

pub fn with_magenta(s: String) -> String {
  esc <> "[35m" <> s
  // <> esc <> "[0m"
}

pub fn with_cyan(s: String) -> String {
  esc <> "[36m" <> s
  // <> esc <> "[0m"
}

pub fn with_white(s: String) -> String {
  esc <> "[37m" <> s
  // <> esc <> "[0m"
}

pub fn with_default(s: String) -> String {
  esc <> "[39m" <> s
  // <> esc <> "[0m"
}

pub fn black(s: String) -> String {
  esc <> "[30m" <> s <> esc <> "[0m"
}

pub fn red(s: String) -> String {
  esc <> "[31m" <> s <> esc <> "[0m"
}

pub fn green(s: String) -> String {
  esc <> "[32m" <> s <> esc <> "[0m"
}

pub fn yellow(s: String) -> String {
  esc <> "[33m" <> s <> esc <> "[0m"
}

pub fn blue(s: String) -> String {
  esc <> "[34m" <> s <> esc <> "[0m"
}

pub fn magenta(s: String) -> String {
  esc <> "[35m" <> s <> esc <> "[0m"
}

pub fn cyan(s: String) -> String {
  esc <> "[36m" <> s <> esc <> "[0m"
}

pub fn white(s: String) -> String {
  esc <> "[37m" <> s <> esc <> "[0m"
}

pub fn default(s: String) -> String {
  esc <> "[39m" <> s <> esc <> "[0m"
}

pub fn with_bg_black(s: String) -> String {
  esc <> "[40m" <> s
  // <> esc <> "[0m"
}

pub fn reset(s: String) -> String {
  s <> esc <> "[0m"
}
