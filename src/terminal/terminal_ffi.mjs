export function enter_raw() {
  process.stdin.setRawMode(true);
}

export function exit_raw() {
  process.stdin.setRawMode(false);
}

export function is_raw_mode() {
  return process.stdin.isRaw;
}

export function window_size() {
  return [process.stdout.columns, process.stdout.rows];
}
