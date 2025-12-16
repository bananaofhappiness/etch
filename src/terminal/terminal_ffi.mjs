export function enter_raw() {
  process.stdin.setRawMode(true);
}

export function window_size() {
  return [process.stdout.columns, process.stdout.rows];
}
