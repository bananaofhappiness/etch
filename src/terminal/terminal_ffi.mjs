import { Ok, Error } from "../gleam.mjs";
import {
  FailedToEnterRawMode,
  FailedToExitRawMode,
  CouldNotGetWindowSize,
} from "../etch/terminal.mjs";

export function enter_raw() {
  try {
    process.stdin.setRawMode(true);
    return new Ok(undefined);
  } catch (error) {
    return new Error(new FailedToEnterRawMode());
  }
}

export function exit_raw() {
  try {
    process.stdin.setRawMode(false);
    return new Ok(undefined);
  } catch (error) {
    return new Error(new FailedToExitRawMode());
  }
}

export function is_raw_mode() {
  return process.stdin.isRaw;
}

export function window_size() {
  const cols = process.stdout.columns;
  const rows = process.stdout.rows;
  if (cols === undefined || rows === undefined) {
    return new Error(new CouldNotGetWindowSize());
  }
  return new Ok([cols, rows]);
}
