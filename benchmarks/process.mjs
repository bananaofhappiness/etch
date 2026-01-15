export function sleep(ms) {
  const start = performance.now();
  while (performance.now() - start < ms) {
    // busy-wait
  }
}

import * as fs from "node:fs";

export function write_file(path, contents) {
  try {
    fs.writeFileSync(path, contents);
    return [true, null];
  } catch (err) {
    return [false, err];
  }
}

// Import Gleam List classes
import { List } from "../prelude.mjs";

export function get_cli_args() {
  const args = process.argv.slice(2);
  return List.fromArray(args);
}
