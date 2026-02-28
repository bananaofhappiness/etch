import { None, Some } from "../../gleam_stdlib/gleam/option.mjs";
import { Ok, Error } from "../gleam.mjs";
import { FailedToParseEvent, Resize } from "../etch/event.mjs";
import { window_size } from "../terminal/terminal_ffi.mjs";
let queue = [];

let resolvers = [];

export function get_chars(str) {
  return new Promise((resolve, reject) => {
    const wasRaw = !!process.stdin.isRaw;
    try {
      process.stdin.resume();
    } catch {}
    const onData = (data) => {
      cleanup();
      const buf = Buffer.isBuffer(data)
        ? data
        : Buffer.from(String(data), "utf8");
      let end = buf.length;
      if (end > 0 && buf[end - 1] === 0x0a) {
        // LF
        end--;
        if (end > 0 && buf[end - 1] === 0x0d) {
          // CR
          end--;
        }
      }
      resolve(Array.from(buf.slice(0, end)));
    };
    const onEnd = () => {
      cleanup();
      resolve([]);
    };
    const onError = (err) => {
      cleanup();
      reject(err);
    };
    function cleanup() {
      process.stdin.off("data", onData);
      process.stdin.off("end", onEnd);
      process.stdin.off("error", onError);
      try {
        process.stdin.pause();
      } catch {}
      try {
        if (!wasRaw && process.stdin.isRaw) process.stdin.setRawMode(false);
      } catch {}
    }
    process.stdin.on("data", onData);
    process.stdin.on("end", onEnd);
    process.stdin.on("error", onError);
  });
}

export function push(event) {
  if (resolvers.length > 0) {
    const resolve = resolvers.shift();
    resolve(new Some(event));
  } else {
    queue.push(new Some(event));
  }
}

export function poll(timeout_ms) {
  if (queue.length > 0) {
    return Promise.resolve(queue.shift());
  }

  return new Promise((resolve) => {
    const timer = setTimeout(() => {
      const index = resolvers.indexOf(resolver);
      if (index !== -1) {
        resolvers.splice(index, 1);
      }
      resolve(new None());
    }, timeout_ms);

    const resolver = (event) => {
      clearTimeout(timer);
      resolve(new Some(event));
    };

    resolvers.push(resolver);
  });
}

export function read() {
  if (queue.length > 0) {
    return Promise.resolve(queue.shift());
  }

  return new Promise((resolve) => {
    resolvers.push(resolve);
  });
}

export function get_cursor_position() {
  return new Promise((resolve, reject) => {
    const wasRaw = !!process.stdin.isRaw;
    try {
      process.stdin.resume();
    } catch {}
    if (!process.stdin.isRaw) {
      process.stdin.setRawMode(true);
    }
    const onData = (data) => {
      const buf = Buffer.isBuffer(data)
        ? data
        : Buffer.from(String(data), "utf8");

      const str = buf.toString("utf8");
      const cursorMatch = str.match(/^\x1b\[(\d+);(\d+)R/);
      if (cursorMatch) {
        cleanup();
        resolve(new Ok([parseInt(cursorMatch[1]), parseInt(cursorMatch[2])]));
      } else {
        cleanup();
        resolve(
          new Error(new FailedToParseEvent("Could not get cursor position")),
        );
      }
    };
    function cleanup() {
      process.stdin.off("data", onData);
      try {
        process.stdin.pause();
      } catch {}
      try {
        if (!wasRaw && process.stdin.isRaw) process.stdin.setRawMode(false);
      } catch {}
    }
    process.stdin.on("data", onData);
    process.stdout.write("\x1b[6n");
  });
}

export function get_keyboard_enhancement_flags_code() {
  return new Promise((resolve, reject) => {
    const wasRaw = !!process.stdin.isRaw;
    try {
      process.stdin.resume();
    } catch {}
    if (!process.stdin.isRaw) {
      process.stdin.setRawMode(true);
    }

    const onData = (data) => {
      const buf = Buffer.isBuffer(data)
        ? data
        : Buffer.from(String(data), "utf8");

      const str = buf.toString("utf8");

      if (str.startsWith("\x1b[?") && str.endsWith("u")) {
        cleanup();
        const flagsStr = str.slice(3);
        resolve(new Ok(flagsStr));
      } else {
        cleanup();
        resolve(
          new Error(new FailedToParseEvent("Could not get enhancement flags")),
        );
      }
    };

    function cleanup() {
      process.stdin.off("data", onData);
      try {
        process.stdin.pause();
      } catch {}
      try {
        if (!wasRaw && process.stdin.isRaw) process.stdin.setRawMode(false);
      } catch {}
    }

    process.stdin.on("data", onData);
    process.stdout.write("\x1b[?u");
  });
}

export function handle_sigwinch() {
  process.on("SIGWINCH", () => {
    const size = window_size();
    if (size instanceof Ok) {
      const [columns, rows] = size["0"];
      push(new Ok(new Resize(columns, rows)));
    } else {
      push(new Error(new CouldNotGetWindowSize()));
    }
  });
}
