export function exit(n) {
  process.exit(n);
}

export function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}
