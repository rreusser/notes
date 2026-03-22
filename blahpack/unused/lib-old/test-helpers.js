import { readFileSync } from "fs";
import { strict as assert } from "node:assert";
import { fileURLToPath } from "url";
import { dirname, join } from "path";

const TOLERANCE = 1e-14;

export function loadFixture(name) {
  const dir = dirname(fileURLToPath(import.meta.url));
  const path = join(dir, "..", "test", "fixtures", `${name}.jsonl`);
  const lines = readFileSync(path, "utf8").trim().split("\n");
  return lines.map((line) => JSON.parse(line));
}

export function assertClose(actual, expected, tol = TOLERANCE, msg = "") {
  const absErr = Math.abs(actual - expected);
  const scale = Math.max(Math.abs(expected), 1.0);
  const relErr = absErr / scale;
  assert.ok(
    relErr <= tol,
    `${msg}expected ${expected}, got ${actual} (relErr=${relErr.toExponential(3)})`,
  );
}

export function assertArrayClose(actual, expected, tol = TOLERANCE, msg = "") {
  assert.equal(
    actual.length,
    expected.length,
    `${msg}length mismatch: ${actual.length} vs ${expected.length}`,
  );
  for (let i = 0; i < expected.length; i++) {
    assertClose(actual[i], expected[i], tol, `${msg}[${i}] `);
  }
}
