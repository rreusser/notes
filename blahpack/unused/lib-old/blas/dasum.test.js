import { describe, it } from "node:test";
import { loadFixture, assertClose } from "../test-helpers.js";
import { dasum } from "./dasum.js";

const fixture = loadFixture("dasum");

describe("dasum", () => {
  it("basic: sum of absolute values, n=5, incx=1", () => {
    const tc = fixture.find((t) => t.name === "basic");
    const dx = new Float64Array([1, 2, 3, 4, 5]);
    const result = dasum(5, dx, 1);
    assertClose(result, tc.result);
  });

  it("mixed signs: absolute values still sum correctly", () => {
    const tc = fixture.find((t) => t.name === "mixed_signs");
    const dx = new Float64Array([-1, 2, -3, 4, -5]);
    const result = dasum(5, dx, 1);
    assertClose(result, tc.result);
  });

  it("n=0 returns 0", () => {
    const tc = fixture.find((t) => t.name === "n_zero");
    const dx = new Float64Array([1, 2, 3]);
    const result = dasum(0, dx, 1);
    assertClose(result, tc.result);
  });

  it("n=1", () => {
    const tc = fixture.find((t) => t.name === "n_one");
    const dx = new Float64Array([42]);
    const result = dasum(1, dx, 1);
    assertClose(result, tc.result);
  });

  it("non-unit stride: incx=2", () => {
    const tc = fixture.find((t) => t.name === "stride");
    const dx = new Float64Array(6);
    dx[0] = 1; dx[2] = 2; dx[4] = 3;
    const result = dasum(3, dx, 2);
    assertClose(result, tc.result);
  });

  it("n=13 exercises the 6-unrolled loop", () => {
    const tc = fixture.find((t) => t.name === "unrolled");
    const dx = new Float64Array(13);
    for (let j = 0; j < 13; j++) {
      dx[j] = j + 1;
    }
    const result = dasum(13, dx, 1);
    assertClose(result, tc.result);
  });
});
