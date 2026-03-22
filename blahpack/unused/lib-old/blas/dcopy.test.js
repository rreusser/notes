import { describe, it } from "node:test";
import { loadFixture, assertArrayClose } from "../test-helpers.js";
import { dcopy } from "./dcopy.js";

const fixture = loadFixture("dcopy");

describe("dcopy", () => {
  it("basic: dy = dx", () => {
    const tc = fixture.find((t) => t.name === "basic");
    const dx = new Float64Array([1, 2, 3, 4, 5]);
    const dy = new Float64Array([10, 20, 30, 40, 50]);
    dcopy(5, dx, 1, dy, 1);
    assertArrayClose(dy, tc.dy);
  });

  it("n=0 is a no-op", () => {
    const tc = fixture.find((t) => t.name === "n_zero");
    const dy = new Float64Array([10, 20, 30, 40, 50]);
    const dx = new Float64Array([1, 2, 3, 4, 5]);
    dcopy(0, dx, 1, dy, 1);
    assertArrayClose(dy, tc.dy);
  });

  it("n=1", () => {
    const tc = fixture.find((t) => t.name === "n_one");
    const dx = new Float64Array([7]);
    const dy = new Float64Array([99]);
    dcopy(1, dx, 1, dy, 1);
    assertArrayClose(dy, tc.dy);
  });

  it("non-unit strides: incx=2, incy=3", () => {
    const tc = fixture.find((t) => t.name === "stride");
    const dx = new Float64Array(20);
    const dy = new Float64Array(20);
    dx[0] = 1; dx[2] = 2; dx[4] = 3;
    dy[0] = 10; dy[3] = 20; dy[6] = 30;
    dcopy(3, dx, 2, dy, 3);
    assertArrayClose(dy.subarray(0, 9), tc.dy);
  });

  it("negative incx", () => {
    const tc = fixture.find((t) => t.name === "neg_incx");
    const dx = new Float64Array([1, 2, 3]);
    const dy = new Float64Array([10, 20, 30]);
    dcopy(3, dx, -1, dy, 1);
    assertArrayClose(dy, tc.dy);
  });

  it("n=15 exercises 7-unrolled loop", () => {
    const tc = fixture.find((t) => t.name === "unrolled");
    const dx = new Float64Array(15);
    const dy = new Float64Array(15);
    for (let j = 0; j < 15; j++) {
      dx[j] = j + 1;
      dy[j] = (j + 1) * 10;
    }
    dcopy(15, dx, 1, dy, 1);
    assertArrayClose(dy, tc.dy);
  });
});
