import { describe, it } from "node:test";
import { loadFixture, assertClose } from "../test-helpers.js";
import { ddot } from "./ddot.js";

const fixture = loadFixture("ddot");

describe("ddot", () => {
  it("basic dot product (n=5, incx=1, incy=1)", () => {
    const tc = fixture.find((t) => t.name === "basic");
    const dx = new Float64Array([1, 2, 3, 4, 5]);
    const dy = new Float64Array([2, 3, 4, 5, 6]);
    const result = ddot(5, dx, 1, dy, 1);
    assertClose(result, tc.result);
  });

  it("n=0 returns 0", () => {
    const tc = fixture.find((t) => t.name === "n_zero");
    const dx = new Float64Array([1, 2, 3]);
    const dy = new Float64Array([4, 5, 6]);
    const result = ddot(0, dx, 1, dy, 1);
    assertClose(result, tc.result);
  });

  it("n=1", () => {
    const tc = fixture.find((t) => t.name === "n_one");
    const dx = new Float64Array([3]);
    const dy = new Float64Array([7]);
    const result = ddot(1, dx, 1, dy, 1);
    assertClose(result, tc.result);
  });

  it("non-unit strides: incx=2, incy=2", () => {
    const tc = fixture.find((t) => t.name === "stride");
    const dx = new Float64Array(6);
    const dy = new Float64Array(6);
    dx[0] = 1; dx[2] = 2; dx[4] = 3;
    dy[0] = 4; dy[2] = 5; dy[4] = 6;
    const result = ddot(3, dx, 2, dy, 2);
    assertClose(result, tc.result);
  });

  it("negative strides", () => {
    const tc = fixture.find((t) => t.name === "neg_inc");
    const dx = new Float64Array([1, 2, 3]);
    const dy = new Float64Array([4, 5, 6]);
    const result = ddot(3, dx, -1, dy, 1);
    assertClose(result, tc.result);
  });

  it("orthogonal vectors return 0", () => {
    const tc = fixture.find((t) => t.name === "orthogonal");
    const dx = new Float64Array([1, 0]);
    const dy = new Float64Array([0, 1]);
    const result = ddot(2, dx, 1, dy, 1);
    assertClose(result, tc.result);
  });

  it("n=12 exercises 5-unrolled loop", () => {
    const tc = fixture.find((t) => t.name === "unrolled");
    const dx = new Float64Array(12);
    const dy = new Float64Array(12);
    for (let j = 0; j < 12; j++) {
      dx[j] = j + 1;
      dy[j] = (j + 1) * 2;
    }
    const result = ddot(12, dx, 1, dy, 1);
    assertClose(result, tc.result);
  });
});
