import { describe, it } from "node:test";
import { loadFixture, assertArrayClose } from "../test-helpers.js";
import { daxpy } from "./daxpy.js";

const fixture = loadFixture("daxpy");

describe("daxpy", () => {
  it("basic: dy = dy + 2*dx", () => {
    const tc = fixture.find((t) => t.name === "basic");
    const dx = new Float64Array([1, 2, 3, 4, 5]);
    const dy = new Float64Array([10, 20, 30, 40, 50]);
    daxpy(5, 2.0, dx, 1, dy, 1);
    assertArrayClose(dy, tc.dy);
  });

  it("da=0 is a no-op", () => {
    const tc = fixture.find((t) => t.name === "da_zero");
    const dx = new Float64Array([1, 2, 3, 4, 5]);
    const dy = new Float64Array([10, 20, 30, 40, 50]);
    daxpy(5, 0.0, dx, 1, dy, 1);
    assertArrayClose(dy, tc.dy);
  });

  it("n=0 is a no-op", () => {
    const tc = fixture.find((t) => t.name === "n_zero");
    const dy = new Float64Array([10, 20, 30, 40, 50]);
    const dx = new Float64Array([1, 2, 3, 4, 5]);
    daxpy(0, 2.0, dx, 1, dy, 1);
    assertArrayClose(dy, tc.dy);
  });

  it("n=1", () => {
    const tc = fixture.find((t) => t.name === "n_one");
    const dx = new Float64Array([3]);
    const dy = new Float64Array([7]);
    daxpy(1, 5.0, dx, 1, dy, 1);
    assertArrayClose(dy, tc.dy);
  });

  it("non-unit strides: incx=2, incy=3", () => {
    const tc = fixture.find((t) => t.name === "stride");
    const dx = new Float64Array(20);
    const dy = new Float64Array(20);
    dx[0] = 1; dx[2] = 2; dx[4] = 3;
    dy[0] = 10; dy[3] = 20; dy[6] = 30;
    daxpy(3, 2.0, dx, 2, dy, 3);
    assertArrayClose(dy.subarray(0, 9), tc.dy);
  });

  it("negative incx", () => {
    const tc = fixture.find((t) => t.name === "neg_incx");
    const dx = new Float64Array([1, 2, 3]);
    const dy = new Float64Array([10, 20, 30]);
    daxpy(3, 1.0, dx, -1, dy, 1);
    assertArrayClose(dy, tc.dy);
  });

  it("n=10 exercises 4-unrolled loop", () => {
    const tc = fixture.find((t) => t.name === "unrolled");
    const dx = new Float64Array(10);
    const dy = new Float64Array(10);
    for (let j = 0; j < 10; j++) {
      dx[j] = j + 1;
      dy[j] = (j + 1) * 10;
    }
    daxpy(10, 3.0, dx, 1, dy, 1);
    assertArrayClose(dy, tc.dy);
  });
});
