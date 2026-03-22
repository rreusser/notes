import { describe, it } from "node:test";
import { loadFixture, assertArrayClose } from "../test-helpers.js";
import { dgemv } from "./dgemv.js";

const fixture = loadFixture("dgemv");

describe("dgemv", () => {
  it("basic: y = A*x, trans=N", () => {
    const tc = fixture.find((t) => t.name === "basic");
    const a = new Float64Array([1, 2, 3, 4, 5, 6]);
    const x = new Float64Array([1, 2]);
    const y = new Float64Array(3);
    dgemv("N", 3, 2, 1.0, a, 3, x, 1, 0.0, y, 1);
    assertArrayClose(y, tc.y);
  });

  it("transpose: y = A^T*x, trans=T", () => {
    const tc = fixture.find((t) => t.name === "transpose");
    const a = new Float64Array([1, 2, 3, 4, 5, 6]);
    const x = new Float64Array([1, 2, 3]);
    const y = new Float64Array(2);
    dgemv("T", 3, 2, 1.0, a, 3, x, 1, 0.0, y, 1);
    assertArrayClose(y, tc.y);
  });

  it("alpha and beta scaling", () => {
    const tc = fixture.find((t) => t.name === "alpha_beta");
    const a = new Float64Array([1, 2, 3, 4, 5, 6]);
    const x = new Float64Array([1, 2]);
    const y = new Float64Array([10, 20, 30]);
    dgemv("N", 3, 2, 2.0, a, 3, x, 1, 3.0, y, 1);
    assertArrayClose(y, tc.y);
  });

  it("n=0 quick return", () => {
    const tc = fixture.find((t) => t.name === "n_zero");
    const y = new Float64Array([99]);
    dgemv("N", 3, 0, 1.0, new Float64Array(6), 3, new Float64Array(2), 1, 0.0, y, 1);
    assertArrayClose(y, tc.y);
  });

  it("m=0 quick return", () => {
    const tc = fixture.find((t) => t.name === "m_zero");
    const y = new Float64Array([99]);
    dgemv("N", 0, 2, 1.0, new Float64Array(6), 1, new Float64Array(2), 1, 0.0, y, 1);
    assertArrayClose(y, tc.y);
  });

  it("non-unit strides: incx=2, incy=2", () => {
    const tc = fixture.find((t) => t.name === "stride");
    const a = new Float64Array([1, 2, 3, 4, 5, 6]);
    const x = new Float64Array(20);
    x[0] = 1; x[2] = 2;
    const y = new Float64Array(20);
    y[0] = 10; y[2] = 20; y[4] = 30;
    dgemv("N", 3, 2, 1.0, a, 3, x, 2, 1.0, y, 2);
    assertArrayClose(y.subarray(0, 6), tc.y);
  });

  it("transpose with alpha and beta", () => {
    const tc = fixture.find((t) => t.name === "transpose_alpha_beta");
    const a = new Float64Array([1, 2, 3, 4, 5, 6]);
    const x = new Float64Array([1, 1, 1]);
    const y = new Float64Array([5, 10]);
    dgemv("T", 3, 2, 2.0, a, 3, x, 1, 3.0, y, 1);
    assertArrayClose(y, tc.y);
  });

  it("alpha=0 just scales y by beta", () => {
    const tc = fixture.find((t) => t.name === "alpha_zero");
    const a = new Float64Array([1, 2, 3, 4, 5, 6]);
    const x = new Float64Array([1, 1, 1]);
    const y = new Float64Array([10, 20, 30]);
    dgemv("N", 3, 2, 0.0, a, 3, x, 1, 2.0, y, 1);
    assertArrayClose(y, tc.y);
  });
});
