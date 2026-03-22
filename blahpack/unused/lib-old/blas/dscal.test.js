import { describe, it } from "node:test";
import { loadFixture, assertArrayClose } from "../test-helpers.js";
import { dscal } from "./dscal.js";

const fixture = loadFixture("dscal");

describe("dscal", () => {
  it("basic: dx = 2*dx", () => {
    const tc = fixture.find((t) => t.name === "basic");
    const dx = new Float64Array([1, 2, 3, 4, 5]);
    dscal(5, 2.0, dx, 1);
    assertArrayClose(dx, tc.dx);
  });

  it("da=0 zeros out the vector", () => {
    const tc = fixture.find((t) => t.name === "da_zero");
    const dx = new Float64Array([1, 2, 3, 4, 5]);
    dscal(5, 0.0, dx, 1);
    assertArrayClose(dx, tc.dx);
  });

  it("da=1 is a no-op", () => {
    const tc = fixture.find((t) => t.name === "da_one");
    const dx = new Float64Array([1, 2, 3, 4, 5]);
    dscal(5, 1.0, dx, 1);
    assertArrayClose(dx, tc.dx);
  });

  it("n=0 is a no-op", () => {
    const tc = fixture.find((t) => t.name === "n_zero");
    const dx = new Float64Array([1, 2, 3, 4, 5]);
    dscal(0, 2.0, dx, 1);
    assertArrayClose(dx, tc.dx);
  });

  it("n=1", () => {
    const tc = fixture.find((t) => t.name === "n_one");
    const dx = new Float64Array([7]);
    dscal(1, 3.0, dx, 1);
    assertArrayClose(dx, tc.dx);
  });

  it("non-unit stride: incx=2", () => {
    const tc = fixture.find((t) => t.name === "stride");
    const dx = new Float64Array(20);
    dx[0] = 1; dx[2] = 2; dx[4] = 3;
    dx[1] = 99; dx[3] = 99;
    dscal(3, 4.0, dx, 2);
    assertArrayClose(dx.subarray(0, 6), tc.dx);
  });

  it("n=12 exercises 5-unrolled loop", () => {
    const tc = fixture.find((t) => t.name === "unrolled");
    const dx = new Float64Array(12);
    for (let j = 0; j < 12; j++) {
      dx[j] = j + 1;
    }
    dscal(12, 3.0, dx, 1);
    assertArrayClose(dx, tc.dx);
  });
});
