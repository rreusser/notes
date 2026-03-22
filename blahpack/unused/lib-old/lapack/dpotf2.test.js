import { describe, it } from "node:test";
import { loadFixture, assertArrayClose, assertClose } from "../test-helpers.js";
import { dpotf2 } from "./dpotf2.js";

const fixture = loadFixture("dpotf2");

describe("dpotf2", () => {
  it("lower triangular 3x3", () => {
    const tc = fixture.find((t) => t.name === "lower_3x3");
    // A = [4 2 1; 2 5 3; 1 3 6] column-major
    const a = new Float64Array([4, 2, 1, 2, 5, 3, 1, 3, 6]);
    const info = dpotf2("L", 3, a, 3);
    assertClose(info, tc.info);
    assertArrayClose(a, tc.a);
  });

  it("upper triangular 3x3", () => {
    const tc = fixture.find((t) => t.name === "upper_3x3");
    const a = new Float64Array([4, 2, 1, 2, 5, 3, 1, 3, 6]);
    const info = dpotf2("U", 3, a, 3);
    assertClose(info, tc.info);
    assertArrayClose(a, tc.a);
  });

  it("n=1", () => {
    const tc = fixture.find((t) => t.name === "n_one");
    const a = new Float64Array([9]);
    const info = dpotf2("L", 1, a, 1);
    assertClose(info, tc.info);
    assertArrayClose(a, tc.a);
  });

  it("n=0 quick return", () => {
    const tc = fixture.find((t) => t.name === "n_zero");
    const a = new Float64Array([1]);
    const info = dpotf2("L", 0, a, 1);
    assertClose(info, tc.info);
  });

  it("not positive definite returns info > 0", () => {
    const tc = fixture.find((t) => t.name === "not_posdef");
    // [1 2; 2 1] — not positive definite
    const a = new Float64Array([1, 2, 2, 1]);
    const info = dpotf2("L", 2, a, 2);
    assertClose(info, tc.info);
    assertArrayClose(a, tc.a);
  });

  it("4x4 identity matrix", () => {
    const tc = fixture.find((t) => t.name === "identity_4x4");
    const a = new Float64Array(16);
    a[0] = 1; a[5] = 1; a[10] = 1; a[15] = 1;
    const info = dpotf2("L", 4, a, 4);
    assertClose(info, tc.info);
    assertArrayClose(a, tc.a);
  });
});
