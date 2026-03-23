// Exterior potential flow solver using AAA rational approximation.
// Combines the approach of laplace.js (Costa 2020) with the potential flow
// formulation of Baddoo (2020).
//
// Solves: w(z) = z + f(z), Im[w] = 0 on ∂D, where f → 0 as |z| → ∞.
// The boundary condition becomes: Im[f(z_j)] = -y_j.
//
// Unlike laplace.js which solves Re[w] = u, this solver enforces an
// imaginary-part boundary condition. The least-squares system uses:
//   Im[c * φ(z)] = Re(c) * Im(φ) + Im(c) * Re(φ)

import { lssolve } from "../laplace/lssolve.js";

const cadd = ([ar, ai], [br, bi]) => [ar + br, ai + bi];
const csub = ([ar, ai], [br, bi]) => [ar - br, ai - bi];
const cmul = ([ar, ai], [br, bi]) => [ar * br - ai * bi, ar * bi + ai * br];

// Evaluate Σ a_n / (z-c)^n (exterior Runge basis)
function evalRunge(z, c, coeffs) {
  const dz = csub(z, c);
  const d = dz[0] * dz[0] + dz[1] * dz[1];
  const inv = [dz[0] / d, -dz[1] / d]; // 1/(z-c)
  let power = [1, 0];
  let sum = [0, 0];
  for (let n = 0; n < coeffs.length; n++) {
    sum = cadd(sum, cmul(coeffs[n], power));
    power = cmul(power, inv);
  }
  return sum;
}

// Evaluate Σ b_k / (z - p_k) + b0
function evalSingular(z, poles, coeffs, b0) {
  let sum = [b0[0], b0[1]];
  for (let k = 0; k < poles.length; k++) {
    const dz = csub(z, poles[k]);
    const d = dz[0] * dz[0] + dz[1] * dz[1];
    const inv = [dz[0] / d, -dz[1] / d];
    sum = cadd(sum, cmul(coeffs[k], inv));
  }
  return sum;
}

function centroid(vertices) {
  let sx = 0, sy = 0;
  for (const [x, y] of vertices) { sx += x; sy += y; }
  return [sx / vertices.length, sy / vertices.length];
}

export function exteriorFlow(boundaryPoints, polygonVertices, options = {}) {
  const { aaa, pointInPolygon } = options;
  if (!aaa) throw new Error('exteriorFlow requires options.aaa (AAA algorithm function)');
  if (!pointInPolygon) throw new Error('exteriorFlow requires options.pointInPolygon');

  const M = boundaryPoints.length;
  const c = options.center || centroid(polygonVertices);
  const N = options.N || 10 + Math.ceil(Math.log(M));

  // ---- Step 1: Smooth part via 1/(z-c)^n basis ----
  // Solve Im[f_smooth] = -y on boundary
  const ncols = N + 1; // degrees 0..N
  // For Im condition: Im[a_n / (z-c)^n] = Re(a_n)*Im[1/(z-c)^n] + Im(a_n)*Re[1/(z-c)^n]
  // a_0 is real (no Im part), so first column is Im[1] = 0 (skip it).
  // Remaining: 2*N columns for Re/Im of a_1..a_N.
  const ncolsLS = 2 * N;
  const Asmooth = new Float64Array(M * ncolsLS);
  const bsmooth = new Float64Array(M);

  for (let j = 0; j < M; j++) {
    const dz = csub(boundaryPoints[j], c);
    const d = dz[0] * dz[0] + dz[1] * dz[1];
    const inv = [dz[0] / d, -dz[1] / d];
    let power = [inv[0], inv[1]]; // 1/(z-c)^1
    for (let n = 1; n <= N; n++) {
      // Im[a_n * power] = Re(a_n)*Im(power) + Im(a_n)*Re(power)
      Asmooth[j + (2 * (n - 1)) * M] = power[1];       // Re(a_n) column
      Asmooth[j + (2 * (n - 1) + 1) * M] = power[0];   // Im(a_n) column
      power = cmul(power, inv);
    }
    bsmooth[j] = -boundaryPoints[j][1]; // -y
  }

  const xsmooth = lssolve(Asmooth, bsmooth, M, ncolsLS);

  const smoothCoeffs = [[0, 0]]; // a_0 = 0
  for (let n = 1; n <= N; n++) {
    smoothCoeffs.push([xsmooth[2 * (n - 1)], xsmooth[2 * (n - 1) + 1]]);
  }

  // ---- Step 2: Compute residual ----
  const residual = new Array(M);
  for (let j = 0; j < M; j++) {
    const fSmooth = evalRunge(boundaryPoints[j], c, smoothCoeffs);
    residual[j] = -boundaryPoints[j][1] - fSmooth[1]; // target - Im[fSmooth]
  }

  // ---- Step 3: AAA on residual (treat as real-valued) ----
  const zComplex = boundaryPoints.map(([r, i]) => [r, i]);
  const fComplex = residual.map(r => [r, 0]);
  const mmax = options.mmax || 200;
  const approx = aaa(zComplex, fComplex, 1e-13, mmax);

  const allPoles = approx.pol || [];

  // ---- Step 4: Filter poles — keep only those INSIDE the body ----
  const filteredPoles = [];
  for (const pole of allPoles) {
    if (pointInPolygon(pole, polygonVertices)) {
      filteredPoles.push(pole);
    }
  }
  const K = filteredPoles.length;

  // ---- Step 5: Singular part — fit Im condition ----
  const singCols = 2 * K + 1; // Re/Im of b_k + real b_0
  const Asing = new Float64Array(M * singCols);
  const bsing = new Float64Array(M);

  for (let j = 0; j < M; j++) {
    for (let k = 0; k < K; k++) {
      const dz = csub(boundaryPoints[j], filteredPoles[k]);
      const d = dz[0] * dz[0] + dz[1] * dz[1];
      const invRe = dz[0] / d;
      const invIm = -dz[1] / d;
      // Im[b_k * inv] = Re(b_k)*invIm + Im(b_k)*invRe
      Asing[j + k * M] = invIm;
      Asing[j + (k + K) * M] = invRe;
    }
    // b_0 constant: Im[b_0] where b_0 is real → 0. Skip? No — b_0 can be complex.
    // Actually for Im condition with real b_0: Im[b_0] = 0. Not useful.
    // Instead, let b_0 be purely imaginary: Im[i*b_0] = b_0. Column = 1.
    Asing[j + 2 * K * M] = 1;
    bsing[j] = residual[j];
  }

  const xsing = lssolve(Asing, bsing, M, singCols);

  const singularCoeffs = [];
  for (let k = 0; k < K; k++) {
    singularCoeffs.push([xsing[k], xsing[k + K]]);
  }
  const b0 = [0, xsing[2 * K]]; // purely imaginary constant

  // ---- Step 6: Imaginary constant correction ----
  // Ensure Im[f(c)] is correct (center of body, not in flow domain, but sets the gauge)
  const imCorr = [0, 0];

  // ---- Compute boundary error ----
  let maxError = 0;
  for (let j = 0; j < M; j++) {
    const fS = evalRunge(boundaryPoints[j], c, smoothCoeffs);
    const fP = evalSingular(boundaryPoints[j], filteredPoles, singularCoeffs, b0);
    const fIm = fS[1] + fP[1];
    const err = Math.abs(-boundaryPoints[j][1] - fIm);
    if (err > maxError) maxError = err;
  }

  // ---- Build evaluator ----
  function evaluate(z) {
    const fS = evalRunge(z, c, smoothCoeffs);
    const fP = evalSingular(z, filteredPoles, singularCoeffs, b0);
    const fR = fS[0] + fP[0];
    const fI = fS[1] + fP[1];
    // w = z + f
    const psi = z[1] + fI;
    const wR = z[0] + fR;
    return { psi, wR, fR, fI };
  }

  return {
    evaluate,
    poles: filteredPoles,
    allPoles,
    smoothCoeffs,
    singularCoeffs,
    b0,
    maxError,
    center: c,
    N,
  };
}
