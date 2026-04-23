// Laplace solver for 2D Dirichlet problems on polygonal domains.
// Based on Costa (2020), "Solving Laplace problems with the AAA algorithm".
//
// Given boundary points z_j and values u(z_j), finds w(z) analytic inside
// the domain such that Re(w) ≈ u on the boundary and Re(w) is harmonic inside.

import { aaa } from "../../aaa-pure.js";
import { lssolve } from "./lssolve.js";

// ---- Complex arithmetic ----
const cadd = ([ar, ai], [br, bi]) => [ar + br, ai + bi];
const csub = ([ar, ai], [br, bi]) => [ar - br, ai - bi];
const cmul = ([ar, ai], [br, bi]) => [ar * br - ai * bi, ar * bi + ai * br];

// ---- Point-in-polygon (ray casting) ----
function pointInPolygon([px, py], vertices) {
  let crossings = 0;
  const n = vertices.length;
  for (let i = 0; i < n; i++) {
    const [x1, y1] = vertices[i];
    const [x2, y2] = vertices[(i + 1) % n];
    if ((y1 <= py && y2 > py) || (y2 <= py && y1 > py)) {
      const t = (py - y1) / (y2 - y1);
      const xIntersect = x1 + t * (x2 - x1);
      if (px < xIntersect) crossings++;
    }
  }
  return crossings % 2 === 1;
}

// ---- Evaluate complex polynomial Σ a_n (z-c)^n ----
function evalPoly(z, c, coeffs) {
  const dz = csub(z, c);
  let power = [1, 0];
  let sum = [0, 0];
  for (let n = 0; n < coeffs.length; n++) {
    sum = cadd(sum, cmul(coeffs[n], power));
    power = cmul(power, dz);
  }
  return sum;
}

// ---- Evaluate singular part Σ b_k/(z-p_k) + b0 ----
function evalSingular(z, poles, coeffs, b0) {
  let sum = [b0[0], b0[1]];
  for (let k = 0; k < poles.length; k++) {
    const dz = csub(z, poles[k]);
    const denom = dz[0] * dz[0] + dz[1] * dz[1];
    const inv = [dz[0] / denom, -dz[1] / denom];
    sum = cadd(sum, cmul(coeffs[k], inv));
  }
  return sum;
}

// ---- Main Laplace solver ----
export function laplace(boundaryPoints, boundaryValues, polygonVertices, options = {}) {
  const M = boundaryPoints.length;
  const c = options.center || centroid(polygonVertices);
  const N = options.N || 10 + Math.ceil(Math.log(M));
  const interior = options.interior !== false;

  // ---- Step 1: Smooth part (polynomial fit) ----
  const ncols = N + 1;
  const ncolsLS = ncols + N;
  const Asmooth = new Float64Array(M * ncolsLS);
  const bsmooth = new Float64Array(M);

  for (let j = 0; j < M; j++) {
    const dz = csub(boundaryPoints[j], c);
    let power = [1, 0];
    for (let n = 0; n <= N; n++) {
      Asmooth[j + n * M] = power[0];
      if (n >= 1) {
        Asmooth[j + (ncols + n - 1) * M] = -power[1];
      }
      power = cmul(power, dz);
    }
    bsmooth[j] = boundaryValues[j];
  }

  const xsmooth = lssolve(Asmooth, bsmooth, M, ncolsLS);

  const smoothCoeffs = [];
  smoothCoeffs.push([xsmooth[0], 0]);
  for (let n = 1; n <= N; n++) {
    smoothCoeffs.push([xsmooth[n], xsmooth[ncols + n - 1]]);
  }

  // ---- Step 2: Compute residual ----
  const residual = new Array(M);
  for (let j = 0; j < M; j++) {
    const wSmooth = evalPoly(boundaryPoints[j], c, smoothCoeffs);
    residual[j] = boundaryValues[j] - wSmooth[0];
  }

  // ---- Step 3: AAA on residual ----
  const zComplex = boundaryPoints.map(([r, i]) => [r, i]);
  const fComplex = residual.map((r) => [r, 0]);
  const mmax = options.mmax || 1000;
  const approx = aaa(zComplex, fComplex, 1e-13, mmax);

  const allPoles = approx.pol || [];
  const allZeros = approx.zer || [];

  // ---- Step 4: Filter poles ----
  const filteredPoles = [];
  for (const pole of allPoles) {
    const isInside = pointInPolygon(pole, polygonVertices);
    if (interior ? !isInside : isInside) {
      filteredPoles.push(pole);
    }
  }

  const K = filteredPoles.length;

  // ---- Step 5: Singular part (Cauchy matrix fit) ----
  const singCols = 2 * K + 1;
  const Asing = new Float64Array(M * singCols);
  const bsing = new Float64Array(M);

  for (let j = 0; j < M; j++) {
    for (let k = 0; k < K; k++) {
      const dz = csub(boundaryPoints[j], filteredPoles[k]);
      const denom = dz[0] * dz[0] + dz[1] * dz[1];
      const invRe = dz[0] / denom;
      const invIm = -dz[1] / denom;
      Asing[j + k * M] = invRe;
      Asing[j + (k + K) * M] = -invIm;
    }
    Asing[j + 2 * K * M] = 1;
    bsing[j] = residual[j];
  }

  const xsing = lssolve(Asing, bsing, M, singCols);

  const singularCoeffs = [];
  for (let k = 0; k < K; k++) {
    singularCoeffs.push([xsing[k], xsing[k + K]]);
  }
  const b0 = [xsing[2 * K], 0];

  // ---- Step 6: Imaginary constant correction ----
  const kz = evalPoly(c, c, smoothCoeffs)[1];
  const kp = evalSingular(c, filteredPoles, singularCoeffs, b0)[1];
  const imCorr = [0, -(kz + kp)];

  // ---- Compute boundary error ----
  let maxError = 0;
  for (let j = 0; j < M; j++) {
    const wSmooth = evalPoly(boundaryPoints[j], c, smoothCoeffs);
    const wSing = evalSingular(boundaryPoints[j], filteredPoles, singularCoeffs, b0);
    const w = cadd(cadd(wSmooth, wSing), imCorr);
    const err = Math.abs(boundaryValues[j] - w[0]);
    if (err > maxError) maxError = err;
  }

  // ---- Build evaluator ----
  function evaluateComplex(z) {
    const wS = evalPoly(z, c, smoothCoeffs);
    const wP = evalSingular(z, filteredPoles, singularCoeffs, b0);
    return cadd(cadd(wS, wP), imCorr);
  }

  function evaluate(z) {
    if (typeof z[0] === "number") {
      return evaluateComplex(z)[0];
    }
    return z.map((zi) => evaluate(zi));
  }

  return {
    evaluate,
    evaluateComplex,
    poles: filteredPoles,
    allPoles,
    allZeros,
    smoothCoeffs,
    singularCoeffs,
    b0,
    maxError,
    center: c,
    imCorr: imCorr[1],
    N,
  };
}

function centroid(vertices) {
  let sx = 0, sy = 0;
  for (const [x, y] of vertices) { sx += x; sy += y; }
  return [sx / vertices.length, sy / vertices.length];
}

export { pointInPolygon };
