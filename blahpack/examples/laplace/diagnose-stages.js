#!/usr/bin/env node
// Diagnose each stage of the Laplace solver independently.
// Compare against Costa (2020) reference values.
//
// Paper reports for L-shaped domain with stp=0.01:
//   N = 17, ~186 poles total, 66 exterior, |u - Re(w)|_inf = 6.47e-7
//   Key: mmax=1000, lawson=0, Arnoldi-stabilized Vandermonde

import { aaa, timings } from '../../aaa-pure.js';
import { lssolve } from './lssolve.js';

const cadd = ([ar, ai], [br, bi]) => [ar + br, ai + bi];
const csub = ([ar, ai], [br, bi]) => [ar - br, ai - bi];
const cmul = ([ar, ai], [br, bi]) => [ar * br - ai * bi, ar * bi + ai * br];

function linspace(a, b, step) {
  const arr = [];
  if (a <= b) for (let x = a; x <= b + step/2; x += step) arr.push(x);
  else for (let x = a; x >= b - step/2; x -= step) arr.push(x);
  return arr;
}

function pointInPolygon([px, py], vertices) {
  let crossings = 0;
  const n = vertices.length;
  for (let i = 0; i < n; i++) {
    const [x1, y1] = vertices[i];
    const [x2, y2] = vertices[(i + 1) % n];
    if ((y1 <= py && y2 > py) || (y2 <= py && y1 > py)) {
      const t = (py - y1) / (y2 - y1);
      if (px < x1 + t * (x2 - x1)) crossings++;
    }
  }
  return crossings % 2 === 1;
}

function evalPoly(z, c, coeffs) {
  const dz = csub(z, c);
  let power = [1, 0], sum = [0, 0];
  for (let n = 0; n < coeffs.length; n++) {
    sum = cadd(sum, cmul(coeffs[n], power));
    power = cmul(power, dz);
  }
  return sum;
}

// ---- Setup ----
const vertices = [[0,0], [2,0], [2,1], [1,1], [1,2], [0,2]];
const stp = 0.01;
const boundary = [];
for (const x of linspace(0, 2, stp)) boundary.push([x, 0]);
for (const y of linspace(0, 1, stp)) boundary.push([2, y]);
for (const x of linspace(2, 1, stp)) boundary.push([x, 1]);
for (const y of linspace(1, 2, stp)) boundary.push([1, y]);
for (const x of linspace(1, 0, stp)) boundary.push([x, 2]);
for (const y of linspace(2, 0, stp)) boundary.push([0, y]);

const M = boundary.length;
const c = [0.5, 0.5];
const u = boundary.map(([x]) => x * x);

console.log(`=== Laplace Solver Stage Diagnostics ===`);
console.log(`Boundary points: ${M}`);
console.log(`Paper reference: N=17, ~186 poles, 66 exterior, err=6.47e-7\n`);

// ==================================================================
// STAGE 1: Smooth polynomial fit
// ==================================================================
console.log('--- Stage 1: Smooth polynomial fit ---');

const N = 10 + Math.ceil(Math.log(M)); // Same as paper: 17
console.log(`N = ${N}`);

const ncols = N + 1;
const ncolsLS = ncols + N;
const Asmooth = new Float64Array(M * ncolsLS);
const bsmooth = new Float64Array(M);

for (let j = 0; j < M; j++) {
  const dz = csub(boundary[j], c);
  let power = [1, 0];
  for (let n = 0; n <= N; n++) {
    Asmooth[j + n * M] = power[0];
    if (n >= 1) Asmooth[j + (ncols + n - 1) * M] = -power[1];
    power = cmul(power, dz);
  }
  bsmooth[j] = u[j];
}

// Check condition number of Vandermonde-like matrix (approximation: max/min column norms)
let maxColNorm = 0, minColNorm = Infinity;
for (let j = 0; j < ncolsLS; j++) {
  let norm = 0;
  for (let i = 0; i < M; i++) norm += Asmooth[i + j * M] ** 2;
  norm = Math.sqrt(norm);
  if (norm > maxColNorm) maxColNorm = norm;
  if (norm < minColNorm && norm > 0) minColNorm = norm;
}
console.log(`Vandermonde column norm ratio: ${(maxColNorm / minColNorm).toExponential(2)} (higher = more ill-conditioned)`);

const xsmooth = lssolve(Asmooth, bsmooth, M, ncolsLS);
const smoothCoeffs = [[xsmooth[0], 0]];
for (let n = 1; n <= N; n++) smoothCoeffs.push([xsmooth[n], xsmooth[ncols + n - 1]]);

// Evaluate smooth part error
const residual = new Array(M);
let maxResidual = 0;
let smoothFitError = 0;
for (let j = 0; j < M; j++) {
  const wSmooth = evalPoly(boundary[j], c, smoothCoeffs);
  residual[j] = u[j] - wSmooth[0];
  maxResidual = Math.max(maxResidual, Math.abs(residual[j]));
  // Also check the smooth fit itself
  smoothFitError = Math.max(smoothFitError, Math.abs(u[j] - wSmooth[0]));
}
console.log(`Max |residual| after smooth fit: ${maxResidual.toExponential(3)}`);
console.log(`(This is what AAA must approximate)\n`);

// Test with lower N to see if Arnoldi matters
for (const testN of [5, 10, 17, 20, 25]) {
  const tnc = testN + 1;
  const tnLS = tnc + testN;
  const At = new Float64Array(M * tnLS);
  const bt = new Float64Array(M);
  for (let j = 0; j < M; j++) {
    const dz = csub(boundary[j], c);
    let power = [1, 0];
    for (let n = 0; n <= testN; n++) {
      At[j + n * M] = power[0];
      if (n >= 1) At[j + (tnc + n - 1) * M] = -power[1];
      power = cmul(power, dz);
    }
    bt[j] = u[j];
  }
  const xt = lssolve(At, bt, M, tnLS);
  const sc = [[xt[0], 0]];
  for (let n = 1; n <= testN; n++) sc.push([xt[n], xt[tnc + n - 1]]);
  let maxR = 0;
  for (let j = 0; j < M; j++) {
    const w = evalPoly(boundary[j], c, sc);
    maxR = Math.max(maxR, Math.abs(u[j] - w[0]));
  }
  console.log(`  N=${testN.toString().padStart(2)}: max residual = ${maxR.toExponential(3)}`);
}

// ==================================================================
// STAGE 2: AAA on residual
// ==================================================================
console.log('\n--- Stage 2: AAA on residual ---');

const zComplex = boundary.map(p => p);
const fComplex = residual.map(r => [r, 0]);

// Test with different mmax values
for (const mmax of [100, 200, 500, 1000]) {
  timings.reset();
  const t0 = performance.now();
  const approx = aaa(zComplex, fComplex, 1e-13, mmax);
  const elapsed = performance.now() - t0;
  const allPoles = approx.pol || [];

  // Count interior/exterior poles
  let nExterior = 0, nInterior = 0;
  for (const pole of allPoles) {
    if (pointInPolygon(pole, vertices)) nInterior++;
    else nExterior++;
  }

  console.log(`  mmax=${mmax.toString().padStart(4)}: ${approx.converged ? 'CONVERGED' : 'not conv.'} in ${approx.errvec.length} iters, final err=${approx.errvec[approx.errvec.length-1].toExponential(3)}, ${allPoles.length} poles (${nExterior} ext, ${nInterior} int), ${elapsed.toFixed(0)}ms`);
}

// ==================================================================
// STAGE 3: Pole filtering + singular LS fit
// ==================================================================
console.log('\n--- Stage 3: Singular LS fit ---');

// Use mmax=1000 for the full pipeline
timings.reset();
const approx = aaa(zComplex, fComplex, 1e-13, 1000);
const allPoles = approx.pol || [];

const filteredPoles = [];
for (const pole of allPoles) {
  if (!pointInPolygon(pole, vertices)) filteredPoles.push(pole);
}
console.log(`Total poles: ${allPoles.length}, exterior (kept): ${filteredPoles.length}`);

const K = filteredPoles.length;
const singCols = 2 * K + 1;
const Asing = new Float64Array(M * singCols);
const bsing = new Float64Array(M);
for (let j = 0; j < M; j++) {
  for (let k = 0; k < K; k++) {
    const dz = csub(boundary[j], filteredPoles[k]);
    const denom = dz[0] * dz[0] + dz[1] * dz[1];
    Asing[j + k * M] = dz[0] / denom;
    Asing[j + (k + K) * M] = -(- dz[1] / denom);
    // = dz[1] / denom — note: -imag(1/(z-p)) = Im(z-p)/|z-p|^2
  }
  Asing[j + 2 * K * M] = 1;
  bsing[j] = residual[j];
}
const xsing = lssolve(Asing, bsing, M, singCols);

const singularCoeffs = [];
for (let k = 0; k < K; k++) singularCoeffs.push([xsing[k], xsing[k + K]]);
const b0 = [xsing[2 * K], 0];

// Evaluate singular fit
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

// Imaginary correction
const kz = evalPoly(c, c, smoothCoeffs)[1];
const kp = evalSingular(c, filteredPoles, singularCoeffs, b0)[1];
const imCorr = [0, -(kz + kp)];

// Full solution error
let maxError = 0;
let maxSingError = 0;
for (let j = 0; j < M; j++) {
  const wSmooth = evalPoly(boundary[j], c, smoothCoeffs);
  const wSing = evalSingular(boundary[j], filteredPoles, singularCoeffs, b0);
  const w = cadd(cadd(wSmooth, wSing), imCorr);
  const err = Math.abs(u[j] - w[0]);
  if (err > maxError) maxError = err;

  // Singular-only fit of residual
  const singOnly = evalSingular(boundary[j], filteredPoles, singularCoeffs, b0);
  const singErr = Math.abs(residual[j] - singOnly[0]);
  if (singErr > maxSingError) maxSingError = singErr;
}

console.log(`Singular LS fit of residual: max err = ${maxSingError.toExponential(3)}`);
console.log(`\nFull solution: |u - Re(w)|_inf = ${maxError.toExponential(3)}`);
console.log(`Paper reports:                      6.47e-7\n`);

// Test point
const u_test = (() => {
  const z = [0.99, 0.99];
  const wS = evalPoly(z, c, smoothCoeffs);
  const wP = evalSingular(z, filteredPoles, singularCoeffs, b0);
  const w = cadd(cadd(wS, wP), imCorr);
  return w[0];
})();
console.log(`u(0.99+0.99i) = ${u_test}`);
console.log(`Exact:          1.0267919261073`);
console.log(`Error:          ${Math.abs(u_test - 1.0267919261073).toExponential(3)}`);
