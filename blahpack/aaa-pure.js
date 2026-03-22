// Pure JavaScript AAA algorithm — no WASM dependency.
// Uses blahpack's zgesvd for the SVD step.
// Reference: Nakatsukasa, Sète, Trefethen, "The AAA Algorithm for Rational
// Approximation", SIAM J. Sci. Comput. 40(3), A1494–A1522, 2018.
// MATLAB code: arXiv:1612.00337v2, aaa_alg.m

import { prz, cleanup, feval, barycentricRoots } from './prz.js';
import zgesvd from './lib/lapack/base/zgesvd/lib/ndarray.js';
import { createRequire } from 'module';
const require = createRequire(import.meta.url);
const Complex128Array = require( '@stdlib/array/complex128' );
const reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );

// Timing accumulator — populated during aaa() calls, read by profiling harness
export const timings = {
  svd: 0,
  cauchy: 0,
  aMatrix: 0,
  ratEval: 0,
  prz: 0,
  total: 0,
  iters: 0,
  reset() { this.svd = 0; this.cauchy = 0; this.aMatrix = 0; this.ratEval = 0; this.prz = 0; this.total = 0; this.iters = 0; }
};

export function aaa(Z, F, tol = 1e-13, mmax = 100) {
  const t_total = performance.now();
  const M = Z.length;
  if (M !== F.length) throw new Error('Z and F must have the same length');
  if (mmax > M) mmax = M;

  // Flatten inputs to interleaved Float64Arrays for efficient inner-loop access.
  // Z and F are arrays of [re, im] pairs from the caller.
  const Zr = new Float64Array(2 * M);
  const Fr = new Float64Array(2 * M);
  for (let i = 0; i < M; i++) {
    Zr[2 * i] = Z[i][0]; Zr[2 * i + 1] = Z[i][1];
    Fr[2 * i] = F[i][0]; Fr[2 * i + 1] = F[i][1];
  }

  // Support points/values as flat interleaved arrays (grow up to mmax)
  const zr = new Float64Array(2 * mmax);
  const fr = new Float64Array(2 * mmax);
  // Weights (rewritten each iteration, up to mmax)
  const wr = new Float64Array(2 * mmax);
  const errvec = [];

  // Cauchy matrix C(i, k): M rows, up to mmax columns, interleaved complex.
  // Column-major: C(i, k) real at Cr[2*(i + k*M)], imag at +1.
  const Cr = new Float64Array(2 * M * mmax);

  // J = index set of non-support points, stored as a flat integer array
  const J = new Int32Array(M);
  for (let i = 0; i < M; i++) J[i] = i;
  let nJ = M;

  // R = mean(F) — interleaved
  const Rr = new Float64Array(2 * M);
  let meanR = 0, meanI = 0;
  for (let i = 0; i < M; i++) { meanR += Fr[2 * i]; meanI += Fr[2 * i + 1]; }
  meanR /= M; meanI /= M;
  for (let i = 0; i < M; i++) { Rr[2 * i] = meanR; Rr[2 * i + 1] = meanI; }

  // Fnorm = max |F|
  let Fnorm = 0;
  for (let i = 0; i < M; i++) {
    const a = Math.hypot(Fr[2 * i], Fr[2 * i + 1]);
    if (a > Fnorm) Fnorm = a;
  }

  // Pre-allocate SVD workspace at maximum sizes to avoid per-iteration allocation.
  // Complex arrays use Complex128Array (strides/offsets in complex elements).
  // Real arrays (singular values, RWORK) use Float64Array.
  const maxN = mmax;
  const maxM_svd = M;
  const Adata = new Complex128Array(maxM_svd * maxN);
  const Av = reinterpret(Adata, 0); // Float64Array view for efficient fill
  const sData = new Float64Array(maxN);
  const UData = new Complex128Array(1);
  const VTData = new Complex128Array(maxN * maxN);
  const VTv = reinterpret(VTData, 0); // Float64Array view for reading results
  const lworkMax = Math.max(1, 8 * (maxM_svd + maxN));
  const WORK = new Complex128Array(lworkMax);
  const RWORK = new Float64Array(5 * maxN);

  let converged = false;
  let m = 0;

  for (let iter = 0; iter < mmax; iter++) {
    timings.iters++;

    // j = argmax |F - R|
    let maxVal = -1, j = 0;
    for (let i = 0; i < M; i++) {
      const dr = Fr[2 * i] - Rr[2 * i], di = Fr[2 * i + 1] - Rr[2 * i + 1];
      const a = dr * dr + di * di; // compare squared magnitudes (skip sqrt)
      if (a > maxVal) { maxVal = a; j = i; }
    }

    // z = [z; Z(j)]; f = [f; F(j)]
    zr[2 * m] = Zr[2 * j]; zr[2 * m + 1] = Zr[2 * j + 1];
    fr[2 * m] = Fr[2 * j]; fr[2 * m + 1] = Fr[2 * j + 1];
    m++;

    // J(J==j) = []
    for (let i = 0; i < nJ; i++) {
      if (J[i] === j) {
        for (let k = i; k < nJ - 1; k++) J[k] = J[k + 1];
        nJ--;
        break;
      }
    }

    // C(:, iter) = 1 / (Z - Z(j))
    let t0 = performance.now();
    const zjr = Zr[2 * j], zji = Zr[2 * j + 1];
    for (let i = 0; i < M; i++) {
      const dr = Zr[2 * i] - zjr, di = Zr[2 * i + 1] - zji;
      const d = dr * dr + di * di;
      const base = 2 * (i + iter * M);
      if (d === 0) {
        Cr[base] = 0; Cr[base + 1] = 0;
      } else {
        Cr[base] = dr / d; Cr[base + 1] = -di / d;
      }
    }
    timings.cauchy += performance.now() - t0;

    // A = SF*C - C*Sf where SF = diag(F), Sf = diag(f)
    // i.e. A(ii, k) = C(J[ii], k) * (F(J[ii]) - f[k])
    // Fill via Float64Array view of Adata (Complex128Array).
    // Column-major: element (ii, k) at complex index ii + k*nJ.
    t0 = performance.now();
    for (let k = 0; k < m; k++) {
      const fkr = fr[2 * k], fki = fr[2 * k + 1];
      for (let ii = 0; ii < nJ; ii++) {
        const ci = J[ii];
        const cBase = 2 * (ci + k * M);
        const cr = Cr[cBase], cim = Cr[cBase + 1];
        const dr = Fr[2 * ci] - fkr, di = Fr[2 * ci + 1] - fki;
        const idx = 2 * (ii + k * nJ);
        Av[idx] = cr * dr - cim * di;
        Av[idx + 1] = cr * di + cim * dr;
      }
    }
    timings.aMatrix += performance.now() - t0;

    // SVD(A(J,:), 0) — thin SVD, we only need V(:, m)
    // LAPACK returns VT = V^H; we need last row of VT, conjugated.
    const lwork = Math.max(1, 8 * (nJ + m));

    // Zero out VT region we'll read from
    for (let i = 0; i < 2 * m * m; i++) VTv[i] = 0;

    t0 = performance.now();
    // Strides/offsets in complex elements (Complex128Array convention).
    const info = zgesvd(
      'N', 'A', nJ, m,
      Adata, 1, nJ, 0,
      sData, 1, 0,
      UData, 1, 1, 0,
      VTData, 1, m, 0,
      WORK, 1, 0, lwork,
      RWORK, 1, 0
    );
    timings.svd += performance.now() - t0;

    if (info !== 0) {
      console.warn('zgesvd info =', info);
    }

    // w = V(:, m) = conj(VT(m-1, :))
    // VT is m x m column-major. VT(i, j) at Float64 index 2*(i + j*m).
    const lastRow = m - 1;
    for (let jj = 0; jj < m; jj++) {
      const idx = 2 * (lastRow + jj * m);
      wr[2 * jj] = VTv[idx];
      wr[2 * jj + 1] = -VTv[idx + 1]; // conjugate
    }

    // R = F; R(J) = N(J)./D(J) where N = C*(w.*f), D = C*w
    t0 = performance.now();
    for (let i = 0; i < 2 * M; i++) Rr[i] = Fr[i];

    for (let ii = 0; ii < nJ; ii++) {
      const ci = J[ii];
      let nr = 0, ni = 0, dr = 0, di = 0;
      for (let k = 0; k < m; k++) {
        const cBase = 2 * (ci + k * M);
        const cr = Cr[cBase], cim = Cr[cBase + 1];
        const wkr = wr[2 * k], wki = wr[2 * k + 1];
        const fkr = fr[2 * k], fki = fr[2 * k + 1];
        // w[k] * f[k]
        const wfr = wkr * fkr - wki * fki;
        const wfi = wkr * fki + wki * fkr;
        nr += cr * wfr - cim * wfi;
        ni += cr * wfi + cim * wfr;
        dr += cr * wkr - cim * wki;
        di += cr * wki + cim * wkr;
      }
      const d = dr * dr + di * di;
      Rr[2 * ci] = (nr * dr + ni * di) / d;
      Rr[2 * ci + 1] = (ni * dr - nr * di) / d;
    }
    timings.ratEval += performance.now() - t0;

    // err = norm(F-R, inf)
    let err = 0;
    for (let i = 0; i < M; i++) {
      const a = Math.hypot(Fr[2 * i] - Rr[2 * i], Fr[2 * i + 1] - Rr[2 * i + 1]);
      if (a > err) err = a;
    }
    errvec.push(err);

    if (err <= tol * Fnorm) {
      converged = true;
      break;
    }
  }

  // Convert flat arrays to [re, im] pair arrays for output and prz compatibility
  const zOut = new Array(m);
  const fOut = new Array(m);
  const wOut = new Array(m);
  for (let i = 0; i < m; i++) {
    zOut[i] = [zr[2 * i], zr[2 * i + 1]];
    fOut[i] = [fr[2 * i], fr[2 * i + 1]];
    wOut[i] = [wr[2 * i], wr[2 * i + 1]];
  }

  const t_prz = performance.now();
  const { pol, res, zer } = prz(zOut, fOut, wOut);
  timings.prz += performance.now() - t_prz;

  timings.total += performance.now() - t_total;

  return {
    converged,
    z: zOut, f: fOut, w: wOut,
    errvec,
    pol, res, zer,
    eval: (zv) => feval([zv], zOut, fOut, wOut)[0],
    delete: () => {},
  };
}

export { prz, cleanup, feval, barycentricRoots };
