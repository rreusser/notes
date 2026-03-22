// Pure JavaScript AAA algorithm — no WASM dependency.
// Uses blahpack's zgesvd for the SVD step.
// Reference: Nakatsukasa, Sète, Trefethen, "The AAA Algorithm for Rational
// Approximation", SIAM J. Sci. Comput. 40(3), A1494–A1522, 2018.
// MATLAB code: arXiv:1612.00337v2, aaa_alg.m

import { prz, cleanup, feval, barycentricRoots } from './prz.js';
import zgesvd from './lib/lapack/base/zgesvd/lib/ndarray.js';

export function aaa(Z, F, tol = 1e-13, mmax = 100) {
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
  // A is at most M x mmax, but nJ decreases and m increases. Max product bounded by M*mmax.
  const maxN = mmax;
  const maxM_svd = M;
  const AdataMax = new Float64Array(2 * maxM_svd * maxN);
  const sDataMax = new Float64Array(maxN);
  const UData = new Float64Array(2);
  const VTDataMax = new Float64Array(2 * maxN * maxN);
  const lworkMax = Math.max(1, 8 * (maxM_svd + maxN));
  const WORKMax = new Float64Array(2 * lworkMax);
  const RWORKMax = new Float64Array(5 * maxN);

  let converged = false;
  let m = 0;

  for (let iter = 0; iter < mmax; iter++) {
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
        // Shift remaining elements left
        for (let k = i; k < nJ - 1; k++) J[k] = J[k + 1];
        nJ--;
        break;
      }
    }

    // C(:, iter) = 1 / (Z - Z(j))
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

    // A = SF*C - C*Sf where SF = diag(F), Sf = diag(f)
    // i.e. A(ii, k) = C(J[ii], k) * (F(J[ii]) - f[k])
    // Pack into column-major interleaved Float64Array for zgesvd
    for (let k = 0; k < m; k++) {
      const fkr = fr[2 * k], fki = fr[2 * k + 1];
      for (let ii = 0; ii < nJ; ii++) {
        const ci = J[ii];
        const cBase = 2 * (ci + k * M);
        const cr = Cr[cBase], cim = Cr[cBase + 1];
        const dr = Fr[2 * ci] - fkr, di = Fr[2 * ci + 1] - fki;
        const idx = 2 * (ii + k * nJ);
        AdataMax[idx] = cr * dr - cim * di;
        AdataMax[idx + 1] = cr * di + cim * dr;
      }
    }

    // SVD(A(J,:), 0) — thin SVD, we only need V(:, m)
    // LAPACK returns VT = V^H; we need last row of VT, conjugated.
    const minMN = Math.min(nJ, m);
    const lwork = Math.max(1, 8 * (nJ + m));

    // Zero out VT region we'll read from
    for (let i = 0; i < 2 * m * m; i++) VTDataMax[i] = 0;

    const info = zgesvd(
      'N', 'A', nJ, m,
      AdataMax, 1, nJ, 0,
      sDataMax, 1, 0,
      UData, 1, 1, 0,
      VTDataMax, 1, m, 0,
      WORKMax, 1, 0, lwork,
      RWORKMax, 1, 0
    );

    if (info !== 0) {
      console.warn('zgesvd info =', info);
    }

    // w = V(:, m) = conj(VT(m-1, :))
    // VT is m x m column-major. VT(i, j) at 2*(i + j*m).
    const lastRow = m - 1;
    for (let jj = 0; jj < m; jj++) {
      const idx = 2 * (lastRow + jj * m);
      wr[2 * jj] = VTDataMax[idx];
      wr[2 * jj + 1] = -VTDataMax[idx + 1]; // conjugate
    }

    // R = F; R(J) = N(J)./D(J) where N = C*(w.*f), D = C*w
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

  const { pol, res, zer } = prz(zOut, fOut, wOut);

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
