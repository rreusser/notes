// AAA algorithm variant using QR-first SVD for tall matrices.
// Instead of direct bidiagonal reduction of the M×N Loewner matrix,
// first QR-factorize to reduce to N×N, then SVD the small R.
//
// This follows LAPACK's zgesvd Path 1 (M much larger than N, jobu='N'),
// which our zgesvd translation currently omits.
//
// Reference: LAPACK zgesvd.f lines 665-720

import { prz, cleanup, feval, barycentricRoots } from './prz.js';
import zgesvd from './lib/lapack/base/zgesvd/lib/ndarray.js';
import { createRequire } from 'module';
const require = createRequire(import.meta.url);
const Complex128Array = require( '@stdlib/array/complex128' );
const reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
const zgeqrf = require( './lib/lapack/base/zgeqrf/lib/base.js' );
const zlaset = require( './lib/lapack/base/zlaset/lib/base.js' );
const zlacpy = require( './lib/lapack/base/zlacpy/lib/base.js' );
const Complex128 = require( '@stdlib/complex/float64/ctor' );

const CZERO = new Complex128( 0.0, 0.0 );

// Timing accumulator
export const timings = {
  svd: 0, qr: 0, cauchy: 0, aMatrix: 0, ratEval: 0, prz: 0, total: 0, iters: 0,
  reset() { this.svd = 0; this.qr = 0; this.cauchy = 0; this.aMatrix = 0; this.ratEval = 0; this.prz = 0; this.total = 0; this.iters = 0; }
};

export function aaa(Z, F, tol = 1e-13, mmax = 100) {
  const t_total = performance.now();
  const M = Z.length;
  if (M !== F.length) throw new Error('Z and F must have the same length');
  if (mmax > M) mmax = M;

  const Zr = new Float64Array(2 * M);
  const Fr = new Float64Array(2 * M);
  for (let i = 0; i < M; i++) {
    Zr[2 * i] = Z[i][0]; Zr[2 * i + 1] = Z[i][1];
    Fr[2 * i] = F[i][0]; Fr[2 * i + 1] = F[i][1];
  }

  const zr = new Float64Array(2 * mmax);
  const fr = new Float64Array(2 * mmax);
  const wr = new Float64Array(2 * mmax);
  const errvec = [];
  const Cr = new Float64Array(2 * M * mmax);
  const J = new Int32Array(M);
  for (let i = 0; i < M; i++) J[i] = i;
  let nJ = M;

  const Rr = new Float64Array(2 * M);
  let meanR = 0, meanI = 0;
  for (let i = 0; i < M; i++) { meanR += Fr[2 * i]; meanI += Fr[2 * i + 1]; }
  meanR /= M; meanI /= M;
  for (let i = 0; i < M; i++) { Rr[2 * i] = meanR; Rr[2 * i + 1] = meanI; }

  let Fnorm = 0;
  for (let i = 0; i < M; i++) {
    const a = Math.hypot(Fr[2 * i], Fr[2 * i + 1]);
    if (a > Fnorm) Fnorm = a;
  }

  // Pre-allocate workspaces
  const maxN = mmax;
  const maxM_svd = M;

  // Full Loewner matrix (for QR factorization, overwritten in-place)
  const Adata = new Complex128Array(maxM_svd * maxN);
  const Av = reinterpret(Adata, 0);

  // Small R matrix for SVD (N×N)
  const Rmat = new Complex128Array(maxN * maxN);
  const Rv = reinterpret(Rmat, 0);

  // QR workspace
  const TAU = new Complex128Array(maxN);
  const qrWorkSize = Math.max(1, 64 * maxN); // generous workspace
  const qrWork = new Complex128Array(qrWorkSize);

  // SVD workspace (for the small N×N R matrix)
  const sData = new Float64Array(maxN);
  const UData = new Complex128Array(1);
  const VTData = new Complex128Array(maxN * maxN);
  const VTv = reinterpret(VTData, 0);
  const minMN_max = maxN;
  const svdWsz = Math.max(1, 3 * minMN_max + minMN_max + minMN_max * minMN_max);
  const svdWork = new Complex128Array(svdWsz);
  const svdRwork = new Float64Array(5 * maxN);

  let converged = false;
  let m = 0;

  for (let iter = 0; iter < mmax; iter++) {
    timings.iters++;

    let maxVal = -1, j = 0;
    for (let i = 0; i < M; i++) {
      const dr = Fr[2 * i] - Rr[2 * i], di = Fr[2 * i + 1] - Rr[2 * i + 1];
      const a = dr * dr + di * di;
      if (a > maxVal) { maxVal = a; j = i; }
    }

    zr[2 * m] = Zr[2 * j]; zr[2 * m + 1] = Zr[2 * j + 1];
    fr[2 * m] = Fr[2 * j]; fr[2 * m + 1] = Fr[2 * j + 1];
    m++;

    for (let i = 0; i < nJ; i++) {
      if (J[i] === j) {
        for (let k = i; k < nJ - 1; k++) J[k] = J[k + 1];
        nJ--;
        break;
      }
    }

    let t0 = performance.now();
    const zjr = Zr[2 * j], zji = Zr[2 * j + 1];
    for (let i = 0; i < M; i++) {
      const dr = Zr[2 * i] - zjr, di = Zr[2 * i + 1] - zji;
      const d = dr * dr + di * di;
      const base = 2 * (i + iter * M);
      if (d === 0) { Cr[base] = 0; Cr[base + 1] = 0; }
      else { Cr[base] = dr / d; Cr[base + 1] = -di / d; }
    }
    timings.cauchy += performance.now() - t0;

    // Build A matrix into Adata (column-major, nJ × m)
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

    // QR-first SVD: for tall matrices (nJ >> m), first QR factorize,
    // then SVD the small m×m upper triangular R.
    for (let i = 0; i < 2 * m * m; i++) VTv[i] = 0;

    t0 = performance.now();
    let info;

    if (nJ >= 2 * m && m > 1) {
      // Path 1: QR first, then small SVD
      // Step 1: QR factorize nJ×m matrix A → Q*R
      zgeqrf(nJ, m, Adata, 1, nJ, 0, TAU, 1, 0, qrWork, 1, 0);
      timings.qr += performance.now() - t0;

      // Step 2: Copy upper triangular R to Rmat (m×m)
      // Zero Rmat first
      for (let i = 0; i < 2 * m * m; i++) Rv[i] = 0;
      // Copy upper triangle
      for (let jj = 0; jj < m; jj++) {
        for (let ii = 0; ii <= jj; ii++) {
          const srcIdx = 2 * (ii + jj * nJ);
          const dstIdx = 2 * (ii + jj * m);
          Rv[dstIdx] = Av[srcIdx];
          Rv[dstIdx + 1] = Av[srcIdx + 1];
        }
      }

      // Step 3: SVD of small m×m R matrix
      t0 = performance.now();
      info = zgesvd(
        'N', 'A', m, m,
        Rmat, 1, m, 0,
        sData, 1, 0,
        UData, 1, 1, 0,
        VTData, 1, m, 0,
        svdWork, 1, 0, svdWsz,
        svdRwork, 1, 0
      );
    } else {
      // Small matrix or m=1: direct SVD
      info = zgesvd(
        'N', 'A', nJ, m,
        Adata, 1, nJ, 0,
        sData, 1, 0,
        UData, 1, 1, 0,
        VTData, 1, m, 0,
        svdWork, 1, 0, svdWsz,
        svdRwork, 1, 0
      );
    }
    timings.svd += performance.now() - t0;

    if (info !== 0) {
      console.warn('SVD info =', info);
    }

    // w = conj(last row of VT)
    const lastRow = m - 1;
    for (let jj = 0; jj < m; jj++) {
      const idx = 2 * (lastRow + jj * m);
      wr[2 * jj] = VTv[idx];
      wr[2 * jj + 1] = -VTv[idx + 1];
    }

    // R = F; R(J) = N(J)/D(J)
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
        const wfr = wkr * fkr - wki * fki, wfi = wkr * fki + wki * fkr;
        nr += cr * wfr - cim * wfi; ni += cr * wfi + cim * wfr;
        dr += cr * wkr - cim * wki; di += cr * wki + cim * wkr;
      }
      const d = dr * dr + di * di;
      Rr[2 * ci] = (nr * dr + ni * di) / d;
      Rr[2 * ci + 1] = (ni * dr - nr * di) / d;
    }
    timings.ratEval += performance.now() - t0;

    let err = 0;
    for (let i = 0; i < M; i++) {
      const a = Math.hypot(Fr[2 * i] - Rr[2 * i], Fr[2 * i + 1] - Rr[2 * i + 1]);
      if (a > err) err = a;
    }
    errvec.push(err);

    if (err <= tol * Fnorm) { converged = true; break; }
  }

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
    converged, z: zOut, f: fOut, w: wOut, errvec, pol, res, zer,
    eval: (zv) => feval([zv], zOut, fOut, wOut)[0],
    delete: () => {},
  };
}

export { prz, cleanup, feval, barycentricRoots };
