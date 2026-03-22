#!/usr/bin/env node
// Diagnostic: instrument the AAA inner loop to check SVD quality
// on the actual Loewner matrices it generates.

import { createRequire } from 'module';
const require = createRequire(import.meta.url);
const Complex128Array = require('@stdlib/array/complex128');
const reinterpret = require('@stdlib/strided/base/reinterpret-complex128');
const zgesvd = require('../../lib/lapack/base/zgesvd/lib/ndarray.js');

function linspace(a, b, step) {
  const arr = [];
  if (a <= b) for (let x = a; x <= b + step/2; x += step) arr.push(x);
  else for (let x = a; x >= b - step/2; x -= step) arr.push(x);
  return arr;
}

// Simple test: exp(z) on unit circle (known to converge fast in MATLAB AAA)
const M = 200;
const Z = [], F = [];
for (let i = 0; i < M; i++) {
  const theta = 2 * Math.PI * i / M;
  Z.push([Math.cos(theta), Math.sin(theta)]);
  F.push([Math.exp(Math.cos(theta)) * Math.cos(Math.sin(theta)),
          Math.exp(Math.cos(theta)) * Math.sin(Math.sin(theta))]);
}

// Re-implement the AAA core with SVD diagnostics
const Zr = new Float64Array(2 * M);
const Fr = new Float64Array(2 * M);
for (let i = 0; i < M; i++) {
  Zr[2*i] = Z[i][0]; Zr[2*i+1] = Z[i][1];
  Fr[2*i] = F[i][0]; Fr[2*i+1] = F[i][1];
}

const mmax = 30;
const zr = new Float64Array(2 * mmax);
const fr = new Float64Array(2 * mmax);
const wr = new Float64Array(2 * mmax);
const Cr = new Float64Array(2 * M * mmax);
const J = new Int32Array(M);
for (let i = 0; i < M; i++) J[i] = i;
let nJ = M;
const Rr = new Float64Array(2 * M);
let meanR = 0, meanI = 0;
for (let i = 0; i < M; i++) { meanR += Fr[2*i]; meanI += Fr[2*i+1]; }
meanR /= M; meanI /= M;
for (let i = 0; i < M; i++) { Rr[2*i] = meanR; Rr[2*i+1] = meanI; }
let Fnorm = 0;
for (let i = 0; i < M; i++) {
  const a = Math.hypot(Fr[2*i], Fr[2*i+1]);
  if (a > Fnorm) Fnorm = a;
}

let m = 0;
console.log('=== AAA iteration diagnostics ===\n');
console.log('iter  nJ    m   s_min        s_max        ||Aw||       err');
console.log('----  ---   --  -----------  -----------  -----------  -----------');

for (let iter = 0; iter < mmax; iter++) {
  let maxVal = -1, j = 0;
  for (let i = 0; i < M; i++) {
    const dr = Fr[2*i] - Rr[2*i], di = Fr[2*i+1] - Rr[2*i+1];
    const a = dr*dr + di*di;
    if (a > maxVal) { maxVal = a; j = i; }
  }

  zr[2*m] = Zr[2*j]; zr[2*m+1] = Zr[2*j+1];
  fr[2*m] = Fr[2*j]; fr[2*m+1] = Fr[2*j+1];
  m++;

  for (let i = 0; i < nJ; i++) {
    if (J[i] === j) {
      for (let k = i; k < nJ - 1; k++) J[k] = J[k+1];
      nJ--;
      break;
    }
  }

  const zjr = Zr[2*j], zji = Zr[2*j+1];
  for (let i = 0; i < M; i++) {
    const dr = Zr[2*i] - zjr, di = Zr[2*i+1] - zji;
    const d = dr*dr + di*di;
    const base = 2 * (i + iter * M);
    if (d === 0) { Cr[base] = 0; Cr[base+1] = 0; }
    else { Cr[base] = dr/d; Cr[base+1] = -di/d; }
  }

  // Build A matrix
  const Adata = new Complex128Array(nJ * m);
  const Av = reinterpret(Adata, 0);
  for (let k = 0; k < m; k++) {
    const fkr = fr[2*k], fki = fr[2*k+1];
    for (let ii = 0; ii < nJ; ii++) {
      const ci = J[ii];
      const cBase = 2 * (ci + k * M);
      const cr = Cr[cBase], cim = Cr[cBase+1];
      const dr = Fr[2*ci] - fkr, di = Fr[2*ci+1] - fki;
      const idx = 2 * (ii + k * nJ);
      Av[idx] = cr*dr - cim*di;
      Av[idx+1] = cr*di + cim*dr;
    }
  }

  // SVD
  const sData = new Float64Array(m);
  const UData = new Complex128Array(1);
  const VTData = new Complex128Array(m * m);
  const VTv = reinterpret(VTData, 0);
  const lwork = Math.max(1, 8 * (nJ + m));
  const WORK = new Complex128Array(lwork);
  const RWORK = new Float64Array(5 * m);

  // Also compute with full U for reconstruction check
  const Adata2 = new Complex128Array(nJ * m);
  const Av2 = reinterpret(Adata2, 0);
  for (let i = 0; i < 2 * nJ * m; i++) Av2[i] = Av[i];

  const UDataFull = new Complex128Array(nJ * nJ);
  const VTData2 = new Complex128Array(m * m);
  const VTv2 = reinterpret(VTData2, 0);
  const sData2 = new Float64Array(m);
  const WORK2 = new Complex128Array(lwork);
  const RWORK2 = new Float64Array(5 * m);

  const info = zgesvd(
    'N', 'A', nJ, m,
    Adata, 1, nJ, 0,
    sData, 1, 0,
    UData, 1, 1, 0,
    VTData, 1, m, 0,
    WORK, 1, 0, lwork,
    RWORK, 1, 0
  );

  const info2 = zgesvd(
    'A', 'A', nJ, m,
    Adata2, 1, nJ, 0,
    sData2, 1, 0,
    UDataFull, 1, nJ, 0,
    VTData2, 1, m, 0,
    WORK2, 1, 0, lwork,
    RWORK2, 1, 0
  );

  // Extract w = conj(last row of VT)
  const lastRow = m - 1;
  for (let jj = 0; jj < m; jj++) {
    const idx = 2 * (lastRow + jj * m);
    wr[2*jj] = VTv[idx];
    wr[2*jj+1] = -VTv[idx+1];
  }

  // Compute ||A * w|| using original A data
  const Aorig = new Float64Array(2 * nJ * m);
  for (let k = 0; k < m; k++) {
    const fkr = fr[2*k], fki = fr[2*k+1];
    for (let ii = 0; ii < nJ; ii++) {
      const ci = J[ii];
      const cBase = 2 * (ci + k * M);
      const cr = Cr[cBase], cim = Cr[cBase+1];
      const dr = Fr[2*ci] - fkr, di = Fr[2*ci+1] - fki;
      const idx = 2 * (ii + k * nJ);
      Aorig[idx] = cr*dr - cim*di;
      Aorig[idx+1] = cr*di + cim*dr;
    }
  }

  let awNorm = 0;
  for (let ii = 0; ii < nJ; ii++) {
    let rr = 0, ri = 0;
    for (let k = 0; k < m; k++) {
      const idx = 2 * (ii + k * nJ);
      rr += Aorig[idx] * wr[2*k] - Aorig[idx+1] * wr[2*k+1];
      ri += Aorig[idx] * wr[2*k+1] + Aorig[idx+1] * wr[2*k];
    }
    awNorm = Math.max(awNorm, Math.hypot(rr, ri));
  }

  // Check singular value agreement between N,A and A,A
  let svDiff = 0;
  for (let i = 0; i < m; i++) svDiff = Math.max(svDiff, Math.abs(sData[i] - sData2[i]));

  // Compute approximation error
  for (let i = 0; i < 2 * M; i++) Rr[i] = Fr[i];
  for (let ii = 0; ii < nJ; ii++) {
    const ci = J[ii];
    let nr = 0, ni = 0, dr = 0, di = 0;
    for (let k = 0; k < m; k++) {
      const cBase = 2 * (ci + k * M);
      const cr = Cr[cBase], cim = Cr[cBase+1];
      const wkr = wr[2*k], wki = wr[2*k+1];
      const fkr = fr[2*k], fki = fr[2*k+1];
      const wfr = wkr*fkr - wki*fki, wfi = wkr*fki + wki*fkr;
      nr += cr*wfr - cim*wfi; ni += cr*wfi + cim*wfr;
      dr += cr*wkr - cim*wki; di += cr*wki + cim*wkr;
    }
    const d = dr*dr + di*di;
    Rr[2*ci] = (nr*dr + ni*di)/d;
    Rr[2*ci+1] = (ni*dr - nr*di)/d;
  }
  let err = 0;
  for (let i = 0; i < M; i++) {
    const a = Math.hypot(Fr[2*i] - Rr[2*i], Fr[2*i+1] - Rr[2*i+1]);
    if (a > err) err = a;
  }

  console.log(
    `${(iter+1).toString().padStart(4)}  ${nJ.toString().padStart(3)}   ${m.toString().padStart(2)}  ${sData[m-1].toExponential(4).padStart(11)}  ${sData[0].toExponential(4).padStart(11)}  ${awNorm.toExponential(4).padStart(11)}  ${err.toExponential(4).padStart(11)}` +
    (svDiff > 1e-12 ? `  SV MISMATCH: ${svDiff.toExponential(2)}` : '') +
    (info !== 0 ? `  INFO=${info}` : '')
  );

  if (err <= 1e-13 * Fnorm) {
    console.log('\nConverged!');
    break;
  }
}
