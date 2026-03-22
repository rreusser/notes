#!/usr/bin/env node
// Diagnostic 2: test the exact zgesvd code path AAA uses (jobu='N', jobvt='A')
// and test with Cauchy-like matrices.

import { createRequire } from 'module';
const require = createRequire(import.meta.url);
const Complex128Array = require('@stdlib/array/complex128');
const reinterpret = require('@stdlib/strided/base/reinterpret-complex128');
const zgesvd = require('../../lib/lapack/base/zgesvd/lib/ndarray.js');

function svd(M, N, Aflat, jobu, jobvt) {
  const A = new Complex128Array(Aflat);
  const minMN = Math.min(M, N);
  const Usize = jobu === 'N' ? 1 : M * (jobu === 'S' ? minMN : M);
  const VTsize = jobvt === 'N' ? 1 : (jobvt === 'S' ? minMN : N) * N;
  const s = new Float64Array(minMN);
  const U = new Complex128Array(Usize);
  const VT = new Complex128Array(VTsize);
  const lwork = Math.max(1, 8 * (M + N));
  const WORK = new Complex128Array(lwork);
  const RWORK = new Float64Array(5 * minMN);

  const ldu = jobu === 'N' ? 1 : M;
  const ldvt = jobvt === 'N' ? 1 : (jobvt === 'S' ? minMN : N);

  const info = zgesvd(
    jobu, jobvt, M, N,
    A, 1, M, 0,
    s, 1, 0,
    U, 1, ldu, 0,
    VT, 1, ldvt, 0,
    WORK, 1, 0, lwork,
    RWORK, 1, 0
  );

  return { info, s: Array.from(s), U, VT, Uv: reinterpret(U, 0), VTv: reinterpret(VT, 0), M, N, minMN, ldvt };
}

// Compare singular values from two different jobu settings
function comparePaths(label, M, N, Aflat) {
  const full = svd(M, N, [...Aflat], 'A', 'A');
  const noU  = svd(M, N, [...Aflat], 'N', 'A');

  let sErr = 0;
  for (let i = 0; i < full.minMN; i++) {
    sErr = Math.max(sErr, Math.abs(full.s[i] - noU.s[i]));
  }

  // Compare VT matrices
  let vtErr = 0;
  const ldvt = N;
  for (let i = 0; i < N; i++) {
    for (let j = 0; j < N; j++) {
      const idx = 2 * (i + j * ldvt);
      const dr = full.VTv[idx] - noU.VTv[idx];
      const di = full.VTv[idx + 1] - noU.VTv[idx + 1];
      vtErr = Math.max(vtErr, Math.hypot(dr, di));
    }
  }

  // Check that VT is unitary: VT * VT^H = I
  let unitErr = 0;
  for (let i = 0; i < N; i++) {
    for (let j = 0; j < N; j++) {
      let rr = 0, ri = 0;
      for (let k = 0; k < N; k++) {
        // VT[i,k] * conj(VT[j,k])
        const a = 2 * (i + k * ldvt);
        const b = 2 * (j + k * ldvt);
        rr += noU.VTv[a] * noU.VTv[b] + noU.VTv[a+1] * noU.VTv[b+1];
        ri += noU.VTv[a+1] * noU.VTv[b] - noU.VTv[a] * noU.VTv[b+1];
      }
      const expected_r = (i === j) ? 1 : 0;
      unitErr = Math.max(unitErr, Math.hypot(rr - expected_r, ri));
    }
  }

  const pass = sErr < 1e-10 && unitErr < 1e-10;
  console.log(`${pass ? 'PASS' : 'FAIL'}  ${label}`);
  console.log(`  s(full) = [${full.s.map(v=>v.toExponential(4)).join(', ')}]`);
  console.log(`  s(N,A)  = [${noU.s.map(v=>v.toExponential(4)).join(', ')}]`);
  console.log(`  singular value diff: ${sErr.toExponential(2)}`);
  console.log(`  VT diff (full vs N,A): ${vtErr.toExponential(2)}`);
  console.log(`  VT unitarity error: ${unitErr.toExponential(2)}`);
  console.log('');
  return pass;
}

console.log('=== zgesvd jobu=N vs jobu=A comparison ===\n');

// Test 1: Simple
comparePaths('3x3 real diagonal', 3, 3,
  [5,0, 0,0, 0,0, 0,0, 3,0, 0,0, 0,0, 0,0, 1,0]);

// Test 2: Complex
comparePaths('3x3 complex', 3, 3,
  [1,1, 0,2, 3,0, 0,-1, 2,1, 1,0, 1,0, 0,0, 3,1]);

// Test 3: Tall (like AAA)
const M3 = 50, N3 = 5;
const A3 = new Float64Array(2 * M3 * N3);
for (let j = 0; j < N3; j++) {
  for (let i = 0; i < M3; i++) {
    A3[2 * (i + j * M3)] = Math.cos(i * (j + 1) * 0.1);
    A3[2 * (i + j * M3) + 1] = Math.sin(i * (j + 1) * 0.1);
  }
}
comparePaths('50x5 trig', M3, N3, Array.from(A3));

// Test 4: Cauchy-like matrix (this is what AAA generates)
const M4 = 100, N4 = 10;
const A4 = new Float64Array(2 * M4 * N4);
const Z = [];
for (let i = 0; i < M4; i++) {
  const theta = 2 * Math.PI * i / M4;
  Z.push([Math.cos(theta), Math.sin(theta)]);
}
const zj = [];
for (let k = 0; k < N4; k++) {
  const theta = 2 * Math.PI * (k + 0.5) / N4;
  zj.push([0.5 * Math.cos(theta), 0.5 * Math.sin(theta)]);
}
// A[i,k] = 1/(Z[i] - zj[k]) * (F[i] - f[k])
// with F[i] = 1/(Z[i] - 0.3), f[k] = 1/(zj[k] - 0.3)
for (let k = 0; k < N4; k++) {
  const fkr = zj[k][0] - 0.3, fki = zj[k][1];
  const fkd = fkr*fkr + fki*fki;
  const fk = [fkr/fkd, -fki/fkd];
  for (let i = 0; i < M4; i++) {
    const dr = Z[i][0] - zj[k][0], di = Z[i][1] - zj[k][1];
    const d = dr*dr + di*di;
    const cr = dr/d, ci = -di/d;  // 1/(Z[i]-zj[k])
    const Fir = Z[i][0] - 0.3, Fii = Z[i][1];
    const Fid = Fir*Fir + Fii*Fii;
    const Fi = [Fir/Fid, -Fii/Fid];
    const diffr = Fi[0] - fk[0], diffi = Fi[1] - fk[1];
    const idx = 2 * (i + k * M4);
    A4[idx] = cr*diffr - ci*diffi;
    A4[idx+1] = cr*diffi + ci*diffr;
  }
}
comparePaths('100x10 Cauchy (AAA-like)', M4, N4, Array.from(A4));

// Test 5: Extract weight from last row of VT and verify it's the right null vector
console.log('=== Last-row-of-VT extraction (weight vector) ===\n');
{
  const { s, VTv, minMN } = svd(M4, N4, Array.from(A4), 'N', 'A');
  const m = N4;
  const lastRow = m - 1;
  const w = [];
  for (let jj = 0; jj < m; jj++) {
    const idx = 2 * (lastRow + jj * m);
    // conj of last row of VT
    w.push([VTv[idx], -VTv[idx+1]]);
  }

  // Verify: A * w should be small (w is right singular vector for smallest sv)
  const Aw = new Float64Array(2 * M4);
  for (let i = 0; i < M4; i++) {
    let rr = 0, ri = 0;
    for (let k = 0; k < N4; k++) {
      const idx = 2 * (i + k * M4);
      const ar = A4[idx], ai = A4[idx+1];
      rr += ar * w[k][0] - ai * w[k][1];
      ri += ar * w[k][1] + ai * w[k][0];
    }
    Aw[2*i] = rr; Aw[2*i+1] = ri;
  }
  let awNorm = 0;
  for (let i = 0; i < M4; i++) awNorm = Math.max(awNorm, Math.hypot(Aw[2*i], Aw[2*i+1]));
  console.log(`  smallest sv: ${s[minMN-1].toExponential(4)}`);
  console.log(`  ||A * w||_inf: ${awNorm.toExponential(4)}`);
  console.log(`  ratio ||A*w|| / s_min: ${(awNorm / Math.max(s[minMN-1], 1e-300)).toExponential(4)}`);
  console.log(`  (should be ~1 if w is correct right singular vector)`);
}

console.log('\n=== Done ===');
