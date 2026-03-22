#!/usr/bin/env node
// Diagnostic: test zgesvd on matrices with known singular values.
// If zgesvd is broken, this will show exactly where.

import { createRequire } from 'module';
const require = createRequire(import.meta.url);
const Complex128Array = require('@stdlib/array/complex128');
const reinterpret = require('@stdlib/strided/base/reinterpret-complex128');
const zgesvd = require('../../lib/lapack/base/zgesvd/lib/ndarray.js');

function runSVD(label, M, N, Aflat, expectedS) {
  const A = new Complex128Array(Aflat);
  const minMN = Math.min(M, N);
  const s = new Float64Array(minMN);
  const U = new Complex128Array(M * M);
  const VT = new Complex128Array(N * N);
  const lwork = Math.max(1, 8 * (M + N));
  const WORK = new Complex128Array(lwork);
  const RWORK = new Float64Array(5 * minMN);

  const info = zgesvd(
    'A', 'A', M, N,
    A, 1, M, 0,
    s, 1, 0,
    U, 1, M, 0,
    VT, 1, N, 0,
    WORK, 1, 0, lwork,
    RWORK, 1, 0
  );

  const sArr = Array.from(s);
  let maxErr = 0;
  for (let i = 0; i < minMN; i++) {
    maxErr = Math.max(maxErr, Math.abs(s[i] - expectedS[i]));
  }

  // Verify U * diag(s) * VT reconstructs A (using original Aflat)
  const Uv = reinterpret(U, 0);
  const VTv = reinterpret(VT, 0);
  const Aorig = new Float64Array(Aflat);
  let reconErr = 0;
  for (let i = 0; i < M; i++) {
    for (let j = 0; j < N; j++) {
      let rr = 0, ri = 0;
      for (let k = 0; k < minMN; k++) {
        // U[i,k] * s[k] * VT[k,j]
        const uIdx = 2 * (i + k * M);
        const vtIdx = 2 * (k + j * N);
        const ur = Uv[uIdx], ui = Uv[uIdx + 1];
        const vr = VTv[vtIdx], vi = VTv[vtIdx + 1];
        // (ur + ui*i) * s[k] * (vr + vi*i)
        const sur = ur * s[k], sui = ui * s[k];
        rr += sur * vr - sui * vi;
        ri += sur * vi + sui * vr;
      }
      const origIdx = 2 * (i + j * M);
      const dr = rr - Aorig[origIdx];
      const di = ri - Aorig[origIdx + 1];
      reconErr = Math.max(reconErr, Math.hypot(dr, di));
    }
  }

  const pass = maxErr < 1e-10 && reconErr < 1e-10;
  console.log(`${pass ? 'PASS' : 'FAIL'}  ${label}`);
  console.log(`  info=${info}  s=[${sArr.map(v => v.toFixed(6)).join(', ')}]`);
  if (expectedS) {
    console.log(`  expected=[${expectedS.map(v => v.toFixed(6)).join(', ')}]`);
    console.log(`  max singular value error: ${maxErr.toExponential(2)}`);
  }
  console.log(`  max reconstruction error (U*S*VT vs A): ${reconErr.toExponential(2)}`);
  console.log('');

  return { info, s: sArr, maxErr, reconErr, pass };
}

console.log('=== zgesvd Diagnostic ===\n');

// Test 1: 2x2 diagonal (trivial)
runSVD('2x2 real diagonal', 2, 2,
  [2,0, 0,0, 0,0, 3,0],
  [3, 2]);

// Test 2: 2x2 with off-diagonal
// A = [[1, 1], [0, 1]] → singular values = (1+√5)/2 and (√5-1)/2
// Actually easier: A = [[3,0],[4,0]] as 1x2 → s = [5]
// Let's use A = [[0,0,1,0],[1,0,0,0]] (2x2 permutation, s=[1,1])
runSVD('2x2 permutation', 2, 2,
  [0,0, 1,0, 1,0, 0,0],
  [1, 1]);

// Test 3: 2x2 complex Hermitian
// A = [[2, 1+i], [1-i, 3]] → eigenvalues are known
// Singular values of Hermitian = |eigenvalues|
// eigenvalues: (5 ± √(1+1+1))/2 = (5 ± √3)/2 → nope, let me just compute
// A = [[2,0, 1,1], [1,-1, 3,0]] col-major
runSVD('2x2 complex Hermitian', 2, 2,
  [2,0, 1,-1, 1,1, 3,0],
  [3 + Math.sqrt(2), 3 - Math.sqrt(2)].sort((a, b) => b - a));

// Test 4: 3x3 real diagonal
runSVD('3x3 real diagonal', 3, 3,
  [5,0, 0,0, 0,0, 0,0, 3,0, 0,0, 0,0, 0,0, 1,0],
  [5, 3, 1]);

// Test 5: Tall matrix (4x2)
// A = [[1,0],[0,1],[1,0],[0,1]] → s = [√2, √2]
runSVD('4x2 repeated identity rows', 4, 2,
  [1,0, 0,0, 1,0, 0,0, 0,0, 1,0, 0,0, 1,0],
  [Math.sqrt(2), Math.sqrt(2)]);

// Test 6: Wide matrix (2x4)
runSVD('2x4 repeated identity cols', 2, 4,
  [1,0, 0,0, 0,0, 1,0, 1,0, 0,0, 0,0, 1,0],
  [Math.sqrt(2), Math.sqrt(2)]);

// Test 7: Purely imaginary
// A = [[i, 0], [0, 2i]] → s = [2, 1]
runSVD('2x2 purely imaginary diagonal', 2, 2,
  [0,1, 0,0, 0,0, 0,2],
  [2, 1]);

// Test 8: Larger random-ish matrix (10x10) with known structure
// A = diag(10, 9, 8, ..., 1) rotated by a unitary — but simpler:
// just use a diagonal
const n = 10;
const diag10 = new Float64Array(2 * n * n);
for (let i = 0; i < n; i++) diag10[2 * (i + i * n)] = n - i;
runSVD('10x10 real diagonal', n, n,
  Array.from(diag10),
  Array.from({length: n}, (_, i) => n - i));

// Test 9: 20x5 tall matrix (closer to AAA usage pattern)
// Column-major, rank-1: A = u * v^H where u is 20x1, v is 5x1
const M9 = 20, N9 = 5;
const A9 = new Float64Array(2 * M9 * N9);
for (let j = 0; j < N9; j++) {
  for (let i = 0; i < M9; i++) {
    const idx = 2 * (i + j * M9);
    // A[i,j] = (i+1) * (j+1) + i*(j+1)*0.1i  (rank-1-ish, complex)
    A9[idx] = (i + 1) * (j + 1);
    A9[idx + 1] = 0;
  }
}
// This is rank-1 with singular value = ||u|| * ||v||
const unorm = Math.sqrt(M9 * (M9 + 1) * (2 * M9 + 1) / 6);
const vnorm = Math.sqrt(N9 * (N9 + 1) * (2 * N9 + 1) / 6);
const expected9 = new Array(N9).fill(0);
expected9[0] = unorm * vnorm;
runSVD('20x5 rank-1 (real)', M9, N9, Array.from(A9), expected9);

// Test 10: Match AAA iteration pattern exactly — M>>N, N small
// This is the shape zgesvd sees in AAA: ~800 rows, ~20 cols
const M10 = 100, N10 = 10;
const A10 = new Float64Array(2 * M10 * N10);
const expected10 = [];
for (let k = 0; k < N10; k++) {
  const sv = N10 - k; // singular values 10, 9, ..., 1
  expected10.push(sv);
  for (let i = 0; i < M10; i++) {
    // Build A = U * diag(s) * VT with simple orthogonal U, VT = I
    // Column k of A = sv * e_k (padded with zeros)
    if (i === k) {
      A10[2 * (i + k * M10)] = sv;
    }
  }
}
runSVD(`${M10}x${N10} diagonal (AAA-shaped)`, M10, N10, Array.from(A10), expected10);

// Summary
console.log('=== Done ===');
