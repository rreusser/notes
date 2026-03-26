// Web worker for pseudospectra computation.
// Computes eigenvalues via dgeev and sigma_min(zI - A) on a grid via dgesvd.

import { dgeev, dgesvd } from './pspectra-bundle.js';

self.onmessage = function (e) {
  const { type, id } = e.data;

  if (type === 'eigenvalues') {
    const { matrix, N } = e.data;
    const A = new Float64Array(matrix);
    const WR = new Float64Array(N);
    const WI = new Float64Array(N);
    const VL = new Float64Array(N * N);
    const VR = new Float64Array(N * N);
    const info = dgeev(
      'compute-vectors', 'compute-vectors', N,
      A, 1, N, 0,
      WR, 1, 0,
      WI, 1, 0,
      VL, 1, N, 0,
      VR, 1, N, 0
    );

    // Compute eigenvalue condition numbers: kappa_k = 1 / |y_k^H x_k|
    // where y_k is the left eigenvector (column of VL) and x_k is right (column of VR).
    // For complex eigenvalue pairs (WI[k] != 0), columns k and k+1 of VR/VL
    // store the real and imaginary parts respectively.
    const condNums = new Float64Array(N);
    let k = 0;
    while (k < N) {
      if (WI[k] === 0) {
        // Real eigenvalue: y^T x (both real vectors, column k)
        let dot = 0;
        for (let i = 0; i < N; i++) dot += VL[i + k * N] * VR[i + k * N];
        condNums[k] = 1 / Math.max(Math.abs(dot), 1e-16);
        k++;
      } else {
        // Complex pair: columns k, k+1 store real/imag parts
        // y^H x = (yr - i*yi)^T (xr + i*xi) = (yr^T xr + yi^T xi) + i(yr^T xi - yi^T xr)
        let dotRR = 0, dotII = 0, dotRI = 0, dotIR = 0;
        for (let i = 0; i < N; i++) {
          const lr = VL[i + k * N], li = VL[i + (k + 1) * N];
          const rr = VR[i + k * N], ri = VR[i + (k + 1) * N];
          dotRR += lr * rr;
          dotII += li * ri;
          dotRI += lr * ri;
          dotIR += li * rr;
        }
        const re = dotRR + dotII;
        const im = dotRI - dotIR;
        const mag = Math.hypot(re, im);
        condNums[k] = 1 / Math.max(mag, 1e-16);
        condNums[k + 1] = condNums[k]; // conjugate pair has same condition number
        k += 2;
      }
    }

    self.postMessage({ id, type: 'eigenvalues', WR, WI, VR, condNums, info });
  } else if (type === 'pseudospectra') {
    const { matrix, N, xMin, xMax, yMin, yMax, gridN } = e.data;

    // Pre-allocate SVD workspace (reused for every grid point)
    const B = new Float64Array(N * N);
    const s = new Float64Array(N);
    const U = new Float64Array(1);
    const VT = new Float64Array(1);

    const grid = new Float32Array(gridN * gridN);

    // Process in batches of rows for progressive rendering
    const batchSize = Math.max(1, Math.floor(gridN / 20));

    for (let row = 0; row < gridN; row++) {
      const y = yMin + (row + 0.5) / gridN * (yMax - yMin);
      for (let col = 0; col < gridN; col++) {
        const x = xMin + (col + 0.5) / gridN * (xMax - xMin);

        // Form B = zI - A (z = x + iy)
        for (let i = 0; i < N * N; i++) B[i] = -matrix[i];
        for (let i = 0; i < N; i++) {
          B[i + i * N] += x; // real part of z on diagonal
        }

        // For complex z, we need to handle the imaginary part.
        // sigma_min(zI - A) where z = x + iy and A is real.
        // We use the identity: sigma_min(zI - A) = sigma_min of the real 2N x 2N block:
        //   [ xI - A,  -yI ]
        //   [  yI,   xI - A ]
        // But for simplicity with real dgesvd, use:
        // sigma_min((x+iy)I - A)^2 = min eigenvalue of ((xI-A)^2 + y^2 I)
        // Actually, the correct approach: the singular values of the complex matrix (zI - A)
        // equal the singular values of the 2N x 2N real matrix:
        //   [ xI - A,  -yI ]
        //   [  yI,   xI - A ]

        if (y === 0) {
          // Real z: just use dgesvd on B = xI - A
          const info = dgesvd(
            'none', 'none', N, N,
            B, 1, N, 0,
            s, 1, 0,
            U, 1, 1, 0,
            VT, 1, 1, 0
          );
          grid[row * gridN + col] = Math.log10(Math.max(s[N - 1], 1e-18));
        } else {
          // Complex z: form the 2N x 2N real representation
          // This is allocated per-point (small N, fast)
          const NN = 2 * N;
          const B2 = new Float64Array(NN * NN);
          const s2 = new Float64Array(NN);
          const U2 = new Float64Array(1);
          const VT2 = new Float64Array(1);

          // [ xI-A,  -yI ]
          // [  yI,  xI-A ]
          for (let i = 0; i < N; i++) {
            for (let j = 0; j < N; j++) {
              const v = -matrix[i + j * N] + (i === j ? x : 0);
              B2[i + j * NN] = v;           // top-left
              B2[(i + N) + (j + N) * NN] = v; // bottom-right
            }
            B2[i + (i + N) * NN] = -y;      // top-right: -yI
            B2[(i + N) + i * NN] = y;        // bottom-left: yI
          }

          const info = dgesvd(
            'none', 'none', NN, NN,
            B2, 1, NN, 0,
            s2, 1, 0,
            U2, 1, 1, 0,
            VT2, 1, 1, 0
          );
          // Singular values come in pairs; the smallest is what we want
          grid[row * gridN + col] = Math.log10(Math.max(s2[NN - 1], 1e-18));
        }
      }

      // Send progress in batches
      if ((row + 1) % batchSize === 0 || row === gridN - 1) {
        self.postMessage({
          id, type: 'progress',
          completedRows: row + 1,
          totalRows: gridN,
        });
      }
    }

    // Send the complete grid
    self.postMessage({
      id, type: 'pseudospectra',
      grid, gridN, xMin, xMax, yMin, yMax,
    }, [grid.buffer]);
  } else if (type === 'transient') {
    const { matrix, N, tMax, nSteps } = e.data;

    const times = new Float64Array(nSteps);
    const norms = new Float64Array(nSteps);
    const s = new Float64Array(N);
    const U = new Float64Array(1);
    const VT = new Float64Array(1);

    for (let step = 0; step < nSteps; step++) {
      const t = step / (nSteps - 1) * tMax;
      times[step] = t;

      // Compute e^{tA} via scaling and squaring with Taylor series.
      // 1. Choose scaling factor so ||tA / 2^k|| < 1
      // 2. Compute exp(tA / 2^k) via Taylor series (13 terms)
      // 3. Square k times

      // Estimate ||tA||_1 (column-sum norm)
      let normA = 0;
      for (let j = 0; j < N; j++) {
        let colSum = 0;
        for (let i = 0; i < N; i++) colSum += Math.abs(t * matrix[i + j * N]);
        if (colSum > normA) normA = colSum;
      }

      // Choose k so that normA / 2^k < 0.5
      let k = 0;
      while (normA > 0.5) { normA *= 0.5; k++; }
      const scale = t / (1 << k);

      // Scaled matrix B = scale * A
      const B = new Float64Array(N * N);
      for (let i = 0; i < N * N; i++) B[i] = scale * matrix[i];

      // Taylor series: exp(B) ≈ I + B + B^2/2! + ... + B^q/q!
      // Compute as: result = I, term = I, then term = term * B / j, result += term
      const result = new Float64Array(N * N);
      const term = new Float64Array(N * N);
      const tmp = new Float64Array(N * N);
      for (let i = 0; i < N; i++) { result[i + i * N] = 1; term[i + i * N] = 1; }

      const q = 13;
      for (let j = 1; j <= q; j++) {
        // tmp = term * B
        matmul(tmp, term, B, N);
        // term = tmp / j
        for (let i = 0; i < N * N; i++) term[i] = tmp[i] / j;
        // result += term
        for (let i = 0; i < N * N; i++) result[i] += term[i];
      }

      // Squaring phase: result = result^(2^k)
      for (let j = 0; j < k; j++) {
        matmul(tmp, result, result, N);
        result.set(tmp);
      }

      // 2-norm via SVD
      dgesvd('none', 'none', N, N,
        result, 1, N, 0, s, 1, 0, U, 1, 1, 0, VT, 1, 1, 0);
      norms[step] = s[0];
    }

    self.postMessage({ id, type: 'transient', times, norms });
  }
};

function matmul(C, A, B, N) {
  for (let i = 0; i < N; i++) {
    for (let j = 0; j < N; j++) {
      let s = 0;
      for (let k = 0; k < N; k++) {
        s += A[i + k * N] * B[k + j * N];
      }
      C[i + j * N] = s;
    }
  }
}
