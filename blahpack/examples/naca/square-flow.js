// Potential flow past a square — the simplest lightning solver test case.
// Following Baddoo (2020) §2.1, Figure 1.
//
// w(z) = z + f(z) where f is the Runge+Newman ansatz (eq 4).
// f decays at infinity so w ~ z (uniform flow).
// Boundary condition: Im[w] = 0 on ∂D, i.e., Im[f] = -Im[z] = -y.

import { lssolve } from '../laplace/lssolve.js';
import { vaOrthog, vaEval, vaEvalBasis } from '../lib/va-orthog.js';

function placePoles(corner, angle, N, sigma, L) {
  const poles = [];
  const sqrtN = Math.sqrt(N);
  for (let j = 1; j <= N; j++) {
    const dist = L * Math.exp(-sigma * (sqrtN - Math.sqrt(j)));
    poles.push([corner[0] + dist * Math.cos(angle), corner[1] + dist * Math.sin(angle)]);
  }
  return poles;
}

export function squareFlow(options = {}) {
  const {
    N = 24,           // poles per corner
    nPoly = 20,       // VA polynomial degree
    nSample = 200,    // boundary points per side
    sigma = 4,
    L = 2,
    halfSide = 0.5,   // half-side length of square
  } = options;

  const s = halfSide;
  const corners = [[-s, -s], [s, -s], [s, s], [-s, s]];
  // Bisector angles: point outward from each corner
  const bisectors = [-3*Math.PI/4, -Math.PI/4, Math.PI/4, 3*Math.PI/4];

  // Place lightning poles at each corner
  const allPoles = [];
  for (let k = 0; k < 4; k++) {
    allPoles.push(...placePoles(corners[k], bisectors[k], N, sigma, L));
  }
  const nPoles = allPoles.length;

  // Boundary sample points (uniform along each side, excluding corners)
  const boundary = [];
  const sides = [
    [corners[0], corners[1]], [corners[1], corners[2]],
    [corners[2], corners[3]], [corners[3], corners[0]],
  ];
  for (const [start, end] of sides) {
    for (let i = 1; i <= nSample; i++) {
      const t = i / (nSample + 1);
      boundary.push([
        start[0] + t * (end[0] - start[0]),
        start[1] + t * (end[1] - start[1]),
      ]);
    }
  }
  const M = boundary.length;

  // z* = center of the square (inside the body, outside the flow domain)
  const zStar = [0, 0];

  // Transform to ζ = 1/(z - z*) for VA orthogonalization
  const zetaBdy = boundary.map(z => {
    const dx = z[0] - zStar[0], dy = z[1] - zStar[1];
    const d = dx * dx + dy * dy;
    return [dx / d, -dy / d];
  });

  // VA orthogonalization on the ζ-space boundary
  const va = vaOrthog(zetaBdy, nPoly);
  const { Q: polyQ } = vaEvalBasis(va, zetaBdy);
  const nPolyBasis = nPoly + 1;

  // Build least-squares system: Im[f(z_j)] = -y_j
  // f(z) = Σ cₙ qₙ(ζ) + Σ bₖ/(z - pₖ)
  const nCols = 2 * nPolyBasis + 2 * nPoles;
  const A = new Float64Array(M * nCols);
  const b = new Float64Array(M);

  for (let j = 0; j < M; j++) {
    b[j] = -boundary[j][1]; // -y

    // VA polynomial columns
    for (let n = 0; n < nPolyBasis; n++) {
      const qR = polyQ[2 * (j + n * M)];
      const qI = polyQ[2 * (j + n * M) + 1];
      A[j + (2 * n) * M] = qI;         // Re(cₙ) * Im(qₙ)
      A[j + (2 * n + 1) * M] = qR;     // Im(cₙ) * Re(qₙ)
    }

    // Newman pole columns: 1/(z - pₖ)
    for (let k = 0; k < nPoles; k++) {
      const dx = boundary[j][0] - allPoles[k][0];
      const dy = boundary[j][1] - allPoles[k][1];
      const d = dx * dx + dy * dy;
      const col = 2 * nPolyBasis + 2 * k;
      A[j + col * M] = -dy / d;       // Im(1/(z-p))
      A[j + (col + 1) * M] = dx / d;  // Re(1/(z-p))
    }
  }

  // Column scaling and solve
  const activeCols = [], colScales = [];
  for (let c = 0; c < nCols; c++) {
    let norm = 0;
    for (let r = 0; r < M; r++) norm += A[r + c * M] ** 2;
    norm = Math.sqrt(norm);
    if (norm > 1e-14) { activeCols.push(c); colScales.push(norm); }
  }
  const nActive = activeCols.length;
  const Ar = new Float64Array(M * nActive);
  for (let j = 0; j < nActive; j++) {
    for (let r = 0; r < M; r++) Ar[r + j * M] = A[r + activeCols[j] * M] / colScales[j];
  }
  const xr = lssolve(Ar, new Float64Array(b), M, nActive);
  const allCoeffs = new Float64Array(nCols);
  for (let j = 0; j < nActive; j++) allCoeffs[activeCols[j]] = xr[j] / colScales[j];

  // Extract coefficients
  const polyCoeffs = [];
  for (let n = 0; n < nPolyBasis; n++) polyCoeffs.push([allCoeffs[2*n], allCoeffs[2*n+1]]);
  const poleCoeffs = [];
  for (let k = 0; k < nPoles; k++) {
    const col = 2 * nPolyBasis + 2 * k;
    poleCoeffs.push([allCoeffs[col], allCoeffs[col + 1]]);
  }

  // Evaluator: w(z) = z + f(z)
  function evaluate(z) {
    const zr = z[0], zi = z[1];
    let fr = 0, fi = 0;

    // VA polynomial part
    const dx = zr - zStar[0], dy = zi - zStar[1];
    const d = dx * dx + dy * dy;
    const zetaR = dx / d, zetaI = -dy / d;
    const { q } = vaEval(va, [zetaR, zetaI]);
    for (let n = 0; n < nPolyBasis; n++) {
      const cr = polyCoeffs[n][0], ci = polyCoeffs[n][1];
      fr += cr * q[2*n] - ci * q[2*n+1];
      fi += cr * q[2*n+1] + ci * q[2*n];
    }

    // Newman part
    for (let k = 0; k < nPoles; k++) {
      const px = zr - allPoles[k][0], py = zi - allPoles[k][1];
      const pd = px * px + py * py;
      const cr = poleCoeffs[k][0], ci = poleCoeffs[k][1];
      fr += cr * px/pd + ci * py/pd;
      fi += -cr * py/pd + ci * px/pd;
    }

    // w = z + f
    const wR = zr + fr;
    const wI = zi + fi; // ψ = Im(w)
    return { psi: wI, wR, fr, fi };
  }

  // Boundary error
  let maxError = 0;
  for (let j = 0; j < M; j++) {
    const { psi } = evaluate(boundary[j]);
    const err = Math.abs(psi); // should be 0
    if (err > maxError) maxError = err;
  }

  return { evaluate, boundary, corners, allPoles, polyCoeffs, poleCoeffs, maxError, va, zStar, nPoles, nPoly };
}
