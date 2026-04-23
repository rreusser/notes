// Exterior potential flow solver using AAA rational approximation.
//
// Solves: w(z) = z + f(z), Im[w] = 0 on ∂D, where f → 0 as |z| → ∞.
// Optionally enforces the Kutta condition at the trailing edge.
//
// Approach:
//   1. Run AAA on Im[z] restricted to the boundary to get pole locations.
//   2. Filter poles to keep only those inside the body.
//   3. Build a combined LS system: polynomial 1/(z-c)^n + poles 1/(z-p_k) + Γ log(z-c).
//   4. Solve with optional Kutta condition row.

import { lssolve } from "../laplace/lssolve.js";

const csub = ([ar, ai], [br, bi]) => [ar - br, ai - bi];
const cmul = ([ar, ai], [br, bi]) => [ar * br - ai * bi, ar * bi + ai * br];

function centroid(vertices) {
  let sx = 0, sy = 0;
  for (const [x, y] of vertices) { sx += x; sy += y; }
  return [sx / vertices.length, sy / vertices.length];
}

export function exteriorFlow(boundaryPoints, polygonVertices, options = {}) {
  const { aaa, pointInPolygon } = options;
  if (!aaa) throw new Error('exteriorFlow requires options.aaa');
  if (!pointInPolygon) throw new Error('exteriorFlow requires options.pointInPolygon');

  const M = boundaryPoints.length;
  const c = options.center || centroid(polygonVertices);
  const N = options.N || 10;
  const kutta = options.kutta || false;
  const zTE = options.trailingEdge || null;
  const kuttaWeight = kutta ? (options.kuttaWeight || M * M) : 0;

  // ---- Step 1: Run AAA to discover pole locations ----
  // Use boundary values -y (the target for Im[f]) as a real-valued function.
  const zComplex = boundaryPoints.map(([r, i]) => [r, i]);
  const fComplex = boundaryPoints.map(([x, y]) => [-y, 0]);
  const mmax = options.mmax || 200;
  const approx = aaa(zComplex, fComplex, 1e-13, mmax);

  // ---- Step 2: Filter poles — keep those inside the body ----
  const allPoles = approx.pol || [];
  const minPoleDist = options.minPoleDist || 0.005;
  const corners = options.corners || [];
  const cornerRadius = options.cornerRadius || 0.1;
  const poles = [];
  for (const pole of allPoles) {
    if (!pointInPolygon(pole, polygonVertices)) continue;
    let nearCorner = false;
    for (const corner of corners) {
      const dx = pole[0] - corner[0], dy = pole[1] - corner[1];
      if (dx * dx + dy * dy < cornerRadius * cornerRadius) { nearCorner = true; break; }
    }
    if (nearCorner) { poles.push(pole); continue; }
    let minD2 = Infinity;
    for (let j = 0; j < M; j++) {
      const dx = pole[0] - boundaryPoints[j][0], dy = pole[1] - boundaryPoints[j][1];
      minD2 = Math.min(minD2, dx * dx + dy * dy);
    }
    if (Math.sqrt(minD2) >= minPoleDist) poles.push(pole);
  }
  const K = poles.length;

  // ---- Step 3: Build combined LS system ----
  // Unknowns: 2*N (Re/Im of a_1..a_N polynomial) + 2*K (Re/Im of b_k poles) + 1 (Γ if kutta)
  const nCols = 2 * N + 2 * K + (kutta ? 1 : 0);
  const nRows = M + (kutta ? 1 : 0);
  const A = new Float64Array(nRows * nCols);
  const b = new Float64Array(nRows);

  for (let j = 0; j < M; j++) {
    b[j] = -boundaryPoints[j][1]; // Im[f] = -y

    // Polynomial columns: 1/(z-c)^n for n=1..N
    const dz = csub(boundaryPoints[j], c);
    const d = dz[0] * dz[0] + dz[1] * dz[1];
    const inv = [dz[0] / d, -dz[1] / d];
    let power = [inv[0], inv[1]]; // start at n=1
    for (let n = 0; n < N; n++) {
      A[j + (2 * n) * nRows] = power[1];       // Im part for Re(a_n)
      A[j + (2 * n + 1) * nRows] = power[0];   // Re part for Im(a_n)
      power = cmul(power, inv);
    }

    // Pole columns: 1/(z - p_k)
    for (let k = 0; k < K; k++) {
      const pdz = csub(boundaryPoints[j], poles[k]);
      const pd = pdz[0] * pdz[0] + pdz[1] * pdz[1];
      const col = 2 * N + 2 * k;
      A[j + col * nRows] = -pdz[1] / pd;       // Im(1/(z-p))
      A[j + (col + 1) * nRows] = pdz[0] / pd;  // Re(1/(z-p))
    }

    // Γ column: Im[(Γ/2πi) log(z-c)] = -(Γ/2π) ln|z-c|
    if (kutta) {
      A[j + (nCols - 1) * nRows] = -0.5 * Math.log(d) / (2 * Math.PI);
    }
  }

  // Kutta row: Re[f'(z_TE)] = -1 (cancel the free-stream velocity g'=1)
  // This enforces stagnation at the trailing edge: w'(z_TE) = g'(z_TE) + f'(z_TE) = 0
  // Since g'(z) = 1 (uniform flow), we need Re[f'(z_TE)] = -1.
  if (kutta && zTE) {
    const row = M;
    b[row] = kuttaWeight * (-1); // Re[f'] = -Re[g'] = -1

    // Re[c * φ'] = Re(c)*Re(φ') - Im(c)*Im(φ')
    const dz = csub(zTE, c);
    const d = dz[0] * dz[0] + dz[1] * dz[1];
    const inv = [dz[0] / d, -dz[1] / d];
    let power = [inv[0], inv[1]];
    for (let n = 0; n < N; n++) {
      const next = cmul(power, inv);
      const dpR = -(n + 1) * next[0], dpI = -(n + 1) * next[1];
      A[row + (2 * n) * nRows] = kuttaWeight * dpR;       // Re(a_n) * Re(φ')
      A[row + (2 * n + 1) * nRows] = kuttaWeight * (-dpI); // Im(a_n) * (-Im(φ'))
      power = next;
    }

    for (let k = 0; k < K; k++) {
      const pdz = csub(zTE, poles[k]);
      const pd = pdz[0] * pdz[0] + pdz[1] * pdz[1];
      const pd2 = pd * pd;
      const dpR = -(pdz[0] * pdz[0] - pdz[1] * pdz[1]) / pd2;
      const dpI = 2 * pdz[0] * pdz[1] / pd2;
      const col = 2 * N + 2 * k;
      A[row + col * nRows] = kuttaWeight * dpR;
      A[row + (col + 1) * nRows] = kuttaWeight * (-dpI);
    }

    // Circulation derivative: Re[Γ/(2πi(z-c))]
    // 1/(i(z-c)) = -i/(z-c) = -(dy + i dx)/d
    // Re of that = -dy/d. Per unit Γ: -dy/(2πd)
    A[row + (nCols - 1) * nRows] = kuttaWeight * (-dz[1] / (2 * Math.PI * d));
  }

  // ---- Step 4: Column scaling and solve ----
  const colNorms = new Float64Array(nCols);
  const activeCols = [];
  for (let col = 0; col < nCols; col++) {
    let norm = 0;
    for (let r = 0; r < nRows; r++) norm += A[r + col * nRows] ** 2;
    colNorms[col] = Math.sqrt(norm);
    if (colNorms[col] > 1e-14) activeCols.push(col);
  }
  const nActive = activeCols.length;
  const Ar = new Float64Array(nRows * nActive);
  const colScale = new Float64Array(nActive);
  for (let j = 0; j < nActive; j++) {
    colScale[j] = 1 / colNorms[activeCols[j]];
    for (let r = 0; r < nRows; r++) Ar[r + j * nRows] = A[r + activeCols[j] * nRows] * colScale[j];
  }
  const xr = lssolve(Ar, new Float64Array(b), nRows, nActive);
  const allCoeffs = new Float64Array(nCols);
  for (let j = 0; j < nActive; j++) allCoeffs[activeCols[j]] = xr[j] * colScale[j];

  // Extract
  const smoothCoeffs = [[0, 0]]; // a_0 = 0
  for (let n = 0; n < N; n++) smoothCoeffs.push([allCoeffs[2*n], allCoeffs[2*n+1]]);
  const singularCoeffs = [];
  for (let k = 0; k < K; k++) singularCoeffs.push([allCoeffs[2*N+2*k], allCoeffs[2*N+2*k+1]]);
  const Gamma = kutta ? allCoeffs[nCols - 1] : 0;

  // ---- Evaluator ----
  function evaluate(z) {
    // Runge part: Σ aₙ/(z-c)^n
    const dz = csub(z, c);
    const d = dz[0] * dz[0] + dz[1] * dz[1];
    const inv = [dz[0] / d, -dz[1] / d];
    let power = [1, 0];
    let fR = 0, fI = 0;
    for (let n = 0; n < smoothCoeffs.length; n++) {
      fR += smoothCoeffs[n][0] * power[0] - smoothCoeffs[n][1] * power[1];
      fI += smoothCoeffs[n][0] * power[1] + smoothCoeffs[n][1] * power[0];
      power = cmul(power, inv);
    }
    // Singular part: Σ bₖ/(z-pₖ)
    for (let k = 0; k < K; k++) {
      const pdz = csub(z, poles[k]);
      const pd = pdz[0] * pdz[0] + pdz[1] * pdz[1];
      const cr = singularCoeffs[k][0], ci = singularCoeffs[k][1];
      fR += cr * pdz[0] / pd + ci * pdz[1] / pd;
      fI += -cr * pdz[1] / pd + ci * pdz[0] / pd;
    }
    // Circulation: (Γ/2πi) log(z-c)
    if (Gamma !== 0) {
      const logR = 0.5 * Math.log(d);
      const theta = Math.atan2(dz[1], dz[0]);
      fR += Gamma / (2 * Math.PI) * theta;
      fI += -Gamma / (2 * Math.PI) * logR;
    }
    return { psi: z[1] + fI, wR: z[0] + fR, fR, fI };
  }

  // Boundary error
  let maxError = 0;
  for (let j = 0; j < M; j++) {
    const err = Math.abs(evaluate(boundaryPoints[j]).psi);
    if (err > maxError) maxError = err;
  }

  const CL = -2 * Gamma;
  return { evaluate, poles, allPoles, smoothCoeffs, singularCoeffs, Gamma, CL, maxError, center: c, N };
}
