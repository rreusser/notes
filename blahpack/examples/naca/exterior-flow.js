// Exterior potential flow solver using AAA rational approximation.
// Combines the approach of laplace.js (Costa 2020) with the potential flow
// formulation of Baddoo (2020).
//
// Solves: w(z) = z + f(z), Im[w] = 0 on ∂D, where f → 0 as |z| → ∞.
// The boundary condition becomes: Im[f(z_j)] = -y_j.
//
// Unlike laplace.js which solves Re[w] = u, this solver enforces an
// imaginary-part boundary condition. The least-squares system uses:
//   Im[c * φ(z)] = Re(c) * Im(φ) + Im(c) * Re(φ)

import { lssolve } from "../laplace/lssolve.js";

const cadd = ([ar, ai], [br, bi]) => [ar + br, ai + bi];
const csub = ([ar, ai], [br, bi]) => [ar - br, ai - bi];
const cmul = ([ar, ai], [br, bi]) => [ar * br - ai * bi, ar * bi + ai * br];

// Evaluate Σ a_n / (z-c)^n (exterior Runge basis)
function evalRunge(z, c, coeffs) {
  const dz = csub(z, c);
  const d = dz[0] * dz[0] + dz[1] * dz[1];
  const inv = [dz[0] / d, -dz[1] / d]; // 1/(z-c)
  let power = [1, 0];
  let sum = [0, 0];
  for (let n = 0; n < coeffs.length; n++) {
    sum = cadd(sum, cmul(coeffs[n], power));
    power = cmul(power, inv);
  }
  return sum;
}

// Evaluate Σ b_k / (z - p_k) + b0
function evalSingular(z, poles, coeffs, b0) {
  let sum = [b0[0], b0[1]];
  for (let k = 0; k < poles.length; k++) {
    const dz = csub(z, poles[k]);
    const d = dz[0] * dz[0] + dz[1] * dz[1];
    const inv = [dz[0] / d, -dz[1] / d];
    sum = cadd(sum, cmul(coeffs[k], inv));
  }
  return sum;
}

function centroid(vertices) {
  let sx = 0, sy = 0;
  for (const [x, y] of vertices) { sx += x; sy += y; }
  return [sx / vertices.length, sy / vertices.length];
}

export function exteriorFlow(boundaryPoints, polygonVertices, options = {}) {
  const { aaa, pointInPolygon } = options;
  if (!aaa) throw new Error('exteriorFlow requires options.aaa (AAA algorithm function)');
  if (!pointInPolygon) throw new Error('exteriorFlow requires options.pointInPolygon');

  const M = boundaryPoints.length;
  const c = options.center || centroid(polygonVertices);
  const N = options.N || 10 + Math.ceil(Math.log(M));

  // ---- Step 1: Smooth part via 1/(z-c)^n basis + optional circulation ----
  // f(z) = Σ aₙ/(z-c)^n + (Γ/2πi) log(z-c)
  // Boundary condition: Im[f] = -y
  // Kutta condition (optional): Im[f'(z_TE) + 1] = 0 (smooth departure at TE)
  const kutta = options.kutta || false;
  const zTE = options.trailingEdge || null;
  const kuttaWeight = kutta ? Math.sqrt(M) : 0;

  // Columns: 2*N (Re/Im of a_1..a_N) + 1 (Γ, if kutta)
  const ncolsLS = 2 * N + (kutta ? 1 : 0);
  const nrowsLS = M + (kutta ? 1 : 0);
  const Asmooth = new Float64Array(nrowsLS * ncolsLS);
  const bsmooth = new Float64Array(nrowsLS);

  for (let j = 0; j < M; j++) {
    const dz = csub(boundaryPoints[j], c);
    const d = dz[0] * dz[0] + dz[1] * dz[1];
    const inv = [dz[0] / d, -dz[1] / d];
    let power = [inv[0], inv[1]]; // 1/(z-c)^1
    for (let n = 1; n <= N; n++) {
      Asmooth[j + (2 * (n - 1)) * nrowsLS] = power[1];       // Re(aₙ) column
      Asmooth[j + (2 * (n - 1) + 1) * nrowsLS] = power[0];   // Im(aₙ) column
      power = cmul(power, inv);
    }
    bsmooth[j] = -boundaryPoints[j][1]; // -y

    // Γ column: Im[(Γ/2πi) log(z-c)] = -(Γ/2π) ln|z-c|
    if (kutta) {
      Asmooth[j + (ncolsLS - 1) * nrowsLS] = -Math.log(Math.sqrt(d)) / (2 * Math.PI);
    }
  }

  // Kutta row: Im[f'(z_TE)] = -Im[g'(z_TE)] = -Im[1] = 0
  // f'(z) = Σ -n aₙ/(z-c)^{n+1} + Γ/(2πi(z-c))
  if (kutta && zTE) {
    const row = M;
    bsmooth[row] = 0; // Im[g'] = Im[1] = 0, so Im[f'] should be 0
    const dz = csub(zTE, c);
    const d = dz[0] * dz[0] + dz[1] * dz[1];
    const inv = [dz[0] / d, -dz[1] / d];
    let power = [inv[0], inv[1]]; // 1/(z-c)^1
    for (let n = 1; n <= N; n++) {
      const next = cmul(power, inv); // 1/(z-c)^{n+1}
      // d/dz [aₙ/(z-c)^n] = -n aₙ/(z-c)^{n+1}
      const dpR = -n * next[0], dpI = -n * next[1];
      Asmooth[row + (2 * (n - 1)) * nrowsLS] = kuttaWeight * dpI;
      Asmooth[row + (2 * (n - 1) + 1) * nrowsLS] = kuttaWeight * dpR;
      power = next;
    }
    // Γ derivative: d/dz [(Γ/2πi) log(z-c)] = Γ/(2πi(z-c))
    // = (Γ/2π)(Im[1/(z-c)] - i Re[1/(z-c)]) ... wait:
    // 1/(i(z-c)) = -i/(z-c) = -i(dx-idy)/d = (-dy - idx)/d
    // So Im[Γ/(2πi(z-c))] = -Γ dx/(2πd)
    const dx = zTE[0] - c[0], dy = zTE[1] - c[1];
    const dd = dx*dx + dy*dy;
    Asmooth[row + (ncolsLS - 1) * nrowsLS] = kuttaWeight * (-dx / (2 * Math.PI * dd));
  }

  const xsmooth = lssolve(Asmooth, bsmooth, nrowsLS, ncolsLS);

  const smoothCoeffs = [[0, 0]]; // a_0 = 0
  for (let n = 1; n <= N; n++) {
    smoothCoeffs.push([xsmooth[2 * (n - 1)], xsmooth[2 * (n - 1) + 1]]);
  }
  const Gamma = kutta ? xsmooth[ncolsLS - 1] : 0;

  // ---- Step 2: Compute residual ----
  const residual = new Array(M);
  for (let j = 0; j < M; j++) {
    const fSmooth = evalRunge(boundaryPoints[j], c, smoothCoeffs);
    // Circulation contribution: Im[(Γ/2πi) log(z-c)] = -(Γ/2π) ln|z-c|
    const dz = csub(boundaryPoints[j], c);
    const logR = 0.5 * Math.log(dz[0]*dz[0] + dz[1]*dz[1]);
    const circIm = -Gamma / (2 * Math.PI) * logR;
    residual[j] = -boundaryPoints[j][1] - fSmooth[1] - circIm;
  }

  // ---- Step 3: AAA on residual (treat as real-valued) ----
  const zComplex = boundaryPoints.map(([r, i]) => [r, i]);
  const fComplex = residual.map(r => [r, 0]);
  const mmax = options.mmax || 200;
  const approx = aaa(zComplex, fComplex, 1e-13, mmax);

  const allPoles = approx.pol || [];

  // ---- Step 4: Filter poles — keep only those well inside the body ----
  // Discard poles outside the body or too close to the boundary (which cause
  // visible singularity artifacts in the flow field). Exception: poles near
  // corners are always kept — they resolve the corner singularity.
  const minPoleDist = options.minPoleDist || 0.005;
  const corners = options.corners || [];
  const cornerRadius = options.cornerRadius || 0.1;
  const filteredPoles = [];
  for (const pole of allPoles) {
    if (!pointInPolygon(pole, polygonVertices)) continue;
    // Always keep poles near a corner
    let nearCorner = false;
    for (const corner of corners) {
      const dx = pole[0] - corner[0], dy = pole[1] - corner[1];
      if (dx * dx + dy * dy < cornerRadius * cornerRadius) {
        nearCorner = true;
        break;
      }
    }
    if (nearCorner) {
      filteredPoles.push(pole);
      continue;
    }
    // For non-corner poles, enforce minimum distance to boundary
    let minD2 = Infinity;
    for (let j = 0; j < M; j++) {
      const dx = pole[0] - boundaryPoints[j][0];
      const dy = pole[1] - boundaryPoints[j][1];
      minD2 = Math.min(minD2, dx * dx + dy * dy);
    }
    if (Math.sqrt(minD2) >= minPoleDist) {
      filteredPoles.push(pole);
    }
  }
  const K = filteredPoles.length;

  // ---- Step 5: Singular part — fit Im condition ----
  const singCols = 2 * K + 1; // Re/Im of b_k + real b_0
  const Asing = new Float64Array(M * singCols);
  const bsing = new Float64Array(M);

  for (let j = 0; j < M; j++) {
    for (let k = 0; k < K; k++) {
      const dz = csub(boundaryPoints[j], filteredPoles[k]);
      const d = dz[0] * dz[0] + dz[1] * dz[1];
      const invRe = dz[0] / d;
      const invIm = -dz[1] / d;
      // Im[b_k * inv] = Re(b_k)*invIm + Im(b_k)*invRe
      Asing[j + k * M] = invIm;
      Asing[j + (k + K) * M] = invRe;
    }
    // b_0 constant: Im[b_0] where b_0 is real → 0. Skip? No — b_0 can be complex.
    // Actually for Im condition with real b_0: Im[b_0] = 0. Not useful.
    // Instead, let b_0 be purely imaginary: Im[i*b_0] = b_0. Column = 1.
    Asing[j + 2 * K * M] = 1;
    bsing[j] = residual[j];
  }

  const xsing = lssolve(Asing, bsing, M, singCols);

  const singularCoeffs = [];
  for (let k = 0; k < K; k++) {
    singularCoeffs.push([xsing[k], xsing[k + K]]);
  }
  const b0 = [0, xsing[2 * K]]; // purely imaginary constant

  // ---- Step 6: Imaginary constant correction ----
  // Ensure Im[f(c)] is correct (center of body, not in flow domain, but sets the gauge)
  const imCorr = [0, 0];

  // ---- Evaluate circulation term ----
  function evalCirc(z) {
    if (Gamma === 0) return [0, 0];
    const dz = csub(z, c);
    const logR = 0.5 * Math.log(dz[0]*dz[0] + dz[1]*dz[1]);
    const logTheta = Math.atan2(dz[1], dz[0]);
    // (Γ/2πi) log(z-c) = (Γ/2π)(θ - i ln|z-c|)
    return [Gamma / (2*Math.PI) * logTheta, -Gamma / (2*Math.PI) * logR];
  }

  // ---- Compute boundary error ----
  let maxError = 0;
  for (let j = 0; j < M; j++) {
    const fS = evalRunge(boundaryPoints[j], c, smoothCoeffs);
    const fP = evalSingular(boundaryPoints[j], filteredPoles, singularCoeffs, b0);
    const fC = evalCirc(boundaryPoints[j]);
    const fIm = fS[1] + fP[1] + fC[1];
    const err = Math.abs(-boundaryPoints[j][1] - fIm);
    if (err > maxError) maxError = err;
  }

  // ---- Build evaluator ----
  function evaluate(z) {
    const fS = evalRunge(z, c, smoothCoeffs);
    const fP = evalSingular(z, filteredPoles, singularCoeffs, b0);
    const fC = evalCirc(z);
    const fR = fS[0] + fP[0] + fC[0];
    const fI = fS[1] + fP[1] + fC[1];
    // w = z + f
    const psi = z[1] + fI;
    const wR = z[0] + fR;
    return { psi, wR, fR, fI };
  }

  const CL = -2 * Gamma; // CL = -2Γ/U for unit velocity

  return {
    evaluate,
    Gamma, CL,
    poles: filteredPoles,
    allPoles,
    smoothCoeffs,
    singularCoeffs,
    b0,
    maxError,
    center: c,
    N,
  };
}
