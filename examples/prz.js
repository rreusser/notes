// Poles, residues, zeros for AAA rational approximation.
// Uses QZ algorithm for numerically stable pole/zero computation.
// All complex arithmetic uses in-place gl-matrix-style operations.

import { zggev, cabs, cmul, cadd, csub, cscale, cset } from "./qz.js";

// Scratch arrays for prz computations
const _p1 = [0, 0], _p2 = [0, 0], _p3 = [0, 0];

// ---- Generalized eigenvalue approach for poles/zeros ----
function barycentricRoots(z, weights) {
  const m = z.length;
  if (m <= 1) return [];

  const n = m + 1;
  const E = [];
  for (let i = 0; i < n; i++) {
    E[i] = [];
    for (let j = 0; j < n; j++) E[i][j] = [0, 0];
  }
  for (let j = 0; j < m; j++) { E[0][j + 1][0] = weights[j][0]; E[0][j + 1][1] = weights[j][1]; }
  for (let i = 0; i < m; i++) cset(E[i + 1][0], 1, 0);
  for (let i = 0; i < m; i++) { E[i + 1][i + 1][0] = z[i][0]; E[i + 1][i + 1][1] = z[i][1]; }

  const B = [];
  for (let i = 0; i < n; i++) {
    B[i] = [];
    for (let j = 0; j < n; j++) B[i][j] = [0, 0];
  }
  for (let i = 1; i < n; i++) cset(B[i][i], 1, 0);

  return zggev(E, B, n);
}

// Evaluate barycentric rational function r(zz) = N(zz)/D(zz)
function feval(zz, z, f, w) {
  const result = new Array(zz.length);
  for (let idx = 0; idx < zz.length; idx++) {
    const zv = zz[idx];
    // Check if zv coincides with a support point
    let exact = -1;
    for (let j = 0; j < z.length; j++) {
      if (zv[0] === z[j][0] && zv[1] === z[j][1]) { exact = j; break; }
    }
    if (exact >= 0) {
      result[idx] = [f[exact][0], f[exact][1]];
      continue;
    }

    let nr = 0, ni = 0, dr = 0, di = 0;
    for (let j = 0; j < z.length; j++) {
      // inv = 1 / (zv - z[j])
      const dx = zv[0] - z[j][0], dy = zv[1] - z[j][1];
      const d = dx * dx + dy * dy;
      const invr = dx / d, invi = -dy / d;
      // wf = w[j] * f[j]
      const wfr = w[j][0] * f[j][0] - w[j][1] * f[j][1];
      const wfi = w[j][0] * f[j][1] + w[j][1] * f[j][0];
      // wf * inv
      nr += wfr * invr - wfi * invi;
      ni += wfr * invi + wfi * invr;
      // w[j] * inv
      dr += w[j][0] * invr - w[j][1] * invi;
      di += w[j][0] * invi + w[j][1] * invr;
    }
    // result = N / D
    const dd = dr * dr + di * di;
    result[idx] = [(nr * dr + ni * di) / dd, (ni * dr - nr * di) / dd];
  }
  return result;
}

// ---- PRZ: Compute poles, residues, zeros ----
export function prz(z, f, w) {
  const pol = barycentricRoots(z, w);

  // Zeros: weights = w .* f
  const wf = new Array(w.length);
  for (let i = 0; i < w.length; i++) {
    wf[i] = [0, 0];
    cmul(wf[i], w[i], f[i]);
  }
  const zer = barycentricRoots(z, wf);

  // Residues via 4-point numerical contour integration
  const dz = new Array(4);
  for (let k = 0; k < 4; k++) {
    const theta = (2 * Math.PI * (k + 1)) / 4;
    dz[k] = [1e-5 * Math.cos(theta), 1e-5 * Math.sin(theta)];
  }

  const res = new Array(pol.length);
  for (let j = 0; j < pol.length; j++) {
    const pts = new Array(4);
    for (let k = 0; k < 4; k++) {
      pts[k] = [pol[j][0] + dz[k][0], pol[j][1] + dz[k][1]];
    }
    const vals = feval(pts, z, f, w);
    let rr = 0, ri = 0;
    for (let k = 0; k < 4; k++) {
      // rr += vals[k] * dz[k]
      rr += vals[k][0] * dz[k][0] - vals[k][1] * dz[k][1];
      ri += vals[k][0] * dz[k][1] + vals[k][1] * dz[k][0];
    }
    res[j] = [0.25 * rr, 0.25 * ri];
  }

  return { pol, res, zer };
}

// ---- Cleanup: remove Froissart doublets ----
export function cleanup(z, f, w, pol, res, zer) {
  const negligible = [];
  for (let i = 0; i < res.length; i++) {
    if (cabs(res[i]) < 1e-13) negligible.push(i);
  }
  if (negligible.length === 0) return { z, f, w, pol, res, zer };

  const remove = new Set();
  for (const ni of negligible) {
    let minDist = Infinity, nearest = -1;
    for (let k = 0; k < z.length; k++) {
      if (remove.has(k)) continue;
      const dr = z[k][0] - pol[ni][0], di = z[k][1] - pol[ni][1];
      const dist = Math.sqrt(dr * dr + di * di);
      if (dist < minDist) { minDist = dist; nearest = k; }
    }
    if (nearest >= 0) remove.add(nearest);
  }

  const newZ = [], newF = [], newW = [];
  for (let i = 0; i < z.length; i++) {
    if (!remove.has(i)) {
      newZ.push(z[i]); newF.push(f[i]); newW.push(w[i]);
    }
  }

  const result = prz(newZ, newF, newW);
  return { z: newZ, f: newF, w: newW, pol: result.pol, res: result.res, zer: result.zer };
}

export { feval, barycentricRoots };
