// Potential flow past a NACA airfoil using the lightning solver method.
// Reference: Baddoo (2020), "Lightning Solvers for Potential Flows", eq. (4)-(17).
//
// The complex potential is w(z) = g(z) + f(z) where:
//   g(z) = U·z·e^{-iα} + (Γ/2πi) log(z - z*)  (free stream + circulation)
//   f(z) = Σ_{j=0}^{n1} a_j/(z-z*)^j + Σ_{k=1}^{n2} b_k/(z-p_k)  (boundary correction)
//
// The Runge part (powers of 1/(z-z*)) captures the smooth solution.
// The Newman part (simple poles near the TE) captures the corner singularity.
// Boundary condition: Im[w] = 0 on the airfoil (streamline).
// Kutta condition: w'(z_TE) = 0 (smooth departure at trailing edge).

import { lssolve } from '../laplace/lssolve.js';
import { nacaAirfoil, trailingEdgeBisector } from './naca.js';

// ---- Lightning pole placement ----
function placePoles(corner, angle, N, sigma, L) {
  const poles = [];
  const sqrtN = Math.sqrt(N);
  for (let j = 1; j <= N; j++) {
    const dist = L * Math.exp(-sigma * (sqrtN - Math.sqrt(j)));
    poles.push([
      corner[0] + dist * Math.cos(angle),
      corner[1] + dist * Math.sin(angle),
    ]);
  }
  return poles;
}

// ---- Main solver ----
export function potentialFlow(options = {}) {
  const {
    digits = '2412',
    nPoints = 400,
    nPoles = 30,          // Newman poles at trailing edge
    nRunge = 10,          // Runge polynomial degree in 1/(z-z*)
    sigma = 4,
    L = 4,
    alpha = 5,            // angle of attack (degrees)
    U = 1,
    kutta = true,
  } = options;

  const alphaRad = alpha * Math.PI / 180;

  // ---- Step 1: Airfoil geometry ----
  const boundary = nacaAirfoil(digits, nPoints);
  const M = boundary.length;
  const te = boundary[0];

  // Center point inside airfoil (for Runge basis and circulation log).
  // Must be off the real axis for Im[1/(z-z*)^n] to be nonzero on the boundary.
  // Use a point on the camber line at 30% chord.
  const M_cam = parseInt(digits[0]) / 100;
  const P_cam = parseInt(digits[1]) / 10 || 0.3;
  const yCamber = M_cam > 0 ? (M_cam / (P_cam * P_cam)) * (2 * P_cam * 0.3 - 0.09) : 0.01;
  const zStar = [0.3, yCamber || 0.01];

  // ---- Step 2: Lightning poles at trailing edge ----
  const bisectorAngle = trailingEdgeBisector(boundary);
  const poles = placePoles(te, bisectorAngle, nPoles, sigma, L);

  // ---- Step 3: Build least-squares system ----
  // Unknowns (all real, because we split Re/Im):
  //   Runge: Re(a_0), Re(a_1), Im(a_1), Re(a_2), Im(a_2), ..., Re(a_{n1}), Im(a_{n1})
  //     = 1 + 2*nRunge unknowns (a_0 is real because Im[a_0]=const is gauge freedom)
  //   Newman: Re(b_1), Im(b_1), ..., Re(b_{n2}), Im(b_{n2})
  //     = 2*nPoles unknowns
  //   Gamma: 1 unknown (if kutta)
  //
  // Boundary condition: Im[f(z_j) + g(z_j)] = 0
  //   Im[f] = Im[Runge] + Im[Newman]
  //   Im[g] = Im[U z e^{-iα}] + (Γ/2π) Re[log(z-z*)] ... wait:
  //     g(z) = Uz e^{-iα} + (Γ/2πi) log(z-z*)
  //     Im[g] = U(y cosα - x sinα) - (Γ/2π) ln|z-z*|
  //
  // For basis function φ(z) with coefficient c = c_r + i c_i:
  //   Im[c·φ] = c_r · Im(φ) + c_i · Re(φ)

  // a_0 dropped (doesn't appear in Im condition), so Runge has 2*nRunge unknowns
  const nRungeUnknowns = 2 * nRunge;
  const nNewmanUnknowns = 2 * nPoles;
  const nGammaUnknowns = kutta ? 1 : 0;
  const nCols = nRungeUnknowns + nNewmanUnknowns + nGammaUnknowns;
  const nRows = M + (kutta ? 1 : 0);

  const A = new Float64Array(nRows * nCols);
  const b = new Float64Array(nRows);

  // Precompute 1/(z_j - z*) and its powers
  for (let j = 0; j < M; j++) {
    const zj = boundary[j];

    // Free-stream RHS
    const gIm = U * (zj[1] * Math.cos(alphaRad) - zj[0] * Math.sin(alphaRad));
    b[j] = -gIm;

    // Runge basis: φ_n(z) = 1/(z - z*)^n
    const dx = zj[0] - zStar[0], dy = zj[1] - zStar[1];
    const d = dx * dx + dy * dy;
    // 1/(z-z*) = (dx - i dy) / d
    let pR = 1, pI = 0; // (1/(z-z*))^0 = 1
    // a_0: real coefficient, basis = 1 (constant). Im[a_0 · 1] = 0, but
    // we need it for the real part. Actually Im[a_0] = 0 by convention,
    // so Im[a_0 · 1] = 0. This column would be zero! Skip a_0 in Im equation.
    // Wait, a_0 is complex in general. We choose it real (gauge freedom).
    // For Im condition: Im[a_0 · 1] = 0 since a_0 is real. So first column = 0.
    // Better: include a_0 as contributing to Re[f], but our boundary condition
    // is on Im[f]. a_0 real doesn't appear in Im. So skip it? No — a_0 is the
    // constant that doesn't affect Im at all. It's a degree of freedom in Re[f]
    // that we don't need to solve for in the Im condition.
    //
    // Actually, the boundary condition is Im[w] = 0 = Im[g] + Im[f].
    // If a_0 is real, Im[a_0] = 0, so it doesn't appear. We can drop it.
    // For n >= 1: Im[a_n / (z-z*)^n] = Re(a_n)·Im[1/(z-z*)^n] + Im(a_n)·Re[1/(z-z*)^n]

    // Power iteration: p^{n+1} = p^n * (1/(z-z*))
    const invR = dx / d, invI = -dy / d;

    for (let n = 1; n <= nRunge; n++) {
      const newR = pR * invR - pI * invI;
      const newI = pR * invI + pI * invR;
      pR = newR; pI = newI;
      // Column for Re(a_n): contributes pI to Im[f]
      const colRe = 2 * (n - 1);     // columns: [Re(a1), Im(a1), Re(a2), Im(a2), ...]
      const colIm = 2 * (n - 1) + 1;
      A[j + colRe * nRows] = pI;
      A[j + colIm * nRows] = pR;
    }

    // Newman basis: φ_k(z) = 1/(z - p_k)
    for (let k = 0; k < nPoles; k++) {
      const px = zj[0] - poles[k][0], py = zj[1] - poles[k][1];
      const pd = px * px + py * py;
      const phiR = px / pd, phiI = -py / pd;
      const colBase = nRungeUnknowns + 2 * k;
      A[j + colBase * nRows] = phiI;       // Re(b_k) contributes Im(φ)
      A[j + (colBase + 1) * nRows] = phiR; // Im(b_k) contributes Re(φ)
    }

    // Circulation column: Im[(Γ/2πi) log(z-z*)] = -(Γ/2π) ln|z-z*|
    if (kutta) {
      const logR = 0.5 * Math.log(d);
      A[j + (nCols - 1) * nRows] = -logR / (2 * Math.PI);
    }
  }

  // ---- Kutta condition: Im[w'(z_TE)] = 0 ----
  if (kutta) {
    const row = M;
    const zTE = te;

    // g'(z) = U e^{-iα}, Im[g'] = -U sin(α)
    b[row] = U * Math.sin(alphaRad);

    // Runge basis derivative: d/dz [1/(z-z*)^n] = -n/(z-z*)^{n+1}
    const dx = zTE[0] - zStar[0], dy = zTE[1] - zStar[1];
    const d = dx * dx + dy * dy;
    const invR = dx / d, invI = -dy / d;
    let pR = invR, pI = invI; // (1/(z-z*))^1
    for (let n = 1; n <= nRunge; n++) {
      // Derivative: -n * (1/(z-z*))^{n+1}
      const nextR = pR * invR - pI * invI;
      const nextI = pR * invI + pI * invR;
      const dpR = -n * nextR, dpI = -n * nextI;
      const colRe = 2 * (n - 1);
      const colIm = 2 * (n - 1) + 1;
      A[row + colRe * nRows] = dpI;
      A[row + colIm * nRows] = dpR;
      pR = nextR; pI = nextI;
    }

    // Newman basis derivative: -1/(z-p_k)^2
    for (let k = 0; k < nPoles; k++) {
      const px = zTE[0] - poles[k][0], py = zTE[1] - poles[k][1];
      const pd = px * px + py * py;
      const pd2 = pd * pd;
      const dpR = -(px * px - py * py) / pd2;
      const dpI = 2 * px * py / pd2;
      const colBase = nRungeUnknowns + 2 * k;
      A[row + colBase * nRows] = dpI;
      A[row + (colBase + 1) * nRows] = dpR;
    }

    // Circulation derivative: Im[Γ/(2πi(z-z*))] = -(Γ/2π) Re[1/(z-z*)] ... wait:
    // d/dz [(Γ/2πi) log(z-z*)] = Γ/(2πi(z-z*))
    // = (Γ/2π) · (-i) · (dx-idy)/d = (Γ/2π) · (-dy - i dx)/d
    // Im = -(Γ/2π) · dx/d, per unit Γ: -dx/(2πd)
    A[row + (nCols - 1) * nRows] = -dx / (2 * Math.PI * d);
  }

  // ---- Step 4: Column scaling and solve ----
  const actualNCols = nCols - 1; // We dropped a_0
  // Wait, I already adjusted indexing above. Let me re-count.
  // Columns: 0..2*nRunge-1 are the Runge part (Re/Im of a_1..a_{nRunge})
  // That's 2*nRunge columns.
  // Then 2*nPoles columns for Newman.
  // Then 1 for Gamma.
  // Total = 2*nRunge + 2*nPoles + (kutta?1:0)
  // But nRungeUnknowns = 1 + 2*nRunge. I wrote colBase = nRungeUnknowns - 1 + 2*k.
  // So the Newman columns start at index 2*nRunge.
  // And Gamma at index 2*nRunge + 2*nPoles.
  // So nCols should be 2*nRunge + 2*nPoles + (kutta?1:0).
  // But I set nCols = nRungeUnknowns + nNewmanUnknowns + nGammaUnknowns = (1+2*nRunge) + 2*nPoles + ...
  // That's 1 too many columns because I dropped a_0. The extra column is unused!

  // Column scaling for stability
  const colNorms = new Float64Array(nCols);
  const activeCols = [];
  for (let c = 0; c < nCols; c++) {
    let norm2 = 0;
    for (let r = 0; r < nRows; r++) norm2 += A[r + c * nRows] ** 2;
    colNorms[c] = Math.sqrt(norm2);
    if (colNorms[c] > 1e-14) activeCols.push(c);
  }
  const nActive = activeCols.length;
  const Ar = new Float64Array(nRows * nActive);
  const colScale = new Float64Array(nActive);
  for (let j = 0; j < nActive; j++) {
    const c = activeCols[j];
    colScale[j] = 1 / colNorms[c];
    for (let r = 0; r < nRows; r++) Ar[r + j * nRows] = A[r + c * nRows] * colScale[j];
  }

  const bCopy = new Float64Array(b);
  const xr = lssolve(Ar, bCopy, nRows, nActive);

  const allCoeffs = new Float64Array(nCols);
  for (let j = 0; j < nActive; j++) allCoeffs[activeCols[j]] = xr[j] * colScale[j];

  // Extract Runge coefficients (complex, a_1..a_{nRunge})
  const rungeCoeffs = [[0, 0]]; // a_0 = 0 (dropped)
  for (let n = 1; n <= nRunge; n++) {
    rungeCoeffs.push([allCoeffs[2 * (n - 1)], allCoeffs[2 * (n - 1) + 1]]);
  }

  // Extract Newman coefficients
  const newmanCoeffs = [];
  for (let k = 0; k < nPoles; k++) {
    newmanCoeffs.push([allCoeffs[2 * nRunge + 2 * k], allCoeffs[2 * nRunge + 2 * k + 1]]);
  }

  const Gamma = kutta ? allCoeffs[nCols - 1] : 0;

  // ---- Step 5: Build evaluator ----
  function evaluate(z) {
    const zr = z[0], zi = z[1];

    // f(z) = Σ a_n / (z-z*)^n + Σ b_k / (z-p_k)
    let fr = 0, fi = 0, fdr = 0, fdi = 0;

    // Runge part
    const dx = zr - zStar[0], dy = zi - zStar[1];
    const d = dx * dx + dy * dy;
    const invR = dx / d, invI = -dy / d;
    let pR = 1, pI = 0;
    for (let n = 1; n <= nRunge; n++) {
      const newR = pR * invR - pI * invI;
      const newI = pR * invI + pI * invR;
      pR = newR; pI = newI;
      const cr = rungeCoeffs[n][0], ci = rungeCoeffs[n][1];
      fr += cr * pR - ci * pI;
      fi += cr * pI + ci * pR;
      // Derivative: -n/(z-z*)^{n+1}
      const nextR = pR * invR - pI * invI;
      const nextI = pR * invI + pI * invR;
      fdr += cr * (-n * nextR) - ci * (-n * nextI);
      fdi += cr * (-n * nextI) + ci * (-n * nextR);
    }

    // Newman part
    for (let k = 0; k < nPoles; k++) {
      const px = zr - poles[k][0], py = zi - poles[k][1];
      const pd = px * px + py * py;
      const phiR = px / pd, phiI = -py / pd;
      const cr = newmanCoeffs[k][0], ci = newmanCoeffs[k][1];
      fr += cr * phiR - ci * phiI;
      fi += cr * phiI + ci * phiR;
      // Derivative: -1/(z-p_k)^2
      const pd2 = pd * pd;
      const dpR = -(px * px - py * py) / pd2;
      const dpI = 2 * px * py / pd2;
      fdr += cr * dpR - ci * dpI;
      fdi += cr * dpI + ci * dpR;
    }

    // Circulation term
    const logR = 0.5 * Math.log(d);
    const logTheta = Math.atan2(dy, dx);
    const cR = Gamma / (2 * Math.PI) * logTheta;
    const cI = -Gamma / (2 * Math.PI) * logR;
    const cdR = -Gamma / (2 * Math.PI) * dy / d;
    const cdI = -Gamma / (2 * Math.PI) * dx / d;
    fr += cR; fi += cI;
    fdr += cdR; fdi += cdI;

    // Free stream
    const gR = U * (zr * Math.cos(alphaRad) + zi * Math.sin(alphaRad));
    const gI = U * (zi * Math.cos(alphaRad) - zr * Math.sin(alphaRad));
    const gdR = U * Math.cos(alphaRad);
    const gdI = -U * Math.sin(alphaRad);

    const wR = gR + fr, wI = gI + fi;
    const velR = gdR + fdr, velI = gdI + fdi;
    const u = velR, v = -velI;
    const speed = Math.sqrt(u * u + v * v);
    const psi = wI;
    const cp = 1 - speed * speed / (U * U);

    return { wR, wI, psi, u, v, speed, cp };
  }

  // ---- Step 6: Boundary error ----
  let maxError = 0;
  const psi0 = evaluate(boundary[0]).psi;
  for (let j = 0; j < M; j++) {
    const err = Math.abs(evaluate(boundary[j]).psi - psi0);
    if (err > maxError) maxError = err;
  }

  const CL = -2 * Gamma / U;

  return {
    evaluate, boundary, poles, rungeCoeffs, newmanCoeffs,
    Gamma, CL, maxError, alpha, U, digits, nPoles, nRunge, zStar,
  };
}
