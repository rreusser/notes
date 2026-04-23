// Potential flow past a NACA airfoil using the lightning solver method.
// Reference: Baddoo (2020), "Lightning Solvers for Potential Flows".
//
// The complex potential is w(z) = g(z) + f(z) where:
//   g(z) = U·z·e^{-iα} + (Γ/2πi) log(z - z*)   (free stream + circulation)
//   f(z) = Runge part (smooth) + Newman part (corner singularity)
//
// Runge part: VA-orthogonalized polynomials in ζ = 1/(z - z*)
// Newman part: simple poles 1/(z - p_k) exponentially clustered at the TE.
// Boundary condition: Im[w] = 0 on the airfoil (streamline).
// Kutta condition: Im[w'(z_TE)] = 0 (smooth departure at trailing edge).

import { lssolve } from '../laplace/lssolve.js';
import { vaOrthog, vaEval, vaEvalBasis } from '../lib/va-orthog.js';
import { nacaAirfoil, trailingEdgeBisector } from './naca.js';

const cmul_r = (ar, ai, br, bi) => ar * br - ai * bi;
const cmul_i = (ar, ai, br, bi) => ar * bi + ai * br;

function placePoles(corner, angle, N, sigma, L) {
  const poles = [];
  const sqrtN = Math.sqrt(N);
  for (let j = 1; j <= N; j++) {
    const dist = L * Math.exp(-sigma * (sqrtN - Math.sqrt(j)));
    poles.push([corner[0] + dist * Math.cos(angle), corner[1] + dist * Math.sin(angle)]);
  }
  return poles;
}

export function potentialFlow(options = {}) {
  const {
    digits = '2412',
    nPoints = 400,
    nPoles = 0,
    nPoly = 20,
    sigma = 4,
    L = 4,
    alpha = 5,
    U = 1,
    kutta = true,
  } = options;

  const alphaRad = alpha * Math.PI / 180;
  const boundary = nacaAirfoil(digits, nPoints);
  const M = boundary.length;
  const te = boundary[0];

  // z* inside the airfoil (for Runge basis and circulation log).
  // Must be well inside the boundary to keep |1/(z-z*)| bounded on ∂D.
  // Use the camber line at 30% chord (thickest point of NACA 4-digit).
  const maxCamber = parseInt(digits[0]) / 100;
  const camberPos = parseInt(digits[1]) / 10 || 0.4;
  const xStar = 0.3;
  const yStar = maxCamber > 0
    ? (maxCamber / (camberPos * camberPos)) * (2 * camberPos * xStar - xStar * xStar)
    : 0;
  const zStar = [xStar, yStar];

  // Lightning poles at trailing edge
  const bisectorAngle = trailingEdgeBisector(boundary);
  const poles = placePoles(te, bisectorAngle, nPoles, sigma, L);

  // Transform boundary to ζ = 1/(z - z*) for VA orthogonalization
  const zetaBoundary = boundary.map(z => {
    const dx = z[0] - zStar[0], dy = z[1] - zStar[1];
    const d = dx * dx + dy * dy;
    return [dx / d, -dy / d];
  });
  const va = vaOrthog(zetaBoundary, nPoly);
  const { Q: polyQ, D: polyD } = vaEvalBasis(va, zetaBoundary);
  const nPolyBasis = nPoly + 1;

  // Build least-squares system.
  // Unknowns: Re(a_n), Im(a_n) for n=0..nPoly (Runge, 2*nPolyBasis values)
  //           Re(b_k), Im(b_k) for k=0..nPoles-1 (Newman, 2*nPoles values)
  //           Gamma (circulation, 1 value, if kutta)
  const nCols = 2 * nPolyBasis + 2 * nPoles + (kutta ? 1 : 0);
  const nRows = M + (kutta ? 1 : 0);
  const A = new Float64Array(nRows * nCols);
  const b = new Float64Array(nRows);

  for (let j = 0; j < M; j++) {
    const zj = boundary[j];
    b[j] = -U * (zj[1] * Math.cos(alphaRad) - zj[0] * Math.sin(alphaRad));

    // Runge basis: q_n(ζ_j) evaluated by VA
    for (let n = 0; n < nPolyBasis; n++) {
      const qR = polyQ[2 * (j + n * M)];
      const qI = polyQ[2 * (j + n * M) + 1];
      A[j + (2 * n) * nRows] = qI;         // Re(a_n) * Im(q_n)
      A[j + (2 * n + 1) * nRows] = qR;     // Im(a_n) * Re(q_n)
    }

    // Newman basis: 1/(z - p_k)
    for (let k = 0; k < nPoles; k++) {
      const dx = zj[0] - poles[k][0], dy = zj[1] - poles[k][1];
      const d = dx * dx + dy * dy;
      const col = 2 * nPolyBasis + 2 * k;
      A[j + col * nRows] = -dy / d;             // Im(1/(z-p)) = -dy/d
      A[j + (col + 1) * nRows] = dx / d;        // Re(1/(z-p)) = dx/d
    }

    // Gamma column
    if (kutta) {
      const dx = zj[0] - zStar[0], dy = zj[1] - zStar[1];
      const d = dx * dx + dy * dy;
      A[j + (nCols - 1) * nRows] = -0.5 * Math.log(d) / (2 * Math.PI);
    }
  }

  // Kutta condition row: Im[w'(z_TE)] = 0
  // Weight the Kutta row so it's as influential as the boundary rows.
  // sqrt(M) gives it weight proportional to the number of boundary rows.
  const kuttaWeight = M;
  if (kutta) {
    const row = M;
    b[row] = kuttaWeight * U * Math.sin(alphaRad);

    // Runge derivative: d/dz [q_n(ζ(z))] = q_n'(ζ) · dζ/dz
    // dζ/dz = -1/(z-z*)² = -ζ²
    const dxs = te[0] - zStar[0], dys = te[1] - zStar[1];
    const ds = dxs * dxs + dys * dys;
    const zetaR = dxs / ds, zetaI = -dys / ds;
    const dzdR = -(zetaR * zetaR - zetaI * zetaI);
    const dzdI = -2 * zetaR * zetaI;

    const { dq } = vaEval(va, [zetaR, zetaI]);
    for (let n = 0; n < nPolyBasis; n++) {
      const dpR = dq[2 * n], dpI = dq[2 * n + 1];
      const dphiR = cmul_r(dpR, dpI, dzdR, dzdI);
      const dphiI = cmul_i(dpR, dpI, dzdR, dzdI);
      A[row + (2 * n) * nRows] = kuttaWeight * dphiI;
      A[row + (2 * n + 1) * nRows] = kuttaWeight * dphiR;
    }

    // Newman derivative: -1/(z-p_k)²
    for (let k = 0; k < nPoles; k++) {
      const dx = te[0] - poles[k][0], dy = te[1] - poles[k][1];
      const d = dx * dx + dy * dy;
      const d2 = d * d;
      const dpR = -(dx * dx - dy * dy) / d2;
      const dpI = 2 * dx * dy / d2;
      const col = 2 * nPolyBasis + 2 * k;
      A[row + col * nRows] = kuttaWeight * dpI;
      A[row + (col + 1) * nRows] = kuttaWeight * dpR;
    }

    // Gamma derivative
    A[row + (nCols - 1) * nRows] = kuttaWeight * (-dxs / (2 * Math.PI * ds));
  }

  // Column scaling, Tikhonov regularization, and solve.
  // Regularization prevents Newman coefficients from blowing up by
  // augmenting the system: [A; λI] x ≈ [b; 0].
  const lambda = options.lambda || 1e-10;

  const colNorms = new Float64Array(nCols);
  const activeCols = [];
  for (let c = 0; c < nCols; c++) {
    let norm2 = 0;
    for (let r = 0; r < nRows; r++) norm2 += A[r + c * nRows] ** 2;
    colNorms[c] = Math.sqrt(norm2);
    if (colNorms[c] > 1e-14) activeCols.push(c);
  }
  const nActive = activeCols.length;
  const augRows = nRows + nActive; // extra rows for regularization
  const Ar = new Float64Array(augRows * nActive);
  const br = new Float64Array(augRows);
  const colScale = new Float64Array(nActive);
  for (let j = 0; j < nActive; j++) {
    colScale[j] = 1 / colNorms[activeCols[j]];
    for (let r = 0; r < nRows; r++) Ar[r + j * augRows] = A[r + activeCols[j] * nRows] * colScale[j];
    // Tikhonov row
    Ar[nRows + j + j * augRows] = lambda;
  }
  for (let r = 0; r < nRows; r++) br[r] = b[r];

  const xr = lssolve(Ar, br, augRows, nActive);
  const allCoeffs = new Float64Array(nCols);
  for (let j = 0; j < nActive; j++) allCoeffs[activeCols[j]] = xr[j] * colScale[j];

  const polyCoeffs = [];
  for (let n = 0; n < nPolyBasis; n++) polyCoeffs.push([allCoeffs[2*n], allCoeffs[2*n+1]]);
  const newmanCoeffs = [];
  for (let k = 0; k < nPoles; k++) {
    const col = 2 * nPolyBasis + 2 * k;
    newmanCoeffs.push([allCoeffs[col], allCoeffs[col + 1]]);
  }
  const Gamma = kutta ? allCoeffs[nCols - 1] : 0;

  // Evaluator
  function evaluate(z) {
    const zr = z[0], zi = z[1];
    let fr = 0, fi = 0, fdr = 0, fdi = 0;

    // Runge part via VA in ζ-space
    const dxs = zr - zStar[0], dys = zi - zStar[1];
    const ds = dxs * dxs + dys * dys;
    const zetaR = dxs / ds, zetaI = -dys / ds;
    const dzdR = -(zetaR * zetaR - zetaI * zetaI);
    const dzdI = -2 * zetaR * zetaI;

    const { q, dq } = vaEval(va, [zetaR, zetaI]);
    for (let n = 0; n < nPolyBasis; n++) {
      const cr = polyCoeffs[n][0], ci = polyCoeffs[n][1];
      fr += cr * q[2*n] - ci * q[2*n+1];
      fi += cr * q[2*n+1] + ci * q[2*n];
      const dpR = dq[2*n], dpI = dq[2*n+1];
      const chainR = cmul_r(dpR, dpI, dzdR, dzdI);
      const chainI = cmul_i(dpR, dpI, dzdR, dzdI);
      fdr += cr * chainR - ci * chainI;
      fdi += cr * chainI + ci * chainR;
    }

    // Newman part
    for (let k = 0; k < nPoles; k++) {
      const dx = zr - poles[k][0], dy = zi - poles[k][1];
      const d = dx * dx + dy * dy;
      const cr = newmanCoeffs[k][0], ci = newmanCoeffs[k][1];
      fr += cr * dx/d - ci * (-dy/d);
      fi += cr * (-dy/d) + ci * dx/d;
      const d2 = d * d;
      const dpR = -(dx*dx-dy*dy)/d2, dpI = 2*dx*dy/d2;
      fdr += cr * dpR - ci * dpI;
      fdi += cr * dpI + ci * dpR;
    }

    // Circulation
    const logR = 0.5 * Math.log(ds);
    const logTheta = Math.atan2(dys, dxs);
    fr += Gamma / (2*Math.PI) * logTheta;
    fi += -Gamma / (2*Math.PI) * logR;
    fdr += -Gamma / (2*Math.PI) * dys / ds;
    fdi += -Gamma / (2*Math.PI) * dxs / ds;

    // Free stream
    const gR = U * (zr * Math.cos(alphaRad) + zi * Math.sin(alphaRad));
    const gI = U * (zi * Math.cos(alphaRad) - zr * Math.sin(alphaRad));
    const u = U * Math.cos(alphaRad) + fdr;
    const v = -((-U * Math.sin(alphaRad)) + fdi);
    const speed = Math.sqrt(u * u + v * v);
    const psi = gI + fi;
    const cp = 1 - speed * speed / (U * U);

    return { wR: gR + fr, wI: psi, psi, u, v, speed, cp };
  }

  // Boundary error
  let maxError = 0;
  const psi0 = evaluate(boundary[0]).psi;
  for (let j = 0; j < M; j++) {
    const err = Math.abs(evaluate(boundary[j]).psi - psi0);
    if (err > maxError) maxError = err;
  }

  const CL = -2 * Gamma / U;

  return {
    evaluate, boundary, poles, polyCoeffs, newmanCoeffs,
    Gamma, CL, maxError, alpha, U, digits, nPoles, nPoly, zStar, va,
  };
}
