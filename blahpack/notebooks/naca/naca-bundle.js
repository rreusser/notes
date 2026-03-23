// examples/laplace/lssolve.js
function lssolve(A, b, m, n) {
  for (let k = 0; k < n; k++) {
    let norm = 0;
    for (let i = k; i < m; i++) norm += A[i + k * m] * A[i + k * m];
    norm = Math.sqrt(norm);
    if (norm === 0) continue;
    const alpha = A[k + k * m] >= 0 ? -norm : norm;
    A[k + k * m] -= alpha;
    let vnorm2 = 0;
    for (let i = k; i < m; i++) vnorm2 += A[i + k * m] * A[i + k * m];
    if (vnorm2 === 0) continue;
    const scale = 2 / vnorm2;
    for (let j = k + 1; j < n; j++) {
      let dot = 0;
      for (let i = k; i < m; i++) dot += A[i + k * m] * A[i + j * m];
      dot *= scale;
      for (let i = k; i < m; i++) A[i + j * m] -= dot * A[i + k * m];
    }
    let dotb = 0;
    for (let i = k; i < m; i++) dotb += A[i + k * m] * b[i];
    dotb *= scale;
    for (let i = k; i < m; i++) b[i] -= dotb * A[i + k * m];
    A[k + k * m] = alpha;
  }
  const x = new Float64Array(n);
  for (let k = n - 1; k >= 0; k--) {
    let s = b[k];
    for (let j = k + 1; j < n; j++) s -= A[k + j * m] * x[j];
    x[k] = s / A[k + k * m];
  }
  return x;
}

// examples/lib/va-orthog.js
function cmul_r(ar, ai, br, bi) {
  return ar * br - ai * bi;
}
function cmul_i(ar, ai, br, bi) {
  return ar * bi + ai * br;
}
function vaOrthog(Z, n) {
  const M = Z.length;
  const invM = 1 / M;
  const Q = new Float64Array(2 * M * (n + 1));
  const H = new Float64Array(2 * (n + 1) * n);
  for (let i = 0; i < M; i++) {
    Q[2 * (i + 0 * M)] = 1;
    Q[2 * (i + 0 * M) + 1] = 0;
  }
  for (let k = 0; k < n; k++) {
    const colK = k * M;
    const colK1 = (k + 1) * M;
    for (let i = 0; i < M; i++) {
      const qr = Q[2 * (i + colK)], qi = Q[2 * (i + colK) + 1];
      const zr = Z[i][0], zi = Z[i][1];
      Q[2 * (i + colK1)] = cmul_r(zr, zi, qr, qi);
      Q[2 * (i + colK1) + 1] = cmul_i(zr, zi, qr, qi);
    }
    for (let j = 0; j <= k; j++) {
      let hr = 0, hi = 0;
      const colJ = j * M;
      for (let i = 0; i < M; i++) {
        const qjr = Q[2 * (i + colJ)], qji = Q[2 * (i + colJ) + 1];
        const qr = Q[2 * (i + colK1)], qi = Q[2 * (i + colK1) + 1];
        hr += qjr * qr + qji * qi;
        hi += qjr * qi - qji * qr;
      }
      hr *= invM;
      hi *= invM;
      H[2 * (j + k * (n + 1))] = hr;
      H[2 * (j + k * (n + 1)) + 1] = hi;
      for (let i = 0; i < M; i++) {
        const qjr = Q[2 * (i + colJ)], qji = Q[2 * (i + colJ) + 1];
        Q[2 * (i + colK1)] -= cmul_r(hr, hi, qjr, qji);
        Q[2 * (i + colK1) + 1] -= cmul_i(hr, hi, qjr, qji);
      }
    }
    let norm2 = 0;
    for (let i = 0; i < M; i++) {
      const qr = Q[2 * (i + colK1)], qi = Q[2 * (i + colK1) + 1];
      norm2 += qr * qr + qi * qi;
    }
    const hkk = Math.sqrt(norm2 * invM);
    H[2 * (k + 1 + k * (n + 1))] = hkk;
    H[2 * (k + 1 + k * (n + 1)) + 1] = 0;
    if (hkk > 0) {
      const inv = 1 / hkk;
      for (let i = 0; i < M; i++) {
        Q[2 * (i + colK1)] *= inv;
        Q[2 * (i + colK1) + 1] *= inv;
      }
    }
  }
  return { H, n, Q, M };
}
function vaEval(va, z) {
  const { H, n } = va;
  const zr = z[0], zi = z[1];
  const stride = n + 1;
  const q = new Float64Array(2 * (n + 1));
  const dq = new Float64Array(2 * (n + 1));
  q[0] = 1;
  q[1] = 0;
  dq[0] = 0;
  dq[1] = 0;
  for (let k = 0; k < n; k++) {
    let nr = cmul_r(zr, zi, q[2 * k], q[2 * k + 1]);
    let ni = cmul_i(zr, zi, q[2 * k], q[2 * k + 1]);
    let dnr = cmul_r(zr, zi, dq[2 * k], dq[2 * k + 1]) + q[2 * k];
    let dni = cmul_i(zr, zi, dq[2 * k], dq[2 * k + 1]) + q[2 * k + 1];
    for (let j = 0; j <= k; j++) {
      const hjkr = H[2 * (j + k * stride)];
      const hjki = H[2 * (j + k * stride) + 1];
      nr -= cmul_r(hjkr, hjki, q[2 * j], q[2 * j + 1]);
      ni -= cmul_i(hjkr, hjki, q[2 * j], q[2 * j + 1]);
      dnr -= cmul_r(hjkr, hjki, dq[2 * j], dq[2 * j + 1]);
      dni -= cmul_i(hjkr, hjki, dq[2 * j], dq[2 * j + 1]);
    }
    const hkk = H[2 * (k + 1 + k * stride)];
    if (Math.abs(hkk) > 1e-300) {
      const inv = 1 / hkk;
      q[2 * (k + 1)] = nr * inv;
      q[2 * (k + 1) + 1] = ni * inv;
      dq[2 * (k + 1)] = dnr * inv;
      dq[2 * (k + 1) + 1] = dni * inv;
    }
  }
  return { q, dq };
}
function vaEvalBasis(va, Z) {
  const { H, n } = va;
  const M = Z.length;
  const stride = n + 1;
  const Q = new Float64Array(2 * M * (n + 1));
  const D = new Float64Array(2 * M * (n + 1));
  for (let i = 0; i < M; i++) {
    Q[2 * (i + 0 * M)] = 1;
  }
  for (let k = 0; k < n; k++) {
    const colK = k * M;
    const colK1 = (k + 1) * M;
    const hkk = H[2 * (k + 1 + k * stride)];
    const inv = Math.abs(hkk) > 1e-300 ? 1 / hkk : 0;
    for (let i = 0; i < M; i++) {
      const zr = Z[i][0], zi = Z[i][1];
      const qr = Q[2 * (i + colK)], qi = Q[2 * (i + colK) + 1];
      const dr = D[2 * (i + colK)], di = D[2 * (i + colK) + 1];
      let nr = cmul_r(zr, zi, qr, qi);
      let ni = cmul_i(zr, zi, qr, qi);
      let dnr = cmul_r(zr, zi, dr, di) + qr;
      let dni = cmul_i(zr, zi, dr, di) + qi;
      for (let j = 0; j <= k; j++) {
        const hjkr = H[2 * (j + k * stride)];
        const hjki = H[2 * (j + k * stride) + 1];
        nr -= cmul_r(hjkr, hjki, Q[2 * (i + j * M)], Q[2 * (i + j * M) + 1]);
        ni -= cmul_i(hjkr, hjki, Q[2 * (i + j * M)], Q[2 * (i + j * M) + 1]);
        dnr -= cmul_r(hjkr, hjki, D[2 * (i + j * M)], D[2 * (i + j * M) + 1]);
        dni -= cmul_i(hjkr, hjki, D[2 * (i + j * M)], D[2 * (i + j * M) + 1]);
      }
      Q[2 * (i + colK1)] = nr * inv;
      Q[2 * (i + colK1) + 1] = ni * inv;
      D[2 * (i + colK1)] = dnr * inv;
      D[2 * (i + colK1) + 1] = dni * inv;
    }
  }
  return { Q, D };
}

// examples/naca/naca.js
function thickness(x, t) {
  return 5 * t * (0.2969 * Math.sqrt(x) - 0.126 * x - 0.3516 * x * x + 0.2843 * x * x * x - 0.1015 * x * x * x * x);
}
function camber(x, m, p) {
  if (m === 0) return { yc: 0, dyc: 0 };
  if (x < p) {
    return {
      yc: m / (p * p) * (2 * p * x - x * x),
      dyc: 2 * m / (p * p) * (p - x)
    };
  }
  return {
    yc: m / ((1 - p) * (1 - p)) * (1 - 2 * p + 2 * p * x - x * x),
    dyc: 2 * m / ((1 - p) * (1 - p)) * (p - x)
  };
}
function nacaAirfoil(digits, nPoints = 200) {
  const M = parseInt(digits[0]) / 100;
  const P = parseInt(digits[1]) / 10;
  const T = parseInt(digits.slice(2)) / 100;
  const n = Math.floor(nPoints / 2);
  const upper = [];
  const lower = [];
  for (let i = 0; i <= n; i++) {
    const beta = Math.PI * i / n;
    const xc = 0.5 * (1 - Math.cos(beta));
    const yt = thickness(xc, T);
    const { yc, dyc } = camber(xc, M, P || 0.01);
    const theta = Math.atan(dyc);
    upper.push([xc - yt * Math.sin(theta), yc + yt * Math.cos(theta)]);
    lower.push([xc + yt * Math.sin(theta), yc - yt * Math.cos(theta)]);
  }
  const points = [];
  for (let i = n; i >= 0; i--) points.push(upper[i]);
  for (let i = 1; i < n; i++) points.push(lower[i]);
  return points;
}
function trailingEdgeBisector(points) {
  const n = points.length;
  const p0 = points[0];
  const p1 = points[1];
  const pn1 = points[n - 1];
  const pn2 = points[n - 2];
  const t1 = [p1[0] - p0[0], p1[1] - p0[1]];
  const t2 = [pn2[0] - pn1[0], pn2[1] - pn1[1]];
  const bx = -(t1[0] + t2[0]);
  const by = -(t1[1] + t2[1]);
  return Math.atan2(by, bx);
}

// examples/naca/potential-flow.js
var cmul_r2 = (ar, ai, br, bi) => ar * br - ai * bi;
var cmul_i2 = (ar, ai, br, bi) => ar * bi + ai * br;
function placePoles(corner, angle, N, sigma, L) {
  const poles = [];
  const sqrtN = Math.sqrt(N);
  for (let j = 1; j <= N; j++) {
    const dist = L * Math.exp(-sigma * (sqrtN - Math.sqrt(j)));
    poles.push([corner[0] + dist * Math.cos(angle), corner[1] + dist * Math.sin(angle)]);
  }
  return poles;
}
function potentialFlow(options = {}) {
  const {
    digits = "2412",
    nPoints = 400,
    nPoles = 0,
    nPoly = 20,
    sigma = 4,
    L = 4,
    alpha = 5,
    U = 1,
    kutta = true
  } = options;
  const alphaRad = alpha * Math.PI / 180;
  const boundary = nacaAirfoil(digits, nPoints);
  const M = boundary.length;
  const te = boundary[0];
  const maxCamber = parseInt(digits[0]) / 100;
  const camberPos = parseInt(digits[1]) / 10 || 0.4;
  const xStar = 0.3;
  const yStar = maxCamber > 0 ? maxCamber / (camberPos * camberPos) * (2 * camberPos * xStar - xStar * xStar) : 0;
  const zStar = [xStar, yStar];
  const bisectorAngle = trailingEdgeBisector(boundary);
  const poles = placePoles(te, bisectorAngle, nPoles, sigma, L);
  const zetaBoundary = boundary.map((z) => {
    const dx = z[0] - zStar[0], dy = z[1] - zStar[1];
    const d = dx * dx + dy * dy;
    return [dx / d, -dy / d];
  });
  const va = vaOrthog(zetaBoundary, nPoly);
  const { Q: polyQ, D: polyD } = vaEvalBasis(va, zetaBoundary);
  const nPolyBasis = nPoly + 1;
  const nCols = 2 * nPolyBasis + 2 * nPoles + (kutta ? 1 : 0);
  const nRows = M + (kutta ? 1 : 0);
  const A = new Float64Array(nRows * nCols);
  const b = new Float64Array(nRows);
  for (let j = 0; j < M; j++) {
    const zj = boundary[j];
    b[j] = -U * (zj[1] * Math.cos(alphaRad) - zj[0] * Math.sin(alphaRad));
    for (let n = 0; n < nPolyBasis; n++) {
      const qR = polyQ[2 * (j + n * M)];
      const qI = polyQ[2 * (j + n * M) + 1];
      A[j + 2 * n * nRows] = qI;
      A[j + (2 * n + 1) * nRows] = qR;
    }
    for (let k = 0; k < nPoles; k++) {
      const dx = zj[0] - poles[k][0], dy = zj[1] - poles[k][1];
      const d = dx * dx + dy * dy;
      const col = 2 * nPolyBasis + 2 * k;
      A[j + col * nRows] = -dy / d;
      A[j + (col + 1) * nRows] = dx / d;
    }
    if (kutta) {
      const dx = zj[0] - zStar[0], dy = zj[1] - zStar[1];
      const d = dx * dx + dy * dy;
      A[j + (nCols - 1) * nRows] = -0.5 * Math.log(d) / (2 * Math.PI);
    }
  }
  const kuttaWeight = M;
  if (kutta) {
    const row = M;
    b[row] = kuttaWeight * U * Math.sin(alphaRad);
    const dxs = te[0] - zStar[0], dys = te[1] - zStar[1];
    const ds = dxs * dxs + dys * dys;
    const zetaR = dxs / ds, zetaI = -dys / ds;
    const dzdR = -(zetaR * zetaR - zetaI * zetaI);
    const dzdI = -2 * zetaR * zetaI;
    const { dq } = vaEval(va, [zetaR, zetaI]);
    for (let n = 0; n < nPolyBasis; n++) {
      const dpR = dq[2 * n], dpI = dq[2 * n + 1];
      const dphiR = cmul_r2(dpR, dpI, dzdR, dzdI);
      const dphiI = cmul_i2(dpR, dpI, dzdR, dzdI);
      A[row + 2 * n * nRows] = kuttaWeight * dphiI;
      A[row + (2 * n + 1) * nRows] = kuttaWeight * dphiR;
    }
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
    A[row + (nCols - 1) * nRows] = kuttaWeight * (-dxs / (2 * Math.PI * ds));
  }
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
  const augRows = nRows + nActive;
  const Ar = new Float64Array(augRows * nActive);
  const br = new Float64Array(augRows);
  const colScale = new Float64Array(nActive);
  for (let j = 0; j < nActive; j++) {
    colScale[j] = 1 / colNorms[activeCols[j]];
    for (let r = 0; r < nRows; r++) Ar[r + j * augRows] = A[r + activeCols[j] * nRows] * colScale[j];
    Ar[nRows + j + j * augRows] = lambda;
  }
  for (let r = 0; r < nRows; r++) br[r] = b[r];
  const xr = lssolve(Ar, br, augRows, nActive);
  const allCoeffs = new Float64Array(nCols);
  for (let j = 0; j < nActive; j++) allCoeffs[activeCols[j]] = xr[j] * colScale[j];
  const polyCoeffs = [];
  for (let n = 0; n < nPolyBasis; n++) polyCoeffs.push([allCoeffs[2 * n], allCoeffs[2 * n + 1]]);
  const newmanCoeffs = [];
  for (let k = 0; k < nPoles; k++) {
    const col = 2 * nPolyBasis + 2 * k;
    newmanCoeffs.push([allCoeffs[col], allCoeffs[col + 1]]);
  }
  const Gamma = kutta ? allCoeffs[nCols - 1] : 0;
  function evaluate(z) {
    const zr = z[0], zi = z[1];
    let fr = 0, fi = 0, fdr = 0, fdi = 0;
    const dxs = zr - zStar[0], dys = zi - zStar[1];
    const ds = dxs * dxs + dys * dys;
    const zetaR = dxs / ds, zetaI = -dys / ds;
    const dzdR = -(zetaR * zetaR - zetaI * zetaI);
    const dzdI = -2 * zetaR * zetaI;
    const { q, dq } = vaEval(va, [zetaR, zetaI]);
    for (let n = 0; n < nPolyBasis; n++) {
      const cr = polyCoeffs[n][0], ci = polyCoeffs[n][1];
      fr += cr * q[2 * n] - ci * q[2 * n + 1];
      fi += cr * q[2 * n + 1] + ci * q[2 * n];
      const dpR = dq[2 * n], dpI = dq[2 * n + 1];
      const chainR = cmul_r2(dpR, dpI, dzdR, dzdI);
      const chainI = cmul_i2(dpR, dpI, dzdR, dzdI);
      fdr += cr * chainR - ci * chainI;
      fdi += cr * chainI + ci * chainR;
    }
    for (let k = 0; k < nPoles; k++) {
      const dx = zr - poles[k][0], dy = zi - poles[k][1];
      const d = dx * dx + dy * dy;
      const cr = newmanCoeffs[k][0], ci = newmanCoeffs[k][1];
      fr += cr * dx / d - ci * (-dy / d);
      fi += cr * (-dy / d) + ci * dx / d;
      const d2 = d * d;
      const dpR = -(dx * dx - dy * dy) / d2, dpI = 2 * dx * dy / d2;
      fdr += cr * dpR - ci * dpI;
      fdi += cr * dpI + ci * dpR;
    }
    const logR = 0.5 * Math.log(ds);
    const logTheta = Math.atan2(dys, dxs);
    fr += Gamma / (2 * Math.PI) * logTheta;
    fi += -Gamma / (2 * Math.PI) * logR;
    fdr += -Gamma / (2 * Math.PI) * dys / ds;
    fdi += -Gamma / (2 * Math.PI) * dxs / ds;
    const gR = U * (zr * Math.cos(alphaRad) + zi * Math.sin(alphaRad));
    const gI = U * (zi * Math.cos(alphaRad) - zr * Math.sin(alphaRad));
    const u = U * Math.cos(alphaRad) + fdr;
    const v = -(-U * Math.sin(alphaRad) + fdi);
    const speed = Math.sqrt(u * u + v * v);
    const psi = gI + fi;
    const cp = 1 - speed * speed / (U * U);
    return { wR: gR + fr, wI: psi, psi, u, v, speed, cp };
  }
  let maxError = 0;
  const psi0 = evaluate(boundary[0]).psi;
  for (let j = 0; j < M; j++) {
    const err = Math.abs(evaluate(boundary[j]).psi - psi0);
    if (err > maxError) maxError = err;
  }
  const CL = -2 * Gamma / U;
  return {
    evaluate,
    boundary,
    poles,
    polyCoeffs,
    newmanCoeffs,
    Gamma,
    CL,
    maxError,
    alpha,
    U,
    digits,
    nPoles,
    nPoly,
    zStar,
    va
  };
}

// examples/naca/square-flow.js
function placePoles2(corner, angle, N, sigma, L) {
  const poles = [];
  const sqrtN = Math.sqrt(N);
  for (let j = 1; j <= N; j++) {
    const dist = L * Math.exp(-sigma * (sqrtN - Math.sqrt(j)));
    poles.push([corner[0] + dist * Math.cos(angle), corner[1] + dist * Math.sin(angle)]);
  }
  return poles;
}
function squareFlow(options = {}) {
  const {
    N = 24,
    // poles per corner
    nPoly = 20,
    // VA polynomial degree
    nSample = 200,
    // boundary points per side
    sigma = 4,
    L = 0.5,
    // max pole distance from corner (must stay inside body)
    halfSide = 0.5
    // half-side length of square
  } = options;
  const s = halfSide;
  const corners = [[-s, -s], [s, -s], [s, s], [-s, s]];
  const bisectors = [Math.PI / 4, 3 * Math.PI / 4, -3 * Math.PI / 4, -Math.PI / 4];
  const allPoles = [];
  for (let k = 0; k < 4; k++) {
    allPoles.push(...placePoles2(corners[k], bisectors[k], N, sigma, L));
  }
  const nPoles = allPoles.length;
  const boundary = [];
  const sides = [
    [corners[0], corners[1]],
    [corners[1], corners[2]],
    [corners[2], corners[3]],
    [corners[3], corners[0]]
  ];
  for (const [start, end] of sides) {
    for (let i = 1; i <= nSample; i++) {
      const t = i / (nSample + 1);
      boundary.push([
        start[0] + t * (end[0] - start[0]),
        start[1] + t * (end[1] - start[1])
      ]);
    }
  }
  const M = boundary.length;
  const zStar = [0, 0];
  const zetaBdy = boundary.map((z) => {
    const dx = z[0] - zStar[0], dy = z[1] - zStar[1];
    const d = dx * dx + dy * dy;
    return [dx / d, -dy / d];
  });
  const va = vaOrthog(zetaBdy, nPoly);
  const { Q: polyQ } = vaEvalBasis(va, zetaBdy);
  const nPolyBasis = nPoly + 1;
  const nCols = 2 * nPolyBasis + 2 * nPoles;
  const A = new Float64Array(M * nCols);
  const b = new Float64Array(M);
  for (let j = 0; j < M; j++) {
    b[j] = -boundary[j][1];
    for (let n = 0; n < nPolyBasis; n++) {
      const qR = polyQ[2 * (j + n * M)];
      const qI = polyQ[2 * (j + n * M) + 1];
      A[j + 2 * n * M] = qI;
      A[j + (2 * n + 1) * M] = qR;
    }
    for (let k = 0; k < nPoles; k++) {
      const dx = boundary[j][0] - allPoles[k][0];
      const dy = boundary[j][1] - allPoles[k][1];
      const d = dx * dx + dy * dy;
      const col = 2 * nPolyBasis + 2 * k;
      A[j + col * M] = -dy / d;
      A[j + (col + 1) * M] = dx / d;
    }
  }
  const activeCols = [], colScales = [];
  for (let c = 0; c < nCols; c++) {
    let norm = 0;
    for (let r = 0; r < M; r++) norm += A[r + c * M] ** 2;
    norm = Math.sqrt(norm);
    if (norm > 1e-14) {
      activeCols.push(c);
      colScales.push(norm);
    }
  }
  const nActive = activeCols.length;
  const Ar = new Float64Array(M * nActive);
  for (let j = 0; j < nActive; j++) {
    for (let r = 0; r < M; r++) Ar[r + j * M] = A[r + activeCols[j] * M] / colScales[j];
  }
  const xr = lssolve(Ar, new Float64Array(b), M, nActive);
  const allCoeffs = new Float64Array(nCols);
  for (let j = 0; j < nActive; j++) allCoeffs[activeCols[j]] = xr[j] / colScales[j];
  const polyCoeffs = [];
  for (let n = 0; n < nPolyBasis; n++) polyCoeffs.push([allCoeffs[2 * n], allCoeffs[2 * n + 1]]);
  const poleCoeffs = [];
  for (let k = 0; k < nPoles; k++) {
    const col = 2 * nPolyBasis + 2 * k;
    poleCoeffs.push([allCoeffs[col], allCoeffs[col + 1]]);
  }
  function evaluate(z) {
    const zr = z[0], zi = z[1];
    let fr = 0, fi = 0;
    const dx = zr - zStar[0], dy = zi - zStar[1];
    const d = dx * dx + dy * dy;
    const zetaR = dx / d, zetaI = -dy / d;
    const { q } = vaEval(va, [zetaR, zetaI]);
    for (let n = 0; n < nPolyBasis; n++) {
      const cr = polyCoeffs[n][0], ci = polyCoeffs[n][1];
      fr += cr * q[2 * n] - ci * q[2 * n + 1];
      fi += cr * q[2 * n + 1] + ci * q[2 * n];
    }
    for (let k = 0; k < nPoles; k++) {
      const px = zr - allPoles[k][0], py = zi - allPoles[k][1];
      const pd = px * px + py * py;
      const cr = poleCoeffs[k][0], ci = poleCoeffs[k][1];
      fr += cr * px / pd + ci * py / pd;
      fi += -cr * py / pd + ci * px / pd;
    }
    const wR = zr + fr;
    const wI = zi + fi;
    return { psi: wI, wR, fr, fi };
  }
  let maxError = 0;
  for (let j = 0; j < M; j++) {
    const { psi } = evaluate(boundary[j]);
    const err = Math.abs(psi);
    if (err > maxError) maxError = err;
  }
  return { evaluate, boundary, corners, allPoles, polyCoeffs, poleCoeffs, maxError, va, zStar, nPoles, nPoly };
}
export {
  nacaAirfoil,
  potentialFlow,
  squareFlow
};
