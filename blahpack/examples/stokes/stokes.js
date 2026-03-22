// 2D Stokes flow solver via Goursat functions and lightning rational approximation.
// Reference: Xue, Waters, Trefethen, "Computation of 2D Stokes flows via lightning
// and AAA rational approximation", arXiv:2306.13545v2, 2023.
//
// Solves the biharmonic equation ∇⁴ψ = 0 using:
//   ψ = Im[conj(z) f(z) + g(z)]
//   u - iv = -conj(f(z)) + conj(z) f'(z) + g'(z)
//   p/μ - iω = 4 f'(z)

import { lssolve } from '../laplace/lssolve.js';

// ---- Complex arithmetic ----
const cadd = (a, b) => [a[0] + b[0], a[1] + b[1]];
const csub = (a, b) => [a[0] - b[0], a[1] - b[1]];
const cmul = (a, b) => [a[0]*b[0] - a[1]*b[1], a[0]*b[1] + a[1]*b[0]];
const cconj = (a) => [a[0], -a[1]];
const cdiv = (a, b) => {
  const d = b[0]*b[0] + b[1]*b[1];
  return [(a[0]*b[0] + a[1]*b[1]) / d, (a[1]*b[0] - a[0]*b[1]) / d];
};

// ---- Lightning pole placement ----
// Places N poles along the exterior bisector of a corner.
// β_kn = w + L * exp(iθ) * exp(-σ(√N - √n)), n = 1..N
function placeLightningPoles(corner, angle, N, sigma, L) {
  const poles = [];
  const sqrtN = Math.sqrt(N);
  for (let n = 1; n <= N; n++) {
    const t = L * Math.exp(-sigma * (sqrtN - Math.sqrt(n)));
    poles.push([
      corner[0] + t * Math.cos(angle),
      corner[1] + t * Math.sin(angle),
    ]);
  }
  return poles;
}

// ---- Evaluate rational + polynomial representation ----
// r(z) = Σ c_k / (z - β_k) + Σ a_n z^n
// r'(z) = Σ -c_k / (z - β_k)² + Σ n a_n z^(n-1)
function evalRational(z, poles, ratCoeffs, polyCoeffs) {
  let val = [0, 0], deriv = [0, 0];

  // Rational part
  for (let k = 0; k < poles.length; k++) {
    const dz = csub(z, poles[k]);
    const inv = cdiv([1, 0], dz);
    const inv2 = cmul(inv, inv);
    val = cadd(val, cmul(ratCoeffs[k], inv));
    deriv = csub(deriv, cmul(ratCoeffs[k], inv2));
  }

  // Polynomial part
  let power = [1, 0];
  for (let n = 0; n < polyCoeffs.length; n++) {
    val = cadd(val, cmul(polyCoeffs[n], power));
    if (n >= 1) {
      // derivative: n * a_n * z^(n-1)
      let dpow = [1, 0];
      for (let j = 0; j < n - 1; j++) dpow = cmul(dpow, z);
      deriv = cadd(deriv, cmul([n * polyCoeffs[n][0], n * polyCoeffs[n][1]], dpow));
    }
    power = cmul(power, z);
  }

  return { val, deriv };
}

// ---- Main Stokes solver ----
export function stokes(options = {}) {
  const {
    N = 24,           // lightning poles per corner
    nPoly = 20,       // polynomial degree
    nSample = 100,    // sample points per side
    sigma = 4,        // clustering parameter
    L = 2,            // pole distance scale
    // Lid-driven cavity on [-1,1]^2 by default
    corners = [[-1,-1], [1,-1], [1,1], [-1,1]],
    // Exterior bisector angles for each corner (outward at 45°)
    bisectors = [-3*Math.PI/4, -Math.PI/4, Math.PI/4, 3*Math.PI/4],
    // Boundary condition: returns [u, v] at point [x, y]
    velocityBC = ([x, y]) => {
      // Lid-driven cavity: u=1 on top, 0 elsewhere
      if (Math.abs(y - 1) < 1e-10) return [1, 0];
      return [0, 0];
    },
    // Boundary segments: arrays of [x, y] points
    boundarySegments = null,
  } = options;

  // ---- Step 1: Place lightning poles ----
  const allPoles = [];
  for (let k = 0; k < corners.length; k++) {
    const cornerPoles = placeLightningPoles(corners[k], bisectors[k], N, sigma, L);
    allPoles.push(...cornerPoles);
  }
  const nPoles = allPoles.length;

  // ---- Step 2: Build boundary sample points ----
  let boundary;
  if (boundarySegments) {
    boundary = boundarySegments.flat();
  } else {
    // Default: square [-1,1]^2 with tanh-clustered points near corners
    boundary = [];
    const sides = [
      [[-1,-1], [1,-1]],  // bottom
      [[1,-1], [1,1]],    // right
      [[1,1], [-1,1]],    // top
      [[-1,1], [-1,-1]],  // left
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
  }

  const M = boundary.length;
  const nBasisF = nPoles + nPoly + 1; // basis functions for f (rat + poly 0..nPoly)
  const nBasisG = nPoles + nPoly;     // basis functions for g (rat + poly 1..nPoly, no constant)
  // g's constant term is excluded: g appears only through g'(z) in the velocity BC,
  // and the derivative of a constant is zero. This is a gauge freedom (ψ is defined
  // up to a constant).
  const nCols = 2 * nBasisF + 2 * nBasisG; // real+imag for f and g
  const nRows = 2 * M; // u and v for each point

  // ---- Step 3: Build least-squares system ----
  const A = new Float64Array(nRows * nCols);
  const b = new Float64Array(nRows);

  for (let j = 0; j < M; j++) {
    const z = boundary[j];
    const x = z[0], y = z[1];
    const [u_bc, v_bc] = velocityBC(z);
    b[j] = u_bc;
    b[M + j] = v_bc;

    // For each basis function, compute phi(z) and phi'(z), then
    // fill the matrix columns based on the velocity formula:
    //   u - iv = -conj(f) + conj(z)*f' + g'
    //   u = Re[-conj(f) + conj(z)*f' + g']
    //   v = -Im[-conj(f) + conj(z)*f' + g']

    for (let k = 0; k < nBasisF + nBasisG; k++) {
      const isG = k >= nBasisF;
      const kk = isG ? k - nBasisF : k;

      // Evaluate basis function and derivative
      let pr, pi, dpr, dpi;
      if (kk < nPoles) {
        // Rational basis: 1/(z - β_k)
        const dz = [z[0] - allPoles[kk][0], z[1] - allPoles[kk][1]];
        const d = dz[0]*dz[0] + dz[1]*dz[1];
        pr = dz[0] / d;
        pi = -dz[1] / d;
        // Derivative: -1/(z - β_k)²
        const d2 = d * d;
        const dz2r = dz[0]*dz[0] - dz[1]*dz[1];
        const dz2i = 2*dz[0]*dz[1];
        dpr = -dz2r / d2;
        dpi = dz2i / d2;
      } else {
        // Polynomial basis: z^n
        // For g, skip constant (n starts at 1): n = kk - nPoles + (isG ? 1 : 0)
        const n = kk - nPoles + (isG ? 1 : 0);
        let powr = 1, powi = 0;
        for (let i = 0; i < n; i++) {
          const nr = powr * z[0] - powi * z[1];
          const ni = powr * z[1] + powi * z[0];
          powr = nr; powi = ni;
        }
        pr = powr; pi = powi;
        // Derivative: n * z^(n-1)
        if (n === 0) {
          dpr = 0; dpi = 0;
        } else {
          let dpowr = 1, dpowi = 0;
          for (let i = 0; i < n - 1; i++) {
            const nr = dpowr * z[0] - dpowi * z[1];
            const ni = dpowr * z[1] + dpowi * z[0];
            dpowr = nr; dpowi = ni;
          }
          dpr = n * dpowr; dpi = n * dpowi;
        }
      }

      // Column indices: [f_re, f_im, g_re, g_im]
      // f coefficients: columns 0..2*nBasisF-1
      // g coefficients: columns 2*nBasisF..nCols-1
      const colRe = isG ? 2*nBasisF + 2*kk : 2*kk;
      const colIm = colRe + 1;

      if (isG) {
        // g contributes g'(z) to velocity: u - iv = ... + g'(z)
        // u from g_re coeff: Re(phi')  = dpr
        // u from g_im coeff: -Im(phi') = -dpi
        // v from g_re coeff: -Im(phi') = -dpi
        // v from g_im coeff: -Re(phi') = -dpr
        A[j + colRe * nRows] = dpr;
        A[j + colIm * nRows] = -dpi;
        A[M + j + colRe * nRows] = -dpi;
        A[M + j + colIm * nRows] = -dpr;
      } else {
        // f contributes -conj(f) + conj(z)*f' to velocity
        // -conj(c * phi) where c = cr + i*ci:
        //   = -(cr*pr - ci*pi) + i*(cr*pi + ci*pr)
        // conj(z) * c * phi' where conj(z) = x - iy:
        //   Real: x*(cr*dpr - ci*dpi) + y*(cr*dpi + ci*dpr)
        //   Imag: x*(cr*dpi + ci*dpr) - y*(cr*dpr - ci*dpi)

        // u = Re[-conj(f)] + Re[conj(z)*f']
        // u from cr: -pr + x*dpr + y*dpi
        // u from ci:  pi - x*dpi + y*dpr
        A[j + colRe * nRows] = -pr + x*dpr + y*dpi;
        A[j + colIm * nRows] = pi - x*dpi + y*dpr;

        // v = -Im[-conj(f) + conj(z)*f']
        // -v = Im[-conj(f)] + Im[conj(z)*f']
        // -v from cr: pi + x*dpi - y*dpr
        // -v from ci: pr + x*dpr + y*dpi  -- wait, need to be careful
        // v = -Im[...] so:
        // v from cr: -(pi + x*dpi - y*dpr)
        // v from ci: -(pr + x*dpr + y*dpi)  -- hmm, let me rederive

        // Im[-conj(c*phi)] = Im[-(cr*pr-ci*pi) + i*(cr*pi+ci*pr)]
        //   = cr*pi + ci*pr
        // Im[conj(z)*c*phi'] = x*(cr*dpi+ci*dpr) - y*(cr*dpr-ci*dpi)
        // So Im[total] from cr: pi + x*dpi - y*dpr
        //    Im[total] from ci: pr + x*dpr + y*dpi
        // v = -Im[total]
        A[M + j + colRe * nRows] = -(pi + x*dpi - y*dpr);
        A[M + j + colIm * nRows] = -(pr + x*dpr + y*dpi);
      }
    }
  }

  // ---- Step 4: Remove zero columns (gauge freedoms) and solve ----
  // Known gauge freedom: Re(f's z^1 coeff) produces a zero column because
  // -conj(c*z) + z̄*c*1 = 0 for real c. Detect and remove all zero columns.
  const colNorms = new Float64Array(nCols);
  for (let c = 0; c < nCols; c++) {
    let norm2 = 0;
    for (let r = 0; r < nRows; r++) norm2 += A[r + c * nRows] ** 2;
    colNorms[c] = norm2;
  }
  const activeCols = [];
  for (let c = 0; c < nCols; c++) {
    if (colNorms[c] > 1e-30) activeCols.push(c);
  }
  const nActive = activeCols.length;

  // Build reduced system
  const Ar = new Float64Array(nRows * nActive);
  for (let j = 0; j < nActive; j++) {
    const c = activeCols[j];
    for (let r = 0; r < nRows; r++) Ar[r + j * nRows] = A[r + c * nRows];
  }
  const br = new Float64Array(b);
  const xr = lssolve(Ar, br, nRows, nActive);

  // Map back to full coefficient vector
  const coeffs = new Float64Array(nCols);
  for (let j = 0; j < nActive; j++) coeffs[activeCols[j]] = xr[j];

  // ---- Step 5: Extract coefficients ----
  const fRatCoeffs = [], fPolyCoeffs = [];
  const gRatCoeffs = [], gPolyCoeffs = [];

  for (let k = 0; k < nBasisF; k++) {
    const cr = coeffs[2*k], ci = coeffs[2*k + 1];
    if (k < nPoles) fRatCoeffs.push([cr, ci]);
    else fPolyCoeffs.push([cr, ci]);
  }
  // g polynomial starts at degree 1 (constant excluded)
  gPolyCoeffs.push([0, 0]); // degree 0 = constant = 0 (gauge choice)
  for (let k = 0; k < nBasisG; k++) {
    const cr = coeffs[2*nBasisF + 2*k], ci = coeffs[2*nBasisF + 2*k + 1];
    if (k < nPoles) gRatCoeffs.push([cr, ci]);
    else gPolyCoeffs.push([cr, ci]); // these are degree 1, 2, ..., nPoly
  }

  // ---- Step 6: Build evaluator ----
  function evaluate(z) {
    const f = evalRational(z, allPoles, fRatCoeffs, fPolyCoeffs);
    const g = evalRational(z, allPoles, gRatCoeffs, gPolyCoeffs);

    // ψ = Im[conj(z) * f(z) + g(z)]
    const zbar = [z[0], -z[1]];
    const zbar_f = cmul(zbar, f.val);
    const psi = zbar_f[1] + g.val[1]; // Im[zbar*f + g]

    // u - iv = -conj(f) + conj(z)*f' + g'
    const neg_fbar = [-f.val[0], f.val[1]]; // -conj(f)
    const zbar_fp = cmul(zbar, f.deriv);     // conj(z)*f'
    const vel = cadd(cadd(neg_fbar, zbar_fp), g.deriv);
    const u = vel[0], v = -vel[1];

    // p/μ - iω = 4f'(z)
    const pressure = 4 * f.deriv[0];
    const vorticity = -4 * f.deriv[1];
    const speed = Math.sqrt(u*u + v*v);

    return { psi, u, v, pressure, vorticity, speed };
  }

  // ---- Step 7: Compute boundary error ----
  let maxError = 0;
  for (let j = 0; j < M; j++) {
    const { u, v } = evaluate(boundary[j]);
    const [u_bc, v_bc] = velocityBC(boundary[j]);
    const err = Math.hypot(u - u_bc, v - v_bc);
    if (err > maxError) maxError = err;
  }

  return {
    poles: allPoles,
    fRatCoeffs, fPolyCoeffs,
    gRatCoeffs, gPolyCoeffs,
    nPoles, nPoly,
    corners, boundary,
    maxError,
    evaluate,
  };
}
