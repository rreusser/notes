// QZ algorithm for complex generalized eigenvalue problems.
// Faithful JavaScript implementation based on LAPACK's ZGGHRD + ZHGEQZ.
// All complex arithmetic uses in-place gl-matrix-style operations (out parameter first).

// ---- Machine constants (IEEE 754 double precision) ----
const SAFMIN = Number.MIN_VALUE * (1 / Number.EPSILON);
const SAFMAX = 1 / SAFMIN;
const ULP = Number.EPSILON;
const BASE = 2;

// ---- Complex arithmetic (in-place, gl-matrix style) ----
// All functions write to `out` and return `out`. Safe when out aliases a or b.

function cadd(out, a, b) { out[0] = a[0] + b[0]; out[1] = a[1] + b[1]; return out; }
function csub(out, a, b) { out[0] = a[0] - b[0]; out[1] = a[1] - b[1]; return out; }
function cmul(out, a, b) {
  const ar = a[0], ai = a[1], br = b[0], bi = b[1];
  out[0] = ar * br - ai * bi;
  out[1] = ar * bi + ai * br;
  return out;
}
function cscale(out, s, a) { out[0] = s * a[0]; out[1] = s * a[1]; return out; }
function cconj(out, a) { out[0] = a[0]; out[1] = -a[1]; return out; }
function cneg(out, a) { out[0] = -a[0]; out[1] = -a[1]; return out; }
function cset(out, re, im) { out[0] = re; out[1] = im; return out; }
function ccopy(out, a) { out[0] = a[0]; out[1] = a[1]; return out; }
function cabs(a) { return Math.sqrt(a[0] * a[0] + a[1] * a[1]); }
function cabs1(a) { return Math.abs(a[0]) + Math.abs(a[1]); }

// Apply Givens rotation in-place: [a, b] → [c*a + s*b, c*b - conj(s)*a]
function grot(a, b, c, sr, si) {
  const ar = a[0], ai = a[1], br = b[0], bi = b[1];
  a[0] = c * ar + sr * br - si * bi;
  a[1] = c * ai + sr * bi + si * br;
  b[0] = c * br - sr * ar - si * ai;
  b[1] = c * bi - sr * ai + si * ar;
}

// Module-level scratch arrays
const _s = [0, 0]; // Givens s output
const _r = [0, 0]; // Givens r output
const _t = [0, 0], _u = [0, 0], _v = [0, 0], _w = [0, 0];
const _t2 = [0, 0], _t3 = [0, 0], _t4 = [0, 0], _t5 = [0, 0];
const _t6 = [0, 0], _t7 = [0, 0], _t8 = [0, 0], _t9 = [0, 0];

// ---- Safe complex division (ZLADIV / DLADIV) ----
function dladiv2(a, b, c, d, r, t) {
  if (r !== 0) {
    const br = b * r;
    if (br !== 0) return (a + br) * t;
    return a * t + (b * t) * r;
  }
  return (a + d * (b / c)) * t;
}

function zladiv(out, x, y) {
  const a = x[0], b = x[1], c = y[0], d = y[1];
  const ov = Number.MAX_VALUE;
  const un = SAFMIN;
  const eps = ULP;
  const bs = 2;
  const be = bs / (eps * eps);
  let aa = a, bb = b, cc = c, dd = d, s = 1;
  const ab = Math.max(Math.abs(a), Math.abs(b));
  const cd = Math.max(Math.abs(c), Math.abs(d));

  if (ab >= 0.5 * ov) { aa *= 0.5; bb *= 0.5; s *= 2; }
  if (cd >= 0.5 * ov) { cc *= 0.5; dd *= 0.5; s *= 0.5; }
  if (ab <= un * bs / eps) { aa *= be; bb *= be; s /= be; }
  if (cd <= un * bs / eps) { cc *= be; dd *= be; s *= be; }

  let p, q;
  if (Math.abs(d) <= Math.abs(c)) {
    const r = dd / cc;
    const t = 1 / (cc + dd * r);
    p = dladiv2(aa, bb, cc, dd, r, t);
    q = dladiv2(bb, -aa, cc, dd, r, t);
  } else {
    const r = cc / dd;
    const t = 1 / (dd + cc * r);
    p = dladiv2(bb, aa, dd, cc, r, t);
    q = -dladiv2(aa, -bb, dd, cc, r, t);
  }
  out[0] = p * s;
  out[1] = q * s;
  return out;
}

// ---- Givens rotation (ZLARTG) ----
// Writes s to s_out, r to r_out. Returns c (real scalar).
// Safe when r_out aliases f (reads f completely before writing r_out).
function zlartg(s_out, r_out, f, g) {
  const fr = f[0], fi = f[1], gr = g[0], gi = g[1];
  const abssq = (r, i) => r * r + i * i;
  const rtmin = Math.sqrt(SAFMIN);

  if (gr === 0 && gi === 0) {
    cset(s_out, 0, 0);
    cset(r_out, fr, fi);
    return 1;
  }
  if (fr === 0 && fi === 0) {
    if (fi === 0 && gi === 0) {
      const d = Math.abs(gr);
      cset(r_out, d, 0);
      cset(s_out, gr / d, -gi / d);
    } else if (fr === 0 && gr === 0) {
      const d = Math.abs(gi);
      cset(r_out, d, 0);
      cset(s_out, gr / d, -gi / d);
    } else {
      const g1 = Math.max(Math.abs(gr), Math.abs(gi));
      const rtmax = Math.sqrt(SAFMAX / 2);
      if (g1 > rtmin && g1 < rtmax) {
        const g2 = abssq(gr, gi);
        const d = Math.sqrt(g2);
        cset(s_out, gr / d, -gi / d);
        cset(r_out, d, 0);
      } else {
        const u = Math.min(SAFMAX, Math.max(SAFMIN, g1));
        const gsr = gr / u, gsi = gi / u;
        const g2 = abssq(gsr, gsi);
        const d = Math.sqrt(g2);
        cset(s_out, gsr / d, -gsi / d);
        cset(r_out, d * u, 0);
      }
    }
    return 0;
  }

  const f1 = Math.max(Math.abs(fr), Math.abs(fi));
  const g1 = Math.max(Math.abs(gr), Math.abs(gi));
  let rtmax = Math.sqrt(SAFMAX / 4);

  let c;
  if (f1 > rtmin && f1 < rtmax && g1 > rtmin && g1 < rtmax) {
    const f2 = abssq(fr, fi);
    const g2 = abssq(gr, gi);
    const h2 = f2 + g2;
    if (f2 >= h2 * SAFMIN) {
      c = Math.sqrt(f2 / h2);
      cset(r_out, fr / c, fi / c);
      rtmax *= 2;
      if (f2 > rtmin && h2 < rtmax) {
        const k = 1 / Math.sqrt(f2 * h2);
        // s = conj(g) * f * k
        s_out[0] = (gr * fr + gi * fi) * k;
        s_out[1] = (-gi * fr + gr * fi) * k;
      } else {
        // s = conj(g) * (r / h2)
        const rx = r_out[0] / h2, ry = r_out[1] / h2;
        s_out[0] = gr * rx + gi * ry;
        s_out[1] = -gi * rx + gr * ry;
      }
    } else {
      const d = Math.sqrt(f2 * h2);
      c = f2 / d;
      if (c >= SAFMIN) {
        cset(r_out, fr / c, fi / c);
      } else {
        cset(r_out, fr * (h2 / d), fi * (h2 / d));
      }
      const k = 1 / d;
      s_out[0] = (gr * fr + gi * fi) * k;
      s_out[1] = (-gi * fr + gr * fi) * k;
    }
  } else {
    const u = Math.min(SAFMAX, Math.max(SAFMIN, f1, g1));
    const gsr = gr / u, gsi = gi / u;
    const g2 = abssq(gsr, gsi);
    let f2, fsr, fsi, w;
    if (f1 / u < rtmin) {
      const v = Math.min(SAFMAX, Math.max(SAFMIN, f1));
      w = v / u;
      fsr = fr / v; fsi = fi / v;
      f2 = abssq(fsr, fsi);
    } else {
      w = 1;
      fsr = fr / u; fsi = fi / u;
      f2 = abssq(fsr, fsi);
    }
    const h2 = f2 * w * w + g2;
    if (f2 >= h2 * SAFMIN) {
      c = Math.sqrt(f2 / h2);
      cset(r_out, fsr / c, fsi / c);
      rtmax *= 2;
      if (f2 > rtmin && h2 < rtmax) {
        const k = 1 / Math.sqrt(f2 * h2);
        s_out[0] = (gsr * fsr + gsi * fsi) * k;
        s_out[1] = (-gsi * fsr + gsr * fsi) * k;
      } else {
        const rx = r_out[0] / h2, ry = r_out[1] / h2;
        s_out[0] = gsr * rx + gsi * ry;
        s_out[1] = -gsi * rx + gsr * ry;
      }
    } else {
      const d = Math.sqrt(f2 * h2);
      c = f2 / d;
      if (c >= SAFMIN) {
        cset(r_out, fsr / c, fsi / c);
      } else {
        cset(r_out, fsr * (h2 / d), fsi * (h2 / d));
      }
      const k = 1 / d;
      s_out[0] = (gsr * fsr + gsi * fsi) * k;
      s_out[1] = (-gsi * fsr + gsr * fsi) * k;
    }
    c *= w;
    r_out[0] *= u;
    r_out[1] *= u;
  }
  return c;
}

// ---- Matrix helpers ----
function cmatrix(n) {
  const M = new Array(n);
  for (let i = 0; i < n; i++) {
    M[i] = new Array(n);
    for (let j = 0; j < n; j++) M[i][j] = [0, 0];
  }
  return M;
}

function zlanhs_frobenius(H, n, ilo, ihi) {
  let scale = 0, sumsq = 1;
  for (let j = ilo; j <= ihi; j++) {
    for (let i = ilo; i <= Math.min(ihi, j + 1); i++) {
      const val = cabs(H[i][j]);
      if (val !== 0) {
        if (scale < val) {
          sumsq = 1 + sumsq * (scale / val) ** 2;
          scale = val;
        } else {
          sumsq += (val / scale) ** 2;
        }
      }
    }
  }
  return scale * Math.sqrt(sumsq);
}

// ---- ZGGHRD ----
function zgghrd(A, B, n, ilo, ihi) {
  for (let j = 0; j < n - 1; j++)
    for (let i = j + 1; i < n; i++)
      cset(B[i][j], 0, 0);

  for (let jcol = ilo; jcol <= ihi - 2; jcol++) {
    for (let jrow = ihi; jrow >= jcol + 2; jrow--) {
      // Step 1: rotate rows jrow-1, jrow to kill A[jrow][jcol]
      const c1 = zlartg(_s, A[jrow - 1][jcol], A[jrow - 1][jcol], A[jrow][jcol]);
      cset(A[jrow][jcol], 0, 0);
      const s1r = _s[0], s1i = _s[1];

      for (let j = jcol + 1; j < n; j++) grot(A[jrow - 1][j], A[jrow][j], c1, s1r, s1i);
      for (let j = jrow - 1; j < n; j++) grot(B[jrow - 1][j], B[jrow][j], c1, s1r, s1i);

      // Step 2: rotate columns jrow, jrow-1 to kill B[jrow][jrow-1]
      const c2 = zlartg(_s, B[jrow][jrow], B[jrow][jrow], B[jrow][jrow - 1]);
      cset(B[jrow][jrow - 1], 0, 0);
      const s2r = _s[0], s2i = _s[1];

      for (let j = 0; j <= ihi; j++) grot(A[j][jrow], A[j][jrow - 1], c2, s2r, s2i);
      for (let j = 0; j < jrow; j++) grot(B[j][jrow], B[j][jrow - 1], c2, s2r, s2i);
    }
  }
}

// ---- ZHGEQZ ----
function zhgeqz(H, T, n, ilo, ihi) {
  const alpha = new Array(n);
  const beta = new Array(n);
  for (let i = 0; i < n; i++) { alpha[i] = [0, 0]; beta[i] = [0, 0]; }

  if (n === 0) return { alpha, beta, info: 0 };

  const safmin = SAFMIN;
  const ulp = ULP * BASE;
  const anorm = zlanhs_frobenius(H, n, ilo, ihi);
  const bnorm = zlanhs_frobenius(T, n, ilo, ihi);
  const atol = Math.max(safmin, ulp * anorm);
  const btol = Math.max(safmin, ulp * bnorm);
  const ascale = 1 / Math.max(safmin, anorm);
  const bscale = 1 / Math.max(safmin, bnorm);

  // Eigenvalues outside active block
  for (let j = ihi + 1; j < n; j++) {
    const absb = cabs(T[j][j]);
    if (absb > safmin) {
      const k = 1 / absb;
      // signbc = conj(T[j][j]) / absb
      _t[0] = T[j][j][0] * k; _t[1] = -T[j][j][1] * k;
      cset(T[j][j], absb, 0);
      cmul(H[j][j], _t, H[j][j]);
    } else {
      cset(T[j][j], 0, 0);
    }
    ccopy(alpha[j], H[j][j]);
    ccopy(beta[j], T[j][j]);
  }

  if (ihi < ilo) {
    for (let j = 0; j < ilo; j++) { ccopy(alpha[j], H[j][j]); ccopy(beta[j], T[j][j]); }
    return { alpha, beta, info: 0 };
  }

  let ilast = ihi;
  let ifrstm = ilo;
  let ilastm = ihi;
  let iiter = 0;
  const eshift = [0, 0];
  const maxit = 30 * (ihi - ilo + 1);
  let info = 0;

  for (let jiter = 1; jiter <= maxit; jiter++) {
    let converged = false;
    let tZeroAtIlast = false;
    let ifirst = -1;

    if (ilast === ilo) {
      converged = true;
    } else if (cabs1(H[ilast][ilast - 1]) <= Math.max(safmin,
          ulp * (cabs1(H[ilast][ilast]) + cabs1(H[ilast - 1][ilast - 1])))) {
      cset(H[ilast][ilast - 1], 0, 0);
      converged = true;
    }

    if (!converged && cabs(T[ilast][ilast]) <= btol) {
      cset(T[ilast][ilast], 0, 0);
      tZeroAtIlast = true;
    }

    if (!converged && !tZeroAtIlast) {
      let found = false;
      for (let j = ilast - 1; j >= ilo; j--) {
        let ilazro;
        if (j === ilo) {
          ilazro = true;
        } else if (cabs1(H[j][j - 1]) <= Math.max(safmin,
              ulp * (cabs1(H[j][j]) + cabs1(H[j - 1][j - 1])))) {
          cset(H[j][j - 1], 0, 0);
          ilazro = true;
        } else {
          ilazro = false;
        }

        if (cabs(T[j][j]) < btol) {
          cset(T[j][j], 0, 0);
          let ilazr2 = false;
          if (!ilazro && cabs1(H[j][j - 1]) * (ascale * cabs1(H[j + 1][j])) <=
              cabs1(H[j][j]) * (ascale * atol)) {
            ilazr2 = true;
          }

          if (ilazro || ilazr2) {
            for (let jch = j; jch <= ilast - 1; jch++) {
              const gc = zlartg(_s, H[jch][jch], H[jch][jch], H[jch + 1][jch]);
              cset(H[jch + 1][jch], 0, 0);
              const sr = _s[0], si = _s[1];
              for (let jc = jch + 1; jc <= ilastm; jc++) grot(H[jch][jc], H[jch + 1][jc], gc, sr, si);
              for (let jc = jch + 1; jc <= ilastm; jc++) grot(T[jch][jc], T[jch + 1][jc], gc, sr, si);
              if (ilazr2) cscale(H[jch][jch - 1], gc, H[jch][jch - 1]);
              ilazr2 = false;
              if (cabs1(T[jch + 1][jch + 1]) >= btol) {
                if (jch + 1 >= ilast) converged = true;
                else ifirst = jch + 1;
                found = true;
                break;
              }
              cset(T[jch + 1][jch + 1], 0, 0);
            }
            if (!found) tZeroAtIlast = true;
            found = true;
            break;
          } else {
            for (let jch = j; jch <= ilast - 1; jch++) {
              const gc1 = zlartg(_s, T[jch][jch + 1], T[jch][jch + 1], T[jch + 1][jch + 1]);
              cset(T[jch + 1][jch + 1], 0, 0);
              const s1r = _s[0], s1i = _s[1];
              if (jch < ilastm - 1) for (let jc = jch + 2; jc <= ilastm; jc++) grot(T[jch][jc], T[jch + 1][jc], gc1, s1r, s1i);
              for (let jc = jch - 1; jc <= ilastm; jc++) grot(H[jch][jc], H[jch + 1][jc], gc1, s1r, s1i);
              const gc2 = zlartg(_s, H[jch + 1][jch], H[jch + 1][jch], H[jch + 1][jch - 1]);
              cset(H[jch + 1][jch - 1], 0, 0);
              const s2r = _s[0], s2i = _s[1];
              for (let jr = ifrstm; jr <= jch + 1; jr++) grot(H[jr][jch], H[jr][jch - 1], gc2, s2r, s2i);
              for (let jr = ifrstm; jr <= jch; jr++) grot(T[jr][jch], T[jr][jch - 1], gc2, s2r, s2i);
            }
            tZeroAtIlast = true;
            found = true;
            break;
          }
        } else if (ilazro) {
          ifirst = j;
          found = true;
          break;
        }
      }
      if (!found) { info = 2 * n + 1; break; }
    }

    if (tZeroAtIlast && !converged) {
      const gc = zlartg(_s, H[ilast][ilast], H[ilast][ilast], H[ilast][ilast - 1]);
      cset(H[ilast][ilast - 1], 0, 0);
      const sr = _s[0], si = _s[1];
      for (let jr = ifrstm; jr < ilast; jr++) grot(H[jr][ilast], H[jr][ilast - 1], gc, sr, si);
      for (let jr = ifrstm; jr < ilast; jr++) grot(T[jr][ilast], T[jr][ilast - 1], gc, sr, si);
      converged = true;
    }

    if (converged) {
      const absb = cabs(T[ilast][ilast]);
      if (absb > safmin) {
        const k = 1 / absb;
        _t[0] = T[ilast][ilast][0] * k; _t[1] = -T[ilast][ilast][1] * k;
        cset(T[ilast][ilast], absb, 0);
        cmul(H[ilast][ilast], _t, H[ilast][ilast]);
      } else {
        cset(T[ilast][ilast], 0, 0);
      }
      ccopy(alpha[ilast], H[ilast][ilast]);
      ccopy(beta[ilast], T[ilast][ilast]);

      ilast--;
      if (ilast < ilo) break;
      iiter = 0;
      cset(eshift, 0, 0);
      ilastm = ilast;
      if (ifrstm > ilast) ifrstm = ilo;
      continue;
    }

    // QZ step
    iiter++;
    ifrstm = ifirst;

    // Compute shift
    const shift = _t5;
    if (iiter % 10 !== 0) {
      // Wilkinson shift
      const u12 = _t2, ad11 = _t3, ad21 = _t4, ad12 = _t6, ad22 = _t7;
      cscale(_t, bscale, T[ilast - 1][ilast]); cscale(_u, bscale, T[ilast][ilast]); zladiv(u12, _t, _u);
      cscale(_t, ascale, H[ilast - 1][ilast - 1]); cscale(_u, bscale, T[ilast - 1][ilast - 1]); zladiv(ad11, _t, _u);
      cscale(_t, ascale, H[ilast][ilast - 1]); zladiv(ad21, _t, _u);
      cscale(_t, ascale, H[ilast - 1][ilast]); cscale(_u, bscale, T[ilast][ilast]); zladiv(ad12, _t, _u);
      cscale(_t, ascale, H[ilast][ilast]); zladiv(ad22, _t, _u);

      // abi22 = ad22 - u12*ad21, abi12 = ad12 - u12*ad11
      cmul(_t, u12, ad21); csub(_t8, ad22, _t); // _t8 = abi22
      cmul(_t, u12, ad11); csub(_t9, ad12, _t); // _t9 = abi12

      ccopy(shift, _t8); // shift = abi22

      // ctemp = sqrt(abi12) * sqrt(ad21)
      csqrt(_t, _t9); csqrt(_u, ad21); cmul(_v, _t, _u); // _v = ctemp

      const ctemp_abs1 = cabs1(_v);
      if (ctemp_abs1 !== 0) {
        csub(_t, ad11, shift); cscale(_w, 0.5, _t); // _w = x = 0.5*(ad11-shift)
        const x_abs1 = cabs1(_w);
        const tempMax = Math.max(ctemp_abs1, x_abs1);
        const invMax = 1 / tempMax;
        // y = tempMax * sqrt((x/tempMax)^2 + (ctemp/tempMax)^2)
        cscale(_t, invMax, _w); cmul(_t, _t, _t);
        cscale(_u, invMax, _v); cmul(_u, _u, _u);
        cadd(_t, _t, _u); csqrt(_t, _t);
        cscale(_t, tempMax, _t); // _t = y
        if (x_abs1 > 0) {
          cscale(_u, 1 / x_abs1, _w); // _u = x/|x|
          if (_u[0] * _t[0] + _u[1] * _t[1] < 0) { _t[0] = -_t[0]; _t[1] = -_t[1]; }
        }
        // shift -= ctemp * ctemp / (x + y)
        cadd(_t, _w, _t); zladiv(_t, _v, _t); cmul(_t, _v, _t);
        csub(shift, shift, _t);
      }
    } else {
      // Exceptional shift
      if (iiter % 20 === 0 && bscale * cabs1(T[ilast][ilast]) > safmin) {
        cscale(_t, ascale, H[ilast][ilast]); cscale(_u, bscale, T[ilast][ilast]);
        zladiv(_t, _t, _u); cadd(eshift, eshift, _t);
      } else {
        cscale(_t, ascale, H[ilast][ilast - 1]); cscale(_u, bscale, T[ilast - 1][ilast - 1]);
        zladiv(_t, _t, _u); cadd(eshift, eshift, _t);
      }
      ccopy(shift, eshift);
    }

    // Check for two consecutive small subdiagonals
    let istart = ifirst;
    // ctemp stored in _t
    cscale(_t, ascale, H[ifirst][ifirst]);
    cscale(_u, bscale, T[ifirst][ifirst]); cmul(_u, shift, _u);
    csub(_t, _t, _u);
    for (let j = ilast - 1; j >= ifirst + 1; j--) {
      cscale(_u, ascale, H[j][j]); cscale(_v, bscale, T[j][j]);
      cmul(_v, shift, _v); csub(_u, _u, _v); // _u = ct
      const temp = cabs1(_u);
      const temp2 = ascale * cabs1(H[j + 1][j]);
      const tempr = Math.max(temp, temp2);
      let t1 = temp, t2 = temp2;
      if (tempr < 1 && tempr !== 0) { t1 /= tempr; t2 /= tempr; }
      if (cabs1(H[j][j - 1]) * t2 <= t1 * atol) {
        istart = j;
        ccopy(_t, _u);
        break;
      }
    }

    // Initial rotation: _t = ctemp, _u = ctemp2
    cscale(_u, ascale, H[istart + 1][istart]);
    let c = zlartg(_s, _r, _t, _u);
    let sr = _s[0], si = _s[1];

    // Implicit QZ sweep
    for (let j = istart; j <= ilast - 1; j++) {
      if (j > istart) {
        c = zlartg(_s, H[j][j - 1], H[j][j - 1], H[j + 1][j - 1]);
        cset(H[j + 1][j - 1], 0, 0);
        sr = _s[0]; si = _s[1];
      }

      for (let jc = j; jc <= ilastm; jc++) grot(H[j][jc], H[j + 1][jc], c, sr, si);
      for (let jc = j; jc <= ilastm; jc++) grot(T[j][jc], T[j + 1][jc], c, sr, si);

      const c2 = zlartg(_s, T[j + 1][j + 1], T[j + 1][j + 1], T[j + 1][j]);
      cset(T[j + 1][j], 0, 0);
      const s2r = _s[0], s2i = _s[1];

      for (let jr = ifrstm; jr <= Math.min(j + 2, ilast); jr++) grot(H[jr][j + 1], H[jr][j], c2, s2r, s2i);
      for (let jr = ifrstm; jr <= j; jr++) grot(T[jr][j + 1], T[jr][j], c2, s2r, s2i);
    }
  }

  // Eigenvalues 0:ilo-1
  for (let j = 0; j < ilo; j++) {
    const absb = cabs(T[j][j]);
    if (absb > safmin) {
      const k = 1 / absb;
      _t[0] = T[j][j][0] * k; _t[1] = -T[j][j][1] * k;
      cset(T[j][j], absb, 0);
      cmul(H[j][j], _t, H[j][j]);
    } else {
      cset(T[j][j], 0, 0);
    }
    ccopy(alpha[j], H[j][j]);
    ccopy(beta[j], T[j][j]);
  }

  if (info === 0 && ilast >= ilo) info = ilast + 1;
  return { alpha, beta, info };
}

// ---- Complex square root ----
function csqrt(out, z) {
  const ar = z[0], ai = z[1];
  const r = Math.sqrt(ar * ar + ai * ai);
  if (r === 0) { cset(out, 0, 0); return out; }
  const s = ai >= 0 ? 1 : -1;
  out[0] = Math.sqrt((r + ar) / 2);
  out[1] = s * Math.sqrt((r - ar) / 2);
  return out;
}

// ---- Top-level: generalized eigenvalues of (A, B) ----
export function zggev(A, B, n) {
  if (n === 0) return [];

  const H = A.map(row => row.map(v => [v[0], v[1]]));
  const T = B.map(row => row.map(v => [v[0], v[1]]));

  zgghrd(H, T, n, 0, n - 1);
  const { alpha, beta } = zhgeqz(H, T, n, 0, n - 1);

  const eigenvalues = [];
  for (let i = 0; i < n; i++) {
    if (cabs(beta[i]) > SAFMIN) {
      const eig = [0, 0];
      zladiv(eig, alpha[i], beta[i]);
      eigenvalues.push(eig);
    }
  }
  return eigenvalues;
}

export { zlartg, zladiv, cabs, cabs1, cmul, cadd, csub, cscale, cconj, cneg, cset, ccopy, grot, cmatrix };
