// DGEMV: General matrix-vector multiply
//   y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y
import { lsame, xerbla } from "../helpers.js";

export function dgemv(trans, m, n, alpha, a, lda, x, incx, beta, y, incy) {
  var temp;
  var i, info, ix, iy, j, jx, jy, kx, ky, lenx, leny;

  // Test the input parameters.
  info = 0;
  if (!lsame(trans, "N") && !lsame(trans, "T") && !lsame(trans, "C")) {
    info = 1;
  } else if (m < 0) {
    info = 2;
  } else if (n < 0) {
    info = 3;
  } else if (lda < Math.max(1, m)) {
    info = 6;
  } else if (incx === 0) {
    info = 8;
  } else if (incy === 0) {
    info = 11;
  }
  if (info !== 0) {
    xerbla("DGEMV ", info);
    return;
  }

  // Quick return if possible.
  if (m === 0 || n === 0 || (alpha === 0.0 && beta === 1.0)) return;

  // Set LENX and LENY, the lengths of the vectors x and y, and set
  // up the start points in X and Y.
  if (lsame(trans, "N")) {
    lenx = n;
    leny = m;
  } else {
    lenx = m;
    leny = n;
  }
  if (incx > 0) {
    kx = 0;
  } else {
    kx = -(lenx - 1) * incx;
  }
  if (incy > 0) {
    ky = 0;
  } else {
    ky = -(leny - 1) * incy;
  }

  // Start the operations. In this version the elements of A are
  // accessed sequentially with one pass through A.

  // First form y := beta*y.
  if (beta !== 1.0) {
    if (incy === 1) {
      if (beta === 0.0) {
        for (i = 0; i < leny; i++) {
          y[i] = 0.0;
        }
      } else {
        for (i = 0; i < leny; i++) {
          y[i] = beta * y[i];
        }
      }
    } else {
      iy = ky;
      if (beta === 0.0) {
        for (i = 0; i < leny; i++) {
          y[iy] = 0.0;
          iy = iy + incy;
        }
      } else {
        for (i = 0; i < leny; i++) {
          y[iy] = beta * y[iy];
          iy = iy + incy;
        }
      }
    }
  }
  if (alpha === 0.0) return;

  if (lsame(trans, "N")) {
    // Form y := alpha*A*x + y.
    jx = kx;
    if (incy === 1) {
      for (j = 0; j < n; j++) {
        temp = alpha * x[jx];
        for (i = 0; i < m; i++) {
          y[i] = y[i] + temp * a[j * lda + i];
        }
        jx = jx + incx;
      }
    } else {
      for (j = 0; j < n; j++) {
        temp = alpha * x[jx];
        iy = ky;
        for (i = 0; i < m; i++) {
          y[iy] = y[iy] + temp * a[j * lda + i];
          iy = iy + incy;
        }
        jx = jx + incx;
      }
    }
  } else {
    // Form y := alpha*A**T*x + y.
    jy = ky;
    if (incx === 1) {
      for (j = 0; j < n; j++) {
        temp = 0.0;
        for (i = 0; i < m; i++) {
          temp = temp + a[j * lda + i] * x[i];
        }
        y[jy] = y[jy] + alpha * temp;
        jy = jy + incy;
      }
    } else {
      for (j = 0; j < n; j++) {
        temp = 0.0;
        ix = kx;
        for (i = 0; i < m; i++) {
          temp = temp + a[j * lda + i] * x[ix];
          ix = ix + incx;
        }
        y[jy] = y[jy] + alpha * temp;
        jy = jy + incy;
      }
    }
  }
}
