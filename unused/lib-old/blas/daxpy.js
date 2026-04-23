// DAXPY: constant times a vector plus a vector.
//   dy := dy + da * dx
export function daxpy(n, da, dx, incx, dy, incy) {
  var mp1;
  var ix;
  var iy;
  var i;
  var m;
  if (n <= 0) return;
  if (da === 0) return;
  if (incx === 1 && incy === 1) {
    // code for both increments equal to 1
    // clean-up loop
    m = n % 4;
    if (m !== 0) {
      for (i = 0; i < m; i++) {
        dy[i] = dy[i] + da * dx[i];
      }
    }
    if (n < 4) return;
    mp1 = m;
    for (i = mp1; i < n; i += 4) {
      dy[i] = dy[i] + da * dx[i];
      dy[i + 1] = dy[i + 1] + da * dx[i + 1];
      dy[i + 2] = dy[i + 2] + da * dx[i + 2];
      dy[i + 3] = dy[i + 3] + da * dx[i + 3];
    }
  } else {
    // code for unequal increments or equal increments not equal to 1
    ix = 0;
    iy = 0;
    if (incx < 0) ix = (-n + 1) * incx;
    if (incy < 0) iy = (-n + 1) * incy;
    for (i = 0; i < n; i++) {
      dy[iy] = dy[iy] + da * dx[ix];
      ix = ix + incx;
      iy = iy + incy;
    }
  }
}
