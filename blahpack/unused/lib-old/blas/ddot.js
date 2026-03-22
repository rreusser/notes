// DDOT: forms the dot product of two vectors.
export function ddot(n, dx, incx, dy, incy) {
  var dtemp;
  var mp1;
  var ix;
  var iy;
  var i;
  var m;
  dtemp = 0.0;
  if (n <= 0) return 0.0;
  if (incx === 1 && incy === 1) {
    // code for both increments equal to 1
    // clean-up loop
    m = n % 5;
    if (m !== 0) {
      for (i = 0; i < m; i++) {
        dtemp = dtemp + dx[i] * dy[i];
      }
      if (n < 5) return dtemp;
    }
    mp1 = m;
    for (i = mp1; i < n; i += 5) {
      dtemp = dtemp + dx[i] * dy[i] + dx[i + 1] * dy[i + 1] +
              dx[i + 2] * dy[i + 2] + dx[i + 3] * dy[i + 3] + dx[i + 4] * dy[i + 4];
    }
  } else {
    // code for unequal increments or equal increments not equal to 1
    ix = 0;
    iy = 0;
    if (incx < 0) ix = (-n + 1) * incx;
    if (incy < 0) iy = (-n + 1) * incy;
    for (i = 0; i < n; i++) {
      dtemp = dtemp + dx[ix] * dy[iy];
      ix = ix + incx;
      iy = iy + incy;
    }
  }
  return dtemp;
}
