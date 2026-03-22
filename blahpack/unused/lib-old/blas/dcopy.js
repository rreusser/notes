// DCOPY: copy a vector x to a vector y.
//   dy := dx
export function dcopy(n, dx, incx, dy, incy) {
  var mp1;
  var ix;
  var iy;
  var i;
  var m;
  if (n <= 0) return;
  if (incx === 1 && incy === 1) {
    // code for both increments equal to 1
    // clean-up loop
    m = n % 7;
    if (m !== 0) {
      for (i = 0; i < m; i++) {
        dy[i] = dx[i];
      }
    }
    if (n < 7) return;
    mp1 = m;
    for (i = mp1; i < n; i += 7) {
      dy[i] = dx[i];
      dy[i + 1] = dx[i + 1];
      dy[i + 2] = dx[i + 2];
      dy[i + 3] = dx[i + 3];
      dy[i + 4] = dx[i + 4];
      dy[i + 5] = dx[i + 5];
      dy[i + 6] = dx[i + 6];
    }
  } else {
    // code for unequal increments or equal increments not equal to 1
    ix = 0;
    iy = 0;
    if (incx < 0) ix = (-n + 1) * incx;
    if (incy < 0) iy = (-n + 1) * incy;
    for (i = 0; i < n; i++) {
      dy[iy] = dx[ix];
      ix = ix + incx;
      iy = iy + incy;
    }
  }
}
