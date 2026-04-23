// DSCAL: scales a vector by a constant. dx := da * dx
export function dscal(n, da, dx, incx) {
  var mp1;
  var nincx;
  var i;
  var m;
  if (n <= 0 || incx <= 0 || da === 1.0) return;
  if (incx === 1) {
    // code for increment equal to 1
    // clean-up loop
    m = n % 5;
    if (m !== 0) {
      for (i = 0; i < m; i++) {
        dx[i] = da * dx[i];
      }
      if (n < 5) return;
    }
    mp1 = m;
    for (i = mp1; i < n; i += 5) {
      dx[i] = da * dx[i];
      dx[i + 1] = da * dx[i + 1];
      dx[i + 2] = da * dx[i + 2];
      dx[i + 3] = da * dx[i + 3];
      dx[i + 4] = da * dx[i + 4];
    }
  } else {
    // code for increment not equal to 1
    nincx = n * incx;
    for (i = 0; i < nincx; i += incx) {
      dx[i] = da * dx[i];
    }
  }
}
