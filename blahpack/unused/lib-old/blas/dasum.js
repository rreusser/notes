// DASUM: takes the sum of the absolute values of a vector.
export function dasum(n, dx, incx) {
  var dtemp;
  var nincx;
  var mp1;
  var i;
  var m;
  dtemp = 0;
  if (n <= 0 || incx <= 0) return 0;
  if (incx === 1) {
    // code for increment equal to 1
    // clean-up loop
    m = n % 6;
    if (m !== 0) {
      for (i = 0; i < m; i++) {
        dtemp = dtemp + Math.abs(dx[i]);
      }
      if (n < 6) {
        return dtemp;
      }
    }
    mp1 = m;
    for (i = mp1; i < n; i += 6) {
      dtemp =
        dtemp +
        Math.abs(dx[i]) +
        Math.abs(dx[i + 1]) +
        Math.abs(dx[i + 2]) +
        Math.abs(dx[i + 3]) +
        Math.abs(dx[i + 4]) +
        Math.abs(dx[i + 5]);
    }
  } else {
    // code for increment not equal to 1
    nincx = n * incx;
    for (i = 0; i < nincx; i += incx) {
      dtemp = dtemp + Math.abs(dx[i]);
    }
  }
  return dtemp;
}
