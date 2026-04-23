//! DPOTF2: Cholesky factorization of a symmetric positive definite matrix
//! (unblocked algorithm, Level 2 BLAS)
function dpotf2(uplo, n, A, strideA1, strideA2, offsetA, info) {
  var disnan;
  var lsame;
  var upper;
  var ddot;
  var zero;
  var ajj;
  var one;
  var j;
  one = 1;
  zero = 0;
  info = 0;
  upper = uplo === "U";
  if (!upper && !(uplo === "L")) {
    info = -1;
  } else if (n < 0) {
    info = -2;
  } else if (lda < max(1, n)) {
    info = -4;
  }
  if (info !== 0) {
    xerbla("DPOTF2", -info);
    return;
  }
  if (n === 0) return;
  if (upper) {
    for (j = 1; j <= n; j += 1) {
      ajj =
        A[j - 1 + (j - 1) * lda] -
        ddot(j - 1, A[j - 1 + 0 * lda], 1, A[j - 1 + 0 * lda], 1);
      if (ajj <= zero || disnan(ajj)) {
        A[j - 1 + (j - 1) * lda] = ajj;
        info = j;
        return;
      }
      ajj = sqrt(ajj);
      A[j - 1 + (j - 1) * lda] = ajj;
      if (j < n) {
        dgemv(
          "Transpose",
          j - 1,
          n - j,
          -one,
          A[j + 1 - 1 + 0 * lda],
          lda,
          A[j - 1 + 0 * lda],
          1,
          one,
          A[j + 1 - 1 + (j - 1) * lda],
          lda,
        )(dscal(n - j, one / ajj, A[j + 1 - 1 + (j - 1) * lda], lda));
      }
    }
  } else {
    for (j = 1; j <= n; j += 1) {
      ajj =
        A[j - 1 + (j - 1) * lda] -
        ddot(j - 1, A[0 + (j - 1) * lda], lda, A[0 + (j - 1) * lda], lda);
      if (ajj <= zero || disnan(ajj)) {
        A[j - 1 + (j - 1) * lda] = ajj;
        info = j;
        return;
      }
      ajj = sqrt(ajj);
      A[j - 1 + (j - 1) * lda] = ajj;
      if (j < n) {
        dgemv(
          "No transpose",
          n - j,
          j - 1,
          -one,
          A[0 + (j + 1 - 1) * lda],
          lda,
          A[0 + (j - 1) * lda],
          lda,
          one,
          A[j - 1 + (j + 1 - 1) * lda],
          1,
        )(dscal(n - j, one / ajj, A[j - 1 + (j + 1 - 1) * lda], 1));
      }
    }
  }
  return;
}

