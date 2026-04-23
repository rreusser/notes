// DPOTF2: Cholesky factorization of a symmetric positive definite matrix
// (unblocked algorithm, Level 2 BLAS)
//
// Computes A = U**T * U (uplo='U') or A = L * L**T (uplo='L')
// a is a flat Float64Array in column-major order, dimension (lda, n)
//
// Returns info: 0 = success, k > 0 = leading minor of order k not positive definite
import { xerbla, lsame } from "../helpers.js";
import { ddot } from "../blas/ddot.js";
import { dgemv } from "../blas/dgemv.js";
import { dscal } from "../blas/dscal.js";

export function dpotf2(uplo, n, a, lda) {
  var info = 0;
  var upper;
  var ajj;
  var j;

  upper = lsame(uplo, "U");
  if (!upper && !lsame(uplo, "L")) {
    info = -1;
  } else if (n < 0) {
    info = -2;
  } else if (lda < Math.max(1, n)) {
    info = -4;
  }
  if (info !== 0) {
    xerbla("DPOTF2", -info);
  }

  if (n === 0) return 0;

  if (upper) {
    // Compute the Cholesky factorization A = U**T * U.
    for (j = 0; j < n; j++) {
      // Compute U(j,j) and test for non-positive-definiteness.
      // Fortran: AJJ = A(J,J) - DDOT(J-1, A(1,J), 1, A(1,J), 1)
      ajj = a[j + j * lda] - ddot(j, a.subarray(j * lda), 1, a.subarray(j * lda), 1);
      if (ajj <= 0.0 || Number.isNaN(ajj)) {
        a[j + j * lda] = ajj;
        return j + 1;
      }
      ajj = Math.sqrt(ajj);
      a[j + j * lda] = ajj;
      // Compute elements j+1:n-1 of row j.
      if (j < n - 1) {
        // Fortran: DGEMV('T', J-1, N-J, -1, A(1,J+1), LDA, A(1,J), 1, 1, A(J,J+1), LDA)
        dgemv("Transpose", j, n - j - 1, -1.0,
          a.subarray((j + 1) * lda), lda,
          a.subarray(j * lda), 1,
          1.0,
          a.subarray(j + (j + 1) * lda), lda);
        // Fortran: DSCAL(N-J, 1/AJJ, A(J,J+1), LDA)
        dscal(n - j - 1, 1.0 / ajj, a.subarray(j + (j + 1) * lda), lda);
      }
    }
  } else {
    // Compute the Cholesky factorization A = L * L**T.
    for (j = 0; j < n; j++) {
      // Compute L(j,j) and test for non-positive-definiteness.
      // Fortran: AJJ = A(J,J) - DDOT(J-1, A(J,1), LDA, A(J,1), LDA)
      ajj = a[j + j * lda] - ddot(j, a.subarray(j), lda, a.subarray(j), lda);
      if (ajj <= 0.0 || Number.isNaN(ajj)) {
        a[j + j * lda] = ajj;
        return j + 1;
      }
      ajj = Math.sqrt(ajj);
      a[j + j * lda] = ajj;
      // Compute elements j+1:n-1 of column j.
      if (j < n - 1) {
        // Fortran: DGEMV('N', N-J, J-1, -1, A(J+1,1), LDA, A(J,1), LDA, 1, A(J+1,J), 1)
        dgemv("No transpose", n - j - 1, j, -1.0,
          a.subarray(j + 1), lda,
          a.subarray(j), lda,
          1.0,
          a.subarray(j + 1 + j * lda), 1);
        // Fortran: DSCAL(N-J, 1/AJJ, A(J+1,J), 1)
        dscal(n - j - 1, 1.0 / ajj, a.subarray(j + 1 + j * lda), 1);
      }
    }
  }
  return 0;
}
