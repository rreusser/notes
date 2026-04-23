// Real least-squares solver via Householder QR factorization.
// Solves the overdetermined system A*x ≈ b in the least-squares sense.

// A: column-major Float64Array of size m*n (A[i + j*m] = A_{ij})
// b: Float64Array of length m (overwritten with Q^T * b)
// m: number of rows, n: number of columns (m >= n)
// Returns: Float64Array of length n (the least-squares solution x)
export function lssolve(A, b, m, n) {
  // Householder QR: reduce A to upper triangular R, apply same transforms to b
  for (let k = 0; k < n; k++) {
    // Compute norm of sub-column A[k:m-1, k]
    let norm = 0;
    for (let i = k; i < m; i++) norm += A[i + k * m] * A[i + k * m];
    norm = Math.sqrt(norm);

    if (norm === 0) continue;

    // Choose sign to avoid cancellation
    const alpha = A[k + k * m] >= 0 ? -norm : norm;

    // Householder vector v stored in A[k:m-1, k]
    A[k + k * m] -= alpha;

    // Compute ||v||^2
    let vnorm2 = 0;
    for (let i = k; i < m; i++) vnorm2 += A[i + k * m] * A[i + k * m];

    if (vnorm2 === 0) continue;

    const scale = 2 / vnorm2;

    // Apply reflector to columns k+1..n-1
    for (let j = k + 1; j < n; j++) {
      let dot = 0;
      for (let i = k; i < m; i++) dot += A[i + k * m] * A[i + j * m];
      dot *= scale;
      for (let i = k; i < m; i++) A[i + j * m] -= dot * A[i + k * m];
    }

    // Apply reflector to b
    let dotb = 0;
    for (let i = k; i < m; i++) dotb += A[i + k * m] * b[i];
    dotb *= scale;
    for (let i = k; i < m; i++) b[i] -= dotb * A[i + k * m];

    // Store R[k,k]
    A[k + k * m] = alpha;
  }

  // Back-substitution: solve R[0:n-1, 0:n-1] * x = b[0:n-1]
  const x = new Float64Array(n);
  for (let k = n - 1; k >= 0; k--) {
    let s = b[k];
    for (let j = k + 1; j < n; j++) s -= A[k + j * m] * x[j];
    x[k] = s / A[k + k * m];
  }

  return x;
}
