program test_zgemm
  use test_utils
  implicit none
  complex*16 :: a(4,4), b(4,4), c(4,4), alpha, beta
  double precision :: c_r(32)
  equivalence (c, c_r)
  integer :: m, n, k, lda, ldb, ldc

  lda = 4
  ldb = 4
  ldc = 4

  ! Test 1: basic N,N — C := alpha*A*B + beta*C
  ! A = [ (1,1) (3,0) ]   B = [ (1,0) (0,1) ]
  !     [ (2,0) (4,1) ]       [ (0,-1) (1,0) ]
  ! alpha = (1,0), beta = (0,0)
  m = 2
  n = 2
  k = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  c = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 1.0d0)
  a(2,1) = (2.0d0, 0.0d0)
  a(1,2) = (3.0d0, 0.0d0)
  a(2,2) = (4.0d0, 1.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, -1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  beta = (0.0d0, 0.0d0)
  call zgemm('N', 'N', m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
  call begin_test('zgemm_basic_nn')
  call print_cmatrix('c', c_r, ldc, m, n)
  call end_test()

  ! Test 2: trans='C' on A — C := alpha*A^H*B + beta*C
  m = 2
  n = 2
  k = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  c = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 1.0d0)
  a(2,1) = (2.0d0, 2.0d0)
  a(1,2) = (3.0d0, 3.0d0)
  a(2,2) = (4.0d0, 4.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (1.0d0, 1.0d0)
  b(2,2) = (2.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  beta = (0.0d0, 0.0d0)
  call zgemm('C', 'N', m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
  call begin_test('zgemm_conjA')
  call print_cmatrix('c', c_r, ldc, m, n)
  call end_test()

  ! Test 3: alpha and beta scaling
  m = 2
  n = 2
  k = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  c = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 0.0d0)
  a(2,1) = (0.0d0, 0.0d0)
  a(1,2) = (0.0d0, 0.0d0)
  a(2,2) = (1.0d0, 0.0d0)
  b(1,1) = (2.0d0, 1.0d0)
  b(2,1) = (0.0d0, 0.0d0)
  b(1,2) = (0.0d0, 0.0d0)
  b(2,2) = (3.0d0, 2.0d0)
  c(1,1) = (1.0d0, 1.0d0)
  c(2,1) = (0.0d0, 0.0d0)
  c(1,2) = (0.0d0, 0.0d0)
  c(2,2) = (1.0d0, 1.0d0)
  alpha = (2.0d0, 1.0d0)
  beta = (1.0d0, -1.0d0)
  call zgemm('N', 'N', m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
  call begin_test('zgemm_alpha_beta')
  call print_cmatrix('c', c_r, ldc, m, n)
  call end_test()

  ! Test 4: M=0 quick return
  c = (0.0d0, 0.0d0)
  c(1,1) = (99.0d0, 88.0d0)
  alpha = (1.0d0, 0.0d0)
  beta = (1.0d0, 0.0d0)
  call zgemm('N', 'N', 0, 2, 2, alpha, a, lda, b, ldb, beta, c, ldc)
  call begin_test('zgemm_m_zero')
  call print_cmatrix('c', c_r, ldc, 1, 1)
  call end_test()

  ! Test 5: trans='T' on B — C := alpha*A*B^T + beta*C
  m = 2
  n = 2
  k = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  c = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 1.0d0)
  a(2,1) = (2.0d0, 0.0d0)
  a(1,2) = (0.0d0, 1.0d0)
  a(2,2) = (1.0d0, -1.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 0.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  beta = (0.0d0, 0.0d0)
  call zgemm('N', 'T', m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
  call begin_test('zgemm_transB')
  call print_cmatrix('c', c_r, ldc, m, n)
  call end_test()

  ! Test 6: trans='C' on B — C := alpha*A*B^H + beta*C
  m = 2
  n = 2
  k = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  c = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 0.0d0)
  a(2,1) = (0.0d0, 1.0d0)
  a(1,2) = (2.0d0, 0.0d0)
  a(2,2) = (0.0d0, 2.0d0)
  b(1,1) = (1.0d0, 1.0d0)
  b(2,1) = (2.0d0, 2.0d0)
  b(1,2) = (3.0d0, 3.0d0)
  b(2,2) = (4.0d0, 4.0d0)
  alpha = (1.0d0, 0.0d0)
  beta = (0.0d0, 0.0d0)
  call zgemm('N', 'C', m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
  call begin_test('zgemm_conjB')
  call print_cmatrix('c', c_r, ldc, m, n)
  call end_test()

  ! Test 7: transA='T', transB='N' — C := alpha*A^T*B + beta*C
  m = 2
  n = 2
  k = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  c = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 1.0d0)
  a(2,1) = (2.0d0, 2.0d0)
  a(1,2) = (3.0d0, 3.0d0)
  a(2,2) = (4.0d0, 4.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (1.0d0, 1.0d0)
  b(2,2) = (2.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  beta = (0.0d0, 0.0d0)
  call zgemm('T', 'N', m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
  call begin_test('zgemm_transA_N')
  call print_cmatrix('c', c_r, ldc, m, n)
  call end_test()

  ! Test 8: alpha=(0,0) with beta=(0,0) — C should be zeroed
  m = 2
  n = 2
  k = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  c = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 1.0d0)
  a(2,1) = (2.0d0, 0.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  c(1,1) = (5.0d0, 3.0d0)
  c(2,1) = (7.0d0, 2.0d0)
  c(1,2) = (1.0d0, 1.0d0)
  c(2,2) = (9.0d0, 4.0d0)
  alpha = (0.0d0, 0.0d0)
  beta = (0.0d0, 0.0d0)
  call zgemm('N', 'N', m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
  call begin_test('zgemm_alpha_zero_beta_zero')
  call print_cmatrix('c', c_r, ldc, m, n)
  call end_test()

  ! Test 9: alpha=(0,0) with non-trivial beta — C := beta*C
  m = 2
  n = 2
  k = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  c = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 1.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  c(1,1) = (2.0d0, 1.0d0)
  c(2,1) = (3.0d0, 2.0d0)
  c(1,2) = (4.0d0, 3.0d0)
  c(2,2) = (5.0d0, 4.0d0)
  alpha = (0.0d0, 0.0d0)
  beta = (2.0d0, 1.0d0)
  call zgemm('N', 'N', m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
  call begin_test('zgemm_alpha_zero_beta_scale')
  call print_cmatrix('c', c_r, ldc, m, n)
  call end_test()

  ! Test 10: transA='C', transB='C' — C := alpha*A^H*B^H + beta*C
  m = 2
  n = 2
  k = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  c = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 1.0d0)
  a(2,1) = (2.0d0, 2.0d0)
  a(1,2) = (3.0d0, 3.0d0)
  a(2,2) = (4.0d0, 4.0d0)
  b(1,1) = (1.0d0, 1.0d0)
  b(2,1) = (2.0d0, 2.0d0)
  b(1,2) = (3.0d0, 3.0d0)
  b(2,2) = (4.0d0, 4.0d0)
  alpha = (1.0d0, 0.0d0)
  beta = (0.0d0, 0.0d0)
  call zgemm('C', 'C', m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
  call begin_test('zgemm_conjA_conjB')
  call print_cmatrix('c', c_r, ldc, m, n)
  call end_test()

  ! Test 11: transA='T', transB='T' — C := alpha*A^T*B^T + beta*C
  m = 2
  n = 2
  k = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  c = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 1.0d0)
  a(2,1) = (2.0d0, 0.0d0)
  a(1,2) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (2.0d0, 1.0d0)
  b(2,2) = (1.0d0, -1.0d0)
  alpha = (1.0d0, 0.0d0)
  beta = (0.0d0, 0.0d0)
  call zgemm('T', 'T', m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
  call begin_test('zgemm_transA_transB')
  call print_cmatrix('c', c_r, ldc, m, n)
  call end_test()

  ! Test 12: transA='T', transB='C' — C := alpha*A^T*B^H + beta*C
  m = 2
  n = 2
  k = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  c = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 1.0d0)
  a(2,1) = (2.0d0, 0.0d0)
  a(1,2) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  b(1,1) = (1.0d0, 1.0d0)
  b(2,1) = (2.0d0, 2.0d0)
  b(1,2) = (3.0d0, 3.0d0)
  b(2,2) = (4.0d0, 4.0d0)
  alpha = (1.0d0, 0.0d0)
  beta = (0.0d0, 0.0d0)
  call zgemm('T', 'C', m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
  call begin_test('zgemm_transA_conjB')
  call print_cmatrix('c', c_r, ldc, m, n)
  call end_test()

  ! Test 13: transA='C', transB='T' — C := alpha*A^H*B^T + beta*C
  m = 2
  n = 2
  k = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  c = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 1.0d0)
  a(2,1) = (2.0d0, 2.0d0)
  a(1,2) = (3.0d0, 3.0d0)
  a(2,2) = (4.0d0, 4.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (2.0d0, 1.0d0)
  b(2,2) = (1.0d0, -1.0d0)
  alpha = (1.0d0, 0.0d0)
  beta = (0.0d0, 0.0d0)
  call zgemm('C', 'T', m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
  call begin_test('zgemm_conjA_transB')
  call print_cmatrix('c', c_r, ldc, m, n)
  call end_test()

  ! Test 14: K=0 — should just scale C by beta
  m = 2
  n = 2
  k = 0
  c = (0.0d0, 0.0d0)
  c(1,1) = (1.0d0, 2.0d0)
  c(2,1) = (3.0d0, 4.0d0)
  c(1,2) = (5.0d0, 6.0d0)
  c(2,2) = (7.0d0, 8.0d0)
  alpha = (1.0d0, 0.0d0)
  beta = (2.0d0, 0.0d0)
  call zgemm('N', 'N', m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
  call begin_test('zgemm_k_zero')
  call print_cmatrix('c', c_r, ldc, m, n)
  call end_test()

  ! Test 15: transA='N', transB='T', with non-trivial beta
  m = 2
  n = 2
  k = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  c = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 0.0d0)
  a(2,1) = (0.0d0, 0.0d0)
  a(1,2) = (0.0d0, 0.0d0)
  a(2,2) = (1.0d0, 0.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 0.0d0)
  b(1,2) = (0.0d0, 0.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  c(1,1) = (1.0d0, 1.0d0)
  c(2,1) = (2.0d0, 2.0d0)
  c(1,2) = (3.0d0, 3.0d0)
  c(2,2) = (4.0d0, 4.0d0)
  alpha = (1.0d0, 0.0d0)
  beta = (2.0d0, 1.0d0)
  call zgemm('N', 'T', m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
  call begin_test('zgemm_NT_beta_scale')
  call print_cmatrix('c', c_r, ldc, m, n)
  call end_test()

  ! Test 16: transA='C', transB='C', with non-zero beta
  m = 2
  n = 2
  k = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  c = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 0.0d0)
  a(2,1) = (0.0d0, 0.0d0)
  a(1,2) = (0.0d0, 0.0d0)
  a(2,2) = (1.0d0, 0.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 0.0d0)
  b(1,2) = (0.0d0, 0.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  c(1,1) = (1.0d0, 1.0d0)
  c(2,1) = (2.0d0, 2.0d0)
  c(1,2) = (3.0d0, 3.0d0)
  c(2,2) = (4.0d0, 4.0d0)
  alpha = (1.0d0, 0.0d0)
  beta = (0.5d0, 0.5d0)
  call zgemm('C', 'C', m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
  call begin_test('zgemm_CC_beta_nonzero')
  call print_cmatrix('c', c_r, ldc, m, n)
  call end_test()

  ! Test 17: transA='N', transB='C' with non-trivial beta
  m = 2
  n = 2
  k = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  c = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 0.0d0)
  a(2,1) = (0.0d0, 0.0d0)
  a(1,2) = (0.0d0, 0.0d0)
  a(2,2) = (1.0d0, 0.0d0)
  b(1,1) = (1.0d0, 1.0d0)
  b(2,1) = (2.0d0, 2.0d0)
  b(1,2) = (3.0d0, 3.0d0)
  b(2,2) = (4.0d0, 4.0d0)
  c(1,1) = (1.0d0, 0.0d0)
  c(2,1) = (0.0d0, 0.0d0)
  c(1,2) = (0.0d0, 0.0d0)
  c(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  beta = (2.0d0, 0.0d0)
  call zgemm('N', 'C', m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
  call begin_test('zgemm_NC_beta_scale')
  call print_cmatrix('c', c_r, ldc, m, n)
  call end_test()

  ! Test 18: transA='T', transB='N' with non-zero beta
  m = 2
  n = 2
  k = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  c = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 0.0d0)
  a(2,1) = (0.0d0, 0.0d0)
  a(1,2) = (0.0d0, 0.0d0)
  a(2,2) = (1.0d0, 0.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (1.0d0, 1.0d0)
  b(2,2) = (2.0d0, 0.0d0)
  c(1,1) = (1.0d0, 1.0d0)
  c(2,1) = (2.0d0, 2.0d0)
  c(1,2) = (3.0d0, 3.0d0)
  c(2,2) = (4.0d0, 4.0d0)
  alpha = (1.0d0, 0.0d0)
  beta = (1.0d0, 1.0d0)
  call zgemm('T', 'N', m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
  call begin_test('zgemm_TN_beta_nonzero')
  call print_cmatrix('c', c_r, ldc, m, n)
  call end_test()

  ! Test 19: transA='C', transB='N' with non-zero beta
  m = 2
  n = 2
  k = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  c = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 0.0d0)
  a(2,1) = (0.0d0, 0.0d0)
  a(1,2) = (0.0d0, 0.0d0)
  a(2,2) = (1.0d0, 0.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (1.0d0, 1.0d0)
  b(2,2) = (2.0d0, 0.0d0)
  c(1,1) = (1.0d0, 1.0d0)
  c(2,1) = (2.0d0, 2.0d0)
  c(1,2) = (3.0d0, 3.0d0)
  c(2,2) = (4.0d0, 4.0d0)
  alpha = (1.0d0, 0.0d0)
  beta = (1.0d0, 1.0d0)
  call zgemm('C', 'N', m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
  call begin_test('zgemm_CN_beta_nonzero')
  call print_cmatrix('c', c_r, ldc, m, n)
  call end_test()

  ! Test 20: transA='C', transB='T' with non-zero beta
  m = 2
  n = 2
  k = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  c = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 0.0d0)
  a(2,1) = (0.0d0, 0.0d0)
  a(1,2) = (0.0d0, 0.0d0)
  a(2,2) = (1.0d0, 0.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (2.0d0, 1.0d0)
  b(2,2) = (1.0d0, -1.0d0)
  c(1,1) = (1.0d0, 1.0d0)
  c(2,1) = (2.0d0, 2.0d0)
  c(1,2) = (3.0d0, 3.0d0)
  c(2,2) = (4.0d0, 4.0d0)
  alpha = (1.0d0, 0.0d0)
  beta = (0.5d0, 0.0d0)
  call zgemm('C', 'T', m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
  call begin_test('zgemm_CT_beta_nonzero')
  call print_cmatrix('c', c_r, ldc, m, n)
  call end_test()

  ! Test 21: transA='T', transB='C' with non-zero beta
  m = 2
  n = 2
  k = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  c = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 0.0d0)
  a(2,1) = (0.0d0, 0.0d0)
  a(1,2) = (0.0d0, 0.0d0)
  a(2,2) = (1.0d0, 0.0d0)
  b(1,1) = (1.0d0, 1.0d0)
  b(2,1) = (2.0d0, 2.0d0)
  b(1,2) = (3.0d0, 3.0d0)
  b(2,2) = (4.0d0, 4.0d0)
  c(1,1) = (1.0d0, 1.0d0)
  c(2,1) = (2.0d0, 2.0d0)
  c(1,2) = (3.0d0, 3.0d0)
  c(2,2) = (4.0d0, 4.0d0)
  alpha = (1.0d0, 0.0d0)
  beta = (0.5d0, 0.0d0)
  call zgemm('T', 'C', m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
  call begin_test('zgemm_TC_beta_nonzero')
  call print_cmatrix('c', c_r, ldc, m, n)
  call end_test()

  ! Test 22: transA='T', transB='T' with non-zero beta
  m = 2
  n = 2
  k = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  c = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 0.0d0)
  a(2,1) = (0.0d0, 0.0d0)
  a(1,2) = (0.0d0, 0.0d0)
  a(2,2) = (1.0d0, 0.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (2.0d0, 1.0d0)
  b(2,2) = (1.0d0, -1.0d0)
  c(1,1) = (1.0d0, 1.0d0)
  c(2,1) = (2.0d0, 2.0d0)
  c(1,2) = (3.0d0, 3.0d0)
  c(2,2) = (4.0d0, 4.0d0)
  alpha = (1.0d0, 0.0d0)
  beta = (0.5d0, 0.0d0)
  call zgemm('T', 'T', m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
  call begin_test('zgemm_TT_beta_nonzero')
  call print_cmatrix('c', c_r, ldc, m, n)
  call end_test()

contains
  subroutine print_cmatrix(name, arr, lda_val, m_val, n_val)
    character(*), intent(in) :: name
    integer, intent(in) :: lda_val, m_val, n_val
    double precision, intent(in) :: arr(2*lda_val, *)
    integer :: i, j
    logical :: first
    write(*, '(A,A,A)', advance='no') ',"', trim(name), '":['
    first = .true.
    do j = 1, n_val
      do i = 1, m_val
        if (.not. first) write(*, '(A)', advance='no') ','
        first = .false.
        write(*, '(ES25.17E3)', advance='no') arr(2*(i-1)+1, j)
        write(*, '(A)', advance='no') ','
        write(*, '(ES25.17E3)', advance='no') arr(2*(i-1)+2, j)
      end do
    end do
    write(*, '(A)', advance='no') ']'
  end subroutine

end program
