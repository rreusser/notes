program test_dgemm
  use test_utils
  implicit none
  double precision :: a(20), b(20), c(20)

  ! Test 1: basic N,N — C = 1.0*A*B + 0.0*C
  ! A = [1 3; 2 4] (2x2, col-major), B = [5 7; 6 8] (2x2)
  ! C = A*B = [1*5+3*6, 2*5+4*6; 1*7+3*8, 2*7+4*8] = [23, 34; 31, 46]
  a = 0.0d0; b = 0.0d0; c = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0; a(4) = 4.0d0
  b(1) = 5.0d0; b(2) = 6.0d0; b(3) = 7.0d0; b(4) = 8.0d0
  call dgemm('N', 'N', 2, 2, 2, 1.0d0, a, 2, b, 2, 0.0d0, c, 2)
  call begin_test('basic_nn')
  call print_matrix('C', c, 2, 2, 2)
  call end_test()

  ! Test 2: T,N — C = 1.0*A^T*B + 0.0*C
  ! A = [1 2; 3 4] stored as [1,3,2,4] col-major (2x2)
  ! A^T = [1 3; 2 4]
  ! B = [5 7; 6 8]
  ! C = A^T*B = [1*5+2*6, 3*5+4*6; 1*7+2*8, 3*7+4*8] = [17, 39; 23, 53]
  ! But A is (K x M) in T case, so nrowa=K=2
  ! transA='T', M=2, N=2, K=2: A is K x M = 2x2
  a = 0.0d0; b = 0.0d0; c = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0; a(4) = 4.0d0
  b(1) = 5.0d0; b(2) = 6.0d0; b(3) = 7.0d0; b(4) = 8.0d0
  call dgemm('T', 'N', 2, 2, 2, 1.0d0, a, 2, b, 2, 0.0d0, c, 2)
  call begin_test('tn')
  call print_matrix('C', c, 2, 2, 2)
  call end_test()

  ! Test 3: N,T — C = 1.0*A*B^T + 0.0*C
  ! A = [1 3; 2 4] (2x2, col-major)
  ! B stored as [5 6; 7 8] col-major (N x K = 2x2)
  ! B^T = [5 7; 6 8]... Wait, in N,T: B is transposed, so B is (N x K)
  ! transB='T': nrowb=N=2, so B is NxK=2x2
  ! B^T(l,j) = B(j,l)
  a = 0.0d0; b = 0.0d0; c = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0; a(4) = 4.0d0
  b(1) = 5.0d0; b(2) = 7.0d0; b(3) = 6.0d0; b(4) = 8.0d0
  call dgemm('N', 'T', 2, 2, 2, 1.0d0, a, 2, b, 2, 0.0d0, c, 2)
  call begin_test('nt')
  call print_matrix('C', c, 2, 2, 2)
  call end_test()

  ! Test 4: alpha=0, beta=2.0 — C = 0*A*B + 2*C
  c = 0.0d0
  c(1) = 1.0d0; c(2) = 2.0d0; c(3) = 3.0d0; c(4) = 4.0d0
  call dgemm('N', 'N', 2, 2, 2, 0.0d0, a, 2, b, 2, 2.0d0, c, 2)
  call begin_test('alpha_zero')
  call print_matrix('C', c, 2, 2, 2)
  call end_test()

  ! Test 5: beta=0 — C = alpha*A*B + 0*C (C garbage should be overwritten)
  a = 0.0d0; b = 0.0d0; c = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0; a(4) = 4.0d0
  b(1) = 5.0d0; b(2) = 6.0d0; b(3) = 7.0d0; b(4) = 8.0d0
  c(1) = 999.0d0; c(2) = 999.0d0; c(3) = 999.0d0; c(4) = 999.0d0
  call dgemm('N', 'N', 2, 2, 2, 1.0d0, a, 2, b, 2, 0.0d0, c, 2)
  call begin_test('beta_zero')
  call print_matrix('C', c, 2, 2, 2)
  call end_test()

  ! Test 6: M=0 quick return
  c = 0.0d0
  c(1) = 99.0d0
  call dgemm('N', 'N', 0, 2, 2, 1.0d0, a, 1, b, 2, 0.0d0, c, 1)
  call begin_test('m_zero')
  call print_matrix('C', c, 1, 1, 1)
  call end_test()

  ! Test 7: alpha and beta scaling
  ! C = 2.0*A*B + 3.0*C
  a = 0.0d0; b = 0.0d0; c = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0; a(4) = 4.0d0
  b(1) = 5.0d0; b(2) = 6.0d0; b(3) = 7.0d0; b(4) = 8.0d0
  c(1) = 1.0d0; c(2) = 1.0d0; c(3) = 1.0d0; c(4) = 1.0d0
  call dgemm('N', 'N', 2, 2, 2, 2.0d0, a, 2, b, 2, 3.0d0, c, 2)
  call begin_test('alpha_beta')
  call print_matrix('C', c, 2, 2, 2)
  call end_test()

  ! Test 8: Non-square M=3, N=2, K=2
  ! A is 3x2, B is 2x2, C is 3x2
  a = 0.0d0; b = 0.0d0; c = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0
  a(4) = 4.0d0; a(5) = 5.0d0; a(6) = 6.0d0
  b(1) = 1.0d0; b(2) = 0.0d0; b(3) = 0.0d0; b(4) = 1.0d0
  call dgemm('N', 'N', 3, 2, 2, 1.0d0, a, 3, b, 2, 0.0d0, c, 3)
  call begin_test('nonsquare')
  call print_matrix('C', c, 3, 3, 2)
  call end_test()

  ! Test 9: T,T — C = A^T * B^T
  a = 0.0d0; b = 0.0d0; c = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0; a(4) = 4.0d0
  b(1) = 5.0d0; b(2) = 6.0d0; b(3) = 7.0d0; b(4) = 8.0d0
  call dgemm('T', 'T', 2, 2, 2, 1.0d0, a, 2, b, 2, 0.0d0, c, 2)
  call begin_test('tt')
  call print_matrix('C', c, 2, 2, 2)
  call end_test()

end program
