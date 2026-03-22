program test_dsyr2k
  use test_utils
  implicit none
  double precision :: a(100), b(100), c(100)

  ! Test 1: upper, no transpose, 3x3, K=2
  ! C := alpha*A*B^T + alpha*B*A^T + beta*C, A is 3x2, B is 3x2, C is 3x3 upper
  a = 0.0d0; b = 0.0d0; c = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0
  a(4) = 4.0d0; a(5) = 5.0d0; a(6) = 6.0d0
  b(1) = 0.5d0; b(2) = 1.5d0; b(3) = 2.5d0
  b(4) = 3.5d0; b(5) = 4.5d0; b(6) = 5.5d0
  c(1) = 1.0d0; c(5) = 1.0d0; c(9) = 1.0d0
  call dsyr2k('U', 'N', 3, 2, 2.0d0, a, 3, b, 3, 1.0d0, c, 3)
  call begin_test('upper_N')
  call print_matrix('c', c, 3, 3, 3)
  call end_test()

  ! Test 2: lower, no transpose, 3x3, K=2
  a = 0.0d0; b = 0.0d0; c = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0
  a(4) = 4.0d0; a(5) = 5.0d0; a(6) = 6.0d0
  b(1) = 0.5d0; b(2) = 1.5d0; b(3) = 2.5d0
  b(4) = 3.5d0; b(5) = 4.5d0; b(6) = 5.5d0
  c(1) = 1.0d0; c(5) = 1.0d0; c(9) = 1.0d0
  call dsyr2k('L', 'N', 3, 2, 2.0d0, a, 3, b, 3, 1.0d0, c, 3)
  call begin_test('lower_N')
  call print_matrix('c', c, 3, 3, 3)
  call end_test()

  ! Test 3: upper, transpose, 3x3, K=2
  ! C := alpha*A^T*B + alpha*B^T*A + beta*C, A is 2x3, B is 2x3, C is 3x3 upper
  a = 0.0d0; b = 0.0d0; c = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0
  a(3) = 3.0d0; a(4) = 4.0d0
  a(5) = 5.0d0; a(6) = 6.0d0
  b(1) = 0.5d0; b(2) = 1.5d0
  b(3) = 2.5d0; b(4) = 3.5d0
  b(5) = 4.5d0; b(6) = 5.5d0
  c(1) = 1.0d0; c(5) = 1.0d0; c(9) = 1.0d0
  call dsyr2k('U', 'T', 3, 2, 2.0d0, a, 2, b, 2, 1.0d0, c, 3)
  call begin_test('upper_T')
  call print_matrix('c', c, 3, 3, 3)
  call end_test()

  ! Test 4: lower, transpose
  a = 0.0d0; b = 0.0d0; c = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0
  a(3) = 3.0d0; a(4) = 4.0d0
  a(5) = 5.0d0; a(6) = 6.0d0
  b(1) = 0.5d0; b(2) = 1.5d0
  b(3) = 2.5d0; b(4) = 3.5d0
  b(5) = 4.5d0; b(6) = 5.5d0
  c(1) = 1.0d0; c(5) = 1.0d0; c(9) = 1.0d0
  call dsyr2k('L', 'T', 3, 2, 2.0d0, a, 2, b, 2, 1.0d0, c, 3)
  call begin_test('lower_T')
  call print_matrix('c', c, 3, 3, 3)
  call end_test()

  ! Test 5: alpha=0, beta=2 (just scale C upper)
  c = 0.0d0
  c(1) = 2.0d0; c(4) = 3.0d0; c(5) = 4.0d0; c(7) = 5.0d0; c(8) = 6.0d0; c(9) = 7.0d0
  call dsyr2k('U', 'N', 3, 2, 0.0d0, a, 3, b, 3, 2.0d0, c, 3)
  call begin_test('alpha_zero')
  call print_matrix('c', c, 3, 3, 3)
  call end_test()

  ! Test 6: beta=0 (zero out C before update)
  a = 0.0d0; b = 0.0d0; c = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0
  a(4) = 4.0d0; a(5) = 5.0d0; a(6) = 6.0d0
  b(1) = 0.5d0; b(2) = 1.5d0; b(3) = 2.5d0
  b(4) = 3.5d0; b(5) = 4.5d0; b(6) = 5.5d0
  c(1) = 99.0d0; c(5) = 99.0d0; c(9) = 99.0d0
  call dsyr2k('U', 'N', 3, 2, 1.0d0, a, 3, b, 3, 0.0d0, c, 3)
  call begin_test('beta_zero')
  call print_matrix('c', c, 3, 3, 3)
  call end_test()

  ! Test 7: N=0 quick return
  call dsyr2k('U', 'N', 0, 2, 1.0d0, a, 1, b, 1, 1.0d0, c, 1)
  call begin_test('n_zero')
  call print_int('done', 1)
  call end_test()

  ! Test 8: alpha=0, beta=0 -> zero upper triangle
  c = 0.0d0
  c(1) = 5.0d0; c(4) = 6.0d0; c(5) = 7.0d0; c(7) = 8.0d0; c(8) = 9.0d0; c(9) = 10.0d0
  call dsyr2k('U', 'N', 3, 2, 0.0d0, a, 3, b, 3, 0.0d0, c, 3)
  call begin_test('alpha_zero_beta_zero')
  call print_matrix('c', c, 3, 3, 3)
  call end_test()

  ! Test 9: alpha=0, beta=0, lower -> zero lower triangle
  c = 0.0d0
  c(1) = 5.0d0; c(2) = 6.0d0; c(3) = 7.0d0; c(5) = 8.0d0; c(6) = 9.0d0; c(9) = 10.0d0
  call dsyr2k('L', 'N', 3, 2, 0.0d0, a, 3, b, 3, 0.0d0, c, 3)
  call begin_test('alpha_zero_beta_zero_lower')
  call print_matrix('c', c, 3, 3, 3)
  call end_test()

  ! Test 10: alpha=0, beta=3, upper -> scale upper triangle
  c = 0.0d0
  c(1) = 2.0d0; c(4) = 3.0d0; c(5) = 4.0d0; c(7) = 5.0d0; c(8) = 6.0d0; c(9) = 7.0d0
  call dsyr2k('U', 'N', 3, 2, 0.0d0, a, 3, b, 3, 3.0d0, c, 3)
  call begin_test('alpha_zero_beta_scale_upper')
  call print_matrix('c', c, 3, 3, 3)
  call end_test()

  ! Test 11: alpha=0, beta=3, lower -> scale lower triangle
  c = 0.0d0
  c(1) = 2.0d0; c(2) = 3.0d0; c(3) = 5.0d0; c(5) = 4.0d0; c(6) = 6.0d0; c(9) = 7.0d0
  call dsyr2k('L', 'N', 3, 2, 0.0d0, a, 3, b, 3, 3.0d0, c, 3)
  call begin_test('alpha_zero_beta_scale_lower')
  call print_matrix('c', c, 3, 3, 3)
  call end_test()

  ! Test 12: upper_N with beta=0.5
  a = 0.0d0; b = 0.0d0; c = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0
  a(4) = 4.0d0; a(5) = 5.0d0; a(6) = 6.0d0
  b(1) = 0.5d0; b(2) = 1.5d0; b(3) = 2.5d0
  b(4) = 3.5d0; b(5) = 4.5d0; b(6) = 5.5d0
  c(1) = 1.0d0; c(5) = 1.0d0; c(9) = 1.0d0
  call dsyr2k('U', 'N', 3, 2, 1.0d0, a, 3, b, 3, 0.5d0, c, 3)
  call begin_test('upper_N_beta_half')
  call print_matrix('c', c, 3, 3, 3)
  call end_test()

  ! Test 13: lower_N with beta=0
  a = 0.0d0; b = 0.0d0; c = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0
  a(4) = 4.0d0; a(5) = 5.0d0; a(6) = 6.0d0
  b(1) = 0.5d0; b(2) = 1.5d0; b(3) = 2.5d0
  b(4) = 3.5d0; b(5) = 4.5d0; b(6) = 5.5d0
  c(1) = 99.0d0; c(5) = 99.0d0; c(9) = 99.0d0
  call dsyr2k('L', 'N', 3, 2, 1.0d0, a, 3, b, 3, 0.0d0, c, 3)
  call begin_test('lower_N_beta_zero')
  call print_matrix('c', c, 3, 3, 3)
  call end_test()

  ! Test 14: lower_N with beta=0.5
  a = 0.0d0; b = 0.0d0; c = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0
  a(4) = 4.0d0; a(5) = 5.0d0; a(6) = 6.0d0
  b(1) = 0.5d0; b(2) = 1.5d0; b(3) = 2.5d0
  b(4) = 3.5d0; b(5) = 4.5d0; b(6) = 5.5d0
  c(1) = 1.0d0; c(5) = 1.0d0; c(9) = 1.0d0
  call dsyr2k('L', 'N', 3, 2, 1.0d0, a, 3, b, 3, 0.5d0, c, 3)
  call begin_test('lower_N_beta_half')
  call print_matrix('c', c, 3, 3, 3)
  call end_test()

  ! Test 15: K=0 with beta != 1 (should scale C)
  c = 0.0d0
  c(1) = 2.0d0; c(4) = 3.0d0; c(5) = 4.0d0; c(7) = 5.0d0; c(8) = 6.0d0; c(9) = 7.0d0
  call dsyr2k('U', 'N', 3, 0, 1.0d0, a, 3, b, 3, 2.0d0, c, 3)
  call begin_test('k_zero_beta_scale')
  call print_matrix('c', c, 3, 3, 3)
  call end_test()

  ! Test 16: upper_T with beta=0
  a = 0.0d0; b = 0.0d0; c = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0
  a(3) = 3.0d0; a(4) = 4.0d0
  a(5) = 5.0d0; a(6) = 6.0d0
  b(1) = 0.5d0; b(2) = 1.5d0
  b(3) = 2.5d0; b(4) = 3.5d0
  b(5) = 4.5d0; b(6) = 5.5d0
  c(1) = 99.0d0; c(5) = 99.0d0; c(9) = 99.0d0
  call dsyr2k('U', 'T', 3, 2, 1.0d0, a, 2, b, 2, 0.0d0, c, 3)
  call begin_test('upper_T_beta_zero')
  call print_matrix('c', c, 3, 3, 3)
  call end_test()

  ! Test 17: lower_T with beta=0
  a = 0.0d0; b = 0.0d0; c = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0
  a(3) = 3.0d0; a(4) = 4.0d0
  a(5) = 5.0d0; a(6) = 6.0d0
  b(1) = 0.5d0; b(2) = 1.5d0
  b(3) = 2.5d0; b(4) = 3.5d0
  b(5) = 4.5d0; b(6) = 5.5d0
  c(1) = 99.0d0; c(5) = 99.0d0; c(9) = 99.0d0
  call dsyr2k('L', 'T', 3, 2, 1.0d0, a, 2, b, 2, 0.0d0, c, 3)
  call begin_test('lower_T_beta_zero')
  call print_matrix('c', c, 3, 3, 3)
  call end_test()

end program
