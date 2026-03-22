program test_zherk
  use test_utils
  implicit none
  complex*16 :: a(100), c(100)
  double precision :: a_r(200), c_r(200)
  equivalence (a, a_r)
  equivalence (c, c_r)
  integer :: lda, ldc

  ! Test 1: upper, N (no-transpose), N=3, K=2
  ! C := alpha*A*A^H + beta*C, A is 3x2, C is 3x3 upper
  a = (0.0d0, 0.0d0); c = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 2.0d0); a(2) = (3.0d0, 4.0d0); a(3) = (5.0d0, 6.0d0)
  a(4) = (7.0d0, 8.0d0); a(5) = (9.0d0, 10.0d0); a(6) = (11.0d0, 12.0d0)
  c(1) = (1.0d0, 0.0d0); c(5) = (1.0d0, 0.0d0); c(9) = (1.0d0, 0.0d0)
  call zherk('U', 'N', 3, 2, 2.0d0, a, 3, 1.0d0, c, 3)
  call begin_test('upper_N')
  call print_array('c', c_r, 18)
  call end_test()

  ! Test 2: lower, N (no-transpose), N=3, K=2
  a = (0.0d0, 0.0d0); c = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 2.0d0); a(2) = (3.0d0, 4.0d0); a(3) = (5.0d0, 6.0d0)
  a(4) = (7.0d0, 8.0d0); a(5) = (9.0d0, 10.0d0); a(6) = (11.0d0, 12.0d0)
  c(1) = (1.0d0, 0.0d0); c(5) = (1.0d0, 0.0d0); c(9) = (1.0d0, 0.0d0)
  call zherk('L', 'N', 3, 2, 2.0d0, a, 3, 1.0d0, c, 3)
  call begin_test('lower_N')
  call print_array('c', c_r, 18)
  call end_test()

  ! Test 3: upper, C (conjugate-transpose), N=3, K=2
  ! C := alpha*A^H*A + beta*C, A is 2x3
  a = (0.0d0, 0.0d0); c = (0.0d0, 0.0d0)
  ! A is 2x3, col-major: A(1,1)=(1,2) A(2,1)=(3,4) A(1,2)=(5,6) A(2,2)=(7,8) A(1,3)=(9,10) A(2,3)=(11,12)
  a(1) = (1.0d0, 2.0d0); a(2) = (3.0d0, 4.0d0)
  a(3) = (5.0d0, 6.0d0); a(4) = (7.0d0, 8.0d0)
  a(5) = (9.0d0, 10.0d0); a(6) = (11.0d0, 12.0d0)
  c(1) = (1.0d0, 0.0d0); c(5) = (1.0d0, 0.0d0); c(9) = (1.0d0, 0.0d0)
  call zherk('U', 'C', 3, 2, 2.0d0, a, 2, 1.0d0, c, 3)
  call begin_test('upper_C')
  call print_array('c', c_r, 18)
  call end_test()

  ! Test 4: lower, C (conjugate-transpose), N=3, K=2
  a = (0.0d0, 0.0d0); c = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 2.0d0); a(2) = (3.0d0, 4.0d0)
  a(3) = (5.0d0, 6.0d0); a(4) = (7.0d0, 8.0d0)
  a(5) = (9.0d0, 10.0d0); a(6) = (11.0d0, 12.0d0)
  c(1) = (1.0d0, 0.0d0); c(5) = (1.0d0, 0.0d0); c(9) = (1.0d0, 0.0d0)
  call zherk('L', 'C', 3, 2, 2.0d0, a, 2, 1.0d0, c, 3)
  call begin_test('lower_C')
  call print_array('c', c_r, 18)
  call end_test()

  ! Test 5: alpha=0, beta=2 (just scale C), upper
  c = (0.0d0, 0.0d0)
  c(1) = (2.0d0, 0.0d0); c(4) = (3.0d0, 1.0d0); c(5) = (4.0d0, 0.0d0)
  c(7) = (5.0d0, 2.0d0); c(8) = (6.0d0, 3.0d0); c(9) = (7.0d0, 0.0d0)
  call zherk('U', 'N', 3, 2, 0.0d0, a, 3, 2.0d0, c, 3)
  call begin_test('alpha_zero')
  call print_array('c', c_r, 18)
  call end_test()

  ! Test 6: alpha=0, beta=0 (zero out upper triangle)
  c = (0.0d0, 0.0d0)
  c(1) = (5.0d0, 0.0d0); c(4) = (6.0d0, 1.0d0); c(5) = (7.0d0, 0.0d0)
  c(7) = (8.0d0, 2.0d0); c(8) = (9.0d0, 3.0d0); c(9) = (10.0d0, 0.0d0)
  call zherk('U', 'N', 3, 2, 0.0d0, a, 3, 0.0d0, c, 3)
  call begin_test('alpha_zero_beta_zero')
  call print_array('c', c_r, 18)
  call end_test()

  ! Test 7: alpha=0, beta=0, lower
  c = (0.0d0, 0.0d0)
  c(1) = (5.0d0, 0.0d0); c(2) = (6.0d0, 1.0d0); c(3) = (7.0d0, 2.0d0)
  c(5) = (8.0d0, 0.0d0); c(6) = (9.0d0, 3.0d0); c(9) = (10.0d0, 0.0d0)
  call zherk('L', 'N', 3, 2, 0.0d0, a, 3, 0.0d0, c, 3)
  call begin_test('alpha_zero_beta_zero_lower')
  call print_array('c', c_r, 18)
  call end_test()

  ! Test 8: alpha=0, beta scale, lower
  c = (0.0d0, 0.0d0)
  c(1) = (2.0d0, 0.0d0); c(2) = (3.0d0, 1.0d0); c(3) = (5.0d0, 2.0d0)
  c(5) = (4.0d0, 0.0d0); c(6) = (6.0d0, 3.0d0); c(9) = (7.0d0, 0.0d0)
  call zherk('L', 'N', 3, 2, 0.0d0, a, 3, 3.0d0, c, 3)
  call begin_test('alpha_zero_beta_scale_lower')
  call print_array('c', c_r, 18)
  call end_test()

  ! Test 9: beta=0 upper N
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 2.0d0); a(2) = (3.0d0, 4.0d0); a(3) = (5.0d0, 6.0d0)
  a(4) = (7.0d0, 8.0d0); a(5) = (9.0d0, 10.0d0); a(6) = (11.0d0, 12.0d0)
  c = (0.0d0, 0.0d0)
  c(1) = (99.0d0, 0.0d0); c(5) = (99.0d0, 0.0d0); c(9) = (99.0d0, 0.0d0)
  call zherk('U', 'N', 3, 2, 1.0d0, a, 3, 0.0d0, c, 3)
  call begin_test('beta_zero')
  call print_array('c', c_r, 18)
  call end_test()

  ! Test 10: upper_N with beta != 0 and beta != 1
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 2.0d0); a(2) = (3.0d0, 4.0d0); a(3) = (5.0d0, 6.0d0)
  a(4) = (7.0d0, 8.0d0); a(5) = (9.0d0, 10.0d0); a(6) = (11.0d0, 12.0d0)
  c = (0.0d0, 0.0d0)
  c(1) = (1.0d0, 0.0d0); c(5) = (1.0d0, 0.0d0); c(9) = (1.0d0, 0.0d0)
  call zherk('U', 'N', 3, 2, 1.0d0, a, 3, 0.5d0, c, 3)
  call begin_test('upper_N_beta_half')
  call print_array('c', c_r, 18)
  call end_test()

  ! Test 11: lower_N with beta=0
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 2.0d0); a(2) = (3.0d0, 4.0d0); a(3) = (5.0d0, 6.0d0)
  a(4) = (7.0d0, 8.0d0); a(5) = (9.0d0, 10.0d0); a(6) = (11.0d0, 12.0d0)
  c = (0.0d0, 0.0d0)
  c(1) = (99.0d0, 0.0d0); c(5) = (99.0d0, 0.0d0); c(9) = (99.0d0, 0.0d0)
  call zherk('L', 'N', 3, 2, 1.0d0, a, 3, 0.0d0, c, 3)
  call begin_test('lower_N_beta_zero')
  call print_array('c', c_r, 18)
  call end_test()

  ! Test 12: lower_N with beta != 0, beta != 1
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 2.0d0); a(2) = (3.0d0, 4.0d0); a(3) = (5.0d0, 6.0d0)
  a(4) = (7.0d0, 8.0d0); a(5) = (9.0d0, 10.0d0); a(6) = (11.0d0, 12.0d0)
  c = (0.0d0, 0.0d0)
  c(1) = (1.0d0, 0.0d0); c(5) = (1.0d0, 0.0d0); c(9) = (1.0d0, 0.0d0)
  call zherk('L', 'N', 3, 2, 1.0d0, a, 3, 0.5d0, c, 3)
  call begin_test('lower_N_beta_half')
  call print_array('c', c_r, 18)
  call end_test()

  ! Test 13: upper_C with beta=0
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 2.0d0); a(2) = (3.0d0, 4.0d0)
  a(3) = (5.0d0, 6.0d0); a(4) = (7.0d0, 8.0d0)
  a(5) = (9.0d0, 10.0d0); a(6) = (11.0d0, 12.0d0)
  c = (0.0d0, 0.0d0)
  c(1) = (99.0d0, 0.0d0); c(5) = (99.0d0, 0.0d0); c(9) = (99.0d0, 0.0d0)
  call zherk('U', 'C', 3, 2, 1.0d0, a, 2, 0.0d0, c, 3)
  call begin_test('upper_C_beta_zero')
  call print_array('c', c_r, 18)
  call end_test()

  ! Test 14: lower_C with beta=0
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 2.0d0); a(2) = (3.0d0, 4.0d0)
  a(3) = (5.0d0, 6.0d0); a(4) = (7.0d0, 8.0d0)
  a(5) = (9.0d0, 10.0d0); a(6) = (11.0d0, 12.0d0)
  c = (0.0d0, 0.0d0)
  c(1) = (99.0d0, 0.0d0); c(5) = (99.0d0, 0.0d0); c(9) = (99.0d0, 0.0d0)
  call zherk('L', 'C', 3, 2, 1.0d0, a, 2, 0.0d0, c, 3)
  call begin_test('lower_C_beta_zero')
  call print_array('c', c_r, 18)
  call end_test()

end program
