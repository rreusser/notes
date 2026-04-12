program test_zla_heamv
  use test_utils
  implicit none
  integer, parameter :: BLAS_UPPER = 121
  integer, parameter :: BLAS_LOWER = 122
  integer, parameter :: LDA = 4

  complex*16 :: A(LDA, LDA), x(4)
  double precision :: A_r(2*LDA*LDA)
  double precision :: x_r(8)
  equivalence (A, A_r)
  equivalence (x, x_r)
  double precision :: y(7)
  complex*16 :: xneg(7)
  double precision :: xneg_r(14)
  equivalence (xneg, xneg_r)
  integer :: n

  n = 3

  ! Hermitian test matrix (diagonal real, off-diagonal complex, conjugate-symmetric)
  ! A = [  2.0         (1.0,2.0)   (3.0,-1.0) ]
  !     [ (1.0,-2.0)    5.0        (0.5, 1.5) ]
  !     [ (3.0,1.0)    (0.5,-1.5)   4.0       ]
  A = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0,  0.0d0)
  A(1,2) = (1.0d0,  2.0d0)
  A(1,3) = (3.0d0, -1.0d0)
  A(2,1) = (1.0d0, -2.0d0)
  A(2,2) = (5.0d0,  0.0d0)
  A(2,3) = (0.5d0,  1.5d0)
  A(3,1) = (3.0d0,  1.0d0)
  A(3,2) = (0.5d0, -1.5d0)
  A(3,3) = (4.0d0,  0.0d0)

  x(1) = (1.0d0,  0.5d0)
  x(2) = (-2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)

  ! Test 1: upper, alpha=1, beta=0
  y = 0.0d0
  call ZLA_HEAMV(BLAS_UPPER, n, 1.0d0, A, LDA, x, 1, 0.0d0, y, 1)
  call begin_test('upper_basic')
  call print_array('y', y, n)
  call end_test()

  ! Test 2: lower, alpha=1, beta=0
  y = 0.0d0
  call ZLA_HEAMV(BLAS_LOWER, n, 1.0d0, A, LDA, x, 1, 0.0d0, y, 1)
  call begin_test('lower_basic')
  call print_array('y', y, n)
  call end_test()

  ! Test 3: upper, alpha=2, beta=0.5, nonzero y
  y(1) = -1.0d0; y(2) = 2.0d0; y(3) = -3.0d0
  call ZLA_HEAMV(BLAS_UPPER, n, 2.0d0, A, LDA, x, 1, 0.5d0, y, 1)
  call begin_test('upper_scaled')
  call print_array('y', y, n)
  call end_test()

  ! Test 4: lower, alpha=2, beta=0.5, nonzero y
  y(1) = -1.0d0; y(2) = 2.0d0; y(3) = -3.0d0
  call ZLA_HEAMV(BLAS_LOWER, n, 2.0d0, A, LDA, x, 1, 0.5d0, y, 1)
  call begin_test('lower_scaled')
  call print_array('y', y, n)
  call end_test()

  ! Test 5: quick return n=0
  y(1) = 99.0d0; y(2) = 99.0d0; y(3) = 99.0d0
  call ZLA_HEAMV(BLAS_UPPER, 0, 1.0d0, A, LDA, x, 1, 0.0d0, y, 1)
  call begin_test('quick_return_n_zero')
  call print_array('y', y, 3)
  call end_test()

  ! Test 6: alpha=0, beta=1 quick return
  y(1) = 7.0d0; y(2) = 8.0d0; y(3) = 9.0d0
  call ZLA_HEAMV(BLAS_UPPER, n, 0.0d0, A, LDA, x, 1, 1.0d0, y, 1)
  call begin_test('alpha_zero_beta_one')
  call print_array('y', y, n)
  call end_test()

  ! Test 7: alpha=0, beta=2 (not quick return; scales |y|)
  y(1) = -1.0d0; y(2) = 2.0d0; y(3) = -3.0d0
  call ZLA_HEAMV(BLAS_UPPER, n, 0.0d0, A, LDA, x, 1, 2.0d0, y, 1)
  call begin_test('alpha_zero_beta_two')
  call print_array('y', y, n)
  call end_test()

  ! Test 8: upper, negative incy
  y = 0.0d0
  y(1) = 10.0d0; y(2) = 20.0d0; y(3) = 30.0d0
  call ZLA_HEAMV(BLAS_UPPER, n, 1.0d0, A, LDA, x, 1, 1.0d0, y, -1)
  call begin_test('upper_negincy')
  call print_array('y', y, n)
  call end_test()

  ! Test 9: lower with non-unit incx
  xneg = (0.0d0, 0.0d0)
  xneg(1) = (1.0d0, 0.5d0)
  xneg(3) = (-2.0d0, 1.0d0)
  xneg(5) = (3.0d0, -1.0d0)
  y = 0.0d0
  call ZLA_HEAMV(BLAS_LOWER, n, 1.0d0, A, LDA, xneg, 2, 0.0d0, y, 1)
  call begin_test('lower_incx2')
  call print_array('y', y, n)
  call end_test()

  ! Test 10: symbolic zero — A is all zero and y is zero
  A = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.5d0); x(2) = (1.0d0, 0.0d0); x(3) = (1.0d0, 1.0d0)
  y(1) = 0.0d0; y(2) = 0.0d0; y(3) = 0.0d0
  call ZLA_HEAMV(BLAS_UPPER, n, 1.0d0, A, LDA, x, 1, 0.0d0, y, 1)
  call begin_test('symbolic_zero_upper')
  call print_array('y', y, n)
  call end_test()

  ! Test 11: symbolic zero, lower
  y(1) = 0.0d0; y(2) = 0.0d0; y(3) = 0.0d0
  call ZLA_HEAMV(BLAS_LOWER, n, 1.0d0, A, LDA, x, 1, 0.0d0, y, 1)
  call begin_test('symbolic_zero_lower')
  call print_array('y', y, n)
  call end_test()

  ! Test 12: n=1 edge case
  A(1,1) = (4.0d0, 0.0d0)
  x(1) = (-2.0d0, 1.0d0)
  y(1) = 0.0d0
  call ZLA_HEAMV(BLAS_UPPER, 1, 1.0d0, A, LDA, x, 1, 0.0d0, y, 1)
  call begin_test('n_one_upper')
  call print_array('y', y, 1)
  call end_test()

  y(1) = 0.0d0
  call ZLA_HEAMV(BLAS_LOWER, 1, 1.0d0, A, LDA, x, 1, 0.0d0, y, 1)
  call begin_test('n_one_lower')
  call print_array('y', y, 1)
  call end_test()

  ! Test 13: upper, negative incx, negative incy
  ! Restore the Hermitian matrix
  A = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0,  0.0d0)
  A(1,2) = (1.0d0,  2.0d0)
  A(1,3) = (3.0d0, -1.0d0)
  A(2,1) = (1.0d0, -2.0d0)
  A(2,2) = (5.0d0,  0.0d0)
  A(2,3) = (0.5d0,  1.5d0)
  A(3,1) = (3.0d0,  1.0d0)
  A(3,2) = (0.5d0, -1.5d0)
  A(3,3) = (4.0d0,  0.0d0)

  x(1) = (1.0d0, 0.5d0)
  x(2) = (-2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  y(1) = 1.0d0; y(2) = 2.0d0; y(3) = 3.0d0
  call ZLA_HEAMV(BLAS_UPPER, n, 1.0d0, A, LDA, x, -1, 1.0d0, y, -1)
  call begin_test('upper_negincx_negincy')
  call print_array('y', y, n)
  call end_test()

end program
