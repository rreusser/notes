program test_zla_syamv
  use test_utils
  implicit none
  integer, parameter :: BLAS_UPPER = 121
  integer, parameter :: BLAS_LOWER = 122
  complex*16 :: A(4, 4), x(4), xbig(6)
  double precision :: A_r(32), x_r(8), xbig_r(12)
  equivalence (A, A_r)
  equivalence (x, x_r)
  equivalence (xbig, xbig_r)
  double precision :: y(4), yneg(7)
  integer :: n

  ! Symmetric complex test matrix (upper triangle)
  ! A = [ (1,2)   (-2,1)   (3,-1) ]
  !     [ (-2,1)  (5,0)    (-6,2) ]
  !     [ (3,-1)  (-6,2)   (9,3)  ]
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 2.0d0);  A(1,2) = (-2.0d0, 1.0d0); A(1,3) = (3.0d0, -1.0d0)
  A(2,1) = (-2.0d0, 1.0d0); A(2,2) = (5.0d0, 0.0d0);  A(2,3) = (-6.0d0, 2.0d0)
  A(3,1) = (3.0d0, -1.0d0); A(3,2) = (-6.0d0, 2.0d0); A(3,3) = (9.0d0, 3.0d0)

  x(1) = (1.0d0, -1.0d0)
  x(2) = (-2.0d0, 0.5d0)
  x(3) = (3.0d0, 2.0d0)
  n = 3

  ! Test 1: upper, alpha=1, beta=0
  y = 0.0d0
  call ZLA_SYAMV(BLAS_UPPER, n, 1.0d0, A, 4, x, 1, 0.0d0, y, 1)
  call begin_test('upper_basic')
  call print_array('A', A_r, 2*4*n)
  call print_array('x', x_r, 2*n)
  call print_array('y', y, n)
  call end_test()

  ! Test 2: lower, alpha=1, beta=0
  y = 0.0d0
  call ZLA_SYAMV(BLAS_LOWER, n, 1.0d0, A, 4, x, 1, 0.0d0, y, 1)
  call begin_test('lower_basic')
  call print_array('y', y, n)
  call end_test()

  ! Test 3: upper, alpha=2, beta=0.5, nonzero y
  y(1) = -1.0d0; y(2) = 2.0d0; y(3) = -3.0d0
  call ZLA_SYAMV(BLAS_UPPER, n, 2.0d0, A, 4, x, 1, 0.5d0, y, 1)
  call begin_test('upper_scaled')
  call print_array('y', y, n)
  call end_test()

  ! Test 4: lower, alpha=2, beta=0.5, nonzero y
  y(1) = -1.0d0; y(2) = 2.0d0; y(3) = -3.0d0
  call ZLA_SYAMV(BLAS_LOWER, n, 2.0d0, A, 4, x, 1, 0.5d0, y, 1)
  call begin_test('lower_scaled')
  call print_array('y', y, n)
  call end_test()

  ! Test 5: quick return n=0
  y(1) = 99.0d0; y(2) = 99.0d0; y(3) = 99.0d0
  call ZLA_SYAMV(BLAS_UPPER, 0, 1.0d0, A, 4, x, 1, 0.0d0, y, 1)
  call begin_test('quick_return_n_zero')
  call print_array('y', y, 3)
  call end_test()

  ! Test 6: alpha=0, beta=1 quick return
  y(1) = 7.0d0; y(2) = 8.0d0; y(3) = 9.0d0
  call ZLA_SYAMV(BLAS_UPPER, n, 0.0d0, A, 4, x, 1, 1.0d0, y, 1)
  call begin_test('alpha_zero_beta_one')
  call print_array('y', y, n)
  call end_test()

  ! Test 7: alpha=0, beta=2 (not a quick return; scales |y|)
  y(1) = -1.0d0; y(2) = 2.0d0; y(3) = -3.0d0
  call ZLA_SYAMV(BLAS_UPPER, n, 0.0d0, A, 4, x, 1, 2.0d0, y, 1)
  call begin_test('alpha_zero_beta_two')
  call print_array('y', y, n)
  call end_test()

  ! Test 8: upper, negative incy
  yneg = 0.0d0
  yneg(1) = 10.0d0; yneg(2) = 20.0d0; yneg(3) = 30.0d0
  call ZLA_SYAMV(BLAS_UPPER, n, 1.0d0, A, 4, x, 1, 1.0d0, yneg, -1)
  call begin_test('upper_negincy')
  call print_array('y', yneg, n)
  call end_test()

  ! Test 9: symbolic zero — A is all zero and y is zero
  A = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0); x(2) = (1.0d0, 0.0d0); x(3) = (1.0d0, 0.0d0)
  y(1) = 0.0d0; y(2) = 0.0d0; y(3) = 0.0d0
  call ZLA_SYAMV(BLAS_UPPER, n, 1.0d0, A, 4, x, 1, 0.0d0, y, 1)
  call begin_test('symbolic_zero_upper')
  call print_array('y', y, n)
  call end_test()

  ! Test 10: symbolic zero, lower
  y(1) = 0.0d0; y(2) = 0.0d0; y(3) = 0.0d0
  call ZLA_SYAMV(BLAS_LOWER, n, 1.0d0, A, 4, x, 1, 0.0d0, y, 1)
  call begin_test('symbolic_zero_lower')
  call print_array('y', y, n)
  call end_test()

  ! Test 11: n=1 edge case
  A(1,1) = (4.0d0, -3.0d0)
  x(1) = (-2.0d0, 1.0d0)
  y(1) = 0.0d0
  call ZLA_SYAMV(BLAS_UPPER, 1, 1.0d0, A, 4, x, 1, 0.0d0, y, 1)
  call begin_test('n_one_upper')
  call print_array('y', y, 1)
  call end_test()

  y(1) = 0.0d0
  call ZLA_SYAMV(BLAS_LOWER, 1, 1.0d0, A, 4, x, 1, 0.0d0, y, 1)
  call begin_test('n_one_lower')
  call print_array('y', y, 1)
  call end_test()

  ! Test 12: lower with non-unit incx=2
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 2.0d0);  A(1,2) = (-2.0d0, 1.0d0); A(1,3) = (3.0d0, -1.0d0)
  A(2,1) = (-2.0d0, 1.0d0); A(2,2) = (5.0d0, 0.0d0);  A(2,3) = (-6.0d0, 2.0d0)
  A(3,1) = (3.0d0, -1.0d0); A(3,2) = (-6.0d0, 2.0d0); A(3,3) = (9.0d0, 3.0d0)
  xbig = (0.0d0, 0.0d0)
  xbig(1) = (1.0d0, -1.0d0)
  xbig(3) = (-2.0d0, 0.5d0)
  xbig(5) = (3.0d0, 2.0d0)
  y = 0.0d0
  call ZLA_SYAMV(BLAS_LOWER, n, 1.0d0, A, 4, xbig, 2, 0.0d0, y, 1)
  call begin_test('lower_incx2')
  call print_array('x', xbig_r, 2*6)
  call print_array('y', y, n)
  call end_test()

end program
