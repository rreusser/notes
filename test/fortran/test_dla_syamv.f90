program test_dla_syamv
  use test_utils
  implicit none
  integer, parameter :: BLAS_UPPER = 121
  integer, parameter :: BLAS_LOWER = 122
  double precision :: A(4, 4), x(4), y(4), yneg(7), xneg(7)
  integer :: n

  ! Symmetric test matrix (only the referenced triangle values matter)
  ! A = [  1  -2   3  ]
  !     [ -2   5  -6  ]
  !     [  3  -6   9  ]
  A = 0.0d0
  A(1,1) =  1.0d0; A(1,2) = -2.0d0; A(1,3) =  3.0d0
  A(2,1) = -2.0d0; A(2,2) =  5.0d0; A(2,3) = -6.0d0
  A(3,1) =  3.0d0; A(3,2) = -6.0d0; A(3,3) =  9.0d0

  ! Test 1: upper, alpha=1, beta=0
  n = 3
  x(1) = 1.0d0; x(2) = -2.0d0; x(3) = 3.0d0
  y = 0.0d0
  call DLA_SYAMV(BLAS_UPPER, n, 1.0d0, A, 4, x, 1, 0.0d0, y, 1)
  call begin_test('upper_basic')
  call print_array('y', y, n)
  call end_test()

  ! Test 2: lower, alpha=1, beta=0 (should give same result since symmetric)
  y = 0.0d0
  call DLA_SYAMV(BLAS_LOWER, n, 1.0d0, A, 4, x, 1, 0.0d0, y, 1)
  call begin_test('lower_basic')
  call print_array('y', y, n)
  call end_test()

  ! Test 3: upper, alpha=2, beta=0.5, nonzero y
  y(1) = -1.0d0; y(2) = 2.0d0; y(3) = -3.0d0
  call DLA_SYAMV(BLAS_UPPER, n, 2.0d0, A, 4, x, 1, 0.5d0, y, 1)
  call begin_test('upper_scaled')
  call print_array('y', y, n)
  call end_test()

  ! Test 4: lower, alpha=2, beta=0.5, nonzero y
  y(1) = -1.0d0; y(2) = 2.0d0; y(3) = -3.0d0
  call DLA_SYAMV(BLAS_LOWER, n, 2.0d0, A, 4, x, 1, 0.5d0, y, 1)
  call begin_test('lower_scaled')
  call print_array('y', y, n)
  call end_test()

  ! Test 5: quick return n=0
  y(1) = 99.0d0; y(2) = 99.0d0; y(3) = 99.0d0
  call DLA_SYAMV(BLAS_UPPER, 0, 1.0d0, A, 4, x, 1, 0.0d0, y, 1)
  call begin_test('quick_return_n_zero')
  call print_array('y', y, 3)
  call end_test()

  ! Test 6: alpha=0, beta=1 quick return
  y(1) = 7.0d0; y(2) = 8.0d0; y(3) = 9.0d0
  call DLA_SYAMV(BLAS_UPPER, n, 0.0d0, A, 4, x, 1, 1.0d0, y, 1)
  call begin_test('alpha_zero_beta_one')
  call print_array('y', y, n)
  call end_test()

  ! Test 7: alpha=0, beta=2 (not a quick return; scales |y|)
  y(1) = -1.0d0; y(2) = 2.0d0; y(3) = -3.0d0
  call DLA_SYAMV(BLAS_UPPER, n, 0.0d0, A, 4, x, 1, 2.0d0, y, 1)
  call begin_test('alpha_zero_beta_two')
  call print_array('y', y, n)
  call end_test()

  ! Test 8: upper, negative incy
  x(1) = 1.0d0; x(2) = -2.0d0; x(3) = 3.0d0
  yneg = 0.0d0
  yneg(1) = 10.0d0; yneg(2) = 20.0d0; yneg(3) = 30.0d0
  call DLA_SYAMV(BLAS_UPPER, n, 1.0d0, A, 4, x, 1, 1.0d0, yneg, -1)
  call begin_test('upper_negincy')
  call print_array('y', yneg, n)
  call end_test()

  ! Test 9: lower with non-unit incx
  ! x values stored at stride 2: x(1)=1, x(3)=-2, x(5)=3
  xneg = 0.0d0
  xneg(1) = 1.0d0; xneg(3) = -2.0d0; xneg(5) = 3.0d0
  y = 0.0d0
  call DLA_SYAMV(BLAS_LOWER, n, 1.0d0, A, 4, xneg, 2, 0.0d0, y, 1)
  call begin_test('lower_incx2')
  call print_array('y', y, n)
  call end_test()

  ! Test 10: upper with negative incx, negative incy
  x(1) = 1.0d0; x(2) = -2.0d0; x(3) = 3.0d0
  y(1) = 1.0d0; y(2) = 2.0d0; y(3) = 3.0d0
  call DLA_SYAMV(BLAS_UPPER, n, 1.0d0, A, 4, x, -1, 1.0d0, y, -1)
  call begin_test('upper_negincx_negincy')
  call print_array('y', y, n)
  call end_test()

  ! Test 11: symbolic zero — A is all zero and y is zero, should remain zero
  A = 0.0d0
  x(1) = 1.0d0; x(2) = 1.0d0; x(3) = 1.0d0
  y(1) = 0.0d0; y(2) = 0.0d0; y(3) = 0.0d0
  call DLA_SYAMV(BLAS_UPPER, n, 1.0d0, A, 4, x, 1, 0.0d0, y, 1)
  call begin_test('symbolic_zero_upper')
  call print_array('y', y, n)
  call end_test()

  ! Test 12: symbolic zero, lower uplo
  y(1) = 0.0d0; y(2) = 0.0d0; y(3) = 0.0d0
  call DLA_SYAMV(BLAS_LOWER, n, 1.0d0, A, 4, x, 1, 0.0d0, y, 1)
  call begin_test('symbolic_zero_lower')
  call print_array('y', y, n)
  call end_test()

  ! Test 13: n=1 edge case
  A(1,1) = 4.0d0
  x(1) = -2.0d0
  y(1) = 0.0d0
  call DLA_SYAMV(BLAS_UPPER, 1, 1.0d0, A, 4, x, 1, 0.0d0, y, 1)
  call begin_test('n_one_upper')
  call print_array('y', y, 1)
  call end_test()

  y(1) = 0.0d0
  call DLA_SYAMV(BLAS_LOWER, 1, 1.0d0, A, 4, x, 1, 0.0d0, y, 1)
  call begin_test('n_one_lower')
  call print_array('y', y, 1)
  call end_test()

end program
