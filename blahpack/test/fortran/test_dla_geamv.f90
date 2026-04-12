program test_dla_geamv
  use test_utils
  implicit none
  integer, parameter :: BLAS_NO_TRANS = 111
  integer, parameter :: BLAS_TRANS = 112
  double precision :: A(4, 3), x(4), y(4), yneg(7)
  integer :: m, n

  ! Test 1: no-transpose, alpha=1, beta=0, m=3, n=3
  m = 3
  n = 3
  A(1,1) =  1.0d0; A(1,2) = -2.0d0; A(1,3) =  3.0d0
  A(2,1) = -4.0d0; A(2,2) =  5.0d0; A(2,3) = -6.0d0
  A(3,1) =  7.0d0; A(3,2) = -8.0d0; A(3,3) =  9.0d0
  x(1) =  1.0d0; x(2) = -2.0d0; x(3) =  3.0d0
  y = 0.0d0
  call DLA_GEAMV(BLAS_NO_TRANS, m, n, 1.0d0, A, 4, x, 1, 0.0d0, y, 1)
  call begin_test('notrans_basic')
  call print_array('y', y, m)
  call end_test()

  ! Test 2: transpose, alpha=1, beta=0
  m = 3
  n = 3
  y = 0.0d0
  call DLA_GEAMV(BLAS_TRANS, m, n, 1.0d0, A, 4, x, 1, 0.0d0, y, 1)
  call begin_test('trans_basic')
  call print_array('y', y, n)
  call end_test()

  ! Test 3: no-transpose, alpha=2, beta=0.5, nonzero y
  m = 3
  n = 3
  y(1) = -1.0d0; y(2) =  2.0d0; y(3) = -3.0d0
  call DLA_GEAMV(BLAS_NO_TRANS, m, n, 2.0d0, A, 4, x, 1, 0.5d0, y, 1)
  call begin_test('notrans_scaled')
  call print_array('y', y, m)
  call end_test()

  ! Test 4: transpose, alpha=2, beta=0.5, nonzero y
  y(1) = -1.0d0; y(2) =  2.0d0; y(3) = -3.0d0
  call DLA_GEAMV(BLAS_TRANS, m, n, 2.0d0, A, 4, x, 1, 0.5d0, y, 1)
  call begin_test('trans_scaled')
  call print_array('y', y, n)
  call end_test()

  ! Test 5: quick return n=0
  y(1) = 99.0d0; y(2) = 99.0d0; y(3) = 99.0d0
  call DLA_GEAMV(BLAS_NO_TRANS, 3, 0, 1.0d0, A, 4, x, 1, 0.0d0, y, 1)
  call begin_test('quick_return_n_zero')
  call print_array('y', y, 3)
  call end_test()

  ! Test 6: alpha=0, beta=1 quick return
  y(1) = 7.0d0; y(2) = 8.0d0; y(3) = 9.0d0
  call DLA_GEAMV(BLAS_NO_TRANS, 3, 3, 0.0d0, A, 4, x, 1, 1.0d0, y, 1)
  call begin_test('alpha_zero_beta_one')
  call print_array('y', y, 3)
  call end_test()

  ! Test 7: alpha=0, beta=2
  y(1) = -1.0d0; y(2) = 2.0d0; y(3) = -3.0d0
  call DLA_GEAMV(BLAS_NO_TRANS, 3, 3, 0.0d0, A, 4, x, 1, 2.0d0, y, 1)
  call begin_test('alpha_zero_beta_two')
  call print_array('y', y, 3)
  call end_test()

  ! Test 8: negative stride y (no-trans), incx=1, incy=-1
  m = 3
  n = 3
  yneg = 0.0d0
  yneg(1) = 10.0d0; yneg(2) = 20.0d0; yneg(3) = 30.0d0
  call DLA_GEAMV(BLAS_NO_TRANS, m, n, 1.0d0, A, 4, x, 1, 1.0d0, yneg, -1)
  call begin_test('notrans_negincy')
  call print_array('y', yneg, 3)
  call end_test()

  ! Test 9: non-unit incx, no-trans
  m = 3
  n = 2
  A(1,1) = 1.0d0; A(1,2) = 2.0d0
  A(2,1) = 3.0d0; A(2,2) = 4.0d0
  A(3,1) = 5.0d0; A(3,2) = 6.0d0
  yneg = 0.0d0
  yneg(1) = 1.0d0; yneg(2) = 0.0d0; yneg(3) = 2.0d0; yneg(4) = 0.0d0
  ! x with incx=2, effective values x(1)=2, x(3)=-3 so lenx = n = 2
  yneg(5) = 2.0d0; yneg(6) = 0.0d0; yneg(7) = -3.0d0
  y = 0.0d0
  call DLA_GEAMV(BLAS_NO_TRANS, m, n, 1.0d0, A, 4, yneg(5), 2, 0.0d0, y, 1)
  call begin_test('notrans_incx2')
  call print_array('y', y, m)
  call end_test()

  ! Test 10: transpose with neg incx, incy
  m = 3
  n = 3
  A(1,1) =  1.0d0; A(1,2) = -2.0d0; A(1,3) =  3.0d0
  A(2,1) = -4.0d0; A(2,2) =  5.0d0; A(2,3) = -6.0d0
  A(3,1) =  7.0d0; A(3,2) = -8.0d0; A(3,3) =  9.0d0
  x(1) = 1.0d0; x(2) = -2.0d0; x(3) = 3.0d0
  y(1) = 1.0d0; y(2) = 2.0d0; y(3) = 3.0d0
  call DLA_GEAMV(BLAS_TRANS, m, n, 1.0d0, A, 4, x, -1, 1.0d0, y, -1)
  call begin_test('trans_negincx_negincy')
  call print_array('y', y, n)
  call end_test()

  ! Test 11: symbolic zero — if row is all zero and y is zero, no perturbation
  m = 2
  n = 2
  A(1,1) = 0.0d0; A(1,2) = 0.0d0
  A(2,1) = 1.0d0; A(2,2) = 1.0d0
  x(1) = 1.0d0; x(2) = 1.0d0
  y(1) = 0.0d0; y(2) = 0.0d0
  call DLA_GEAMV(BLAS_NO_TRANS, m, n, 1.0d0, A, 4, x, 1, 0.0d0, y, 1)
  call begin_test('symbolic_zero_row')
  call print_array('y', y, m)
  call end_test()

end program
