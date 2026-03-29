program test_zlartv
  use test_utils
  implicit none

  integer, parameter :: NMAX = 10
  complex*16 :: X(NMAX), Y(NMAX), S(NMAX)
  double precision :: C(NMAX)
  double precision :: X_R(2*NMAX), Y_R(2*NMAX), S_R(2*NMAX)
  equivalence (X, X_R)
  equivalence (Y, Y_R)
  equivalence (S, S_R)
  integer :: i

  ! Test 1: basic operation, N=4, unit strides
  ! x and y are complex, c is real, s is complex
  X(1) = dcmplx(1.0d0, 2.0d0)
  X(2) = dcmplx(3.0d0, 4.0d0)
  X(3) = dcmplx(5.0d0, 6.0d0)
  X(4) = dcmplx(7.0d0, 8.0d0)
  Y(1) = dcmplx(9.0d0, 10.0d0)
  Y(2) = dcmplx(11.0d0, 12.0d0)
  Y(3) = dcmplx(13.0d0, 14.0d0)
  Y(4) = dcmplx(15.0d0, 16.0d0)
  C(1) = 0.8d0
  C(2) = 0.6d0
  C(3) = 0.5d0
  C(4) = 0.0d0
  S(1) = dcmplx(0.6d0, 0.0d0)
  S(2) = dcmplx(0.0d0, 0.8d0)
  S(3) = dcmplx(0.5d0, 0.5d0)
  S(4) = dcmplx(1.0d0, 0.0d0)

  call ZLARTV(4, X, 1, Y, 1, C, S, 1)

  call begin_test('basic')
  call print_int('N', 4)
  call print_array('x', X_R, 8)
  call print_array('y', Y_R, 8)
  call end_test()

  ! Test 2: N=0 (quick return)
  X(1) = dcmplx(1.0d0, 2.0d0)
  Y(1) = dcmplx(3.0d0, 4.0d0)
  C(1) = 0.5d0
  S(1) = dcmplx(0.8d0, 0.1d0)

  call ZLARTV(0, X, 1, Y, 1, C, S, 1)

  call begin_test('n_zero')
  call print_int('N', 0)
  call print_array('x', X_R, 2)
  call print_array('y', Y_R, 2)
  call end_test()

  ! Test 3: N=1 (single element)
  X(1) = dcmplx(3.0d0, 4.0d0)
  Y(1) = dcmplx(5.0d0, 6.0d0)
  C(1) = 0.6d0
  S(1) = dcmplx(0.8d0, 0.0d0)

  call ZLARTV(1, X, 1, Y, 1, C, S, 1)

  call begin_test('n_one')
  call print_int('N', 1)
  call print_array('x', X_R, 2)
  call print_array('y', Y_R, 2)
  call end_test()

  ! Test 4: non-unit strides (INCX=2, INCY=2, INCC=2)
  do i = 1, NMAX
    X(i) = dcmplx(0.0d0, 0.0d0)
    Y(i) = dcmplx(0.0d0, 0.0d0)
    C(i) = 0.0d0
    S(i) = dcmplx(0.0d0, 0.0d0)
  end do
  X(1) = dcmplx(1.0d0, 2.0d0)
  X(3) = dcmplx(3.0d0, 4.0d0)
  X(5) = dcmplx(5.0d0, 6.0d0)
  Y(1) = dcmplx(7.0d0, 8.0d0)
  Y(3) = dcmplx(9.0d0, 10.0d0)
  Y(5) = dcmplx(11.0d0, 12.0d0)
  C(1) = 0.8d0
  C(3) = 0.6d0
  C(5) = 0.5d0
  S(1) = dcmplx(0.6d0, 0.1d0)
  S(3) = dcmplx(0.5d0, 0.3d0)
  S(5) = dcmplx(0.7d0, 0.2d0)

  call ZLARTV(3, X, 2, Y, 2, C, S, 2)

  call begin_test('non_unit_stride')
  call print_int('N', 3)
  call print_array('x', X_R, 10)
  call print_array('y', Y_R, 10)
  call end_test()

  ! Test 5: identity rotation (c=1, s=0)
  X(1) = dcmplx(10.0d0, 20.0d0)
  X(2) = dcmplx(30.0d0, 40.0d0)
  X(3) = dcmplx(50.0d0, 60.0d0)
  Y(1) = dcmplx(70.0d0, 80.0d0)
  Y(2) = dcmplx(90.0d0, 100.0d0)
  Y(3) = dcmplx(110.0d0, 120.0d0)
  C(1) = 1.0d0
  C(2) = 1.0d0
  C(3) = 1.0d0
  S(1) = dcmplx(0.0d0, 0.0d0)
  S(2) = dcmplx(0.0d0, 0.0d0)
  S(3) = dcmplx(0.0d0, 0.0d0)

  call ZLARTV(3, X, 1, Y, 1, C, S, 1)

  call begin_test('identity')
  call print_int('N', 3)
  call print_array('x', X_R, 6)
  call print_array('y', Y_R, 6)
  call end_test()

  ! Test 6: swap rotation (c=0, s=1+0i)
  X(1) = dcmplx(1.0d0, 2.0d0)
  X(2) = dcmplx(3.0d0, 4.0d0)
  Y(1) = dcmplx(5.0d0, 6.0d0)
  Y(2) = dcmplx(7.0d0, 8.0d0)
  C(1) = 0.0d0
  C(2) = 0.0d0
  S(1) = dcmplx(1.0d0, 0.0d0)
  S(2) = dcmplx(1.0d0, 0.0d0)

  call ZLARTV(2, X, 1, Y, 1, C, S, 1)

  call begin_test('swap')
  call print_int('N', 2)
  call print_array('x', X_R, 4)
  call print_array('y', Y_R, 4)
  call end_test()

  ! Test 7: purely imaginary s
  X(1) = dcmplx(1.0d0, 0.0d0)
  X(2) = dcmplx(0.0d0, 1.0d0)
  Y(1) = dcmplx(0.0d0, 1.0d0)
  Y(2) = dcmplx(1.0d0, 0.0d0)
  C(1) = 0.0d0
  C(2) = 0.0d0
  S(1) = dcmplx(0.0d0, 1.0d0)
  S(2) = dcmplx(0.0d0, 1.0d0)

  call ZLARTV(2, X, 1, Y, 1, C, S, 1)

  call begin_test('imag_s')
  call print_int('N', 2)
  call print_array('x', X_R, 4)
  call print_array('y', Y_R, 4)
  call end_test()

  ! Test 8: mixed strides (INCX=1, INCY=3, INCC=2)
  do i = 1, NMAX
    X(i) = dcmplx(0.0d0, 0.0d0)
    Y(i) = dcmplx(0.0d0, 0.0d0)
    C(i) = 0.0d0
    S(i) = dcmplx(0.0d0, 0.0d0)
  end do
  X(1) = dcmplx(2.0d0, 1.0d0)
  X(2) = dcmplx(4.0d0, 3.0d0)
  Y(1) = dcmplx(6.0d0, 5.0d0)
  Y(4) = dcmplx(8.0d0, 7.0d0)
  C(1) = 0.8d0
  C(3) = 0.6d0
  S(1) = dcmplx(0.6d0, 0.1d0)
  S(3) = dcmplx(0.8d0, 0.2d0)

  call ZLARTV(2, X, 1, Y, 3, C, S, 2)

  call begin_test('mixed_strides')
  call print_int('N', 2)
  call print_array('x', X_R, 4)
  call print_array('y', Y_R, 8)
  call end_test()

end program
