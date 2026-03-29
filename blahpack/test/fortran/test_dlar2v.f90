program test_dlar2v
  use test_utils
  implicit none

  integer, parameter :: NMAX = 10
  double precision :: X(NMAX), Y(NMAX), Z(NMAX), C(NMAX), S(NMAX)
  integer :: i

  ! Test 1: basic operation, N=4, unit strides
  X(1) = 1.0d0
  X(2) = 2.0d0
  X(3) = 3.0d0
  X(4) = 4.0d0
  Y(1) = 5.0d0
  Y(2) = 6.0d0
  Y(3) = 7.0d0
  Y(4) = 8.0d0
  Z(1) = 0.5d0
  Z(2) = 1.0d0
  Z(3) = 1.5d0
  Z(4) = 2.0d0
  ! cos/sin pairs for 30, 45, 60, 90 degrees
  C(1) = 0.8660254037844387d0
  S(1) = 0.5d0
  C(2) = 0.7071067811865476d0
  S(2) = 0.7071067811865476d0
  C(3) = 0.5d0
  S(3) = 0.8660254037844387d0
  C(4) = 0.0d0
  S(4) = 1.0d0

  call DLAR2V(4, X, Y, Z, 1, C, S, 1)

  call begin_test('basic')
  call print_int('N', 4)
  call print_array('x', X, 4)
  call print_array('y', Y, 4)
  call print_array('z', Z, 4)
  call end_test()

  ! Test 2: N=0 (quick return)
  X(1) = 1.0d0
  Y(1) = 2.0d0
  Z(1) = 0.5d0
  C(1) = 0.5d0
  S(1) = 0.8660254037844387d0

  call DLAR2V(0, X, Y, Z, 1, C, S, 1)

  call begin_test('n_zero')
  call print_int('N', 0)
  call print_array('x', X, 1)
  call print_array('y', Y, 1)
  call print_array('z', Z, 1)
  call end_test()

  ! Test 3: N=1 (single element)
  X(1) = 3.0d0
  Y(1) = 4.0d0
  Z(1) = 1.0d0
  C(1) = 0.6d0
  S(1) = 0.8d0

  call DLAR2V(1, X, Y, Z, 1, C, S, 1)

  call begin_test('n_one')
  call print_int('N', 1)
  call print_array('x', X, 1)
  call print_array('y', Y, 1)
  call print_array('z', Z, 1)
  call end_test()

  ! Test 4: non-unit strides (INCX=2, INCC=2)
  do i = 1, NMAX
    X(i) = 0.0d0
    Y(i) = 0.0d0
    Z(i) = 0.0d0
    C(i) = 0.0d0
    S(i) = 0.0d0
  end do
  X(1) = 1.0d0
  X(3) = 2.0d0
  X(5) = 3.0d0
  Y(1) = 4.0d0
  Y(3) = 5.0d0
  Y(5) = 6.0d0
  Z(1) = 0.5d0
  Z(3) = 1.0d0
  Z(5) = 1.5d0
  C(1) = 0.8d0
  C(3) = 0.6d0
  C(5) = 0.5d0
  S(1) = 0.6d0
  S(3) = 0.8d0
  S(5) = 0.8660254037844387d0

  call DLAR2V(3, X, Y, Z, 2, C, S, 2)

  call begin_test('non_unit_stride')
  call print_int('N', 3)
  call print_array('x', X, 5)
  call print_array('y', Y, 5)
  call print_array('z', Z, 5)
  call end_test()

  ! Test 5: identity rotation (c=1, s=0) — matrices unchanged
  X(1) = 10.0d0
  X(2) = 20.0d0
  X(3) = 30.0d0
  Y(1) = 40.0d0
  Y(2) = 50.0d0
  Y(3) = 60.0d0
  Z(1) = 5.0d0
  Z(2) = 10.0d0
  Z(3) = 15.0d0
  C(1) = 1.0d0
  C(2) = 1.0d0
  C(3) = 1.0d0
  S(1) = 0.0d0
  S(2) = 0.0d0
  S(3) = 0.0d0

  call DLAR2V(3, X, Y, Z, 1, C, S, 1)

  call begin_test('identity')
  call print_int('N', 3)
  call print_array('x', X, 3)
  call print_array('y', Y, 3)
  call print_array('z', Z, 3)
  call end_test()

  ! Test 6: swap rotation (c=0, s=1)
  X(1) = 1.0d0
  X(2) = 2.0d0
  Y(1) = 3.0d0
  Y(2) = 4.0d0
  Z(1) = 0.5d0
  Z(2) = 1.0d0
  C(1) = 0.0d0
  C(2) = 0.0d0
  S(1) = 1.0d0
  S(2) = 1.0d0

  call DLAR2V(2, X, Y, Z, 1, C, S, 1)

  call begin_test('swap')
  call print_int('N', 2)
  call print_array('x', X, 2)
  call print_array('y', Y, 2)
  call print_array('z', Z, 2)
  call end_test()

  ! Test 7: mixed strides (INCX=3, INCC=2)
  do i = 1, NMAX
    X(i) = 0.0d0
    Y(i) = 0.0d0
    Z(i) = 0.0d0
    C(i) = 0.0d0
    S(i) = 0.0d0
  end do
  X(1) = 2.0d0
  X(4) = 4.0d0
  Y(1) = 6.0d0
  Y(4) = 8.0d0
  Z(1) = 1.0d0
  Z(4) = 2.0d0
  C(1) = 0.8d0
  C(3) = 0.6d0
  S(1) = 0.6d0
  S(3) = 0.8d0

  call DLAR2V(2, X, Y, Z, 3, C, S, 2)

  call begin_test('mixed_strides')
  call print_int('N', 2)
  call print_array('x', X, 4)
  call print_array('y', Y, 4)
  call print_array('z', Z, 4)
  call end_test()

end program
