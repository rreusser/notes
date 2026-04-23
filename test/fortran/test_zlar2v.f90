program test_zlar2v
  use test_utils
  implicit none

  integer, parameter :: NMAX = 10
  complex*16 :: X(NMAX), Y(NMAX), Z(NMAX), S(NMAX)
  double precision :: C(NMAX)
  double precision :: X_r(2*NMAX), Y_r(2*NMAX), Z_r(2*NMAX), S_r(2*NMAX)
  equivalence (X, X_r)
  equivalence (Y, Y_r)
  equivalence (Z, Z_r)
  equivalence (S, S_r)
  integer :: i

  ! Test 1: basic operation, N=4, unit strides
  ! X and Y are real-valued (imaginary part = 0), Z and S are complex
  X(1) = (1.0d0, 0.0d0)
  X(2) = (2.0d0, 0.0d0)
  X(3) = (3.0d0, 0.0d0)
  X(4) = (4.0d0, 0.0d0)
  Y(1) = (5.0d0, 0.0d0)
  Y(2) = (6.0d0, 0.0d0)
  Y(3) = (7.0d0, 0.0d0)
  Y(4) = (8.0d0, 0.0d0)
  Z(1) = (0.5d0, 0.1d0)
  Z(2) = (1.0d0, -0.2d0)
  Z(3) = (1.5d0, 0.3d0)
  Z(4) = (2.0d0, -0.4d0)
  ! cos/sin pairs for various angles; S is complex
  C(1) = 0.8660254037844387d0
  S(1) = (0.5d0, 0.0d0)
  C(2) = 0.7071067811865476d0
  S(2) = (0.5d0, 0.5d0)
  C(3) = 0.5d0
  S(3) = (0.0d0, 0.8660254037844387d0)
  C(4) = 0.0d0
  S(4) = (0.8d0, 0.6d0)

  call ZLAR2V(4, X, Y, Z, 1, C, S, 1)

  call begin_test('basic')
  call print_int('N', 4)
  call print_array('x', X_r, 8)
  call print_array('y', Y_r, 8)
  call print_array('z', Z_r, 8)
  call end_test()

  ! Test 2: N=0 (quick return)
  X(1) = (1.0d0, 0.0d0)
  Y(1) = (2.0d0, 0.0d0)
  Z(1) = (0.5d0, 0.1d0)
  C(1) = 0.5d0
  S(1) = (0.8660254037844387d0, 0.0d0)

  call ZLAR2V(0, X, Y, Z, 1, C, S, 1)

  call begin_test('n_zero')
  call print_int('N', 0)
  call print_array('x', X_r, 2)
  call print_array('y', Y_r, 2)
  call print_array('z', Z_r, 2)
  call end_test()

  ! Test 3: N=1 (single element)
  X(1) = (3.0d0, 0.0d0)
  Y(1) = (4.0d0, 0.0d0)
  Z(1) = (1.0d0, 0.5d0)
  C(1) = 0.6d0
  S(1) = (0.8d0, 0.0d0)

  call ZLAR2V(1, X, Y, Z, 1, C, S, 1)

  call begin_test('n_one')
  call print_int('N', 1)
  call print_array('x', X_r, 2)
  call print_array('y', Y_r, 2)
  call print_array('z', Z_r, 2)
  call end_test()

  ! Test 4: non-unit strides (INCX=2, INCC=2)
  do i = 1, NMAX
    X(i) = (0.0d0, 0.0d0)
    Y(i) = (0.0d0, 0.0d0)
    Z(i) = (0.0d0, 0.0d0)
    C(i) = 0.0d0
    S(i) = (0.0d0, 0.0d0)
  end do
  X(1) = (1.0d0, 0.0d0)
  X(3) = (2.0d0, 0.0d0)
  X(5) = (3.0d0, 0.0d0)
  Y(1) = (4.0d0, 0.0d0)
  Y(3) = (5.0d0, 0.0d0)
  Y(5) = (6.0d0, 0.0d0)
  Z(1) = (0.5d0, 0.2d0)
  Z(3) = (1.0d0, -0.3d0)
  Z(5) = (1.5d0, 0.4d0)
  C(1) = 0.8d0
  C(3) = 0.6d0
  C(5) = 0.5d0
  S(1) = (0.6d0, 0.0d0)
  S(3) = (0.0d0, 0.8d0)
  S(5) = (0.5d0, 0.5d0)

  call ZLAR2V(3, X, Y, Z, 2, C, S, 2)

  call begin_test('non_unit_stride')
  call print_int('N', 3)
  call print_array('x', X_r, 10)
  call print_array('y', Y_r, 10)
  call print_array('z', Z_r, 10)
  call end_test()

  ! Test 5: identity rotation (c=1, s=0) — matrices unchanged
  X(1) = (10.0d0, 0.0d0)
  X(2) = (20.0d0, 0.0d0)
  X(3) = (30.0d0, 0.0d0)
  Y(1) = (40.0d0, 0.0d0)
  Y(2) = (50.0d0, 0.0d0)
  Y(3) = (60.0d0, 0.0d0)
  Z(1) = (5.0d0, 1.0d0)
  Z(2) = (10.0d0, -2.0d0)
  Z(3) = (15.0d0, 3.0d0)
  C(1) = 1.0d0
  C(2) = 1.0d0
  C(3) = 1.0d0
  S(1) = (0.0d0, 0.0d0)
  S(2) = (0.0d0, 0.0d0)
  S(3) = (0.0d0, 0.0d0)

  call ZLAR2V(3, X, Y, Z, 1, C, S, 1)

  call begin_test('identity')
  call print_int('N', 3)
  call print_array('x', X_r, 6)
  call print_array('y', Y_r, 6)
  call print_array('z', Z_r, 6)
  call end_test()

  ! Test 6: pure imaginary S (c=0.6, s=(0,0.8))
  X(1) = (1.0d0, 0.0d0)
  X(2) = (2.0d0, 0.0d0)
  Y(1) = (3.0d0, 0.0d0)
  Y(2) = (4.0d0, 0.0d0)
  Z(1) = (0.5d0, 0.3d0)
  Z(2) = (1.0d0, -0.5d0)
  C(1) = 0.6d0
  C(2) = 0.6d0
  S(1) = (0.0d0, 0.8d0)
  S(2) = (0.0d0, 0.8d0)

  call ZLAR2V(2, X, Y, Z, 1, C, S, 1)

  call begin_test('pure_imag_s')
  call print_int('N', 2)
  call print_array('x', X_r, 4)
  call print_array('y', Y_r, 4)
  call print_array('z', Z_r, 4)
  call end_test()

  ! Test 7: swap rotation (c=0, s=(1,0)) — pure swap
  X(1) = (1.0d0, 0.0d0)
  X(2) = (2.0d0, 0.0d0)
  Y(1) = (3.0d0, 0.0d0)
  Y(2) = (4.0d0, 0.0d0)
  Z(1) = (0.5d0, 0.7d0)
  Z(2) = (1.0d0, -0.3d0)
  C(1) = 0.0d0
  C(2) = 0.0d0
  S(1) = (1.0d0, 0.0d0)
  S(2) = (1.0d0, 0.0d0)

  call ZLAR2V(2, X, Y, Z, 1, C, S, 1)

  call begin_test('swap')
  call print_int('N', 2)
  call print_array('x', X_r, 4)
  call print_array('y', Y_r, 4)
  call print_array('z', Z_r, 4)
  call end_test()

  ! Test 8: mixed strides (INCX=3, INCC=2)
  do i = 1, NMAX
    X(i) = (0.0d0, 0.0d0)
    Y(i) = (0.0d0, 0.0d0)
    Z(i) = (0.0d0, 0.0d0)
    C(i) = 0.0d0
    S(i) = (0.0d0, 0.0d0)
  end do
  X(1) = (2.0d0, 0.0d0)
  X(4) = (4.0d0, 0.0d0)
  Y(1) = (6.0d0, 0.0d0)
  Y(4) = (8.0d0, 0.0d0)
  Z(1) = (1.0d0, 0.5d0)
  Z(4) = (2.0d0, -1.0d0)
  C(1) = 0.8d0
  C(3) = 0.6d0
  S(1) = (0.6d0, 0.0d0)
  S(3) = (0.5d0, 0.5d0)

  call ZLAR2V(2, X, Y, Z, 3, C, S, 2)

  call begin_test('mixed_strides')
  call print_int('N', 2)
  call print_array('x', X_r, 8)
  call print_array('y', Y_r, 8)
  call print_array('z', Z_r, 8)
  call end_test()

end program
