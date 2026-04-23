program test_zlarf
  use test_utils
  implicit none

  complex*16 :: v(10), C(25), work(10), tau
  double precision :: v_real(20), C_real(50), tau_real(2)
  equivalence (v, v_real)
  equivalence (C, C_real)
  equivalence (tau, tau_real)
  integer :: i

  ! Test 1: Left side, 3x2 matrix, H = I - tau * v * v^H
  ! v = [1; 0.5+0.5i; -0.3+0.2i]
  ! tau = 1.5 + 0.3i
  ! C = [1+0i, 2+1i; 3+1i, 4-1i; 5+2i, 6+0i]  (3x2, col-major)
  v(1) = (1.0d0, 0.0d0)
  v(2) = (0.5d0, 0.5d0)
  v(3) = (-0.3d0, 0.2d0)
  tau = (1.5d0, 0.3d0)
  ! Col 1
  C(1) = (1.0d0, 0.0d0)
  C(2) = (3.0d0, 1.0d0)
  C(3) = (5.0d0, 2.0d0)
  ! Col 2
  C(4) = (2.0d0, 1.0d0)
  C(5) = (4.0d0, -1.0d0)
  C(6) = (6.0d0, 0.0d0)
  work = (0.0d0, 0.0d0)
  call zlarf('L', 3, 2, v, 1, tau, C, 3, work)
  call begin_test('zlarf_left_3x2')
  call print_array('C', C_real, 12)
  call end_test()

  ! Test 2: Right side, 2x3 matrix
  ! v = [1; 0.5+0.5i; -0.3+0.2i]
  ! tau = 1.5 + 0.3i
  ! C = [1+0i, 3+1i, 5+2i; 2+1i, 4-1i, 6+0i]  (2x3, col-major)
  v(1) = (1.0d0, 0.0d0)
  v(2) = (0.5d0, 0.5d0)
  v(3) = (-0.3d0, 0.2d0)
  tau = (1.5d0, 0.3d0)
  ! Col 1
  C(1) = (1.0d0, 0.0d0)
  C(2) = (2.0d0, 1.0d0)
  ! Col 2
  C(3) = (3.0d0, 1.0d0)
  C(4) = (4.0d0, -1.0d0)
  ! Col 3
  C(5) = (5.0d0, 2.0d0)
  C(6) = (6.0d0, 0.0d0)
  work = (0.0d0, 0.0d0)
  call zlarf('R', 2, 3, v, 1, tau, C, 2, work)
  call begin_test('zlarf_right_2x3')
  call print_array('C', C_real, 12)
  call end_test()

  ! Test 3: tau = 0 (identity transform)
  v(1) = (1.0d0, 0.0d0)
  v(2) = (0.5d0, 0.5d0)
  tau = (0.0d0, 0.0d0)
  C(1) = (1.0d0, 0.0d0)
  C(2) = (2.0d0, 1.0d0)
  C(3) = (3.0d0, 2.0d0)
  C(4) = (4.0d0, 3.0d0)
  work = (0.0d0, 0.0d0)
  call zlarf('L', 2, 2, v, 1, tau, C, 2, work)
  call begin_test('zlarf_tau_zero')
  call print_array('C', C_real, 8)
  call end_test()

  ! Test 4: n=0 or m=0
  v(1) = (1.0d0, 0.0d0)
  tau = (1.0d0, 0.0d0)
  C(1) = (1.0d0, 0.0d0)
  work = (0.0d0, 0.0d0)
  call zlarf('L', 1, 0, v, 1, tau, C, 1, work)
  call begin_test('zlarf_n_zero')
  call print_array('C', C_real, 2)
  call end_test()

  ! Test 5: Left side, 4x3 matrix, stride 1
  v(1) = (1.0d0, 0.0d0)
  v(2) = (0.2d0, 0.3d0)
  v(3) = (-0.5d0, 0.1d0)
  v(4) = (0.4d0, -0.6d0)
  tau = (1.2d0, -0.4d0)
  ! Build a known 4x3 matrix
  C(1) = (1.0d0, 0.0d0)
  C(2) = (0.0d0, 1.0d0)
  C(3) = (2.0d0, -1.0d0)
  C(4) = (3.0d0, 0.5d0)
  C(5) = (-1.0d0, 2.0d0)
  C(6) = (0.5d0, 0.5d0)
  C(7) = (1.5d0, -0.5d0)
  C(8) = (-2.0d0, 1.0d0)
  C(9) = (0.0d0, 0.0d0)
  C(10) = (1.0d0, 1.0d0)
  C(11) = (-0.5d0, 0.0d0)
  C(12) = (2.0d0, -2.0d0)
  work = (0.0d0, 0.0d0)
  call zlarf('L', 4, 3, v, 1, tau, C, 4, work)
  call begin_test('zlarf_left_4x3')
  call print_array('C', C_real, 24)
  call end_test()

end program
