program test_dgetrs
  use test_utils
  implicit none
  double precision :: a(100), b(100), b_orig(100), residual
  integer :: ipiv(10), info, i, j

  ! We first factorize a known matrix, then solve.

  ! Test 1: 3x3 system, no-transpose solve
  ! A = [2 1 1; 4 3 3; 8 7 9], b = [1; 1; 1]
  ! Store original b for verification
  a = 0.0d0; b = 0.0d0
  a(1) = 2.0d0; a(2) = 4.0d0; a(3) = 8.0d0
  a(4) = 1.0d0; a(5) = 3.0d0; a(6) = 7.0d0
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 9.0d0
  b(1) = 1.0d0; b(2) = 1.0d0; b(3) = 1.0d0
  call dgetrf(3, 3, a, 3, ipiv, info)
  call dgetrs('N', 3, 1, a, 3, ipiv, b, 3, info)
  call begin_test('solve_3x3')
  call print_array('x', b, 3)
  call print_int('info', info)
  call end_test()

  ! Test 2: transpose solve with same factorized A
  b(1) = 1.0d0; b(2) = 1.0d0; b(3) = 1.0d0
  call dgetrs('T', 3, 1, a, 3, ipiv, b, 3, info)
  call begin_test('solve_3x3_trans')
  call print_array('x', b, 3)
  call print_int('info', info)
  call end_test()

  ! Test 3: multiple RHS (NRHS=2)
  ! Refactorize
  a = 0.0d0; b = 0.0d0
  a(1) = 2.0d0; a(2) = 4.0d0; a(3) = 8.0d0
  a(4) = 1.0d0; a(5) = 3.0d0; a(6) = 7.0d0
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 9.0d0
  ! B is 3x2 col-major: b1 = [1;0;0], b2 = [0;1;0]
  b(1) = 1.0d0; b(2) = 0.0d0; b(3) = 0.0d0
  b(4) = 0.0d0; b(5) = 1.0d0; b(6) = 0.0d0
  call dgetrf(3, 3, a, 3, ipiv, info)
  call dgetrs('N', 3, 2, a, 3, ipiv, b, 3, info)
  call begin_test('multi_rhs')
  call print_array('x', b, 6)
  call print_int('info', info)
  call end_test()

  ! Test 4: N=0 quick return
  call dgetrs('N', 0, 1, a, 1, ipiv, b, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 5: NRHS=0 quick return
  call dgetrs('N', 3, 0, a, 3, ipiv, b, 3, info)
  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: 1x1 system
  a(1) = 5.0d0
  ipiv(1) = 1
  b(1) = 10.0d0
  ! Factorize
  call dgetrf(1, 1, a, 1, ipiv, info)
  call dgetrs('N', 1, 1, a, 1, ipiv, b, 1, info)
  call begin_test('1x1')
  call print_array('x', b, 1)
  call print_int('info', info)
  call end_test()

  ! Test 7: identity matrix (no pivoting needed)
  a = 0.0d0
  a(1) = 1.0d0; a(5) = 1.0d0; a(9) = 1.0d0
  b(1) = 3.0d0; b(2) = 5.0d0; b(3) = 7.0d0
  call dgetrf(3, 3, a, 3, ipiv, info)
  call dgetrs('N', 3, 1, a, 3, ipiv, b, 3, info)
  call begin_test('identity')
  call print_array('x', b, 3)
  call print_int('info', info)
  call end_test()

end program
