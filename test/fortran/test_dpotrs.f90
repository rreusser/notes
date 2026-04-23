program test_dpotrs
  use test_utils
  implicit none
  double precision :: a(100), b(100), a_copy(100)
  integer :: info

  ! Use SPD matrix A = [4 2 1; 2 5 3; 1 3 9]

  ! Test 1: lower Cholesky solve, single RHS
  ! First factorize with dpotrf, then solve
  a = 0.0d0; b = 0.0d0
  a(1) = 4.0d0; a(2) = 2.0d0; a(3) = 1.0d0
  a(4) = 2.0d0; a(5) = 5.0d0; a(6) = 3.0d0
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 9.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  call dpotrf('L', 3, a, 3, info)
  call dpotrs('L', 3, 1, a, 3, b, 3, info)
  call begin_test('lower_single_rhs')
  call print_array('x', b, 3)
  call print_int('info', info)
  call end_test()

  ! Test 2: upper Cholesky solve, single RHS
  a = 0.0d0; b = 0.0d0
  a(1) = 4.0d0; a(2) = 2.0d0; a(3) = 1.0d0
  a(4) = 2.0d0; a(5) = 5.0d0; a(6) = 3.0d0
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 9.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  call dpotrf('U', 3, a, 3, info)
  call dpotrs('U', 3, 1, a, 3, b, 3, info)
  call begin_test('upper_single_rhs')
  call print_array('x', b, 3)
  call print_int('info', info)
  call end_test()

  ! Test 3: lower, multiple RHS (NRHS=2)
  a = 0.0d0; b = 0.0d0
  a(1) = 4.0d0; a(2) = 2.0d0; a(3) = 1.0d0
  a(4) = 2.0d0; a(5) = 5.0d0; a(6) = 3.0d0
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 9.0d0
  ! B is 3x2 col-major
  b(1) = 1.0d0; b(2) = 0.0d0; b(3) = 0.0d0
  b(4) = 0.0d0; b(5) = 1.0d0; b(6) = 0.0d0
  call dpotrf('L', 3, a, 3, info)
  call dpotrs('L', 3, 2, a, 3, b, 3, info)
  call begin_test('lower_multi_rhs')
  call print_array('x', b, 6)
  call print_int('info', info)
  call end_test()

  ! Test 4: N=0 quick return
  call dpotrs('L', 0, 1, a, 1, b, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 5: NRHS=0 quick return
  call dpotrs('L', 3, 0, a, 3, b, 3, info)
  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: 1x1 system
  a(1) = 2.0d0  ! L such that L*L^T = 4; L = 2
  b(1) = 6.0d0
  call dpotrs('L', 1, 1, a, 1, b, 1, info)
  call begin_test('one_by_one')
  call print_array('x', b, 1)
  call print_int('info', info)
  call end_test()

  ! Test 7: upper, multiple RHS (NRHS=3)
  a = 0.0d0; b = 0.0d0
  a(1) = 4.0d0; a(2) = 2.0d0; a(3) = 1.0d0
  a(4) = 2.0d0; a(5) = 5.0d0; a(6) = 3.0d0
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 9.0d0
  ! B is 3x3 (identity -> computes A^{-1})
  b(1) = 1.0d0; b(2) = 0.0d0; b(3) = 0.0d0
  b(4) = 0.0d0; b(5) = 1.0d0; b(6) = 0.0d0
  b(7) = 0.0d0; b(8) = 0.0d0; b(9) = 1.0d0
  call dpotrf('U', 3, a, 3, info)
  call dpotrs('U', 3, 3, a, 3, b, 3, info)
  call begin_test('upper_multi_rhs_3')
  call print_array('x', b, 9)
  call print_int('info', info)
  call end_test()

end program
