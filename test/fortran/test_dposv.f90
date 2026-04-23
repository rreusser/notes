program test_dposv
  use test_utils
  implicit none
  double precision :: a(100), b(100)
  integer :: info

  ! Test 1: 3x3 SPD system, lower, single RHS
  ! A = [4 2 1; 2 5 3; 1 3 9], b = [1; 2; 3]
  a = 0.0d0; b = 0.0d0
  a(1) = 4.0d0; a(2) = 2.0d0; a(3) = 1.0d0
  a(4) = 2.0d0; a(5) = 5.0d0; a(6) = 3.0d0
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 9.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  call dposv('L', 3, 1, a, 3, b, 3, info)
  call begin_test('lower_3x3')
  call print_array('x', b, 3)
  call print_int('info', info)
  call end_test()

  ! Test 2: 3x3 SPD system, upper, single RHS
  a = 0.0d0; b = 0.0d0
  a(1) = 4.0d0; a(2) = 2.0d0; a(3) = 1.0d0
  a(4) = 2.0d0; a(5) = 5.0d0; a(6) = 3.0d0
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 9.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  call dposv('U', 3, 1, a, 3, b, 3, info)
  call begin_test('upper_3x3')
  call print_array('x', b, 3)
  call print_int('info', info)
  call end_test()

  ! Test 3: not positive definite (info > 0)
  a = 0.0d0; b = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0
  a(4) = 2.0d0; a(5) = 1.0d0; a(6) = 4.0d0
  a(7) = 3.0d0; a(8) = 4.0d0; a(9) = 1.0d0
  b(1) = 1.0d0; b(2) = 1.0d0; b(3) = 1.0d0
  call dposv('L', 3, 1, a, 3, b, 3, info)
  call begin_test('not_posdef')
  call print_int('info', info)
  call end_test()

  ! Test 4: N=0 quick return
  call dposv('L', 0, 1, a, 1, b, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 5: known answer: A*x = b where A is identity
  ! A = I_3, b = [3; 5; 7] -> x = [3; 5; 7]
  a = 0.0d0; b = 0.0d0
  a(1) = 1.0d0; a(5) = 1.0d0; a(9) = 1.0d0
  b(1) = 3.0d0; b(2) = 5.0d0; b(3) = 7.0d0
  call dposv('L', 3, 1, a, 3, b, 3, info)
  call begin_test('identity')
  call print_array('x', b, 3)
  call print_int('info', info)
  call end_test()

  ! Test 6: multiple RHS
  a = 0.0d0; b = 0.0d0
  a(1) = 4.0d0; a(2) = 2.0d0; a(3) = 1.0d0
  a(4) = 2.0d0; a(5) = 5.0d0; a(6) = 3.0d0
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 9.0d0
  ! B is 3x2: first column [1;0;0], second [0;1;0]
  b(1) = 1.0d0; b(2) = 0.0d0; b(3) = 0.0d0
  b(4) = 0.0d0; b(5) = 1.0d0; b(6) = 0.0d0
  call dposv('L', 3, 2, a, 3, b, 3, info)
  call begin_test('multi_rhs')
  call print_array('x', b, 6)
  call print_int('info', info)
  call end_test()

  ! Test 7: NRHS=0
  a = 0.0d0
  a(1) = 4.0d0; a(5) = 5.0d0; a(9) = 9.0d0
  call dposv('L', 3, 0, a, 3, b, 3, info)
  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

end program
