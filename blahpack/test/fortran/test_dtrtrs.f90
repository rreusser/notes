program test_dtrtrs
  use test_utils
  implicit none
  double precision :: a(100), b(100)
  integer :: info

  ! Test 1: 3x3 upper triangular solve, no transpose
  ! A = [2 1 3; 0 4 5; 0 0 6], b = [1; 2; 3]
  a = 0.0d0; b = 0.0d0
  ! Col-major: A(1,1)=2 A(2,1)=0 A(3,1)=0 A(1,2)=1 A(2,2)=4 A(3,2)=0 A(1,3)=3 A(2,3)=5 A(3,3)=6
  a(1) = 2.0d0; a(2) = 0.0d0; a(3) = 0.0d0
  a(4) = 1.0d0; a(5) = 4.0d0; a(6) = 0.0d0
  a(7) = 3.0d0; a(8) = 5.0d0; a(9) = 6.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  call dtrtrs('U', 'N', 'N', 3, 1, a, 3, b, 3, info)
  call begin_test('upper_no_trans')
  call print_array('x', b, 3)
  call print_int('info', info)
  call end_test()

  ! Test 2: 3x3 lower triangular solve, no transpose
  ! L = [2 0 0; 1 4 0; 3 5 6]
  a = 0.0d0; b = 0.0d0
  a(1) = 2.0d0; a(2) = 1.0d0; a(3) = 3.0d0
  a(4) = 0.0d0; a(5) = 4.0d0; a(6) = 5.0d0
  a(7) = 0.0d0; a(8) = 0.0d0; a(9) = 6.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  call dtrtrs('L', 'N', 'N', 3, 1, a, 3, b, 3, info)
  call begin_test('lower_no_trans')
  call print_array('x', b, 3)
  call print_int('info', info)
  call end_test()

  ! Test 3: 3x3 upper triangular solve, transpose
  a = 0.0d0; b = 0.0d0
  a(1) = 2.0d0; a(2) = 0.0d0; a(3) = 0.0d0
  a(4) = 1.0d0; a(5) = 4.0d0; a(6) = 0.0d0
  a(7) = 3.0d0; a(8) = 5.0d0; a(9) = 6.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  call dtrtrs('U', 'T', 'N', 3, 1, a, 3, b, 3, info)
  call begin_test('upper_trans')
  call print_array('x', b, 3)
  call print_int('info', info)
  call end_test()

  ! Test 4: 3x3 upper triangular solve, unit diagonal
  ! A = [1 2 3; 0 1 4; 0 0 1] (unit diag)
  a = 0.0d0; b = 0.0d0
  a(1) = 1.0d0; a(2) = 0.0d0; a(3) = 0.0d0
  a(4) = 2.0d0; a(5) = 1.0d0; a(6) = 0.0d0
  a(7) = 3.0d0; a(8) = 4.0d0; a(9) = 1.0d0
  b(1) = 10.0d0; b(2) = 5.0d0; b(3) = 1.0d0
  call dtrtrs('U', 'N', 'U', 3, 1, a, 3, b, 3, info)
  call begin_test('upper_unit_diag')
  call print_array('x', b, 3)
  call print_int('info', info)
  call end_test()

  ! Test 5: N=0 quick return
  call dtrtrs('U', 'N', 'N', 0, 1, a, 1, b, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: singular (zero diagonal) -> info > 0
  a = 0.0d0; b = 0.0d0
  a(1) = 2.0d0; a(2) = 0.0d0; a(3) = 0.0d0
  a(4) = 1.0d0; a(5) = 0.0d0; a(6) = 0.0d0  ! A(2,2) = 0 -> singular
  a(7) = 3.0d0; a(8) = 5.0d0; a(9) = 6.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  call dtrtrs('U', 'N', 'N', 3, 1, a, 3, b, 3, info)
  call begin_test('singular')
  call print_int('info', info)
  call end_test()

  ! Test 7: multiple RHS (NRHS=2)
  a = 0.0d0; b = 0.0d0
  a(1) = 2.0d0; a(2) = 0.0d0; a(3) = 0.0d0
  a(4) = 1.0d0; a(5) = 4.0d0; a(6) = 0.0d0
  a(7) = 3.0d0; a(8) = 5.0d0; a(9) = 6.0d0
  ! B is 3x2, col-major
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  b(4) = 4.0d0; b(5) = 5.0d0; b(6) = 6.0d0
  call dtrtrs('U', 'N', 'N', 3, 2, a, 3, b, 3, info)
  call begin_test('multi_rhs')
  call print_array('x', b, 6)
  call print_int('info', info)
  call end_test()

  ! Test 8: lower triangular, transpose
  a = 0.0d0; b = 0.0d0
  a(1) = 2.0d0; a(2) = 1.0d0; a(3) = 3.0d0
  a(4) = 0.0d0; a(5) = 4.0d0; a(6) = 5.0d0
  a(7) = 0.0d0; a(8) = 0.0d0; a(9) = 6.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  call dtrtrs('L', 'T', 'N', 3, 1, a, 3, b, 3, info)
  call begin_test('lower_trans')
  call print_array('x', b, 3)
  call print_int('info', info)
  call end_test()

end program
