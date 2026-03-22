program test_dgesv
  use test_utils
  implicit none
  double precision :: a(100), b(100), a_orig(100), b_check(100), residual
  integer :: ipiv(10), info, i, j

  ! Test 1: 3x3 system Ax=b with known answer
  ! A = [2 1 1; 4 3 3; 8 7 9], b = [4; 10; 24]
  ! Solution: x = [1; 1; 1]
  a = 0.0d0; b = 0.0d0
  a(1) = 2.0d0; a(2) = 4.0d0; a(3) = 8.0d0
  a(4) = 1.0d0; a(5) = 3.0d0; a(6) = 7.0d0
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 9.0d0
  b(1) = 4.0d0; b(2) = 10.0d0; b(3) = 24.0d0
  call dgesv(3, 1, a, 3, ipiv, b, 3, info)
  call begin_test('solve_3x3')
  call print_array('x', b, 3)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

  ! Test 2: singular matrix (info > 0)
  a = 0.0d0; b = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0
  a(4) = 2.0d0; a(5) = 4.0d0; a(6) = 6.0d0
  a(7) = 3.0d0; a(8) = 6.0d0; a(9) = 9.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  call dgesv(3, 1, a, 3, ipiv, b, 3, info)
  call begin_test('singular')
  call print_int('info', info)
  call end_test()

  ! Test 3: N=0 quick return
  call dgesv(0, 1, a, 1, ipiv, b, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 4: NRHS=0 quick return
  a(1) = 5.0d0
  call dgesv(1, 0, a, 1, ipiv, b, 1, info)
  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

  ! Test 5: multiple RHS
  ! A = [1 2; 3 4], B = [5 11; 6 12]  (2x2 col-major)
  ! Solve AX = B where X is 2x2
  a = 0.0d0; b = 0.0d0
  a(1) = 1.0d0; a(2) = 3.0d0
  a(3) = 2.0d0; a(4) = 4.0d0
  b(1) = 5.0d0; b(2) = 6.0d0
  b(3) = 11.0d0; b(4) = 12.0d0
  call dgesv(2, 2, a, 2, ipiv, b, 2, info)
  call begin_test('multi_rhs')
  call print_array('x', b, 4)
  call print_int_array('ipiv', ipiv, 2)
  call print_int('info', info)
  call end_test()

  ! Test 6: 1x1 system: 5x = 10 => x = 2
  a(1) = 5.0d0
  b(1) = 10.0d0
  call dgesv(1, 1, a, 1, ipiv, b, 1, info)
  call begin_test('1x1')
  call print_array('x', b, 1)
  call print_int('info', info)
  call end_test()

  ! Test 7: 4x4 system with known solution
  ! A = [4 1 2 3; 1 5 1 2; 2 1 6 1; 3 2 1 7]
  ! x = [1; 2; 3; 4]
  ! b = A*x = [4+2+6+12, 1+10+3+8, 2+2+18+4, 3+4+3+28] = [24, 22, 26, 38]
  a = 0.0d0; b = 0.0d0
  a(1) = 4.0d0; a(2) = 1.0d0; a(3) = 2.0d0; a(4) = 3.0d0
  a(5) = 1.0d0; a(6) = 5.0d0; a(7) = 1.0d0; a(8) = 2.0d0
  a(9) = 2.0d0; a(10) = 1.0d0; a(11) = 6.0d0; a(12) = 1.0d0
  a(13) = 3.0d0; a(14) = 2.0d0; a(15) = 1.0d0; a(16) = 7.0d0
  b(1) = 24.0d0; b(2) = 22.0d0; b(3) = 26.0d0; b(4) = 38.0d0
  call dgesv(4, 1, a, 4, ipiv, b, 4, info)
  call begin_test('4x4')
  call print_array('x', b, 4)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

end program
