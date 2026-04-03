program test_dgetf2
  use test_utils
  implicit none
  double precision :: a(100)
  integer :: ipiv(10), info

  ! Test 1: 3x3 non-singular matrix
  ! A = [2 1 1; 4 3 3; 8 7 9] col-major
  a = 0.0d0
  a(1) = 2.0d0; a(2) = 4.0d0; a(3) = 8.0d0
  a(4) = 1.0d0; a(5) = 3.0d0; a(6) = 7.0d0
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 9.0d0
  ipiv = 0
  call dgetf2(3, 3, a, 3, ipiv, info)
  call begin_test('3x3')
  call print_array('a', a, 9)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

  ! Test 2: 4x4 matrix
  ! A = [2 1 1 0; 4 3 3 1; 8 7 9 5; 6 7 9 8] col-major
  a = 0.0d0
  a(1) = 2.0d0; a(2) = 4.0d0; a(3) = 8.0d0; a(4) = 6.0d0
  a(5) = 1.0d0; a(6) = 3.0d0; a(7) = 7.0d0; a(8) = 7.0d0
  a(9) = 1.0d0; a(10) = 3.0d0; a(11) = 9.0d0; a(12) = 9.0d0
  a(13) = 0.0d0; a(14) = 1.0d0; a(15) = 5.0d0; a(16) = 8.0d0
  ipiv = 0
  call dgetf2(4, 4, a, 4, ipiv, info)
  call begin_test('4x4')
  call print_array('a', a, 16)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 3: M > N (4x3 tall matrix)
  ! A = [2 1 0; 0 3 1; 1 0 4; 0 1 2] col-major
  a = 0.0d0
  a(1) = 2.0d0; a(2) = 0.0d0; a(3) = 1.0d0; a(4) = 0.0d0
  a(5) = 1.0d0; a(6) = 3.0d0; a(7) = 0.0d0; a(8) = 1.0d0
  a(9) = 0.0d0; a(10) = 1.0d0; a(11) = 4.0d0; a(12) = 2.0d0
  ipiv = 0
  call dgetf2(4, 3, a, 4, ipiv, info)
  call begin_test('4x3')
  call print_array('a', a, 12)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

  ! Test 4: M < N (3x4 wide matrix)
  a = 0.0d0
  a(1) = 1.0d0; a(2) = 4.0d0; a(3) = 7.0d0
  a(4) = 2.0d0; a(5) = 5.0d0; a(6) = 8.0d0
  a(7) = 3.0d0; a(8) = 6.0d0; a(9) = 9.0d0
  a(10) = 10.0d0; a(11) = 11.0d0; a(12) = 12.0d0
  ipiv = 0
  call dgetf2(3, 4, a, 3, ipiv, info)
  call begin_test('3x4')
  call print_array('a', a, 12)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

  ! Test 5: singular matrix (INFO > 0)
  ! A = [1 0 0; 0 0 0; 0 0 1] — zero pivot at (2,2)
  a = 0.0d0
  a(1) = 1.0d0; a(2) = 0.0d0; a(3) = 0.0d0
  a(4) = 0.0d0; a(5) = 0.0d0; a(6) = 0.0d0
  a(7) = 0.0d0; a(8) = 0.0d0; a(9) = 1.0d0
  ipiv = 0
  call dgetf2(3, 3, a, 3, ipiv, info)
  call begin_test('singular')
  call print_array('a', a, 9)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

  ! Test 6: N=0 (quick return)
  a(1) = 99.0d0
  call dgetf2(3, 0, a, 3, ipiv, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 7: M=0 (quick return)
  call dgetf2(0, 3, a, 1, ipiv, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! Test 8: 1x1 matrix
  a(1) = 5.0d0
  ipiv = 0
  call dgetf2(1, 1, a, 1, ipiv, info)
  call begin_test('1x1')
  call print_array('a', a, 1)
  call print_int_array('ipiv', ipiv, 1)
  call print_int('info', info)
  call end_test()

  ! Test 9: 1x1 singular (zero)
  a(1) = 0.0d0
  ipiv = 0
  call dgetf2(1, 1, a, 1, ipiv, info)
  call begin_test('1x1_singular')
  call print_array('a', a, 1)
  call print_int_array('ipiv', ipiv, 1)
  call print_int('info', info)
  call end_test()

  ! Test 10: Nx1 column vector
  a(1) = 1.0d0; a(2) = 5.0d0; a(3) = 3.0d0
  ipiv = 0
  call dgetf2(3, 1, a, 3, ipiv, info)
  call begin_test('col_vector')
  call print_array('a', a, 3)
  call print_int_array('ipiv', ipiv, 1)
  call print_int('info', info)
  call end_test()

  ! Test 11: 1xN row vector
  a(1) = 2.0d0; a(2) = 3.0d0; a(3) = 7.0d0
  ipiv = 0
  call dgetf2(1, 3, a, 1, ipiv, info)
  call begin_test('row_vector')
  call print_array('a', a, 3)
  call print_int_array('ipiv', ipiv, 1)
  call print_int('info', info)
  call end_test()

  ! Test 12: small diagonal triggering sfmin branch
  ! Use a very small but nonzero pivot to exercise the sfmin scaling path
  a = 0.0d0
  a(1) = 1.0d-310; a(2) = 2.0d0; a(3) = 3.0d0
  a(4) = 4.0d0; a(5) = 5.0d0; a(6) = 6.0d0
  a(7) = 7.0d0; a(8) = 8.0d0; a(9) = 9.0d0
  ipiv = 0
  call dgetf2(3, 3, a, 3, ipiv, info)
  call begin_test('sfmin')
  call print_array('a', a, 9)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

  ! Test 13: all-tiny column to force sfmin scaling path
  ! When the max element in a column is below sfmin, idamax still picks it
  ! but the division must use element-by-element loop instead of dscal
  a = 0.0d0
  a(1) = 3.0d-310; a(2) = 1.0d-310; a(3) = 2.0d-310
  a(4) = 1.0d0; a(5) = 2.0d0; a(6) = 3.0d0
  a(7) = 4.0d0; a(8) = 5.0d0; a(9) = 6.0d0
  ipiv = 0
  call dgetf2(3, 3, a, 3, ipiv, info)
  call begin_test('sfmin_path')
  call print_array('a', a, 9)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

end program
