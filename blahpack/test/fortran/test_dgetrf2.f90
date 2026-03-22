program test_dgetrf2
  use test_utils
  implicit none
  double precision :: a(100), a_orig(100)
  integer :: ipiv(10), info, i

  ! Test 1: 3x3 non-singular matrix
  ! A = [2 1 1; 4 3 3; 8 7 9] col-major
  a = 0.0d0
  a(1) = 2.0d0; a(2) = 4.0d0; a(3) = 8.0d0
  a(4) = 1.0d0; a(5) = 3.0d0; a(6) = 7.0d0
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 9.0d0
  ipiv = 0
  call dgetrf2(3, 3, a, 3, ipiv, info)
  call begin_test('3x3')
  call print_array('a', a, 9)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

  ! Test 2: 4x3 tall matrix (M > N)
  a = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0; a(4) = 4.0d0
  a(5) = 5.0d0; a(6) = 6.0d0; a(7) = 7.0d0; a(8) = 8.0d0
  a(9) = 9.0d0; a(10) = 10.0d0; a(11) = 11.0d0; a(12) = 12.0d0
  ipiv = 0
  call dgetrf2(4, 3, a, 4, ipiv, info)
  call begin_test('4x3')
  call print_array('a', a, 12)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

  ! Test 3: 3x4 wide matrix (M < N)
  a = 0.0d0
  a(1) = 1.0d0; a(2) = 4.0d0; a(3) = 7.0d0
  a(4) = 2.0d0; a(5) = 5.0d0; a(6) = 8.0d0
  a(7) = 3.0d0; a(8) = 6.0d0; a(9) = 9.0d0
  a(10) = 10.0d0; a(11) = 11.0d0; a(12) = 12.0d0
  ipiv = 0
  call dgetrf2(3, 4, a, 3, ipiv, info)
  call begin_test('3x4')
  call print_array('a', a, 12)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

  ! Test 4: singular matrix (info > 0)
  ! A = [1 2 3; 2 4 6; 3 6 9] — rank 1
  a = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0
  a(4) = 2.0d0; a(5) = 4.0d0; a(6) = 6.0d0
  a(7) = 3.0d0; a(8) = 6.0d0; a(9) = 9.0d0
  ipiv = 0
  call dgetrf2(3, 3, a, 3, ipiv, info)
  call begin_test('singular')
  call print_array('a', a, 9)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

  ! Test 5: N=0 (quick return)
  a(1) = 99.0d0
  call dgetrf2(3, 0, a, 3, ipiv, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: M=0 (quick return)
  call dgetrf2(0, 3, a, 1, ipiv, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! Test 7: 1x1 matrix
  a(1) = 5.0d0
  ipiv = 0
  call dgetrf2(1, 1, a, 1, ipiv, info)
  call begin_test('1x1')
  call print_array('a', a, 1)
  call print_int_array('ipiv', ipiv, 1)
  call print_int('info', info)
  call end_test()

  ! Test 8: 1x1 singular (zero)
  a(1) = 0.0d0
  ipiv = 0
  call dgetrf2(1, 1, a, 1, ipiv, info)
  call begin_test('1x1_singular')
  call print_array('a', a, 1)
  call print_int_array('ipiv', ipiv, 1)
  call print_int('info', info)
  call end_test()

  ! Test 9: Nx1 column vector
  a(1) = 1.0d0; a(2) = 5.0d0; a(3) = 3.0d0
  ipiv = 0
  call dgetrf2(3, 1, a, 3, ipiv, info)
  call begin_test('col_vector')
  call print_array('a', a, 3)
  call print_int_array('ipiv', ipiv, 1)
  call print_int('info', info)
  call end_test()

  ! Test 10: 1xN row vector
  a(1) = 2.0d0; a(2) = 3.0d0; a(3) = 7.0d0
  ipiv = 0
  call dgetrf2(1, 3, a, 1, ipiv, info)
  call begin_test('row_vector')
  call print_array('a', a, 3)
  call print_int_array('ipiv', ipiv, 1)
  call print_int('info', info)
  call end_test()

end program
