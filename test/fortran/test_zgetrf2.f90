program test_zgetrf2
  use test_utils
  implicit none
  double precision :: a_r(200)
  complex*16 :: a(100)
  equivalence (a, a_r)
  integer :: ipiv(10), info, i

  ! Test 1: 3x3 non-singular complex matrix
  ! A = [(2+1i) (1+0.5i) (1+0.1i); (4+2i) (3+1i) (3+0.5i); (8+3i) (7+2i) (9+1i)] col-major
  a = (0.0d0, 0.0d0)
  a(1) = (2.0d0, 1.0d0); a(2) = (4.0d0, 2.0d0); a(3) = (8.0d0, 3.0d0)
  a(4) = (1.0d0, 0.5d0); a(5) = (3.0d0, 1.0d0); a(6) = (7.0d0, 2.0d0)
  a(7) = (1.0d0, 0.1d0); a(8) = (3.0d0, 0.5d0); a(9) = (9.0d0, 1.0d0)
  ipiv = 0
  call zgetrf2(3, 3, a, 3, ipiv, info)
  call begin_test('3x3')
  call print_array('a', a_r, 18)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

  ! Test 2: 4x3 tall matrix (M > N)
  a = (0.0d0, 0.0d0)
  a(1) = (2.0d0, 1.0d0); a(2) = (0.0d0, 0.5d0); a(3) = (1.0d0, 0.2d0); a(4) = (0.0d0, 0.1d0)
  a(5) = (1.0d0, 0.3d0); a(6) = (3.0d0, 1.0d0); a(7) = (0.0d0, 0.4d0); a(8) = (1.0d0, 0.5d0)
  a(9) = (0.0d0, 0.1d0); a(10) = (1.0d0, 0.6d0); a(11) = (4.0d0, 2.0d0); a(12) = (2.0d0, 1.0d0)
  ipiv = 0
  call zgetrf2(4, 3, a, 4, ipiv, info)
  call begin_test('4x3')
  call print_array('a', a_r, 24)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

  ! Test 3: 3x4 wide matrix (M < N)
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.5d0); a(2) = (4.0d0, 1.0d0); a(3) = (7.0d0, 2.0d0)
  a(4) = (2.0d0, 0.3d0); a(5) = (5.0d0, 1.5d0); a(6) = (8.0d0, 2.5d0)
  a(7) = (3.0d0, 0.1d0); a(8) = (6.0d0, 0.5d0); a(9) = (9.0d0, 3.0d0)
  a(10) = (10.0d0, 1.0d0); a(11) = (11.0d0, 2.0d0); a(12) = (12.0d0, 3.0d0)
  ipiv = 0
  call zgetrf2(3, 4, a, 3, ipiv, info)
  call begin_test('3x4')
  call print_array('a', a_r, 24)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

  ! Test 4: singular matrix (info > 0)
  ! A = [(1+0i) (0+0i) (0+0i); (0+0i) (0+0i) (0+0i); (0+0i) (0+0i) (1+0i)]
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0); a(2) = (0.0d0, 0.0d0); a(3) = (0.0d0, 0.0d0)
  a(4) = (0.0d0, 0.0d0); a(5) = (0.0d0, 0.0d0); a(6) = (0.0d0, 0.0d0)
  a(7) = (0.0d0, 0.0d0); a(8) = (0.0d0, 0.0d0); a(9) = (1.0d0, 0.0d0)
  ipiv = 0
  call zgetrf2(3, 3, a, 3, ipiv, info)
  call begin_test('singular')
  call print_array('a', a_r, 18)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

  ! Test 5: 1x1 matrix
  a(1) = (5.0d0, 3.0d0)
  ipiv = 0
  call zgetrf2(1, 1, a, 1, ipiv, info)
  call begin_test('1x1')
  call print_array('a', a_r, 2)
  call print_int_array('ipiv', ipiv, 1)
  call print_int('info', info)
  call end_test()

  ! Test 6: 1x1 singular (zero)
  a(1) = (0.0d0, 0.0d0)
  ipiv = 0
  call zgetrf2(1, 1, a, 1, ipiv, info)
  call begin_test('1x1_singular')
  call print_array('a', a_r, 2)
  call print_int_array('ipiv', ipiv, 1)
  call print_int('info', info)
  call end_test()

  ! Test 7: Nx1 column vector
  a(1) = (1.0d0, 0.5d0); a(2) = (5.0d0, 2.0d0); a(3) = (3.0d0, 1.0d0)
  ipiv = 0
  call zgetrf2(3, 1, a, 3, ipiv, info)
  call begin_test('col_vector')
  call print_array('a', a_r, 6)
  call print_int_array('ipiv', ipiv, 1)
  call print_int('info', info)
  call end_test()

  ! Test 8: 1xN row vector
  a(1) = (2.0d0, 1.0d0); a(2) = (3.0d0, 0.5d0); a(3) = (7.0d0, 2.0d0)
  ipiv = 0
  call zgetrf2(1, 3, a, 1, ipiv, info)
  call begin_test('row_vector')
  call print_array('a', a_r, 6)
  call print_int_array('ipiv', ipiv, 1)
  call print_int('info', info)
  call end_test()

  ! Test 9: 4x4 well-conditioned complex matrix
  ! Use a diagonally dominant matrix to ensure non-trivial A22
  a = (0.0d0, 0.0d0)
  a(1) = (10.0d0, 1.0d0); a(2) = (1.0d0, 2.0d0); a(3) = (2.0d0, -1.0d0); a(4) = (3.0d0, 0.5d0)
  a(5) = (1.0d0, -1.0d0); a(6) = (12.0d0, 2.0d0); a(7) = (1.0d0, 3.0d0); a(8) = (2.0d0, -0.5d0)
  a(9) = (2.0d0, 0.5d0); a(10) = (3.0d0, -1.0d0); a(11) = (15.0d0, 1.0d0); a(12) = (1.0d0, 2.0d0)
  a(13) = (1.0d0, 1.0d0); a(14) = (2.0d0, 0.5d0); a(15) = (3.0d0, -2.0d0); a(16) = (20.0d0, 3.0d0)
  ipiv = 0
  call zgetrf2(4, 4, a, 4, ipiv, info)
  call begin_test('4x4')
  call print_array('a', a_r, 32)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

end program
