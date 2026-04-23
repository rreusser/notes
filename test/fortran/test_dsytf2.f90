program test_dsytf2
  use test_utils
  implicit none
  double precision :: a(100)
  integer :: ipiv(10), info

  ! Test 1: 4x4 symmetric indefinite, UPLO='L'
  ! A = [ 2 -1  0  0;
  !      -1  2 -1  0;
  !       0 -1  2 -1;
  !       0  0 -1  2 ] (tridiagonal, symmetric positive definite actually)
  ! Store lower triangle, column-major
  a = 0.0d0
  a(1) = 2.0d0; a(2) = -1.0d0; a(3) = 0.0d0; a(4) = 0.0d0
  a(5) = 0.0d0; a(6) = 2.0d0; a(7) = -1.0d0; a(8) = 0.0d0
  a(9) = 0.0d0; a(10) = 0.0d0; a(11) = 2.0d0; a(12) = -1.0d0
  a(13) = 0.0d0; a(14) = 0.0d0; a(15) = 0.0d0; a(16) = 2.0d0
  ipiv = 0
  call dsytf2('L', 4, a, 4, ipiv, info)
  call begin_test('4x4_lower')
  call print_array('a', a, 16)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 2: 4x4 symmetric indefinite, UPLO='U'
  ! Same matrix, upper triangle
  a = 0.0d0
  a(1) = 2.0d0; a(2) = 0.0d0; a(3) = 0.0d0; a(4) = 0.0d0
  a(5) = -1.0d0; a(6) = 2.0d0; a(7) = 0.0d0; a(8) = 0.0d0
  a(9) = 0.0d0; a(10) = -1.0d0; a(11) = 2.0d0; a(12) = 0.0d0
  a(13) = 0.0d0; a(14) = 0.0d0; a(15) = -1.0d0; a(16) = 2.0d0
  ipiv = 0
  call dsytf2('U', 4, a, 4, ipiv, info)
  call begin_test('4x4_upper')
  call print_array('a', a, 16)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 3: 4x4 truly indefinite (forces 2x2 pivots), UPLO='L'
  ! A = [ 0  1  2  3;
  !       1  0  4  5;
  !       2  4  0  6;
  !       3  5  6  0 ] - zero diagonal forces 2x2 pivots
  a = 0.0d0
  a(1) = 0.0d0; a(2) = 1.0d0; a(3) = 2.0d0; a(4) = 3.0d0
  a(5) = 0.0d0; a(6) = 0.0d0; a(7) = 4.0d0; a(8) = 5.0d0
  a(9) = 0.0d0; a(10) = 0.0d0; a(11) = 0.0d0; a(12) = 6.0d0
  a(13) = 0.0d0; a(14) = 0.0d0; a(15) = 0.0d0; a(16) = 0.0d0
  ipiv = 0
  call dsytf2('L', 4, a, 4, ipiv, info)
  call begin_test('4x4_indef_lower')
  call print_array('a', a, 16)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 4: same truly indefinite, UPLO='U'
  a = 0.0d0
  a(1) = 0.0d0; a(2) = 0.0d0; a(3) = 0.0d0; a(4) = 0.0d0
  a(5) = 1.0d0; a(6) = 0.0d0; a(7) = 0.0d0; a(8) = 0.0d0
  a(9) = 2.0d0; a(10) = 4.0d0; a(11) = 0.0d0; a(12) = 0.0d0
  a(13) = 3.0d0; a(14) = 5.0d0; a(15) = 6.0d0; a(16) = 0.0d0
  ipiv = 0
  call dsytf2('U', 4, a, 4, ipiv, info)
  call begin_test('4x4_indef_upper')
  call print_array('a', a, 16)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 5: N=0 quick return
  ipiv = 0
  call dsytf2('L', 0, a, 1, ipiv, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: N=1
  a(1) = 5.0d0
  ipiv = 0
  call dsytf2('L', 1, a, 1, ipiv, info)
  call begin_test('n_one')
  call print_array('a', a, 1)
  call print_int_array('ipiv', ipiv, 1)
  call print_int('info', info)
  call end_test()

  ! Test 7: singular matrix (all zeros)
  a = 0.0d0
  a(1) = 0.0d0; a(2) = 0.0d0; a(3) = 0.0d0; a(4) = 0.0d0
  ipiv = 0
  call dsytf2('L', 2, a, 2, ipiv, info)
  call begin_test('singular')
  call print_array('a', a, 4)
  call print_int_array('ipiv', ipiv, 2)
  call print_int('info', info)
  call end_test()

  ! Test 8: 5x5 with mixed 1x1 and 2x2 pivots, UPLO='L'
  ! A is symmetric indefinite:
  ! [ 1  -2   0   3   1;
  !  -2   0   4  -1   2;
  !   0   4  -3   2   0;
  !   3  -1   2   1  -2;
  !   1   2   0  -2   4 ]
  a = 0.0d0
  ! col 1
  a(1) = 1.0d0; a(2) = -2.0d0; a(3) = 0.0d0; a(4) = 3.0d0; a(5) = 1.0d0
  ! col 2
  a(6) = 0.0d0; a(7) = 0.0d0; a(8) = 4.0d0; a(9) = -1.0d0; a(10) = 2.0d0
  ! col 3
  a(11) = 0.0d0; a(12) = 0.0d0; a(13) = -3.0d0; a(14) = 2.0d0; a(15) = 0.0d0
  ! col 4
  a(16) = 0.0d0; a(17) = 0.0d0; a(18) = 0.0d0; a(19) = 1.0d0; a(20) = -2.0d0
  ! col 5
  a(21) = 0.0d0; a(22) = 0.0d0; a(23) = 0.0d0; a(24) = 0.0d0; a(25) = 4.0d0
  ipiv = 0
  call dsytf2('L', 5, a, 5, ipiv, info)
  call begin_test('5x5_lower')
  call print_array('a', a, 25)
  call print_int_array('ipiv', ipiv, 5)
  call print_int('info', info)
  call end_test()

  ! Test 9: 5x5 with mixed pivots, UPLO='U'
  ! Same matrix, upper triangle stored
  a = 0.0d0
  ! col 1
  a(1) = 1.0d0
  ! col 2
  a(6) = -2.0d0; a(7) = 0.0d0
  ! col 3
  a(11) = 0.0d0; a(12) = 4.0d0; a(13) = -3.0d0
  ! col 4
  a(16) = 3.0d0; a(17) = -1.0d0; a(18) = 2.0d0; a(19) = 1.0d0
  ! col 5
  a(21) = 1.0d0; a(22) = 2.0d0; a(23) = 0.0d0; a(24) = -2.0d0; a(25) = 4.0d0
  ipiv = 0
  call dsytf2('U', 5, a, 5, ipiv, info)
  call begin_test('5x5_upper')
  call print_array('a', a, 25)
  call print_int_array('ipiv', ipiv, 5)
  call print_int('info', info)
  call end_test()

end program
