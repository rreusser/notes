program test_dsytrf
  use test_utils
  implicit none
  double precision :: a(100), work(200)
  integer :: ipiv(10), info

  ! Test 1: 4x4 symmetric positive definite, UPLO='L'
  ! A = [ 4  2  1  0;
  !       2  5  2  1;
  !       1  2  6  3;
  !       0  1  3  8 ]
  a = 0.0d0
  a(1) = 4.0d0; a(2) = 2.0d0; a(3) = 1.0d0; a(4) = 0.0d0
  a(5) = 0.0d0; a(6) = 5.0d0; a(7) = 2.0d0; a(8) = 1.0d0
  a(9) = 0.0d0; a(10) = 0.0d0; a(11) = 6.0d0; a(12) = 3.0d0
  a(13) = 0.0d0; a(14) = 0.0d0; a(15) = 0.0d0; a(16) = 8.0d0
  ipiv = 0
  call dsytrf('L', 4, a, 4, ipiv, work, 200, info)
  call begin_test('4x4_lower')
  call print_array('a', a, 16)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 2: 4x4 symmetric positive definite, UPLO='U'
  a = 0.0d0
  a(1) = 4.0d0
  a(5) = 2.0d0; a(6) = 5.0d0
  a(9) = 1.0d0; a(10) = 2.0d0; a(11) = 6.0d0
  a(13) = 0.0d0; a(14) = 1.0d0; a(15) = 3.0d0; a(16) = 8.0d0
  ipiv = 0
  call dsytrf('U', 4, a, 4, ipiv, work, 200, info)
  call begin_test('4x4_upper')
  call print_array('a', a, 16)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 3: 4x4 truly indefinite (forces 2x2 pivots), UPLO='L'
  ! A = [ 0  1  2  3;
  !       1  0  4  5;
  !       2  4  0  6;
  !       3  5  6  0 ]
  a = 0.0d0
  a(1) = 0.0d0; a(2) = 1.0d0; a(3) = 2.0d0; a(4) = 3.0d0
  a(5) = 0.0d0; a(6) = 0.0d0; a(7) = 4.0d0; a(8) = 5.0d0
  a(9) = 0.0d0; a(10) = 0.0d0; a(11) = 0.0d0; a(12) = 6.0d0
  a(13) = 0.0d0; a(14) = 0.0d0; a(15) = 0.0d0; a(16) = 0.0d0
  ipiv = 0
  call dsytrf('L', 4, a, 4, ipiv, work, 200, info)
  call begin_test('4x4_indef_lower')
  call print_array('a', a, 16)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 4: 4x4 truly indefinite, UPLO='U'
  a = 0.0d0
  a(1) = 0.0d0
  a(5) = 1.0d0; a(6) = 0.0d0
  a(9) = 2.0d0; a(10) = 4.0d0; a(11) = 0.0d0
  a(13) = 3.0d0; a(14) = 5.0d0; a(15) = 6.0d0; a(16) = 0.0d0
  ipiv = 0
  call dsytrf('U', 4, a, 4, ipiv, work, 200, info)
  call begin_test('4x4_indef_upper')
  call print_array('a', a, 16)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 5: N=0
  ipiv = 0
  call dsytrf('L', 0, a, 1, ipiv, work, 200, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: N=1
  a(1) = 7.0d0
  ipiv = 0
  call dsytrf('L', 1, a, 1, ipiv, work, 200, info)
  call begin_test('n_one')
  call print_array('a', a, 1)
  call print_int_array('ipiv', ipiv, 1)
  call print_int('info', info)
  call end_test()

  ! Test 7: singular matrix
  a = 0.0d0
  ipiv = 0
  call dsytrf('L', 2, a, 2, ipiv, work, 200, info)
  call begin_test('singular')
  call print_array('a', a, 4)
  call print_int_array('ipiv', ipiv, 2)
  call print_int('info', info)
  call end_test()

  ! Test 8: 5x5 mixed 1x1 and 2x2 pivots, UPLO='L'
  a = 0.0d0
  a(1) = 1.0d0; a(2) = -2.0d0; a(3) = 0.0d0; a(4) = 3.0d0; a(5) = 1.0d0
  a(6) = 0.0d0; a(7) = 0.0d0; a(8) = 4.0d0; a(9) = -1.0d0; a(10) = 2.0d0
  a(11) = 0.0d0; a(12) = 0.0d0; a(13) = -3.0d0; a(14) = 2.0d0; a(15) = 0.0d0
  a(16) = 0.0d0; a(17) = 0.0d0; a(18) = 0.0d0; a(19) = 1.0d0; a(20) = -2.0d0
  a(21) = 0.0d0; a(22) = 0.0d0; a(23) = 0.0d0; a(24) = 0.0d0; a(25) = 4.0d0
  ipiv = 0
  call dsytrf('L', 5, a, 5, ipiv, work, 200, info)
  call begin_test('5x5_lower')
  call print_array('a', a, 25)
  call print_int_array('ipiv', ipiv, 5)
  call print_int('info', info)
  call end_test()

end program
