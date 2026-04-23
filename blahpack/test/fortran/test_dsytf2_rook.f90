program test_dsytf2_rook
  use test_utils
  implicit none
  double precision :: a(100)
  integer :: ipiv(10), info

  ! Test 1: 4x4 tridiagonal, UPLO='L'
  a = 0.0d0
  a(1) = 2.0d0; a(2) = -1.0d0; a(3) = 0.0d0; a(4) = 0.0d0
  a(5) = 0.0d0; a(6) = 2.0d0; a(7) = -1.0d0; a(8) = 0.0d0
  a(9) = 0.0d0; a(10) = 0.0d0; a(11) = 2.0d0; a(12) = -1.0d0
  a(13) = 0.0d0; a(14) = 0.0d0; a(15) = 0.0d0; a(16) = 2.0d0
  ipiv = 0
  call dsytf2_rook('L', 4, a, 4, ipiv, info)
  call begin_test('4x4_lower')
  call print_array('a', a, 16)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 2: 4x4 tridiagonal, UPLO='U'
  a = 0.0d0
  a(1) = 2.0d0
  a(5) = -1.0d0; a(6) = 2.0d0
  a(9) = 0.0d0; a(10) = -1.0d0; a(11) = 2.0d0
  a(13) = 0.0d0; a(14) = 0.0d0; a(15) = -1.0d0; a(16) = 2.0d0
  ipiv = 0
  call dsytf2_rook('U', 4, a, 4, ipiv, info)
  call begin_test('4x4_upper')
  call print_array('a', a, 16)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 3: 4x4 truly indefinite (zero diagonal), UPLO='L'
  a = 0.0d0
  a(1) = 0.0d0; a(2) = 1.0d0; a(3) = 2.0d0; a(4) = 3.0d0
  a(5) = 0.0d0; a(6) = 0.0d0; a(7) = 4.0d0; a(8) = 5.0d0
  a(9) = 0.0d0; a(10) = 0.0d0; a(11) = 0.0d0; a(12) = 6.0d0
  a(13) = 0.0d0; a(14) = 0.0d0; a(15) = 0.0d0; a(16) = 0.0d0
  ipiv = 0
  call dsytf2_rook('L', 4, a, 4, ipiv, info)
  call begin_test('4x4_indef_lower')
  call print_array('a', a, 16)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 4: same, UPLO='U'
  a = 0.0d0
  a(5) = 1.0d0
  a(9) = 2.0d0; a(10) = 4.0d0
  a(13) = 3.0d0; a(14) = 5.0d0; a(15) = 6.0d0
  ipiv = 0
  call dsytf2_rook('U', 4, a, 4, ipiv, info)
  call begin_test('4x4_indef_upper')
  call print_array('a', a, 16)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 5: N=0
  ipiv = 0
  call dsytf2_rook('L', 0, a, 1, ipiv, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: N=1
  a = 0.0d0
  a(1) = 5.0d0
  ipiv = 0
  call dsytf2_rook('L', 1, a, 1, ipiv, info)
  call begin_test('n_one')
  call print_array('a', a, 1)
  call print_int_array('ipiv', ipiv, 1)
  call print_int('info', info)
  call end_test()

  ! Test 7: singular
  a = 0.0d0
  ipiv = 0
  call dsytf2_rook('L', 2, a, 2, ipiv, info)
  call begin_test('singular')
  call print_array('a', a, 4)
  call print_int_array('ipiv', ipiv, 2)
  call print_int('info', info)
  call end_test()

  ! Test 8: 5x5 symmetric indefinite, UPLO='L'
  a = 0.0d0
  a(1) = 1.0d0; a(2) = -2.0d0; a(3) = 0.0d0; a(4) = 3.0d0; a(5) = 1.0d0
  a(7) = 0.0d0; a(8) = 4.0d0; a(9) = -1.0d0; a(10) = 2.0d0
  a(13) = -3.0d0; a(14) = 2.0d0; a(15) = 0.0d0
  a(19) = 1.0d0; a(20) = -2.0d0
  a(25) = 4.0d0
  ipiv = 0
  call dsytf2_rook('L', 5, a, 5, ipiv, info)
  call begin_test('5x5_lower')
  call print_array('a', a, 25)
  call print_int_array('ipiv', ipiv, 5)
  call print_int('info', info)
  call end_test()

  ! Test 9: 5x5, UPLO='U'
  a = 0.0d0
  a(1) = 1.0d0
  a(6) = -2.0d0; a(7) = 0.0d0
  a(11) = 0.0d0; a(12) = 4.0d0; a(13) = -3.0d0
  a(16) = 3.0d0; a(17) = -1.0d0; a(18) = 2.0d0; a(19) = 1.0d0
  a(21) = 1.0d0; a(22) = 2.0d0; a(23) = 0.0d0; a(24) = -2.0d0; a(25) = 4.0d0
  ipiv = 0
  call dsytf2_rook('U', 5, a, 5, ipiv, info)
  call begin_test('5x5_upper')
  call print_array('a', a, 25)
  call print_int_array('ipiv', ipiv, 5)
  call print_int('info', info)
  call end_test()

  ! Test 10: 6x6 matrix designed to trigger rook search iteration (row-chain growing)
  ! Small diagonal, increasing off-diagonals force rook to iterate.
  ! A(i,j) = i+j+0.1 chain-style — every row max is next column max.
  ! Use symmetric indefinite with 0.5 on diagonal and increasing off-diagonals
  a = 0.0d0
  ! lower triangle column-major, 6x6
  ! col 1
  a(1) = 0.5d0; a(2) = 1.0d0; a(3) = 2.0d0; a(4) = 3.0d0; a(5) = 4.0d0; a(6) = 5.0d0
  ! col 2
  a(8) = 0.5d0; a(9) = 1.5d0; a(10) = 2.5d0; a(11) = 3.5d0; a(12) = 4.5d0
  ! col 3
  a(15) = 0.5d0; a(16) = 1.2d0; a(17) = 2.2d0; a(18) = 3.2d0
  ! col 4
  a(22) = 0.5d0; a(23) = 1.1d0; a(24) = 2.1d0
  ! col 5
  a(29) = 0.5d0; a(30) = 1.3d0
  ! col 6
  a(36) = 0.5d0
  ipiv = 0
  call dsytf2_rook('L', 6, a, 6, ipiv, info)
  call begin_test('6x6_rook_lower')
  call print_array('a', a, 36)
  call print_int_array('ipiv', ipiv, 6)
  call print_int('info', info)
  call end_test()

  ! Test 11: 6x6 same with UPLO='U'
  a = 0.0d0
  ! col 1
  a(1) = 0.5d0
  ! col 2
  a(7) = 1.0d0; a(8) = 0.5d0
  ! col 3
  a(13) = 2.0d0; a(14) = 1.5d0; a(15) = 0.5d0
  ! col 4
  a(19) = 3.0d0; a(20) = 2.5d0; a(21) = 1.2d0; a(22) = 0.5d0
  ! col 5
  a(25) = 4.0d0; a(26) = 3.5d0; a(27) = 2.2d0; a(28) = 1.1d0; a(29) = 0.5d0
  ! col 6
  a(31) = 5.0d0; a(32) = 4.5d0; a(33) = 3.2d0; a(34) = 2.1d0; a(35) = 1.3d0; a(36) = 0.5d0
  ipiv = 0
  call dsytf2_rook('U', 6, a, 6, ipiv, info)
  call begin_test('6x6_rook_upper')
  call print_array('a', a, 36)
  call print_int_array('ipiv', ipiv, 6)
  call print_int('info', info)
  call end_test()

  ! Test 12: 3x3 zero diagonal forcing 2x2 pivot with p != k (first-swap path)
  a = 0.0d0
  a(1) = 0.0d0; a(2) = 1.0d0; a(3) = 2.0d0
  a(5) = 0.0d0; a(6) = 3.0d0
  a(9) = 0.0d0
  ipiv = 0
  call dsytf2_rook('L', 3, a, 3, ipiv, info)
  call begin_test('3x3_indef_lower')
  call print_array('a', a, 9)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

  ! Test 13: same upper
  a = 0.0d0
  a(4) = 1.0d0
  a(7) = 2.0d0; a(8) = 3.0d0
  ipiv = 0
  call dsytf2_rook('U', 3, a, 3, ipiv, info)
  call begin_test('3x3_indef_upper')
  call print_array('a', a, 9)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

  ! Test 14: singular upper (zero matrix, UPLO='U')
  a = 0.0d0
  ipiv = 0
  call dsytf2_rook('U', 2, a, 2, ipiv, info)
  call begin_test('singular_upper')
  call print_array('a', a, 4)
  call print_int_array('ipiv', ipiv, 2)
  call print_int('info', info)
  call end_test()

  ! Test 15: 5x5 designed to make rook pivot search iterate (upper)
  ! Use exponentially growing off-diagonals so row max keeps chasing new columns.
  a = 0.0d0
  a(1) = 0.1d0
  a(6) = 1.0d0; a(7) = 0.1d0
  a(11) = 2.0d0; a(12) = 5.0d0; a(13) = 0.1d0
  a(16) = 3.0d0; a(17) = 10.0d0; a(18) = 50.0d0; a(19) = 0.1d0
  a(21) = 4.0d0; a(22) = 20.0d0; a(23) = 100.0d0; a(24) = 500.0d0; a(25) = 0.1d0
  ipiv = 0
  call dsytf2_rook('U', 5, a, 5, ipiv, info)
  call begin_test('5x5_chase_upper')
  call print_array('a', a, 25)
  call print_int_array('ipiv', ipiv, 5)
  call print_int('info', info)
  call end_test()

  ! Test 16: same chase pattern lower
  a = 0.0d0
  a(1) = 0.1d0; a(2) = 1.0d0; a(3) = 2.0d0; a(4) = 3.0d0; a(5) = 4.0d0
  a(7) = 0.1d0; a(8) = 5.0d0; a(9) = 10.0d0; a(10) = 20.0d0
  a(13) = 0.1d0; a(14) = 50.0d0; a(15) = 100.0d0
  a(19) = 0.1d0; a(20) = 500.0d0
  a(25) = 0.1d0
  ipiv = 0
  call dsytf2_rook('L', 5, a, 5, ipiv, info)
  call begin_test('5x5_chase_lower')
  call print_array('a', a, 25)
  call print_int_array('ipiv', ipiv, 5)
  call print_int('info', info)
  call end_test()

end program
