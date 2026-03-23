program test_dlasyf
  use test_utils
  implicit none
  double precision :: a(100), w(100)
  integer :: ipiv(10), info, kb

  ! Test 1: 6x6 symmetric indefinite, UPLO='L', NB=3
  ! This exercises the blocked panel factorization
  ! A = [ 2 -1  0  1  0  0;
  !      -1  3 -1  0  1  0;
  !       0 -1  2 -1  0  1;
  !       1  0 -1  4 -1  0;
  !       0  1  0 -1  3 -1;
  !       0  0  1  0 -1  2 ]
  a = 0.0d0
  ! col 1
  a(1) = 2.0d0; a(2) = -1.0d0; a(3) = 0.0d0; a(4) = 1.0d0; a(5) = 0.0d0; a(6) = 0.0d0
  ! col 2
  a(7) = 0.0d0; a(8) = 3.0d0; a(9) = -1.0d0; a(10) = 0.0d0; a(11) = 1.0d0; a(12) = 0.0d0
  ! col 3
  a(13) = 0.0d0; a(14) = 0.0d0; a(15) = 2.0d0; a(16) = -1.0d0; a(17) = 0.0d0; a(18) = 1.0d0
  ! col 4
  a(19) = 0.0d0; a(20) = 0.0d0; a(21) = 0.0d0; a(22) = 4.0d0; a(23) = -1.0d0; a(24) = 0.0d0
  ! col 5
  a(25) = 0.0d0; a(26) = 0.0d0; a(27) = 0.0d0; a(28) = 0.0d0; a(29) = 3.0d0; a(30) = -1.0d0
  ! col 6
  a(31) = 0.0d0; a(32) = 0.0d0; a(33) = 0.0d0; a(34) = 0.0d0; a(35) = 0.0d0; a(36) = 2.0d0
  w = 0.0d0
  ipiv = 0
  kb = 0
  call dlasyf('L', 6, 3, kb, a, 6, ipiv, w, 6, info)
  call begin_test('6x6_lower_nb3')
  call print_array('a', a, 36)
  call print_int_array('ipiv', ipiv, 6)
  call print_int('kb', kb)
  call print_int('info', info)
  call end_test()

  ! Test 2: 6x6 symmetric indefinite, UPLO='U', NB=3
  a = 0.0d0
  ! col 1
  a(1) = 2.0d0
  ! col 2
  a(7) = -1.0d0; a(8) = 3.0d0
  ! col 3
  a(14) = -1.0d0; a(15) = 2.0d0
  ! col 4
  a(19) = 1.0d0; a(22) = 4.0d0
  ! col 5
  a(26) = 1.0d0; a(28) = -1.0d0; a(29) = 3.0d0
  ! col 6
  a(33) = 1.0d0; a(35) = -1.0d0; a(36) = 2.0d0
  w = 0.0d0
  ipiv = 0
  kb = 0
  call dlasyf('U', 6, 3, kb, a, 6, ipiv, w, 6, info)
  call begin_test('6x6_upper_nb3')
  call print_array('a', a, 36)
  call print_int_array('ipiv', ipiv, 6)
  call print_int('kb', kb)
  call print_int('info', info)
  call end_test()

  ! Test 3: 6x6 with zero diagonal to force 2x2 pivots, UPLO='L', NB=3
  a = 0.0d0
  ! col 1
  a(1) = 0.0d0; a(2) = 3.0d0; a(3) = 1.0d0; a(4) = 2.0d0; a(5) = 0.0d0; a(6) = 1.0d0
  ! col 2
  a(8) = 0.0d0; a(9) = 2.0d0; a(10) = 1.0d0; a(11) = 3.0d0; a(12) = 0.0d0
  ! col 3
  a(15) = 0.0d0; a(16) = 1.0d0; a(17) = 2.0d0; a(18) = 3.0d0
  ! col 4
  a(22) = 5.0d0; a(23) = 1.0d0; a(24) = 2.0d0
  ! col 5
  a(29) = 0.0d0; a(30) = 1.0d0
  ! col 6
  a(36) = 0.0d0
  w = 0.0d0
  ipiv = 0
  kb = 0
  call dlasyf('L', 6, 3, kb, a, 6, ipiv, w, 6, info)
  call begin_test('6x6_indef_lower_nb3')
  call print_array('a', a, 36)
  call print_int_array('ipiv', ipiv, 6)
  call print_int('kb', kb)
  call print_int('info', info)
  call end_test()

  ! Test 4: 6x6 with zero diagonal, UPLO='U', NB=3
  a = 0.0d0
  ! Store upper triangle of same type of matrix
  ! col 1: a(1)=0
  a(1) = 0.0d0
  ! col 2: a(7)=3, a(8)=0
  a(7) = 3.0d0; a(8) = 0.0d0
  ! col 3: a(13)=1, a(14)=2, a(15)=0
  a(13) = 1.0d0; a(14) = 2.0d0; a(15) = 0.0d0
  ! col 4: a(19)=2, a(20)=1, a(21)=1, a(22)=5
  a(19) = 2.0d0; a(20) = 1.0d0; a(21) = 1.0d0; a(22) = 5.0d0
  ! col 5: a(25)=0, a(26)=3, a(27)=2, a(28)=1, a(29)=0
  a(25) = 0.0d0; a(26) = 3.0d0; a(27) = 2.0d0; a(28) = 1.0d0; a(29) = 0.0d0
  ! col 6: a(31)=1, a(32)=0, a(33)=3, a(34)=2, a(35)=1, a(36)=0
  a(31) = 1.0d0; a(32) = 0.0d0; a(33) = 3.0d0; a(34) = 2.0d0; a(35) = 1.0d0; a(36) = 0.0d0
  w = 0.0d0
  ipiv = 0
  kb = 0
  call dlasyf('U', 6, 3, kb, a, 6, ipiv, w, 6, info)
  call begin_test('6x6_indef_upper_nb3')
  call print_array('a', a, 36)
  call print_int_array('ipiv', ipiv, 6)
  call print_int('kb', kb)
  call print_int('info', info)
  call end_test()

end program
