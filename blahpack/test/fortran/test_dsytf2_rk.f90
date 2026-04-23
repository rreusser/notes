program test_dsytf2_rk
  use test_utils
  implicit none
  double precision :: a(100)
  double precision :: e(10)
  integer :: ipiv(10), info

  ! Test 1: 4x4 symmetric tridiagonal, UPLO='L'
  a = 0.0d0
  a(1) = 2.0d0; a(2) = -1.0d0; a(3) = 0.0d0; a(4) = 0.0d0
  a(5) = 0.0d0; a(6) = 2.0d0; a(7) = -1.0d0; a(8) = 0.0d0
  a(9) = 0.0d0; a(10) = 0.0d0; a(11) = 2.0d0; a(12) = -1.0d0
  a(13) = 0.0d0; a(14) = 0.0d0; a(15) = 0.0d0; a(16) = 2.0d0
  ipiv = 0; e = 0.0d0
  call dsytf2_rk('L', 4, a, 4, e, ipiv, info)
  call begin_test('4x4_lower')
  call print_array('a', a, 16)
  call print_array('e', e, 4)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 2: 4x4 symmetric tridiagonal, UPLO='U'
  a = 0.0d0
  a(1) = 2.0d0; a(2) = 0.0d0; a(3) = 0.0d0; a(4) = 0.0d0
  a(5) = -1.0d0; a(6) = 2.0d0; a(7) = 0.0d0; a(8) = 0.0d0
  a(9) = 0.0d0; a(10) = -1.0d0; a(11) = 2.0d0; a(12) = 0.0d0
  a(13) = 0.0d0; a(14) = 0.0d0; a(15) = -1.0d0; a(16) = 2.0d0
  ipiv = 0; e = 0.0d0
  call dsytf2_rk('U', 4, a, 4, e, ipiv, info)
  call begin_test('4x4_upper')
  call print_array('a', a, 16)
  call print_array('e', e, 4)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 3: 4x4 indefinite (zero diagonal -> forces 2x2 pivots), UPLO='L'
  a = 0.0d0
  a(1) = 0.0d0; a(2) = 1.0d0; a(3) = 2.0d0; a(4) = 3.0d0
  a(5) = 0.0d0; a(6) = 0.0d0; a(7) = 4.0d0; a(8) = 5.0d0
  a(9) = 0.0d0; a(10) = 0.0d0; a(11) = 0.0d0; a(12) = 6.0d0
  a(13) = 0.0d0; a(14) = 0.0d0; a(15) = 0.0d0; a(16) = 0.0d0
  ipiv = 0; e = 0.0d0
  call dsytf2_rk('L', 4, a, 4, e, ipiv, info)
  call begin_test('4x4_indef_lower')
  call print_array('a', a, 16)
  call print_array('e', e, 4)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 4: 4x4 indefinite, UPLO='U'
  a = 0.0d0
  a(1) = 0.0d0; a(2) = 0.0d0; a(3) = 0.0d0; a(4) = 0.0d0
  a(5) = 1.0d0; a(6) = 0.0d0; a(7) = 0.0d0; a(8) = 0.0d0
  a(9) = 2.0d0; a(10) = 4.0d0; a(11) = 0.0d0; a(12) = 0.0d0
  a(13) = 3.0d0; a(14) = 5.0d0; a(15) = 6.0d0; a(16) = 0.0d0
  ipiv = 0; e = 0.0d0
  call dsytf2_rk('U', 4, a, 4, e, ipiv, info)
  call begin_test('4x4_indef_upper')
  call print_array('a', a, 16)
  call print_array('e', e, 4)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 5: N=1, lower
  a = 0.0d0; a(1) = 5.0d0
  ipiv = 0; e = 0.0d0
  call dsytf2_rk('L', 1, a, 1, e, ipiv, info)
  call begin_test('n1_lower')
  call print_array('a', a, 1)
  call print_array('e', e, 1)
  call print_int_array('ipiv', ipiv, 1)
  call print_int('info', info)
  call end_test()

  ! Test 6: 5x5 well-conditioned mixed signs, UPLO='L'
  a = 0.0d0
  ! Column 1
  a(1)=4.0d0;  a(2)=1.0d0;  a(3)=-2.0d0; a(4)=0.5d0; a(5)=1.5d0
  ! Column 2
  a(6)=0.0d0;  a(7)=-3.0d0; a(8)=1.0d0;  a(9)=2.0d0; a(10)=0.0d0
  ! Column 3
  a(11)=0.0d0; a(12)=0.0d0; a(13)=5.0d0; a(14)=-1.0d0; a(15)=0.5d0
  ! Column 4
  a(16)=0.0d0; a(17)=0.0d0; a(18)=0.0d0; a(19)=2.0d0; a(20)=1.0d0
  ! Column 5
  a(21)=0.0d0; a(22)=0.0d0; a(23)=0.0d0; a(24)=0.0d0; a(25)=-4.0d0
  ipiv = 0; e = 0.0d0
  call dsytf2_rk('L', 5, a, 5, e, ipiv, info)
  call begin_test('5x5_lower')
  call print_array('a', a, 25)
  call print_array('e', e, 5)
  call print_int_array('ipiv', ipiv, 5)
  call print_int('info', info)
  call end_test()

  ! Test 7: 5x5 same matrix, UPLO='U'
  a = 0.0d0
  ! Upper triangle column-major: column j has entries i=1..j
  ! A(1,1)=4
  a(1) = 4.0d0
  ! A(1,2)=1, A(2,2)=-3
  a(6) = 1.0d0; a(7) = -3.0d0
  ! A(1,3)=-2, A(2,3)=1, A(3,3)=5
  a(11) = -2.0d0; a(12) = 1.0d0; a(13) = 5.0d0
  ! A(1,4)=0.5, A(2,4)=2, A(3,4)=-1, A(4,4)=2
  a(16) = 0.5d0; a(17) = 2.0d0; a(18) = -1.0d0; a(19) = 2.0d0
  ! A(1,5)=1.5, A(2,5)=0, A(3,5)=0.5, A(4,5)=1, A(5,5)=-4
  a(21) = 1.5d0; a(22) = 0.0d0; a(23) = 0.5d0; a(24) = 1.0d0; a(25) = -4.0d0
  ipiv = 0; e = 0.0d0
  call dsytf2_rk('U', 5, a, 5, e, ipiv, info)
  call begin_test('5x5_upper')
  call print_array('a', a, 25)
  call print_array('e', e, 5)
  call print_int_array('ipiv', ipiv, 5)
  call print_int('info', info)
  call end_test()

  ! Test 8: 5x5 forcing non-trivial pivots, UPLO='L'
  ! Small diagonal with a large off-diagonal column
  ! This should trigger both 2x2 blocks and row swaps
  a = 0.0d0
  a(1)=0.1d0;  a(2)=10.0d0; a(3)=2.0d0;  a(4)=3.0d0;  a(5)=1.0d0
  a(6)=0.0d0;  a(7)=0.2d0;  a(8)=1.0d0;  a(9)=-2.0d0; a(10)=4.0d0
  a(11)=0.0d0; a(12)=0.0d0; a(13)=0.3d0; a(14)=5.0d0; a(15)=-1.0d0
  a(16)=0.0d0; a(17)=0.0d0; a(18)=0.0d0; a(19)=0.4d0; a(20)=6.0d0
  a(21)=0.0d0; a(22)=0.0d0; a(23)=0.0d0; a(24)=0.0d0; a(25)=0.5d0
  ipiv = 0; e = 0.0d0
  call dsytf2_rk('L', 5, a, 5, e, ipiv, info)
  call begin_test('5x5_pivots_lower')
  call print_array('a', a, 25)
  call print_array('e', e, 5)
  call print_int_array('ipiv', ipiv, 5)
  call print_int('info', info)
  call end_test()

  ! Test 9: 5x5 small diagonal, UPLO='U'
  a = 0.0d0
  ! A(1,1)=0.1
  a(1) = 0.1d0
  ! col 2: A(1,2)=10, A(2,2)=0.2
  a(6) = 10.0d0; a(7) = 0.2d0
  ! col 3: A(1,3)=2, A(2,3)=1, A(3,3)=0.3
  a(11) = 2.0d0; a(12) = 1.0d0; a(13) = 0.3d0
  ! col 4: A(1,4)=3, A(2,4)=-2, A(3,4)=5, A(4,4)=0.4
  a(16) = 3.0d0; a(17) = -2.0d0; a(18) = 5.0d0; a(19) = 0.4d0
  ! col 5: A(1,5)=1, A(2,5)=4, A(3,5)=-1, A(4,5)=6, A(5,5)=0.5
  a(21) = 1.0d0; a(22) = 4.0d0; a(23) = -1.0d0; a(24) = 6.0d0; a(25) = 0.5d0
  ipiv = 0; e = 0.0d0
  call dsytf2_rk('U', 5, a, 5, e, ipiv, info)
  call begin_test('5x5_pivots_upper')
  call print_array('a', a, 25)
  call print_array('e', e, 5)
  call print_int_array('ipiv', ipiv, 5)
  call print_int('info', info)
  call end_test()

  ! Test 10: singular (zero column) to trigger info > 0, UPLO='L'
  a = 0.0d0
  a(1)=1.0d0; a(2)=0.0d0; a(3)=0.0d0
  a(4)=0.0d0; a(5)=0.0d0; a(6)=0.0d0
  a(7)=0.0d0; a(8)=0.0d0; a(9)=2.0d0
  ipiv = 0; e = 0.0d0
  call dsytf2_rk('L', 3, a, 3, e, ipiv, info)
  call begin_test('3x3_singular_lower')
  call print_array('a', a, 9)
  call print_array('e', e, 3)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

  ! Test 11: singular UPLO='U'
  a = 0.0d0
  a(1)=1.0d0
  a(4)=0.0d0; a(5)=0.0d0
  a(7)=0.0d0; a(8)=0.0d0; a(9)=2.0d0
  ipiv = 0; e = 0.0d0
  call dsytf2_rk('U', 3, a, 3, e, ipiv, info)
  call begin_test('3x3_singular_upper')
  call print_array('a', a, 9)
  call print_array('e', e, 3)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

  ! Test 12: upper 1x1 pivot with actual swap.
  ! A(5,5) tiny, A(1,1) large, A(1,5) large forces kp=0 swap with kk=4
  a = 0.0d0
  a(1)=9.0d0
  a(6)=0.5d0; a(7)=8.0d0
  a(11)=0.1d0; a(12)=0.2d0; a(13)=7.0d0
  a(16)=0.3d0; a(17)=0.4d0; a(18)=0.6d0; a(19)=6.0d0
  a(21)=10.0d0; a(22)=0.7d0; a(23)=0.8d0; a(24)=0.9d0; a(25)=0.01d0
  ipiv = 0; e = 0.0d0
  call dsytf2_rk('U', 5, a, 5, e, ipiv, info)
  call begin_test('5x5_swap1x1_upper')
  call print_array('a', a, 25)
  call print_array('e', e, 5)
  call print_int_array('ipiv', ipiv, 5)
  call print_int('info', info)
  call end_test()

  ! Test 13: lower version of the same
  a = 0.0d0
  a(1)=0.01d0; a(2)=0.7d0; a(3)=0.8d0; a(4)=0.9d0; a(5)=10.0d0
  a(6)=0.0d0; a(7)=6.0d0; a(8)=0.6d0; a(9)=0.4d0; a(10)=0.3d0
  a(11)=0.0d0; a(12)=0.0d0; a(13)=7.0d0; a(14)=0.2d0; a(15)=0.1d0
  a(16)=0.0d0; a(17)=0.0d0; a(18)=0.0d0; a(19)=8.0d0; a(20)=0.5d0
  a(21)=0.0d0; a(22)=0.0d0; a(23)=0.0d0; a(24)=0.0d0; a(25)=9.0d0
  ipiv = 0; e = 0.0d0
  call dsytf2_rk('L', 5, a, 5, e, ipiv, info)
  call begin_test('5x5_swap1x1_lower')
  call print_array('a', a, 25)
  call print_array('e', e, 5)
  call print_int_array('ipiv', ipiv, 5)
  call print_int('info', info)
  call end_test()

  ! Test 14: upper 2x2 pivot with p != k swap (rook iteration cycles)
  ! A(4,4)=A(5,5)=0 (tiny), A(4,5) large, A(3,5) large, A(1,4) large, A(1,1) moderate
  a = 0.0d0
  a(1)=3.0d0
  a(6)=0.1d0; a(7)=2.5d0
  a(11)=0.2d0; a(12)=0.3d0; a(13)=2.0d0
  a(16)=20.0d0; a(17)=0.4d0; a(18)=0.5d0; a(19)=0.02d0
  a(21)=0.6d0; a(22)=0.7d0; a(23)=30.0d0; a(24)=25.0d0; a(25)=0.01d0
  ipiv = 0; e = 0.0d0
  call dsytf2_rk('U', 5, a, 5, e, ipiv, info)
  call begin_test('5x5_rook_upper')
  call print_array('a', a, 25)
  call print_array('e', e, 5)
  call print_int_array('ipiv', ipiv, 5)
  call print_int('info', info)
  call end_test()

  ! Test 15: lower 2x2 pivot with p != k swap (rook iteration cycles)
  a = 0.0d0
  a(1)=0.01d0; a(2)=25.0d0; a(3)=30.0d0; a(4)=0.7d0; a(5)=0.6d0
  a(6)=0.0d0; a(7)=0.02d0; a(8)=0.5d0; a(9)=0.4d0; a(10)=20.0d0
  a(11)=0.0d0; a(12)=0.0d0; a(13)=2.0d0; a(14)=0.3d0; a(15)=0.2d0
  a(16)=0.0d0; a(17)=0.0d0; a(18)=0.0d0; a(19)=2.5d0; a(20)=0.1d0
  a(21)=0.0d0; a(22)=0.0d0; a(23)=0.0d0; a(24)=0.0d0; a(25)=3.0d0
  ipiv = 0; e = 0.0d0
  call dsytf2_rk('L', 5, a, 5, e, ipiv, info)
  call begin_test('5x5_rook_lower')
  call print_array('a', a, 25)
  call print_array('e', e, 5)
  call print_int_array('ipiv', ipiv, 5)
  call print_int('info', info)
  call end_test()

  ! Test 16: lower rook cycle -> p != k first swap
  ! A(1,1)=tiny, A(2,1)=100 large, A(2,5)=200 larger, A(5,1)=100
  ! rook cycles: imax=2 -> case 4 -> imax=5 -> case 3 with p=2, k=1, p!=k
  a = 0.0d0
  a(1)=0.01d0; a(2)=100.0d0; a(3)=0.5d0; a(4)=0.3d0; a(5)=100.0d0
  a(6)=0.0d0;  a(7)=0.02d0;  a(8)=0.4d0; a(9)=0.2d0; a(10)=200.0d0
  a(11)=0.0d0; a(12)=0.0d0;  a(13)=2.0d0; a(14)=0.1d0; a(15)=0.05d0
  a(16)=0.0d0; a(17)=0.0d0;  a(18)=0.0d0; a(19)=3.0d0; a(20)=0.06d0
  a(21)=0.0d0; a(22)=0.0d0;  a(23)=0.0d0; a(24)=0.0d0; a(25)=0.03d0
  ipiv = 0; e = 0.0d0
  call dsytf2_rk('L', 5, a, 5, e, ipiv, info)
  call begin_test('5x5_rook_cycle_lower')
  call print_array('a', a, 25)
  call print_array('e', e, 5)
  call print_int_array('ipiv', ipiv, 5)
  call print_int('info', info)
  call end_test()

  ! Test 17: upper rook cycle -> p != k first swap (mirrored)
  a = 0.0d0
  ! Column 1: A(1,1) = 0.03
  a(1)=0.03d0
  ! Column 2: A(1,2)=0.06, A(2,2)=3
  a(6)=0.06d0; a(7)=3.0d0
  ! Column 3: A(1,3)=0.05, A(2,3)=0.1, A(3,3)=2
  a(11)=0.05d0; a(12)=0.1d0; a(13)=2.0d0
  ! Column 4: A(1,4)=200, A(2,4)=0.2, A(3,4)=0.4, A(4,4)=0.02
  a(16)=200.0d0; a(17)=0.2d0; a(18)=0.4d0; a(19)=0.02d0
  ! Column 5: A(1,5)=100, A(2,5)=0.3, A(3,5)=0.5, A(4,5)=100, A(5,5)=0.01
  a(21)=100.0d0; a(22)=0.3d0; a(23)=0.5d0; a(24)=100.0d0; a(25)=0.01d0
  ipiv = 0; e = 0.0d0
  call dsytf2_rk('U', 5, a, 5, e, ipiv, info)
  call begin_test('5x5_rook_cycle_upper')
  call print_array('a', a, 25)
  call print_array('e', e, 5)
  call print_int_array('ipiv', ipiv, 5)
  call print_int('info', info)
  call end_test()

end program test_dsytf2_rk
