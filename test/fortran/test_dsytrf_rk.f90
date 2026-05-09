program test_dsytrf_rk
  use test_utils
  implicit none
  double precision :: a(2500), e(50), work(2000)
  integer :: ipiv(50), info, i, j

  ! Test 1: 4x4 symmetric positive definite, UPLO='L'
  ! A = [ 4  2  1  0;
  !       2  5  2  1;
  !       1  2  6  3;
  !       0  1  3  8 ]
  a = 0.0d0; e = 0.0d0; ipiv = 0
  a(1) = 4.0d0; a(2) = 2.0d0; a(3) = 1.0d0; a(4) = 0.0d0
  a(5) = 0.0d0; a(6) = 5.0d0; a(7) = 2.0d0; a(8) = 1.0d0
  a(9) = 0.0d0; a(10) = 0.0d0; a(11) = 6.0d0; a(12) = 3.0d0
  a(13) = 0.0d0; a(14) = 0.0d0; a(15) = 0.0d0; a(16) = 8.0d0
  call dsytrf_rk('L', 4, a, 4, e, ipiv, work, 2000, info)
  call begin_test('4x4_lower')
  call print_array('a', a, 16)
  call print_array('e', e, 4)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 2: 4x4 symmetric positive definite, UPLO='U'
  a = 0.0d0; e = 0.0d0; ipiv = 0
  a(1) = 4.0d0
  a(5) = 2.0d0; a(6) = 5.0d0
  a(9) = 1.0d0; a(10) = 2.0d0; a(11) = 6.0d0
  a(13) = 0.0d0; a(14) = 1.0d0; a(15) = 3.0d0; a(16) = 8.0d0
  call dsytrf_rk('U', 4, a, 4, e, ipiv, work, 2000, info)
  call begin_test('4x4_upper')
  call print_array('a', a, 16)
  call print_array('e', e, 4)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 3: 4x4 truly indefinite (forces 2x2 pivots), UPLO='L'
  a = 0.0d0; e = 0.0d0; ipiv = 0
  a(1) = 0.0d0; a(2) = 1.0d0; a(3) = 2.0d0; a(4) = 3.0d0
  a(5) = 0.0d0; a(6) = 0.0d0; a(7) = 4.0d0; a(8) = 5.0d0
  a(9) = 0.0d0; a(10) = 0.0d0; a(11) = 0.0d0; a(12) = 6.0d0
  a(13) = 0.0d0; a(14) = 0.0d0; a(15) = 0.0d0; a(16) = 0.0d0
  call dsytrf_rk('L', 4, a, 4, e, ipiv, work, 2000, info)
  call begin_test('4x4_indef_lower')
  call print_array('a', a, 16)
  call print_array('e', e, 4)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 4: 4x4 truly indefinite, UPLO='U'
  a = 0.0d0; e = 0.0d0; ipiv = 0
  a(1) = 0.0d0
  a(5) = 1.0d0; a(6) = 0.0d0
  a(9) = 2.0d0; a(10) = 4.0d0; a(11) = 0.0d0
  a(13) = 3.0d0; a(14) = 5.0d0; a(15) = 6.0d0; a(16) = 0.0d0
  call dsytrf_rk('U', 4, a, 4, e, ipiv, work, 2000, info)
  call begin_test('4x4_indef_upper')
  call print_array('a', a, 16)
  call print_array('e', e, 4)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 5: N=0 — quick return
  a = 0.0d0; e = 0.0d0; ipiv = 0
  call dsytrf_rk('L', 0, a, 1, e, ipiv, work, 2000, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: N=1
  a = 0.0d0; e = 0.0d0; ipiv = 0
  a(1) = 7.0d0
  call dsytrf_rk('L', 1, a, 1, e, ipiv, work, 2000, info)
  call begin_test('n_one')
  call print_array('a', a, 1)
  call print_array('e', e, 1)
  call print_int_array('ipiv', ipiv, 1)
  call print_int('info', info)
  call end_test()

  ! Test 7: singular (zero block)
  a = 0.0d0; e = 0.0d0; ipiv = 0
  call dsytrf_rk('L', 2, a, 2, e, ipiv, work, 2000, info)
  call begin_test('singular_lower')
  call print_array('a', a, 4)
  call print_array('e', e, 2)
  call print_int_array('ipiv', ipiv, 2)
  call print_int('info', info)
  call end_test()

  ! Test 8: 5x5 mixed 1x1 and 2x2 pivots, UPLO='L' (diagonally dominant for stability)
  a = 0.0d0; e = 0.0d0; ipiv = 0
  a(1)=4.0d0;  a(2)=1.0d0;  a(3)=-2.0d0; a(4)=0.5d0; a(5)=1.5d0
  a(6)=0.0d0;  a(7)=-3.0d0; a(8)=1.0d0;  a(9)=2.0d0; a(10)=0.0d0
  a(11)=0.0d0; a(12)=0.0d0; a(13)=5.0d0; a(14)=-1.0d0; a(15)=0.5d0
  a(16)=0.0d0; a(17)=0.0d0; a(18)=0.0d0; a(19)=2.0d0; a(20)=1.0d0
  a(21)=0.0d0; a(22)=0.0d0; a(23)=0.0d0; a(24)=0.0d0; a(25)=-4.0d0
  call dsytrf_rk('L', 5, a, 5, e, ipiv, work, 2000, info)
  call begin_test('5x5_lower')
  call print_array('a', a, 25)
  call print_array('e', e, 5)
  call print_int_array('ipiv', ipiv, 5)
  call print_int('info', info)
  call end_test()

  ! Test 9: 5x5 same matrix, UPLO='U'
  a = 0.0d0; e = 0.0d0; ipiv = 0
  a(1) = 4.0d0
  a(6) = 1.0d0; a(7) = -3.0d0
  a(11) = -2.0d0; a(12) = 1.0d0; a(13) = 5.0d0
  a(16) = 0.5d0; a(17) = 2.0d0; a(18) = -1.0d0; a(19) = 2.0d0
  a(21) = 1.5d0; a(22) = 0.0d0; a(23) = 0.5d0; a(24) = 1.0d0; a(25) = -4.0d0
  call dsytrf_rk('U', 5, a, 5, e, ipiv, work, 2000, info)
  call begin_test('5x5_upper')
  call print_array('a', a, 25)
  call print_array('e', e, 5)
  call print_int_array('ipiv', ipiv, 5)
  call print_int('info', info)
  call end_test()

  ! Test 10: 50x50 well-conditioned (forces blocked path with NB=32)
  ! Use deterministic transcendental fill with diagonal boost.
  ! NMAX in our local arrays is 50, so declare A(50,50). Pack into 2500 entries.
  a = 0.0d0; e = 0.0d0; ipiv = 0
  do j = 1, 50
    do i = 1, 50
      if (i.ge.j) then
        a((j-1)*50 + i) = sin(dble(i)*0.7d0) * cos(dble(j)*0.3d0) + &
                          0.5d0 * sin(dble(i+j)*0.13d0)
      end if
    end do
    ! Diagonal boost for well-conditioning
    a((j-1)*50 + j) = 50.0d0 + dble(j) * 0.1d0
  end do
  call dsytrf_rk('L', 50, a, 50, e, ipiv, work, 2000, info)
  call begin_test('50x50_lower_blocked')
  call print_array('a', a, 2500)
  call print_array('e', e, 50)
  call print_int_array('ipiv', ipiv, 50)
  call print_int('info', info)
  call end_test()

  ! Test 11: 50x50 upper blocked
  a = 0.0d0; e = 0.0d0; ipiv = 0
  do j = 1, 50
    do i = 1, j
      a((j-1)*50 + i) = sin(dble(i)*0.7d0) * cos(dble(j)*0.3d0) + &
                        0.5d0 * sin(dble(i+j)*0.13d0)
    end do
    a((j-1)*50 + j) = 50.0d0 + dble(j) * 0.1d0
  end do
  call dsytrf_rk('U', 50, a, 50, e, ipiv, work, 2000, info)
  call begin_test('50x50_upper_blocked')
  call print_array('a', a, 2500)
  call print_array('e', e, 50)
  call print_int_array('ipiv', ipiv, 50)
  call print_int('info', info)
  call end_test()

  ! Test 12: 33x33 lower (just over NB=32 — exercises one blocked panel + small unblocked tail)
  a = 0.0d0; e = 0.0d0; ipiv = 0
  do j = 1, 33
    do i = j, 33
      a((j-1)*33 + i) = sin(dble(i)*1.1d0) * cos(dble(j)*0.5d0) + &
                        0.3d0 * sin(dble(i*j)*0.07d0)
    end do
    a((j-1)*33 + j) = 33.0d0 + dble(j) * 0.05d0
  end do
  call dsytrf_rk('L', 33, a, 33, e, ipiv, work, 2000, info)
  call begin_test('33x33_lower_blocked')
  call print_array('a', a, 33*33)
  call print_array('e', e, 33)
  call print_int_array('ipiv', ipiv, 33)
  call print_int('info', info)
  call end_test()

  ! Test 13: 33x33 upper
  a = 0.0d0; e = 0.0d0; ipiv = 0
  do j = 1, 33
    do i = 1, j
      a((j-1)*33 + i) = sin(dble(i)*1.1d0) * cos(dble(j)*0.5d0) + &
                        0.3d0 * sin(dble(i*j)*0.07d0)
    end do
    a((j-1)*33 + j) = 33.0d0 + dble(j) * 0.05d0
  end do
  call dsytrf_rk('U', 33, a, 33, e, ipiv, work, 2000, info)
  call begin_test('33x33_upper_blocked')
  call print_array('a', a, 33*33)
  call print_array('e', e, 33)
  call print_int_array('ipiv', ipiv, 33)
  call print_int('info', info)
  call end_test()

end program test_dsytrf_rk
