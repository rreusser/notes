program test_dsygst
  use test_utils
  implicit none
  double precision :: a(16), b(16)
  integer :: info

  ! Parameters for blocked test
  integer, parameter :: NBIG = 70
  double precision :: a_big(NBIG*NBIG), b_big(NBIG*NBIG)
  double precision :: a_packed(NBIG*NBIG)
  integer :: i, j

  ! Test 1: ITYPE=1, UPLO='U', N=3
  ! Same setup as dsygs2 tests
  b = 0.0d0
  b(1) = 4.0d0; b(2) = 2.0d0; b(4) = 2.0d0; b(5) = 5.0d0
  b(6) = 1.0d0; b(8) = 1.0d0; b(9) = 3.0d0
  call dpotrf('U', 3, b, 3, info)

  a = 0.0d0
  a(1) = 4.0d0; a(4) = 2.0d0; a(5) = 5.0d0
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 6.0d0
  call dsygst(1, 'U', 3, a, 3, b, 3, info)
  call begin_test('itype1_upper')
  call print_int('info', info)
  call print_matrix('A', a, 3, 3, 3)
  call end_test()

  ! Test 2: ITYPE=1, UPLO='L', N=3
  b = 0.0d0
  b(1) = 4.0d0; b(2) = 2.0d0; b(4) = 2.0d0; b(5) = 5.0d0
  b(6) = 1.0d0; b(8) = 1.0d0; b(9) = 3.0d0
  call dpotrf('L', 3, b, 3, info)

  a = 0.0d0
  a(1) = 4.0d0; a(2) = 2.0d0; a(3) = 1.0d0
  a(5) = 5.0d0; a(6) = 3.0d0; a(9) = 6.0d0
  call dsygst(1, 'L', 3, a, 3, b, 3, info)
  call begin_test('itype1_lower')
  call print_int('info', info)
  call print_matrix('A', a, 3, 3, 3)
  call end_test()

  ! Test 3: ITYPE=2, UPLO='U', N=3
  b = 0.0d0
  b(1) = 4.0d0; b(2) = 2.0d0; b(4) = 2.0d0; b(5) = 5.0d0
  b(6) = 1.0d0; b(8) = 1.0d0; b(9) = 3.0d0
  call dpotrf('U', 3, b, 3, info)

  a = 0.0d0
  a(1) = 4.0d0; a(4) = 2.0d0; a(5) = 5.0d0
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 6.0d0
  call dsygst(2, 'U', 3, a, 3, b, 3, info)
  call begin_test('itype2_upper')
  call print_int('info', info)
  call print_matrix('A', a, 3, 3, 3)
  call end_test()

  ! Test 4: ITYPE=2, UPLO='L', N=3
  b = 0.0d0
  b(1) = 4.0d0; b(2) = 2.0d0; b(4) = 2.0d0; b(5) = 5.0d0
  b(6) = 1.0d0; b(8) = 1.0d0; b(9) = 3.0d0
  call dpotrf('L', 3, b, 3, info)

  a = 0.0d0
  a(1) = 4.0d0; a(2) = 2.0d0; a(3) = 1.0d0
  a(5) = 5.0d0; a(6) = 3.0d0; a(9) = 6.0d0
  call dsygst(2, 'L', 3, a, 3, b, 3, info)
  call begin_test('itype2_lower')
  call print_int('info', info)
  call print_matrix('A', a, 3, 3, 3)
  call end_test()

  ! Test 5: ITYPE=3, UPLO='L', N=3
  b = 0.0d0
  b(1) = 4.0d0; b(2) = 2.0d0; b(4) = 2.0d0; b(5) = 5.0d0
  b(6) = 1.0d0; b(8) = 1.0d0; b(9) = 3.0d0
  call dpotrf('L', 3, b, 3, info)

  a = 0.0d0
  a(1) = 4.0d0; a(2) = 2.0d0; a(3) = 1.0d0
  a(5) = 5.0d0; a(6) = 3.0d0; a(9) = 6.0d0
  call dsygst(3, 'L', 3, a, 3, b, 3, info)
  call begin_test('itype3_lower')
  call print_int('info', info)
  call print_matrix('A', a, 3, 3, 3)
  call end_test()

  ! Test 6: N=0 quick return
  call dsygst(1, 'U', 0, a, 1, b, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 7: N=1
  a(1) = 9.0d0
  b(1) = 3.0d0
  call dsygst(1, 'U', 1, a, 1, b, 1, info)
  call begin_test('n_one')
  call print_int('info', info)
  call print_scalar('A11', a(1))
  call end_test()

  ! ============================================================
  ! Test 8: Blocked path - ITYPE=1, UPLO='U', N=70
  ! Build diagonally dominant SPD B and symmetric A
  b_big = 0.0d0
  do j = 1, NBIG
    do i = 1, NBIG
      if (i == j) then
        b_big((j-1)*NBIG + i) = dble(NBIG) + 1.0d0
      else if (abs(i - j) == 1) then
        b_big((j-1)*NBIG + i) = 0.5d0
      end if
    end do
  end do
  call dpotrf('U', NBIG, b_big, NBIG, info)

  a_big = 0.0d0
  do j = 1, NBIG
    do i = 1, j
      if (i == j) then
        a_big((j-1)*NBIG + i) = dble(2*NBIG) + dble(i)
      else
        a_big((j-1)*NBIG + i) = 0.1d0 * dble(i + j)
      end if
    end do
  end do
  call dsygst(1, 'U', NBIG, a_big, NBIG, b_big, NBIG, info)

  ! Pack (already contiguous since LDA=NBIG)
  do j = 1, NBIG
    do i = 1, NBIG
      a_packed((j-1)*NBIG + i) = a_big((j-1)*NBIG + i)
    end do
  end do
  call begin_test('blocked_itype1_upper_70')
  call print_int('info', info)
  call print_array('A', a_packed, NBIG*NBIG)
  call end_test()

  ! ============================================================
  ! Test 9: Blocked path - ITYPE=1, UPLO='L', N=70
  b_big = 0.0d0
  do j = 1, NBIG
    do i = 1, NBIG
      if (i == j) then
        b_big((j-1)*NBIG + i) = dble(NBIG) + 1.0d0
      else if (abs(i - j) == 1) then
        b_big((j-1)*NBIG + i) = 0.5d0
      end if
    end do
  end do
  call dpotrf('L', NBIG, b_big, NBIG, info)

  a_big = 0.0d0
  do j = 1, NBIG
    do i = j, NBIG
      if (i == j) then
        a_big((j-1)*NBIG + i) = dble(2*NBIG) + dble(i)
      else
        a_big((j-1)*NBIG + i) = 0.1d0 * dble(i + j)
      end if
    end do
  end do
  call dsygst(1, 'L', NBIG, a_big, NBIG, b_big, NBIG, info)

  do j = 1, NBIG
    do i = 1, NBIG
      a_packed((j-1)*NBIG + i) = a_big((j-1)*NBIG + i)
    end do
  end do
  call begin_test('blocked_itype1_lower_70')
  call print_int('info', info)
  call print_array('A', a_packed, NBIG*NBIG)
  call end_test()

  ! ============================================================
  ! Test 10: Blocked path - ITYPE=2, UPLO='U', N=70
  b_big = 0.0d0
  do j = 1, NBIG
    do i = 1, NBIG
      if (i == j) then
        b_big((j-1)*NBIG + i) = dble(NBIG) + 1.0d0
      else if (abs(i - j) == 1) then
        b_big((j-1)*NBIG + i) = 0.5d0
      end if
    end do
  end do
  call dpotrf('U', NBIG, b_big, NBIG, info)

  a_big = 0.0d0
  do j = 1, NBIG
    do i = 1, j
      if (i == j) then
        a_big((j-1)*NBIG + i) = dble(2*NBIG) + dble(i)
      else
        a_big((j-1)*NBIG + i) = 0.1d0 * dble(i + j)
      end if
    end do
  end do
  call dsygst(2, 'U', NBIG, a_big, NBIG, b_big, NBIG, info)

  do j = 1, NBIG
    do i = 1, NBIG
      a_packed((j-1)*NBIG + i) = a_big((j-1)*NBIG + i)
    end do
  end do
  call begin_test('blocked_itype2_upper_70')
  call print_int('info', info)
  call print_array('A', a_packed, NBIG*NBIG)
  call end_test()

  ! ============================================================
  ! Test 11: Blocked path - ITYPE=2, UPLO='L', N=70
  b_big = 0.0d0
  do j = 1, NBIG
    do i = 1, NBIG
      if (i == j) then
        b_big((j-1)*NBIG + i) = dble(NBIG) + 1.0d0
      else if (abs(i - j) == 1) then
        b_big((j-1)*NBIG + i) = 0.5d0
      end if
    end do
  end do
  call dpotrf('L', NBIG, b_big, NBIG, info)

  a_big = 0.0d0
  do j = 1, NBIG
    do i = j, NBIG
      if (i == j) then
        a_big((j-1)*NBIG + i) = dble(2*NBIG) + dble(i)
      else
        a_big((j-1)*NBIG + i) = 0.1d0 * dble(i + j)
      end if
    end do
  end do
  call dsygst(2, 'L', NBIG, a_big, NBIG, b_big, NBIG, info)

  do j = 1, NBIG
    do i = 1, NBIG
      a_packed((j-1)*NBIG + i) = a_big((j-1)*NBIG + i)
    end do
  end do
  call begin_test('blocked_itype2_lower_70')
  call print_int('info', info)
  call print_array('A', a_packed, NBIG*NBIG)
  call end_test()

  ! ============================================================
  ! Test 12: Blocked path - ITYPE=3, UPLO='U', N=70
  b_big = 0.0d0
  do j = 1, NBIG
    do i = 1, NBIG
      if (i == j) then
        b_big((j-1)*NBIG + i) = dble(NBIG) + 1.0d0
      else if (abs(i - j) == 1) then
        b_big((j-1)*NBIG + i) = 0.5d0
      end if
    end do
  end do
  call dpotrf('U', NBIG, b_big, NBIG, info)

  a_big = 0.0d0
  do j = 1, NBIG
    do i = 1, j
      if (i == j) then
        a_big((j-1)*NBIG + i) = dble(2*NBIG) + dble(i)
      else
        a_big((j-1)*NBIG + i) = 0.1d0 * dble(i + j)
      end if
    end do
  end do
  call dsygst(3, 'U', NBIG, a_big, NBIG, b_big, NBIG, info)

  do j = 1, NBIG
    do i = 1, NBIG
      a_packed((j-1)*NBIG + i) = a_big((j-1)*NBIG + i)
    end do
  end do
  call begin_test('blocked_itype3_upper_70')
  call print_int('info', info)
  call print_array('A', a_packed, NBIG*NBIG)
  call end_test()

  ! ============================================================
  ! Test 13: Blocked path - ITYPE=3, UPLO='L', N=70
  b_big = 0.0d0
  do j = 1, NBIG
    do i = 1, NBIG
      if (i == j) then
        b_big((j-1)*NBIG + i) = dble(NBIG) + 1.0d0
      else if (abs(i - j) == 1) then
        b_big((j-1)*NBIG + i) = 0.5d0
      end if
    end do
  end do
  call dpotrf('L', NBIG, b_big, NBIG, info)

  a_big = 0.0d0
  do j = 1, NBIG
    do i = j, NBIG
      if (i == j) then
        a_big((j-1)*NBIG + i) = dble(2*NBIG) + dble(i)
      else
        a_big((j-1)*NBIG + i) = 0.1d0 * dble(i + j)
      end if
    end do
  end do
  call dsygst(3, 'L', NBIG, a_big, NBIG, b_big, NBIG, info)

  do j = 1, NBIG
    do i = 1, NBIG
      a_packed((j-1)*NBIG + i) = a_big((j-1)*NBIG + i)
    end do
  end do
  call begin_test('blocked_itype3_lower_70')
  call print_int('info', info)
  call print_array('A', a_packed, NBIG*NBIG)
  call end_test()

end program
