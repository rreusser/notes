program test_zhegst
  use test_utils
  implicit none
  complex*16 :: a(16), b(16)
  double precision :: a_r(32), b_r(32)
  equivalence (a, a_r)
  equivalence (b, b_r)
  integer :: info

  ! Parameters for blocked test
  integer, parameter :: NBIG = 70
  complex*16 :: a_big(NBIG*NBIG), b_big(NBIG*NBIG)
  double precision :: a_big_r(2*NBIG*NBIG), b_big_r(2*NBIG*NBIG)
  equivalence (a_big, a_big_r)
  equivalence (b_big, b_big_r)
  double precision :: a_packed_r(2*NBIG*NBIG)
  integer :: i, j

  ! Use same matrices as zhegs2 tests
  ! B = [4 1+i 0; 1-i 5 2-i; 0 2+i 6] (Hermitian positive definite)
  ! A = [10 2+i 1-2i; 2-i 8 3+i; 1+2i 3-i 7] (Hermitian)

  ! Test 1: ITYPE=1, UPLO='U', N=3
  b = (0.0d0, 0.0d0)
  b(1) = (4.0d0, 0.0d0); b(2) = (1.0d0, -1.0d0)
  b(4) = (1.0d0, 1.0d0); b(5) = (5.0d0, 0.0d0); b(6) = (2.0d0, 1.0d0)
  b(8) = (2.0d0, -1.0d0); b(9) = (6.0d0, 0.0d0)
  call zpotrf('U', 3, b, 3, info)

  a = (0.0d0, 0.0d0)
  a(1) = (10.0d0, 0.0d0); a(4) = (2.0d0, 1.0d0); a(5) = (8.0d0, 0.0d0)
  a(7) = (1.0d0, -2.0d0); a(8) = (3.0d0, 1.0d0); a(9) = (7.0d0, 0.0d0)

  call zhegst(1, 'U', 3, a, 3, b, 3, info)
  call begin_test('itype1_upper')
  call print_int('info', info)
  call print_array('A', a_r, 18)
  call end_test()

  ! Test 2: ITYPE=1, UPLO='L', N=3
  b = (0.0d0, 0.0d0)
  b(1) = (4.0d0, 0.0d0); b(2) = (1.0d0, -1.0d0)
  b(4) = (1.0d0, 1.0d0); b(5) = (5.0d0, 0.0d0); b(6) = (2.0d0, 1.0d0)
  b(8) = (2.0d0, -1.0d0); b(9) = (6.0d0, 0.0d0)
  call zpotrf('L', 3, b, 3, info)

  a = (0.0d0, 0.0d0)
  a(1) = (10.0d0, 0.0d0); a(2) = (2.0d0, -1.0d0); a(3) = (1.0d0, 2.0d0)
  a(5) = (8.0d0, 0.0d0); a(6) = (3.0d0, -1.0d0); a(9) = (7.0d0, 0.0d0)

  call zhegst(1, 'L', 3, a, 3, b, 3, info)
  call begin_test('itype1_lower')
  call print_int('info', info)
  call print_array('A', a_r, 18)
  call end_test()

  ! Test 3: ITYPE=2, UPLO='U', N=3
  b = (0.0d0, 0.0d0)
  b(1) = (4.0d0, 0.0d0); b(2) = (1.0d0, -1.0d0)
  b(4) = (1.0d0, 1.0d0); b(5) = (5.0d0, 0.0d0); b(6) = (2.0d0, 1.0d0)
  b(8) = (2.0d0, -1.0d0); b(9) = (6.0d0, 0.0d0)
  call zpotrf('U', 3, b, 3, info)

  a = (0.0d0, 0.0d0)
  a(1) = (10.0d0, 0.0d0); a(4) = (2.0d0, 1.0d0); a(5) = (8.0d0, 0.0d0)
  a(7) = (1.0d0, -2.0d0); a(8) = (3.0d0, 1.0d0); a(9) = (7.0d0, 0.0d0)

  call zhegst(2, 'U', 3, a, 3, b, 3, info)
  call begin_test('itype2_upper')
  call print_int('info', info)
  call print_array('A', a_r, 18)
  call end_test()

  ! Test 4: ITYPE=2, UPLO='L', N=3
  b = (0.0d0, 0.0d0)
  b(1) = (4.0d0, 0.0d0); b(2) = (1.0d0, -1.0d0)
  b(4) = (1.0d0, 1.0d0); b(5) = (5.0d0, 0.0d0); b(6) = (2.0d0, 1.0d0)
  b(8) = (2.0d0, -1.0d0); b(9) = (6.0d0, 0.0d0)
  call zpotrf('L', 3, b, 3, info)

  a = (0.0d0, 0.0d0)
  a(1) = (10.0d0, 0.0d0); a(2) = (2.0d0, -1.0d0); a(3) = (1.0d0, 2.0d0)
  a(5) = (8.0d0, 0.0d0); a(6) = (3.0d0, -1.0d0); a(9) = (7.0d0, 0.0d0)

  call zhegst(2, 'L', 3, a, 3, b, 3, info)
  call begin_test('itype2_lower')
  call print_int('info', info)
  call print_array('A', a_r, 18)
  call end_test()

  ! Test 5: ITYPE=3, UPLO='U', N=3
  b = (0.0d0, 0.0d0)
  b(1) = (4.0d0, 0.0d0); b(2) = (1.0d0, -1.0d0)
  b(4) = (1.0d0, 1.0d0); b(5) = (5.0d0, 0.0d0); b(6) = (2.0d0, 1.0d0)
  b(8) = (2.0d0, -1.0d0); b(9) = (6.0d0, 0.0d0)
  call zpotrf('U', 3, b, 3, info)

  a = (0.0d0, 0.0d0)
  a(1) = (10.0d0, 0.0d0); a(4) = (2.0d0, 1.0d0); a(5) = (8.0d0, 0.0d0)
  a(7) = (1.0d0, -2.0d0); a(8) = (3.0d0, 1.0d0); a(9) = (7.0d0, 0.0d0)

  call zhegst(3, 'U', 3, a, 3, b, 3, info)
  call begin_test('itype3_upper')
  call print_int('info', info)
  call print_array('A', a_r, 18)
  call end_test()

  ! Test 6: ITYPE=3, UPLO='L', N=3
  b = (0.0d0, 0.0d0)
  b(1) = (4.0d0, 0.0d0); b(2) = (1.0d0, -1.0d0)
  b(4) = (1.0d0, 1.0d0); b(5) = (5.0d0, 0.0d0); b(6) = (2.0d0, 1.0d0)
  b(8) = (2.0d0, -1.0d0); b(9) = (6.0d0, 0.0d0)
  call zpotrf('L', 3, b, 3, info)

  a = (0.0d0, 0.0d0)
  a(1) = (10.0d0, 0.0d0); a(2) = (2.0d0, -1.0d0); a(3) = (1.0d0, 2.0d0)
  a(5) = (8.0d0, 0.0d0); a(6) = (3.0d0, -1.0d0); a(9) = (7.0d0, 0.0d0)

  call zhegst(3, 'L', 3, a, 3, b, 3, info)
  call begin_test('itype3_lower')
  call print_int('info', info)
  call print_array('A', a_r, 18)
  call end_test()

  ! Test 7: N=0 quick return
  call zhegst(1, 'U', 0, a, 1, b, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 8: N=1
  a(1) = (9.0d0, 0.0d0)
  b(1) = (3.0d0, 0.0d0)
  call zhegst(1, 'U', 1, a, 1, b, 1, info)
  call begin_test('n_one')
  call print_int('info', info)
  call print_array('A', a_r, 2)
  call end_test()

  ! ============================================================
  ! Test 9: Blocked path - ITYPE=1, UPLO='U', N=70
  ! Build diagonally dominant HPD B and Hermitian A
  b_big = (0.0d0, 0.0d0)
  do j = 1, NBIG
    do i = 1, NBIG
      if (i == j) then
        b_big((j-1)*NBIG + i) = dcmplx(dble(NBIG) + 1.0d0, 0.0d0)
      else if (i == j - 1) then
        b_big((j-1)*NBIG + i) = dcmplx(0.5d0, 0.1d0)
      else if (i == j + 1) then
        b_big((j-1)*NBIG + i) = dcmplx(0.5d0, -0.1d0)
      end if
    end do
  end do
  call zpotrf('U', NBIG, b_big, NBIG, info)

  a_big = (0.0d0, 0.0d0)
  do j = 1, NBIG
    do i = 1, j
      if (i == j) then
        a_big((j-1)*NBIG + i) = dcmplx(dble(2*NBIG) + dble(i), 0.0d0)
      else
        a_big((j-1)*NBIG + i) = dcmplx(0.1d0 * dble(i + j), 0.05d0 * dble(j - i))
      end if
    end do
  end do
  call zhegst(1, 'U', NBIG, a_big, NBIG, b_big, NBIG, info)

  ! Pack: copy interleaved re/im
  do j = 1, NBIG
    do i = 1, NBIG
      a_packed_r(((j-1)*NBIG + (i-1))*2 + 1) = dble(a_big((j-1)*NBIG + i))
      a_packed_r(((j-1)*NBIG + (i-1))*2 + 2) = dimag(a_big((j-1)*NBIG + i))
    end do
  end do
  call begin_test('blocked_itype1_upper_70')
  call print_int('info', info)
  call print_array('A', a_packed_r, 2*NBIG*NBIG)
  call end_test()

  ! ============================================================
  ! Test 10: Blocked path - ITYPE=1, UPLO='L', N=70
  b_big = (0.0d0, 0.0d0)
  do j = 1, NBIG
    do i = 1, NBIG
      if (i == j) then
        b_big((j-1)*NBIG + i) = dcmplx(dble(NBIG) + 1.0d0, 0.0d0)
      else if (i == j - 1) then
        b_big((j-1)*NBIG + i) = dcmplx(0.5d0, 0.1d0)
      else if (i == j + 1) then
        b_big((j-1)*NBIG + i) = dcmplx(0.5d0, -0.1d0)
      end if
    end do
  end do
  call zpotrf('L', NBIG, b_big, NBIG, info)

  a_big = (0.0d0, 0.0d0)
  do j = 1, NBIG
    do i = j, NBIG
      if (i == j) then
        a_big((j-1)*NBIG + i) = dcmplx(dble(2*NBIG) + dble(i), 0.0d0)
      else
        a_big((j-1)*NBIG + i) = dcmplx(0.1d0 * dble(i + j), -0.05d0 * dble(i - j))
      end if
    end do
  end do
  call zhegst(1, 'L', NBIG, a_big, NBIG, b_big, NBIG, info)

  do j = 1, NBIG
    do i = 1, NBIG
      a_packed_r(((j-1)*NBIG + (i-1))*2 + 1) = dble(a_big((j-1)*NBIG + i))
      a_packed_r(((j-1)*NBIG + (i-1))*2 + 2) = dimag(a_big((j-1)*NBIG + i))
    end do
  end do
  call begin_test('blocked_itype1_lower_70')
  call print_int('info', info)
  call print_array('A', a_packed_r, 2*NBIG*NBIG)
  call end_test()

  ! ============================================================
  ! Test 11: Blocked path - ITYPE=2, UPLO='U', N=70
  b_big = (0.0d0, 0.0d0)
  do j = 1, NBIG
    do i = 1, NBIG
      if (i == j) then
        b_big((j-1)*NBIG + i) = dcmplx(dble(NBIG) + 1.0d0, 0.0d0)
      else if (i == j - 1) then
        b_big((j-1)*NBIG + i) = dcmplx(0.5d0, 0.1d0)
      else if (i == j + 1) then
        b_big((j-1)*NBIG + i) = dcmplx(0.5d0, -0.1d0)
      end if
    end do
  end do
  call zpotrf('U', NBIG, b_big, NBIG, info)

  a_big = (0.0d0, 0.0d0)
  do j = 1, NBIG
    do i = 1, j
      if (i == j) then
        a_big((j-1)*NBIG + i) = dcmplx(dble(2*NBIG) + dble(i), 0.0d0)
      else
        a_big((j-1)*NBIG + i) = dcmplx(0.1d0 * dble(i + j), 0.05d0 * dble(j - i))
      end if
    end do
  end do
  call zhegst(2, 'U', NBIG, a_big, NBIG, b_big, NBIG, info)

  do j = 1, NBIG
    do i = 1, NBIG
      a_packed_r(((j-1)*NBIG + (i-1))*2 + 1) = dble(a_big((j-1)*NBIG + i))
      a_packed_r(((j-1)*NBIG + (i-1))*2 + 2) = dimag(a_big((j-1)*NBIG + i))
    end do
  end do
  call begin_test('blocked_itype2_upper_70')
  call print_int('info', info)
  call print_array('A', a_packed_r, 2*NBIG*NBIG)
  call end_test()

  ! ============================================================
  ! Test 12: Blocked path - ITYPE=2, UPLO='L', N=70
  b_big = (0.0d0, 0.0d0)
  do j = 1, NBIG
    do i = 1, NBIG
      if (i == j) then
        b_big((j-1)*NBIG + i) = dcmplx(dble(NBIG) + 1.0d0, 0.0d0)
      else if (i == j - 1) then
        b_big((j-1)*NBIG + i) = dcmplx(0.5d0, 0.1d0)
      else if (i == j + 1) then
        b_big((j-1)*NBIG + i) = dcmplx(0.5d0, -0.1d0)
      end if
    end do
  end do
  call zpotrf('L', NBIG, b_big, NBIG, info)

  a_big = (0.0d0, 0.0d0)
  do j = 1, NBIG
    do i = j, NBIG
      if (i == j) then
        a_big((j-1)*NBIG + i) = dcmplx(dble(2*NBIG) + dble(i), 0.0d0)
      else
        a_big((j-1)*NBIG + i) = dcmplx(0.1d0 * dble(i + j), -0.05d0 * dble(i - j))
      end if
    end do
  end do
  call zhegst(2, 'L', NBIG, a_big, NBIG, b_big, NBIG, info)

  do j = 1, NBIG
    do i = 1, NBIG
      a_packed_r(((j-1)*NBIG + (i-1))*2 + 1) = dble(a_big((j-1)*NBIG + i))
      a_packed_r(((j-1)*NBIG + (i-1))*2 + 2) = dimag(a_big((j-1)*NBIG + i))
    end do
  end do
  call begin_test('blocked_itype2_lower_70')
  call print_int('info', info)
  call print_array('A', a_packed_r, 2*NBIG*NBIG)
  call end_test()

  ! ============================================================
  ! Test 13: Blocked path - ITYPE=3, UPLO='U', N=70
  b_big = (0.0d0, 0.0d0)
  do j = 1, NBIG
    do i = 1, NBIG
      if (i == j) then
        b_big((j-1)*NBIG + i) = dcmplx(dble(NBIG) + 1.0d0, 0.0d0)
      else if (i == j - 1) then
        b_big((j-1)*NBIG + i) = dcmplx(0.5d0, 0.1d0)
      else if (i == j + 1) then
        b_big((j-1)*NBIG + i) = dcmplx(0.5d0, -0.1d0)
      end if
    end do
  end do
  call zpotrf('U', NBIG, b_big, NBIG, info)

  a_big = (0.0d0, 0.0d0)
  do j = 1, NBIG
    do i = 1, j
      if (i == j) then
        a_big((j-1)*NBIG + i) = dcmplx(dble(2*NBIG) + dble(i), 0.0d0)
      else
        a_big((j-1)*NBIG + i) = dcmplx(0.1d0 * dble(i + j), 0.05d0 * dble(j - i))
      end if
    end do
  end do
  call zhegst(3, 'U', NBIG, a_big, NBIG, b_big, NBIG, info)

  do j = 1, NBIG
    do i = 1, NBIG
      a_packed_r(((j-1)*NBIG + (i-1))*2 + 1) = dble(a_big((j-1)*NBIG + i))
      a_packed_r(((j-1)*NBIG + (i-1))*2 + 2) = dimag(a_big((j-1)*NBIG + i))
    end do
  end do
  call begin_test('blocked_itype3_upper_70')
  call print_int('info', info)
  call print_array('A', a_packed_r, 2*NBIG*NBIG)
  call end_test()

  ! ============================================================
  ! Test 14: Blocked path - ITYPE=3, UPLO='L', N=70
  b_big = (0.0d0, 0.0d0)
  do j = 1, NBIG
    do i = 1, NBIG
      if (i == j) then
        b_big((j-1)*NBIG + i) = dcmplx(dble(NBIG) + 1.0d0, 0.0d0)
      else if (i == j - 1) then
        b_big((j-1)*NBIG + i) = dcmplx(0.5d0, 0.1d0)
      else if (i == j + 1) then
        b_big((j-1)*NBIG + i) = dcmplx(0.5d0, -0.1d0)
      end if
    end do
  end do
  call zpotrf('L', NBIG, b_big, NBIG, info)

  a_big = (0.0d0, 0.0d0)
  do j = 1, NBIG
    do i = j, NBIG
      if (i == j) then
        a_big((j-1)*NBIG + i) = dcmplx(dble(2*NBIG) + dble(i), 0.0d0)
      else
        a_big((j-1)*NBIG + i) = dcmplx(0.1d0 * dble(i + j), -0.05d0 * dble(i - j))
      end if
    end do
  end do
  call zhegst(3, 'L', NBIG, a_big, NBIG, b_big, NBIG, info)

  do j = 1, NBIG
    do i = 1, NBIG
      a_packed_r(((j-1)*NBIG + (i-1))*2 + 1) = dble(a_big((j-1)*NBIG + i))
      a_packed_r(((j-1)*NBIG + (i-1))*2 + 2) = dimag(a_big((j-1)*NBIG + i))
    end do
  end do
  call begin_test('blocked_itype3_lower_70')
  call print_int('info', info)
  call print_array('A', a_packed_r, 2*NBIG*NBIG)
  call end_test()

end program
