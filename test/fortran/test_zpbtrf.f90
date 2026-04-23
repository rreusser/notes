program test_zpbtrf
  use test_utils
  implicit none
  ! For the blocked path (NB=32), need kd>32 and matrices of size N>kd+NB
  ! For testing, use small cases that fall through to zpbtf2,
  ! plus a larger case with kd=33 to exercise the blocked path.

  ! Small cases (unblocked path)
  complex*16 :: AB(100)
  double precision :: AB_r(200)
  equivalence (AB, AB_r)
  integer :: info

  ! Large case for blocked path
  integer, parameter :: N_LARGE = 100, KD_LARGE = 33
  integer, parameter :: LDAB_LARGE = KD_LARGE + 1
  complex*16 :: ABL(LDAB_LARGE * N_LARGE)
  double precision :: ABL_r(2 * LDAB_LARGE * N_LARGE)
  equivalence (ABL, ABL_r)
  integer :: i, j, kk

  ! Test 1: upper, N=3, KD=1 (unblocked path)
  AB = (0.0d0, 0.0d0)
  AB(2) = (4.0d0, 0.0d0)
  AB(3) = (1.0d0, 1.0d0)
  AB(4) = (5.0d0, 0.0d0)
  AB(5) = (2.0d0, -1.0d0)
  AB(6) = (6.0d0, 0.0d0)
  call zpbtrf('U', 3, 1, AB, 2, info)
  call begin_test('upper_3x3_kd1')
  call print_int('info', info)
  call print_array('AB', AB_r, 12)
  call end_test()

  ! Test 2: lower, N=3, KD=1 (unblocked path)
  AB = (0.0d0, 0.0d0)
  AB(1) = (4.0d0, 0.0d0)
  AB(2) = (1.0d0, -1.0d0)
  AB(3) = (5.0d0, 0.0d0)
  AB(4) = (2.0d0, 1.0d0)
  AB(5) = (6.0d0, 0.0d0)
  call zpbtrf('L', 3, 1, AB, 2, info)
  call begin_test('lower_3x3_kd1')
  call print_int('info', info)
  call print_array('AB', AB_r, 12)
  call end_test()

  ! Test 3: N=0
  call zpbtrf('U', 0, 1, AB, 2, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 4: N=1
  AB(1) = (9.0d0, 0.0d0)
  call zpbtrf('U', 1, 0, AB, 1, info)
  call begin_test('n_one')
  call print_int('info', info)
  call print_array('AB', AB_r, 2)
  call end_test()

  ! Test 5: not HPD
  AB = (0.0d0, 0.0d0)
  AB(2) = (1.0d0, 0.0d0)
  AB(3) = (2.0d0, 1.0d0)
  AB(4) = (1.0d0, 0.0d0)
  call zpbtrf('U', 2, 1, AB, 2, info)
  call begin_test('not_hpd')
  call print_int('info', info)
  call end_test()

  ! Test 6: upper, blocked path (KD=33, N=100)
  ! Build a diagonally dominant HPD banded matrix
  ABL = (0.0d0, 0.0d0)
  do j = 1, N_LARGE
    ! Diagonal: ABL(KD_LARGE+1, j)
    ABL((j-1)*LDAB_LARGE + KD_LARGE + 1) = dcmplx(100.0d0, 0.0d0)
    ! Superdiagonals
    do kk = 1, KD_LARGE
      if (j + kk <= N_LARGE) then
        ABL((j+kk-1)*LDAB_LARGE + KD_LARGE + 1 - kk) = &
          dcmplx(0.5d0 / dble(kk), 0.1d0 / dble(kk))
      end if
    end do
  end do
  call zpbtrf('U', N_LARGE, KD_LARGE, ABL, LDAB_LARGE, info)
  call begin_test('upper_blocked')
  call print_int('info', info)
  ! Print only the first 8 complex elements to keep fixture small
  call print_array('AB', ABL_r, 16)
  call end_test()

  ! Test 7: lower, blocked path (KD=33, N=100)
  ABL = (0.0d0, 0.0d0)
  do j = 1, N_LARGE
    ! Diagonal: ABL(1, j)
    ABL((j-1)*LDAB_LARGE + 1) = dcmplx(100.0d0, 0.0d0)
    ! Subdiagonals
    do kk = 1, KD_LARGE
      if (j + kk <= N_LARGE) then
        ABL((j-1)*LDAB_LARGE + 1 + kk) = &
          dcmplx(0.5d0 / dble(kk), -0.1d0 / dble(kk))
      end if
    end do
  end do
  call zpbtrf('L', N_LARGE, KD_LARGE, ABL, LDAB_LARGE, info)
  call begin_test('lower_blocked')
  call print_int('info', info)
  call print_array('AB', ABL_r, 16)
  call end_test()

end program
