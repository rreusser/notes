program test_zhetrd_he2hb
  use test_utils
  implicit none

  integer :: INFO, LWORK, N, KD, i, j
  complex*16 :: A8(8,8), AB8(4,8), TAU8(8), WORK(2000)
  complex*16 :: A2(2,2), AB2(2,2), TAU2(2)
  complex*16 :: A12(12,12), AB12(5,12), TAU12(12)
  double precision :: A8_r(128), AB8_r(64), TAU8_r(16)
  double precision :: A2_r(8), AB2_r(8), TAU2_r(4)
  double precision :: A12_r(288), AB12_r(120), TAU12_r(24)
  equivalence (A8, A8_r)
  equivalence (AB8, AB8_r)
  equivalence (TAU8, TAU8_r)
  equivalence (A2, A2_r)
  equivalence (AB2, AB2_r)
  equivalence (TAU2, TAU2_r)
  equivalence (A12, A12_r)
  equivalence (AB12, AB12_r)
  equivalence (TAU12, TAU12_r)

  LWORK = 2000

  ! ------------------------------------------------------------------
  ! Test 1: UPLO='U', N=8, KD=3 (main blocked path)
  ! Build a diagonally dominant Hermitian matrix
  ! ------------------------------------------------------------------
  N = 8
  KD = 3
  do j = 1, N
    do i = 1, N
      if (i == j) then
        A8(i,j) = dcmplx(dble(20 + i), 0.0d0)
      else if (i < j) then
        A8(i,j) = dcmplx(1.0d0/dble(j-i+1), 0.5d0/dble(j-i+1))
        A8(j,i) = dcmplx(1.0d0/dble(j-i+1), -0.5d0/dble(j-i+1))
      end if
    end do
  end do
  AB8 = dcmplx(0.0d0, 0.0d0)
  TAU8 = dcmplx(0.0d0, 0.0d0)
  call ZHETRD_HE2HB('U', N, KD, A8, 8, AB8, KD+1, TAU8, WORK, LWORK, INFO)
  call begin_test('upper_n8_kd3')
  call print_int('info', INFO)
  call print_array('A', A8_r, 2*N*N)
  call print_array('AB', AB8_r, 2*(KD+1)*N)
  call print_array('TAU', TAU8_r, 2*(N-KD))
  call end_test()

  ! ------------------------------------------------------------------
  ! Test 2: UPLO='L', N=8, KD=3 (main blocked path)
  ! ------------------------------------------------------------------
  N = 8
  KD = 3
  do j = 1, N
    do i = 1, N
      if (i == j) then
        A8(i,j) = dcmplx(dble(20 + i), 0.0d0)
      else if (i < j) then
        A8(i,j) = dcmplx(1.0d0/dble(j-i+1), 0.5d0/dble(j-i+1))
        A8(j,i) = dcmplx(1.0d0/dble(j-i+1), -0.5d0/dble(j-i+1))
      end if
    end do
  end do
  AB8 = dcmplx(0.0d0, 0.0d0)
  TAU8 = dcmplx(0.0d0, 0.0d0)
  call ZHETRD_HE2HB('L', N, KD, A8, 8, AB8, KD+1, TAU8, WORK, LWORK, INFO)
  call begin_test('lower_n8_kd3')
  call print_int('info', INFO)
  call print_array('A', A8_r, 2*N*N)
  call print_array('AB', AB8_r, 2*(KD+1)*N)
  call print_array('TAU', TAU8_r, 2*(N-KD))
  call end_test()

  ! ------------------------------------------------------------------
  ! Test 3: Quick return path UPLO='U', N=2, KD=1 (N <= KD+1)
  ! Just copies A to AB.
  ! ------------------------------------------------------------------
  N = 2
  KD = 1
  A2(1,1) = dcmplx(4.0d0, 0.0d0)
  A2(1,2) = dcmplx(1.0d0, 1.0d0)
  A2(2,1) = dcmplx(1.0d0, -1.0d0)
  A2(2,2) = dcmplx(5.0d0, 0.0d0)
  AB2 = dcmplx(0.0d0, 0.0d0)
  TAU2 = dcmplx(0.0d0, 0.0d0)
  call ZHETRD_HE2HB('U', N, KD, A2, 2, AB2, KD+1, TAU2, WORK, LWORK, INFO)
  call begin_test('upper_n2_kd1_quick')
  call print_int('info', INFO)
  call print_array('AB', AB2_r, 2*(KD+1)*N)
  call end_test()

  ! ------------------------------------------------------------------
  ! Test 4: Quick return path UPLO='L', N=2, KD=1 (N <= KD+1)
  ! ------------------------------------------------------------------
  N = 2
  KD = 1
  A2(1,1) = dcmplx(4.0d0, 0.0d0)
  A2(1,2) = dcmplx(1.0d0, 1.0d0)
  A2(2,1) = dcmplx(1.0d0, -1.0d0)
  A2(2,2) = dcmplx(5.0d0, 0.0d0)
  AB2 = dcmplx(0.0d0, 0.0d0)
  TAU2 = dcmplx(0.0d0, 0.0d0)
  call ZHETRD_HE2HB('L', N, KD, A2, 2, AB2, KD+1, TAU2, WORK, LWORK, INFO)
  call begin_test('lower_n2_kd1_quick')
  call print_int('info', INFO)
  call print_array('AB', AB2_r, 2*(KD+1)*N)
  call end_test()

  ! ------------------------------------------------------------------
  ! Test 5: N=0 (quick return, no work)
  ! ------------------------------------------------------------------
  call ZHETRD_HE2HB('U', 0, 1, A2, 1, AB2, 2, TAU2, WORK, LWORK, INFO)
  call begin_test('n_zero')
  call print_int('info', INFO)
  call end_test()

  ! ------------------------------------------------------------------
  ! Test 6: UPLO='U', N=12, KD=4 (multiple panels)
  ! ------------------------------------------------------------------
  N = 12
  KD = 4
  do j = 1, N
    do i = 1, N
      if (i == j) then
        A12(i,j) = dcmplx(dble(30 + i), 0.0d0)
      else if (i < j) then
        A12(i,j) = dcmplx(1.0d0/dble(j-i+1), 0.3d0/dble(j-i+1))
        A12(j,i) = dcmplx(1.0d0/dble(j-i+1), -0.3d0/dble(j-i+1))
      end if
    end do
  end do
  AB12 = dcmplx(0.0d0, 0.0d0)
  TAU12 = dcmplx(0.0d0, 0.0d0)
  call ZHETRD_HE2HB('U', N, KD, A12, 12, AB12, KD+1, TAU12, WORK, LWORK, INFO)
  call begin_test('upper_n12_kd4')
  call print_int('info', INFO)
  call print_array('A', A12_r, 2*N*N)
  call print_array('AB', AB12_r, 2*(KD+1)*N)
  call print_array('TAU', TAU12_r, 2*(N-KD))
  call end_test()

  ! ------------------------------------------------------------------
  ! Test 7: UPLO='L', N=12, KD=4 (multiple panels)
  ! ------------------------------------------------------------------
  N = 12
  KD = 4
  do j = 1, N
    do i = 1, N
      if (i == j) then
        A12(i,j) = dcmplx(dble(30 + i), 0.0d0)
      else if (i < j) then
        A12(i,j) = dcmplx(1.0d0/dble(j-i+1), 0.3d0/dble(j-i+1))
        A12(j,i) = dcmplx(1.0d0/dble(j-i+1), -0.3d0/dble(j-i+1))
      end if
    end do
  end do
  AB12 = dcmplx(0.0d0, 0.0d0)
  TAU12 = dcmplx(0.0d0, 0.0d0)
  call ZHETRD_HE2HB('L', N, KD, A12, 12, AB12, KD+1, TAU12, WORK, LWORK, INFO)
  call begin_test('lower_n12_kd4')
  call print_int('info', INFO)
  call print_array('A', A12_r, 2*N*N)
  call print_array('AB', AB12_r, 2*(KD+1)*N)
  call print_array('TAU', TAU12_r, 2*(N-KD))
  call end_test()

end program
