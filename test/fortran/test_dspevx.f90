program test_dspevx
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4
  integer :: N, LDZ, IL, IU, M, INFO, i, j
  double precision :: AP(NMAX*(NMAX+1)/2), W(NMAX), Z(NMAX, NMAX), WORK(8*NMAX)
  integer :: IWORK(5*NMAX), IFAIL(NMAX)
  double precision :: VL, VU, ABSTOL
  double precision :: Zflat(NMAX*NMAX)

  ! =====================================================
  ! Test 1: JOBZ='V', RANGE='A', UPLO='L', 4x4
  ! Matrix:
  !  4  1 -2  0
  !  1  3  0  1
  ! -2  0  5 -1
  !  0  1 -1  6
  ! Lower packed (column-major):
  ! col 1: 4, 1, -2, 0
  ! col 2: 3, 0, 1
  ! col 3: 5, -1
  ! col 4: 6
  ! =====================================================
  N = 4
  LDZ = NMAX
  ABSTOL = 0.0d0

  AP(1) = 4.0d0; AP(2) = 1.0d0; AP(3) = -2.0d0; AP(4) = 0.0d0
  AP(5) = 3.0d0; AP(6) = 0.0d0; AP(7) = 1.0d0
  AP(8) = 5.0d0; AP(9) = -1.0d0
  AP(10) = 6.0d0

  call DSPEVX('V', 'A', 'L', N, AP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('dspevx_4x4_V_A_L')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  do j = 1, M
    do i = 1, N
      Zflat((j-1)*N + i) = Z(i,j)
    end do
  end do
  call print_array('Z', Zflat, N*M)
  call end_test()

  ! =====================================================
  ! Test 2: JOBZ='V', RANGE='A', UPLO='U', 4x4
  ! Upper packed (column-major):
  ! col 1: 4
  ! col 2: 1, 3
  ! col 3: -2, 0, 5
  ! col 4: 0, 1, -1, 6
  ! =====================================================
  N = 4
  LDZ = NMAX
  ABSTOL = 0.0d0

  AP(1) = 4.0d0
  AP(2) = 1.0d0; AP(3) = 3.0d0
  AP(4) = -2.0d0; AP(5) = 0.0d0; AP(6) = 5.0d0
  AP(7) = 0.0d0; AP(8) = 1.0d0; AP(9) = -1.0d0; AP(10) = 6.0d0

  call DSPEVX('V', 'A', 'U', N, AP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('dspevx_4x4_V_A_U')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  do j = 1, M
    do i = 1, N
      Zflat((j-1)*N + i) = Z(i,j)
    end do
  end do
  call print_array('Z', Zflat, N*M)
  call end_test()

  ! =====================================================
  ! Test 3: JOBZ='N', RANGE='A', UPLO='L', 4x4
  ! =====================================================
  N = 4
  LDZ = 1
  ABSTOL = 0.0d0

  AP(1) = 4.0d0; AP(2) = 1.0d0; AP(3) = -2.0d0; AP(4) = 0.0d0
  AP(5) = 3.0d0; AP(6) = 0.0d0; AP(7) = 1.0d0
  AP(8) = 5.0d0; AP(9) = -1.0d0
  AP(10) = 6.0d0

  call DSPEVX('N', 'A', 'L', N, AP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('dspevx_4x4_N_A_L')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  call end_test()

  ! =====================================================
  ! Test 4: JOBZ='V', RANGE='V' (value range), UPLO='L'
  ! Select eigenvalues in (2.5, 5.5]
  ! =====================================================
  N = 4
  LDZ = NMAX
  ABSTOL = 0.0d0
  VL = 2.5d0
  VU = 5.5d0

  AP(1) = 4.0d0; AP(2) = 1.0d0; AP(3) = -2.0d0; AP(4) = 0.0d0
  AP(5) = 3.0d0; AP(6) = 0.0d0; AP(7) = 1.0d0
  AP(8) = 5.0d0; AP(9) = -1.0d0
  AP(10) = 6.0d0

  call DSPEVX('V', 'V', 'L', N, AP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('dspevx_4x4_V_V_L')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  do j = 1, M
    do i = 1, N
      Zflat((j-1)*N + i) = Z(i,j)
    end do
  end do
  call print_array('Z', Zflat, N*M)
  call end_test()

  ! =====================================================
  ! Test 5: JOBZ='V', RANGE='I' (index range), UPLO='L'
  ! Select eigenvalues 2 through 3
  ! =====================================================
  N = 4
  LDZ = NMAX
  ABSTOL = 0.0d0
  IL = 2
  IU = 3

  AP(1) = 4.0d0; AP(2) = 1.0d0; AP(3) = -2.0d0; AP(4) = 0.0d0
  AP(5) = 3.0d0; AP(6) = 0.0d0; AP(7) = 1.0d0
  AP(8) = 5.0d0; AP(9) = -1.0d0
  AP(10) = 6.0d0

  call DSPEVX('V', 'I', 'L', N, AP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('dspevx_4x4_V_I_L')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  do j = 1, M
    do i = 1, N
      Zflat((j-1)*N + i) = Z(i,j)
    end do
  end do
  call print_array('Z', Zflat, N*M)
  call end_test()

  ! =====================================================
  ! Test 6: JOBZ='N', RANGE='V', UPLO='U'
  ! =====================================================
  N = 4
  LDZ = 1
  ABSTOL = 0.0d0
  VL = 0.0d0
  VU = 4.0d0

  AP(1) = 4.0d0
  AP(2) = 1.0d0; AP(3) = 3.0d0
  AP(4) = -2.0d0; AP(5) = 0.0d0; AP(6) = 5.0d0
  AP(7) = 0.0d0; AP(8) = 1.0d0; AP(9) = -1.0d0; AP(10) = 6.0d0

  call DSPEVX('N', 'V', 'U', N, AP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('dspevx_4x4_N_V_U')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  call end_test()

  ! =====================================================
  ! Test 7: N=1, JOBZ='V', RANGE='A'
  ! =====================================================
  N = 1
  LDZ = NMAX
  ABSTOL = 0.0d0

  AP(1) = 7.5d0

  call DSPEVX('V', 'A', 'L', N, AP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('dspevx_1x1_V_A')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  call print_array('Z', Z(1,1), 1)
  call end_test()

  ! =====================================================
  ! Test 8: N=0
  ! =====================================================
  N = 0
  LDZ = 1

  call DSPEVX('V', 'A', 'L', N, AP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('dspevx_0x0')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! =====================================================
  ! Test 9: N=1, JOBZ='V', RANGE='V' (value excludes the eigenvalue)
  ! =====================================================
  N = 1
  LDZ = NMAX
  VL = 0.0d0
  VU = 5.0d0
  AP(1) = 7.5d0

  call DSPEVX('V', 'V', 'L', N, AP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('dspevx_1x1_V_V_excluded')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! =====================================================
  ! Test 10: N=1, JOBZ='V', RANGE='V' (value includes the eigenvalue)
  ! =====================================================
  N = 1
  LDZ = NMAX
  VL = 5.0d0
  VU = 10.0d0
  AP(1) = 7.5d0

  call DSPEVX('V', 'V', 'L', N, AP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('dspevx_1x1_V_V_included')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  call print_array('Z', Z(1,1), 1)
  call end_test()

  ! =====================================================
  ! Test 11: N=1, JOBZ='N', RANGE='I'
  ! =====================================================
  N = 1
  LDZ = 1
  IL = 1
  IU = 1
  AP(1) = 3.0d0

  call DSPEVX('N', 'I', 'U', N, AP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('dspevx_1x1_N_I')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  call end_test()

  ! =====================================================
  ! Test 12: JOBZ='N', RANGE='I', UPLO='U', IL=1,IU=N (fast path)
  ! =====================================================
  N = 4
  LDZ = 1
  IL = 1
  IU = 4
  ABSTOL = 0.0d0

  AP(1) = 4.0d0
  AP(2) = 1.0d0; AP(3) = 3.0d0
  AP(4) = -2.0d0; AP(5) = 0.0d0; AP(6) = 5.0d0
  AP(7) = 0.0d0; AP(8) = 1.0d0; AP(9) = -1.0d0; AP(10) = 6.0d0

  call DSPEVX('N', 'I', 'U', N, AP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('dspevx_4x4_N_I_U_fast')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  call end_test()

  ! =====================================================
  ! Test 13: JOBZ='V', RANGE='I', UPLO='U', IL=1,IU=N (fast path with vectors)
  ! =====================================================
  N = 4
  LDZ = NMAX
  IL = 1
  IU = 4
  ABSTOL = 0.0d0

  AP(1) = 4.0d0
  AP(2) = 1.0d0; AP(3) = 3.0d0
  AP(4) = -2.0d0; AP(5) = 0.0d0; AP(6) = 5.0d0
  AP(7) = 0.0d0; AP(8) = 1.0d0; AP(9) = -1.0d0; AP(10) = 6.0d0

  call DSPEVX('V', 'I', 'U', N, AP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('dspevx_4x4_V_I_U_fast')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  do j = 1, M
    do i = 1, N
      Zflat((j-1)*N + i) = Z(i,j)
    end do
  end do
  call print_array('Z', Zflat, N*M)
  call end_test()

end program test_dspevx
