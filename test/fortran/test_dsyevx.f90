program test_dsyevx
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4
  integer :: N, LDA, LDZ, LWORK, IL, IU, M, INFO, i, j
  double precision :: A(NMAX, NMAX), Z(NMAX, NMAX), WORK(256)
  double precision :: W(NMAX)
  integer :: IWORK(5*NMAX), IFAIL(NMAX)
  double precision :: VL, VU, ABSTOL
  double precision :: Aflat(NMAX*NMAX), Zflat(NMAX*NMAX)

  LWORK = 256

  ! =====================================================
  ! Test 1: JOBZ='V', RANGE='A', UPLO='L', 4x4
  ! Diagonally dominant symmetric matrix
  ! =====================================================
  N = 4
  LDA = NMAX
  LDZ = NMAX
  ABSTOL = 0.0d0

  A(1,1) =  4.0d0; A(1,2) =  1.0d0; A(1,3) = -2.0d0; A(1,4) =  0.0d0
  A(2,1) =  1.0d0; A(2,2) =  3.0d0; A(2,3) =  0.0d0; A(2,4) =  1.0d0
  A(3,1) = -2.0d0; A(3,2) =  0.0d0; A(3,3) =  5.0d0; A(3,4) = -1.0d0
  A(4,1) =  0.0d0; A(4,2) =  1.0d0; A(4,3) = -1.0d0; A(4,4) =  6.0d0

  call DSYEVX('V', 'A', 'L', N, A, LDA, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, LWORK, IWORK, IFAIL, INFO)
  call begin_test('dsyevx_4x4_V_A_L')
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
  ! =====================================================
  N = 4
  LDA = NMAX
  LDZ = NMAX

  A(1,1) =  4.0d0; A(1,2) =  1.0d0; A(1,3) = -2.0d0; A(1,4) =  0.0d0
  A(2,1) =  1.0d0; A(2,2) =  3.0d0; A(2,3) =  0.0d0; A(2,4) =  1.0d0
  A(3,1) = -2.0d0; A(3,2) =  0.0d0; A(3,3) =  5.0d0; A(3,4) = -1.0d0
  A(4,1) =  0.0d0; A(4,2) =  1.0d0; A(4,3) = -1.0d0; A(4,4) =  6.0d0

  call DSYEVX('V', 'A', 'U', N, A, LDA, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, LWORK, IWORK, IFAIL, INFO)
  call begin_test('dsyevx_4x4_V_A_U')
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
  LDA = NMAX
  LDZ = NMAX

  A(1,1) =  4.0d0; A(1,2) =  1.0d0; A(1,3) = -2.0d0; A(1,4) =  0.0d0
  A(2,1) =  1.0d0; A(2,2) =  3.0d0; A(2,3) =  0.0d0; A(2,4) =  1.0d0
  A(3,1) = -2.0d0; A(3,2) =  0.0d0; A(3,3) =  5.0d0; A(3,4) = -1.0d0
  A(4,1) =  0.0d0; A(4,2) =  1.0d0; A(4,3) = -1.0d0; A(4,4) =  6.0d0

  call DSYEVX('N', 'A', 'L', N, A, LDA, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, LWORK, IWORK, IFAIL, INFO)
  call begin_test('dsyevx_4x4_N_A_L')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  call end_test()

  ! =====================================================
  ! Test 4: JOBZ='V', RANGE='V' (value range), UPLO='L'
  ! Select eigenvalues in [2.5, 5.5]
  ! =====================================================
  N = 4
  LDA = NMAX
  LDZ = NMAX
  VL = 2.5d0
  VU = 5.5d0

  A(1,1) =  4.0d0; A(1,2) =  1.0d0; A(1,3) = -2.0d0; A(1,4) =  0.0d0
  A(2,1) =  1.0d0; A(2,2) =  3.0d0; A(2,3) =  0.0d0; A(2,4) =  1.0d0
  A(3,1) = -2.0d0; A(3,2) =  0.0d0; A(3,3) =  5.0d0; A(3,4) = -1.0d0
  A(4,1) =  0.0d0; A(4,2) =  1.0d0; A(4,3) = -1.0d0; A(4,4) =  6.0d0

  call DSYEVX('V', 'V', 'L', N, A, LDA, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, LWORK, IWORK, IFAIL, INFO)
  call begin_test('dsyevx_4x4_V_V_L')
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
  LDA = NMAX
  LDZ = NMAX
  IL = 2
  IU = 3

  A(1,1) =  4.0d0; A(1,2) =  1.0d0; A(1,3) = -2.0d0; A(1,4) =  0.0d0
  A(2,1) =  1.0d0; A(2,2) =  3.0d0; A(2,3) =  0.0d0; A(2,4) =  1.0d0
  A(3,1) = -2.0d0; A(3,2) =  0.0d0; A(3,3) =  5.0d0; A(3,4) = -1.0d0
  A(4,1) =  0.0d0; A(4,2) =  1.0d0; A(4,3) = -1.0d0; A(4,4) =  6.0d0

  call DSYEVX('V', 'I', 'L', N, A, LDA, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, LWORK, IWORK, IFAIL, INFO)
  call begin_test('dsyevx_4x4_V_I_L')
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
  LDA = NMAX
  LDZ = NMAX
  VL = 0.0d0
  VU = 4.0d0

  A(1,1) =  4.0d0; A(1,2) =  1.0d0; A(1,3) = -2.0d0; A(1,4) =  0.0d0
  A(2,1) =  1.0d0; A(2,2) =  3.0d0; A(2,3) =  0.0d0; A(2,4) =  1.0d0
  A(3,1) = -2.0d0; A(3,2) =  0.0d0; A(3,3) =  5.0d0; A(3,4) = -1.0d0
  A(4,1) =  0.0d0; A(4,2) =  1.0d0; A(4,3) = -1.0d0; A(4,4) =  6.0d0

  call DSYEVX('N', 'V', 'U', N, A, LDA, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, LWORK, IWORK, IFAIL, INFO)
  call begin_test('dsyevx_4x4_N_V_U')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  call end_test()

  ! =====================================================
  ! Test 7: N=1, JOBZ='V', RANGE='A'
  ! =====================================================
  N = 1
  LDA = NMAX
  LDZ = NMAX
  ABSTOL = 0.0d0

  A(1,1) = 7.5d0

  call DSYEVX('V', 'A', 'L', N, A, LDA, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, LWORK, IWORK, IFAIL, INFO)
  call begin_test('dsyevx_1x1_V_A')
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
  LDA = NMAX
  LDZ = NMAX

  call DSYEVX('V', 'A', 'L', N, A, LDA, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, LWORK, IWORK, IFAIL, INFO)
  call begin_test('dsyevx_0x0')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! =====================================================
  ! Test 9: N=1, JOBZ='V', RANGE='V' (value excludes the eigenvalue)
  ! =====================================================
  N = 1
  LDA = NMAX
  LDZ = NMAX
  VL = 0.0d0
  VU = 5.0d0
  A(1,1) = 7.5d0

  call DSYEVX('V', 'V', 'L', N, A, LDA, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, LWORK, IWORK, IFAIL, INFO)
  call begin_test('dsyevx_1x1_V_V_excluded')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! =====================================================
  ! Test 10: N=1, JOBZ='V', RANGE='V' (value includes the eigenvalue)
  ! =====================================================
  N = 1
  LDA = NMAX
  LDZ = NMAX
  VL = 5.0d0
  VU = 10.0d0
  A(1,1) = 7.5d0

  call DSYEVX('V', 'V', 'L', N, A, LDA, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, LWORK, IWORK, IFAIL, INFO)
  call begin_test('dsyevx_1x1_V_V_included')
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
  LDA = NMAX
  LDZ = NMAX
  IL = 1
  IU = 1
  A(1,1) = 3.0d0

  call DSYEVX('N', 'I', 'U', N, A, LDA, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, LWORK, IWORK, IFAIL, INFO)
  call begin_test('dsyevx_1x1_N_I')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  call end_test()

  ! =====================================================
  ! Test 12: JOBZ='N', RANGE='I', UPLO='U', IL=IU=N (all eigenvalues)
  ! This tests the fast path (IL=1, IU=N, ABSTOL<=0)
  ! =====================================================
  N = 4
  LDA = NMAX
  LDZ = NMAX
  IL = 1
  IU = 4
  ABSTOL = 0.0d0

  A(1,1) =  4.0d0; A(1,2) =  1.0d0; A(1,3) = -2.0d0; A(1,4) =  0.0d0
  A(2,1) =  1.0d0; A(2,2) =  3.0d0; A(2,3) =  0.0d0; A(2,4) =  1.0d0
  A(3,1) = -2.0d0; A(3,2) =  0.0d0; A(3,3) =  5.0d0; A(3,4) = -1.0d0
  A(4,1) =  0.0d0; A(4,2) =  1.0d0; A(4,3) = -1.0d0; A(4,4) =  6.0d0

  call DSYEVX('N', 'I', 'U', N, A, LDA, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, LWORK, IWORK, IFAIL, INFO)
  call begin_test('dsyevx_4x4_N_I_U_fast')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  call end_test()

  ! =====================================================
  ! Test 13: JOBZ='V', RANGE='I', UPLO='U', IL=1,IU=N (fast path with vectors)
  ! =====================================================
  N = 4
  LDA = NMAX
  LDZ = NMAX
  IL = 1
  IU = 4
  ABSTOL = 0.0d0

  A(1,1) =  4.0d0; A(1,2) =  1.0d0; A(1,3) = -2.0d0; A(1,4) =  0.0d0
  A(2,1) =  1.0d0; A(2,2) =  3.0d0; A(2,3) =  0.0d0; A(2,4) =  1.0d0
  A(3,1) = -2.0d0; A(3,2) =  0.0d0; A(3,3) =  5.0d0; A(3,4) = -1.0d0
  A(4,1) =  0.0d0; A(4,2) =  1.0d0; A(4,3) = -1.0d0; A(4,4) =  6.0d0

  call DSYEVX('V', 'I', 'U', N, A, LDA, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, LWORK, IWORK, IFAIL, INFO)
  call begin_test('dsyevx_4x4_V_I_U_fast')
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

end program test_dsyevx
