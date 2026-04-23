program test_dsygvx
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4
  integer :: N, LDA, LDB, LDZ, LWORK, IL, IU, M, INFO, i, j
  double precision :: A(NMAX, NMAX), B(NMAX, NMAX), Z(NMAX, NMAX)
  double precision :: W(NMAX), WORK(256)
  integer :: IWORK(5*NMAX), IFAIL(NMAX)
  double precision :: VL, VU, ABSTOL
  double precision :: Aflat(NMAX*NMAX), Zflat(NMAX*NMAX)

  LWORK = 256
  ABSTOL = 0.0d0

  ! =====================================================
  ! Test 1: ITYPE=1, JOBZ='V', RANGE='A', UPLO='L', 4x4
  ! A*x = lambda*B*x
  ! =====================================================
  N = 4
  LDA = NMAX
  LDB = NMAX
  LDZ = NMAX

  ! Symmetric A (diag dominant)
  A(1,1) = 10.0d0; A(1,2) =  1.0d0; A(1,3) =  0.5d0; A(1,4) =  0.0d0
  A(2,1) =  1.0d0; A(2,2) =  8.0d0; A(2,3) =  0.0d0; A(2,4) =  0.5d0
  A(3,1) =  0.5d0; A(3,2) =  0.0d0; A(3,3) = 12.0d0; A(3,4) =  1.0d0
  A(4,1) =  0.0d0; A(4,2) =  0.5d0; A(4,3) =  1.0d0; A(4,4) =  6.0d0

  ! SPD B (diag dominant)
  B(1,1) = 4.0d0; B(1,2) = 1.0d0; B(1,3) = 0.0d0; B(1,4) = 0.0d0
  B(2,1) = 1.0d0; B(2,2) = 5.0d0; B(2,3) = 1.0d0; B(2,4) = 0.0d0
  B(3,1) = 0.0d0; B(3,2) = 1.0d0; B(3,3) = 6.0d0; B(3,4) = 1.0d0
  B(4,1) = 0.0d0; B(4,2) = 0.0d0; B(4,3) = 1.0d0; B(4,4) = 3.0d0

  call DSYGVX(1, 'V', 'A', 'L', N, A, LDA, B, LDB, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, LWORK, IWORK, IFAIL, INFO)
  call begin_test('itype1_V_A_L')
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
  ! Test 2: ITYPE=1, JOBZ='V', RANGE='A', UPLO='U', 4x4
  ! =====================================================
  N = 4

  A(1,1) = 10.0d0; A(1,2) =  1.0d0; A(1,3) =  0.5d0; A(1,4) =  0.0d0
  A(2,1) =  1.0d0; A(2,2) =  8.0d0; A(2,3) =  0.0d0; A(2,4) =  0.5d0
  A(3,1) =  0.5d0; A(3,2) =  0.0d0; A(3,3) = 12.0d0; A(3,4) =  1.0d0
  A(4,1) =  0.0d0; A(4,2) =  0.5d0; A(4,3) =  1.0d0; A(4,4) =  6.0d0

  B(1,1) = 4.0d0; B(1,2) = 1.0d0; B(1,3) = 0.0d0; B(1,4) = 0.0d0
  B(2,1) = 1.0d0; B(2,2) = 5.0d0; B(2,3) = 1.0d0; B(2,4) = 0.0d0
  B(3,1) = 0.0d0; B(3,2) = 1.0d0; B(3,3) = 6.0d0; B(3,4) = 1.0d0
  B(4,1) = 0.0d0; B(4,2) = 0.0d0; B(4,3) = 1.0d0; B(4,4) = 3.0d0

  call DSYGVX(1, 'V', 'A', 'U', N, A, LDA, B, LDB, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, LWORK, IWORK, IFAIL, INFO)
  call begin_test('itype1_V_A_U')
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
  ! Test 3: ITYPE=1, JOBZ='N', RANGE='A', UPLO='L'
  ! Eigenvalues only
  ! =====================================================
  N = 4

  A(1,1) = 10.0d0; A(1,2) =  1.0d0; A(1,3) =  0.5d0; A(1,4) =  0.0d0
  A(2,1) =  1.0d0; A(2,2) =  8.0d0; A(2,3) =  0.0d0; A(2,4) =  0.5d0
  A(3,1) =  0.5d0; A(3,2) =  0.0d0; A(3,3) = 12.0d0; A(3,4) =  1.0d0
  A(4,1) =  0.0d0; A(4,2) =  0.5d0; A(4,3) =  1.0d0; A(4,4) =  6.0d0

  B(1,1) = 4.0d0; B(1,2) = 1.0d0; B(1,3) = 0.0d0; B(1,4) = 0.0d0
  B(2,1) = 1.0d0; B(2,2) = 5.0d0; B(2,3) = 1.0d0; B(2,4) = 0.0d0
  B(3,1) = 0.0d0; B(3,2) = 1.0d0; B(3,3) = 6.0d0; B(3,4) = 1.0d0
  B(4,1) = 0.0d0; B(4,2) = 0.0d0; B(4,3) = 1.0d0; B(4,4) = 3.0d0

  call DSYGVX(1, 'N', 'A', 'L', N, A, LDA, B, LDB, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, LWORK, IWORK, IFAIL, INFO)
  call begin_test('itype1_N_A_L')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  call end_test()

  ! =====================================================
  ! Test 4: ITYPE=1, JOBZ='V', RANGE='V', UPLO='L'
  ! Value range
  ! =====================================================
  N = 4

  A(1,1) = 10.0d0; A(1,2) =  1.0d0; A(1,3) =  0.5d0; A(1,4) =  0.0d0
  A(2,1) =  1.0d0; A(2,2) =  8.0d0; A(2,3) =  0.0d0; A(2,4) =  0.5d0
  A(3,1) =  0.5d0; A(3,2) =  0.0d0; A(3,3) = 12.0d0; A(3,4) =  1.0d0
  A(4,1) =  0.0d0; A(4,2) =  0.5d0; A(4,3) =  1.0d0; A(4,4) =  6.0d0

  B(1,1) = 4.0d0; B(1,2) = 1.0d0; B(1,3) = 0.0d0; B(1,4) = 0.0d0
  B(2,1) = 1.0d0; B(2,2) = 5.0d0; B(2,3) = 1.0d0; B(2,4) = 0.0d0
  B(3,1) = 0.0d0; B(3,2) = 1.0d0; B(3,3) = 6.0d0; B(3,4) = 1.0d0
  B(4,1) = 0.0d0; B(4,2) = 0.0d0; B(4,3) = 1.0d0; B(4,4) = 3.0d0

  VL = 1.5d0
  VU = 2.5d0
  call DSYGVX(1, 'V', 'V', 'L', N, A, LDA, B, LDB, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, LWORK, IWORK, IFAIL, INFO)
  call begin_test('itype1_V_V_L')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  if (M > 0) then
    do j = 1, M
      do i = 1, N
        Zflat((j-1)*N + i) = Z(i,j)
      end do
    end do
    call print_array('Z', Zflat, N*M)
  end if
  call end_test()

  ! =====================================================
  ! Test 5: ITYPE=1, JOBZ='V', RANGE='I', UPLO='L'
  ! Index range: eigenvalues 2 through 3
  ! =====================================================
  N = 4

  A(1,1) = 10.0d0; A(1,2) =  1.0d0; A(1,3) =  0.5d0; A(1,4) =  0.0d0
  A(2,1) =  1.0d0; A(2,2) =  8.0d0; A(2,3) =  0.0d0; A(2,4) =  0.5d0
  A(3,1) =  0.5d0; A(3,2) =  0.0d0; A(3,3) = 12.0d0; A(3,4) =  1.0d0
  A(4,1) =  0.0d0; A(4,2) =  0.5d0; A(4,3) =  1.0d0; A(4,4) =  6.0d0

  B(1,1) = 4.0d0; B(1,2) = 1.0d0; B(1,3) = 0.0d0; B(1,4) = 0.0d0
  B(2,1) = 1.0d0; B(2,2) = 5.0d0; B(2,3) = 1.0d0; B(2,4) = 0.0d0
  B(3,1) = 0.0d0; B(3,2) = 1.0d0; B(3,3) = 6.0d0; B(3,4) = 1.0d0
  B(4,1) = 0.0d0; B(4,2) = 0.0d0; B(4,3) = 1.0d0; B(4,4) = 3.0d0

  IL = 2
  IU = 3
  call DSYGVX(1, 'V', 'I', 'L', N, A, LDA, B, LDB, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, LWORK, IWORK, IFAIL, INFO)
  call begin_test('itype1_V_I_L')
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
  ! Test 6: ITYPE=2, JOBZ='V', RANGE='A', UPLO='L'
  ! A*B*x = lambda*x
  ! =====================================================
  N = 4

  A(1,1) = 10.0d0; A(1,2) =  1.0d0; A(1,3) =  0.5d0; A(1,4) =  0.0d0
  A(2,1) =  1.0d0; A(2,2) =  8.0d0; A(2,3) =  0.0d0; A(2,4) =  0.5d0
  A(3,1) =  0.5d0; A(3,2) =  0.0d0; A(3,3) = 12.0d0; A(3,4) =  1.0d0
  A(4,1) =  0.0d0; A(4,2) =  0.5d0; A(4,3) =  1.0d0; A(4,4) =  6.0d0

  B(1,1) = 4.0d0; B(1,2) = 1.0d0; B(1,3) = 0.0d0; B(1,4) = 0.0d0
  B(2,1) = 1.0d0; B(2,2) = 5.0d0; B(2,3) = 1.0d0; B(2,4) = 0.0d0
  B(3,1) = 0.0d0; B(3,2) = 1.0d0; B(3,3) = 6.0d0; B(3,4) = 1.0d0
  B(4,1) = 0.0d0; B(4,2) = 0.0d0; B(4,3) = 1.0d0; B(4,4) = 3.0d0

  call DSYGVX(2, 'V', 'A', 'L', N, A, LDA, B, LDB, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, LWORK, IWORK, IFAIL, INFO)
  call begin_test('itype2_V_A_L')
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
  ! Test 7: ITYPE=3, JOBZ='V', RANGE='A', UPLO='L'
  ! B*A*x = lambda*x
  ! =====================================================
  N = 4

  A(1,1) = 10.0d0; A(1,2) =  1.0d0; A(1,3) =  0.5d0; A(1,4) =  0.0d0
  A(2,1) =  1.0d0; A(2,2) =  8.0d0; A(2,3) =  0.0d0; A(2,4) =  0.5d0
  A(3,1) =  0.5d0; A(3,2) =  0.0d0; A(3,3) = 12.0d0; A(3,4) =  1.0d0
  A(4,1) =  0.0d0; A(4,2) =  0.5d0; A(4,3) =  1.0d0; A(4,4) =  6.0d0

  B(1,1) = 4.0d0; B(1,2) = 1.0d0; B(1,3) = 0.0d0; B(1,4) = 0.0d0
  B(2,1) = 1.0d0; B(2,2) = 5.0d0; B(2,3) = 1.0d0; B(2,4) = 0.0d0
  B(3,1) = 0.0d0; B(3,2) = 1.0d0; B(3,3) = 6.0d0; B(3,4) = 1.0d0
  B(4,1) = 0.0d0; B(4,2) = 0.0d0; B(4,3) = 1.0d0; B(4,4) = 3.0d0

  call DSYGVX(3, 'V', 'A', 'L', N, A, LDA, B, LDB, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, LWORK, IWORK, IFAIL, INFO)
  call begin_test('itype3_V_A_L')
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
  ! Test 8: N=0, quick return
  ! =====================================================
  N = 0
  call DSYGVX(1, 'V', 'A', 'L', N, A, NMAX, B, NMAX, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, NMAX, WORK, LWORK, IWORK, IFAIL, INFO)
  call begin_test('N0')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! =====================================================
  ! Test 9: Non-positive-definite B (INFO > N)
  ! =====================================================
  N = 3

  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 0.0d0
  A(2,1) = 1.0d0; A(2,2) = 3.0d0; A(2,3) = 1.0d0
  A(3,1) = 0.0d0; A(3,2) = 1.0d0; A(3,3) = 2.0d0

  ! B is not positive definite
  B(1,1) =  1.0d0; B(1,2) =  2.0d0; B(1,3) =  0.0d0
  B(2,1) =  2.0d0; B(2,2) =  1.0d0; B(2,3) =  0.0d0
  B(3,1) =  0.0d0; B(3,2) =  0.0d0; B(3,3) =  1.0d0

  call DSYGVX(1, 'V', 'A', 'L', N, A, NMAX, B, NMAX, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, NMAX, WORK, LWORK, IWORK, IFAIL, INFO)
  call begin_test('nonposdef_B')
  call print_int('N', N)
  call print_int('info', INFO)
  call end_test()

  ! =====================================================
  ! Test 10: ITYPE=3, JOBZ='V', RANGE='A', UPLO='U'
  ! =====================================================
  N = 4

  A(1,1) = 10.0d0; A(1,2) =  1.0d0; A(1,3) =  0.5d0; A(1,4) =  0.0d0
  A(2,1) =  1.0d0; A(2,2) =  8.0d0; A(2,3) =  0.0d0; A(2,4) =  0.5d0
  A(3,1) =  0.5d0; A(3,2) =  0.0d0; A(3,3) = 12.0d0; A(3,4) =  1.0d0
  A(4,1) =  0.0d0; A(4,2) =  0.5d0; A(4,3) =  1.0d0; A(4,4) =  6.0d0

  B(1,1) = 4.0d0; B(1,2) = 1.0d0; B(1,3) = 0.0d0; B(1,4) = 0.0d0
  B(2,1) = 1.0d0; B(2,2) = 5.0d0; B(2,3) = 1.0d0; B(2,4) = 0.0d0
  B(3,1) = 0.0d0; B(3,2) = 1.0d0; B(3,3) = 6.0d0; B(3,4) = 1.0d0
  B(4,1) = 0.0d0; B(4,2) = 0.0d0; B(4,3) = 1.0d0; B(4,4) = 3.0d0

  call DSYGVX(3, 'V', 'A', 'U', N, A, LDA, B, LDB, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, LWORK, IWORK, IFAIL, INFO)
  call begin_test('itype3_V_A_U')
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
  ! Test 11: ITYPE=2, JOBZ='V', RANGE='I', UPLO='U'
  ! =====================================================
  N = 4

  A(1,1) = 10.0d0; A(1,2) =  1.0d0; A(1,3) =  0.5d0; A(1,4) =  0.0d0
  A(2,1) =  1.0d0; A(2,2) =  8.0d0; A(2,3) =  0.0d0; A(2,4) =  0.5d0
  A(3,1) =  0.5d0; A(3,2) =  0.0d0; A(3,3) = 12.0d0; A(3,4) =  1.0d0
  A(4,1) =  0.0d0; A(4,2) =  0.5d0; A(4,3) =  1.0d0; A(4,4) =  6.0d0

  B(1,1) = 4.0d0; B(1,2) = 1.0d0; B(1,3) = 0.0d0; B(1,4) = 0.0d0
  B(2,1) = 1.0d0; B(2,2) = 5.0d0; B(2,3) = 1.0d0; B(2,4) = 0.0d0
  B(3,1) = 0.0d0; B(3,2) = 1.0d0; B(3,3) = 6.0d0; B(3,4) = 1.0d0
  B(4,1) = 0.0d0; B(4,2) = 0.0d0; B(4,3) = 1.0d0; B(4,4) = 3.0d0

  IL = 1
  IU = 2
  call DSYGVX(2, 'V', 'I', 'U', N, A, LDA, B, LDB, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, LWORK, IWORK, IFAIL, INFO)
  call begin_test('itype2_V_I_U')
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

end program
