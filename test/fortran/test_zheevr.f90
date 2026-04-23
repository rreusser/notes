program test_zheevr
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4
  integer :: N, LDA, LDZ, LWORK, LRWORK, LIWORK, IL, IU, M, INFO, i, j
  complex*16 :: A(NMAX, NMAX), Z(NMAX, NMAX), WORK(256)
  double precision :: W(NMAX), RWORK(256)
  integer :: IWORK(256), ISUPPZ(2*NMAX)
  double precision :: VL, VU, ABSTOL

  ! For printing complex arrays via EQUIVALENCE
  double precision :: Aflat_r(2*NMAX*NMAX), Zflat_r(2*NMAX*NMAX)
  complex*16 :: Aflat(NMAX*NMAX), Zflat(NMAX*NMAX)
  equivalence (Aflat, Aflat_r)
  equivalence (Zflat, Zflat_r)

  LWORK = 256
  LRWORK = 256
  LIWORK = 256
  ABSTOL = 0.0d0

  ! =====================================================
  ! Test 1: JOBZ='V', RANGE='A', UPLO='L', 4x4
  ! Hermitian matrix (diag dominant, real diagonal)
  ! =====================================================
  N = 4
  LDA = NMAX
  LDZ = NMAX

  A(1,1) = dcmplx(10.0d0, 0.0d0)
  A(2,1) = dcmplx( 1.0d0,-0.5d0); A(1,2) = dcmplx( 1.0d0, 0.5d0)
  A(3,1) = dcmplx( 0.0d0, 1.0d0); A(1,3) = dcmplx( 0.0d0,-1.0d0)
  A(4,1) = dcmplx( 0.5d0, 0.0d0); A(1,4) = dcmplx( 0.5d0, 0.0d0)
  A(2,2) = dcmplx( 8.0d0, 0.0d0)
  A(3,2) = dcmplx( 0.5d0, 0.5d0); A(2,3) = dcmplx( 0.5d0,-0.5d0)
  A(4,2) = dcmplx( 0.0d0, 1.0d0); A(2,4) = dcmplx( 0.0d0,-1.0d0)
  A(3,3) = dcmplx(12.0d0, 0.0d0)
  A(4,3) = dcmplx( 1.0d0,-0.5d0); A(3,4) = dcmplx( 1.0d0, 0.5d0)
  A(4,4) = dcmplx( 6.0d0, 0.0d0)

  call ZHEEVR('V', 'A', 'L', N, A, LDA, VL, VU, IL, IU, ABSTOL, &
              M, W, Z, LDZ, ISUPPZ, WORK, LWORK, RWORK, LRWORK, &
              IWORK, LIWORK, INFO)
  call begin_test('V_A_L')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  do j = 1, M
    do i = 1, N
      Zflat((j-1)*N + i) = Z(i,j)
    end do
  end do
  call print_array('Z', Zflat_r, 2*N*M)
  call end_test()

  ! =====================================================
  ! Test 2: JOBZ='V', RANGE='A', UPLO='U', 4x4
  ! =====================================================
  N = 4

  A(1,1) = dcmplx(10.0d0, 0.0d0)
  A(2,1) = dcmplx( 1.0d0,-0.5d0); A(1,2) = dcmplx( 1.0d0, 0.5d0)
  A(3,1) = dcmplx( 0.0d0, 1.0d0); A(1,3) = dcmplx( 0.0d0,-1.0d0)
  A(4,1) = dcmplx( 0.5d0, 0.0d0); A(1,4) = dcmplx( 0.5d0, 0.0d0)
  A(2,2) = dcmplx( 8.0d0, 0.0d0)
  A(3,2) = dcmplx( 0.5d0, 0.5d0); A(2,3) = dcmplx( 0.5d0,-0.5d0)
  A(4,2) = dcmplx( 0.0d0, 1.0d0); A(2,4) = dcmplx( 0.0d0,-1.0d0)
  A(3,3) = dcmplx(12.0d0, 0.0d0)
  A(4,3) = dcmplx( 1.0d0,-0.5d0); A(3,4) = dcmplx( 1.0d0, 0.5d0)
  A(4,4) = dcmplx( 6.0d0, 0.0d0)

  call ZHEEVR('V', 'A', 'U', N, A, LDA, VL, VU, IL, IU, ABSTOL, &
              M, W, Z, LDZ, ISUPPZ, WORK, LWORK, RWORK, LRWORK, &
              IWORK, LIWORK, INFO)
  call begin_test('V_A_U')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  do j = 1, M
    do i = 1, N
      Zflat((j-1)*N + i) = Z(i,j)
    end do
  end do
  call print_array('Z', Zflat_r, 2*N*M)
  call end_test()

  ! =====================================================
  ! Test 3: JOBZ='N', RANGE='A', UPLO='L'
  ! Eigenvalues only
  ! =====================================================
  N = 4

  A(1,1) = dcmplx(10.0d0, 0.0d0)
  A(2,1) = dcmplx( 1.0d0,-0.5d0); A(1,2) = dcmplx( 1.0d0, 0.5d0)
  A(3,1) = dcmplx( 0.0d0, 1.0d0); A(1,3) = dcmplx( 0.0d0,-1.0d0)
  A(4,1) = dcmplx( 0.5d0, 0.0d0); A(1,4) = dcmplx( 0.5d0, 0.0d0)
  A(2,2) = dcmplx( 8.0d0, 0.0d0)
  A(3,2) = dcmplx( 0.5d0, 0.5d0); A(2,3) = dcmplx( 0.5d0,-0.5d0)
  A(4,2) = dcmplx( 0.0d0, 1.0d0); A(2,4) = dcmplx( 0.0d0,-1.0d0)
  A(3,3) = dcmplx(12.0d0, 0.0d0)
  A(4,3) = dcmplx( 1.0d0,-0.5d0); A(3,4) = dcmplx( 1.0d0, 0.5d0)
  A(4,4) = dcmplx( 6.0d0, 0.0d0)

  call ZHEEVR('N', 'A', 'L', N, A, LDA, VL, VU, IL, IU, ABSTOL, &
              M, W, Z, LDZ, ISUPPZ, WORK, LWORK, RWORK, LRWORK, &
              IWORK, LIWORK, INFO)
  call begin_test('N_A_L')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  call end_test()

  ! =====================================================
  ! Test 4: JOBZ='V', RANGE='V', UPLO='L'
  ! Value range
  ! =====================================================
  N = 4

  A(1,1) = dcmplx(10.0d0, 0.0d0)
  A(2,1) = dcmplx( 1.0d0,-0.5d0); A(1,2) = dcmplx( 1.0d0, 0.5d0)
  A(3,1) = dcmplx( 0.0d0, 1.0d0); A(1,3) = dcmplx( 0.0d0,-1.0d0)
  A(4,1) = dcmplx( 0.5d0, 0.0d0); A(1,4) = dcmplx( 0.5d0, 0.0d0)
  A(2,2) = dcmplx( 8.0d0, 0.0d0)
  A(3,2) = dcmplx( 0.5d0, 0.5d0); A(2,3) = dcmplx( 0.5d0,-0.5d0)
  A(4,2) = dcmplx( 0.0d0, 1.0d0); A(2,4) = dcmplx( 0.0d0,-1.0d0)
  A(3,3) = dcmplx(12.0d0, 0.0d0)
  A(4,3) = dcmplx( 1.0d0,-0.5d0); A(3,4) = dcmplx( 1.0d0, 0.5d0)
  A(4,4) = dcmplx( 6.0d0, 0.0d0)

  VL = 7.0d0
  VU = 11.0d0
  call ZHEEVR('V', 'V', 'L', N, A, LDA, VL, VU, IL, IU, ABSTOL, &
              M, W, Z, LDZ, ISUPPZ, WORK, LWORK, RWORK, LRWORK, &
              IWORK, LIWORK, INFO)
  call begin_test('V_V_L')
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
    call print_array('Z', Zflat_r, 2*N*M)
  end if
  call end_test()

  ! =====================================================
  ! Test 5: JOBZ='V', RANGE='I', UPLO='L'
  ! Index range: eigenvalues 2 through 3
  ! =====================================================
  N = 4

  A(1,1) = dcmplx(10.0d0, 0.0d0)
  A(2,1) = dcmplx( 1.0d0,-0.5d0); A(1,2) = dcmplx( 1.0d0, 0.5d0)
  A(3,1) = dcmplx( 0.0d0, 1.0d0); A(1,3) = dcmplx( 0.0d0,-1.0d0)
  A(4,1) = dcmplx( 0.5d0, 0.0d0); A(1,4) = dcmplx( 0.5d0, 0.0d0)
  A(2,2) = dcmplx( 8.0d0, 0.0d0)
  A(3,2) = dcmplx( 0.5d0, 0.5d0); A(2,3) = dcmplx( 0.5d0,-0.5d0)
  A(4,2) = dcmplx( 0.0d0, 1.0d0); A(2,4) = dcmplx( 0.0d0,-1.0d0)
  A(3,3) = dcmplx(12.0d0, 0.0d0)
  A(4,3) = dcmplx( 1.0d0,-0.5d0); A(3,4) = dcmplx( 1.0d0, 0.5d0)
  A(4,4) = dcmplx( 6.0d0, 0.0d0)

  IL = 2
  IU = 3
  call ZHEEVR('V', 'I', 'L', N, A, LDA, VL, VU, IL, IU, ABSTOL, &
              M, W, Z, LDZ, ISUPPZ, WORK, LWORK, RWORK, LRWORK, &
              IWORK, LIWORK, INFO)
  call begin_test('V_I_L')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  do j = 1, M
    do i = 1, N
      Zflat((j-1)*N + i) = Z(i,j)
    end do
  end do
  call print_array('Z', Zflat_r, 2*N*M)
  call end_test()

  ! =====================================================
  ! Test 6: N=0
  ! =====================================================
  N = 0
  call ZHEEVR('V', 'A', 'L', N, A, NMAX, VL, VU, IL, IU, ABSTOL, &
              M, W, Z, NMAX, ISUPPZ, WORK, LWORK, RWORK, LRWORK, &
              IWORK, LIWORK, INFO)
  call begin_test('N0')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! =====================================================
  ! Test 7: N=1
  ! =====================================================
  N = 1
  A(1,1) = dcmplx(5.0d0, 0.0d0)
  call ZHEEVR('V', 'A', 'L', N, A, NMAX, VL, VU, IL, IU, ABSTOL, &
              M, W, Z, NMAX, ISUPPZ, WORK, LWORK, RWORK, LRWORK, &
              IWORK, LIWORK, INFO)
  call begin_test('N1')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  Zflat(1) = Z(1,1)
  call print_array('Z', Zflat_r, 2*N*M)
  call end_test()

  ! =====================================================
  ! Test 8: JOBZ='V', RANGE='V', UPLO='U'
  ! =====================================================
  N = 4

  A(1,1) = dcmplx(10.0d0, 0.0d0)
  A(2,1) = dcmplx( 1.0d0,-0.5d0); A(1,2) = dcmplx( 1.0d0, 0.5d0)
  A(3,1) = dcmplx( 0.0d0, 1.0d0); A(1,3) = dcmplx( 0.0d0,-1.0d0)
  A(4,1) = dcmplx( 0.5d0, 0.0d0); A(1,4) = dcmplx( 0.5d0, 0.0d0)
  A(2,2) = dcmplx( 8.0d0, 0.0d0)
  A(3,2) = dcmplx( 0.5d0, 0.5d0); A(2,3) = dcmplx( 0.5d0,-0.5d0)
  A(4,2) = dcmplx( 0.0d0, 1.0d0); A(2,4) = dcmplx( 0.0d0,-1.0d0)
  A(3,3) = dcmplx(12.0d0, 0.0d0)
  A(4,3) = dcmplx( 1.0d0,-0.5d0); A(3,4) = dcmplx( 1.0d0, 0.5d0)
  A(4,4) = dcmplx( 6.0d0, 0.0d0)

  VL = 7.0d0
  VU = 11.0d0
  call ZHEEVR('V', 'V', 'U', N, A, LDA, VL, VU, IL, IU, ABSTOL, &
              M, W, Z, LDZ, ISUPPZ, WORK, LWORK, RWORK, LRWORK, &
              IWORK, LIWORK, INFO)
  call begin_test('V_V_U')
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
    call print_array('Z', Zflat_r, 2*N*M)
  end if
  call end_test()

end program
