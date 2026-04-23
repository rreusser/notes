program test_dsyevr
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4
  integer :: N, LDA, LDZ, LWORK, LIWORK, IL, IU, M, INFO, i, j
  double precision :: A(NMAX, NMAX), Z(NMAX, NMAX), WORK(512)
  double precision :: W(NMAX)
  integer :: IWORK(512), ISUPPZ(2*NMAX)
  double precision :: VL, VU, ABSTOL
  double precision :: Aflat(NMAX*NMAX), Zflat(NMAX*NMAX)

  LWORK = 512
  LIWORK = 512
  ABSTOL = 0.0d0

  ! =====================================================
  ! Test 1: JOBZ='V', RANGE='A', UPLO='L', 4x4
  ! Diagonally dominant symmetric matrix
  ! =====================================================
  N = 4
  LDA = NMAX
  LDZ = NMAX

  ! Pack column-major lower triangle only (Fortran reads lower)
  A = 0.0d0
  A(1,1) = 10.0d0
  A(2,1) =  1.0d0;  A(2,2) = 8.0d0
  A(3,1) =  0.5d0;  A(3,2) = 0.5d0;  A(3,3) = 12.0d0
  A(4,1) =  0.25d0; A(4,2) = 0.0d0;  A(4,3) =  1.0d0;  A(4,4) = 6.0d0

  ! Save A for printing (pack the full symmetric matrix column-major)
  do j = 1, N
    do i = 1, N
      if (i >= j) then
        Aflat((j-1)*N + i) = A(i,j)
      else
        Aflat((j-1)*N + i) = A(j,i)
      end if
    end do
  end do

  call DSYEVR('V', 'A', 'L', N, A, LDA, VL, VU, IL, IU, ABSTOL, &
              M, W, Z, LDZ, ISUPPZ, WORK, LWORK, IWORK, LIWORK, INFO)
  call begin_test('V_A_L')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('A_in', Aflat, N*N)
  call print_array('w', W, M)
  call print_matrix('Z', Z, LDZ, N, M)
  call end_test()

  ! =====================================================
  ! Test 2: JOBZ='V', RANGE='A', UPLO='U', 4x4
  ! Same matrix but stored as upper triangle
  ! =====================================================
  N = 4

  A = 0.0d0
  A(1,1) = 10.0d0
  A(1,2) =  1.0d0;  A(2,2) = 8.0d0
  A(1,3) =  0.5d0;  A(2,3) = 0.5d0;  A(3,3) = 12.0d0
  A(1,4) =  0.25d0; A(2,4) = 0.0d0;  A(3,4) =  1.0d0;  A(4,4) = 6.0d0

  call DSYEVR('V', 'A', 'U', N, A, LDA, VL, VU, IL, IU, ABSTOL, &
              M, W, Z, LDZ, ISUPPZ, WORK, LWORK, IWORK, LIWORK, INFO)
  call begin_test('V_A_U')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  call print_matrix('Z', Z, LDZ, N, M)
  call end_test()

  ! =====================================================
  ! Test 3: JOBZ='N', RANGE='A', UPLO='L'
  ! Eigenvalues only
  ! =====================================================
  N = 4

  A = 0.0d0
  A(1,1) = 10.0d0
  A(2,1) =  1.0d0;  A(2,2) = 8.0d0
  A(3,1) =  0.5d0;  A(3,2) = 0.5d0;  A(3,3) = 12.0d0
  A(4,1) =  0.25d0; A(4,2) = 0.0d0;  A(4,3) =  1.0d0;  A(4,4) = 6.0d0

  call DSYEVR('N', 'A', 'L', N, A, LDA, VL, VU, IL, IU, ABSTOL, &
              M, W, Z, LDZ, ISUPPZ, WORK, LWORK, IWORK, LIWORK, INFO)
  call begin_test('N_A_L')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  call end_test()

  ! =====================================================
  ! Test 4: JOBZ='V', RANGE='V', UPLO='L'
  ! Value range: eigenvalues in (7, 11)
  ! =====================================================
  N = 4

  A = 0.0d0
  A(1,1) = 10.0d0
  A(2,1) =  1.0d0;  A(2,2) = 8.0d0
  A(3,1) =  0.5d0;  A(3,2) = 0.5d0;  A(3,3) = 12.0d0
  A(4,1) =  0.25d0; A(4,2) = 0.0d0;  A(4,3) =  1.0d0;  A(4,4) = 6.0d0

  VL = 7.0d0
  VU = 11.0d0
  call DSYEVR('V', 'V', 'L', N, A, LDA, VL, VU, IL, IU, ABSTOL, &
              M, W, Z, LDZ, ISUPPZ, WORK, LWORK, IWORK, LIWORK, INFO)
  call begin_test('V_V_L')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  if (M > 0) then
    call print_matrix('Z', Z, LDZ, N, M)
  end if
  call end_test()

  ! =====================================================
  ! Test 5: JOBZ='V', RANGE='I', UPLO='L'
  ! Index range: eigenvalues 2 through 3
  ! =====================================================
  N = 4

  A = 0.0d0
  A(1,1) = 10.0d0
  A(2,1) =  1.0d0;  A(2,2) = 8.0d0
  A(3,1) =  0.5d0;  A(3,2) = 0.5d0;  A(3,3) = 12.0d0
  A(4,1) =  0.25d0; A(4,2) = 0.0d0;  A(4,3) =  1.0d0;  A(4,4) = 6.0d0

  IL = 2
  IU = 3
  call DSYEVR('V', 'I', 'L', N, A, LDA, VL, VU, IL, IU, ABSTOL, &
              M, W, Z, LDZ, ISUPPZ, WORK, LWORK, IWORK, LIWORK, INFO)
  call begin_test('V_I_L')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  call print_matrix('Z', Z, LDZ, N, M)
  call end_test()

  ! =====================================================
  ! Test 6: N=0
  ! =====================================================
  N = 0
  call DSYEVR('V', 'A', 'L', N, A, NMAX, VL, VU, IL, IU, ABSTOL, &
              M, W, Z, NMAX, ISUPPZ, WORK, LWORK, IWORK, LIWORK, INFO)
  call begin_test('N0')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! =====================================================
  ! Test 7: N=1
  ! =====================================================
  N = 1
  A(1,1) = 5.0d0
  call DSYEVR('V', 'A', 'L', N, A, NMAX, VL, VU, IL, IU, ABSTOL, &
              M, W, Z, NMAX, ISUPPZ, WORK, LWORK, IWORK, LIWORK, INFO)
  call begin_test('N1')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  call print_matrix('Z', Z, NMAX, N, M)
  call end_test()

  ! =====================================================
  ! Test 8: JOBZ='N', RANGE='V', UPLO='U'
  ! Eigenvalues only, value range, upper
  ! =====================================================
  N = 4

  A = 0.0d0
  A(1,1) = 10.0d0
  A(1,2) =  1.0d0;  A(2,2) = 8.0d0
  A(1,3) =  0.5d0;  A(2,3) = 0.5d0;  A(3,3) = 12.0d0
  A(1,4) =  0.25d0; A(2,4) = 0.0d0;  A(3,4) =  1.0d0;  A(4,4) = 6.0d0

  VL = 7.0d0
  VU = 11.0d0
  call DSYEVR('N', 'V', 'U', N, A, LDA, VL, VU, IL, IU, ABSTOL, &
              M, W, Z, LDZ, ISUPPZ, WORK, LWORK, IWORK, LIWORK, INFO)
  call begin_test('N_V_U')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  call end_test()

  ! =====================================================
  ! Test 9: JOBZ='N', RANGE='I', UPLO='L'
  ! Eigenvalues only, index range
  ! =====================================================
  N = 4

  A = 0.0d0
  A(1,1) = 10.0d0
  A(2,1) =  1.0d0;  A(2,2) = 8.0d0
  A(3,1) =  0.5d0;  A(3,2) = 0.5d0;  A(3,3) = 12.0d0
  A(4,1) =  0.25d0; A(4,2) = 0.0d0;  A(4,3) =  1.0d0;  A(4,4) = 6.0d0

  IL = 1
  IU = 2
  call DSYEVR('N', 'I', 'L', N, A, LDA, VL, VU, IL, IU, ABSTOL, &
              M, W, Z, LDZ, ISUPPZ, WORK, LWORK, IWORK, LIWORK, INFO)
  call begin_test('N_I_L')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  call end_test()

end program
