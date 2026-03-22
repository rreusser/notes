program test_zheev
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4
  integer :: N, LDA, LWORK, INFO, i, j
  complex*16 :: A(NMAX, NMAX), WORK(200)
  double precision :: W(NMAX), RWORK(3*NMAX)
  double precision :: A_r(2*NMAX*NMAX)
  equivalence (A, A_r)

  ! =====================================================
  ! Test 1: JOBZ='V', UPLO='L', 4x4 Hermitian matrix
  ! =====================================================
  N = 4
  LDA = NMAX
  LWORK = 200

  ! Hermitian matrix: A(i,j) = conj(A(j,i)), diagonal is real
  ! Row 1
  A(1,1) = (2.0d0, 0.0d0)
  A(1,2) = (0.0d0, 0.0d0)  ! will be overwritten by lower
  A(1,3) = (0.0d0, 0.0d0)
  A(1,4) = (0.0d0, 0.0d0)
  ! Row 2
  A(2,1) = (1.0d0, 1.0d0)
  A(2,2) = (3.0d0, 0.0d0)
  A(2,3) = (0.0d0, 0.0d0)
  A(2,4) = (0.0d0, 0.0d0)
  ! Row 3
  A(3,1) = (0.5d0, -0.5d0)
  A(3,2) = (0.0d0, 2.0d0)
  A(3,3) = (4.0d0, 0.0d0)
  A(3,4) = (0.0d0, 0.0d0)
  ! Row 4
  A(4,1) = (0.0d0, 0.0d0)
  A(4,2) = (1.0d0, -1.0d0)
  A(4,3) = (0.5d0, 0.5d0)
  A(4,4) = (5.0d0, 0.0d0)

  call ZHEEV('V', 'L', N, A, LDA, W, WORK, LWORK, RWORK, INFO)
  call begin_test('zheev_4x4_V_L')
  call print_int('N', N)
  call print_int('info', INFO)
  call print_array('w', W, N)
  call print_array('A', A_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 2: JOBZ='V', UPLO='U', 4x4 Hermitian matrix
  ! =====================================================
  N = 4
  LDA = NMAX
  LWORK = 200

  ! Same matrix, stored in upper triangle
  A(1,1) = (2.0d0, 0.0d0)
  A(1,2) = (1.0d0, -1.0d0)   ! conj of A(2,1)
  A(1,3) = (0.5d0, 0.5d0)    ! conj of A(3,1)
  A(1,4) = (0.0d0, 0.0d0)
  A(2,1) = (0.0d0, 0.0d0)    ! not referenced
  A(2,2) = (3.0d0, 0.0d0)
  A(2,3) = (0.0d0, -2.0d0)   ! conj of A(3,2)
  A(2,4) = (1.0d0, 1.0d0)    ! conj of A(4,2)
  A(3,1) = (0.0d0, 0.0d0)
  A(3,2) = (0.0d0, 0.0d0)
  A(3,3) = (4.0d0, 0.0d0)
  A(3,4) = (0.5d0, -0.5d0)   ! conj of A(4,3)
  A(4,1) = (0.0d0, 0.0d0)
  A(4,2) = (0.0d0, 0.0d0)
  A(4,3) = (0.0d0, 0.0d0)
  A(4,4) = (5.0d0, 0.0d0)

  call ZHEEV('V', 'U', N, A, LDA, W, WORK, LWORK, RWORK, INFO)
  call begin_test('zheev_4x4_V_U')
  call print_int('N', N)
  call print_int('info', INFO)
  call print_array('w', W, N)
  call print_array('A', A_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 3: JOBZ='N', eigenvalues only, 4x4
  ! =====================================================
  N = 4
  LDA = NMAX
  LWORK = 200

  ! Same matrix, lower triangle
  A(1,1) = (2.0d0, 0.0d0)
  A(2,1) = (1.0d0, 1.0d0)
  A(3,1) = (0.5d0, -0.5d0)
  A(4,1) = (0.0d0, 0.0d0)
  A(2,2) = (3.0d0, 0.0d0)
  A(3,2) = (0.0d0, 2.0d0)
  A(4,2) = (1.0d0, -1.0d0)
  A(3,3) = (4.0d0, 0.0d0)
  A(4,3) = (0.5d0, 0.5d0)
  A(4,4) = (5.0d0, 0.0d0)

  call ZHEEV('N', 'L', N, A, LDA, W, WORK, LWORK, RWORK, INFO)
  call begin_test('zheev_4x4_N_L')
  call print_int('N', N)
  call print_int('info', INFO)
  call print_array('w', W, N)
  call end_test()

  ! =====================================================
  ! Test 4: 3x3 Hermitian, JOBZ='V', UPLO='L'
  ! =====================================================
  N = 3
  LDA = NMAX
  LWORK = 200

  A(1,1) = (4.0d0, 0.0d0)
  A(2,1) = (1.0d0, -2.0d0)
  A(3,1) = (0.0d0, 1.0d0)
  A(2,2) = (5.0d0, 0.0d0)
  A(3,2) = (2.0d0, 0.0d0)
  A(3,3) = (6.0d0, 0.0d0)

  call ZHEEV('V', 'L', N, A, LDA, W, WORK, LWORK, RWORK, INFO)
  call begin_test('zheev_3x3_V_L')
  call print_int('N', N)
  call print_int('info', INFO)
  call print_array('w', W, N)
  ! Only output 3x3 portion
  call print_array('A', A_r, 2*LDA*N)
  call end_test()

  ! =====================================================
  ! Test 5: 3x3 Hermitian, JOBZ='V', UPLO='U'
  ! =====================================================
  N = 3
  LDA = NMAX
  LWORK = 200

  A(1,1) = (4.0d0, 0.0d0)
  A(1,2) = (1.0d0, 2.0d0)    ! conj of (1,-2)
  A(1,3) = (0.0d0, -1.0d0)   ! conj of (0,1)
  A(2,2) = (5.0d0, 0.0d0)
  A(2,3) = (2.0d0, 0.0d0)
  A(3,3) = (6.0d0, 0.0d0)

  call ZHEEV('V', 'U', N, A, LDA, W, WORK, LWORK, RWORK, INFO)
  call begin_test('zheev_3x3_V_U')
  call print_int('N', N)
  call print_int('info', INFO)
  call print_array('w', W, N)
  call print_array('A', A_r, 2*LDA*N)
  call end_test()

  ! =====================================================
  ! Test 6: N=1, JOBZ='V'
  ! =====================================================
  N = 1
  LDA = NMAX
  LWORK = 200

  A(1,1) = (7.5d0, 0.0d0)

  call ZHEEV('V', 'L', N, A, LDA, W, WORK, LWORK, RWORK, INFO)
  call begin_test('zheev_1x1_V')
  call print_int('N', N)
  call print_int('info', INFO)
  call print_array('w', W, N)
  call print_array('A', A_r, 2)
  call end_test()

  ! =====================================================
  ! Test 7: N=0 (empty)
  ! =====================================================
  N = 0
  LDA = NMAX
  LWORK = 200

  call ZHEEV('V', 'L', N, A, LDA, W, WORK, LWORK, RWORK, INFO)
  call begin_test('zheev_0x0')
  call print_int('N', N)
  call print_int('info', INFO)
  call end_test()

  ! =====================================================
  ! Test 8: 2x2 diagonal matrix
  ! =====================================================
  N = 2
  LDA = NMAX
  LWORK = 200

  A(1,1) = (3.0d0, 0.0d0)
  A(2,1) = (0.0d0, 0.0d0)
  A(2,2) = (1.0d0, 0.0d0)

  call ZHEEV('V', 'L', N, A, LDA, W, WORK, LWORK, RWORK, INFO)
  call begin_test('zheev_2x2_diag')
  call print_int('N', N)
  call print_int('info', INFO)
  call print_array('w', W, N)
  call print_array('A', A_r, 2*LDA*N)
  call end_test()

  ! =====================================================
  ! Test 9: JOBZ='N', UPLO='U', 3x3
  ! =====================================================
  N = 3
  LDA = NMAX
  LWORK = 200

  A(1,1) = (4.0d0, 0.0d0)
  A(1,2) = (1.0d0, 2.0d0)
  A(1,3) = (0.0d0, -1.0d0)
  A(2,2) = (5.0d0, 0.0d0)
  A(2,3) = (2.0d0, 0.0d0)
  A(3,3) = (6.0d0, 0.0d0)

  call ZHEEV('N', 'U', N, A, LDA, W, WORK, LWORK, RWORK, INFO)
  call begin_test('zheev_3x3_N_U')
  call print_int('N', N)
  call print_int('info', INFO)
  call print_array('w', W, N)
  call end_test()

end program test_zheev
