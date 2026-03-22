program test_zggev
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4
  complex*16 :: A(NMAX, NMAX), B(NMAX, NMAX)
  complex*16 :: ALPHA(NMAX), BETA(NMAX)
  complex*16 :: VL(NMAX, NMAX), VR(NMAX, NMAX)
  complex*16 :: WORK(8*NMAX)
  double precision :: RWORK(8*NMAX)
  double precision :: A_r(2*NMAX*NMAX), B_r(2*NMAX*NMAX)
  double precision :: VL_r(2*NMAX*NMAX), VR_r(2*NMAX*NMAX)
  double precision :: ALPHA_r(2*NMAX), BETA_r(2*NMAX)
  equivalence (A, A_r)
  equivalence (B, B_r)
  equivalence (VL, VL_r)
  equivalence (VR, VR_r)
  equivalence (ALPHA, ALPHA_r)
  equivalence (BETA, BETA_r)
  integer :: info, n, lda, ldb, ldvl, ldvr, lwork

  lda = NMAX
  ldb = NMAX
  ldvl = NMAX
  ldvr = NMAX

  ! ==================================================================
  ! Test 1: N=0 quick return
  ! ==================================================================
  n = 0
  lwork = 1
  A = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)

  call ZGGEV('N', 'N', n, A, lda, B, ldb, ALPHA, BETA, &
             VL, ldvl, VR, ldvr, WORK, lwork, RWORK, info)

  call begin_test('n_eq_0')
  call print_int('info', info)
  call end_test()

  ! ==================================================================
  ! Test 2: N=1 trivial case
  ! ==================================================================
  n = 1
  lwork = 8*NMAX

  A = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  ALPHA = (0.0d0, 0.0d0)
  BETA = (0.0d0, 0.0d0)
  VL = (0.0d0, 0.0d0)
  VR = (0.0d0, 0.0d0)

  A(1,1) = (3.0d0, 1.0d0)
  B(1,1) = (2.0d0, 0.5d0)

  call ZGGEV('V', 'V', n, A, lda, B, ldb, ALPHA, BETA, &
             VL, ldvl, VR, ldvr, WORK, lwork, RWORK, info)

  call begin_test('n_eq_1')
  call print_int('info', info)
  call print_array('alpha', ALPHA_r(1), 2*n)
  call print_array('beta', BETA_r(1), 2*n)
  call print_array('VL', VL_r(1), 2*n*n)
  call print_array('VR', VR_r(1), 2*n*n)
  call end_test()

  ! ==================================================================
  ! Test 3: 3x3 right eigenvectors only (JOBVL='N', JOBVR='V')
  ! ==================================================================
  n = 3
  lwork = 8*NMAX

  A = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  ALPHA = (0.0d0, 0.0d0)
  BETA = (0.0d0, 0.0d0)
  VL = (0.0d0, 0.0d0)
  VR = (0.0d0, 0.0d0)

  ! Matrix A
  A(1,1) = (2.0d0, 1.0d0)
  A(1,2) = (1.0d0, 0.5d0)
  A(1,3) = (0.5d0, -0.5d0)
  A(2,1) = (1.0d0, -1.0d0)
  A(2,2) = (3.0d0, 0.0d0)
  A(2,3) = (1.0d0, 1.0d0)
  A(3,1) = (0.5d0, 0.5d0)
  A(3,2) = (0.5d0, -0.5d0)
  A(3,3) = (4.0d0, -1.0d0)

  ! Matrix B
  B(1,1) = (3.0d0, 0.0d0)
  B(1,2) = (1.0d0, 0.5d0)
  B(1,3) = (0.5d0, 0.5d0)
  B(2,1) = (0.5d0, -0.5d0)
  B(2,2) = (2.0d0, 1.0d0)
  B(2,3) = (1.0d0, 0.0d0)
  B(3,1) = (0.0d0, 0.5d0)
  B(3,2) = (0.5d0, 0.0d0)
  B(3,3) = (1.0d0, 0.5d0)

  call ZGGEV('N', 'V', n, A, lda, B, ldb, ALPHA, BETA, &
             VL, ldvl, VR, ldvr, WORK, lwork, RWORK, info)

  call begin_test('right_evec_3x3')
  call print_int('info', info)
  call print_array('alpha', ALPHA_r(1), 2*n)
  call print_array('beta', BETA_r(1), 2*n)
  ! Print VR column by column (interleaved)
  call print_array('VR_col1', VR_r(1), 2*n)
  call print_array('VR_col2', VR_r(2*ldvr+1), 2*n)
  call print_array('VR_col3', VR_r(4*ldvr+1), 2*n)
  call end_test()

  ! ==================================================================
  ! Test 4: 3x3 both eigenvectors (JOBVL='V', JOBVR='V')
  ! ==================================================================
  n = 3
  lwork = 8*NMAX

  A = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  ALPHA = (0.0d0, 0.0d0)
  BETA = (0.0d0, 0.0d0)
  VL = (0.0d0, 0.0d0)
  VR = (0.0d0, 0.0d0)

  ! Same matrices as test 3
  A(1,1) = (2.0d0, 1.0d0)
  A(1,2) = (1.0d0, 0.5d0)
  A(1,3) = (0.5d0, -0.5d0)
  A(2,1) = (1.0d0, -1.0d0)
  A(2,2) = (3.0d0, 0.0d0)
  A(2,3) = (1.0d0, 1.0d0)
  A(3,1) = (0.5d0, 0.5d0)
  A(3,2) = (0.5d0, -0.5d0)
  A(3,3) = (4.0d0, -1.0d0)

  B(1,1) = (3.0d0, 0.0d0)
  B(1,2) = (1.0d0, 0.5d0)
  B(1,3) = (0.5d0, 0.5d0)
  B(2,1) = (0.5d0, -0.5d0)
  B(2,2) = (2.0d0, 1.0d0)
  B(2,3) = (1.0d0, 0.0d0)
  B(3,1) = (0.0d0, 0.5d0)
  B(3,2) = (0.5d0, 0.0d0)
  B(3,3) = (1.0d0, 0.5d0)

  call ZGGEV('V', 'V', n, A, lda, B, ldb, ALPHA, BETA, &
             VL, ldvl, VR, ldvr, WORK, lwork, RWORK, info)

  call begin_test('both_evec_3x3')
  call print_int('info', info)
  call print_array('alpha', ALPHA_r(1), 2*n)
  call print_array('beta', BETA_r(1), 2*n)
  call print_array('VL_col1', VL_r(1), 2*n)
  call print_array('VL_col2', VL_r(2*ldvl+1), 2*n)
  call print_array('VL_col3', VL_r(4*ldvl+1), 2*n)
  call print_array('VR_col1', VR_r(1), 2*n)
  call print_array('VR_col2', VR_r(2*ldvr+1), 2*n)
  call print_array('VR_col3', VR_r(4*ldvr+1), 2*n)
  call end_test()

  ! ==================================================================
  ! Test 5: 3x3 eigenvalues only (JOBVL='N', JOBVR='N')
  ! ==================================================================
  n = 3
  lwork = 8*NMAX

  A = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  ALPHA = (0.0d0, 0.0d0)
  BETA = (0.0d0, 0.0d0)
  VL = (0.0d0, 0.0d0)
  VR = (0.0d0, 0.0d0)

  ! Same matrices as test 3
  A(1,1) = (2.0d0, 1.0d0)
  A(1,2) = (1.0d0, 0.5d0)
  A(1,3) = (0.5d0, -0.5d0)
  A(2,1) = (1.0d0, -1.0d0)
  A(2,2) = (3.0d0, 0.0d0)
  A(2,3) = (1.0d0, 1.0d0)
  A(3,1) = (0.5d0, 0.5d0)
  A(3,2) = (0.5d0, -0.5d0)
  A(3,3) = (4.0d0, -1.0d0)

  B(1,1) = (3.0d0, 0.0d0)
  B(1,2) = (1.0d0, 0.5d0)
  B(1,3) = (0.5d0, 0.5d0)
  B(2,1) = (0.5d0, -0.5d0)
  B(2,2) = (2.0d0, 1.0d0)
  B(2,3) = (1.0d0, 0.0d0)
  B(3,1) = (0.0d0, 0.5d0)
  B(3,2) = (0.5d0, 0.0d0)
  B(3,3) = (1.0d0, 0.5d0)

  call ZGGEV('N', 'N', n, A, lda, B, ldb, ALPHA, BETA, &
             VL, ldvl, VR, ldvr, WORK, lwork, RWORK, info)

  call begin_test('eig_only_3x3')
  call print_int('info', info)
  call print_array('alpha', ALPHA_r(1), 2*n)
  call print_array('beta', BETA_r(1), 2*n)
  call end_test()

  ! ==================================================================
  ! Test 6: 2x2 diagonal case (trivial eigenvalues)
  ! ==================================================================
  n = 2
  lwork = 8*NMAX

  A = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  ALPHA = (0.0d0, 0.0d0)
  BETA = (0.0d0, 0.0d0)
  VL = (0.0d0, 0.0d0)
  VR = (0.0d0, 0.0d0)

  A(1,1) = (4.0d0, 0.0d0)
  A(2,2) = (6.0d0, 0.0d0)

  B(1,1) = (2.0d0, 0.0d0)
  B(2,2) = (3.0d0, 0.0d0)

  call ZGGEV('V', 'V', n, A, lda, B, ldb, ALPHA, BETA, &
             VL, ldvl, VR, ldvr, WORK, lwork, RWORK, info)

  call begin_test('diagonal_2x2')
  call print_int('info', info)
  call print_array('alpha', ALPHA_r(1), 2*n)
  call print_array('beta', BETA_r(1), 2*n)
  call print_array('VL_col1', VL_r(1), 2*n)
  call print_array('VL_col2', VL_r(2*ldvl+1), 2*n)
  call print_array('VR_col1', VR_r(1), 2*n)
  call print_array('VR_col2', VR_r(2*ldvr+1), 2*n)
  call end_test()

  ! ==================================================================
  ! Test 7: N=1 eigenvalues only (no eigenvectors)
  ! ==================================================================
  n = 1
  lwork = 8*NMAX

  A = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  ALPHA = (0.0d0, 0.0d0)
  BETA = (0.0d0, 0.0d0)
  VL = (0.0d0, 0.0d0)
  VR = (0.0d0, 0.0d0)

  A(1,1) = (5.0d0, 2.0d0)
  B(1,1) = (1.0d0, 0.0d0)

  call ZGGEV('N', 'N', n, A, lda, B, ldb, ALPHA, BETA, &
             VL, ldvl, VR, ldvr, WORK, lwork, RWORK, info)

  call begin_test('n_eq_1_noevec')
  call print_int('info', info)
  call print_array('alpha', ALPHA_r(1), 2*n)
  call print_array('beta', BETA_r(1), 2*n)
  call end_test()

  ! ==================================================================
  ! Test 8: left eigenvectors only (JOBVL='V', JOBVR='N')
  ! ==================================================================
  n = 2
  lwork = 8*NMAX

  A = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  ALPHA = (0.0d0, 0.0d0)
  BETA = (0.0d0, 0.0d0)
  VL = (0.0d0, 0.0d0)
  VR = (0.0d0, 0.0d0)

  A(1,1) = (1.0d0, 2.0d0)
  A(1,2) = (3.0d0, -1.0d0)
  A(2,1) = (0.5d0, 0.5d0)
  A(2,2) = (4.0d0, 1.0d0)

  B(1,1) = (2.0d0, 0.0d0)
  B(1,2) = (1.0d0, 1.0d0)
  B(2,1) = (0.0d0, 0.0d0)
  B(2,2) = (3.0d0, -0.5d0)

  call ZGGEV('V', 'N', n, A, lda, B, ldb, ALPHA, BETA, &
             VL, ldvl, VR, ldvr, WORK, lwork, RWORK, info)

  call begin_test('left_evec_2x2')
  call print_int('info', info)
  call print_array('alpha', ALPHA_r(1), 2*n)
  call print_array('beta', BETA_r(1), 2*n)
  call print_array('VL_col1', VL_r(1), 2*n)
  call print_array('VL_col2', VL_r(2*ldvl+1), 2*n)
  call end_test()

end program
