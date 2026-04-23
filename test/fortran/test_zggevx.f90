program test_zggevx
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4
  complex*16 :: A(NMAX, NMAX), B(NMAX, NMAX)
  complex*16 :: ALPHA(NMAX), BETA(NMAX)
  complex*16 :: VL(NMAX, NMAX), VR(NMAX, NMAX)
  complex*16 :: WORK(8*NMAX*NMAX)
  double precision :: RWORK(8*NMAX)
  double precision :: LSCALE(NMAX), RSCALE(NMAX)
  double precision :: RCONDE(NMAX), RCONDV(NMAX)
  integer :: IWORK(NMAX+2)
  logical :: BWORK(NMAX)
  double precision :: A_r(2*NMAX*NMAX), B_r(2*NMAX*NMAX)
  double precision :: VL_r(2*NMAX*NMAX), VR_r(2*NMAX*NMAX)
  double precision :: ALPHA_r(2*NMAX), BETA_r(2*NMAX)
  equivalence (A, A_r)
  equivalence (B, B_r)
  equivalence (VL, VL_r)
  equivalence (VR, VR_r)
  equivalence (ALPHA, ALPHA_r)
  equivalence (BETA, BETA_r)
  integer :: info, n, lda, ldb, ldvl, ldvr, lwork, ilo, ihi
  double precision :: abnrm, bbnrm

  lda = NMAX
  ldb = NMAX
  ldvl = NMAX
  ldvr = NMAX
  lwork = 8*NMAX*NMAX

  ! ==================================================================
  ! Test 1: N=0 quick return
  ! ==================================================================
  n = 0
  A = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)

  call ZGGEVX('N', 'N', 'N', 'N', n, A, lda, B, ldb, ALPHA, BETA, &
              VL, ldvl, VR, ldvr, ilo, ihi, LSCALE, RSCALE, &
              abnrm, bbnrm, RCONDE, RCONDV, WORK, lwork, RWORK, &
              IWORK, BWORK, info)

  call begin_test('n_eq_0')
  call print_int('info', info)
  call end_test()

  ! ==================================================================
  ! Test 2: N=1 trivial case, no balancing, no sense
  ! ==================================================================
  n = 1
  A = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  ALPHA = (0.0d0, 0.0d0)
  BETA = (0.0d0, 0.0d0)
  VL = (0.0d0, 0.0d0)
  VR = (0.0d0, 0.0d0)
  A(1,1) = (3.0d0, 1.0d0)
  B(1,1) = (2.0d0, 0.5d0)

  call ZGGEVX('N', 'V', 'V', 'N', n, A, lda, B, ldb, ALPHA, BETA, &
              VL, ldvl, VR, ldvr, ilo, ihi, LSCALE, RSCALE, &
              abnrm, bbnrm, RCONDE, RCONDV, WORK, lwork, RWORK, &
              IWORK, BWORK, info)

  call begin_test('n_eq_1')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_scalar('abnrm', abnrm)
  call print_scalar('bbnrm', bbnrm)
  call print_array('alpha', ALPHA_r(1), 2*n)
  call print_array('beta', BETA_r(1), 2*n)
  call print_array('VL', VL_r(1), 2*n)
  call print_array('VR', VR_r(1), 2*n)
  call end_test()

  ! ==================================================================
  ! Test 3: 3x3 both eigenvectors, balanc='N', sense='N'
  ! ==================================================================
  n = 3
  A = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  ALPHA = (0.0d0, 0.0d0)
  BETA = (0.0d0, 0.0d0)
  VL = (0.0d0, 0.0d0)
  VR = (0.0d0, 0.0d0)

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

  call ZGGEVX('N', 'V', 'V', 'N', n, A, lda, B, ldb, ALPHA, BETA, &
              VL, ldvl, VR, ldvr, ilo, ihi, LSCALE, RSCALE, &
              abnrm, bbnrm, RCONDE, RCONDV, WORK, lwork, RWORK, &
              IWORK, BWORK, info)

  call begin_test('balN_both_3x3')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_scalar('abnrm', abnrm)
  call print_scalar('bbnrm', bbnrm)
  call print_array('alpha', ALPHA_r(1), 2*n)
  call print_array('beta', BETA_r(1), 2*n)
  call print_array('lscale', LSCALE(1), n)
  call print_array('rscale', RSCALE(1), n)
  call print_array('VL_col1', VL_r(1), 2*n)
  call print_array('VL_col2', VL_r(2*ldvl+1), 2*n)
  call print_array('VL_col3', VL_r(4*ldvl+1), 2*n)
  call print_array('VR_col1', VR_r(1), 2*n)
  call print_array('VR_col2', VR_r(2*ldvr+1), 2*n)
  call print_array('VR_col3', VR_r(4*ldvr+1), 2*n)
  call end_test()

  ! ==================================================================
  ! Test 4: 3x3 eigenvalues only, balanc='P'
  ! ==================================================================
  n = 3
  A = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  ALPHA = (0.0d0, 0.0d0)
  BETA = (0.0d0, 0.0d0)

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

  call ZGGEVX('P', 'N', 'N', 'N', n, A, lda, B, ldb, ALPHA, BETA, &
              VL, ldvl, VR, ldvr, ilo, ihi, LSCALE, RSCALE, &
              abnrm, bbnrm, RCONDE, RCONDV, WORK, lwork, RWORK, &
              IWORK, BWORK, info)

  call begin_test('balP_eigonly_3x3')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_scalar('abnrm', abnrm)
  call print_scalar('bbnrm', bbnrm)
  call print_array('alpha', ALPHA_r(1), 2*n)
  call print_array('beta', BETA_r(1), 2*n)
  call print_array('lscale', LSCALE(1), n)
  call print_array('rscale', RSCALE(1), n)
  call end_test()

  ! ==================================================================
  ! Test 5: 2x2 diagonal, balanc='B' (both permute and scale)
  ! ==================================================================
  n = 2
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

  call ZGGEVX('B', 'V', 'V', 'N', n, A, lda, B, ldb, ALPHA, BETA, &
              VL, ldvl, VR, ldvr, ilo, ihi, LSCALE, RSCALE, &
              abnrm, bbnrm, RCONDE, RCONDV, WORK, lwork, RWORK, &
              IWORK, BWORK, info)

  call begin_test('balB_diag_2x2')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_scalar('abnrm', abnrm)
  call print_scalar('bbnrm', bbnrm)
  call print_array('alpha', ALPHA_r(1), 2*n)
  call print_array('beta', BETA_r(1), 2*n)
  call print_array('lscale', LSCALE(1), n)
  call print_array('rscale', RSCALE(1), n)
  call print_array('VL_col1', VL_r(1), 2*n)
  call print_array('VL_col2', VL_r(2*ldvl+1), 2*n)
  call print_array('VR_col1', VR_r(1), 2*n)
  call print_array('VR_col2', VR_r(2*ldvr+1), 2*n)
  call end_test()

  ! ==================================================================
  ! Test 6: 3x3 right vectors only, balanc='S' (scale only)
  ! ==================================================================
  n = 3
  A = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  ALPHA = (0.0d0, 0.0d0)
  BETA = (0.0d0, 0.0d0)
  VL = (0.0d0, 0.0d0)
  VR = (0.0d0, 0.0d0)

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

  call ZGGEVX('S', 'N', 'V', 'N', n, A, lda, B, ldb, ALPHA, BETA, &
              VL, ldvl, VR, ldvr, ilo, ihi, LSCALE, RSCALE, &
              abnrm, bbnrm, RCONDE, RCONDV, WORK, lwork, RWORK, &
              IWORK, BWORK, info)

  call begin_test('balS_right_3x3')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_scalar('abnrm', abnrm)
  call print_scalar('bbnrm', bbnrm)
  call print_array('alpha', ALPHA_r(1), 2*n)
  call print_array('beta', BETA_r(1), 2*n)
  call print_array('lscale', LSCALE(1), n)
  call print_array('rscale', RSCALE(1), n)
  call print_array('VR_col1', VR_r(1), 2*n)
  call print_array('VR_col2', VR_r(2*ldvr+1), 2*n)
  call print_array('VR_col3', VR_r(4*ldvr+1), 2*n)
  call end_test()

  ! ==================================================================
  ! Test 7: 3x3 sense='E' (eigenvalue condition numbers)
  ! ==================================================================
  n = 3
  A = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  ALPHA = (0.0d0, 0.0d0)
  BETA = (0.0d0, 0.0d0)
  VL = (0.0d0, 0.0d0)
  VR = (0.0d0, 0.0d0)
  RCONDE = 0.0d0
  RCONDV = 0.0d0

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

  call ZGGEVX('B', 'V', 'V', 'E', n, A, lda, B, ldb, ALPHA, BETA, &
              VL, ldvl, VR, ldvr, ilo, ihi, LSCALE, RSCALE, &
              abnrm, bbnrm, RCONDE, RCONDV, WORK, lwork, RWORK, &
              IWORK, BWORK, info)

  call begin_test('sense_E_3x3')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_scalar('abnrm', abnrm)
  call print_scalar('bbnrm', bbnrm)
  call print_array('alpha', ALPHA_r(1), 2*n)
  call print_array('beta', BETA_r(1), 2*n)
  call print_array('rconde', RCONDE(1), n)
  call end_test()

  ! ==================================================================
  ! Test 8: 3x3 sense='V' (right-eigenvector condition numbers)
  ! ==================================================================
  n = 3
  A = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  ALPHA = (0.0d0, 0.0d0)
  BETA = (0.0d0, 0.0d0)
  VL = (0.0d0, 0.0d0)
  VR = (0.0d0, 0.0d0)
  RCONDE = 0.0d0
  RCONDV = 0.0d0

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

  call ZGGEVX('B', 'V', 'V', 'V', n, A, lda, B, ldb, ALPHA, BETA, &
              VL, ldvl, VR, ldvr, ilo, ihi, LSCALE, RSCALE, &
              abnrm, bbnrm, RCONDE, RCONDV, WORK, lwork, RWORK, &
              IWORK, BWORK, info)

  call begin_test('sense_V_3x3')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_scalar('abnrm', abnrm)
  call print_scalar('bbnrm', bbnrm)
  call print_array('alpha', ALPHA_r(1), 2*n)
  call print_array('beta', BETA_r(1), 2*n)
  call print_array('rcondv', RCONDV(1), n)
  call end_test()

  ! ==================================================================
  ! Test 9: 3x3 sense='B' (both eigenvalue and eigenvector cond nums)
  ! ==================================================================
  n = 3
  A = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  ALPHA = (0.0d0, 0.0d0)
  BETA = (0.0d0, 0.0d0)
  VL = (0.0d0, 0.0d0)
  VR = (0.0d0, 0.0d0)
  RCONDE = 0.0d0
  RCONDV = 0.0d0

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

  call ZGGEVX('B', 'V', 'V', 'B', n, A, lda, B, ldb, ALPHA, BETA, &
              VL, ldvl, VR, ldvr, ilo, ihi, LSCALE, RSCALE, &
              abnrm, bbnrm, RCONDE, RCONDV, WORK, lwork, RWORK, &
              IWORK, BWORK, info)

  call begin_test('sense_B_3x3')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_scalar('abnrm', abnrm)
  call print_scalar('bbnrm', bbnrm)
  call print_array('alpha', ALPHA_r(1), 2*n)
  call print_array('beta', BETA_r(1), 2*n)
  call print_array('rconde', RCONDE(1), n)
  call print_array('rcondv', RCONDV(1), n)
  call end_test()

end program
