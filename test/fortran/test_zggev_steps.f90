program test_zggev_steps
  use test_utils
  implicit none

  integer, parameter :: NMAX = 3
  complex*16 :: A(NMAX, NMAX), B(NMAX, NMAX), Asave(NMAX, NMAX), Bsave(NMAX, NMAX)
  complex*16 :: ALPHA(NMAX), BETA(NMAX)
  complex*16 :: TAU(NMAX)
  complex*16 :: WORK(8*NMAX)
  double precision :: RWORK(8*NMAX)
  double precision :: A_r(2*NMAX*NMAX), B_r(2*NMAX*NMAX)
  double precision :: ALPHA_r(2*NMAX), BETA_r(2*NMAX), TAU_r(2*NMAX)
  equivalence (A, A_r)
  equivalence (B, B_r)
  equivalence (ALPHA, ALPHA_r)
  equivalence (BETA, BETA_r)
  equivalence (TAU, TAU_r)
  integer :: info, n, lda, ldb, ilo, ihi, irows, icols, lwork

  n = 3
  lda = NMAX
  ldb = NMAX
  lwork = 8*NMAX

  ! Matrix A
  A(1,1) = (2.0d0, 1.0d0)
  A(2,1) = (1.0d0, -1.0d0)
  A(3,1) = (0.5d0, 0.5d0)
  A(1,2) = (1.0d0, 0.5d0)
  A(2,2) = (3.0d0, 0.0d0)
  A(3,2) = (0.5d0, -0.5d0)
  A(1,3) = (0.5d0, -0.5d0)
  A(2,3) = (1.0d0, 1.0d0)
  A(3,3) = (4.0d0, -1.0d0)

  ! Matrix B
  B(1,1) = (3.0d0, 0.0d0)
  B(2,1) = (0.5d0, -0.5d0)
  B(3,1) = (0.0d0, 0.5d0)
  B(1,2) = (1.0d0, 0.5d0)
  B(2,2) = (2.0d0, 1.0d0)
  B(3,2) = (0.5d0, 0.0d0)
  B(1,3) = (0.5d0, 0.5d0)
  B(2,3) = (1.0d0, 0.0d0)
  B(3,3) = (1.0d0, 0.5d0)

  ! Step 1: Balance
  call ZGGBAL('P', n, A, lda, B, ldb, ilo, ihi, &
              RWORK(1), RWORK(n+1), RWORK(2*n+1), info)

  call begin_test('after_balance')
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_array('A', A_r(1), 2*n*n)
  call print_array('B', B_r(1), 2*n*n)
  call end_test()

  ! Step 2: QR factorize B
  irows = ihi + 1 - ilo
  icols = irows  ! eigenvalues only
  call ZGEQRF(irows, icols, B(ilo, ilo), ldb, TAU, WORK, lwork, info)

  call begin_test('after_qr')
  call print_array('B', B_r(1), 2*n*n)
  call print_array('TAU', TAU_r(1), 2*irows)
  call end_test()

  ! Step 3: Apply Q^H to A
  call ZUNMQR('L', 'C', irows, icols, irows, B(ilo, ilo), ldb, TAU, &
              A(ilo, ilo), lda, WORK, lwork, info)

  call begin_test('after_unmqr')
  call print_array('A', A_r(1), 2*n*n)
  call end_test()

  ! Step 4: Reduce to Hessenberg
  call ZGGHRD('N', 'N', irows, 1, irows, A(ilo, ilo), lda, B(ilo, ilo), ldb, &
              A, lda, A, lda, info)

  call begin_test('after_hessenberg')
  call print_array('A', A_r(1), 2*n*n)
  call print_array('B', B_r(1), 2*n*n)
  call end_test()

  ! Step 5: QZ
  call ZHGEQZ('E', 'N', 'N', n, ilo, ihi, A, lda, B, ldb, &
              ALPHA, BETA, A, lda, A, lda, WORK, lwork, RWORK(2*n+1), info)

  call begin_test('after_qz')
  call print_int('info', info)
  call print_array('alpha', ALPHA_r(1), 2*n)
  call print_array('beta', BETA_r(1), 2*n)
  call end_test()

end program
