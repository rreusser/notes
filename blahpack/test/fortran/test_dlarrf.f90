program test_dlarrf
  use test_utils
  implicit none

  integer, parameter :: NMAX = 16
  integer :: N, CLSTRT, CLEND, INFO, I
  double precision :: SPDIAM, CLGAPL, CLGAPR, PIVMIN, SIGMA
  double precision :: D(NMAX), L(NMAX), LD(NMAX)
  double precision :: W(NMAX), WGAP(NMAX), WERR(NMAX)
  double precision :: DPLUS(NMAX), LPLUS(NMAX), WORK(4*NMAX)
  double precision :: DLAMCH
  external :: DLARRF, DLAMCH

  ! ============================================================
  ! Test 1: Simple 4x4 diagonal-ish tridiagonal
  ! Whole cluster (CLSTRT=1, CLEND=4)
  ! ============================================================
  N = 4
  D(1) = 4.0d0
  D(2) = 3.0d0
  D(3) = 2.0d0
  D(4) = 1.0d0
  L(1) = 0.1d0
  L(2) = 0.1d0
  L(3) = 0.1d0
  L(4) = 0.0d0
  do I = 1, N - 1
    LD(I) = L(I) * D(I)
  end do

  CLSTRT = 1
  CLEND = 4
  W(1) = 0.95d0
  W(2) = 1.95d0
  W(3) = 2.95d0
  W(4) = 4.05d0
  WGAP(1) = 0.9d0
  WGAP(2) = 0.9d0
  WGAP(3) = 1.0d0
  WGAP(4) = 0.0d0
  WERR(1) = 1.0d-3
  WERR(2) = 1.0d-3
  WERR(3) = 1.0d-3
  WERR(4) = 1.0d-3
  SPDIAM = 4.0d0
  CLGAPL = 1.0d0
  CLGAPR = 1.0d0
  PIVMIN = DLAMCH('S')
  SIGMA = 0.0d0

  call DLARRF(N, D, L, LD, CLSTRT, CLEND, W, WGAP, WERR, &
              SPDIAM, CLGAPL, CLGAPR, PIVMIN, SIGMA, &
              DPLUS, LPLUS, WORK, INFO)

  call begin_test('tridiag_4x4_full_cluster')
  call print_int('info', INFO)
  call print_scalar('sigma', SIGMA)
  call print_array('dplus', DPLUS(1:N), N)
  call print_array('lplus', LPLUS(1:N-1), N-1)
  call end_test()

  ! ============================================================
  ! Test 2: Subset cluster of larger tridiagonal (5x5)
  ! ============================================================
  N = 5
  D(1) = 5.0d0
  D(2) = 4.0d0
  D(3) = 3.0d0
  D(4) = 2.0d0
  D(5) = 1.0d0
  L(1) = 0.05d0
  L(2) = 0.05d0
  L(3) = 0.05d0
  L(4) = 0.05d0
  L(5) = 0.0d0
  do I = 1, N - 1
    LD(I) = L(I) * D(I)
  end do

  CLSTRT = 2
  CLEND = 4
  W(1) = 0.99d0
  W(2) = 1.99d0
  W(3) = 2.99d0
  W(4) = 3.99d0
  W(5) = 5.01d0
  WGAP(1) = 0.95d0
  WGAP(2) = 0.95d0
  WGAP(3) = 0.95d0
  WGAP(4) = 0.95d0
  WGAP(5) = 0.0d0
  WERR(1) = 1.0d-4
  WERR(2) = 1.0d-4
  WERR(3) = 1.0d-4
  WERR(4) = 1.0d-4
  WERR(5) = 1.0d-4
  SPDIAM = 5.0d0
  CLGAPL = 0.95d0
  CLGAPR = 0.95d0
  PIVMIN = DLAMCH('S')
  SIGMA = 0.0d0

  call DLARRF(N, D, L, LD, CLSTRT, CLEND, W, WGAP, WERR, &
              SPDIAM, CLGAPL, CLGAPR, PIVMIN, SIGMA, &
              DPLUS, LPLUS, WORK, INFO)

  call begin_test('tridiag_5x5_subset_cluster')
  call print_int('info', INFO)
  call print_scalar('sigma', SIGMA)
  call print_array('dplus', DPLUS(1:N), N)
  call print_array('lplus', LPLUS(1:N-1), N-1)
  call end_test()

  ! ============================================================
  ! Test 3: N=1 (quick path, single eigenvalue)
  ! ============================================================
  N = 1
  D(1) = 3.0d0
  L(1) = 0.0d0
  LD(1) = 0.0d0
  CLSTRT = 1
  CLEND = 1
  W(1) = 3.0d0
  WGAP(1) = 0.0d0
  WERR(1) = 1.0d-6
  SPDIAM = 1.0d0
  CLGAPL = 1.0d0
  CLGAPR = 1.0d0
  PIVMIN = DLAMCH('S')
  SIGMA = 0.0d0

  ! Note: dlarrf with CLEND==CLSTRT divides by zero in AVGAP — skip
  ! Use n=2 instead.

  ! ============================================================
  ! Test 3 (real): N=2 simple
  ! ============================================================
  N = 2
  D(1) = 2.0d0
  D(2) = 4.0d0
  L(1) = 0.1d0
  L(2) = 0.0d0
  LD(1) = L(1) * D(1)
  CLSTRT = 1
  CLEND = 2
  W(1) = 1.99d0
  W(2) = 4.01d0
  WGAP(1) = 1.9d0
  WGAP(2) = 0.0d0
  WERR(1) = 1.0d-5
  WERR(2) = 1.0d-5
  SPDIAM = 2.0d0
  CLGAPL = 1.0d0
  CLGAPR = 1.0d0
  PIVMIN = DLAMCH('S')
  SIGMA = 0.0d0

  call DLARRF(N, D, L, LD, CLSTRT, CLEND, W, WGAP, WERR, &
              SPDIAM, CLGAPL, CLGAPR, PIVMIN, SIGMA, &
              DPLUS, LPLUS, WORK, INFO)

  call begin_test('tridiag_2x2')
  call print_int('info', INFO)
  call print_scalar('sigma', SIGMA)
  call print_array('dplus', DPLUS(1:N), N)
  call print_array('lplus', LPLUS(1:N-1), N-1)
  call end_test()

  ! ============================================================
  ! Test 4: Tight cluster (small clwdth, may take RRR1/RRR2 path or right shift)
  ! ============================================================
  N = 6
  D(1) = 6.0d0
  D(2) = 5.0d0
  D(3) = 2.0d0
  D(4) = 2.000001d0
  D(5) = 2.000002d0
  D(6) = 1.0d0
  L(1) = 0.01d0
  L(2) = 0.01d0
  L(3) = 0.01d0
  L(4) = 0.01d0
  L(5) = 0.01d0
  L(6) = 0.0d0
  do I = 1, N - 1
    LD(I) = L(I) * D(I)
  end do

  CLSTRT = 3
  CLEND = 5
  W(1) = 0.95d0
  W(2) = 1.999998d0
  W(3) = 1.999999d0
  W(4) = 2.000000d0
  W(5) = 2.000001d0
  W(6) = 6.05d0
  WGAP(1) = 1.0d-9
  WGAP(2) = 1.0d-9
  WGAP(3) = 1.0d-9
  WGAP(4) = 1.0d-9
  WGAP(5) = 4.0d0
  WGAP(6) = 0.0d0
  WERR(1) = 1.0d-12
  WERR(2) = 1.0d-12
  WERR(3) = 1.0d-12
  WERR(4) = 1.0d-12
  WERR(5) = 1.0d-12
  WERR(6) = 1.0d-12
  SPDIAM = 6.0d0
  CLGAPL = 1.0d0
  CLGAPR = 4.0d0
  PIVMIN = DLAMCH('S')
  SIGMA = 0.0d0

  call DLARRF(N, D, L, LD, CLSTRT, CLEND, W, WGAP, WERR, &
              SPDIAM, CLGAPL, CLGAPR, PIVMIN, SIGMA, &
              DPLUS, LPLUS, WORK, INFO)

  call begin_test('tight_cluster_6x6')
  call print_int('info', INFO)
  call print_scalar('sigma', SIGMA)
  call print_array('dplus', DPLUS(1:N), N)
  call print_array('lplus', LPLUS(1:N-1), N-1)
  call end_test()

  ! ============================================================
  ! Test 5: 3x3 with widely spread eigenvalues
  ! ============================================================
  N = 3
  D(1) = 10.0d0
  D(2) = 5.0d0
  D(3) = 1.0d0
  L(1) = 0.2d0
  L(2) = 0.2d0
  L(3) = 0.0d0
  LD(1) = L(1) * D(1)
  LD(2) = L(2) * D(2)
  CLSTRT = 1
  CLEND = 3
  W(1) = 0.9d0
  W(2) = 4.9d0
  W(3) = 10.1d0
  WGAP(1) = 3.9d0
  WGAP(2) = 5.0d0
  WGAP(3) = 0.0d0
  WERR(1) = 1.0d-4
  WERR(2) = 1.0d-4
  WERR(3) = 1.0d-4
  SPDIAM = 10.0d0
  CLGAPL = 2.0d0
  CLGAPR = 2.0d0
  PIVMIN = DLAMCH('S')
  SIGMA = 0.0d0

  call DLARRF(N, D, L, LD, CLSTRT, CLEND, W, WGAP, WERR, &
              SPDIAM, CLGAPL, CLGAPR, PIVMIN, SIGMA, &
              DPLUS, LPLUS, WORK, INFO)

  call begin_test('tridiag_3x3_wide')
  call print_int('info', INFO)
  call print_scalar('sigma', SIGMA)
  call print_array('dplus', DPLUS(1:N), N)
  call print_array('lplus', LPLUS(1:N-1), N-1)
  call end_test()

  ! ============================================================
  ! Test 6: N=0 (quick return)
  ! ============================================================
  N = 0
  SIGMA = 1.5d0
  call DLARRF(N, D, L, LD, 1, 1, W, WGAP, WERR, &
              SPDIAM, CLGAPL, CLGAPR, PIVMIN, SIGMA, &
              DPLUS, LPLUS, WORK, INFO)

  call begin_test('n_zero')
  call print_int('info', INFO)
  call print_scalar('sigma', SIGMA)
  call end_test()

end program
