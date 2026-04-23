program test_dlarrb
  use test_utils
  implicit none

  integer, parameter :: NMAX = 10
  integer :: N, IFIRST, ILAST, OFFSET, INFO, TWIST
  double precision :: RTOL1, RTOL2, PIVMIN, SPDIAM
  double precision :: D(NMAX), LLD(NMAX)
  double precision :: W(NMAX), WERR(NMAX), WGAP(NMAX)
  double precision :: WORK(4*NMAX)
  integer :: IWORK(2*NMAX)
  double precision :: DLAMCH
  external :: DLARRB, DLAMCH

  ! ============================================================
  ! Test 1: Simple 4x4 diagonal matrix (LLD all zero)
  ! eigenvalues = diagonal = 1, 3, 5, 7
  ! ============================================================
  N = 4
  D(1) = 1.0d0
  D(2) = 3.0d0
  D(3) = 5.0d0
  D(4) = 7.0d0
  LLD(1) = 0.0d0
  LLD(2) = 0.0d0
  LLD(3) = 0.0d0

  IFIRST = 1
  ILAST = 4
  OFFSET = 0
  W(1) = 1.1d0
  W(2) = 2.9d0
  W(3) = 5.2d0
  W(4) = 6.8d0
  WERR(1) = 0.5d0
  WERR(2) = 0.5d0
  WERR(3) = 0.5d0
  WERR(4) = 0.5d0
  WGAP(1) = 1.5d0
  WGAP(2) = 1.5d0
  WGAP(3) = 1.5d0
  WGAP(4) = 0.0d0

  PIVMIN = DLAMCH('S')
  SPDIAM = 6.0d0
  RTOL1 = 1.0d-8
  RTOL2 = 1.0d-14
  TWIST = -1

  call DLARRB(N, D, LLD, IFIRST, ILAST, RTOL1, RTOL2, OFFSET, &
              W, WGAP, WERR, WORK, IWORK, PIVMIN, SPDIAM, TWIST, INFO)

  call begin_test('diagonal_4x4')
  call print_int('info', INFO)
  call print_array('w', W(1:4), 4)
  call print_array('werr', WERR(1:4), 4)
  call print_array('wgap', WGAP(1:3), 3)
  call end_test()

  ! ============================================================
  ! Test 2: 3x3 tridiagonal T = [2,1;1,3,1;1,2]
  ! LDL^T factorization: d1=2, L1=0.5, d2=3-0.5=2.5, L2=0.4, d3=2-0.4=1.6
  ! LLD(i) = L(i)^2 * d(i); LLD(1)=0.25*2=0.5, LLD(2)=0.16*2.5=0.4
  ! ============================================================
  N = 3
  D(1) = 2.0d0
  D(2) = 2.5d0
  D(3) = 1.6d0
  LLD(1) = 0.5d0
  LLD(2) = 0.4d0

  IFIRST = 1
  ILAST = 3
  OFFSET = 0
  W(1) = 1.1d0
  W(2) = 2.1d0
  W(3) = 3.9d0
  WERR(1) = 0.5d0
  WERR(2) = 0.5d0
  WERR(3) = 0.5d0
  WGAP(1) = 0.8d0
  WGAP(2) = 1.5d0
  WGAP(3) = 0.0d0

  PIVMIN = DLAMCH('S')
  SPDIAM = 3.0d0
  RTOL1 = 1.0d-8
  RTOL2 = 1.0d-14
  TWIST = -1

  call DLARRB(N, D, LLD, IFIRST, ILAST, RTOL1, RTOL2, OFFSET, &
              W, WGAP, WERR, WORK, IWORK, PIVMIN, SPDIAM, TWIST, INFO)

  call begin_test('tridiag_3x3')
  call print_int('info', INFO)
  call print_array('w', W(1:3), 3)
  call print_array('werr', WERR(1:3), 3)
  call print_array('wgap', WGAP(1:2), 2)
  call end_test()

  ! ============================================================
  ! Test 3: N=1 single eigenvalue
  ! ============================================================
  N = 1
  D(1) = 5.0d0
  LLD(1) = 0.0d0

  IFIRST = 1
  ILAST = 1
  OFFSET = 0
  W(1) = 5.1d0
  WERR(1) = 0.5d0
  WGAP(1) = 0.0d0
  PIVMIN = DLAMCH('S')
  SPDIAM = 1.0d0
  RTOL1 = 1.0d-8
  RTOL2 = 1.0d-14
  TWIST = -1

  call DLARRB(N, D, LLD, IFIRST, ILAST, RTOL1, RTOL2, OFFSET, &
              W, WGAP, WERR, WORK, IWORK, PIVMIN, SPDIAM, TWIST, INFO)

  call begin_test('n_one')
  call print_int('info', INFO)
  call print_array('w', W(1:1), 1)
  call print_array('werr', WERR(1:1), 1)
  call end_test()

  ! ============================================================
  ! Test 4: N=0 (quick return)
  ! ============================================================
  N = 0
  call DLARRB(N, D, LLD, 1, 0, RTOL1, RTOL2, 0, &
              W, WGAP, WERR, WORK, IWORK, PIVMIN, SPDIAM, TWIST, INFO)

  call begin_test('n_zero')
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 5: Subset (IFIRST=2, ILAST=3 out of N=4)
  ! ============================================================
  N = 4
  D(1) = 1.0d0
  D(2) = 3.0d0
  D(3) = 5.0d0
  D(4) = 7.0d0
  LLD(1) = 0.0d0
  LLD(2) = 0.0d0
  LLD(3) = 0.0d0

  IFIRST = 2
  ILAST = 3
  OFFSET = 0
  W(2) = 3.1d0
  W(3) = 4.9d0
  WERR(2) = 0.5d0
  WERR(3) = 0.5d0
  WGAP(2) = 1.5d0
  WGAP(3) = 1.5d0

  PIVMIN = DLAMCH('S')
  SPDIAM = 6.0d0
  RTOL1 = 1.0d-8
  RTOL2 = 1.0d-14
  TWIST = -1

  call DLARRB(N, D, LLD, IFIRST, ILAST, RTOL1, RTOL2, OFFSET, &
              W, WGAP, WERR, WORK, IWORK, PIVMIN, SPDIAM, TWIST, INFO)

  call begin_test('subset')
  call print_int('info', INFO)
  call print_array('w', W(2:3), 2)
  call print_array('werr', WERR(2:3), 2)
  call end_test()

  ! ============================================================
  ! Test 6: Nonzero offset (IFIRST=3, ILAST=4, OFFSET=2)
  ! ============================================================
  N = 4
  D(1) = 1.0d0
  D(2) = 3.0d0
  D(3) = 5.0d0
  D(4) = 7.0d0
  LLD(1) = 0.0d0
  LLD(2) = 0.0d0
  LLD(3) = 0.0d0

  IFIRST = 3
  ILAST = 4
  OFFSET = 2
  W(1) = 5.1d0
  W(2) = 6.9d0
  WERR(1) = 0.5d0
  WERR(2) = 0.5d0
  WGAP(1) = 1.5d0
  WGAP(2) = 0.0d0

  PIVMIN = DLAMCH('S')
  SPDIAM = 6.0d0
  RTOL1 = 1.0d-8
  RTOL2 = 1.0d-14
  TWIST = -1

  call DLARRB(N, D, LLD, IFIRST, ILAST, RTOL1, RTOL2, OFFSET, &
              W, WGAP, WERR, WORK, IWORK, PIVMIN, SPDIAM, TWIST, INFO)

  call begin_test('with_offset')
  call print_int('info', INFO)
  call print_array('w', W(1:2), 2)
  call print_array('werr', WERR(1:2), 2)
  call end_test()

  ! ============================================================
  ! Test 7: Twist index set (TWIST=2)
  ! ============================================================
  N = 3
  D(1) = 2.0d0
  D(2) = 2.5d0
  D(3) = 1.6d0
  LLD(1) = 0.5d0
  LLD(2) = 0.4d0

  IFIRST = 1
  ILAST = 3
  OFFSET = 0
  W(1) = 1.1d0
  W(2) = 2.1d0
  W(3) = 3.9d0
  WERR(1) = 0.5d0
  WERR(2) = 0.5d0
  WERR(3) = 0.5d0
  WGAP(1) = 0.8d0
  WGAP(2) = 1.5d0

  PIVMIN = DLAMCH('S')
  SPDIAM = 3.0d0
  RTOL1 = 1.0d-8
  RTOL2 = 1.0d-14
  TWIST = 2

  call DLARRB(N, D, LLD, IFIRST, ILAST, RTOL1, RTOL2, OFFSET, &
              W, WGAP, WERR, WORK, IWORK, PIVMIN, SPDIAM, TWIST, INFO)

  call begin_test('twist_set')
  call print_int('info', INFO)
  call print_array('w', W(1:3), 3)
  call print_array('werr', WERR(1:3), 3)
  call end_test()

  ! ============================================================
  ! Test 8: 5x5 tridiagonal with wider RTOL1
  ! Use diagonal matrix LLD=0, D=[4,3,2,5,6]
  ! ============================================================
  N = 5
  D(1) = 4.0d0
  D(2) = 3.0d0
  D(3) = 2.0d0
  D(4) = 5.0d0
  D(5) = 6.0d0
  LLD(1) = 0.0d0
  LLD(2) = 0.0d0
  LLD(3) = 0.0d0
  LLD(4) = 0.0d0

  IFIRST = 1
  ILAST = 5
  OFFSET = 0
  W(1) = 1.8d0
  W(2) = 3.2d0
  W(3) = 4.1d0
  W(4) = 4.9d0
  W(5) = 6.2d0
  WERR(1) = 0.5d0
  WERR(2) = 0.5d0
  WERR(3) = 0.5d0
  WERR(4) = 0.5d0
  WERR(5) = 0.5d0
  WGAP(1) = 0.8d0
  WGAP(2) = 0.8d0
  WGAP(3) = 0.8d0
  WGAP(4) = 0.8d0

  PIVMIN = DLAMCH('S')
  SPDIAM = 4.0d0
  RTOL1 = 1.0d-4
  RTOL2 = 1.0d-14
  TWIST = -1

  call DLARRB(N, D, LLD, IFIRST, ILAST, RTOL1, RTOL2, OFFSET, &
              W, WGAP, WERR, WORK, IWORK, PIVMIN, SPDIAM, TWIST, INFO)

  call begin_test('tridiag_5x5_coarse')
  call print_int('info', INFO)
  call print_array('w', W(1:5), 5)
  call print_array('werr', WERR(1:5), 5)
  call print_array('wgap', WGAP(1:4), 4)
  call end_test()

end program
