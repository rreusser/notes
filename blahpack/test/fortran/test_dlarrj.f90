program test_dlarrj
  use test_utils
  implicit none

  integer, parameter :: NMAX = 10
  integer :: N, IFIRST, ILAST, OFFSET, INFO, I
  double precision :: RTOL, PIVMIN, SPDIAM
  double precision :: D(NMAX), E2(NMAX)
  double precision :: W(NMAX), WERR(NMAX)
  double precision :: WORK(2*NMAX*2), WORK2(2*NMAX*2)
  integer :: IWORK(2*NMAX*2), IWORK2(2*NMAX*2)
  double precision :: TWO, DLAMCH
  parameter(TWO = 2.0d0)
  external :: DLARRJ, DLAMCH

  ! ============================================================
  ! Test 1: Simple 4x4 diagonal matrix (eigenvalues = diagonal)
  ! ============================================================
  N = 4
  D(1) = 1.0d0
  D(2) = 3.0d0
  D(3) = 5.0d0
  D(4) = 7.0d0
  E2(1) = 0.0d0
  E2(2) = 0.0d0
  E2(3) = 0.0d0

  ! Eigenvalues are exactly 1, 3, 5, 7
  ! Provide approximate values with some error
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

  PIVMIN = DLAMCH('S')
  SPDIAM = 7.0d0 - 1.0d0
  RTOL = 1.0d-14

  call DLARRJ(N, D, E2, IFIRST, ILAST, RTOL, OFFSET, &
              W, WERR, WORK, IWORK, PIVMIN, SPDIAM, INFO)

  call begin_test('diagonal_4x4')
  call print_int('info', INFO)
  call print_array('w', W(1:4), 4)
  call print_array('werr', WERR(1:4), 4)
  call end_test()

  ! ============================================================
  ! Test 2: 3x3 tridiagonal with known eigenvalues
  ! T = [2, 1, 0; 1, 3, 1; 0, 1, 2]
  ! eigenvalues: 1, 2, 4 (roots of lambda^3 - 7*lambda^2 + 14*lambda - 8)
  ! ============================================================
  N = 3
  D(1) = 2.0d0
  D(2) = 3.0d0
  D(3) = 2.0d0
  E2(1) = 1.0d0
  E2(2) = 1.0d0

  IFIRST = 1
  ILAST = 3
  OFFSET = 0
  W(1) = 1.1d0
  W(2) = 2.1d0
  W(3) = 3.9d0
  WERR(1) = 0.5d0
  WERR(2) = 0.5d0
  WERR(3) = 0.5d0

  PIVMIN = DLAMCH('S')
  SPDIAM = 4.0d0 - 1.0d0
  RTOL = 1.0d-14

  call DLARRJ(N, D, E2, IFIRST, ILAST, RTOL, OFFSET, &
              W, WERR, WORK, IWORK, PIVMIN, SPDIAM, INFO)

  call begin_test('tridiag_3x3')
  call print_int('info', INFO)
  call print_array('w', W(1:3), 3)
  call print_array('werr', WERR(1:3), 3)
  call end_test()

  ! ============================================================
  ! Test 3: N=0 (quick return)
  ! ============================================================
  N = 0
  call DLARRJ(N, D, E2, 1, 0, RTOL, 0, &
              W, WERR, WORK, IWORK, PIVMIN, SPDIAM, INFO)

  call begin_test('n_zero')
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 4: N=1 single eigenvalue
  ! ============================================================
  N = 1
  D(1) = 5.0d0

  IFIRST = 1
  ILAST = 1
  OFFSET = 0
  W(1) = 5.1d0
  WERR(1) = 0.5d0
  PIVMIN = DLAMCH('S')
  SPDIAM = 1.0d0
  RTOL = 1.0d-14

  call DLARRJ(N, D, E2, IFIRST, ILAST, RTOL, OFFSET, &
              W, WERR, WORK, IWORK, PIVMIN, SPDIAM, INFO)

  call begin_test('n_one')
  call print_int('info', INFO)
  call print_array('w', W(1:1), 1)
  call print_array('werr', WERR(1:1), 1)
  call end_test()

  ! ============================================================
  ! Test 5: Subset refinement (IFIRST < ILAST with offset)
  ! Refine only eigenvalues 2 and 3 out of 4
  ! ============================================================
  N = 4
  D(1) = 1.0d0
  D(2) = 3.0d0
  D(3) = 5.0d0
  D(4) = 7.0d0
  E2(1) = 0.0d0
  E2(2) = 0.0d0
  E2(3) = 0.0d0

  IFIRST = 2
  ILAST = 3
  OFFSET = 0
  ! W and WERR are accessed as W(I-OFFSET), so W(2) and W(3)
  W(2) = 3.1d0
  W(3) = 4.9d0
  WERR(2) = 0.5d0
  WERR(3) = 0.5d0

  PIVMIN = DLAMCH('S')
  SPDIAM = 7.0d0 - 1.0d0
  RTOL = 1.0d-14

  call DLARRJ(N, D, E2, IFIRST, ILAST, RTOL, OFFSET, &
              W, WERR, WORK, IWORK, PIVMIN, SPDIAM, INFO)

  call begin_test('subset_refinement')
  call print_int('info', INFO)
  call print_array('w', W(2:3), 2)
  call print_array('werr', WERR(2:3), 2)
  call end_test()

  ! ============================================================
  ! Test 6: With nonzero offset
  ! IFIRST=3, ILAST=4, OFFSET=2
  ! W/WERR accessed as W(I-OFFSET) => W(1) and W(2)
  ! ============================================================
  N = 4
  D(1) = 1.0d0
  D(2) = 3.0d0
  D(3) = 5.0d0
  D(4) = 7.0d0
  E2(1) = 0.0d0
  E2(2) = 0.0d0
  E2(3) = 0.0d0

  IFIRST = 3
  ILAST = 4
  OFFSET = 2
  ! W(I-OFFSET): W(3-2)=W(1), W(4-2)=W(2)
  W(1) = 5.1d0
  W(2) = 6.9d0
  WERR(1) = 0.5d0
  WERR(2) = 0.5d0

  PIVMIN = DLAMCH('S')
  SPDIAM = 7.0d0 - 1.0d0
  RTOL = 1.0d-14

  call DLARRJ(N, D, E2, IFIRST, ILAST, RTOL, OFFSET, &
              W, WERR, WORK, IWORK, PIVMIN, SPDIAM, INFO)

  call begin_test('with_offset')
  call print_int('info', INFO)
  call print_array('w', W(1:2), 2)
  call print_array('werr', WERR(1:2), 2)
  call end_test()

  ! ============================================================
  ! Test 7: Already converged intervals (width < rtol*max(|left|,|right|))
  ! ============================================================
  N = 3
  D(1) = 2.0d0
  D(2) = 3.0d0
  D(3) = 2.0d0
  E2(1) = 1.0d0
  E2(2) = 1.0d0

  IFIRST = 1
  ILAST = 3
  OFFSET = 0
  ! Provide very tight initial approximations
  W(1) = 1.0d0
  W(2) = 2.0d0
  W(3) = 4.0d0
  WERR(1) = 1.0d-16
  WERR(2) = 1.0d-16
  WERR(3) = 1.0d-16

  PIVMIN = DLAMCH('S')
  SPDIAM = 3.0d0
  RTOL = 1.0d-14

  call DLARRJ(N, D, E2, IFIRST, ILAST, RTOL, OFFSET, &
              W, WERR, WORK, IWORK, PIVMIN, SPDIAM, INFO)

  call begin_test('already_converged')
  call print_int('info', INFO)
  call print_array('w', W(1:3), 3)
  call print_array('werr', WERR(1:3), 3)
  call end_test()

  ! ============================================================
  ! Test 8: Larger 5x5 tridiagonal
  ! T = [4, 1; 1, 3, 1; 0, 1, 2, 1; 0, 0, 1, 5, 1; 0, 0, 0, 1, 6]
  ! ============================================================
  N = 5
  D(1) = 4.0d0
  D(2) = 3.0d0
  D(3) = 2.0d0
  D(4) = 5.0d0
  D(5) = 6.0d0
  E2(1) = 1.0d0
  E2(2) = 1.0d0
  E2(3) = 1.0d0
  E2(4) = 1.0d0

  ! Provide rough approximations in the Gershgorin disc range
  IFIRST = 1
  ILAST = 5
  OFFSET = 0
  W(1) = 1.0d0
  W(2) = 2.5d0
  W(3) = 4.0d0
  W(4) = 5.5d0
  W(5) = 7.0d0
  WERR(1) = 1.0d0
  WERR(2) = 1.0d0
  WERR(3) = 1.0d0
  WERR(4) = 1.0d0
  WERR(5) = 1.0d0

  PIVMIN = DLAMCH('S')
  SPDIAM = 7.0d0 - 1.0d0
  RTOL = 1.0d-14

  call DLARRJ(N, D, E2, IFIRST, ILAST, RTOL, OFFSET, &
              W, WERR, WORK, IWORK, PIVMIN, SPDIAM, INFO)

  call begin_test('tridiag_5x5')
  call print_int('info', INFO)
  call print_array('w', W(1:5), 5)
  call print_array('werr', WERR(1:5), 5)
  call end_test()

  ! ============================================================
  ! Test 9: Coarse tolerance (RTOL = 1e-4)
  ! ============================================================
  N = 3
  D(1) = 2.0d0
  D(2) = 3.0d0
  D(3) = 2.0d0
  E2(1) = 1.0d0
  E2(2) = 1.0d0

  IFIRST = 1
  ILAST = 3
  OFFSET = 0
  W(1) = 1.1d0
  W(2) = 2.1d0
  W(3) = 3.9d0
  WERR(1) = 0.5d0
  WERR(2) = 0.5d0
  WERR(3) = 0.5d0

  PIVMIN = DLAMCH('S')
  SPDIAM = 3.0d0
  RTOL = 1.0d-4

  call DLARRJ(N, D, E2, IFIRST, ILAST, RTOL, OFFSET, &
              W, WERR, WORK, IWORK, PIVMIN, SPDIAM, INFO)

  call begin_test('coarse_rtol')
  call print_int('info', INFO)
  call print_array('w', W(1:3), 3)
  call print_array('werr', WERR(1:3), 3)
  call end_test()

end program
