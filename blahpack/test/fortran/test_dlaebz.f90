program test_dlaebz
  use test_utils
  implicit none

  integer, parameter :: NMAX = 10, MMAX = 40
  integer :: IJOB, NITMAX, N, MINP, NBMIN, MOUT, INFO, IOUT
  double precision :: ABSTOL, RELTOL, PIVMIN
  double precision :: D(NMAX), E(NMAX), E2(NMAX)
  integer :: NVAL(MMAX), NAB(MMAX, 2)
  double precision :: AB(MMAX, 2), C(MMAX), WORK(MMAX)
  integer :: IWORK(MMAX)
  integer :: IM, I

  double precision :: DLAMCH, SAFEMN, ULP, GL, GU, TNORM, TMP1, TMP2
  double precision :: TWO
  parameter(TWO = 2.0d0)
  external :: DLAEBZ, DLAMCH

  SAFEMN = DLAMCH('S')
  ULP = DLAMCH('P')
  PIVMIN = SAFEMN

  ! ============================================================
  ! Test 1: IJOB=1, simple 5x5 tridiagonal, single interval
  ! ============================================================
  N = 5
  D(1) = 2.0d0
  D(2) = -1.0d0
  D(3) = 3.0d0
  D(4) = 0.5d0
  D(5) = 4.0d0
  E(1) = 1.0d0
  E(2) = 1.0d0
  E(3) = 1.0d0
  E(4) = 1.0d0
  E2(1) = E(1)**2
  E2(2) = E(2)**2
  E2(3) = E(3)**2
  E2(4) = E(4)**2

  MINP = 1
  AB(1, 1) = -10.0d0
  AB(1, 2) = 10.0d0

  call DLAEBZ(1, 0, N, MMAX, MINP, 0, 0.0d0, 0.0d0, PIVMIN, &
               D, E, E2, NVAL, AB, C, MOUT, NAB, WORK, IWORK, INFO)

  call begin_test('ijob1_all')
  call print_int('info', INFO)
  call print_int('mout', MOUT)
  call print_int('nab11', NAB(1,1))
  call print_int('nab12', NAB(1,2))
  call end_test()

  ! ============================================================
  ! Test 2: IJOB=1, two intervals
  ! ============================================================
  MINP = 2
  AB(1, 1) = -10.0d0
  AB(1, 2) = 1.0d0
  AB(2, 1) = 1.0d0
  AB(2, 2) = 10.0d0

  call DLAEBZ(1, 0, N, MMAX, MINP, 0, 0.0d0, 0.0d0, PIVMIN, &
               D, E, E2, NVAL, AB, C, MOUT, NAB, WORK, IWORK, INFO)

  call begin_test('ijob1_two_intervals')
  call print_int('info', INFO)
  call print_int('mout', MOUT)
  call print_int('nab11', NAB(1,1))
  call print_int('nab12', NAB(1,2))
  call print_int('nab21', NAB(2,1))
  call print_int('nab22', NAB(2,2))
  call end_test()

  ! ============================================================
  ! Test 3: Full IJOB=1 then IJOB=2 cycle (mimicking dstebz)
  ! Use MINP=1 for IJOB=2 (matching dstebz convention)
  ! ============================================================
  N = 5
  ABSTOL = 2.0d0 * SAFEMN
  RELTOL = ULP * 2.0d0

  ! Compute Gershgorin interval
  GL = D(1) - abs(E(1))
  GU = D(1) + abs(E(1))
  do I = 2, N-1
    TMP1 = abs(E(I-1)) + abs(E(I))
    GL = min(GL, D(I) - TMP1)
    GU = max(GU, D(I) + TMP1)
  end do
  GL = min(GL, D(N) - abs(E(N-1)))
  GU = max(GU, D(N) + abs(E(N-1)))
  TNORM = max(abs(GL), abs(GU))
  GL = GL - 2.1d0 * TNORM * ULP * N - 2.1d0 * 2.0d0 * PIVMIN
  GU = GU + 2.1d0 * TNORM * ULP * N + 2.1d0 * PIVMIN

  MINP = 1
  AB(1, 1) = GL
  AB(1, 2) = GU

  ! IJOB=1: count eigenvalues
  call DLAEBZ(1, 0, N, MMAX, MINP, 0, ABSTOL, RELTOL, PIVMIN, &
               D, E, E2, NVAL, AB, C, MOUT, NAB, WORK, IWORK, INFO)

  IM = MOUT
  NITMAX = int((log(GU-GL+PIVMIN) - log(PIVMIN)) / log(TWO)) + 2

  ! IJOB=2: bisect (MINP=1, not IM - this is how dstebz calls it)
  call DLAEBZ(2, NITMAX, N, MMAX, MINP, 0, ABSTOL, RELTOL, PIVMIN, &
               D, E, E2, NVAL, AB, C, IOUT, NAB, WORK, IWORK, INFO)

  call begin_test('ijob12_full_cycle')
  call print_int('info', INFO)
  call print_int('im', IM)
  call print_int('iout', IOUT)
  ! Print converged eigenvalue midpoints
  do I = 1, IOUT
    C(I) = 0.5d0 * (AB(I, 1) + AB(I, 2))
  end do
  call print_array('eigenvalues', C(1:IOUT), IOUT)
  call print_int_array('nab1', NAB(1:IOUT, 1), IOUT)
  call print_int_array('nab2', NAB(1:IOUT, 2), IOUT)
  call end_test()

  ! ============================================================
  ! Test 4: IJOB=3 (binary search)
  ! ============================================================
  ! Recompute Gershgorin
  GL = D(1) - abs(E(1))
  GU = D(1) + abs(E(1))
  do I = 2, N-1
    TMP1 = abs(E(I-1)) + abs(E(I))
    GL = min(GL, D(I) - TMP1)
    GU = max(GU, D(I) + TMP1)
  end do
  GL = min(GL, D(N) - abs(E(N-1)))
  GU = max(GU, D(N) + abs(E(N-1)))
  TNORM = max(abs(GL), abs(GU))
  GL = GL - 2.1d0 * TNORM * ULP * N - 2.1d0 * 2.0d0 * PIVMIN
  GU = GU + 2.1d0 * TNORM * ULP * N + 2.1d0 * PIVMIN

  MINP = 2
  AB(1, 1) = GL
  AB(1, 2) = GU
  AB(2, 1) = GL
  AB(2, 2) = GU
  NAB(1, 1) = -1
  NAB(1, 2) = N + 1
  NAB(2, 1) = -1
  NAB(2, 2) = N + 1
  NVAL(1) = 2
  NVAL(2) = 4
  C(1) = GL
  C(2) = GU

  NITMAX = int((log(TNORM+PIVMIN) - log(PIVMIN)) / log(TWO)) + 2

  call DLAEBZ(3, NITMAX, N, MMAX, MINP, 0, ABSTOL, RELTOL, PIVMIN, &
               D, E, E2, NVAL, AB, C, MOUT, NAB, WORK, IWORK, INFO)

  call begin_test('ijob3_search')
  call print_int('info', INFO)
  call print_int('mout', MOUT)
  call print_array('ab1', AB(1:MOUT, 1), MOUT)
  call print_array('ab2', AB(1:MOUT, 2), MOUT)
  call print_int_array('nab1', NAB(1:MOUT, 1), MOUT)
  call print_int_array('nab2', NAB(1:MOUT, 2), MOUT)
  call print_int_array('nval', NVAL(1:MOUT), MOUT)
  call end_test()

  ! ============================================================
  ! Test 5: Invalid IJOB
  ! ============================================================
  call DLAEBZ(0, 0, N, MMAX, 1, 0, 0.0d0, 0.0d0, PIVMIN, &
               D, E, E2, NVAL, AB, C, MOUT, NAB, WORK, IWORK, INFO)

  call begin_test('invalid_ijob')
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 6: N=1
  ! ============================================================
  N = 1
  D(1) = 3.0d0
  MINP = 1
  AB(1, 1) = 0.0d0
  AB(1, 2) = 10.0d0

  call DLAEBZ(1, 0, N, MMAX, MINP, 0, 0.0d0, 0.0d0, PIVMIN, &
               D, E, E2, NVAL, AB, C, MOUT, NAB, WORK, IWORK, INFO)

  call begin_test('ijob1_n1')
  call print_int('info', INFO)
  call print_int('mout', MOUT)
  call print_int('nab11', NAB(1,1))
  call print_int('nab12', NAB(1,2))
  call end_test()

  ! ============================================================
  ! Test 7: IJOB=2 with nbmin>0 (parallel path), MINP=1
  ! ============================================================
  N = 5
  D(1) = 2.0d0
  D(2) = -1.0d0
  D(3) = 3.0d0
  D(4) = 0.5d0
  D(5) = 4.0d0

  GL = D(1) - abs(E(1))
  GU = D(1) + abs(E(1))
  do I = 2, N-1
    TMP1 = abs(E(I-1)) + abs(E(I))
    GL = min(GL, D(I) - TMP1)
    GU = max(GU, D(I) + TMP1)
  end do
  GL = min(GL, D(N) - abs(E(N-1)))
  GU = max(GU, D(N) + abs(E(N-1)))
  TNORM = max(abs(GL), abs(GU))
  GL = GL - 2.1d0 * TNORM * ULP * N - 2.1d0 * 2.0d0 * PIVMIN
  GU = GU + 2.1d0 * TNORM * ULP * N + 2.1d0 * PIVMIN

  MINP = 1
  AB(1, 1) = GL
  AB(1, 2) = GU

  call DLAEBZ(1, 0, N, MMAX, MINP, 0, ABSTOL, RELTOL, PIVMIN, &
               D, E, E2, NVAL, AB, C, MOUT, NAB, WORK, IWORK, INFO)

  NITMAX = int((log(GU-GL+PIVMIN) - log(PIVMIN)) / log(TWO)) + 2

  ! Use NBMIN=2 to trigger parallel path, MINP=1
  call DLAEBZ(2, NITMAX, N, MMAX, MINP, 2, ABSTOL, RELTOL, PIVMIN, &
               D, E, E2, NVAL, AB, C, IOUT, NAB, WORK, IWORK, INFO)

  call begin_test('ijob2_parallel')
  call print_int('info', INFO)
  call print_int('iout', IOUT)
  do I = 1, IOUT
    C(I) = 0.5d0 * (AB(I, 1) + AB(I, 2))
  end do
  call print_array('eigenvalues', C(1:IOUT), IOUT)
  call end_test()

  ! ============================================================
  ! Test 8: IJOB=3 with parallel path (nbmin>0)
  ! ============================================================
  N = 5
  GL = D(1) - abs(E(1))
  GU = D(1) + abs(E(1))
  do I = 2, N-1
    TMP1 = abs(E(I-1)) + abs(E(I))
    GL = min(GL, D(I) - TMP1)
    GU = max(GU, D(I) + TMP1)
  end do
  GL = min(GL, D(N) - abs(E(N-1)))
  GU = max(GU, D(N) + abs(E(N-1)))
  TNORM = max(abs(GL), abs(GU))
  GL = GL - 2.1d0 * TNORM * ULP * N - 2.1d0 * 2.0d0 * PIVMIN
  GU = GU + 2.1d0 * TNORM * ULP * N + 2.1d0 * PIVMIN

  MINP = 2
  AB(1, 1) = GL
  AB(1, 2) = GU
  AB(2, 1) = GL
  AB(2, 2) = GU
  NAB(1, 1) = -1
  NAB(1, 2) = N + 1
  NAB(2, 1) = -1
  NAB(2, 2) = N + 1
  NVAL(1) = 2
  NVAL(2) = 4
  C(1) = GL
  C(2) = GU

  NITMAX = int((log(TNORM+PIVMIN) - log(PIVMIN)) / log(TWO)) + 2

  ! Use NBMIN=1 to trigger parallel path
  call DLAEBZ(3, NITMAX, N, MMAX, MINP, 1, ABSTOL, RELTOL, PIVMIN, &
               D, E, E2, NVAL, AB, C, MOUT, NAB, WORK, IWORK, INFO)

  call begin_test('ijob3_parallel')
  call print_int('info', INFO)
  call print_int('mout', MOUT)
  call print_array('ab1', AB(1:MOUT, 1), MOUT)
  call print_array('ab2', AB(1:MOUT, 2), MOUT)
  call print_int_array('nab1', NAB(1:MOUT, 1), MOUT)
  call print_int_array('nab2', NAB(1:MOUT, 2), MOUT)
  call print_int_array('nval', NVAL(1:MOUT), MOUT)
  call end_test()

end program
