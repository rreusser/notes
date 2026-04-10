program test_dlarrd
  use test_utils
  implicit none

  integer, parameter :: NMAX = 10
  integer :: N, NSPLIT, M, INFO, IL, IU, I
  double precision :: VL, VU, RELTOL, PIVMIN, WL, WU
  double precision :: D(NMAX), E(NMAX), E2(NMAX), GERS(2*NMAX)
  double precision :: W(NMAX), WERR(NMAX), WORK(4*NMAX)
  integer :: ISPLIT(NMAX), IBLOCK(NMAX), INDEXW(NMAX), IWORK(3*NMAX)

  double precision :: DLAMCH, SAFEMN, EPS, TMP1
  external :: DLARRD, DLAMCH

  SAFEMN = DLAMCH('S')
  EPS = DLAMCH('P')
  PIVMIN = SAFEMN
  RELTOL = EPS * 4.0d0

  ! Build 5x5 symmetric tridiagonal matrix
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
  E(5) = 0.0d0
  do I = 1, N
    E2(I) = E(I) * E(I)
  end do

  ! Single block
  NSPLIT = 1
  ISPLIT(1) = N

  ! Gershgorin intervals: GERS(2*i-1) = D(i) - sum|off-diag|, GERS(2*i) = D(i) + sum|off-diag|
  GERS(1) = D(1) - abs(E(1))
  GERS(2) = D(1) + abs(E(1))
  do I = 2, N - 1
    TMP1 = abs(E(I-1)) + abs(E(I))
    GERS(2*I - 1) = D(I) - TMP1
    GERS(2*I)     = D(I) + TMP1
  end do
  GERS(2*N - 1) = D(N) - abs(E(N-1))
  GERS(2*N)     = D(N) + abs(E(N-1))

  ! Test 1: RANGE='A' (all), ORDER='E' (entire, sorted)
  VL = 0.0d0
  VU = 0.0d0
  IL = 0
  IU = 0
  WL = 0.0d0
  WU = 0.0d0
  call DLARRD('A', 'E', N, VL, VU, IL, IU, GERS, RELTOL, D, E, E2, PIVMIN, &
               NSPLIT, ISPLIT, M, W, WERR, WL, WU, IBLOCK, INDEXW, WORK, IWORK, INFO)

  call begin_test('range_all_order_entire')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_array('w', W(1:M), M)
  call print_int_array('iblock', IBLOCK(1:M), M)
  call print_int_array('indexw', INDEXW(1:M), M)
  call end_test()

  ! Test 2: RANGE='I', IL=2, IU=4
  IL = 2
  IU = 4
  call DLARRD('I', 'E', N, VL, VU, IL, IU, GERS, RELTOL, D, E, E2, PIVMIN, &
               NSPLIT, ISPLIT, M, W, WERR, WL, WU, IBLOCK, INDEXW, WORK, IWORK, INFO)

  call begin_test('range_index_2_4')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_array('w', W(1:M), M)
  call print_scalar('wl', WL)
  call print_scalar('wu', WU)
  call end_test()

  ! Test 3: RANGE='V', VL=-2, VU=3
  VL = -2.0d0
  VU = 3.0d0
  call DLARRD('V', 'E', N, VL, VU, IL, IU, GERS, RELTOL, D, E, E2, PIVMIN, &
               NSPLIT, ISPLIT, M, W, WERR, WL, WU, IBLOCK, INDEXW, WORK, IWORK, INFO)

  call begin_test('range_value_neg2_3')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_array('w', W(1:M), M)
  call end_test()

  ! Test 4: N=1
  N = 1
  D(1) = 3.5d0
  NSPLIT = 1
  ISPLIT(1) = 1
  GERS(1) = D(1)
  GERS(2) = D(1)
  call DLARRD('A', 'E', N, 0.0d0, 0.0d0, 0, 0, GERS, RELTOL, D, E, E2, PIVMIN, &
               NSPLIT, ISPLIT, M, W, WERR, WL, WU, IBLOCK, INDEXW, WORK, IWORK, INFO)

  call begin_test('n1')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_array('w', W(1:M), M)
  call end_test()

  ! Test 5: Two blocks - use a bigger matrix with zero off-diag
  N = 6
  D(1) = 2.0d0
  D(2) = -1.0d0
  D(3) = 3.0d0
  D(4) = 0.5d0
  D(5) = 4.0d0
  D(6) = -2.0d0
  E(1) = 1.0d0
  E(2) = 1.0d0
  E(3) = 0.0d0   ! Splits here
  E(4) = 1.0d0
  E(5) = 1.0d0
  E(6) = 0.0d0
  do I = 1, N
    E2(I) = E(I) * E(I)
  end do
  NSPLIT = 2
  ISPLIT(1) = 3
  ISPLIT(2) = 6

  GERS(1) = D(1) - abs(E(1))
  GERS(2) = D(1) + abs(E(1))
  GERS(3) = D(2) - abs(E(1)) - abs(E(2))
  GERS(4) = D(2) + abs(E(1)) + abs(E(2))
  GERS(5) = D(3) - abs(E(2))
  GERS(6) = D(3) + abs(E(2))
  GERS(7) = D(4) - abs(E(4))
  GERS(8) = D(4) + abs(E(4))
  GERS(9) = D(5) - abs(E(4)) - abs(E(5))
  GERS(10) = D(5) + abs(E(4)) + abs(E(5))
  GERS(11) = D(6) - abs(E(5))
  GERS(12) = D(6) + abs(E(5))

  call DLARRD('A', 'E', N, 0.0d0, 0.0d0, 0, 0, GERS, RELTOL, D, E, E2, PIVMIN, &
               NSPLIT, ISPLIT, M, W, WERR, WL, WU, IBLOCK, INDEXW, WORK, IWORK, INFO)

  call begin_test('two_blocks_all')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_array('w', W(1:M), M)
  call print_int_array('iblock', IBLOCK(1:M), M)
  call end_test()

  ! Test 6: Two blocks, ORDER='B' (block ordering, not globally sorted)
  call DLARRD('A', 'B', N, 0.0d0, 0.0d0, 0, 0, GERS, RELTOL, D, E, E2, PIVMIN, &
               NSPLIT, ISPLIT, M, W, WERR, WL, WU, IBLOCK, INDEXW, WORK, IWORK, INFO)

  call begin_test('two_blocks_block_order')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_array('w', W(1:M), M)
  call print_int_array('iblock', IBLOCK(1:M), M)
  call end_test()

end program
