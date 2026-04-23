program test_dstebz
  use test_utils
  implicit none

  integer, parameter :: NMAX = 20
  integer :: N, IL, IU, M, NSPLIT, INFO
  double precision :: VL, VU, ABSTOL
  double precision :: D(NMAX), E(NMAX), W(NMAX)
  integer :: IBLOCK(NMAX), ISPLIT(NMAX), IWORK(3*NMAX)
  double precision :: WORK(4*NMAX)

  double precision :: DLAMCH, SAFEMN
  external :: DSTEBZ, DLAMCH

  SAFEMN = DLAMCH('S')
  ABSTOL = 2.0d0 * SAFEMN

  ! ============================================================
  ! Test 1: N=0 quick return
  ! ============================================================
  N = 0
  call DSTEBZ('A', 'B', N, 0.0d0, 0.0d0, 1, 0, ABSTOL, &
               D, E, M, NSPLIT, W, IBLOCK, ISPLIT, WORK, IWORK, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call print_int('m', M)
  call end_test()

  ! ============================================================
  ! Test 2: N=1
  ! ============================================================
  N = 1
  D(1) = 5.0d0
  call DSTEBZ('A', 'B', N, 0.0d0, 0.0d0, 1, 1, ABSTOL, &
               D, E, M, NSPLIT, W, IBLOCK, ISPLIT, WORK, IWORK, INFO)
  call begin_test('n1_all')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_int('nsplit', NSPLIT)
  call print_array('w', W, M)
  call print_int_array('iblock', IBLOCK, M)
  call print_int_array('isplit', ISPLIT, NSPLIT)
  call end_test()

  ! ============================================================
  ! Test 3: N=1, RANGE='V', eigenvalue outside interval
  ! ============================================================
  D(1) = 5.0d0
  call DSTEBZ('V', 'B', 1, 0.0d0, 3.0d0, 1, 1, ABSTOL, &
               D, E, M, NSPLIT, W, IBLOCK, ISPLIT, WORK, IWORK, INFO)
  call begin_test('n1_v_outside')
  call print_int('info', INFO)
  call print_int('m', M)
  call end_test()

  ! ============================================================
  ! Test 4: 5x5 tridiagonal, RANGE='A', ORDER='B'
  ! T = tridiag([1,1,1,1], [2,-1,3,0.5,4], [1,1,1,1])
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

  call DSTEBZ('A', 'B', N, 0.0d0, 0.0d0, 1, N, ABSTOL, &
               D, E, M, NSPLIT, W, IBLOCK, ISPLIT, WORK, IWORK, INFO)
  call begin_test('n5_all_orderB')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_int('nsplit', NSPLIT)
  call print_array('w', W, M)
  call print_int_array('iblock', IBLOCK, M)
  call print_int_array('isplit', ISPLIT, NSPLIT)
  call end_test()

  ! ============================================================
  ! Test 5: Same matrix, RANGE='A', ORDER='E'
  ! ============================================================
  call DSTEBZ('A', 'E', N, 0.0d0, 0.0d0, 1, N, ABSTOL, &
               D, E, M, NSPLIT, W, IBLOCK, ISPLIT, WORK, IWORK, INFO)
  call begin_test('n5_all_orderE')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_int('nsplit', NSPLIT)
  call print_array('w', W, M)
  call print_int_array('iblock', IBLOCK, M)
  call print_int_array('isplit', ISPLIT, NSPLIT)
  call end_test()

  ! ============================================================
  ! Test 6: RANGE='V', value interval (0, 3]
  ! ============================================================
  call DSTEBZ('V', 'E', N, 0.0d0, 3.0d0, 1, N, ABSTOL, &
               D, E, M, NSPLIT, W, IBLOCK, ISPLIT, WORK, IWORK, INFO)
  call begin_test('n5_rangeV')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_array('w', W, M)
  call print_int_array('iblock', IBLOCK, M)
  call end_test()

  ! ============================================================
  ! Test 7: RANGE='I', eigenvalues 2 through 4
  ! ============================================================
  call DSTEBZ('I', 'E', N, 0.0d0, 0.0d0, 2, 4, ABSTOL, &
               D, E, M, NSPLIT, W, IBLOCK, ISPLIT, WORK, IWORK, INFO)
  call begin_test('n5_rangeI')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_array('w', W, M)
  call print_int_array('iblock', IBLOCK, M)
  call end_test()

  ! ============================================================
  ! Test 8: RANGE='I' with IL=1, IU=N (should simplify to 'A')
  ! ============================================================
  call DSTEBZ('I', 'E', N, 0.0d0, 0.0d0, 1, N, ABSTOL, &
               D, E, M, NSPLIT, W, IBLOCK, ISPLIT, WORK, IWORK, INFO)
  call begin_test('n5_rangeI_all')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_array('w', W, M)
  call end_test()

  ! ============================================================
  ! Test 9: Matrix with a split (E(3) = 0 creates two blocks)
  ! ============================================================
  N = 5
  D(1) = 2.0d0
  D(2) = -1.0d0
  D(3) = 3.0d0
  D(4) = 0.5d0
  D(5) = 4.0d0
  E(1) = 1.0d0
  E(2) = 0.0d0
  E(3) = 1.0d0
  E(4) = 1.0d0

  call DSTEBZ('A', 'B', N, 0.0d0, 0.0d0, 1, N, ABSTOL, &
               D, E, M, NSPLIT, W, IBLOCK, ISPLIT, WORK, IWORK, INFO)
  call begin_test('n5_split_orderB')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_int('nsplit', NSPLIT)
  call print_array('w', W, M)
  call print_int_array('iblock', IBLOCK, M)
  call print_int_array('isplit', ISPLIT, NSPLIT)
  call end_test()

  ! ============================================================
  ! Test 10: Same split matrix, ORDER='E'
  ! ============================================================
  call DSTEBZ('A', 'E', N, 0.0d0, 0.0d0, 1, N, ABSTOL, &
               D, E, M, NSPLIT, W, IBLOCK, ISPLIT, WORK, IWORK, INFO)
  call begin_test('n5_split_orderE')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_int('nsplit', NSPLIT)
  call print_array('w', W, M)
  call print_int_array('iblock', IBLOCK, M)
  call print_int_array('isplit', ISPLIT, NSPLIT)
  call end_test()

  ! ============================================================
  ! Test 11: Diagonal matrix (all E=0)
  ! ============================================================
  N = 4
  D(1) = 5.0d0
  D(2) = 1.0d0
  D(3) = 3.0d0
  D(4) = 2.0d0
  E(1) = 0.0d0
  E(2) = 0.0d0
  E(3) = 0.0d0

  call DSTEBZ('A', 'E', N, 0.0d0, 0.0d0, 1, N, ABSTOL, &
               D, E, M, NSPLIT, W, IBLOCK, ISPLIT, WORK, IWORK, INFO)
  call begin_test('diagonal')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_int('nsplit', NSPLIT)
  call print_array('w', W, M)
  call print_int_array('iblock', IBLOCK, M)
  call print_int_array('isplit', ISPLIT, NSPLIT)
  call end_test()

  ! ============================================================
  ! Test 12: RANGE='V' on split matrix
  ! ============================================================
  N = 5
  D(1) = 2.0d0
  D(2) = -1.0d0
  D(3) = 3.0d0
  D(4) = 0.5d0
  D(5) = 4.0d0
  E(1) = 0.0d0
  E(2) = 0.0d0
  E(3) = 1.0d0
  E(4) = 1.0d0

  call DSTEBZ('V', 'B', N, -2.0d0, 3.5d0, 1, N, ABSTOL, &
               D, E, M, NSPLIT, W, IBLOCK, ISPLIT, WORK, IWORK, INFO)
  call begin_test('split_rangeV')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_int('nsplit', NSPLIT)
  call print_array('w', W, M)
  call print_int_array('iblock', IBLOCK, M)
  call print_int_array('isplit', ISPLIT, NSPLIT)
  call end_test()

  ! ============================================================
  ! Test 13: RANGE='I' on split matrix
  ! ============================================================
  call DSTEBZ('I', 'E', N, 0.0d0, 0.0d0, 2, 4, ABSTOL, &
               D, E, M, NSPLIT, W, IBLOCK, ISPLIT, WORK, IWORK, INFO)
  call begin_test('split_rangeI')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_array('w', W, M)
  call print_int_array('iblock', IBLOCK, M)
  call end_test()

  ! ============================================================
  ! Test 14: Larger matrix 10x10 for more thorough testing
  ! Wilkinson matrix W21+ (shifted): d_i = |i-5|, e_i = 1
  ! ============================================================
  N = 10
  D(1) = 4.0d0
  D(2) = 3.0d0
  D(3) = 2.0d0
  D(4) = 1.0d0
  D(5) = 0.0d0
  D(6) = 1.0d0
  D(7) = 2.0d0
  D(8) = 3.0d0
  D(9) = 4.0d0
  D(10) = 5.0d0
  do N = 1, 9
    E(N) = 1.0d0
  end do
  N = 10

  call DSTEBZ('A', 'E', N, 0.0d0, 0.0d0, 1, N, ABSTOL, &
               D, E, M, NSPLIT, W, IBLOCK, ISPLIT, WORK, IWORK, INFO)
  call begin_test('wilkinson10')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_int('nsplit', NSPLIT)
  call print_array('w', W, M)
  call print_int_array('iblock', IBLOCK, M)
  call end_test()

  ! Test RANGE='I' on Wilkinson: eigenvalues 3 through 7
  call DSTEBZ('I', 'E', N, 0.0d0, 0.0d0, 3, 7, ABSTOL, &
               D, E, M, NSPLIT, W, IBLOCK, ISPLIT, WORK, IWORK, INFO)
  call begin_test('wilkinson10_rangeI')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_array('w', W, M)
  call end_test()

  ! Test RANGE='V' on Wilkinson: eigenvalues in (1, 4]
  call DSTEBZ('V', 'E', N, 1.0d0, 4.0d0, 1, N, ABSTOL, &
               D, E, M, NSPLIT, W, IBLOCK, ISPLIT, WORK, IWORK, INFO)
  call begin_test('wilkinson10_rangeV')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_array('w', W, M)
  call end_test()

end program
