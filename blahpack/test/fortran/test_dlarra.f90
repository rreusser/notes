program test_dlarra
  use test_utils
  implicit none

  integer, parameter :: NMAX = 10
  integer :: N, NSPLIT, INFO, I
  double precision :: SPLTOL, TNRM
  double precision :: D(NMAX), E(NMAX), E2(NMAX)
  integer :: ISPLIT(NMAX)
  external :: DLARRA

  ! ============================================================
  ! Test 1: N=0 quick return
  ! ============================================================
  N = 0
  NSPLIT = -1
  INFO = -1
  call DLARRA(N, D, E, E2, 1.0d0, 1.0d0, NSPLIT, ISPLIT, INFO)

  call begin_test('n_zero')
  call print_int('info', INFO)
  call print_int('nsplit', NSPLIT)
  call end_test()

  ! ============================================================
  ! Test 2: N=1 single element (no off-diagonals to check)
  ! ============================================================
  N = 1
  D(1) = 5.0d0
  E(1) = 0.0d0
  E2(1) = 0.0d0
  call DLARRA(N, D, E, E2, 1.0d0, 5.0d0, NSPLIT, ISPLIT, INFO)

  call begin_test('n_one')
  call print_int('info', INFO)
  call print_int('nsplit', NSPLIT)
  call print_int_array('isplit', ISPLIT, NSPLIT)
  call end_test()

  ! ============================================================
  ! Test 3: SPLTOL < 0, absolute threshold, no splits
  ! D = [4, 3, 2, 5], E = [1, 1, 1], E2 = [1, 1, 1]
  ! TNRM = 5.0, TMP1 = |SPLTOL| * TNRM = 0.1 * 5.0 = 0.5
  ! |E(i)| = 1.0 > 0.5, so no splits
  ! ============================================================
  N = 4
  D(1) = 4.0d0
  D(2) = 3.0d0
  D(3) = 2.0d0
  D(4) = 5.0d0
  E(1) = 1.0d0
  E(2) = 1.0d0
  E(3) = 1.0d0
  E2(1) = 1.0d0
  E2(2) = 1.0d0
  E2(3) = 1.0d0
  SPLTOL = -0.1d0
  TNRM = 5.0d0

  call DLARRA(N, D, E, E2, SPLTOL, TNRM, NSPLIT, ISPLIT, INFO)

  call begin_test('spltol_neg_no_split')
  call print_int('info', INFO)
  call print_int('nsplit', NSPLIT)
  call print_int_array('isplit', ISPLIT, NSPLIT)
  call print_array('e', E, N-1)
  call print_array('e2', E2, N-1)
  call end_test()

  ! ============================================================
  ! Test 4: SPLTOL < 0, absolute threshold, with splits
  ! D = [4, 3, 2, 5], E = [0.01, 1.0, 0.02], E2 = [0.0001, 1.0, 0.0004]
  ! TNRM = 5.0, TMP1 = 0.1 * 5.0 = 0.5
  ! |E(1)| = 0.01 <= 0.5 -> split at 1
  ! |E(2)| = 1.0  > 0.5  -> no split
  ! |E(3)| = 0.02 <= 0.5 -> split at 3
  ! ============================================================
  N = 4
  D(1) = 4.0d0
  D(2) = 3.0d0
  D(3) = 2.0d0
  D(4) = 5.0d0
  E(1) = 0.01d0
  E(2) = 1.0d0
  E(3) = 0.02d0
  E2(1) = 0.0001d0
  E2(2) = 1.0d0
  E2(3) = 0.0004d0
  SPLTOL = -0.1d0
  TNRM = 5.0d0

  call DLARRA(N, D, E, E2, SPLTOL, TNRM, NSPLIT, ISPLIT, INFO)

  call begin_test('spltol_neg_with_splits')
  call print_int('info', INFO)
  call print_int('nsplit', NSPLIT)
  call print_int_array('isplit', ISPLIT, NSPLIT)
  call print_array('e', E, N-1)
  call print_array('e2', E2, N-1)
  call end_test()

  ! ============================================================
  ! Test 5: SPLTOL > 0, relative accuracy, no splits
  ! D = [4, 3, 2, 5], E = [2.0, 1.5, 2.5]
  ! SPLTOL = 0.01
  ! Check: |E(1)| <= SPLTOL * sqrt(|D(1)|) * sqrt(|D(2)|)?
  !   2.0 <= 0.01 * 2.0 * 1.732 = 0.0346 -> NO
  !   1.5 <= 0.01 * 1.732 * 1.414 = 0.0245 -> NO
  !   2.5 <= 0.01 * 1.414 * 2.236 = 0.0316 -> NO
  ! No splits
  ! ============================================================
  N = 4
  D(1) = 4.0d0
  D(2) = 3.0d0
  D(3) = 2.0d0
  D(4) = 5.0d0
  E(1) = 2.0d0
  E(2) = 1.5d0
  E(3) = 2.5d0
  E2(1) = 4.0d0
  E2(2) = 2.25d0
  E2(3) = 6.25d0
  SPLTOL = 0.01d0
  TNRM = 5.0d0

  call DLARRA(N, D, E, E2, SPLTOL, TNRM, NSPLIT, ISPLIT, INFO)

  call begin_test('spltol_pos_no_split')
  call print_int('info', INFO)
  call print_int('nsplit', NSPLIT)
  call print_int_array('isplit', ISPLIT, NSPLIT)
  call print_array('e', E, N-1)
  call print_array('e2', E2, N-1)
  call end_test()

  ! ============================================================
  ! Test 6: SPLTOL > 0, relative accuracy, with splits
  ! D = [4, 3, 2, 5], E = [0.001, 1.0, 0.001]
  ! SPLTOL = 0.01
  ! Check: |E(1)| <= 0.01 * sqrt(4) * sqrt(3) = 0.01 * 2 * 1.732 = 0.0346 -> YES (0.001 <= 0.0346)
  ! Check: |E(2)| <= 0.01 * sqrt(3) * sqrt(2) = 0.01 * 1.732 * 1.414 = 0.0245 -> NO (1.0 > 0.0245)
  ! Check: |E(3)| <= 0.01 * sqrt(2) * sqrt(5) = 0.01 * 1.414 * 2.236 = 0.0316 -> YES (0.001 <= 0.0316)
  ! Splits at 1 and 3
  ! ============================================================
  N = 4
  D(1) = 4.0d0
  D(2) = 3.0d0
  D(3) = 2.0d0
  D(4) = 5.0d0
  E(1) = 0.001d0
  E(2) = 1.0d0
  E(3) = 0.001d0
  E2(1) = 0.000001d0
  E2(2) = 1.0d0
  E2(3) = 0.000001d0
  SPLTOL = 0.01d0
  TNRM = 5.0d0

  call DLARRA(N, D, E, E2, SPLTOL, TNRM, NSPLIT, ISPLIT, INFO)

  call begin_test('spltol_pos_with_splits')
  call print_int('info', INFO)
  call print_int('nsplit', NSPLIT)
  call print_int_array('isplit', ISPLIT, NSPLIT)
  call print_array('e', E, N-1)
  call print_array('e2', E2, N-1)
  call end_test()

  ! ============================================================
  ! Test 7: All off-diagonals zero (every element is a split)
  ! ============================================================
  N = 5
  D(1) = 1.0d0
  D(2) = 2.0d0
  D(3) = 3.0d0
  D(4) = 4.0d0
  D(5) = 5.0d0
  E(1) = 0.0d0
  E(2) = 0.0d0
  E(3) = 0.0d0
  E(4) = 0.0d0
  E2(1) = 0.0d0
  E2(2) = 0.0d0
  E2(3) = 0.0d0
  E2(4) = 0.0d0
  SPLTOL = -0.1d0
  TNRM = 5.0d0

  call DLARRA(N, D, E, E2, SPLTOL, TNRM, NSPLIT, ISPLIT, INFO)

  call begin_test('all_zero_offdiag')
  call print_int('info', INFO)
  call print_int('nsplit', NSPLIT)
  call print_int_array('isplit', ISPLIT, NSPLIT)
  call end_test()

  ! ============================================================
  ! Test 8: Larger 6x6 tridiagonal with mixed splits (SPLTOL < 0)
  ! ============================================================
  N = 6
  D(1) = 10.0d0
  D(2) = 8.0d0
  D(3) = 6.0d0
  D(4) = 4.0d0
  D(5) = 2.0d0
  D(6) = 1.0d0
  E(1) = 0.001d0
  E(2) = 5.0d0
  E(3) = 0.002d0
  E(4) = 3.0d0
  E(5) = 0.003d0
  E2(1) = 0.000001d0
  E2(2) = 25.0d0
  E2(3) = 0.000004d0
  E2(4) = 9.0d0
  E2(5) = 0.000009d0
  SPLTOL = -0.01d0
  TNRM = 10.0d0
  ! TMP1 = 0.01 * 10 = 0.1
  ! |E(1)| = 0.001 <= 0.1 -> split at 1
  ! |E(2)| = 5.0   > 0.1  -> no split
  ! |E(3)| = 0.002 <= 0.1 -> split at 3
  ! |E(4)| = 3.0   > 0.1  -> no split
  ! |E(5)| = 0.003 <= 0.1 -> split at 5

  call DLARRA(N, D, E, E2, SPLTOL, TNRM, NSPLIT, ISPLIT, INFO)

  call begin_test('larger_6x6_neg_spltol')
  call print_int('info', INFO)
  call print_int('nsplit', NSPLIT)
  call print_int_array('isplit', ISPLIT, NSPLIT)
  call print_array('e', E, N-1)
  call print_array('e2', E2, N-1)
  call end_test()

  ! ============================================================
  ! Test 9: SPLTOL > 0, with negative diagonal entries
  ! Tests that ABS(D(I)) is used correctly
  ! ============================================================
  N = 3
  D(1) = -4.0d0
  D(2) = -9.0d0
  D(3) = -1.0d0
  E(1) = 0.01d0
  E(2) = 0.01d0
  E2(1) = 0.0001d0
  E2(2) = 0.0001d0
  SPLTOL = 0.01d0
  TNRM = 9.0d0
  ! Check: |E(1)| <= 0.01 * sqrt(4) * sqrt(9) = 0.01 * 2 * 3 = 0.06 -> YES (0.01 <= 0.06)
  ! Check: |E(2)| <= 0.01 * sqrt(9) * sqrt(1) = 0.01 * 3 * 1 = 0.03 -> YES (0.01 <= 0.03)

  call DLARRA(N, D, E, E2, SPLTOL, TNRM, NSPLIT, ISPLIT, INFO)

  call begin_test('neg_diag_pos_spltol')
  call print_int('info', INFO)
  call print_int('nsplit', NSPLIT)
  call print_int_array('isplit', ISPLIT, NSPLIT)
  call print_array('e', E, N-1)
  call print_array('e2', E2, N-1)
  call end_test()

end program
