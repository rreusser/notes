program test_dlarfb
  use test_utils
  implicit none

  double precision :: V(6, 6), T(2, 2), C(6, 4), WORK(4, 2)
  double precision :: TAU(2), alpha
  integer :: i, j

  ! Build a 2-reflector block for a 4x3 matrix to test dlarfb
  ! We'll construct V and T using dlarfg and dlarft, then apply with dlarfb

  ! Test 1: Left, No transpose, Forward, Columnwise (STOREV='C')
  ! Build V: 4x2 lower unit triangular
  V = 0.0d0
  V(1,1) = 1.0d0;
  V(2,1) = 0.5d0; V(2,2) = 1.0d0
  V(3,1) = 0.25d0; V(3,2) = 0.5d0
  V(4,1) = 0.125d0; V(4,2) = 0.25d0

  ! Build T: 2x2 upper triangular
  T = 0.0d0
  T(1,1) = 1.2d0; T(1,2) = -0.3d0
  T(2,2) = 1.5d0

  ! Build C: 4x3 matrix
  C = 0.0d0
  C(1,1) = 1.0d0; C(1,2) = 2.0d0; C(1,3) = 3.0d0
  C(2,1) = 4.0d0; C(2,2) = 5.0d0; C(2,3) = 6.0d0
  C(3,1) = 7.0d0; C(3,2) = 8.0d0; C(3,3) = 9.0d0
  C(4,1) = 10.0d0; C(4,2) = 11.0d0; C(4,3) = 12.0d0

  WORK = 0.0d0
  call DLARFB('L', 'N', 'F', 'C', 4, 3, 2, V, 6, T, 2, C, 6, WORK, 4)
  call begin_test('left_notrans_fwd_col')
  call print_matrix('C', C, 6, 4, 3)
  call end_test()

  ! Test 2: Left, Transpose, Forward, Columnwise
  C = 0.0d0
  C(1,1) = 1.0d0; C(1,2) = 2.0d0; C(1,3) = 3.0d0
  C(2,1) = 4.0d0; C(2,2) = 5.0d0; C(2,3) = 6.0d0
  C(3,1) = 7.0d0; C(3,2) = 8.0d0; C(3,3) = 9.0d0
  C(4,1) = 10.0d0; C(4,2) = 11.0d0; C(4,3) = 12.0d0

  WORK = 0.0d0
  call DLARFB('L', 'T', 'F', 'C', 4, 3, 2, V, 6, T, 2, C, 6, WORK, 4)
  call begin_test('left_trans_fwd_col')
  call print_matrix('C', C, 6, 4, 3)
  call end_test()

  ! Test 3: Right, No transpose, Forward, Columnwise
  ! V: 3x2 (N=3, K=2), C: 4x3
  V = 0.0d0
  V(1,1) = 1.0d0;
  V(2,1) = 0.5d0; V(2,2) = 1.0d0
  V(3,1) = 0.25d0; V(3,2) = 0.5d0

  C = 0.0d0
  C(1,1) = 1.0d0; C(1,2) = 2.0d0; C(1,3) = 3.0d0
  C(2,1) = 4.0d0; C(2,2) = 5.0d0; C(2,3) = 6.0d0
  C(3,1) = 7.0d0; C(3,2) = 8.0d0; C(3,3) = 9.0d0
  C(4,1) = 10.0d0; C(4,2) = 11.0d0; C(4,3) = 12.0d0

  WORK = 0.0d0
  call DLARFB('R', 'N', 'F', 'C', 4, 3, 2, V, 6, T, 2, C, 6, WORK, 4)
  call begin_test('right_notrans_fwd_col')
  call print_matrix('C', C, 6, 4, 3)
  call end_test()

  ! Test 4: M=0 (quick return)
  C = 99.0d0
  call DLARFB('L', 'N', 'F', 'C', 0, 3, 2, V, 6, T, 2, C, 6, WORK, 4)
  call begin_test('m_zero')
  call print_matrix('C', C, 6, 1, 1)
  call end_test()

  ! Test 5: Left, No transpose, Backward, Columnwise
  ! V: 4x2 with unit triangular at bottom (rows M-K..M-1)
  V = 0.0d0
  V(1,1) = 0.5d0; V(1,2) = 0.25d0
  V(2,1) = 0.25d0; V(2,2) = 0.125d0
  V(3,1) = 1.0d0; V(3,2) = 0.0d0
  V(4,1) = 0.0d0; V(4,2) = 1.0d0

  ! T: 2x2 lower triangular (backward direction)
  T = 0.0d0
  T(1,1) = 1.2d0
  T(2,1) = -0.3d0; T(2,2) = 1.5d0

  C = 0.0d0
  C(1,1) = 1.0d0; C(1,2) = 2.0d0; C(1,3) = 3.0d0
  C(2,1) = 4.0d0; C(2,2) = 5.0d0; C(2,3) = 6.0d0
  C(3,1) = 7.0d0; C(3,2) = 8.0d0; C(3,3) = 9.0d0
  C(4,1) = 10.0d0; C(4,2) = 11.0d0; C(4,3) = 12.0d0

  WORK = 0.0d0
  call DLARFB('L', 'N', 'B', 'C', 4, 3, 2, V, 6, T, 2, C, 6, WORK, 4)
  call begin_test('left_notrans_bwd_col')
  call print_matrix('C', C, 6, 4, 3)
  call end_test()

  ! Test 6: Left, Transpose, Backward, Columnwise
  V = 0.0d0
  V(1,1) = 0.5d0; V(1,2) = 0.25d0
  V(2,1) = 0.25d0; V(2,2) = 0.125d0
  V(3,1) = 1.0d0; V(3,2) = 0.0d0
  V(4,1) = 0.0d0; V(4,2) = 1.0d0

  T = 0.0d0
  T(1,1) = 1.2d0
  T(2,1) = -0.3d0; T(2,2) = 1.5d0

  C = 0.0d0
  C(1,1) = 1.0d0; C(1,2) = 2.0d0; C(1,3) = 3.0d0
  C(2,1) = 4.0d0; C(2,2) = 5.0d0; C(2,3) = 6.0d0
  C(3,1) = 7.0d0; C(3,2) = 8.0d0; C(3,3) = 9.0d0
  C(4,1) = 10.0d0; C(4,2) = 11.0d0; C(4,3) = 12.0d0

  WORK = 0.0d0
  call DLARFB('L', 'T', 'B', 'C', 4, 3, 2, V, 6, T, 2, C, 6, WORK, 4)
  call begin_test('left_trans_bwd_col')
  call print_matrix('C', C, 6, 4, 3)
  call end_test()

  ! Test 7: Right, No transpose, Backward, Columnwise
  ! V: 3x2 (N=3, K=2), unit triangular at bottom
  V = 0.0d0
  V(1,1) = 0.5d0; V(1,2) = 0.25d0
  V(2,1) = 1.0d0; V(2,2) = 0.0d0
  V(3,1) = 0.0d0; V(3,2) = 1.0d0

  T = 0.0d0
  T(1,1) = 1.2d0
  T(2,1) = -0.3d0; T(2,2) = 1.5d0

  C = 0.0d0
  C(1,1) = 1.0d0; C(1,2) = 2.0d0; C(1,3) = 3.0d0
  C(2,1) = 4.0d0; C(2,2) = 5.0d0; C(2,3) = 6.0d0
  C(3,1) = 7.0d0; C(3,2) = 8.0d0; C(3,3) = 9.0d0
  C(4,1) = 10.0d0; C(4,2) = 11.0d0; C(4,3) = 12.0d0

  WORK = 0.0d0
  call DLARFB('R', 'N', 'B', 'C', 4, 3, 2, V, 6, T, 2, C, 6, WORK, 4)
  call begin_test('right_notrans_bwd_col')
  call print_matrix('C', C, 6, 4, 3)
  call end_test()

  ! Test 8: Right, Transpose, Backward, Columnwise
  V = 0.0d0
  V(1,1) = 0.5d0; V(1,2) = 0.25d0
  V(2,1) = 1.0d0; V(2,2) = 0.0d0
  V(3,1) = 0.0d0; V(3,2) = 1.0d0

  T = 0.0d0
  T(1,1) = 1.2d0
  T(2,1) = -0.3d0; T(2,2) = 1.5d0

  C = 0.0d0
  C(1,1) = 1.0d0; C(1,2) = 2.0d0; C(1,3) = 3.0d0
  C(2,1) = 4.0d0; C(2,2) = 5.0d0; C(2,3) = 6.0d0
  C(3,1) = 7.0d0; C(3,2) = 8.0d0; C(3,3) = 9.0d0
  C(4,1) = 10.0d0; C(4,2) = 11.0d0; C(4,3) = 12.0d0

  WORK = 0.0d0
  call DLARFB('R', 'T', 'B', 'C', 4, 3, 2, V, 6, T, 2, C, 6, WORK, 4)
  call begin_test('right_trans_bwd_col')
  call print_matrix('C', C, 6, 4, 3)
  call end_test()

  ! Test 9: Right, Transpose, Forward, Columnwise for extra coverage
  V = 0.0d0
  V(1,1) = 1.0d0
  V(2,1) = 0.5d0; V(2,2) = 1.0d0
  V(3,1) = 0.25d0; V(3,2) = 0.5d0

  T = 0.0d0
  T(1,1) = 1.2d0; T(1,2) = -0.3d0
  T(2,2) = 1.5d0

  C = 0.0d0
  C(1,1) = 1.0d0; C(1,2) = 2.0d0; C(1,3) = 3.0d0
  C(2,1) = 4.0d0; C(2,2) = 5.0d0; C(2,3) = 6.0d0
  C(3,1) = 7.0d0; C(3,2) = 8.0d0; C(3,3) = 9.0d0
  C(4,1) = 10.0d0; C(4,2) = 11.0d0; C(4,3) = 12.0d0

  WORK = 0.0d0
  call DLARFB('R', 'T', 'F', 'C', 4, 3, 2, V, 6, T, 2, C, 6, WORK, 4)
  call begin_test('right_trans_fwd_col')
  call print_matrix('C', C, 6, 4, 3)
  call end_test()

  ! =========================================================
  ! STOREV='R' tests
  ! For STOREV='R', V is K x M (for side='L') or K x N (for side='R')
  ! stored rowwise. V1 is unit upper triangular (first K columns).
  ! =========================================================

  ! Test 10: Left, No transpose, Forward, Rowwise
  ! V: 2 x 4 (K=2, M=4), stored in V(6,6) with LDV=6
  ! V1 is upper unit triangular in first 2 columns
  V = 0.0d0
  V(1,1) = 1.0d0; V(1,2) = 0.5d0; V(1,3) = 0.25d0; V(1,4) = 0.125d0
  V(2,2) = 1.0d0; V(2,3) = 0.5d0; V(2,4) = 0.25d0

  T = 0.0d0
  T(1,1) = 1.2d0; T(1,2) = -0.3d0
  T(2,2) = 1.5d0

  C = 0.0d0
  C(1,1) = 1.0d0; C(1,2) = 2.0d0; C(1,3) = 3.0d0
  C(2,1) = 4.0d0; C(2,2) = 5.0d0; C(2,3) = 6.0d0
  C(3,1) = 7.0d0; C(3,2) = 8.0d0; C(3,3) = 9.0d0
  C(4,1) = 10.0d0; C(4,2) = 11.0d0; C(4,3) = 12.0d0

  WORK = 0.0d0
  call DLARFB('L', 'N', 'F', 'R', 4, 3, 2, V, 6, T, 2, C, 6, WORK, 4)
  call begin_test('left_notrans_fwd_row')
  call print_matrix('C', C, 6, 4, 3)
  call end_test()

  ! Test 11: Left, Transpose, Forward, Rowwise
  V = 0.0d0
  V(1,1) = 1.0d0; V(1,2) = 0.5d0; V(1,3) = 0.25d0; V(1,4) = 0.125d0
  V(2,2) = 1.0d0; V(2,3) = 0.5d0; V(2,4) = 0.25d0

  T = 0.0d0
  T(1,1) = 1.2d0; T(1,2) = -0.3d0
  T(2,2) = 1.5d0

  C = 0.0d0
  C(1,1) = 1.0d0; C(1,2) = 2.0d0; C(1,3) = 3.0d0
  C(2,1) = 4.0d0; C(2,2) = 5.0d0; C(2,3) = 6.0d0
  C(3,1) = 7.0d0; C(3,2) = 8.0d0; C(3,3) = 9.0d0
  C(4,1) = 10.0d0; C(4,2) = 11.0d0; C(4,3) = 12.0d0

  WORK = 0.0d0
  call DLARFB('L', 'T', 'F', 'R', 4, 3, 2, V, 6, T, 2, C, 6, WORK, 4)
  call begin_test('left_trans_fwd_row')
  call print_matrix('C', C, 6, 4, 3)
  call end_test()

  ! Test 12: Right, No transpose, Forward, Rowwise
  ! V: 2 x 3 (K=2, N=3)
  V = 0.0d0
  V(1,1) = 1.0d0; V(1,2) = 0.5d0; V(1,3) = 0.25d0
  V(2,2) = 1.0d0; V(2,3) = 0.5d0

  T = 0.0d0
  T(1,1) = 1.2d0; T(1,2) = -0.3d0
  T(2,2) = 1.5d0

  C = 0.0d0
  C(1,1) = 1.0d0; C(1,2) = 2.0d0; C(1,3) = 3.0d0
  C(2,1) = 4.0d0; C(2,2) = 5.0d0; C(2,3) = 6.0d0
  C(3,1) = 7.0d0; C(3,2) = 8.0d0; C(3,3) = 9.0d0
  C(4,1) = 10.0d0; C(4,2) = 11.0d0; C(4,3) = 12.0d0

  WORK = 0.0d0
  call DLARFB('R', 'N', 'F', 'R', 4, 3, 2, V, 6, T, 2, C, 6, WORK, 4)
  call begin_test('right_notrans_fwd_row')
  call print_matrix('C', C, 6, 4, 3)
  call end_test()

  ! Test 13: Right, Transpose, Forward, Rowwise
  V = 0.0d0
  V(1,1) = 1.0d0; V(1,2) = 0.5d0; V(1,3) = 0.25d0
  V(2,2) = 1.0d0; V(2,3) = 0.5d0

  T = 0.0d0
  T(1,1) = 1.2d0; T(1,2) = -0.3d0
  T(2,2) = 1.5d0

  C = 0.0d0
  C(1,1) = 1.0d0; C(1,2) = 2.0d0; C(1,3) = 3.0d0
  C(2,1) = 4.0d0; C(2,2) = 5.0d0; C(2,3) = 6.0d0
  C(3,1) = 7.0d0; C(3,2) = 8.0d0; C(3,3) = 9.0d0
  C(4,1) = 10.0d0; C(4,2) = 11.0d0; C(4,3) = 12.0d0

  WORK = 0.0d0
  call DLARFB('R', 'T', 'F', 'R', 4, 3, 2, V, 6, T, 2, C, 6, WORK, 4)
  call begin_test('right_trans_fwd_row')
  call print_matrix('C', C, 6, 4, 3)
  call end_test()

  ! Test 14: Left, No transpose, Backward, Rowwise
  ! V: 2 x 4, unit lower triangular at right (columns M-K..M-1 = 2,3)
  V = 0.0d0
  V(1,1) = 0.5d0; V(1,2) = 0.25d0; V(1,3) = 1.0d0; V(1,4) = 0.0d0
  V(2,1) = 0.25d0; V(2,2) = 0.125d0; V(2,3) = 0.0d0; V(2,4) = 1.0d0

  T = 0.0d0
  T(1,1) = 1.2d0
  T(2,1) = -0.3d0; T(2,2) = 1.5d0

  C = 0.0d0
  C(1,1) = 1.0d0; C(1,2) = 2.0d0; C(1,3) = 3.0d0
  C(2,1) = 4.0d0; C(2,2) = 5.0d0; C(2,3) = 6.0d0
  C(3,1) = 7.0d0; C(3,2) = 8.0d0; C(3,3) = 9.0d0
  C(4,1) = 10.0d0; C(4,2) = 11.0d0; C(4,3) = 12.0d0

  WORK = 0.0d0
  call DLARFB('L', 'N', 'B', 'R', 4, 3, 2, V, 6, T, 2, C, 6, WORK, 4)
  call begin_test('left_notrans_bwd_row')
  call print_matrix('C', C, 6, 4, 3)
  call end_test()

  ! Test 15: Left, Transpose, Backward, Rowwise
  V = 0.0d0
  V(1,1) = 0.5d0; V(1,2) = 0.25d0; V(1,3) = 1.0d0; V(1,4) = 0.0d0
  V(2,1) = 0.25d0; V(2,2) = 0.125d0; V(2,3) = 0.0d0; V(2,4) = 1.0d0

  T = 0.0d0
  T(1,1) = 1.2d0
  T(2,1) = -0.3d0; T(2,2) = 1.5d0

  C = 0.0d0
  C(1,1) = 1.0d0; C(1,2) = 2.0d0; C(1,3) = 3.0d0
  C(2,1) = 4.0d0; C(2,2) = 5.0d0; C(2,3) = 6.0d0
  C(3,1) = 7.0d0; C(3,2) = 8.0d0; C(3,3) = 9.0d0
  C(4,1) = 10.0d0; C(4,2) = 11.0d0; C(4,3) = 12.0d0

  WORK = 0.0d0
  call DLARFB('L', 'T', 'B', 'R', 4, 3, 2, V, 6, T, 2, C, 6, WORK, 4)
  call begin_test('left_trans_bwd_row')
  call print_matrix('C', C, 6, 4, 3)
  call end_test()

  ! Test 16: Right, No transpose, Backward, Rowwise
  ! V: 2 x 3, unit lower triangular at right (columns N-K..N-1 = 1,2)
  V = 0.0d0
  V(1,1) = 0.5d0; V(1,2) = 1.0d0; V(1,3) = 0.0d0
  V(2,1) = 0.25d0; V(2,2) = 0.0d0; V(2,3) = 1.0d0

  T = 0.0d0
  T(1,1) = 1.2d0
  T(2,1) = -0.3d0; T(2,2) = 1.5d0

  C = 0.0d0
  C(1,1) = 1.0d0; C(1,2) = 2.0d0; C(1,3) = 3.0d0
  C(2,1) = 4.0d0; C(2,2) = 5.0d0; C(2,3) = 6.0d0
  C(3,1) = 7.0d0; C(3,2) = 8.0d0; C(3,3) = 9.0d0
  C(4,1) = 10.0d0; C(4,2) = 11.0d0; C(4,3) = 12.0d0

  WORK = 0.0d0
  call DLARFB('R', 'N', 'B', 'R', 4, 3, 2, V, 6, T, 2, C, 6, WORK, 4)
  call begin_test('right_notrans_bwd_row')
  call print_matrix('C', C, 6, 4, 3)
  call end_test()

  ! Test 17: Right, Transpose, Backward, Rowwise
  V = 0.0d0
  V(1,1) = 0.5d0; V(1,2) = 1.0d0; V(1,3) = 0.0d0
  V(2,1) = 0.25d0; V(2,2) = 0.0d0; V(2,3) = 1.0d0

  T = 0.0d0
  T(1,1) = 1.2d0
  T(2,1) = -0.3d0; T(2,2) = 1.5d0

  C = 0.0d0
  C(1,1) = 1.0d0; C(1,2) = 2.0d0; C(1,3) = 3.0d0
  C(2,1) = 4.0d0; C(2,2) = 5.0d0; C(2,3) = 6.0d0
  C(3,1) = 7.0d0; C(3,2) = 8.0d0; C(3,3) = 9.0d0
  C(4,1) = 10.0d0; C(4,2) = 11.0d0; C(4,3) = 12.0d0

  WORK = 0.0d0
  call DLARFB('R', 'T', 'B', 'R', 4, 3, 2, V, 6, T, 2, C, 6, WORK, 4)
  call begin_test('right_trans_bwd_row')
  call print_matrix('C', C, 6, 4, 3)
  call end_test()

end program
