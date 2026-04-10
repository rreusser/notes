program test_dtprfb
  use test_utils
  implicit none

  double precision :: V(8, 8), T(3, 3), A(4, 4), B(5, 5), WORK(5, 5)
  integer :: i, j

  ! ========================================================
  ! STOREV='C' (columnwise) tests
  ! V is (M+L or N+L) x K for left/right, triangular top + trapezoidal bottom
  ! In forward direction, the K x K triangular block is at the top of V,
  ! and the L x K rectangular/trapezoidal block is at rows M-L+1..M.
  ! ========================================================

  ! ---------------- Test 1: C, Forward, Left, NoTrans ----------------
  ! M=4, N=3, K=2, L=1.  V is M x K = 4x2. A is K x N = 2x3. B is M x N = 4x3.
  V = 0.0d0
  ! V1 upper unit triangular (K x K = 2x2)
  V(1,1) = 1.0d0
  V(1,2) = 0.3d0
  V(2,2) = 1.0d0
  ! V2 trapezoidal L x K (rows M-L+1..M = row 4): upper triangular with unit diag
  V(4,1) = 0.2d0
  V(4,2) = 1.0d0

  T = 0.0d0
  T(1,1) = 1.2d0; T(1,2) = -0.3d0
  T(2,2) = 0.8d0

  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 3.0d0
  A(2,1) = 4.0d0; A(2,2) = 5.0d0; A(2,3) = 6.0d0

  B = 0.0d0
  B(1,1) = 7.0d0;  B(1,2) = 8.0d0;  B(1,3) = 9.0d0
  B(2,1) = 10.0d0; B(2,2) = 11.0d0; B(2,3) = 12.0d0
  B(3,1) = 13.0d0; B(3,2) = 14.0d0; B(3,3) = 15.0d0
  B(4,1) = 16.0d0; B(4,2) = 17.0d0; B(4,3) = 18.0d0

  WORK = 0.0d0
  call DTPRFB('L', 'N', 'F', 'C', 4, 3, 2, 1, V, 8, T, 3, A, 4, B, 5, WORK, 5)
  call begin_test('col_fwd_left_notrans')
  call print_matrix('A', A, 4, 2, 3)
  call print_matrix('B', B, 5, 4, 3)
  call end_test()

  ! ---------------- Test 2: C, Forward, Left, Trans ----------------
  V = 0.0d0
  V(1,1) = 1.0d0
  V(1,2) = 0.3d0
  V(2,2) = 1.0d0
  V(4,1) = 0.2d0
  V(4,2) = 1.0d0

  T = 0.0d0
  T(1,1) = 1.2d0; T(1,2) = -0.3d0
  T(2,2) = 0.8d0

  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 3.0d0
  A(2,1) = 4.0d0; A(2,2) = 5.0d0; A(2,3) = 6.0d0

  B = 0.0d0
  B(1,1) = 7.0d0;  B(1,2) = 8.0d0;  B(1,3) = 9.0d0
  B(2,1) = 10.0d0; B(2,2) = 11.0d0; B(2,3) = 12.0d0
  B(3,1) = 13.0d0; B(3,2) = 14.0d0; B(3,3) = 15.0d0
  B(4,1) = 16.0d0; B(4,2) = 17.0d0; B(4,3) = 18.0d0

  WORK = 0.0d0
  call DTPRFB('L', 'T', 'F', 'C', 4, 3, 2, 1, V, 8, T, 3, A, 4, B, 5, WORK, 5)
  call begin_test('col_fwd_left_trans')
  call print_matrix('A', A, 4, 2, 3)
  call print_matrix('B', B, 5, 4, 3)
  call end_test()

  ! ---------------- Test 3: C, Forward, Right, NoTrans ----------------
  ! M=3, N=4, K=2, L=1.  V is N x K = 4x2.  A is M x K = 3x2.  B is M x N = 3x4.
  V = 0.0d0
  V(1,1) = 1.0d0
  V(1,2) = 0.3d0
  V(2,2) = 1.0d0
  V(4,1) = 0.2d0
  V(4,2) = 1.0d0

  T = 0.0d0
  T(1,1) = 1.2d0; T(1,2) = -0.3d0
  T(2,2) = 0.8d0

  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0
  A(2,1) = 3.0d0; A(2,2) = 4.0d0
  A(3,1) = 5.0d0; A(3,2) = 6.0d0

  B = 0.0d0
  B(1,1) = 7.0d0;  B(1,2) = 8.0d0;  B(1,3) = 9.0d0;  B(1,4) = 10.0d0
  B(2,1) = 11.0d0; B(2,2) = 12.0d0; B(2,3) = 13.0d0; B(2,4) = 14.0d0
  B(3,1) = 15.0d0; B(3,2) = 16.0d0; B(3,3) = 17.0d0; B(3,4) = 18.0d0

  WORK = 0.0d0
  call DTPRFB('R', 'N', 'F', 'C', 3, 4, 2, 1, V, 8, T, 3, A, 4, B, 5, WORK, 5)
  call begin_test('col_fwd_right_notrans')
  call print_matrix('A', A, 4, 3, 2)
  call print_matrix('B', B, 5, 3, 4)
  call end_test()

  ! ---------------- Test 4: C, Forward, Right, Trans ----------------
  V = 0.0d0
  V(1,1) = 1.0d0
  V(1,2) = 0.3d0
  V(2,2) = 1.0d0
  V(4,1) = 0.2d0
  V(4,2) = 1.0d0

  T = 0.0d0
  T(1,1) = 1.2d0; T(1,2) = -0.3d0
  T(2,2) = 0.8d0

  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0
  A(2,1) = 3.0d0; A(2,2) = 4.0d0
  A(3,1) = 5.0d0; A(3,2) = 6.0d0

  B = 0.0d0
  B(1,1) = 7.0d0;  B(1,2) = 8.0d0;  B(1,3) = 9.0d0;  B(1,4) = 10.0d0
  B(2,1) = 11.0d0; B(2,2) = 12.0d0; B(2,3) = 13.0d0; B(2,4) = 14.0d0
  B(3,1) = 15.0d0; B(3,2) = 16.0d0; B(3,3) = 17.0d0; B(3,4) = 18.0d0

  WORK = 0.0d0
  call DTPRFB('R', 'T', 'F', 'C', 3, 4, 2, 1, V, 8, T, 3, A, 4, B, 5, WORK, 5)
  call begin_test('col_fwd_right_trans')
  call print_matrix('A', A, 4, 3, 2)
  call print_matrix('B', B, 5, 3, 4)
  call end_test()

  ! ---------------- Test 5: C, Backward, Left, NoTrans ----------------
  ! M=4, N=3, K=2, L=1.  V columnwise backward: L x K trapezoidal on top (rows 1..L),
  ! K x K lower triangular at rows M-K+1..M.
  V = 0.0d0
  V(1,1) = 0.2d0; V(1,2) = 0.3d0      ! trapezoidal row
  V(3,1) = 1.0d0                       ! lower unit triangular
  V(4,1) = 0.4d0; V(4,2) = 1.0d0

  T = 0.0d0
  T(1,1) = 1.2d0
  T(2,1) = -0.3d0; T(2,2) = 0.8d0

  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 3.0d0
  A(2,1) = 4.0d0; A(2,2) = 5.0d0; A(2,3) = 6.0d0

  B = 0.0d0
  B(1,1) = 7.0d0;  B(1,2) = 8.0d0;  B(1,3) = 9.0d0
  B(2,1) = 10.0d0; B(2,2) = 11.0d0; B(2,3) = 12.0d0
  B(3,1) = 13.0d0; B(3,2) = 14.0d0; B(3,3) = 15.0d0
  B(4,1) = 16.0d0; B(4,2) = 17.0d0; B(4,3) = 18.0d0

  WORK = 0.0d0
  call DTPRFB('L', 'N', 'B', 'C', 4, 3, 2, 1, V, 8, T, 3, A, 4, B, 5, WORK, 5)
  call begin_test('col_bwd_left_notrans')
  call print_matrix('A', A, 4, 2, 3)
  call print_matrix('B', B, 5, 4, 3)
  call end_test()

  ! ---------------- Test 6: C, Backward, Right, Trans ----------------
  ! M=3, N=4, K=2, L=1.
  V = 0.0d0
  V(1,1) = 0.2d0; V(1,2) = 0.3d0
  V(3,1) = 1.0d0
  V(4,1) = 0.4d0; V(4,2) = 1.0d0

  T = 0.0d0
  T(1,1) = 1.2d0
  T(2,1) = -0.3d0; T(2,2) = 0.8d0

  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0
  A(2,1) = 3.0d0; A(2,2) = 4.0d0
  A(3,1) = 5.0d0; A(3,2) = 6.0d0

  B = 0.0d0
  B(1,1) = 7.0d0;  B(1,2) = 8.0d0;  B(1,3) = 9.0d0;  B(1,4) = 10.0d0
  B(2,1) = 11.0d0; B(2,2) = 12.0d0; B(2,3) = 13.0d0; B(2,4) = 14.0d0
  B(3,1) = 15.0d0; B(3,2) = 16.0d0; B(3,3) = 17.0d0; B(3,4) = 18.0d0

  WORK = 0.0d0
  call DTPRFB('R', 'T', 'B', 'C', 3, 4, 2, 1, V, 8, T, 3, A, 4, B, 5, WORK, 5)
  call begin_test('col_bwd_right_trans')
  call print_matrix('A', A, 4, 3, 2)
  call print_matrix('B', B, 5, 3, 4)
  call end_test()

  ! ========================================================
  ! STOREV='R' (rowwise) tests
  ! V is K x (M+L) or K x (N+L).
  ! Forward: V1 is K x K lower unit triangular (cols 1..K), V2 is K x L (cols M-L+1..M)
  ! Backward: V2 is K x L (cols 1..L), V1 is K x K upper unit triangular (cols M-K+1..M)
  ! ========================================================

  ! ---------------- Test 7: R, Forward, Left, NoTrans ----------------
  ! M=4, N=3, K=2, L=1.  V is K x M = 2x4.
  V = 0.0d0
  V(1,1) = 1.0d0
  V(2,1) = 0.3d0;  V(2,2) = 1.0d0
  V(1,4) = 0.2d0   ! trapezoidal col (K x L)
  V(2,4) = 0.5d0

  T = 0.0d0
  T(1,1) = 1.2d0; T(1,2) = -0.3d0
  T(2,2) = 0.8d0

  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 3.0d0
  A(2,1) = 4.0d0; A(2,2) = 5.0d0; A(2,3) = 6.0d0

  B = 0.0d0
  B(1,1) = 7.0d0;  B(1,2) = 8.0d0;  B(1,3) = 9.0d0
  B(2,1) = 10.0d0; B(2,2) = 11.0d0; B(2,3) = 12.0d0
  B(3,1) = 13.0d0; B(3,2) = 14.0d0; B(3,3) = 15.0d0
  B(4,1) = 16.0d0; B(4,2) = 17.0d0; B(4,3) = 18.0d0

  WORK = 0.0d0
  call DTPRFB('L', 'N', 'F', 'R', 4, 3, 2, 1, V, 8, T, 3, A, 4, B, 5, WORK, 5)
  call begin_test('row_fwd_left_notrans')
  call print_matrix('A', A, 4, 2, 3)
  call print_matrix('B', B, 5, 4, 3)
  call end_test()

  ! ---------------- Test 8: R, Forward, Right, NoTrans ----------------
  ! M=3, N=4, K=2, L=1.  V is K x N = 2x4.
  V = 0.0d0
  V(1,1) = 1.0d0
  V(2,1) = 0.3d0;  V(2,2) = 1.0d0
  V(1,4) = 0.2d0
  V(2,4) = 0.5d0

  T = 0.0d0
  T(1,1) = 1.2d0; T(1,2) = -0.3d0
  T(2,2) = 0.8d0

  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0
  A(2,1) = 3.0d0; A(2,2) = 4.0d0
  A(3,1) = 5.0d0; A(3,2) = 6.0d0

  B = 0.0d0
  B(1,1) = 7.0d0;  B(1,2) = 8.0d0;  B(1,3) = 9.0d0;  B(1,4) = 10.0d0
  B(2,1) = 11.0d0; B(2,2) = 12.0d0; B(2,3) = 13.0d0; B(2,4) = 14.0d0
  B(3,1) = 15.0d0; B(3,2) = 16.0d0; B(3,3) = 17.0d0; B(3,4) = 18.0d0

  WORK = 0.0d0
  call DTPRFB('R', 'N', 'F', 'R', 3, 4, 2, 1, V, 8, T, 3, A, 4, B, 5, WORK, 5)
  call begin_test('row_fwd_right_notrans')
  call print_matrix('A', A, 4, 3, 2)
  call print_matrix('B', B, 5, 3, 4)
  call end_test()

  ! ---------------- Test 9: R, Backward, Left, Trans ----------------
  ! M=4, N=3, K=2, L=1.  V is K x M = 2x4. V2 at cols 1..L, V1 at cols M-K+1..M (cols 3..4).
  V = 0.0d0
  V(1,1) = 0.2d0                        ! trapezoidal col (K x L, col 1)
  V(2,1) = 0.5d0
  V(1,3) = 0.3d0;  V(1,4) = 1.0d0      ! upper unit triangular (cols 3..4)
  V(2,4) = 1.0d0  ! diag of V1 lower-right
  V(1,4) = 1.0d0  ! correct upper unit: row 1 col 4 is upper of 2x2? Actually for K x K upper unit:
  ! reset and be explicit
  V = 0.0d0
  V(1,1) = 0.2d0                        ! V2 column
  V(2,1) = 0.5d0
  ! V1 = K x K upper unit triangular stored at cols M-K+1..M = cols 3..4
  V(1,3) = 1.0d0;  V(1,4) = 0.3d0
  V(2,4) = 1.0d0

  T = 0.0d0
  T(1,1) = 1.2d0
  T(2,1) = -0.3d0; T(2,2) = 0.8d0

  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 3.0d0
  A(2,1) = 4.0d0; A(2,2) = 5.0d0; A(2,3) = 6.0d0

  B = 0.0d0
  B(1,1) = 7.0d0;  B(1,2) = 8.0d0;  B(1,3) = 9.0d0
  B(2,1) = 10.0d0; B(2,2) = 11.0d0; B(2,3) = 12.0d0
  B(3,1) = 13.0d0; B(3,2) = 14.0d0; B(3,3) = 15.0d0
  B(4,1) = 16.0d0; B(4,2) = 17.0d0; B(4,3) = 18.0d0

  WORK = 0.0d0
  call DTPRFB('L', 'T', 'B', 'R', 4, 3, 2, 1, V, 8, T, 3, A, 4, B, 5, WORK, 5)
  call begin_test('row_bwd_left_trans')
  call print_matrix('A', A, 4, 2, 3)
  call print_matrix('B', B, 5, 4, 3)
  call end_test()

  ! ---------------- Test 10: R, Backward, Right, NoTrans ----------------
  V = 0.0d0
  V(1,1) = 0.2d0
  V(2,1) = 0.5d0
  V(1,3) = 1.0d0;  V(1,4) = 0.3d0
  V(2,4) = 1.0d0

  T = 0.0d0
  T(1,1) = 1.2d0
  T(2,1) = -0.3d0; T(2,2) = 0.8d0

  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0
  A(2,1) = 3.0d0; A(2,2) = 4.0d0
  A(3,1) = 5.0d0; A(3,2) = 6.0d0

  B = 0.0d0
  B(1,1) = 7.0d0;  B(1,2) = 8.0d0;  B(1,3) = 9.0d0;  B(1,4) = 10.0d0
  B(2,1) = 11.0d0; B(2,2) = 12.0d0; B(2,3) = 13.0d0; B(2,4) = 14.0d0
  B(3,1) = 15.0d0; B(3,2) = 16.0d0; B(3,3) = 17.0d0; B(3,4) = 18.0d0

  WORK = 0.0d0
  call DTPRFB('R', 'N', 'B', 'R', 3, 4, 2, 1, V, 8, T, 3, A, 4, B, 5, WORK, 5)
  call begin_test('row_bwd_right_notrans')
  call print_matrix('A', A, 4, 3, 2)
  call print_matrix('B', B, 5, 3, 4)
  call end_test()

  ! ---------------- Test 11: L=0 case (no trapezoidal part) ----------------
  ! Column forward left with L=0 -> V is M x K rectangular (plus the K x K upper
  ! triangular at top? actually with L=0, V has no trapezoidal block; V is K x K + (M-K) x K).
  V = 0.0d0
  V(1,1) = 1.0d0
  V(1,2) = 0.3d0
  V(2,2) = 1.0d0
  V(3,1) = 0.4d0; V(3,2) = 0.5d0
  V(4,1) = 0.6d0; V(4,2) = 0.7d0

  T = 0.0d0
  T(1,1) = 1.2d0; T(1,2) = -0.3d0
  T(2,2) = 0.8d0

  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 3.0d0
  A(2,1) = 4.0d0; A(2,2) = 5.0d0; A(2,3) = 6.0d0

  B = 0.0d0
  B(1,1) = 7.0d0;  B(1,2) = 8.0d0;  B(1,3) = 9.0d0
  B(2,1) = 10.0d0; B(2,2) = 11.0d0; B(2,3) = 12.0d0
  B(3,1) = 13.0d0; B(3,2) = 14.0d0; B(3,3) = 15.0d0
  B(4,1) = 16.0d0; B(4,2) = 17.0d0; B(4,3) = 18.0d0

  WORK = 0.0d0
  call DTPRFB('L', 'N', 'F', 'C', 4, 3, 2, 0, V, 8, T, 3, A, 4, B, 5, WORK, 5)
  call begin_test('col_fwd_left_notrans_l0')
  call print_matrix('A', A, 4, 2, 3)
  call print_matrix('B', B, 5, 4, 3)
  call end_test()

  ! ---------------- Test 12: Quick return M=0 ----------------
  A = 99.0d0
  B = 99.0d0
  call DTPRFB('L', 'N', 'F', 'C', 0, 3, 2, 0, V, 8, T, 3, A, 4, B, 5, WORK, 5)
  call begin_test('quick_return_m0')
  call print_matrix('A', A, 4, 1, 1)
  call end_test()

end program
