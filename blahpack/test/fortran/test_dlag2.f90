program test_dlag2
  use test_utils
  implicit none

  double precision :: A(2,2), B(2,2)
  double precision :: SAFMIN, SCALE1, SCALE2, WR1, WR2, WI

  SAFMIN = 2.2250738585072014D-308

  ! Test 1: Real eigenvalues, basic case
  ! A = [4 1; 2 3], B = I
  A(1,1) = 4.0d0; A(2,1) = 2.0d0; A(1,2) = 1.0d0; A(2,2) = 3.0d0
  B(1,1) = 1.0d0; B(2,1) = 0.0d0; B(1,2) = 0.0d0; B(2,2) = 1.0d0
  call DLAG2(A, 2, B, 2, SAFMIN, SCALE1, SCALE2, WR1, WR2, WI)
  call begin_test('real_eigenvalues_identity_B')
  call print_scalar('scale1', SCALE1)
  call print_scalar('scale2', SCALE2)
  call print_scalar('wr1', WR1)
  call print_scalar('wr2', WR2)
  call print_scalar('wi', WI)
  call end_test()

  ! Test 2: Complex eigenvalues
  ! A = [1 -5; 2 1], B = I
  A(1,1) = 1.0d0; A(2,1) = 2.0d0; A(1,2) = -5.0d0; A(2,2) = 1.0d0
  B(1,1) = 1.0d0; B(2,1) = 0.0d0; B(1,2) = 0.0d0; B(2,2) = 1.0d0
  call DLAG2(A, 2, B, 2, SAFMIN, SCALE1, SCALE2, WR1, WR2, WI)
  call begin_test('complex_eigenvalues')
  call print_scalar('scale1', SCALE1)
  call print_scalar('scale2', SCALE2)
  call print_scalar('wr1', WR1)
  call print_scalar('wr2', WR2)
  call print_scalar('wi', WI)
  call end_test()

  ! Test 3: Diagonal A and B
  ! A = [5 0; 0 3], B = [2 0; 0 1]
  A(1,1) = 5.0d0; A(2,1) = 0.0d0; A(1,2) = 0.0d0; A(2,2) = 3.0d0
  B(1,1) = 2.0d0; B(2,1) = 0.0d0; B(1,2) = 0.0d0; B(2,2) = 1.0d0
  call DLAG2(A, 2, B, 2, SAFMIN, SCALE1, SCALE2, WR1, WR2, WI)
  call begin_test('diagonal')
  call print_scalar('scale1', SCALE1)
  call print_scalar('scale2', SCALE2)
  call print_scalar('wr1', WR1)
  call print_scalar('wr2', WR2)
  call print_scalar('wi', WI)
  call end_test()

  ! Test 4: Non-trivial B (upper triangular)
  ! A = [3 1; 1 2], B = [2 1; 0 3]
  A(1,1) = 3.0d0; A(2,1) = 1.0d0; A(1,2) = 1.0d0; A(2,2) = 2.0d0
  B(1,1) = 2.0d0; B(2,1) = 0.0d0; B(1,2) = 1.0d0; B(2,2) = 3.0d0
  call DLAG2(A, 2, B, 2, SAFMIN, SCALE1, SCALE2, WR1, WR2, WI)
  call begin_test('upper_tri_B')
  call print_scalar('scale1', SCALE1)
  call print_scalar('scale2', SCALE2)
  call print_scalar('wr1', WR1)
  call print_scalar('wr2', WR2)
  call print_scalar('wi', WI)
  call end_test()

  ! Test 5: B with very small diagonal (triggers perturbation)
  A(1,1) = 1.0d0; A(2,1) = 1.0d0; A(1,2) = 1.0d0; A(2,2) = 1.0d0
  B(1,1) = 1.0d-200; B(2,1) = 0.0d0; B(1,2) = 0.0d0; B(2,2) = 1.0d0
  call DLAG2(A, 2, B, 2, SAFMIN, SCALE1, SCALE2, WR1, WR2, WI)
  call begin_test('small_B_diagonal')
  call print_scalar('scale1', SCALE1)
  call print_scalar('scale2', SCALE2)
  call print_scalar('wr1', WR1)
  call print_scalar('wr2', WR2)
  call print_scalar('wi', WI)
  call end_test()

  ! Test 6: |s1| <= |s2| branch (s1 closer to zero)
  ! A = [0.1 3; 2 5], B = I
  A(1,1) = 0.1d0; A(2,1) = 2.0d0; A(1,2) = 3.0d0; A(2,2) = 5.0d0
  B(1,1) = 1.0d0; B(2,1) = 0.0d0; B(1,2) = 0.0d0; B(2,2) = 1.0d0
  call DLAG2(A, 2, B, 2, SAFMIN, SCALE1, SCALE2, WR1, WR2, WI)
  call begin_test('s1_leq_s2')
  call print_scalar('scale1', SCALE1)
  call print_scalar('scale2', SCALE2)
  call print_scalar('wr1', WR1)
  call print_scalar('wr2', WR2)
  call print_scalar('wi', WI)
  call end_test()

  ! Test 7: |s1| > |s2| branch
  ! A = [10 1; 1 0.5], B = I
  A(1,1) = 10.0d0; A(2,1) = 1.0d0; A(1,2) = 1.0d0; A(2,2) = 0.5d0
  B(1,1) = 1.0d0; B(2,1) = 0.0d0; B(1,2) = 0.0d0; B(2,2) = 1.0d0
  call DLAG2(A, 2, B, 2, SAFMIN, SCALE1, SCALE2, WR1, WR2, WI)
  call begin_test('s1_gt_s2')
  call print_scalar('scale1', SCALE1)
  call print_scalar('scale2', SCALE2)
  call print_scalar('wr1', WR1)
  call print_scalar('wr2', WR2)
  call print_scalar('wi', WI)
  call end_test()

  ! Test 8: Large A values (tests scaling)
  A(1,1) = 1.0d100; A(2,1) = 2.0d100; A(1,2) = 3.0d100; A(2,2) = 4.0d100
  B(1,1) = 1.0d0; B(2,1) = 0.0d0; B(1,2) = 0.0d0; B(2,2) = 1.0d0
  call DLAG2(A, 2, B, 2, SAFMIN, SCALE1, SCALE2, WR1, WR2, WI)
  call begin_test('large_A')
  call print_scalar('scale1', SCALE1)
  call print_scalar('scale2', SCALE2)
  call print_scalar('wr1', WR1)
  call print_scalar('wr2', WR2)
  call print_scalar('wi', WI)
  call end_test()

  ! Test 9: Negative eigenvalues
  ! A = [-2 1; 1 -3], B = I
  A(1,1) = -2.0d0; A(2,1) = 1.0d0; A(1,2) = 1.0d0; A(2,2) = -3.0d0
  B(1,1) = 1.0d0; B(2,1) = 0.0d0; B(1,2) = 0.0d0; B(2,2) = 1.0d0
  call DLAG2(A, 2, B, 2, SAFMIN, SCALE1, SCALE2, WR1, WR2, WI)
  call begin_test('negative_eigenvalues')
  call print_scalar('scale1', SCALE1)
  call print_scalar('scale2', SCALE2)
  call print_scalar('wr1', WR1)
  call print_scalar('wr2', WR2)
  call print_scalar('wi', WI)
  call end_test()

  ! Test 10: pp > abi22 branch (WR1 = min, WR2 = max)
  ! A = [6 1; 0.1 2], B = I
  A(1,1) = 6.0d0; A(2,1) = 0.1d0; A(1,2) = 1.0d0; A(2,2) = 2.0d0
  B(1,1) = 1.0d0; B(2,1) = 0.0d0; B(1,2) = 0.0d0; B(2,2) = 1.0d0
  call DLAG2(A, 2, B, 2, SAFMIN, SCALE1, SCALE2, WR1, WR2, WI)
  call begin_test('pp_gt_abi22')
  call print_scalar('scale1', SCALE1)
  call print_scalar('scale2', SCALE2)
  call print_scalar('wr1', WR1)
  call print_scalar('wr2', WR2)
  call print_scalar('wi', WI)
  call end_test()

  ! Test 11: Very small A values (tests underflow scaling)
  A(1,1) = 1.0d-200; A(2,1) = 2.0d-200; A(1,2) = 3.0d-200; A(2,2) = 4.0d-200
  B(1,1) = 1.0d0; B(2,1) = 0.0d0; B(1,2) = 0.0d0; B(2,2) = 1.0d0
  call DLAG2(A, 2, B, 2, SAFMIN, SCALE1, SCALE2, WR1, WR2, WI)
  call begin_test('small_A')
  call print_scalar('scale1', SCALE1)
  call print_scalar('scale2', SCALE2)
  call print_scalar('wr1', WR1)
  call print_scalar('wr2', WR2)
  call print_scalar('wi', WI)
  call end_test()

  ! Test 12: Both B diagonals small (triggers both perturbations)
  A(1,1) = 1.0d0; A(2,1) = 0.5d0; A(1,2) = 0.5d0; A(2,2) = 1.0d0
  B(1,1) = 1.0d-200; B(2,1) = 0.0d0; B(1,2) = 0.0d0; B(2,2) = 1.0d-200
  call DLAG2(A, 2, B, 2, SAFMIN, SCALE1, SCALE2, WR1, WR2, WI)
  call begin_test('both_B_diag_small')
  call print_scalar('scale1', SCALE1)
  call print_scalar('scale2', SCALE2)
  call print_scalar('wr1', WR1)
  call print_scalar('wr2', WR2)
  call print_scalar('wi', WI)
  call end_test()

  ! Test 13: B with off-diagonal element
  ! A = [2 0; 0 3], B = [1 0.5; 0 1]
  A(1,1) = 2.0d0; A(2,1) = 0.0d0; A(1,2) = 0.0d0; A(2,2) = 3.0d0
  B(1,1) = 1.0d0; B(2,1) = 0.0d0; B(1,2) = 0.5d0; B(2,2) = 1.0d0
  call DLAG2(A, 2, B, 2, SAFMIN, SCALE1, SCALE2, WR1, WR2, WI)
  call begin_test('B_offdiag')
  call print_scalar('scale1', SCALE1)
  call print_scalar('scale2', SCALE2)
  call print_scalar('wr1', WR1)
  call print_scalar('wr2', WR2)
  call print_scalar('wi', WI)
  call end_test()

  ! Test 14: wsize != 1 branch, wsize > 1
  ! Large eigenvalue scaling
  A(1,1) = 1.0d150; A(2,1) = 0.0d0; A(1,2) = 0.0d0; A(2,2) = 2.0d150
  B(1,1) = 1.0d-10; B(2,1) = 0.0d0; B(1,2) = 0.0d0; B(2,2) = 1.0d-10
  call DLAG2(A, 2, B, 2, SAFMIN, SCALE1, SCALE2, WR1, WR2, WI)
  call begin_test('large_eigenvalue_scaling')
  call print_scalar('scale1', SCALE1)
  call print_scalar('scale2', SCALE2)
  call print_scalar('wr1', WR1)
  call print_scalar('wr2', WR2)
  call print_scalar('wi', WI)
  call end_test()

  ! Test 15: Complex eigenvalues with non-trivial B
  A(1,1) = 1.0d0; A(2,1) = 3.0d0; A(1,2) = -2.0d0; A(2,2) = 1.0d0
  B(1,1) = 2.0d0; B(2,1) = 0.0d0; B(1,2) = 1.0d0; B(2,2) = 2.0d0
  call DLAG2(A, 2, B, 2, SAFMIN, SCALE1, SCALE2, WR1, WR2, WI)
  call begin_test('complex_nontrivial_B')
  call print_scalar('scale1', SCALE1)
  call print_scalar('scale2', SCALE2)
  call print_scalar('wr1', WR1)
  call print_scalar('wr2', WR2)
  call print_scalar('wi', WI)
  call end_test()

  ! Test 16: Negative B diagonal (sign perturbation)
  A(1,1) = 3.0d0; A(2,1) = 1.0d0; A(1,2) = 1.0d0; A(2,2) = 2.0d0
  B(1,1) = -1.0d0; B(2,1) = 0.0d0; B(1,2) = 0.0d0; B(2,2) = 1.0d0
  call DLAG2(A, 2, B, 2, SAFMIN, SCALE1, SCALE2, WR1, WR2, WI)
  call begin_test('negative_B_diag')
  call print_scalar('scale1', SCALE1)
  call print_scalar('scale2', SCALE2)
  call print_scalar('wr1', WR1)
  call print_scalar('wr2', WR2)
  call print_scalar('wi', WI)
  call end_test()

  ! Test 17: b22 small (triggers b22 perturbation, b11 is large)
  A(1,1) = 2.0d0; A(2,1) = 1.0d0; A(1,2) = 1.0d0; A(2,2) = 3.0d0
  B(1,1) = 1.0d0; B(2,1) = 0.0d0; B(1,2) = 0.0d0; B(2,2) = 1.0d-200
  call DLAG2(A, 2, B, 2, SAFMIN, SCALE1, SCALE2, WR1, WR2, WI)
  call begin_test('b22_small')
  call print_scalar('scale1', SCALE1)
  call print_scalar('scale2', SCALE2)
  call print_scalar('wr1', WR1)
  call print_scalar('wr2', WR2)
  call print_scalar('wi', WI)
  call end_test()

  ! Test 18: Very large pp (triggers |pp*rtmin| >= 1 branch)
  ! Need large a11/b11 ratio (ascale * A entries large) and small b22
  A(1,1) = 1.0d0; A(2,1) = 0.0d0; A(1,2) = 0.0d0; A(2,2) = 1.0d0
  B(1,1) = 1.0d-155; B(2,1) = 0.0d0; B(1,2) = 0.0d0; B(2,2) = 1.0d-155
  call DLAG2(A, 2, B, 2, SAFMIN, SCALE1, SCALE2, WR1, WR2, WI)
  call begin_test('large_pp')
  call print_scalar('scale1', SCALE1)
  call print_scalar('scale2', SCALE2)
  call print_scalar('wr1', WR1)
  call print_scalar('wr2', WR2)
  call print_scalar('wi', WI)
  call end_test()

  ! Test 19: Very small pp and qq (triggers pp^2+|qq| <= safmin branch)
  A(1,1) = 1.0d-200; A(2,1) = 1.0d-200; A(1,2) = 1.0d-200; A(2,2) = 1.0d-200
  B(1,1) = 1.0d0; B(2,1) = 0.0d0; B(1,2) = 1.0d0; B(2,2) = 1.0d0
  call DLAG2(A, 2, B, 2, SAFMIN, SCALE1, SCALE2, WR1, WR2, WI)
  call begin_test('tiny_pp_qq')
  call print_scalar('scale1', SCALE1)
  call print_scalar('scale2', SCALE2)
  call print_scalar('wr1', WR1)
  call print_scalar('wr2', WR2)
  call print_scalar('wi', WI)
  call end_test()

  ! Test 20: wsize == 1 (ascale * bsize == 1 naturally)
  ! A = I, B = I => ascale = 0.5, bsize = 1 => eigenvalues are 1
  A(1,1) = 1.0d0; A(2,1) = 0.0d0; A(1,2) = 0.0d0; A(2,2) = 1.0d0
  B(1,1) = 1.0d0; B(2,1) = 0.0d0; B(1,2) = 0.0d0; B(2,2) = 1.0d0
  call DLAG2(A, 2, B, 2, SAFMIN, SCALE1, SCALE2, WR1, WR2, WI)
  call begin_test('identity')
  call print_scalar('scale1', SCALE1)
  call print_scalar('scale2', SCALE2)
  call print_scalar('wr1', WR1)
  call print_scalar('wr2', WR2)
  call print_scalar('wi', WI)
  call end_test()

  ! Test 21: ascale > 1 and bsize > 1 (c5 = 1 branch)
  ! Large A values with large B values
  A(1,1) = 0.4d0; A(2,1) = 0.0d0; A(1,2) = 0.0d0; A(2,2) = 0.3d0
  B(1,1) = 2.0d0; B(2,1) = 0.0d0; B(1,2) = 0.0d0; B(2,2) = 3.0d0
  call DLAG2(A, 2, B, 2, SAFMIN, SCALE1, SCALE2, WR1, WR2, WI)
  call begin_test('ascale_gt1_bsize_gt1')
  call print_scalar('scale1', SCALE1)
  call print_scalar('scale2', SCALE2)
  call print_scalar('wr1', WR1)
  call print_scalar('wr2', WR2)
  call print_scalar('wi', WI)
  call end_test()

  ! Test 22: wsize > 1 for first eigenvalue (large wabs*c2)
  A(1,1) = 1.0d0; A(2,1) = 0.0d0; A(1,2) = 0.0d0; A(2,2) = 0.5d0
  B(1,1) = 1.0d-100; B(2,1) = 0.0d0; B(1,2) = 0.0d0; B(2,2) = 1.0d-100
  call DLAG2(A, 2, B, 2, SAFMIN, SCALE1, SCALE2, WR1, WR2, WI)
  call begin_test('wsize_gt1_eigenvalue1')
  call print_scalar('scale1', SCALE1)
  call print_scalar('scale2', SCALE2)
  call print_scalar('wr1', WR1)
  call print_scalar('wr2', WR2)
  call print_scalar('wi', WI)
  call end_test()

end program
