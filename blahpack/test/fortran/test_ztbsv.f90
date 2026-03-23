program test_ztbsv
  use test_utils
  implicit none
  ! Use separate arrays with proper LDA for each test
  double precision :: x_r(20)
  complex*16 :: x(10)
  equivalence (x, x_r)

  ! LDA=2, 4 columns
  double precision :: A2_r(40)
  complex*16 :: A2(2, 5)
  equivalence (A2, A2_r)

  ! LDA=3, 4 columns
  double precision :: A3_r(40)
  complex*16 :: A3(3, 5)
  equivalence (A3, A3_r)

  ! ============================================================
  ! Test 1: Upper, No transpose, Non-unit, N=3, K=1
  ! Band storage LDA=K+1=2:
  !   Row 1 (superdiag): [*   2+0i  3+0i]
  !   Row 2 (diag):      [4+0i  5+0i  6+0i]
  ! Full A: [4  2  0; 0  5  3; 0  0  6]
  ! Solve Ax = b with b = [10, 19, 18]
  A2 = (0.0d0, 0.0d0)
  A2(1,1) = (0.0d0,0.0d0); A2(2,1) = (4.0d0,0.0d0)
  A2(1,2) = (2.0d0,0.0d0); A2(2,2) = (5.0d0,0.0d0)
  A2(1,3) = (3.0d0,0.0d0); A2(2,3) = (6.0d0,0.0d0)
  x(1) = (10.0d0,0.0d0); x(2) = (19.0d0,0.0d0); x(3) = (18.0d0,0.0d0)
  call ZTBSV('U', 'N', 'N', 3, 1, A2, 2, x, 1)
  call begin_test('upper_notrans_nonunit')
  call print_array('x', x_r, 6)
  call end_test()

  ! ============================================================
  ! Test 2: Lower, No transpose, Non-unit, N=3, K=1
  ! Band storage LDA=2:
  !   Row 1 (diag):    [2+0i  3+0i  4+0i]
  !   Row 2 (subdiag): [1+0i  1+0i  *]
  A2 = (0.0d0, 0.0d0)
  A2(1,1) = (2.0d0,0.0d0); A2(2,1) = (1.0d0,0.0d0)
  A2(1,2) = (3.0d0,0.0d0); A2(2,2) = (1.0d0,0.0d0)
  A2(1,3) = (4.0d0,0.0d0); A2(2,3) = (0.0d0,0.0d0)
  x(1) = (2.0d0,0.0d0); x(2) = (7.0d0,0.0d0); x(3) = (14.0d0,0.0d0)
  call ZTBSV('L', 'N', 'N', 3, 1, A2, 2, x, 1)
  call begin_test('lower_notrans_nonunit')
  call print_array('x', x_r, 6)
  call end_test()

  ! ============================================================
  ! Test 3: Upper, Transpose, Non-unit, N=3, K=1
  A2 = (0.0d0, 0.0d0)
  A2(1,1) = (0.0d0,0.0d0); A2(2,1) = (4.0d0,0.0d0)
  A2(1,2) = (2.0d0,0.0d0); A2(2,2) = (5.0d0,0.0d0)
  A2(1,3) = (3.0d0,0.0d0); A2(2,3) = (6.0d0,0.0d0)
  x(1) = (4.0d0,0.0d0); x(2) = (12.0d0,0.0d0); x(3) = (24.0d0,0.0d0)
  call ZTBSV('U', 'T', 'N', 3, 1, A2, 2, x, 1)
  call begin_test('upper_trans_nonunit')
  call print_array('x', x_r, 6)
  call end_test()

  ! ============================================================
  ! Test 4: Upper, Conjugate transpose, Non-unit, N=3, K=1
  A2 = (0.0d0, 0.0d0)
  A2(1,1) = (0.0d0,0.0d0); A2(2,1) = (2.0d0,0.0d0)
  A2(1,2) = (1.0d0,1.0d0); A2(2,2) = (3.0d0,0.0d0)
  A2(1,3) = (2.0d0,-1.0d0); A2(2,3) = (4.0d0,0.0d0)
  x(1) = (2.0d0,0.0d0); x(2) = (4.0d0,-1.0d0); x(3) = (6.0d0,1.0d0)
  call ZTBSV('U', 'C', 'N', 3, 1, A2, 2, x, 1)
  call begin_test('upper_conjtrans_nonunit')
  call print_array('x', x_r, 6)
  call end_test()

  ! ============================================================
  ! Test 5: Lower, Transpose, Non-unit, N=3, K=1
  A2 = (0.0d0, 0.0d0)
  A2(1,1) = (2.0d0,0.0d0); A2(2,1) = (1.0d0,0.0d0)
  A2(1,2) = (3.0d0,0.0d0); A2(2,2) = (1.0d0,0.0d0)
  A2(1,3) = (4.0d0,0.0d0); A2(2,3) = (0.0d0,0.0d0)
  x(1) = (4.0d0,0.0d0); x(2) = (9.0d0,0.0d0); x(3) = (12.0d0,0.0d0)
  call ZTBSV('L', 'T', 'N', 3, 1, A2, 2, x, 1)
  call begin_test('lower_trans_nonunit')
  call print_array('x', x_r, 6)
  call end_test()

  ! ============================================================
  ! Test 6: Lower, Conjugate transpose, Non-unit, N=3, K=1
  A2 = (0.0d0, 0.0d0)
  A2(1,1) = (2.0d0,0.0d0); A2(2,1) = (1.0d0,1.0d0)
  A2(1,2) = (3.0d0,1.0d0); A2(2,2) = (2.0d0,-1.0d0)
  A2(1,3) = (4.0d0,0.0d0); A2(2,3) = (0.0d0,0.0d0)
  x(1) = (3.0d0,-1.0d0); x(2) = (5.0d0,0.0d0); x(3) = (4.0d0,0.0d0)
  call ZTBSV('L', 'C', 'N', 3, 1, A2, 2, x, 1)
  call begin_test('lower_conjtrans_nonunit')
  call print_array('x', x_r, 6)
  call end_test()

  ! ============================================================
  ! Test 7: Unit diagonal, Upper, N=3, K=1
  A2 = (0.0d0, 0.0d0)
  A2(1,1) = (0.0d0,0.0d0); A2(2,1) = (99.0d0,0.0d0)
  A2(1,2) = (2.0d0,0.0d0); A2(2,2) = (99.0d0,0.0d0)
  A2(1,3) = (3.0d0,0.0d0); A2(2,3) = (99.0d0,0.0d0)
  x(1) = (7.0d0,0.0d0); x(2) = (7.0d0,0.0d0); x(3) = (1.0d0,0.0d0)
  call ZTBSV('U', 'N', 'U', 3, 1, A2, 2, x, 1)
  call begin_test('upper_unit')
  call print_array('x', x_r, 6)
  call end_test()

  ! ============================================================
  ! Test 8: N=0 quick return
  call ZTBSV('U', 'N', 'N', 0, 1, A2, 2, x, 1)
  call begin_test('n_zero')
  call print_array('x', x_r, 6)
  call end_test()

  ! ============================================================
  ! Test 9: Complex entries, Upper, N=3, K=1
  A2 = (0.0d0, 0.0d0)
  A2(1,1) = (0.0d0,0.0d0); A2(2,1) = (3.0d0,0.0d0)
  A2(1,2) = (1.0d0,1.0d0); A2(2,2) = (2.0d0,1.0d0)
  A2(1,3) = (2.0d0,-1.0d0); A2(2,3) = (4.0d0,-1.0d0)
  x(1) = (3.0d0,2.0d0); x(2) = (5.0d0,1.0d0); x(3) = (8.0d0,-2.0d0)
  call ZTBSV('U', 'N', 'N', 3, 1, A2, 2, x, 1)
  call begin_test('upper_complex')
  call print_array('x', x_r, 6)
  call end_test()

  ! ============================================================
  ! Test 10: K=2 (2 superdiagonals), Upper, N=4, LDA=3
  A3 = (0.0d0, 0.0d0)
  A3(1,1) = (0.0d0,0.0d0); A3(2,1) = (0.0d0,0.0d0); A3(3,1) = (4.0d0,0.0d0)
  A3(1,2) = (0.0d0,0.0d0); A3(2,2) = (2.0d0,0.0d0); A3(3,2) = (4.0d0,0.0d0)
  A3(1,3) = (1.0d0,0.0d0); A3(2,3) = (2.0d0,0.0d0); A3(3,3) = (4.0d0,0.0d0)
  A3(1,4) = (1.0d0,0.0d0); A3(2,4) = (2.0d0,0.0d0); A3(3,4) = (4.0d0,0.0d0)
  x(1) = (7.0d0,0.0d0); x(2) = (7.0d0,0.0d0); x(3) = (6.0d0,0.0d0); x(4) = (4.0d0,0.0d0)
  call ZTBSV('U', 'N', 'N', 4, 2, A3, 3, x, 1)
  call begin_test('upper_k2')
  call print_array('x', x_r, 8)
  call end_test()

end program
