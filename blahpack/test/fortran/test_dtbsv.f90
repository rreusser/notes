program test_dtbsv
  use test_utils
  implicit none
  ! AB arrays with proper LDAB sizing
  double precision :: AB2(2, 10), AB3(3, 10), AB1(1, 10), x(10)

  ! ============================================================
  ! Test 1: Upper triangular, no transpose, non-unit, K=1, N=4, LDAB=2
  ! Upper band storage (K=1, LDAB=K+1=2):
  !   Row 1: superdiagonal [*, a12, a23, a34]
  !   Row 2: diagonal      [a11, a22, a33, a44]
  ! Full A = [2 3 0 0; 0 5 6 0; 0 0 8 9; 0 0 0 11]
  ! x_true = [1 1 1 1], b = A*x = [5, 11, 17, 11]
  AB2 = 0.0d0
  AB2(1,1) = 0.0d0; AB2(1,2) = 3.0d0; AB2(1,3) = 6.0d0; AB2(1,4) = 9.0d0
  AB2(2,1) = 2.0d0; AB2(2,2) = 5.0d0; AB2(2,3) = 8.0d0; AB2(2,4) = 11.0d0
  x(1) = 5.0d0; x(2) = 11.0d0; x(3) = 17.0d0; x(4) = 11.0d0
  call DTBSV('U', 'N', 'N', 4, 1, AB2, 2, x, 1)
  call begin_test('upper_N_nonunit_k1')
  call print_array('x', x, 4)
  call end_test()

  ! ============================================================
  ! Test 2: Lower triangular, no transpose, non-unit, K=1, N=4, LDAB=2
  ! Lower band storage:
  !   Row 1: diagonal      [a11, a22, a33, a44]
  !   Row 2: subdiagonal   [a21, a32, a43, *]
  ! Full A = [2 0 0 0; 1 5 0 0; 0 4 8 0; 0 0 7 11]
  ! x = [1 1 1 1], b = [2, 6, 12, 18]
  AB2 = 0.0d0
  AB2(1,1) = 2.0d0; AB2(1,2) = 5.0d0; AB2(1,3) = 8.0d0; AB2(1,4) = 11.0d0
  AB2(2,1) = 1.0d0; AB2(2,2) = 4.0d0; AB2(2,3) = 7.0d0; AB2(2,4) = 0.0d0
  x(1) = 2.0d0; x(2) = 6.0d0; x(3) = 12.0d0; x(4) = 18.0d0
  call DTBSV('L', 'N', 'N', 4, 1, AB2, 2, x, 1)
  call begin_test('lower_N_nonunit_k1')
  call print_array('x', x, 4)
  call end_test()

  ! ============================================================
  ! Test 3: Upper triangular, transpose, non-unit, K=1, N=4
  ! Same A as test 1.
  ! A^T = [2 0 0 0; 3 5 0 0; 0 6 8 0; 0 0 9 11]
  ! x = [1 1 1 1], b = A^T*x = [2, 8, 14, 20]
  AB2 = 0.0d0
  AB2(1,1) = 0.0d0; AB2(1,2) = 3.0d0; AB2(1,3) = 6.0d0; AB2(1,4) = 9.0d0
  AB2(2,1) = 2.0d0; AB2(2,2) = 5.0d0; AB2(2,3) = 8.0d0; AB2(2,4) = 11.0d0
  x(1) = 2.0d0; x(2) = 8.0d0; x(3) = 14.0d0; x(4) = 20.0d0
  call DTBSV('U', 'T', 'N', 4, 1, AB2, 2, x, 1)
  call begin_test('upper_T_nonunit_k1')
  call print_array('x', x, 4)
  call end_test()

  ! ============================================================
  ! Test 4: Lower triangular, transpose, non-unit, K=1, N=4
  ! Same A as test 2.
  ! A^T = [2 1 0 0; 0 5 4 0; 0 0 8 7; 0 0 0 11]
  ! x = [1 1 1 1], b = A^T*x = [3, 9, 15, 11]
  AB2 = 0.0d0
  AB2(1,1) = 2.0d0; AB2(1,2) = 5.0d0; AB2(1,3) = 8.0d0; AB2(1,4) = 11.0d0
  AB2(2,1) = 1.0d0; AB2(2,2) = 4.0d0; AB2(2,3) = 7.0d0; AB2(2,4) = 0.0d0
  x(1) = 3.0d0; x(2) = 9.0d0; x(3) = 15.0d0; x(4) = 11.0d0
  call DTBSV('L', 'T', 'N', 4, 1, AB2, 2, x, 1)
  call begin_test('lower_T_nonunit_k1')
  call print_array('x', x, 4)
  call end_test()

  ! ============================================================
  ! Test 5: Upper, no transpose, unit diagonal, K=1, N=4
  ! Full (unit diag): [1 3 0 0; 0 1 6 0; 0 0 1 9; 0 0 0 1]
  ! x = [1 1 1 1], b = [4, 7, 10, 1]
  AB2 = 0.0d0
  AB2(1,2) = 3.0d0; AB2(1,3) = 6.0d0; AB2(1,4) = 9.0d0
  AB2(2,1) = 99.0d0; AB2(2,2) = 99.0d0; AB2(2,3) = 99.0d0; AB2(2,4) = 99.0d0
  x(1) = 4.0d0; x(2) = 7.0d0; x(3) = 10.0d0; x(4) = 1.0d0
  call DTBSV('U', 'N', 'U', 4, 1, AB2, 2, x, 1)
  call begin_test('upper_N_unit_k1')
  call print_array('x', x, 4)
  call end_test()

  ! ============================================================
  ! Test 6: Lower, no transpose, unit diagonal, K=1, N=4
  ! Full (unit diag): [1 0 0 0; 1 1 0 0; 0 4 1 0; 0 0 7 1]
  ! x = [1 1 1 1], b = [1, 2, 5, 8]
  AB2 = 0.0d0
  AB2(1,1) = 99.0d0; AB2(1,2) = 99.0d0; AB2(1,3) = 99.0d0; AB2(1,4) = 99.0d0
  AB2(2,1) = 1.0d0; AB2(2,2) = 4.0d0; AB2(2,3) = 7.0d0; AB2(2,4) = 0.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 5.0d0; x(4) = 8.0d0
  call DTBSV('L', 'N', 'U', 4, 1, AB2, 2, x, 1)
  call begin_test('lower_N_unit_k1')
  call print_array('x', x, 4)
  call end_test()

  ! ============================================================
  ! Test 7: Upper, no transpose, non-unit, K=2, N=5, LDAB=3
  ! Band storage (K=2):
  !   Row 1: 2nd superdiag [*, *, a13, a24, a35]
  !   Row 2: 1st superdiag [*, a12, a23, a34, a45]
  !   Row 3: diagonal      [a11, a22, a33, a44, a55]
  ! Full:
  !   [1  2  3  0  0]
  !   [0  4  5  6  0]
  !   [0  0  7  8  9]
  !   [0  0  0 10 11]
  !   [0  0  0  0 12]
  ! x = [1 1 1 1 1], b = [6, 15, 24, 21, 12]
  AB3 = 0.0d0
  AB3(3,1) = 1.0d0
  AB3(2,2) = 2.0d0; AB3(3,2) = 4.0d0
  AB3(1,3) = 3.0d0; AB3(2,3) = 5.0d0; AB3(3,3) = 7.0d0
  AB3(1,4) = 6.0d0; AB3(2,4) = 8.0d0; AB3(3,4) = 10.0d0
  AB3(1,5) = 9.0d0; AB3(2,5) = 11.0d0; AB3(3,5) = 12.0d0
  x(1) = 6.0d0; x(2) = 15.0d0; x(3) = 24.0d0; x(4) = 21.0d0; x(5) = 12.0d0
  call DTBSV('U', 'N', 'N', 5, 2, AB3, 3, x, 1)
  call begin_test('upper_N_nonunit_k2')
  call print_array('x', x, 5)
  call end_test()

  ! ============================================================
  ! Test 8: N=1 edge case
  AB1 = 0.0d0
  AB1(1,1) = 3.0d0
  x(1) = 9.0d0
  call DTBSV('U', 'N', 'N', 1, 0, AB1, 1, x, 1)
  call begin_test('n_one')
  call print_array('x', x, 1)
  call end_test()

  ! ============================================================
  ! Test 9: Non-unit stride (incx=2), Upper, K=1, N=3
  ! Full: [2 1 0; 0 3 1; 0 0 4]
  ! x = [1 1 1], b = [3, 4, 4]
  AB2 = 0.0d0
  AB2(1,1) = 0.0d0; AB2(1,2) = 1.0d0; AB2(1,3) = 1.0d0
  AB2(2,1) = 2.0d0; AB2(2,2) = 3.0d0; AB2(2,3) = 4.0d0
  x = 0.0d0
  x(1) = 3.0d0; x(3) = 4.0d0; x(5) = 4.0d0
  call DTBSV('U', 'N', 'N', 3, 1, AB2, 2, x, 2)
  call begin_test('upper_stride2')
  call print_array('x', x, 5)
  call end_test()

  ! ============================================================
  ! Test 10: Negative stride (incx=-1), Lower, K=1, N=3
  ! Full: [2 0 0; 3 4 0; 0 5 6]
  ! x = [1 1 1], b = [2, 7, 11]
  ! incx=-1: x(3)=b(1)=2, x(2)=b(2)=7, x(1)=b(3)=11
  AB2 = 0.0d0
  AB2(1,1) = 2.0d0; AB2(1,2) = 4.0d0; AB2(1,3) = 6.0d0
  AB2(2,1) = 3.0d0; AB2(2,2) = 5.0d0; AB2(2,3) = 0.0d0
  x = 0.0d0
  x(1) = 11.0d0; x(2) = 7.0d0; x(3) = 2.0d0
  call DTBSV('L', 'N', 'N', 3, 1, AB2, 2, x, -1)
  call begin_test('lower_neg_stride')
  call print_array('x', x, 3)
  call end_test()

  ! ============================================================
  ! Test 11: Lower, K=2, N=5, transpose, non-unit, LDAB=3
  ! Lower band storage:
  !   Row 1: diagonal      [a11, a22, a33, a44, a55]
  !   Row 2: 1st subdiag   [a21, a32, a43, a54, *]
  !   Row 3: 2nd subdiag   [a31, a42, a53, *, *]
  ! Full L: [2 0 0 0 0; 1 4 0 0 0; 3 5 6 0 0; 0 2 7 8 0; 0 0 1 3 10]
  ! L^T: [2 1 3 0 0; 0 4 5 2 0; 0 0 6 7 1; 0 0 0 8 3; 0 0 0 0 10]
  ! x = [1 1 1 1 1], b = L^T*x = [6, 11, 14, 11, 10]
  AB3 = 0.0d0
  AB3(1,1) = 2.0d0; AB3(1,2) = 4.0d0; AB3(1,3) = 6.0d0; AB3(1,4) = 8.0d0; AB3(1,5) = 10.0d0
  AB3(2,1) = 1.0d0; AB3(2,2) = 5.0d0; AB3(2,3) = 7.0d0; AB3(2,4) = 3.0d0; AB3(2,5) = 0.0d0
  AB3(3,1) = 3.0d0; AB3(3,2) = 2.0d0; AB3(3,3) = 1.0d0; AB3(3,4) = 0.0d0; AB3(3,5) = 0.0d0
  x(1) = 6.0d0; x(2) = 11.0d0; x(3) = 14.0d0; x(4) = 11.0d0; x(5) = 10.0d0
  call DTBSV('L', 'T', 'N', 5, 2, AB3, 3, x, 1)
  call begin_test('lower_T_nonunit_k2')
  call print_array('x', x, 5)
  call end_test()

  ! ============================================================
  ! Test 12: Upper, transpose, non-unit, K=2, N=5, stride=2
  ! Same A as test 7
  ! A^T: [1 0 0 0 0; 2 4 0 0 0; 3 5 7 0 0; 0 6 8 10 0; 0 0 9 11 12]
  ! x = [1 1 1 1 1], b = A^T*x = [1, 6, 15, 24, 32]
  AB3 = 0.0d0
  AB3(3,1) = 1.0d0
  AB3(2,2) = 2.0d0; AB3(3,2) = 4.0d0
  AB3(1,3) = 3.0d0; AB3(2,3) = 5.0d0; AB3(3,3) = 7.0d0
  AB3(1,4) = 6.0d0; AB3(2,4) = 8.0d0; AB3(3,4) = 10.0d0
  AB3(1,5) = 9.0d0; AB3(2,5) = 11.0d0; AB3(3,5) = 12.0d0
  x = 0.0d0
  x(1) = 1.0d0; x(3) = 6.0d0; x(5) = 15.0d0; x(7) = 24.0d0; x(9) = 32.0d0
  call DTBSV('U', 'T', 'N', 5, 2, AB3, 3, x, 2)
  call begin_test('upper_T_nonunit_k2_stride2')
  call print_array('x', x, 9)
  call end_test()

end program
