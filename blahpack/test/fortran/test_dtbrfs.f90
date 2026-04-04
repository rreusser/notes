program test_dtbrfs
  use test_utils
  implicit none
  double precision :: ab(100), b(100), x(100), ferr(10), berr(10), work(300)
  integer :: iwork(100), info, n, kd, nrhs, ldab, ldb, ldx

  ! Test 1: 4x4 upper triangular band (kd=2), no transpose, non-unit
  ! A = [2 1 3 0; 0 4 5 2; 0 0 6 1; 0 0 0 3]
  ! Band storage (upper, kd=2, ldab=3):
  !   Row 0 (2nd superdiag): *  *  3  2    -> ab(1,3)=3, ab(1,4)=2
  !   Row 1 (1st superdiag): *  1  5  1    -> ab(2,2)=1, ab(2,3)=5, ab(2,4)=1
  !   Row 2 (diagonal):      2  4  6  3    -> ab(3,1)=2, ab(3,2)=4, ab(3,3)=6, ab(3,4)=3
  ! In column-major: col1=[*,*,2], col2=[*,1,4], col3=[3,5,6], col4=[2,1,3]
  ! Fortran: ab(kd+1+i-j, j) = A(i,j) for max(1,j-kd)<=i<=j
  n = 4; kd = 2; nrhs = 1; ldab = 3; ldb = 4; ldx = 4
  ab = 0.0d0; b = 0.0d0; x = 0.0d0
  ! col 1: ab(3,1) = A(1,1) = 2
  ab(3) = 2.0d0
  ! col 2: ab(2,2) = A(1,2) = 1, ab(3,2) = A(2,2) = 4
  ab(5) = 1.0d0; ab(6) = 4.0d0
  ! col 3: ab(1,3) = A(1,3) = 3, ab(2,3) = A(2,3) = 5, ab(3,3) = A(3,3) = 6
  ab(7) = 3.0d0; ab(8) = 5.0d0; ab(9) = 6.0d0
  ! col 4: ab(1,4) = A(2,4) = 2, ab(2,4) = A(3,4) = 1, ab(3,4) = A(4,4) = 3
  ab(10) = 2.0d0; ab(11) = 1.0d0; ab(12) = 3.0d0

  ! b = A * [1;2;3;4] = [2+2+9; 8+15+8; 18+4; 12] = [13; 31; 22; 12]
  b(1) = 13.0d0; b(2) = 31.0d0; b(3) = 22.0d0; b(4) = 12.0d0
  x(1) = b(1); x(2) = b(2); x(3) = b(3); x(4) = b(4)
  call dtbsv('U', 'N', 'N', n, kd, ab, ldab, x, 1)
  call dtbrfs('U', 'N', 'N', n, kd, nrhs, ab, ldab, b, ldb, x, ldx, &
              ferr, berr, work, iwork, info)
  call begin_test('upper_no_trans')
  call print_array('x', x, n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 2: 4x4 lower triangular band (kd=2), no transpose, non-unit
  ! A = [3 0 0 0; 2 5 0 0; 1 4 7 0; 0 6 2 8]
  ! Band storage (lower, kd=2, ldab=3):
  !   Row 0 (diagonal):      3  5  7  8    -> ab(1,j)=A(j,j)
  !   Row 1 (1st subdiag):   2  4  2  *    -> ab(2,j)=A(j+1,j)
  !   Row 2 (2nd subdiag):   1  6  *  *    -> ab(3,j)=A(j+2,j)
  ! ab(1+i-j,j) = A(i,j) for j<=i<=min(n,j+kd)
  n = 4; kd = 2; nrhs = 1; ldab = 3; ldb = 4; ldx = 4
  ab = 0.0d0; b = 0.0d0; x = 0.0d0
  ! col 1: ab(1,1)=3, ab(2,1)=2, ab(3,1)=1
  ab(1) = 3.0d0; ab(2) = 2.0d0; ab(3) = 1.0d0
  ! col 2: ab(1,2)=5, ab(2,2)=4, ab(3,2)=6
  ab(4) = 5.0d0; ab(5) = 4.0d0; ab(6) = 6.0d0
  ! col 3: ab(1,3)=7, ab(2,3)=2
  ab(7) = 7.0d0; ab(8) = 2.0d0
  ! col 4: ab(1,4)=8
  ab(10) = 8.0d0

  ! b = A * [1;2;3;4] = [3; 12; 30; 46]
  ! 3*1 = 3
  ! 2*1+5*2 = 12
  ! 1*1+4*2+7*3 = 30
  ! 6*2+2*3+8*4 = 18+6+32 = 44 (wait, let me recalc)
  ! A(4,2)=6, A(4,3)=2, A(4,4)=8
  ! row4: 6*2+2*3+8*4 = 12+6+32 = 50
  b(1) = 3.0d0; b(2) = 12.0d0; b(3) = 30.0d0; b(4) = 50.0d0
  x(1) = b(1); x(2) = b(2); x(3) = b(3); x(4) = b(4)
  call dtbsv('L', 'N', 'N', n, kd, ab, ldab, x, 1)
  call dtbrfs('L', 'N', 'N', n, kd, nrhs, ab, ldab, b, ldb, x, ldx, &
              ferr, berr, work, iwork, info)
  call begin_test('lower_no_trans')
  call print_array('x', x, n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 3: 4x4 upper triangular band (kd=1), transpose, non-unit
  ! A = [2 1 0 0; 0 4 5 0; 0 0 6 1; 0 0 0 3]  (kd=1)
  ! A^T = [2 0 0 0; 1 4 0 0; 0 5 6 0; 0 0 1 3]
  ! Band storage (upper, kd=1, ldab=2):
  !   Row 0 (1st superdiag): *  1  5  1    -> ab(1,2)=1, ab(1,3)=5, ab(1,4)=1
  !   Row 1 (diagonal):      2  4  6  3    -> ab(2,1)=2, ab(2,2)=4, ab(2,3)=6, ab(2,4)=3
  n = 4; kd = 1; nrhs = 1; ldab = 2; ldb = 4; ldx = 4
  ab = 0.0d0; b = 0.0d0; x = 0.0d0
  ! col 1: ab(2,1)=2
  ab(2) = 2.0d0
  ! col 2: ab(1,2)=1, ab(2,2)=4
  ab(3) = 1.0d0; ab(4) = 4.0d0
  ! col 3: ab(1,3)=5, ab(2,3)=6
  ab(5) = 5.0d0; ab(6) = 6.0d0
  ! col 4: ab(1,4)=1, ab(2,4)=3
  ab(7) = 1.0d0; ab(8) = 3.0d0

  ! b = A^T * [1;2;3;4] = [2; 1+8; 10+18; 3+12] = [2; 9; 28; 15]
  b(1) = 2.0d0; b(2) = 9.0d0; b(3) = 28.0d0; b(4) = 15.0d0
  x(1) = b(1); x(2) = b(2); x(3) = b(3); x(4) = b(4)
  call dtbsv('U', 'T', 'N', n, kd, ab, ldab, x, 1)
  call dtbrfs('U', 'T', 'N', n, kd, nrhs, ab, ldab, b, ldb, x, ldx, &
              ferr, berr, work, iwork, info)
  call begin_test('upper_trans')
  call print_array('x', x, n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 4: 3x3 upper triangular band (kd=1), unit diagonal, no transpose
  ! A = [1 2 0; 0 1 4; 0 0 1]  (kd=1, unit)
  ! Band storage (upper, kd=1, ldab=2):
  !   Row 0 (1st superdiag): *  2  4    -> ab(1,2)=2, ab(1,3)=4
  !   Row 1 (diagonal):      *  *  *    (unit, not referenced)
  n = 3; kd = 1; nrhs = 1; ldab = 2; ldb = 3; ldx = 3
  ab = 0.0d0; b = 0.0d0; x = 0.0d0
  ab(3) = 2.0d0; ab(5) = 4.0d0

  ! b = A * [1;2;3] = [1+4; 2+12; 3] = [5; 14; 3]
  b(1) = 5.0d0; b(2) = 14.0d0; b(3) = 3.0d0
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call dtbsv('U', 'N', 'U', n, kd, ab, ldab, x, 1)
  call dtbrfs('U', 'N', 'U', n, kd, nrhs, ab, ldab, b, ldb, x, ldx, &
              ferr, berr, work, iwork, info)
  call begin_test('upper_unit_no_trans')
  call print_array('x', x, n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 5: 3x3 lower triangular band (kd=1), transpose, unit diagonal
  ! L = [1 0 0; 2 1 0; 0 5 1]  (kd=1, unit)
  ! L^T = [1 2 0; 0 1 5; 0 0 1]
  ! Band storage (lower, kd=1, ldab=2):
  !   Row 0 (diagonal):      *  *  *    (unit)
  !   Row 1 (1st subdiag):   2  5  *    -> ab(2,1)=2, ab(2,2)=5
  n = 3; kd = 1; nrhs = 1; ldab = 2; ldb = 3; ldx = 3
  ab = 0.0d0; b = 0.0d0; x = 0.0d0
  ab(2) = 2.0d0; ab(4) = 5.0d0

  ! b = L^T * [1;2;3] = [1+4; 2+15; 3] = [5; 17; 3]
  b(1) = 5.0d0; b(2) = 17.0d0; b(3) = 3.0d0
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call dtbsv('L', 'T', 'U', n, kd, ab, ldab, x, 1)
  call dtbrfs('L', 'T', 'U', n, kd, nrhs, ab, ldab, b, ldb, x, ldx, &
              ferr, berr, work, iwork, info)
  call begin_test('lower_unit_trans')
  call print_array('x', x, n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 6: Multiple RHS (nrhs=2), upper, kd=2, no transpose, non-unit
  ! Same A as Test 1: [2 1 3 0; 0 4 5 2; 0 0 6 1; 0 0 0 3]
  n = 4; kd = 2; nrhs = 2; ldab = 3; ldb = 4; ldx = 4
  ab = 0.0d0; b = 0.0d0; x = 0.0d0
  ab(3) = 2.0d0
  ab(5) = 1.0d0; ab(6) = 4.0d0
  ab(7) = 3.0d0; ab(8) = 5.0d0; ab(9) = 6.0d0
  ab(10) = 2.0d0; ab(11) = 1.0d0; ab(12) = 3.0d0

  ! RHS 1: b1 = A*[1;2;3;4] = [13; 31; 22; 12]
  b(1) = 13.0d0; b(2) = 31.0d0; b(3) = 22.0d0; b(4) = 12.0d0
  ! RHS 2: b2 = A*[4;5;6;7] = [8+5+18; 20+30+14; 36+7; 21] = [31; 64; 43; 21]
  b(5) = 31.0d0; b(6) = 64.0d0; b(7) = 43.0d0; b(8) = 21.0d0
  x(1) = b(1); x(2) = b(2); x(3) = b(3); x(4) = b(4)
  x(5) = b(5); x(6) = b(6); x(7) = b(7); x(8) = b(8)
  call dtbsv('U', 'N', 'N', n, kd, ab, ldab, x(1), 1)
  call dtbsv('U', 'N', 'N', n, kd, ab, ldab, x(5), 1)
  call dtbrfs('U', 'N', 'N', n, kd, nrhs, ab, ldab, b, ldb, x, ldx, &
              ferr, berr, work, iwork, info)
  call begin_test('multi_rhs')
  call print_array('x', x, n*nrhs)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 7: N=0 quick return
  call dtbrfs('U', 'N', 'N', 0, 1, 1, ab, 2, b, 1, x, 1, &
              ferr, berr, work, iwork, info)
  call begin_test('n_zero')
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_int('info', info)
  call end_test()

  ! Test 8: N=1 edge case
  n = 1; kd = 0; nrhs = 1; ldab = 1; ldb = 1; ldx = 1
  ab(1) = 5.0d0
  b(1) = 15.0d0
  x(1) = b(1)
  call dtbsv('U', 'N', 'N', n, kd, ab, ldab, x, 1)
  call dtbrfs('U', 'N', 'N', n, kd, nrhs, ab, ldab, b, ldb, x, ldx, &
              ferr, berr, work, iwork, info)
  call begin_test('n_one')
  call print_array('x', x, n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 9: lower triangular band (kd=1), no transpose, non-unit
  ! A = [3 0 0; 2 5 0; 0 4 7]
  ! Band storage (lower, kd=1, ldab=2):
  !   Row 0 (diagonal):     3  5  7
  !   Row 1 (1st subdiag):  2  4  *
  n = 3; kd = 1; nrhs = 1; ldab = 2; ldb = 3; ldx = 3
  ab = 0.0d0; b = 0.0d0; x = 0.0d0
  ab(1) = 3.0d0; ab(2) = 2.0d0
  ab(3) = 5.0d0; ab(4) = 4.0d0
  ab(5) = 7.0d0

  ! b = A * [1;2;3] = [3; 12; 29]
  b(1) = 3.0d0; b(2) = 12.0d0; b(3) = 29.0d0
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call dtbsv('L', 'N', 'N', n, kd, ab, ldab, x, 1)
  call dtbrfs('L', 'N', 'N', n, kd, nrhs, ab, ldab, b, ldb, x, ldx, &
              ferr, berr, work, iwork, info)
  call begin_test('lower_no_trans_kd1')
  call print_array('x', x, n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 10: lower triangular band (kd=1), transpose, non-unit
  ! A = [3 0 0; 2 5 0; 0 4 7]
  ! A^T = [3 2 0; 0 5 4; 0 0 7]
  ! b = A^T * [1;2;3] = [3+4; 10+12; 21] = [7; 22; 21]
  n = 3; kd = 1; nrhs = 1; ldab = 2; ldb = 3; ldx = 3
  ab = 0.0d0; b = 0.0d0; x = 0.0d0
  ab(1) = 3.0d0; ab(2) = 2.0d0
  ab(3) = 5.0d0; ab(4) = 4.0d0
  ab(5) = 7.0d0

  b(1) = 7.0d0; b(2) = 22.0d0; b(3) = 21.0d0
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call dtbsv('L', 'T', 'N', n, kd, ab, ldab, x, 1)
  call dtbrfs('L', 'T', 'N', n, kd, nrhs, ab, ldab, b, ldb, x, ldx, &
              ferr, berr, work, iwork, info)
  call begin_test('lower_trans')
  call print_array('x', x, n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 11: lower unit diagonal (kd=1), no transpose
  ! A = [1 0 0; 2 1 0; 0 5 1]
  ! b = A * [1;2;3] = [1; 4; 13]
  n = 3; kd = 1; nrhs = 1; ldab = 2; ldb = 3; ldx = 3
  ab = 0.0d0; b = 0.0d0; x = 0.0d0
  ab(2) = 2.0d0; ab(4) = 5.0d0

  b(1) = 1.0d0; b(2) = 4.0d0; b(3) = 13.0d0
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call dtbsv('L', 'N', 'U', n, kd, ab, ldab, x, 1)
  call dtbrfs('L', 'N', 'U', n, kd, nrhs, ab, ldab, b, ldb, x, ldx, &
              ferr, berr, work, iwork, info)
  call begin_test('lower_unit_no_trans')
  call print_array('x', x, n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 12: upper triangular band (kd=1), transpose, unit diagonal
  ! A = [1 2 0; 0 1 4; 0 0 1]
  ! A^T = [1 0 0; 2 1 0; 0 4 1]
  ! b = A^T * [1;2;3] = [1; 4; 11]
  n = 3; kd = 1; nrhs = 1; ldab = 2; ldb = 3; ldx = 3
  ab = 0.0d0; b = 0.0d0; x = 0.0d0
  ab(3) = 2.0d0; ab(5) = 4.0d0

  b(1) = 1.0d0; b(2) = 4.0d0; b(3) = 11.0d0
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call dtbsv('U', 'T', 'U', n, kd, ab, ldab, x, 1)
  call dtbrfs('U', 'T', 'U', n, kd, nrhs, ab, ldab, b, ldb, x, ldx, &
              ferr, berr, work, iwork, info)
  call begin_test('upper_unit_trans')
  call print_array('x', x, n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

end program
