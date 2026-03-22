program test_ztrsm
  use test_utils
  implicit none
  complex*16 :: a(4, 4), b(4, 4), alpha
  double precision :: b_r(32)
  equivalence (b, b_r)
  integer :: m, n, lda, ldb

  lda = 4
  ldb = 4

  ! =====================================================================
  ! Tests 1-12: All side x uplo x trans combinations with non-unit diag
  ! Using 2x2 matrices with known values
  ! Upper triangular A: a(1,1)=(2,1), a(1,2)=(3,1), a(2,2)=(4,2)
  ! Lower triangular A: a(1,1)=(2,1), a(2,1)=(3,1), a(2,2)=(4,2)
  ! B (2x2): [(1,0),(0,1); (0,1),(1,0)]
  ! alpha = (1,0)
  ! =====================================================================

  ! Test 1: Left, Upper, No-transpose, Non-unit
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(1,2) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrsm('L', 'U', 'N', 'N', m, n, alpha, a, lda, b, ldb)
  call begin_test('left_upper_notrans_nonunit')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 2: Left, Upper, Transpose, Non-unit
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(1,2) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrsm('L', 'U', 'T', 'N', m, n, alpha, a, lda, b, ldb)
  call begin_test('left_upper_trans_nonunit')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 3: Left, Upper, Conj-transpose, Non-unit
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(1,2) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrsm('L', 'U', 'C', 'N', m, n, alpha, a, lda, b, ldb)
  call begin_test('left_upper_conjtrans_nonunit')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 4: Left, Lower, No-transpose, Non-unit
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(2,1) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrsm('L', 'L', 'N', 'N', m, n, alpha, a, lda, b, ldb)
  call begin_test('left_lower_notrans_nonunit')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 5: Left, Lower, Transpose, Non-unit
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(2,1) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrsm('L', 'L', 'T', 'N', m, n, alpha, a, lda, b, ldb)
  call begin_test('left_lower_trans_nonunit')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 6: Left, Lower, Conj-transpose, Non-unit
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(2,1) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrsm('L', 'L', 'C', 'N', m, n, alpha, a, lda, b, ldb)
  call begin_test('left_lower_conjtrans_nonunit')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 7: Right, Upper, No-transpose, Non-unit
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(1,2) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrsm('R', 'U', 'N', 'N', m, n, alpha, a, lda, b, ldb)
  call begin_test('right_upper_notrans_nonunit')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 8: Right, Upper, Transpose, Non-unit
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(1,2) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrsm('R', 'U', 'T', 'N', m, n, alpha, a, lda, b, ldb)
  call begin_test('right_upper_trans_nonunit')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 9: Right, Upper, Conj-transpose, Non-unit
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(1,2) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrsm('R', 'U', 'C', 'N', m, n, alpha, a, lda, b, ldb)
  call begin_test('right_upper_conjtrans_nonunit')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 10: Right, Lower, No-transpose, Non-unit
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(2,1) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrsm('R', 'L', 'N', 'N', m, n, alpha, a, lda, b, ldb)
  call begin_test('right_lower_notrans_nonunit')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 11: Right, Lower, Transpose, Non-unit
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(2,1) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrsm('R', 'L', 'T', 'N', m, n, alpha, a, lda, b, ldb)
  call begin_test('right_lower_trans_nonunit')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 12: Right, Lower, Conj-transpose, Non-unit
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(2,1) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrsm('R', 'L', 'C', 'N', m, n, alpha, a, lda, b, ldb)
  call begin_test('right_lower_conjtrans_nonunit')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! =====================================================================
  ! Tests 13-24: Unit diagonal versions (same as 1-12 but diag='U')
  ! =====================================================================

  ! Test 13: Left, Upper, No-transpose, Unit
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (99.0d0, 99.0d0)
  a(1,2) = (3.0d0, 1.0d0)
  a(2,2) = (99.0d0, 99.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrsm('L', 'U', 'N', 'U', m, n, alpha, a, lda, b, ldb)
  call begin_test('left_upper_notrans_unit')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 14: Left, Upper, Transpose, Unit
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (99.0d0, 99.0d0)
  a(1,2) = (3.0d0, 1.0d0)
  a(2,2) = (99.0d0, 99.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrsm('L', 'U', 'T', 'U', m, n, alpha, a, lda, b, ldb)
  call begin_test('left_upper_trans_unit')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 15: Left, Upper, Conj-transpose, Unit
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (99.0d0, 99.0d0)
  a(1,2) = (3.0d0, 1.0d0)
  a(2,2) = (99.0d0, 99.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrsm('L', 'U', 'C', 'U', m, n, alpha, a, lda, b, ldb)
  call begin_test('left_upper_conjtrans_unit')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 16: Left, Lower, No-transpose, Unit
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (99.0d0, 99.0d0)
  a(2,1) = (3.0d0, 1.0d0)
  a(2,2) = (99.0d0, 99.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrsm('L', 'L', 'N', 'U', m, n, alpha, a, lda, b, ldb)
  call begin_test('left_lower_notrans_unit')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 17: Left, Lower, Transpose, Unit
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (99.0d0, 99.0d0)
  a(2,1) = (3.0d0, 1.0d0)
  a(2,2) = (99.0d0, 99.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrsm('L', 'L', 'T', 'U', m, n, alpha, a, lda, b, ldb)
  call begin_test('left_lower_trans_unit')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 18: Left, Lower, Conj-transpose, Unit
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (99.0d0, 99.0d0)
  a(2,1) = (3.0d0, 1.0d0)
  a(2,2) = (99.0d0, 99.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrsm('L', 'L', 'C', 'U', m, n, alpha, a, lda, b, ldb)
  call begin_test('left_lower_conjtrans_unit')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 19: Right, Upper, No-transpose, Unit
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (99.0d0, 99.0d0)
  a(1,2) = (3.0d0, 1.0d0)
  a(2,2) = (99.0d0, 99.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrsm('R', 'U', 'N', 'U', m, n, alpha, a, lda, b, ldb)
  call begin_test('right_upper_notrans_unit')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 20: Right, Upper, Transpose, Unit
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (99.0d0, 99.0d0)
  a(1,2) = (3.0d0, 1.0d0)
  a(2,2) = (99.0d0, 99.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrsm('R', 'U', 'T', 'U', m, n, alpha, a, lda, b, ldb)
  call begin_test('right_upper_trans_unit')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 21: Right, Upper, Conj-transpose, Unit
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (99.0d0, 99.0d0)
  a(1,2) = (3.0d0, 1.0d0)
  a(2,2) = (99.0d0, 99.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrsm('R', 'U', 'C', 'U', m, n, alpha, a, lda, b, ldb)
  call begin_test('right_upper_conjtrans_unit')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 22: Right, Lower, No-transpose, Unit
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (99.0d0, 99.0d0)
  a(2,1) = (3.0d0, 1.0d0)
  a(2,2) = (99.0d0, 99.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrsm('R', 'L', 'N', 'U', m, n, alpha, a, lda, b, ldb)
  call begin_test('right_lower_notrans_unit')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 23: Right, Lower, Transpose, Unit
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (99.0d0, 99.0d0)
  a(2,1) = (3.0d0, 1.0d0)
  a(2,2) = (99.0d0, 99.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrsm('R', 'L', 'T', 'U', m, n, alpha, a, lda, b, ldb)
  call begin_test('right_lower_trans_unit')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 24: Right, Lower, Conj-transpose, Unit
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (99.0d0, 99.0d0)
  a(2,1) = (3.0d0, 1.0d0)
  a(2,2) = (99.0d0, 99.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrsm('R', 'L', 'C', 'U', m, n, alpha, a, lda, b, ldb)
  call begin_test('right_lower_conjtrans_unit')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! =====================================================================
  ! Edge case tests
  ! =====================================================================

  ! Test 25: alpha = (0,0) -- should zero B
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(1,2) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (0.0d0, 0.0d0)
  call ztrsm('L', 'U', 'N', 'N', m, n, alpha, a, lda, b, ldb)
  call begin_test('alpha_zero')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 26: complex alpha = (0,1)
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 0.0d0)
  a(1,2) = (0.0d0, 0.0d0)
  a(2,2) = (1.0d0, 0.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (2.0d0, 0.0d0)
  b(2,2) = (0.0d0, 2.0d0)
  alpha = (0.0d0, 1.0d0)
  call ztrsm('L', 'U', 'N', 'N', m, n, alpha, a, lda, b, ldb)
  call begin_test('complex_alpha')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 27: complex alpha = (2, -1) with non-identity A
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(1,2) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (2.0d0, -1.0d0)
  call ztrsm('L', 'U', 'N', 'N', m, n, alpha, a, lda, b, ldb)
  call begin_test('complex_alpha_nonidentity')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 28: M=0 quick return
  b = (0.0d0, 0.0d0)
  b(1,1) = (99.0d0, 99.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrsm('L', 'U', 'N', 'N', 0, 2, alpha, a, lda, b, ldb)
  call begin_test('m_zero')
  call print_cmatrix('b', b_r, ldb, 1, 1)
  call end_test()

  ! Test 29: N=0 quick return
  b = (0.0d0, 0.0d0)
  b(1,1) = (99.0d0, 99.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrsm('L', 'U', 'N', 'N', 2, 0, alpha, a, lda, b, ldb)
  call begin_test('n_zero')
  call print_cmatrix('b', b_r, ldb, 1, 1)
  call end_test()

  ! Test 30: 3x3 left upper no-transpose (larger matrix)
  m = 3
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 1.0d0)
  a(1,2) = (2.0d0, 0.0d0)
  a(1,3) = (0.0d0, 1.0d0)
  a(2,2) = (3.0d0, -1.0d0)
  a(2,3) = (1.0d0, 2.0d0)
  a(3,3) = (2.0d0, 0.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(3,1) = (2.0d0, -1.0d0)
  b(1,2) = (-1.0d0, 1.0d0)
  b(2,2) = (3.0d0, 0.0d0)
  b(3,2) = (0.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrsm('L', 'U', 'N', 'N', m, n, alpha, a, lda, b, ldb)
  call begin_test('3x2_left_upper_notrans')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 31: Right, Upper, Conj-trans with complex alpha (2,-1) -- exercises
  ! the right-side conj-trans branch with non-trivial alpha scaling
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(1,2) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (2.0d0, -1.0d0)
  call ztrsm('R', 'U', 'C', 'N', m, n, alpha, a, lda, b, ldb)
  call begin_test('right_upper_conjtrans_alpha')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 32: Right, Lower, Trans with complex alpha
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(2,1) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (2.0d0, -1.0d0)
  call ztrsm('R', 'L', 'T', 'N', m, n, alpha, a, lda, b, ldb)
  call begin_test('right_lower_trans_alpha')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

contains
  subroutine print_cmatrix(name, arr, lda_val, m_val, n_val)
    character(*), intent(in) :: name
    integer, intent(in) :: lda_val, m_val, n_val
    double precision, intent(in) :: arr(2*lda_val, *)
    integer :: i, j
    logical :: first
    write(*, '(A,A,A)', advance='no') ',"', trim(name), '":['
    first = .true.
    do j = 1, n_val
      do i = 1, m_val
        if (.not. first) write(*, '(A)', advance='no') ','
        first = .false.
        write(*, '(ES25.17E3)', advance='no') arr(2*(i-1)+1, j)
        write(*, '(A)', advance='no') ','
        write(*, '(ES25.17E3)', advance='no') arr(2*(i-1)+2, j)
      end do
    end do
    write(*, '(A)', advance='no') ']'
  end subroutine

end program
