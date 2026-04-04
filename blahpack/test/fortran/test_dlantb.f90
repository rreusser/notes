program test_dlantb
  use test_utils
  implicit none
  double precision :: ab(4, 5)
  double precision :: work(10)
  double precision :: result
  double precision :: dlantb
  external :: dlantb
  integer :: n, k, ldab

  ! ============================================================
  ! Test matrix: 5x5 upper triangular with bandwidth k=2
  !
  ! Full matrix A:
  !   [ 1  -4   7   0   0 ]
  !   [ 0   5  -8   6   0 ]
  !   [ 0   0   9  -3   2 ]
  !   [ 0   0   0   4  -1 ]
  !   [ 0   0   0   0   3 ]
  !
  ! Band storage (LDAB=3, K=2, upper):
  !   row 0 (superdiag-2): AB(1,j) = A(j-2, j)
  !   row 1 (superdiag-1): AB(2,j) = A(j-1, j)
  !   row 2 (diagonal):    AB(3,j) = A(j, j)
  !
  !   AB (3 x 5, 1-indexed):
  !   col:   1     2     3     4     5
  !   [ *     *     7     6     2 ]   <- superdiag 2
  !   [ *    -4    -8    -3    -1 ]   <- superdiag 1
  !   [ 1     5     9     4     3 ]   <- diagonal
  ! ============================================================
  n = 5
  k = 2
  ldab = 4  ! >= k+1 = 3

  ab = 0.0d0
  ! Column 1: diagonal only
  ab(3, 1) = 1.0d0
  ! Column 2: superdiag-1 and diagonal
  ab(2, 2) = -4.0d0
  ab(3, 2) = 5.0d0
  ! Column 3: superdiag-2, superdiag-1, diagonal
  ab(1, 3) = 7.0d0
  ab(2, 3) = -8.0d0
  ab(3, 3) = 9.0d0
  ! Column 4
  ab(1, 4) = 6.0d0
  ab(2, 4) = -3.0d0
  ab(3, 4) = 4.0d0
  ! Column 5
  ab(1, 5) = 2.0d0
  ab(2, 5) = -1.0d0
  ab(3, 5) = 3.0d0

  ! Test 1: upper, non-unit, max norm
  result = dlantb('M', 'U', 'N', n, k, ab, ldab, work)
  call begin_test('upper_nonunit_max')
  call print_scalar('result', result)
  call end_test()

  ! Test 2: upper, non-unit, one-norm
  result = dlantb('1', 'U', 'N', n, k, ab, ldab, work)
  call begin_test('upper_nonunit_one')
  call print_scalar('result', result)
  call end_test()

  ! Test 3: upper, non-unit, inf-norm
  result = dlantb('I', 'U', 'N', n, k, ab, ldab, work)
  call begin_test('upper_nonunit_inf')
  call print_scalar('result', result)
  call end_test()

  ! Test 4: upper, non-unit, frobenius norm
  result = dlantb('F', 'U', 'N', n, k, ab, ldab, work)
  call begin_test('upper_nonunit_frob')
  call print_scalar('result', result)
  call end_test()

  ! Test 5: upper, unit diag, max norm
  result = dlantb('M', 'U', 'U', n, k, ab, ldab, work)
  call begin_test('upper_unit_max')
  call print_scalar('result', result)
  call end_test()

  ! Test 6: upper, unit diag, one-norm
  result = dlantb('1', 'U', 'U', n, k, ab, ldab, work)
  call begin_test('upper_unit_one')
  call print_scalar('result', result)
  call end_test()

  ! Test 7: upper, unit diag, inf-norm
  result = dlantb('I', 'U', 'U', n, k, ab, ldab, work)
  call begin_test('upper_unit_inf')
  call print_scalar('result', result)
  call end_test()

  ! Test 8: upper, unit diag, frobenius norm
  result = dlantb('F', 'U', 'U', n, k, ab, ldab, work)
  call begin_test('upper_unit_frob')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! Test matrix: 5x5 lower triangular with bandwidth k=2
  !
  ! Full matrix A:
  !   [ 2   0   0   0   0 ]
  !   [-3   6   0   0   0 ]
  !   [ 1  -5   8   0   0 ]
  !   [ 0   7  -2   3   0 ]
  !   [ 0   0  -4   1   5 ]
  !
  ! Band storage (LDAB=3, K=2, lower):
  !   row 0 (diagonal):    AB(1,j) = A(j, j)
  !   row 1 (subdiag-1):   AB(2,j) = A(j+1, j)
  !   row 2 (subdiag-2):   AB(3,j) = A(j+2, j)
  !
  !   AB (3 x 5, 1-indexed):
  !   col:   1     2     3     4     5
  !   [ 2     6     8     3     5 ]   <- diagonal
  !   [-3    -5    -2     1     * ]   <- subdiag 1
  !   [ 1     7    -4     *     * ]   <- subdiag 2
  ! ============================================================
  ab = 0.0d0
  ! Column 1
  ab(1, 1) = 2.0d0
  ab(2, 1) = -3.0d0
  ab(3, 1) = 1.0d0
  ! Column 2
  ab(1, 2) = 6.0d0
  ab(2, 2) = -5.0d0
  ab(3, 2) = 7.0d0
  ! Column 3
  ab(1, 3) = 8.0d0
  ab(2, 3) = -2.0d0
  ab(3, 3) = -4.0d0
  ! Column 4
  ab(1, 4) = 3.0d0
  ab(2, 4) = 1.0d0
  ! Column 5
  ab(1, 5) = 5.0d0

  ! Test 9: lower, non-unit, max norm
  result = dlantb('M', 'L', 'N', n, k, ab, ldab, work)
  call begin_test('lower_nonunit_max')
  call print_scalar('result', result)
  call end_test()

  ! Test 10: lower, non-unit, one-norm
  result = dlantb('1', 'L', 'N', n, k, ab, ldab, work)
  call begin_test('lower_nonunit_one')
  call print_scalar('result', result)
  call end_test()

  ! Test 11: lower, non-unit, inf-norm
  result = dlantb('I', 'L', 'N', n, k, ab, ldab, work)
  call begin_test('lower_nonunit_inf')
  call print_scalar('result', result)
  call end_test()

  ! Test 12: lower, non-unit, frobenius norm
  result = dlantb('F', 'L', 'N', n, k, ab, ldab, work)
  call begin_test('lower_nonunit_frob')
  call print_scalar('result', result)
  call end_test()

  ! Test 13: lower, unit diag, max norm
  result = dlantb('M', 'L', 'U', n, k, ab, ldab, work)
  call begin_test('lower_unit_max')
  call print_scalar('result', result)
  call end_test()

  ! Test 14: lower, unit diag, one-norm
  result = dlantb('1', 'L', 'U', n, k, ab, ldab, work)
  call begin_test('lower_unit_one')
  call print_scalar('result', result)
  call end_test()

  ! Test 15: lower, unit diag, inf-norm
  result = dlantb('I', 'L', 'U', n, k, ab, ldab, work)
  call begin_test('lower_unit_inf')
  call print_scalar('result', result)
  call end_test()

  ! Test 16: lower, unit diag, frobenius norm
  result = dlantb('F', 'L', 'U', n, k, ab, ldab, work)
  call begin_test('lower_unit_frob')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! Edge case: N=0
  ! ============================================================
  result = dlantb('M', 'U', 'N', 0, 2, ab, ldab, work)
  call begin_test('edge_n0')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! Edge case: 1x1 matrix, K=0
  ! ============================================================
  ab = 0.0d0
  ab(1, 1) = 5.0d0

  result = dlantb('M', 'U', 'N', 1, 0, ab, ldab, work)
  call begin_test('edge_1x1_max')
  call print_scalar('result', result)
  call end_test()

  result = dlantb('1', 'U', 'N', 1, 0, ab, ldab, work)
  call begin_test('edge_1x1_one')
  call print_scalar('result', result)
  call end_test()

  result = dlantb('I', 'U', 'N', 1, 0, ab, ldab, work)
  call begin_test('edge_1x1_inf')
  call print_scalar('result', result)
  call end_test()

  result = dlantb('F', 'U', 'N', 1, 0, ab, ldab, work)
  call begin_test('edge_1x1_frob')
  call print_scalar('result', result)
  call end_test()

  ! 1x1, unit diagonal
  result = dlantb('M', 'U', 'U', 1, 0, ab, ldab, work)
  call begin_test('edge_1x1_unit_max')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! K=0 (diagonal matrix only), N=4
  ! ============================================================
  ab = 0.0d0
  ab(1, 1) = 3.0d0
  ab(1, 2) = -7.0d0
  ab(1, 3) = 2.0d0
  ab(1, 4) = -4.0d0

  ! Upper, K=0: band storage has only the diagonal row
  result = dlantb('M', 'U', 'N', 4, 0, ab, ldab, work)
  call begin_test('diag_k0_upper_max')
  call print_scalar('result', result)
  call end_test()

  result = dlantb('1', 'U', 'N', 4, 0, ab, ldab, work)
  call begin_test('diag_k0_upper_one')
  call print_scalar('result', result)
  call end_test()

  result = dlantb('I', 'U', 'N', 4, 0, ab, ldab, work)
  call begin_test('diag_k0_upper_inf')
  call print_scalar('result', result)
  call end_test()

  result = dlantb('F', 'U', 'N', 4, 0, ab, ldab, work)
  call begin_test('diag_k0_upper_frob')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! K=1 bandwidth, N=4, upper
  !
  ! Full matrix A:
  !   [ 2  -3   0   0 ]
  !   [ 0   4   1   0 ]
  !   [ 0   0  -5   6 ]
  !   [ 0   0   0   7 ]
  !
  ! Band storage (LDAB=2, K=1, upper):
  !   row 0 (superdiag): AB(1,j) = A(j-1,j)
  !   row 1 (diagonal):  AB(2,j) = A(j,j)
  !
  !   AB (2 x 4, 1-indexed):
  !   col:  1    2    3    4
  !   [ *   -3    1    6 ]   <- superdiag
  !   [ 2    4   -5    7 ]   <- diagonal
  ! ============================================================
  ab = 0.0d0
  ab(2, 1) = 2.0d0
  ab(1, 2) = -3.0d0
  ab(2, 2) = 4.0d0
  ab(1, 3) = 1.0d0
  ab(2, 3) = -5.0d0
  ab(1, 4) = 6.0d0
  ab(2, 4) = 7.0d0

  result = dlantb('M', 'U', 'N', 4, 1, ab, ldab, work)
  call begin_test('upper_k1_nonunit_max')
  call print_scalar('result', result)
  call end_test()

  result = dlantb('1', 'U', 'N', 4, 1, ab, ldab, work)
  call begin_test('upper_k1_nonunit_one')
  call print_scalar('result', result)
  call end_test()

  result = dlantb('I', 'U', 'N', 4, 1, ab, ldab, work)
  call begin_test('upper_k1_nonunit_inf')
  call print_scalar('result', result)
  call end_test()

  result = dlantb('F', 'U', 'N', 4, 1, ab, ldab, work)
  call begin_test('upper_k1_nonunit_frob')
  call print_scalar('result', result)
  call end_test()

  ! Unit diagonal variant
  result = dlantb('M', 'U', 'U', 4, 1, ab, ldab, work)
  call begin_test('upper_k1_unit_max')
  call print_scalar('result', result)
  call end_test()

  result = dlantb('1', 'U', 'U', 4, 1, ab, ldab, work)
  call begin_test('upper_k1_unit_one')
  call print_scalar('result', result)
  call end_test()

  result = dlantb('I', 'U', 'U', 4, 1, ab, ldab, work)
  call begin_test('upper_k1_unit_inf')
  call print_scalar('result', result)
  call end_test()

  result = dlantb('F', 'U', 'U', 4, 1, ab, ldab, work)
  call begin_test('upper_k1_unit_frob')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! K=1 bandwidth, N=4, lower
  !
  ! Full matrix A:
  !   [ 2   0   0   0 ]
  !   [-3   4   0   0 ]
  !   [ 0   1  -5   0 ]
  !   [ 0   0   6   7 ]
  !
  ! Band storage (LDAB=2, K=1, lower):
  !   row 0 (diagonal):  AB(1,j) = A(j,j)
  !   row 1 (subdiag):   AB(2,j) = A(j+1,j)
  !
  !   AB (2 x 4, 1-indexed):
  !   col:  1    2    3    4
  !   [ 2    4   -5    7 ]   <- diagonal
  !   [-3    1    6    * ]   <- subdiag
  ! ============================================================
  ab = 0.0d0
  ab(1, 1) = 2.0d0
  ab(2, 1) = -3.0d0
  ab(1, 2) = 4.0d0
  ab(2, 2) = 1.0d0
  ab(1, 3) = -5.0d0
  ab(2, 3) = 6.0d0
  ab(1, 4) = 7.0d0

  result = dlantb('M', 'L', 'N', 4, 1, ab, ldab, work)
  call begin_test('lower_k1_nonunit_max')
  call print_scalar('result', result)
  call end_test()

  result = dlantb('1', 'L', 'N', 4, 1, ab, ldab, work)
  call begin_test('lower_k1_nonunit_one')
  call print_scalar('result', result)
  call end_test()

  result = dlantb('I', 'L', 'N', 4, 1, ab, ldab, work)
  call begin_test('lower_k1_nonunit_inf')
  call print_scalar('result', result)
  call end_test()

  result = dlantb('F', 'L', 'N', 4, 1, ab, ldab, work)
  call begin_test('lower_k1_nonunit_frob')
  call print_scalar('result', result)
  call end_test()

  ! Unit diagonal variant
  result = dlantb('M', 'L', 'U', 4, 1, ab, ldab, work)
  call begin_test('lower_k1_unit_max')
  call print_scalar('result', result)
  call end_test()

  result = dlantb('1', 'L', 'U', 4, 1, ab, ldab, work)
  call begin_test('lower_k1_unit_one')
  call print_scalar('result', result)
  call end_test()

  result = dlantb('I', 'L', 'U', 4, 1, ab, ldab, work)
  call begin_test('lower_k1_unit_inf')
  call print_scalar('result', result)
  call end_test()

  result = dlantb('F', 'L', 'U', 4, 1, ab, ldab, work)
  call begin_test('lower_k1_unit_frob')
  call print_scalar('result', result)
  call end_test()

end program
