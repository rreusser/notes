program test_dlansb
  use test_utils
  implicit none
  double precision :: ab(4, 5)
  double precision :: work(10)
  double precision :: result
  double precision :: dlansb
  external :: dlansb
  integer :: n, k, ldab

  ! ============================================================
  ! Test matrix: 5x5 symmetric band matrix with K=2
  !
  ! Full symmetric matrix A (upper storage):
  !   [ 1  -4   7   0   0 ]
  !   [-4   5  -8   6   0 ]
  !   [ 7  -8   9  -3   2 ]
  !   [ 0   6  -3   4  -1 ]
  !   [ 0   0   2  -1   3 ]
  !
  ! Upper band storage (LDAB=3, K=2):
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

  ! Test 1: upper, max norm
  result = dlansb('M', 'U', n, k, ab, ldab, work)
  call begin_test('upper_max')
  call print_scalar('result', result)
  call end_test()

  ! Test 2: upper, one-norm
  result = dlansb('1', 'U', n, k, ab, ldab, work)
  call begin_test('upper_one')
  call print_scalar('result', result)
  call end_test()

  ! Test 3: upper, inf-norm
  result = dlansb('I', 'U', n, k, ab, ldab, work)
  call begin_test('upper_inf')
  call print_scalar('result', result)
  call end_test()

  ! Test 4: upper, frobenius norm
  result = dlansb('F', 'U', n, k, ab, ldab, work)
  call begin_test('upper_frob')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! Test matrix: 5x5 symmetric band matrix with K=2 (lower storage)
  !
  ! Full symmetric matrix A (lower storage):
  !   [ 2  -3   1   0   0 ]
  !   [-3   6  -5   7   0 ]
  !   [ 1  -5   8  -2  -4 ]
  !   [ 0   7  -2   3   1 ]
  !   [ 0   0  -4   1   5 ]
  !
  ! Lower band storage (LDAB=3, K=2):
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

  ! Test 5: lower, max norm
  result = dlansb('M', 'L', n, k, ab, ldab, work)
  call begin_test('lower_max')
  call print_scalar('result', result)
  call end_test()

  ! Test 6: lower, one-norm
  result = dlansb('1', 'L', n, k, ab, ldab, work)
  call begin_test('lower_one')
  call print_scalar('result', result)
  call end_test()

  ! Test 7: lower, inf-norm
  result = dlansb('I', 'L', n, k, ab, ldab, work)
  call begin_test('lower_inf')
  call print_scalar('result', result)
  call end_test()

  ! Test 8: lower, frobenius norm
  result = dlansb('F', 'L', n, k, ab, ldab, work)
  call begin_test('lower_frob')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! Edge case: N=0
  ! ============================================================
  result = dlansb('M', 'U', 0, 2, ab, ldab, work)
  call begin_test('edge_n0')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! Edge case: 1x1 matrix, K=0
  ! ============================================================
  ab = 0.0d0
  ab(1, 1) = 5.0d0

  result = dlansb('M', 'U', 1, 0, ab, ldab, work)
  call begin_test('edge_1x1_max')
  call print_scalar('result', result)
  call end_test()

  result = dlansb('1', 'U', 1, 0, ab, ldab, work)
  call begin_test('edge_1x1_one')
  call print_scalar('result', result)
  call end_test()

  result = dlansb('I', 'U', 1, 0, ab, ldab, work)
  call begin_test('edge_1x1_inf')
  call print_scalar('result', result)
  call end_test()

  result = dlansb('F', 'U', 1, 0, ab, ldab, work)
  call begin_test('edge_1x1_frob')
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

  result = dlansb('M', 'U', 4, 0, ab, ldab, work)
  call begin_test('diag_k0_upper_max')
  call print_scalar('result', result)
  call end_test()

  result = dlansb('1', 'U', 4, 0, ab, ldab, work)
  call begin_test('diag_k0_upper_one')
  call print_scalar('result', result)
  call end_test()

  result = dlansb('I', 'U', 4, 0, ab, ldab, work)
  call begin_test('diag_k0_upper_inf')
  call print_scalar('result', result)
  call end_test()

  result = dlansb('F', 'U', 4, 0, ab, ldab, work)
  call begin_test('diag_k0_upper_frob')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! K=1 bandwidth, N=4, upper symmetric
  !
  ! Full symmetric matrix A:
  !   [ 2  -3   0   0 ]
  !   [-3   4   1   0 ]
  !   [ 0   1  -5   6 ]
  !   [ 0   0   6   7 ]
  !
  ! Upper band storage (LDAB=2, K=1):
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

  result = dlansb('M', 'U', 4, 1, ab, ldab, work)
  call begin_test('upper_k1_max')
  call print_scalar('result', result)
  call end_test()

  result = dlansb('1', 'U', 4, 1, ab, ldab, work)
  call begin_test('upper_k1_one')
  call print_scalar('result', result)
  call end_test()

  result = dlansb('I', 'U', 4, 1, ab, ldab, work)
  call begin_test('upper_k1_inf')
  call print_scalar('result', result)
  call end_test()

  result = dlansb('F', 'U', 4, 1, ab, ldab, work)
  call begin_test('upper_k1_frob')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! K=1 bandwidth, N=4, lower symmetric
  !
  ! Full symmetric matrix A:
  !   [ 2  -3   0   0 ]
  !   [-3   4   1   0 ]
  !   [ 0   1  -5   6 ]
  !   [ 0   0   6   7 ]
  !
  ! Lower band storage (LDAB=2, K=1):
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

  result = dlansb('M', 'L', 4, 1, ab, ldab, work)
  call begin_test('lower_k1_max')
  call print_scalar('result', result)
  call end_test()

  result = dlansb('1', 'L', 4, 1, ab, ldab, work)
  call begin_test('lower_k1_one')
  call print_scalar('result', result)
  call end_test()

  result = dlansb('I', 'L', 4, 1, ab, ldab, work)
  call begin_test('lower_k1_inf')
  call print_scalar('result', result)
  call end_test()

  result = dlansb('F', 'L', 4, 1, ab, ldab, work)
  call begin_test('lower_k1_frob')
  call print_scalar('result', result)
  call end_test()

end program
