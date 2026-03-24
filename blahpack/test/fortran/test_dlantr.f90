program test_dlantr
  use test_utils
  implicit none
  double precision :: a(6, 6)
  double precision :: work(10)
  double precision :: result
  double precision :: dlantr
  external :: dlantr
  integer :: m, n, lda

  lda = 6

  ! Upper triangular 4x4 matrix:
  !   [ 1  -4   7  -2 ]
  !   [ 0   5  -8   6 ]
  !   [ 0   0   9  -3 ]
  !   [ 0   0   0   4 ]
  m = 4
  n = 4
  a = 0.0d0
  a(1,1) =  1.0d0
  a(1,2) = -4.0d0
  a(2,2) =  5.0d0
  a(1,3) =  7.0d0
  a(2,3) = -8.0d0
  a(3,3) =  9.0d0
  a(1,4) = -2.0d0
  a(2,4) =  6.0d0
  a(3,4) = -3.0d0
  a(4,4) =  4.0d0

  ! Test 1: upper, non-unit, max norm
  result = dlantr('M', 'U', 'N', m, n, a, lda, work)
  call begin_test('upper_nonunit_max')
  call print_scalar('result', result)
  call end_test()

  ! Test 2: upper, non-unit, one-norm
  result = dlantr('1', 'U', 'N', m, n, a, lda, work)
  call begin_test('upper_nonunit_one')
  call print_scalar('result', result)
  call end_test()

  ! Test 3: upper, non-unit, inf-norm
  result = dlantr('I', 'U', 'N', m, n, a, lda, work)
  call begin_test('upper_nonunit_inf')
  call print_scalar('result', result)
  call end_test()

  ! Test 4: upper, non-unit, frobenius norm
  result = dlantr('F', 'U', 'N', m, n, a, lda, work)
  call begin_test('upper_nonunit_frob')
  call print_scalar('result', result)
  call end_test()

  ! Test 5: upper, unit diag, max norm (diagonal treated as 1s)
  result = dlantr('M', 'U', 'U', m, n, a, lda, work)
  call begin_test('upper_unit_max')
  call print_scalar('result', result)
  call end_test()

  ! Test 6: upper, unit diag, one-norm
  result = dlantr('1', 'U', 'U', m, n, a, lda, work)
  call begin_test('upper_unit_one')
  call print_scalar('result', result)
  call end_test()

  ! Test 7: upper, unit diag, inf-norm
  result = dlantr('I', 'U', 'U', m, n, a, lda, work)
  call begin_test('upper_unit_inf')
  call print_scalar('result', result)
  call end_test()

  ! Test 8: upper, unit diag, frobenius norm
  result = dlantr('F', 'U', 'U', m, n, a, lda, work)
  call begin_test('upper_unit_frob')
  call print_scalar('result', result)
  call end_test()

  ! Lower triangular 4x4 matrix:
  !   [ 2   0   0   0 ]
  !   [-3   6   0   0 ]
  !   [ 1  -5   8   0 ]
  !   [-4   7  -2   3 ]
  a = 0.0d0
  a(1,1) =  2.0d0
  a(2,1) = -3.0d0
  a(3,1) =  1.0d0
  a(4,1) = -4.0d0
  a(2,2) =  6.0d0
  a(3,2) = -5.0d0
  a(4,2) =  7.0d0
  a(3,3) =  8.0d0
  a(4,3) = -2.0d0
  a(4,4) =  3.0d0

  ! Test 9: lower, non-unit, max norm
  result = dlantr('M', 'L', 'N', m, n, a, lda, work)
  call begin_test('lower_nonunit_max')
  call print_scalar('result', result)
  call end_test()

  ! Test 10: lower, non-unit, one-norm
  result = dlantr('1', 'L', 'N', m, n, a, lda, work)
  call begin_test('lower_nonunit_one')
  call print_scalar('result', result)
  call end_test()

  ! Test 11: lower, non-unit, inf-norm
  result = dlantr('I', 'L', 'N', m, n, a, lda, work)
  call begin_test('lower_nonunit_inf')
  call print_scalar('result', result)
  call end_test()

  ! Test 12: lower, non-unit, frobenius norm
  result = dlantr('F', 'L', 'N', m, n, a, lda, work)
  call begin_test('lower_nonunit_frob')
  call print_scalar('result', result)
  call end_test()

  ! Test 13: lower, unit diag, max norm
  result = dlantr('M', 'L', 'U', m, n, a, lda, work)
  call begin_test('lower_unit_max')
  call print_scalar('result', result)
  call end_test()

  ! Test 14: lower, unit diag, one-norm
  result = dlantr('1', 'L', 'U', m, n, a, lda, work)
  call begin_test('lower_unit_one')
  call print_scalar('result', result)
  call end_test()

  ! Test 15: lower, unit diag, inf-norm
  result = dlantr('I', 'L', 'U', m, n, a, lda, work)
  call begin_test('lower_unit_inf')
  call print_scalar('result', result)
  call end_test()

  ! Test 16: lower, unit diag, frobenius norm
  result = dlantr('F', 'L', 'U', m, n, a, lda, work)
  call begin_test('lower_unit_frob')
  call print_scalar('result', result)
  call end_test()

  ! Test 17: edge case M=0
  result = dlantr('M', 'U', 'N', 0, 4, a, lda, work)
  call begin_test('edge_m0')
  call print_scalar('result', result)
  call end_test()

  ! Test 18: edge case N=0
  result = dlantr('M', 'U', 'N', 4, 0, a, lda, work)
  call begin_test('edge_n0')
  call print_scalar('result', result)
  call end_test()

  ! Test 19: rectangular upper, M > N (3x2 upper triangle)
  ! [ 1  -4 ]
  ! [ 0   5 ]
  ! [ 0   0 ]
  m = 3
  n = 2
  a = 0.0d0
  a(1,1) =  1.0d0
  a(1,2) = -4.0d0
  a(2,2) =  5.0d0

  result = dlantr('M', 'U', 'N', m, n, a, lda, work)
  call begin_test('rect_upper_mn_max')
  call print_scalar('result', result)
  call end_test()

  result = dlantr('1', 'U', 'N', m, n, a, lda, work)
  call begin_test('rect_upper_mn_one')
  call print_scalar('result', result)
  call end_test()

  result = dlantr('I', 'U', 'N', m, n, a, lda, work)
  call begin_test('rect_upper_mn_inf')
  call print_scalar('result', result)
  call end_test()

  result = dlantr('F', 'U', 'N', m, n, a, lda, work)
  call begin_test('rect_upper_mn_frob')
  call print_scalar('result', result)
  call end_test()

  ! Test 23: rectangular upper, M < N (2x4 upper triangle)
  ! [ 1  -4   7  -2 ]
  ! [ 0   5  -8   6 ]
  m = 2
  n = 4
  a = 0.0d0
  a(1,1) =  1.0d0
  a(1,2) = -4.0d0
  a(2,2) =  5.0d0
  a(1,3) =  7.0d0
  a(2,3) = -8.0d0
  a(1,4) = -2.0d0
  a(2,4) =  6.0d0

  result = dlantr('M', 'U', 'N', m, n, a, lda, work)
  call begin_test('rect_upper_nm_max')
  call print_scalar('result', result)
  call end_test()

  result = dlantr('1', 'U', 'N', m, n, a, lda, work)
  call begin_test('rect_upper_nm_one')
  call print_scalar('result', result)
  call end_test()

  result = dlantr('I', 'U', 'N', m, n, a, lda, work)
  call begin_test('rect_upper_nm_inf')
  call print_scalar('result', result)
  call end_test()

  result = dlantr('F', 'U', 'N', m, n, a, lda, work)
  call begin_test('rect_upper_nm_frob')
  call print_scalar('result', result)
  call end_test()

  ! Test 27: rectangular lower, M > N (4x2 lower triangle)
  ! [ 2   0 ]
  ! [-3   6 ]
  ! [ 1  -5 ]
  ! [-4   7 ]
  m = 4
  n = 2
  a = 0.0d0
  a(1,1) =  2.0d0
  a(2,1) = -3.0d0
  a(3,1) =  1.0d0
  a(4,1) = -4.0d0
  a(2,2) =  6.0d0
  a(3,2) = -5.0d0
  a(4,2) =  7.0d0

  result = dlantr('M', 'L', 'N', m, n, a, lda, work)
  call begin_test('rect_lower_mn_max')
  call print_scalar('result', result)
  call end_test()

  result = dlantr('1', 'L', 'N', m, n, a, lda, work)
  call begin_test('rect_lower_mn_one')
  call print_scalar('result', result)
  call end_test()

  result = dlantr('I', 'L', 'N', m, n, a, lda, work)
  call begin_test('rect_lower_mn_inf')
  call print_scalar('result', result)
  call end_test()

  result = dlantr('F', 'L', 'N', m, n, a, lda, work)
  call begin_test('rect_lower_mn_frob')
  call print_scalar('result', result)
  call end_test()

  ! Test 31: 1x1 matrix
  m = 1
  n = 1
  a = 0.0d0
  a(1,1) = 5.0d0

  result = dlantr('M', 'U', 'N', m, n, a, lda, work)
  call begin_test('edge_1x1_max')
  call print_scalar('result', result)
  call end_test()

  result = dlantr('1', 'U', 'N', m, n, a, lda, work)
  call begin_test('edge_1x1_one')
  call print_scalar('result', result)
  call end_test()

  result = dlantr('I', 'U', 'N', m, n, a, lda, work)
  call begin_test('edge_1x1_inf')
  call print_scalar('result', result)
  call end_test()

  result = dlantr('F', 'U', 'N', m, n, a, lda, work)
  call begin_test('edge_1x1_frob')
  call print_scalar('result', result)
  call end_test()

  ! Test: 1x1 unit diagonal
  result = dlantr('M', 'U', 'U', m, n, a, lda, work)
  call begin_test('edge_1x1_unit_max')
  call print_scalar('result', result)
  call end_test()

end program
