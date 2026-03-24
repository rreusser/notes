program test_zlantr
  use test_utils
  implicit none
  complex*16 :: a(5, 5)
  double precision :: work(10)
  double precision :: result
  double precision :: zlantr
  external :: zlantr
  integer :: m, n, lda

  lda = 5

  ! 3x3 upper triangular:
  ! A = [ (1+2i) (3+4i) (5+6i) ]
  !     [     0  (7+8i) (9+1i) ]
  !     [     0      0  (2+3i) ]

  ! Test 1: max norm, upper, non-unit
  m = 3
  n = 3
  a = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 2.0d0)
  a(1,2) = (3.0d0, 4.0d0)
  a(1,3) = (5.0d0, 6.0d0)
  a(2,2) = (7.0d0, 8.0d0)
  a(2,3) = (9.0d0, 1.0d0)
  a(3,3) = (2.0d0, 3.0d0)
  result = zlantr('M', 'U', 'N', m, n, a, lda, work)
  call begin_test('max_upper_nonunit')
  call print_scalar('result', result)
  call end_test()

  ! Test 2: max norm, upper, unit diag
  result = zlantr('M', 'U', 'U', m, n, a, lda, work)
  call begin_test('max_upper_unit')
  call print_scalar('result', result)
  call end_test()

  ! Test 3: one norm, upper, non-unit
  result = zlantr('1', 'U', 'N', m, n, a, lda, work)
  call begin_test('one_upper_nonunit')
  call print_scalar('result', result)
  call end_test()

  ! Test 4: one norm, upper, unit diag
  result = zlantr('1', 'U', 'U', m, n, a, lda, work)
  call begin_test('one_upper_unit')
  call print_scalar('result', result)
  call end_test()

  ! Test 5: inf norm, upper, non-unit
  result = zlantr('I', 'U', 'N', m, n, a, lda, work)
  call begin_test('inf_upper_nonunit')
  call print_scalar('result', result)
  call end_test()

  ! Test 6: inf norm, upper, unit diag
  result = zlantr('I', 'U', 'U', m, n, a, lda, work)
  call begin_test('inf_upper_unit')
  call print_scalar('result', result)
  call end_test()

  ! Test 7: frobenius, upper, non-unit
  result = zlantr('F', 'U', 'N', m, n, a, lda, work)
  call begin_test('frob_upper_nonunit')
  call print_scalar('result', result)
  call end_test()

  ! Test 8: frobenius, upper, unit diag
  result = zlantr('F', 'U', 'U', m, n, a, lda, work)
  call begin_test('frob_upper_unit')
  call print_scalar('result', result)
  call end_test()

  ! 3x3 lower triangular:
  ! A = [ (1+2i)     0      0  ]
  !     [ (3+4i) (7+8i)     0  ]
  !     [ (5+6i) (9+1i) (2+3i) ]
  a = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 2.0d0)
  a(2,1) = (3.0d0, 4.0d0)
  a(3,1) = (5.0d0, 6.0d0)
  a(2,2) = (7.0d0, 8.0d0)
  a(3,2) = (9.0d0, 1.0d0)
  a(3,3) = (2.0d0, 3.0d0)

  ! Test 9: max norm, lower, non-unit
  result = zlantr('M', 'L', 'N', m, n, a, lda, work)
  call begin_test('max_lower_nonunit')
  call print_scalar('result', result)
  call end_test()

  ! Test 10: max norm, lower, unit diag
  result = zlantr('M', 'L', 'U', m, n, a, lda, work)
  call begin_test('max_lower_unit')
  call print_scalar('result', result)
  call end_test()

  ! Test 11: one norm, lower, non-unit
  result = zlantr('1', 'L', 'N', m, n, a, lda, work)
  call begin_test('one_lower_nonunit')
  call print_scalar('result', result)
  call end_test()

  ! Test 12: one norm, lower, unit diag
  result = zlantr('1', 'L', 'U', m, n, a, lda, work)
  call begin_test('one_lower_unit')
  call print_scalar('result', result)
  call end_test()

  ! Test 13: inf norm, lower, non-unit
  result = zlantr('I', 'L', 'N', m, n, a, lda, work)
  call begin_test('inf_lower_nonunit')
  call print_scalar('result', result)
  call end_test()

  ! Test 14: inf norm, lower, unit diag
  result = zlantr('I', 'L', 'U', m, n, a, lda, work)
  call begin_test('inf_lower_unit')
  call print_scalar('result', result)
  call end_test()

  ! Test 15: frobenius, lower, non-unit
  result = zlantr('F', 'L', 'N', m, n, a, lda, work)
  call begin_test('frob_lower_nonunit')
  call print_scalar('result', result)
  call end_test()

  ! Test 16: frobenius, lower, unit diag
  result = zlantr('F', 'L', 'U', m, n, a, lda, work)
  call begin_test('frob_lower_unit')
  call print_scalar('result', result)
  call end_test()

  ! Test 17: M=0 quick return
  result = zlantr('M', 'U', 'N', 0, 3, a, lda, work)
  call begin_test('m_zero')
  call print_scalar('result', result)
  call end_test()

  ! Test 18: N=0 quick return
  result = zlantr('M', 'U', 'N', 3, 0, a, lda, work)
  call begin_test('n_zero')
  call print_scalar('result', result)
  call end_test()

  ! Test 19: 1x1 matrix
  a = (0.0d0, 0.0d0)
  a(1,1) = (3.0d0, 4.0d0)
  result = zlantr('F', 'U', 'N', 1, 1, a, lda, work)
  call begin_test('frob_1x1')
  call print_scalar('result', result)
  call end_test()

  ! Test 20: 1x1 unit diag (should be 1.0)
  result = zlantr('M', 'U', 'U', 1, 1, a, lda, work)
  call begin_test('max_1x1_unit')
  call print_scalar('result', result)
  call end_test()

  ! Test 21: O norm (same as '1')
  a = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 2.0d0)
  a(1,2) = (3.0d0, 4.0d0)
  a(1,3) = (5.0d0, 6.0d0)
  a(2,2) = (7.0d0, 8.0d0)
  a(2,3) = (9.0d0, 1.0d0)
  a(3,3) = (2.0d0, 3.0d0)
  result = zlantr('O', 'U', 'N', 3, 3, a, lda, work)
  call begin_test('one_O_upper_nonunit')
  call print_scalar('result', result)
  call end_test()

  ! Test 22: Trapezoidal (M > N, lower)
  ! 4x2 lower trapezoidal
  a = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 1.0d0)
  a(2,1) = (2.0d0, 2.0d0)
  a(3,1) = (3.0d0, 3.0d0)
  a(4,1) = (4.0d0, 4.0d0)
  a(2,2) = (5.0d0, 5.0d0)
  a(3,2) = (6.0d0, 6.0d0)
  a(4,2) = (7.0d0, 7.0d0)
  result = zlantr('1', 'L', 'N', 4, 2, a, lda, work)
  call begin_test('one_lower_trap')
  call print_scalar('result', result)
  call end_test()

  ! Test 23: Trapezoidal (M < N, upper)
  ! 2x4 upper trapezoidal
  a = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 1.0d0)
  a(1,2) = (2.0d0, 2.0d0)
  a(2,2) = (3.0d0, 3.0d0)
  a(1,3) = (4.0d0, 4.0d0)
  a(2,3) = (5.0d0, 5.0d0)
  a(1,4) = (6.0d0, 6.0d0)
  a(2,4) = (7.0d0, 7.0d0)
  result = zlantr('I', 'U', 'N', 2, 4, a, lda, work)
  call begin_test('inf_upper_trap')
  call print_scalar('result', result)
  call end_test()

end program
