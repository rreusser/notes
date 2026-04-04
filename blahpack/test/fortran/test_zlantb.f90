program test_zlantb
  use test_utils
  implicit none
  complex*16 :: ab3(3, 4)
  complex*16 :: ab2(2, 3)
  complex*16 :: ab1(1, 3)
  double precision :: work(10)
  double precision :: result
  double precision :: zlantb
  external :: zlantb

  ! ---------------------------------------------------------------
  ! 4x4 upper triangular band matrix with K=2 superdiagonals.
  ! LDAB = K+1 = 3.
  !
  ! Full matrix A:
  !   A = [ (1+2i)  (3+4i)  (5+6i)    0   ]
  !       [    0    (7+8i)  (9+1i)  (2+3i) ]
  !       [    0       0    (4+5i)  (6+7i) ]
  !       [    0       0       0    (8+9i) ]
  !
  ! Band storage (upper, LDAB=3):
  !   Row 1 (K-1 superdiag):  *       *      (5+6i)  (2+3i)
  !   Row 2 (K superdiag):    *      (3+4i)  (9+1i)  (6+7i)
  !   Row 3 (diagonal):      (1+2i)  (7+8i)  (4+5i)  (8+9i)
  !
  ! AB(k+1+i-j,j) = A(i,j) with k=2 (1-indexed)
  ! ---------------------------------------------------------------

  ab3 = (0.0d0, 0.0d0)

  ! Column 1: A(1,1) -> AB(3,1)
  ab3(3,1) = (1.0d0, 2.0d0)

  ! Column 2: A(1,2) -> AB(2,2), A(2,2) -> AB(3,2)
  ab3(2,2) = (3.0d0, 4.0d0)
  ab3(3,2) = (7.0d0, 8.0d0)

  ! Column 3: A(1,3) -> AB(1,3), A(2,3) -> AB(2,3), A(3,3) -> AB(3,3)
  ab3(1,3) = (5.0d0, 6.0d0)
  ab3(2,3) = (9.0d0, 1.0d0)
  ab3(3,3) = (4.0d0, 5.0d0)

  ! Column 4: A(2,4) -> AB(1,4), A(3,4) -> AB(2,4), A(4,4) -> AB(3,4)
  ab3(1,4) = (2.0d0, 3.0d0)
  ab3(2,4) = (6.0d0, 7.0d0)
  ab3(3,4) = (8.0d0, 9.0d0)

  ! Test 1: max norm, upper, non-unit
  result = zlantb('M', 'U', 'N', 4, 2, ab3, 3, work)
  call begin_test('max_upper_nonunit')
  call print_scalar('result', result)
  call end_test()

  ! Test 2: max norm, upper, unit
  result = zlantb('M', 'U', 'U', 4, 2, ab3, 3, work)
  call begin_test('max_upper_unit')
  call print_scalar('result', result)
  call end_test()

  ! Test 3: one norm, upper, non-unit
  result = zlantb('1', 'U', 'N', 4, 2, ab3, 3, work)
  call begin_test('one_upper_nonunit')
  call print_scalar('result', result)
  call end_test()

  ! Test 4: one norm, upper, unit
  result = zlantb('1', 'U', 'U', 4, 2, ab3, 3, work)
  call begin_test('one_upper_unit')
  call print_scalar('result', result)
  call end_test()

  ! Test 5: inf norm, upper, non-unit
  result = zlantb('I', 'U', 'N', 4, 2, ab3, 3, work)
  call begin_test('inf_upper_nonunit')
  call print_scalar('result', result)
  call end_test()

  ! Test 6: inf norm, upper, unit
  result = zlantb('I', 'U', 'U', 4, 2, ab3, 3, work)
  call begin_test('inf_upper_unit')
  call print_scalar('result', result)
  call end_test()

  ! Test 7: frobenius norm, upper, non-unit
  result = zlantb('F', 'U', 'N', 4, 2, ab3, 3, work)
  call begin_test('frob_upper_nonunit')
  call print_scalar('result', result)
  call end_test()

  ! Test 8: frobenius norm, upper, unit
  result = zlantb('F', 'U', 'U', 4, 2, ab3, 3, work)
  call begin_test('frob_upper_unit')
  call print_scalar('result', result)
  call end_test()

  ! ---------------------------------------------------------------
  ! 4x4 lower triangular band matrix with K=2 subdiagonals.
  ! LDAB = K+1 = 3.
  !
  ! Full matrix A:
  !   A = [ (1+2i)    0       0       0   ]
  !       [ (3+4i)  (7+8i)    0       0   ]
  !       [ (5+6i)  (9+1i)  (4+5i)    0   ]
  !       [    0    (2+3i)  (6+7i)  (8+9i) ]
  !
  ! Band storage (lower, LDAB=3):
  !   Row 1 (diagonal):    (1+2i)  (7+8i)  (4+5i)  (8+9i)
  !   Row 2 (1st subdiag): (3+4i)  (9+1i)  (6+7i)     *
  !   Row 3 (2nd subdiag): (5+6i)  (2+3i)     *       *
  !
  ! AB(1+i-j,j) = A(i,j) (1-indexed)
  ! ---------------------------------------------------------------

  ab3 = (0.0d0, 0.0d0)

  ! Column 1: A(1,1) -> AB(1,1), A(2,1) -> AB(2,1), A(3,1) -> AB(3,1)
  ab3(1,1) = (1.0d0, 2.0d0)
  ab3(2,1) = (3.0d0, 4.0d0)
  ab3(3,1) = (5.0d0, 6.0d0)

  ! Column 2: A(2,2) -> AB(1,2), A(3,2) -> AB(2,2), A(4,2) -> AB(3,2)
  ab3(1,2) = (7.0d0, 8.0d0)
  ab3(2,2) = (9.0d0, 1.0d0)
  ab3(3,2) = (2.0d0, 3.0d0)

  ! Column 3: A(3,3) -> AB(1,3), A(4,3) -> AB(2,3)
  ab3(1,3) = (4.0d0, 5.0d0)
  ab3(2,3) = (6.0d0, 7.0d0)

  ! Column 4: A(4,4) -> AB(1,4)
  ab3(1,4) = (8.0d0, 9.0d0)

  ! Test 9: max norm, lower, non-unit
  result = zlantb('M', 'L', 'N', 4, 2, ab3, 3, work)
  call begin_test('max_lower_nonunit')
  call print_scalar('result', result)
  call end_test()

  ! Test 10: max norm, lower, unit
  result = zlantb('M', 'L', 'U', 4, 2, ab3, 3, work)
  call begin_test('max_lower_unit')
  call print_scalar('result', result)
  call end_test()

  ! Test 11: one norm, lower, non-unit
  result = zlantb('1', 'L', 'N', 4, 2, ab3, 3, work)
  call begin_test('one_lower_nonunit')
  call print_scalar('result', result)
  call end_test()

  ! Test 12: one norm, lower, unit
  result = zlantb('1', 'L', 'U', 4, 2, ab3, 3, work)
  call begin_test('one_lower_unit')
  call print_scalar('result', result)
  call end_test()

  ! Test 13: inf norm, lower, non-unit
  result = zlantb('I', 'L', 'N', 4, 2, ab3, 3, work)
  call begin_test('inf_lower_nonunit')
  call print_scalar('result', result)
  call end_test()

  ! Test 14: inf norm, lower, unit
  result = zlantb('I', 'L', 'U', 4, 2, ab3, 3, work)
  call begin_test('inf_lower_unit')
  call print_scalar('result', result)
  call end_test()

  ! Test 15: frobenius norm, lower, non-unit
  result = zlantb('F', 'L', 'N', 4, 2, ab3, 3, work)
  call begin_test('frob_lower_nonunit')
  call print_scalar('result', result)
  call end_test()

  ! Test 16: frobenius norm, lower, unit
  result = zlantb('F', 'L', 'U', 4, 2, ab3, 3, work)
  call begin_test('frob_lower_unit')
  call print_scalar('result', result)
  call end_test()

  ! ---------------------------------------------------------------
  ! Edge cases
  ! ---------------------------------------------------------------

  ! Test 17: N=0 quick return
  result = zlantb('M', 'U', 'N', 0, 2, ab3, 3, work)
  call begin_test('n_zero')
  call print_scalar('result', result)
  call end_test()

  ! Test 18: K=0 (diagonal only), upper, non-unit
  ! 3x3 diagonal matrix with K=0, LDAB=1
  ab1(1,1) = (3.0d0, 4.0d0)
  ab1(1,2) = (1.0d0, 1.0d0)
  ab1(1,3) = (2.0d0, 2.0d0)
  result = zlantb('M', 'U', 'N', 3, 0, ab1, 1, work)
  call begin_test('max_k0_nonunit')
  call print_scalar('result', result)
  call end_test()

  ! Test 19: K=0, frobenius, upper, unit
  result = zlantb('F', 'U', 'U', 3, 0, ab1, 1, work)
  call begin_test('frob_k0_unit')
  call print_scalar('result', result)
  call end_test()

  ! Test 20: 1x1 matrix
  ab1(1,1) = (3.0d0, 4.0d0)
  result = zlantb('F', 'U', 'N', 1, 0, ab1, 1, work)
  call begin_test('frob_1x1')
  call print_scalar('result', result)
  call end_test()

  ! Test 21: K=1 upper, N=3, non-unit, one norm
  ! Full matrix A:
  !   A = [ (1+1i)  (2+2i)    0   ]
  !       [    0    (3+3i)  (4+4i) ]
  !       [    0       0    (5+5i) ]
  ! Band storage (K=1, LDAB=2):
  !   Row 1: *       (2+2i)  (4+4i)
  !   Row 2: (1+1i)  (3+3i)  (5+5i)
  ab2 = (0.0d0, 0.0d0)
  ab2(2,1) = (1.0d0, 1.0d0)
  ab2(1,2) = (2.0d0, 2.0d0)
  ab2(2,2) = (3.0d0, 3.0d0)
  ab2(1,3) = (4.0d0, 4.0d0)
  ab2(2,3) = (5.0d0, 5.0d0)
  result = zlantb('1', 'U', 'N', 3, 1, ab2, 2, work)
  call begin_test('one_k1_upper_nonunit')
  call print_scalar('result', result)
  call end_test()

  ! Test 22: K=1 lower, N=3, unit, inf norm
  ! Full matrix A:
  !   A = [  1       0       0   ]
  !       [ (2+2i)   1       0   ]
  !       [    0    (4+4i)   1   ]
  ! Band storage (K=1, LDAB=2):
  !   Row 1: diag     diag    diag
  !   Row 2: (2+2i)  (4+4i)    *
  ab2 = (0.0d0, 0.0d0)
  ab2(1,1) = (99.0d0, 99.0d0)
  ab2(2,1) = (2.0d0, 2.0d0)
  ab2(1,2) = (99.0d0, 99.0d0)
  ab2(2,2) = (4.0d0, 4.0d0)
  ab2(1,3) = (99.0d0, 99.0d0)
  result = zlantb('I', 'L', 'U', 3, 1, ab2, 2, work)
  call begin_test('inf_k1_lower_unit')
  call print_scalar('result', result)
  call end_test()

end program
