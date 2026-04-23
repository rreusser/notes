program test_zlangb
  use test_utils
  implicit none
  complex*16 :: ab4(4, 5)
  complex*16 :: ab3(3, 4)
  complex*16 :: ab1(1, 3)
  double precision :: work(10)
  double precision :: result
  double precision :: zlangb
  external :: zlangb

  ! ---------------------------------------------------------------
  ! 5x5 general band matrix with KL=1 sub-diagonal, KU=2 super-diagonals.
  ! LDAB = KL+KU+1 = 4.
  !
  ! Full matrix A:
  !   A = [ (1+2i)  (3+4i)  (5+6i)    0       0   ]
  !       [ (7+8i)  (9+1i)  (2+3i)  (4+5i)    0   ]
  !       [    0    (6+7i)  (8+9i)  (1+2i)  (3+4i) ]
  !       [    0       0    (5+6i)  (7+8i)  (9+1i) ]
  !       [    0       0       0    (2+3i)  (4+5i) ]
  !
  ! Band storage (LDAB=4):
  !   AB(KU+1+i-j,j) = A(i,j)
  !   Row 1 (2nd superdiag): *       *      (5+6i)  (4+5i)  (3+4i)
  !   Row 2 (1st superdiag): *      (3+4i)  (2+3i)  (1+2i)  (9+1i)
  !   Row 3 (diagonal):     (1+2i)  (9+1i)  (8+9i)  (7+8i)  (4+5i)
  !   Row 4 (1st subdiag):  (7+8i)  (6+7i)  (5+6i)  (2+3i)     *
  ! ---------------------------------------------------------------

  ab4 = (0.0d0, 0.0d0)

  ! Column 1: A(1,1)->AB(3,1), A(2,1)->AB(4,1)
  ab4(3,1) = (1.0d0, 2.0d0)
  ab4(4,1) = (7.0d0, 8.0d0)

  ! Column 2: A(1,2)->AB(2,2), A(2,2)->AB(3,2), A(3,2)->AB(4,2)
  ab4(2,2) = (3.0d0, 4.0d0)
  ab4(3,2) = (9.0d0, 1.0d0)
  ab4(4,2) = (6.0d0, 7.0d0)

  ! Column 3: A(1,3)->AB(1,3), A(2,3)->AB(2,3), A(3,3)->AB(3,3), A(4,3)->AB(4,3)
  ab4(1,3) = (5.0d0, 6.0d0)
  ab4(2,3) = (2.0d0, 3.0d0)
  ab4(3,3) = (8.0d0, 9.0d0)
  ab4(4,3) = (5.0d0, 6.0d0)

  ! Column 4: A(2,4)->AB(1,4), A(3,4)->AB(2,4), A(4,4)->AB(3,4), A(5,4)->AB(4,4)
  ab4(1,4) = (4.0d0, 5.0d0)
  ab4(2,4) = (1.0d0, 2.0d0)
  ab4(3,4) = (7.0d0, 8.0d0)
  ab4(4,4) = (2.0d0, 3.0d0)

  ! Column 5: A(3,5)->AB(1,5), A(4,5)->AB(2,5), A(5,5)->AB(3,5)
  ab4(1,5) = (3.0d0, 4.0d0)
  ab4(2,5) = (9.0d0, 1.0d0)
  ab4(3,5) = (4.0d0, 5.0d0)

  ! Test 1: max norm
  result = zlangb('M', 5, 1, 2, ab4, 4, work)
  call begin_test('max_5x5')
  call print_scalar('result', result)
  call end_test()

  ! Test 2: one norm
  result = zlangb('1', 5, 1, 2, ab4, 4, work)
  call begin_test('one_5x5')
  call print_scalar('result', result)
  call end_test()

  ! Test 3: inf norm
  result = zlangb('I', 5, 1, 2, ab4, 4, work)
  call begin_test('inf_5x5')
  call print_scalar('result', result)
  call end_test()

  ! Test 4: frobenius norm
  result = zlangb('F', 5, 1, 2, ab4, 4, work)
  call begin_test('frob_5x5')
  call print_scalar('result', result)
  call end_test()

  ! ---------------------------------------------------------------
  ! 4x4 general band matrix with KL=1 sub-diagonal, KU=1 super-diagonal.
  ! LDAB = KL+KU+1 = 3.
  !
  ! Full matrix A:
  !   A = [ (1+1i)  (2+2i)    0       0   ]
  !       [ (3+3i)  (4+4i)  (5+5i)    0   ]
  !       [    0    (6+6i)  (7+7i)  (8+8i) ]
  !       [    0       0    (9+9i)  (1+1i) ]
  !
  ! Band storage (LDAB=3):
  !   Row 1 (superdiag):  *      (2+2i)  (5+5i)  (8+8i)
  !   Row 2 (diagonal):  (1+1i)  (4+4i)  (7+7i)  (1+1i)
  !   Row 3 (subdiag):   (3+3i)  (6+6i)  (9+9i)     *
  ! ---------------------------------------------------------------

  ab3 = (0.0d0, 0.0d0)

  ! Column 1
  ab3(2,1) = (1.0d0, 1.0d0)
  ab3(3,1) = (3.0d0, 3.0d0)

  ! Column 2
  ab3(1,2) = (2.0d0, 2.0d0)
  ab3(2,2) = (4.0d0, 4.0d0)
  ab3(3,2) = (6.0d0, 6.0d0)

  ! Column 3
  ab3(1,3) = (5.0d0, 5.0d0)
  ab3(2,3) = (7.0d0, 7.0d0)
  ab3(3,3) = (9.0d0, 9.0d0)

  ! Column 4
  ab3(1,4) = (8.0d0, 8.0d0)
  ab3(2,4) = (1.0d0, 1.0d0)

  ! Test 5: max norm, tridiag
  result = zlangb('M', 4, 1, 1, ab3, 3, work)
  call begin_test('max_4x4_tridiag')
  call print_scalar('result', result)
  call end_test()

  ! Test 6: one norm, tridiag
  result = zlangb('1', 4, 1, 1, ab3, 3, work)
  call begin_test('one_4x4_tridiag')
  call print_scalar('result', result)
  call end_test()

  ! Test 7: inf norm, tridiag
  result = zlangb('I', 4, 1, 1, ab3, 3, work)
  call begin_test('inf_4x4_tridiag')
  call print_scalar('result', result)
  call end_test()

  ! Test 8: frobenius norm, tridiag
  result = zlangb('F', 4, 1, 1, ab3, 3, work)
  call begin_test('frob_4x4_tridiag')
  call print_scalar('result', result)
  call end_test()

  ! ---------------------------------------------------------------
  ! Edge cases
  ! ---------------------------------------------------------------

  ! Test 9: N=0 quick return
  result = zlangb('M', 0, 1, 2, ab4, 4, work)
  call begin_test('n_zero')
  call print_scalar('result', result)
  call end_test()

  ! Test 10: diagonal only (KL=0, KU=0)
  ! 3x3 diagonal matrix, LDAB=1
  ab1(1,1) = (3.0d0, 4.0d0)
  ab1(1,2) = (1.0d0, 1.0d0)
  ab1(1,3) = (2.0d0, 2.0d0)

  result = zlangb('M', 3, 0, 0, ab1, 1, work)
  call begin_test('max_diag_only')
  call print_scalar('result', result)
  call end_test()

  ! Test 11: one norm, diagonal only
  result = zlangb('1', 3, 0, 0, ab1, 1, work)
  call begin_test('one_diag_only')
  call print_scalar('result', result)
  call end_test()

  ! Test 12: inf norm, diagonal only
  result = zlangb('I', 3, 0, 0, ab1, 1, work)
  call begin_test('inf_diag_only')
  call print_scalar('result', result)
  call end_test()

  ! Test 13: frobenius norm, diagonal only
  result = zlangb('F', 3, 0, 0, ab1, 1, work)
  call begin_test('frob_diag_only')
  call print_scalar('result', result)
  call end_test()

  ! Test 14: 1x1 matrix
  ab1(1,1) = (3.0d0, 4.0d0)
  result = zlangb('F', 1, 0, 0, ab1, 1, work)
  call begin_test('frob_1x1')
  call print_scalar('result', result)
  call end_test()

  ! Test 15: one norm, 1x1
  result = zlangb('1', 1, 0, 0, ab1, 1, work)
  call begin_test('one_1x1')
  call print_scalar('result', result)
  call end_test()

end program
