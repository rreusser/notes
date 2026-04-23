program test_dlaqp2
  use test_utils
  implicit none

  double precision :: A(6, 6), TAU(6), WORK(20), VN1(6), VN2(6)
  integer :: JPVT(6), i, j

  ! Test 1: Basic 4x3 matrix, offset=0
  A = 0.0d0
  A(1,1) = 1.0d0; A(2,1) = 2.0d0; A(3,1) = 0.0d0; A(4,1) = 1.0d0
  A(1,2) = 0.0d0; A(2,2) = 1.0d0; A(3,2) = 3.0d0; A(4,2) = 2.0d0
  A(1,3) = 3.0d0; A(2,3) = 0.0d0; A(3,3) = 1.0d0; A(4,3) = 2.0d0
  JPVT = 0
  do i = 1, 3
    JPVT(i) = i
  end do
  do j = 1, 3
    VN1(j) = 0.0d0
    do i = 1, 4
      VN1(j) = VN1(j) + A(i,j)**2
    end do
    VN1(j) = sqrt(VN1(j))
    VN2(j) = VN1(j)
  end do
  call dlaqp2(4, 3, 0, A, 6, JPVT, TAU, VN1, VN2, WORK)
  call begin_test('basic_4x3')
  call print_matrix('a', A, 6, 4, 3)
  call print_array('tau', TAU, 3)
  call print_int_array('jpvt', JPVT, 3)
  call print_array('vn1', VN1, 3)
  call print_array('vn2', VN2, 3)
  call end_test()

  ! Test 2: Square 3x3 matrix, offset=0
  A = 0.0d0
  A(1,1) = 1.0d0; A(2,1) = 0.0d0; A(3,1) = 1.0d0
  A(1,2) = 2.0d0; A(2,2) = 1.0d0; A(3,2) = 0.0d0
  A(1,3) = 3.0d0; A(2,3) = 2.0d0; A(3,3) = 1.0d0
  JPVT = 0
  do i = 1, 3
    JPVT(i) = i
  end do
  do j = 1, 3
    VN1(j) = 0.0d0
    do i = 1, 3
      VN1(j) = VN1(j) + A(i,j)**2
    end do
    VN1(j) = sqrt(VN1(j))
    VN2(j) = VN1(j)
  end do
  call dlaqp2(3, 3, 0, A, 6, JPVT, TAU, VN1, VN2, WORK)
  call begin_test('square_3x3')
  call print_matrix('a', A, 6, 3, 3)
  call print_array('tau', TAU, 3)
  call print_int_array('jpvt', JPVT, 3)
  call end_test()

  ! Test 3: N=1 edge case
  A = 0.0d0
  A(1,1) = 3.0d0; A(2,1) = 4.0d0; A(3,1) = 0.0d0
  JPVT(1) = 1
  VN1(1) = 5.0d0
  VN2(1) = 5.0d0
  call dlaqp2(3, 1, 0, A, 6, JPVT, TAU, VN1, VN2, WORK)
  call begin_test('n_one')
  call print_matrix('a', A, 6, 3, 1)
  call print_array('tau', TAU, 1)
  call print_int_array('jpvt', JPVT, 1)
  call end_test()

  ! Test 4: 1x1 matrix
  A = 0.0d0
  A(1,1) = 7.0d0
  JPVT(1) = 1
  VN1(1) = 7.0d0
  VN2(1) = 7.0d0
  call dlaqp2(1, 1, 0, A, 6, JPVT, TAU, VN1, VN2, WORK)
  call begin_test('one_by_one')
  call print_matrix('a', A, 6, 1, 1)
  call print_array('tau', TAU, 1)
  call print_int_array('jpvt', JPVT, 1)
  call end_test()

  ! Test 5: N=0 (empty, quick return)
  A = 0.0d0
  JPVT = 0
  call dlaqp2(3, 0, 0, A, 6, JPVT, TAU, VN1, VN2, WORK)
  call begin_test('n_zero')
  call end_test()

  ! Test 6: With offset=1 (first row pivoted but not factored)
  A = 0.0d0
  A(1,1) = 5.0d0; A(2,1) = 0.0d0; A(3,1) = 0.0d0; A(4,1) = 0.0d0
  A(1,2) = 1.0d0; A(2,2) = 1.0d0; A(3,2) = 3.0d0; A(4,2) = 2.0d0
  A(1,3) = 2.0d0; A(2,3) = 2.0d0; A(3,3) = 0.0d0; A(4,3) = 1.0d0
  JPVT = 0
  do i = 1, 3
    JPVT(i) = i
  end do
  ! Column norms of submatrix rows 2..4
  do j = 1, 3
    VN1(j) = 0.0d0
    do i = 2, 4
      VN1(j) = VN1(j) + A(i,j)**2
    end do
    VN1(j) = sqrt(VN1(j))
    VN2(j) = VN1(j)
  end do
  call dlaqp2(4, 3, 1, A, 6, JPVT, TAU, VN1, VN2, WORK)
  call begin_test('offset_1')
  call print_matrix('a', A, 6, 4, 3)
  call print_array('tau', TAU, 3)
  call print_int_array('jpvt', JPVT, 3)
  call end_test()

  ! Test 7: Pivoting reorders columns (column 3 has largest norm)
  A = 0.0d0
  A(1,1) = 0.1d0; A(2,1) = 0.1d0; A(3,1) = 0.1d0
  A(1,2) = 1.0d0; A(2,2) = 1.0d0; A(3,2) = 0.0d0
  A(1,3) = 5.0d0; A(2,3) = 3.0d0; A(3,3) = 4.0d0
  JPVT = 0
  do i = 1, 3
    JPVT(i) = i
  end do
  do j = 1, 3
    VN1(j) = 0.0d0
    do i = 1, 3
      VN1(j) = VN1(j) + A(i,j)**2
    end do
    VN1(j) = sqrt(VN1(j))
    VN2(j) = VN1(j)
  end do
  call dlaqp2(3, 3, 0, A, 6, JPVT, TAU, VN1, VN2, WORK)
  call begin_test('pivot_reorder')
  call print_matrix('a', A, 6, 3, 3)
  call print_array('tau', TAU, 3)
  call print_int_array('jpvt', JPVT, 3)
  call end_test()

  ! Test 8: Nearly collinear columns to trigger norm recomputation
  A = 0.0d0
  A(1,1) = 1.0d0; A(2,1) = 2.0d0; A(3,1) = 3.0d0
  A(4,1) = 4.0d0; A(5,1) = 5.0d0; A(6,1) = 6.0d0
  A(1,2) = 1.0d0; A(2,2) = 2.0d0; A(3,2) = 3.0d0
  A(4,2) = 4.0d0; A(5,2) = 5.0d0; A(6,2) = 6.0d0 + 1.0d-10
  A(1,3) = 1.0d0; A(2,3) = 2.0d0; A(3,3) = 3.0d0
  A(4,3) = 4.0d0; A(5,3) = 5.0d0; A(6,3) = 6.0d0 + 1.0d-15
  JPVT = 0
  do i = 1, 3
    JPVT(i) = i
  end do
  do j = 1, 3
    VN1(j) = 0.0d0
    do i = 1, 6
      VN1(j) = VN1(j) + A(i,j)**2
    end do
    VN1(j) = sqrt(VN1(j))
    VN2(j) = VN1(j)
  end do
  call dlaqp2(6, 3, 0, A, 6, JPVT, TAU, VN1, VN2, WORK)
  call begin_test('collinear_recomp')
  call print_matrix('a', A, 6, 6, 3)
  call print_array('tau', TAU, 3)
  call print_int_array('jpvt', JPVT, 3)
  call print_array('vn1', VN1, 3)
  call print_array('vn2', VN2, 3)
  call end_test()

end program
