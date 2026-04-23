program test_zlaqp2
  use test_utils
  implicit none

  complex*16 :: A(6, 6), TAU(6), WORK(20)
  double precision :: A_r(72), TAU_r(12), VN1(6), VN2(6)
  equivalence (A, A_r)
  equivalence (TAU, TAU_r)
  integer :: JPVT(6), i, j

  ! Test 1: 3x3 matrix with column pivoting, offset=0
  ! A = [1+0i 2+1i 3+0i; 0+1i 1+0i 2+1i; 1+1i 0+0i 1+0i]
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(2,1) = (0.0d0, 1.0d0); A(3,1) = (1.0d0, 1.0d0)
  A(1,2) = (2.0d0, 1.0d0); A(2,2) = (1.0d0, 0.0d0); A(3,2) = (0.0d0, 0.0d0)
  A(1,3) = (3.0d0, 0.0d0); A(2,3) = (2.0d0, 1.0d0); A(3,3) = (1.0d0, 0.0d0)
  JPVT = 0
  do i = 1, 3
    JPVT(i) = i
  end do
  ! Compute column norms
  do j = 1, 3
    VN1(j) = 0.0d0
    do i = 1, 3
      VN1(j) = VN1(j) + dble(A(i,j))**2 + dimag(A(i,j))**2
    end do
    VN1(j) = sqrt(VN1(j))
    VN2(j) = VN1(j)
  end do
  call zlaqp2(3, 3, 0, A, 6, JPVT, TAU, VN1, VN2, WORK)
  call begin_test('basic_3x3')
  call print_array('a', A_r, 36)
  call print_array('tau', TAU_r, 6)
  call print_int_array('jpvt', JPVT, 3)
  call print_array('vn1', VN1, 3)
  call print_array('vn2', VN2, 3)
  call end_test()

  ! Test 2: 4x3 matrix with offset=0
  ! A = [1+0i 0+2i 3+1i; 2+1i 1+0i 0+0i; 0+0i 3+1i 1+0i; 1+1i 2+0i 2+1i]
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(2,1) = (2.0d0, 1.0d0); A(3,1) = (0.0d0, 0.0d0); A(4,1) = (1.0d0, 1.0d0)
  A(1,2) = (0.0d0, 2.0d0); A(2,2) = (1.0d0, 0.0d0); A(3,2) = (3.0d0, 1.0d0); A(4,2) = (2.0d0, 0.0d0)
  A(1,3) = (3.0d0, 1.0d0); A(2,3) = (0.0d0, 0.0d0); A(3,3) = (1.0d0, 0.0d0); A(4,3) = (2.0d0, 1.0d0)
  JPVT = 0
  do i = 1, 3
    JPVT(i) = i
  end do
  do j = 1, 3
    VN1(j) = 0.0d0
    do i = 1, 4
      VN1(j) = VN1(j) + dble(A(i,j))**2 + dimag(A(i,j))**2
    end do
    VN1(j) = sqrt(VN1(j))
    VN2(j) = VN1(j)
  end do
  call zlaqp2(4, 3, 0, A, 6, JPVT, TAU, VN1, VN2, WORK)
  call begin_test('rect_4x3')
  call print_array('a', A_r, 48)
  call print_array('tau', TAU_r, 6)
  call print_int_array('jpvt', JPVT, 3)
  call end_test()

  ! Test 3: with offset > 0 (simulate that first row is already factored)
  ! A = [5+0i 1+0i 2+0i; 0+0i 1+1i 2+0i; 0+0i 3+0i 0+1i; 0+0i 2+1i 1+0i]
  ! offset=1 means the first row is already done, we factor rows 2..4, cols 1..3
  A = (0.0d0, 0.0d0)
  A(1,1) = (5.0d0, 0.0d0); A(2,1) = (0.0d0, 0.0d0); A(3,1) = (0.0d0, 0.0d0); A(4,1) = (0.0d0, 0.0d0)
  A(1,2) = (1.0d0, 0.0d0); A(2,2) = (1.0d0, 1.0d0); A(3,2) = (3.0d0, 0.0d0); A(4,2) = (2.0d0, 1.0d0)
  A(1,3) = (2.0d0, 0.0d0); A(2,3) = (2.0d0, 0.0d0); A(3,3) = (0.0d0, 1.0d0); A(4,3) = (1.0d0, 0.0d0)
  JPVT = 0
  do i = 1, 3
    JPVT(i) = i
  end do
  ! Column norms of the submatrix rows 2..4
  do j = 1, 3
    VN1(j) = 0.0d0
    do i = 2, 4
      VN1(j) = VN1(j) + dble(A(i,j))**2 + dimag(A(i,j))**2
    end do
    VN1(j) = sqrt(VN1(j))
    VN2(j) = VN1(j)
  end do
  call zlaqp2(4, 3, 1, A, 6, JPVT, TAU, VN1, VN2, WORK)
  call begin_test('offset_1')
  call print_array('a', A_r, 48)
  call print_array('tau', TAU_r, 6)
  call print_int_array('jpvt', JPVT, 3)
  call end_test()

  ! Test 4: 1x1 matrix (edge case)
  A = (0.0d0, 0.0d0)
  A(1,1) = (3.0d0, 4.0d0)
  JPVT(1) = 1
  VN1(1) = 5.0d0
  VN2(1) = 5.0d0
  call zlaqp2(1, 1, 0, A, 6, JPVT, TAU, VN1, VN2, WORK)
  call begin_test('one_by_one')
  call print_array('a', A_r, 2)
  call print_array('tau', TAU_r, 2)
  call print_int_array('jpvt', JPVT, 1)
  call end_test()

  ! Test 5: N=0 (empty)
  ! MN = min(M-offset, N) = min(3, 0) = 0, so loop does nothing
  A = (0.0d0, 0.0d0)
  JPVT = 0
  call zlaqp2(3, 0, 0, A, 6, JPVT, TAU, VN1, VN2, WORK)
  call begin_test('n_zero')
  call end_test()

  ! Test 6: Nearly collinear columns to trigger norm recomputation (temp2 <= tol3z)
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(2,1) = (2.0d0, 0.0d0); A(3,1) = (3.0d0, 0.0d0)
  A(4,1) = (4.0d0, 0.0d0); A(5,1) = (5.0d0, 0.0d0); A(6,1) = (6.0d0, 0.0d0)
  A(1,2) = (1.0d0, 1.0d-10); A(2,2) = (2.0d0, 1.0d-10); A(3,2) = (3.0d0, 1.0d-10)
  A(4,2) = (4.0d0, 1.0d-10); A(5,2) = (5.0d0, 1.0d-10); A(6,2) = (6.0d0, 1.0d-10)
  A(1,3) = (1.0d0, 0.0d0); A(2,3) = (2.0d0, 0.0d0); A(3,3) = (3.0d0, 0.0d0)
  A(4,3) = (4.0d0, 0.0d0); A(5,3) = (5.0d0, 0.0d0); A(6,3) = (6.0000000001d0, 0.0d0)
  JPVT = 0
  do i = 1, 3
    JPVT(i) = i
  end do
  do j = 1, 3
    VN1(j) = 0.0d0
    do i = 1, 6
      VN1(j) = VN1(j) + dble(A(i,j))**2 + dimag(A(i,j))**2
    end do
    VN1(j) = sqrt(VN1(j))
    VN2(j) = VN1(j)
  end do
  call zlaqp2(6, 3, 0, A, 6, JPVT, TAU, VN1, VN2, WORK)
  call begin_test('collinear_norm_recomp')
  call print_array('a', A_r, 72)
  call print_array('tau', TAU_r, 6)
  call print_int_array('jpvt', JPVT, 3)
  call end_test()

  ! Test 7: Square collinear -- triggers offpi == M-1 with norm degradation
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(2,1) = (2.0d0, 0.0d0); A(3,1) = (3.0d0, 0.0d0)
  A(1,2) = (1.0d0, 1.0d-14); A(2,2) = (2.0d0, 1.0d-14); A(3,2) = (3.0d0, 1.0d-14)
  A(1,3) = (1.0d0, 0.0d0); A(2,3) = (2.0d0, 1.0d-14); A(3,3) = (3.0d0, 0.0d0)
  JPVT = 0
  do i = 1, 3
    JPVT(i) = i
  end do
  do j = 1, 3
    VN1(j) = 0.0d0
    do i = 1, 3
      VN1(j) = VN1(j) + dble(A(i,j))**2 + dimag(A(i,j))**2
    end do
    VN1(j) = sqrt(VN1(j))
    VN2(j) = VN1(j)
  end do
  call zlaqp2(3, 3, 0, A, 6, JPVT, TAU, VN1, VN2, WORK)
  call begin_test('square_collinear')
  call print_array('a', A_r, 36)
  call print_array('tau', TAU_r, 6)
  call print_int_array('jpvt', JPVT, 3)
  call end_test()

  ! Test 8: offset causes offpi to reach M-1 with remaining columns
  ! M=4, N=3, offset=2 => mn=min(4-2,3)=2, so i goes from 0 to 1
  ! offpi = offset+i = 2,3 => on i=1, offpi=3=M-1
  ! Need collinear columns so norm degrades at offpi=3
  A = (0.0d0, 0.0d0)
  ! Fill so that after offset=2, the submatrix rows 3-4 has collinear columns
  A(1,1) = (5.0d0, 0.0d0); A(2,1) = (4.0d0, 0.0d0)
  A(3,1) = (1.0d0, 0.0d0); A(4,1) = (2.0d0, 0.0d0)
  A(1,2) = (3.0d0, 0.0d0); A(2,2) = (2.0d0, 0.0d0)
  A(3,2) = (1.0d0, 1.0d-15); A(4,2) = (2.0d0, 1.0d-15)
  A(1,3) = (1.0d0, 0.0d0); A(2,3) = (0.0d0, 0.0d0)
  A(3,3) = (1.0d0, 0.0d0); A(4,3) = (2.0d0, 1.0d-15)
  JPVT = 0
  do i = 1, 3
    JPVT(i) = i
  end do
  ! Norms of submatrix rows 3..4
  do j = 1, 3
    VN1(j) = 0.0d0
    do i = 3, 4
      VN1(j) = VN1(j) + dble(A(i,j))**2 + dimag(A(i,j))**2
    end do
    VN1(j) = sqrt(VN1(j))
    VN2(j) = VN1(j)
  end do
  call zlaqp2(4, 3, 2, A, 6, JPVT, TAU, VN1, VN2, WORK)
  call begin_test('offset_collinear_last_row')
  call print_array('a', A_r, 48)
  call print_array('tau', TAU_r, 6)
  call print_int_array('jpvt', JPVT, 3)
  call end_test()

end program
