program test_dlaqps
  use test_utils
  implicit none

  integer, parameter :: MAXM = 8, MAXN = 8
  double precision :: A(MAXM, MAXN), TAU(MAXN), AUXV(MAXN), F(MAXN, MAXN)
  double precision :: VN1(MAXN), VN2(MAXN)
  integer :: JPVT(MAXN), KB
  integer :: i, j, M, N, NB, OFFSET
  double precision :: DNRM2

  ! Test 1: 4x3 matrix, NB=2
  M = 4; N = 3; NB = 2; OFFSET = 0
  A = 0.0d0
  A(1,1) = 1.0d0; A(2,1) = 2.0d0; A(3,1) = 0.0d0; A(4,1) = 1.0d0
  A(1,2) = 0.0d0; A(2,2) = 1.0d0; A(3,2) = 3.0d0; A(4,2) = 2.0d0
  A(1,3) = 3.0d0; A(2,3) = 0.0d0; A(3,3) = 1.0d0; A(4,3) = 2.0d0
  do i = 1, N
    JPVT(i) = i
  end do
  do j = 1, N
    VN1(j) = DNRM2(M, A(1,j), 1)
    VN2(j) = VN1(j)
  end do
  F = 0.0d0
  AUXV = 0.0d0
  KB = 0
  call dlaqps(M, N, OFFSET, NB, KB, A, MAXM, JPVT, TAU, VN1, VN2, AUXV, F, MAXN)
  call begin_test('basic_4x3_nb2')
  call print_int('kb', KB)
  call print_array('a', A, MAXM*N)
  call print_array('tau', TAU, N)
  call print_int_array('jpvt', JPVT, N)
  call print_array('f', F, MAXN*N)
  call print_array('vn1', VN1, N)
  call print_array('vn2', VN2, N)
  call end_test()

  ! Test 2: 6x4 matrix, NB=3
  M = 6; N = 4; NB = 3; OFFSET = 0
  A = 0.0d0
  A(1,1) = 2.0d0; A(2,1) = 1.0d0; A(3,1) = 0.0d0; A(4,1) = 3.0d0; A(5,1) = 1.0d0; A(6,1) = 0.0d0
  A(1,2) = 1.0d0; A(2,2) = 0.0d0; A(3,2) = 2.0d0; A(4,2) = 1.0d0; A(5,2) = 0.0d0; A(6,2) = 3.0d0
  A(1,3) = 0.0d0; A(2,3) = 3.0d0; A(3,3) = 1.0d0; A(4,3) = 0.0d0; A(5,3) = 2.0d0; A(6,3) = 1.0d0
  A(1,4) = 1.0d0; A(2,4) = 2.0d0; A(3,4) = 0.0d0; A(4,4) = 1.0d0; A(5,4) = 3.0d0; A(6,4) = 0.0d0
  do i = 1, N
    JPVT(i) = i
  end do
  do j = 1, N
    VN1(j) = DNRM2(M, A(1,j), 1)
    VN2(j) = VN1(j)
  end do
  F = 0.0d0
  AUXV = 0.0d0
  KB = 0
  call dlaqps(M, N, OFFSET, NB, KB, A, MAXM, JPVT, TAU, VN1, VN2, AUXV, F, MAXN)
  call begin_test('rect_6x4_nb3')
  call print_int('kb', KB)
  call print_array('a', A, MAXM*N)
  call print_array('tau', TAU, N)
  call print_int_array('jpvt', JPVT, N)
  call end_test()

  ! Test 3: NB=1 (should factor just 1 column)
  M = 3; N = 3; NB = 1; OFFSET = 0
  A = 0.0d0
  A(1,1) = 1.0d0; A(2,1) = 2.0d0; A(3,1) = 3.0d0
  A(1,2) = 4.0d0; A(2,2) = 5.0d0; A(3,2) = 6.0d0
  A(1,3) = 0.0d0; A(2,3) = 1.0d0; A(3,3) = 2.0d0
  do i = 1, N
    JPVT(i) = i
  end do
  do j = 1, N
    VN1(j) = DNRM2(M, A(1,j), 1)
    VN2(j) = VN1(j)
  end do
  F = 0.0d0
  AUXV = 0.0d0
  KB = 0
  call dlaqps(M, N, OFFSET, NB, KB, A, MAXM, JPVT, TAU, VN1, VN2, AUXV, F, MAXN)
  call begin_test('nb_1')
  call print_int('kb', KB)
  call print_array('a', A, MAXM*N)
  call print_array('tau', TAU, N)
  call print_int_array('jpvt', JPVT, N)
  call end_test()

  ! Test 4: with offset > 0
  M = 4; N = 2; NB = 2; OFFSET = 1
  A = 0.0d0
  A(1,1) = 5.0d0; A(2,1) = 0.0d0; A(3,1) = 0.0d0; A(4,1) = 0.0d0
  A(1,2) = 1.0d0; A(2,2) = 2.0d0; A(3,2) = 1.0d0; A(4,2) = 3.0d0
  do i = 1, N
    JPVT(i) = i
  end do
  do j = 1, N
    VN1(j) = DNRM2(M - OFFSET, A(OFFSET+1,j), 1)
    VN2(j) = VN1(j)
  end do
  F = 0.0d0
  AUXV = 0.0d0
  KB = 0
  call dlaqps(M, N, OFFSET, NB, KB, A, MAXM, JPVT, TAU, VN1, VN2, AUXV, F, MAXN)
  call begin_test('offset_1')
  call print_int('kb', KB)
  call print_array('a', A, MAXM*N)
  call print_array('tau', TAU, N)
  call print_int_array('jpvt', JPVT, N)
  call end_test()

  ! Test 5: Nearly collinear columns to trigger norm recomputation (lsticc > 0)
  M = 6; N = 3; NB = 2; OFFSET = 0
  A = 0.0d0
  ! Column 1: base vector
  A(1,1) = 1.0d0; A(2,1) = 2.0d0; A(3,1) = 3.0d0
  A(4,1) = 4.0d0; A(5,1) = 5.0d0; A(6,1) = 6.0d0
  ! Column 2: base + tiny perturbation
  A(1,2) = 1.0d0; A(2,2) = 2.0d0; A(3,2) = 3.0d0
  A(4,2) = 4.0d0; A(5,2) = 5.0d0; A(6,2) = 6.0d0 + 1.0d-10
  ! Column 3: different tiny perturbation
  A(1,3) = 1.0d0; A(2,3) = 2.0d0; A(3,3) = 3.0d0
  A(4,3) = 4.0d0; A(5,3) = 5.0d0 + 1.0d-10; A(6,3) = 6.0d0
  do i = 1, N
    JPVT(i) = i
  end do
  do j = 1, N
    VN1(j) = DNRM2(M, A(1,j), 1)
    VN2(j) = VN1(j)
  end do
  F = 0.0d0
  AUXV = 0.0d0
  KB = 0
  call dlaqps(M, N, OFFSET, NB, KB, A, MAXM, JPVT, TAU, VN1, VN2, AUXV, F, MAXN)
  call begin_test('collinear_norm_recomp')
  call print_int('kb', KB)
  call print_array('a', A, MAXM*N)
  call print_array('tau', TAU, N)
  call print_int_array('jpvt', JPVT, N)
  call print_array('vn1', VN1, N)
  call print_array('vn2', VN2, N)
  call end_test()

end program
