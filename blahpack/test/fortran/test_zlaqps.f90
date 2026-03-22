program test_zlaqps
  use test_utils
  implicit none

  integer, parameter :: MAXM = 8, MAXN = 8
  complex*16 :: A(MAXM, MAXN), TAU(MAXN), AUXV(MAXN), F(MAXN, MAXN)
  double precision :: A_r(2*MAXM*MAXN), TAU_r(2*MAXN), F_r(2*MAXN*MAXN)
  equivalence (A, A_r)
  equivalence (TAU, TAU_r)
  equivalence (F, F_r)
  double precision :: VN1(MAXN), VN2(MAXN)
  integer :: JPVT(MAXN), KB
  integer :: i, j, M, N, NB, OFFSET

  ! Test 1: 4x3 matrix, NB=2
  M = 4; N = 3; NB = 2; OFFSET = 0
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(2,1) = (2.0d0, 1.0d0); A(3,1) = (0.0d0, 0.0d0); A(4,1) = (1.0d0, 1.0d0)
  A(1,2) = (0.0d0, 2.0d0); A(2,2) = (1.0d0, 0.0d0); A(3,2) = (3.0d0, 1.0d0); A(4,2) = (2.0d0, 0.0d0)
  A(1,3) = (3.0d0, 1.0d0); A(2,3) = (0.0d0, 0.0d0); A(3,3) = (1.0d0, 0.0d0); A(4,3) = (2.0d0, 1.0d0)
  do i = 1, N
    JPVT(i) = i
  end do
  do j = 1, N
    VN1(j) = 0.0d0
    do i = 1, M
      VN1(j) = VN1(j) + dble(A(i,j))**2 + dimag(A(i,j))**2
    end do
    VN1(j) = sqrt(VN1(j))
    VN2(j) = VN1(j)
  end do
  F = (0.0d0, 0.0d0)
  AUXV = (0.0d0, 0.0d0)
  KB = 0
  call zlaqps(M, N, OFFSET, NB, KB, A, MAXM, JPVT, TAU, VN1, VN2, AUXV, F, MAXN)
  call begin_test('basic_4x3_nb2')
  call print_int('kb', KB)
  call print_array('a', A_r, 2*MAXM*N)
  call print_array('tau', TAU_r, 2*N)
  call print_int_array('jpvt', JPVT, N)
  call print_array('f', F_r, 2*MAXN*N)
  call end_test()

  ! Test 2: 6x4 matrix, NB=3
  M = 6; N = 4; NB = 3; OFFSET = 0
  A = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 0.0d0); A(2,1) = (1.0d0, 1.0d0); A(3,1) = (0.0d0, 1.0d0); A(4,1) = (3.0d0, 0.0d0); A(5,1) = (1.0d0, 0.0d0); A(6,1) = (0.0d0, 2.0d0)
  A(1,2) = (1.0d0, 0.0d0); A(2,2) = (0.0d0, 0.0d0); A(3,2) = (2.0d0, 1.0d0); A(4,2) = (1.0d0, 1.0d0); A(5,2) = (0.0d0, 0.0d0); A(6,2) = (3.0d0, 0.0d0)
  A(1,3) = (0.0d0, 1.0d0); A(2,3) = (3.0d0, 0.0d0); A(3,3) = (1.0d0, 0.0d0); A(4,3) = (0.0d0, 2.0d0); A(5,3) = (2.0d0, 1.0d0); A(6,3) = (1.0d0, 0.0d0)
  A(1,4) = (1.0d0, 1.0d0); A(2,4) = (2.0d0, 0.0d0); A(3,4) = (0.0d0, 0.0d0); A(4,4) = (1.0d0, 0.0d0); A(5,4) = (3.0d0, 1.0d0); A(6,4) = (0.0d0, 1.0d0)
  do i = 1, N
    JPVT(i) = i
  end do
  do j = 1, N
    VN1(j) = 0.0d0
    do i = 1, M
      VN1(j) = VN1(j) + dble(A(i,j))**2 + dimag(A(i,j))**2
    end do
    VN1(j) = sqrt(VN1(j))
    VN2(j) = VN1(j)
  end do
  F = (0.0d0, 0.0d0)
  AUXV = (0.0d0, 0.0d0)
  KB = 0
  call zlaqps(M, N, OFFSET, NB, KB, A, MAXM, JPVT, TAU, VN1, VN2, AUXV, F, MAXN)
  call begin_test('rect_6x4_nb3')
  call print_int('kb', KB)
  call print_array('a', A_r, 2*MAXM*N)
  call print_array('tau', TAU_r, 2*N)
  call print_int_array('jpvt', JPVT, N)
  call end_test()

  ! Test 3: NB=1 (should factor just 1 column)
  M = 3; N = 3; NB = 1; OFFSET = 0
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(2,1) = (2.0d0, 0.0d0); A(3,1) = (3.0d0, 0.0d0)
  A(1,2) = (4.0d0, 1.0d0); A(2,2) = (5.0d0, 1.0d0); A(3,2) = (6.0d0, 1.0d0)
  A(1,3) = (0.0d0, 1.0d0); A(2,3) = (1.0d0, 0.0d0); A(3,3) = (2.0d0, 1.0d0)
  do i = 1, N
    JPVT(i) = i
  end do
  do j = 1, N
    VN1(j) = 0.0d0
    do i = 1, M
      VN1(j) = VN1(j) + dble(A(i,j))**2 + dimag(A(i,j))**2
    end do
    VN1(j) = sqrt(VN1(j))
    VN2(j) = VN1(j)
  end do
  F = (0.0d0, 0.0d0)
  AUXV = (0.0d0, 0.0d0)
  KB = 0
  call zlaqps(M, N, OFFSET, NB, KB, A, MAXM, JPVT, TAU, VN1, VN2, AUXV, F, MAXN)
  call begin_test('nb_1')
  call print_int('kb', KB)
  call print_array('a', A_r, 2*MAXM*N)
  call print_array('tau', TAU_r, 2*N)
  call print_int_array('jpvt', JPVT, N)
  call end_test()

  ! Test 4: with offset > 0
  M = 4; N = 2; NB = 2; OFFSET = 1
  A = (0.0d0, 0.0d0)
  A(1,1) = (5.0d0, 0.0d0); A(2,1) = (0.0d0, 0.0d0); A(3,1) = (0.0d0, 0.0d0); A(4,1) = (0.0d0, 0.0d0)
  A(1,2) = (1.0d0, 0.0d0); A(2,2) = (2.0d0, 1.0d0); A(3,2) = (1.0d0, 0.0d0); A(4,2) = (3.0d0, 0.0d0)
  do i = 1, N
    JPVT(i) = i
  end do
  do j = 1, N
    VN1(j) = 0.0d0
    do i = 2, M
      VN1(j) = VN1(j) + dble(A(i,j))**2 + dimag(A(i,j))**2
    end do
    VN1(j) = sqrt(VN1(j))
    VN2(j) = VN1(j)
  end do
  F = (0.0d0, 0.0d0)
  AUXV = (0.0d0, 0.0d0)
  KB = 0
  call zlaqps(M, N, OFFSET, NB, KB, A, MAXM, JPVT, TAU, VN1, VN2, AUXV, F, MAXN)
  call begin_test('offset_1')
  call print_int('kb', KB)
  call print_array('a', A_r, 2*MAXM*N)
  call print_array('tau', TAU_r, 2*N)
  call print_int_array('jpvt', JPVT, N)
  call end_test()

end program
