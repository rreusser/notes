program test_dorm22
  use test_utils
  implicit none

  ! Q is block 2x2: Q11 (N1xN1), Q12 (N1xN2, upper tri), Q21 (N2xN1, lower tri), Q22 (N2xN2)
  ! Note: dorm22 treats Q12 as "Upper" triangular block at Q(1,N2+1) of size N1xN2
  ! and Q21 as "Lower" triangular block at Q(N1+1,1) of size N2xN1.
  double precision :: Q(8, 8), C(8, 8), WORK(200)
  integer :: info, i, j, LWORK
  integer :: M, N, N1, N2, LDQ, LDC

  LWORK = 200
  LDQ = 8
  LDC = 8

  ! ===== Setup: N1=3, N2=2, NQ=5 =====
  ! Build Q to satisfy the banded structure.
  ! Upper block Q(1:N1, 1:N2) - arbitrary N1xN2
  ! Upper triangular block Q(1:N1, N2+1:NQ) - size N1xN1 upper tri  (wait - mismatch)
  !
  ! Re-reading DORM22: Q is NQ x NQ. The calls are:
  !   DTRMM(.., 'Lower', .., N1, LEN, ONE, Q(1, N2+1), LDQ, ...) -- N1xN1 lower tri block
  !   DGEMM(.., N1, LEN, N2, ONE, Q, LDQ, C(1,I), ...) -- top-left N1xN2 rect
  !   DTRMM(.., 'Upper', .., N2, LEN, ONE, Q(N1+1, 1), LDQ, ...) -- N2xN2 upper tri block
  !   DGEMM(.., N2, LEN, N1, ONE, Q(N1+1,N2+1), LDQ, C(N2+1,I), ..) -- bottom-right N2xN1 rect
  !
  ! So the structure is:
  !   Q(1:N1, 1:N2)       : N1 x N2 rectangular
  !   Q(1:N1, N2+1:N2+N1) : N1 x N1 LOWER triangular
  !   Q(N1+1:N1+N2, 1:N2) : N2 x N2 UPPER triangular
  !   Q(N1+1:N1+N2, N2+1:N2+N1) : N2 x N1 rectangular
  !
  ! The "upper entries" of the lower-tri block and "lower entries" of the upper-tri block
  ! are NOT referenced, but we set them to zero for clarity.
  !
  ! With NQ = N1 + N2, the total column count is N2 + N1 = NQ. Good.

  Q = 0.0d0
  N1 = 3
  N2 = 2
  M = 5

  ! Q(1:3, 1:2): N1 x N2 rectangular block
  Q(1,1) = 0.5d0; Q(1,2) = -0.3d0
  Q(2,1) = 0.2d0; Q(2,2) = 0.8d0
  Q(3,1) = -0.4d0; Q(3,2) = 0.1d0
  ! Q(1:3, 3:5): N1 x N1 lower-triangular block
  Q(1,3) = 1.1d0
  Q(2,3) = 0.7d0;  Q(2,4) = 0.9d0
  Q(3,3) = -0.5d0; Q(3,4) = 0.4d0; Q(3,5) = 1.2d0
  ! Q(4:5, 1:2): N2 x N2 upper-triangular block
  Q(4,1) = 0.6d0;  Q(4,2) = -0.2d0
                   Q(5,2) = 1.3d0
  ! Q(4:5, 3:5): N2 x N1 rectangular block
  Q(4,3) = 0.3d0; Q(4,4) = -0.1d0; Q(4,5) = 0.5d0
  Q(5,3) = -0.7d0; Q(5,4) = 0.4d0; Q(5,5) = 0.2d0

  ! ===== Test 1: LEFT / NoTrans, M=NQ=5, N=4 =====
  N = 4
  C = 0.0d0
  do j = 1, N
    do i = 1, M
      C(i,j) = dble(i - 2 + j) * 0.25d0 + 0.1d0
    end do
  end do
  call dorm22('L', 'N', M, N, N1, N2, Q, LDQ, C, LDC, WORK, LWORK, info)
  call begin_test('left_notrans')
  call print_int('info', info)
  call print_matrix('c', C, LDC, M, N)
  call end_test()

  ! ===== Test 2: LEFT / Trans =====
  C = 0.0d0
  do j = 1, N
    do i = 1, M
      C(i,j) = dble(i - 2 + j) * 0.25d0 + 0.1d0
    end do
  end do
  call dorm22('L', 'T', M, N, N1, N2, Q, LDQ, C, LDC, WORK, LWORK, info)
  call begin_test('left_trans')
  call print_int('info', info)
  call print_matrix('c', C, LDC, M, N)
  call end_test()

  ! ===== Test 3: RIGHT / NoTrans, M=4, N=NQ=5 =====
  M = 4
  N = 5
  C = 0.0d0
  do j = 1, N
    do i = 1, M
      C(i,j) = dble(i - 2 + j) * 0.25d0 + 0.1d0
    end do
  end do
  call dorm22('R', 'N', M, N, N1, N2, Q, LDQ, C, LDC, WORK, LWORK, info)
  call begin_test('right_notrans')
  call print_int('info', info)
  call print_matrix('c', C, LDC, M, N)
  call end_test()

  ! ===== Test 4: RIGHT / Trans =====
  C = 0.0d0
  do j = 1, N
    do i = 1, M
      C(i,j) = dble(i - 2 + j) * 0.25d0 + 0.1d0
    end do
  end do
  call dorm22('R', 'T', M, N, N1, N2, Q, LDQ, C, LDC, WORK, LWORK, info)
  call begin_test('right_trans')
  call print_int('info', info)
  call print_matrix('c', C, LDC, M, N)
  call end_test()

  ! ===== Test 5: N1=0 (pure upper-triangular case) LEFT NoTrans =====
  ! When N1=0, the matrix Q is pure upper triangular (NQ x NQ)
  ! We need a fresh Q: NQ = N2 = 3, so 3x3 upper triangular
  Q = 0.0d0
  N1 = 0
  N2 = 3
  M = 3
  N = 4
  Q(1,1) = 1.0d0; Q(1,2) = 0.5d0; Q(1,3) = -0.2d0
                  Q(2,2) = 0.8d0; Q(2,3) = 0.3d0
                                  Q(3,3) = 1.2d0
  C = 0.0d0
  do j = 1, N
    do i = 1, M
      C(i,j) = dble(i + j) * 0.3d0
    end do
  end do
  call dorm22('L', 'N', M, N, N1, N2, Q, LDQ, C, LDC, WORK, LWORK, info)
  call begin_test('n1_zero_left_notrans')
  call print_int('info', info)
  call print_matrix('c', C, LDC, M, N)
  call end_test()

  ! ===== Test 6: N2=0 (pure lower-triangular case) RIGHT Trans =====
  Q = 0.0d0
  N1 = 3
  N2 = 0
  M = 4
  N = 3
  Q(1,1) = 1.0d0
  Q(2,1) = 0.4d0; Q(2,2) = 0.9d0
  Q(3,1) = -0.3d0; Q(3,2) = 0.6d0; Q(3,3) = 1.1d0
  C = 0.0d0
  do j = 1, N
    do i = 1, M
      C(i,j) = dble(i - j) * 0.2d0 + 0.5d0
    end do
  end do
  call dorm22('R', 'T', M, N, N1, N2, Q, LDQ, C, LDC, WORK, LWORK, info)
  call begin_test('n2_zero_right_trans')
  call print_int('info', info)
  call print_matrix('c', C, LDC, M, N)
  call end_test()

  ! ===== Test 7: M=0 quick return (LEFT: NQ=M, so N1=N2=0) =====
  call dorm22('L', 'N', 0, 4, 0, 0, Q, LDQ, C, LDC, WORK, LWORK, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! ===== Test 8: N=0 quick return =====
  N1 = 3
  N2 = 2
  call dorm22('L', 'N', 5, 0, N1, N2, Q, LDQ, C, LDC, WORK, LWORK, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

end program
