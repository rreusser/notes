program test_dorgbr
  use test_utils
  implicit none

  integer, parameter :: MAXN = 10, LWORK = 2048
  double precision :: A(MAXN, MAXN), D(MAXN), E(MAXN)
  double precision :: TAUQ(MAXN), TAUP(MAXN), WORK(LWORK)
  double precision :: QtQ(MAXN, MAXN)
  integer :: INFO, i, j, k, M, N, KK

  ! ---------------------------------------------------------------
  ! Test 1: VECT='Q', M > N (4x3 original), generate Q (4x3)
  ! dgebrd reduces 4x3 => Q is 4x3, K=3
  ! ---------------------------------------------------------------
  A = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0
  A(3,1) = 3.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0
  A(4,1) = 1.0d0; A(4,2) = 3.0d0; A(4,3) = 1.0d0
  M = 4; N = 3; KK = 3
  TAUQ = 0.0d0; TAUP = 0.0d0; WORK = 0.0d0
  call DGEBRD(M, N, A, MAXN, D, E, TAUQ, TAUP, WORK, LWORK, INFO)
  ! Now generate Q (M x N = 4x3, K=N=3, M >= K)
  call DORGBR('Q', M, N, KK, A, MAXN, TAUQ, WORK, LWORK, INFO)
  ! Verify orthogonality: Q^T * Q should be I_3
  QtQ = 0.0d0
  do i = 1, N
    do j = 1, N
      do k = 1, M
        QtQ(i, j) = QtQ(i, j) + A(k, i) * A(k, j)
      end do
    end do
  end do
  call begin_test('vect_q_m_gt_n')
  call print_matrix('Q', A, MAXN, M, N)
  call print_matrix('QtQ', QtQ, MAXN, N, N)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 2: VECT='P', M > N (4x3 original), generate P^T (3x3)
  ! dgebrd on 4x3 => P^T is 3x3, K=3, K >= N => shift path
  ! ---------------------------------------------------------------
  A = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0
  A(3,1) = 3.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0
  A(4,1) = 1.0d0; A(4,2) = 3.0d0; A(4,3) = 1.0d0
  M = 4; N = 3; KK = N
  TAUQ = 0.0d0; TAUP = 0.0d0; WORK = 0.0d0
  call DGEBRD(M, N, A, MAXN, D, E, TAUQ, TAUP, WORK, LWORK, INFO)
  ! Generate P^T: M=N=3, N=3, K=4 (original M)
  ! For VECT='P', K is the number of rows of the original matrix
  ! K >= N => shift path
  call DORGBR('P', N, N, M, A, MAXN, TAUP, WORK, LWORK, INFO)
  ! Verify orthogonality: P^T * (P^T)^T = P^T * P should be I
  QtQ = 0.0d0
  do i = 1, N
    do j = 1, N
      do k = 1, N
        QtQ(i, j) = QtQ(i, j) + A(i, k) * A(j, k)
      end do
    end do
  end do
  call begin_test('vect_p_m_gt_n')
  call print_matrix('PT', A, MAXN, N, N)
  call print_matrix('PTtPT', QtQ, MAXN, N, N)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 3: VECT='Q', M < N (3x4 original), generate Q (3x3)
  ! dgebrd on 3x4 => Q is 3x3, K=4, M < K => shift path
  ! ---------------------------------------------------------------
  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 4.0d0; A(1,3) = 2.0d0; A(1,4) = 1.0d0
  A(2,1) = 3.0d0; A(2,2) = 1.0d0; A(2,3) = 5.0d0; A(2,4) = 2.0d0
  A(3,1) = 2.0d0; A(3,2) = 3.0d0; A(3,3) = 1.0d0; A(3,4) = 4.0d0
  M = 3; N = 4; KK = M
  TAUQ = 0.0d0; TAUP = 0.0d0; WORK = 0.0d0
  call DGEBRD(M, N, A, MAXN, D, E, TAUQ, TAUP, WORK, LWORK, INFO)
  ! Generate Q: M=3, N=3, K=4 (original N)
  ! For VECT='Q', M < K => shift path
  call DORGBR('Q', M, M, N, A, MAXN, TAUQ, WORK, LWORK, INFO)
  ! Verify orthogonality
  QtQ = 0.0d0
  do i = 1, M
    do j = 1, M
      do k = 1, M
        QtQ(i, j) = QtQ(i, j) + A(k, i) * A(k, j)
      end do
    end do
  end do
  call begin_test('vect_q_m_lt_n')
  call print_matrix('Q', A, MAXN, M, M)
  call print_matrix('QtQ', QtQ, MAXN, M, M)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 4: VECT='P', M < N (3x4 original), generate P^T (3x4)
  ! dgebrd on 3x4 => P^T has K=3, K < N => direct path
  ! ---------------------------------------------------------------
  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 4.0d0; A(1,3) = 2.0d0; A(1,4) = 1.0d0
  A(2,1) = 3.0d0; A(2,2) = 1.0d0; A(2,3) = 5.0d0; A(2,4) = 2.0d0
  A(3,1) = 2.0d0; A(3,2) = 3.0d0; A(3,3) = 1.0d0; A(3,4) = 4.0d0
  M = 3; N = 4; KK = 3
  TAUQ = 0.0d0; TAUP = 0.0d0; WORK = 0.0d0
  call DGEBRD(M, N, A, MAXN, D, E, TAUQ, TAUP, WORK, LWORK, INFO)
  ! Generate P^T: M=3, N=4, K=3 (original M), K < N => direct path
  call DORGBR('P', M, N, KK, A, MAXN, TAUP, WORK, LWORK, INFO)
  ! Verify orthogonality: P^T * (P^T)^T should be I_3
  QtQ = 0.0d0
  do i = 1, M
    do j = 1, M
      do k = 1, N
        QtQ(i, j) = QtQ(i, j) + A(i, k) * A(j, k)
      end do
    end do
  end do
  call begin_test('vect_p_m_lt_n')
  call print_matrix('PT', A, MAXN, M, N)
  call print_matrix('PTtPT', QtQ, MAXN, M, M)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 5: VECT='Q', square (4x4), generate Q (4x4)
  ! ---------------------------------------------------------------
  A = 0.0d0
  A(1,1) = 3.0d0; A(1,2) = 1.0d0; A(1,3) = 2.0d0; A(1,4) = 1.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 1.0d0; A(2,4) = 3.0d0
  A(3,1) = 2.0d0; A(3,2) = 1.0d0; A(3,3) = 5.0d0; A(3,4) = 2.0d0
  A(4,1) = 1.0d0; A(4,2) = 3.0d0; A(4,3) = 2.0d0; A(4,4) = 4.0d0
  M = 4; N = 4; KK = 4
  TAUQ = 0.0d0; TAUP = 0.0d0; WORK = 0.0d0
  call DGEBRD(M, N, A, MAXN, D, E, TAUQ, TAUP, WORK, LWORK, INFO)
  call DORGBR('Q', M, N, KK, A, MAXN, TAUQ, WORK, LWORK, INFO)
  QtQ = 0.0d0
  do i = 1, N
    do j = 1, N
      do k = 1, M
        QtQ(i, j) = QtQ(i, j) + A(k, i) * A(k, j)
      end do
    end do
  end do
  call begin_test('vect_q_square')
  call print_matrix('Q', A, MAXN, M, N)
  call print_matrix('QtQ', QtQ, MAXN, N, N)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 6: VECT='P', square (4x4), generate P^T (4x4)
  ! ---------------------------------------------------------------
  A = 0.0d0
  A(1,1) = 3.0d0; A(1,2) = 1.0d0; A(1,3) = 2.0d0; A(1,4) = 1.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 1.0d0; A(2,4) = 3.0d0
  A(3,1) = 2.0d0; A(3,2) = 1.0d0; A(3,3) = 5.0d0; A(3,4) = 2.0d0
  A(4,1) = 1.0d0; A(4,2) = 3.0d0; A(4,3) = 2.0d0; A(4,4) = 4.0d0
  M = 4; N = 4; KK = 4
  TAUQ = 0.0d0; TAUP = 0.0d0; WORK = 0.0d0
  call DGEBRD(M, N, A, MAXN, D, E, TAUQ, TAUP, WORK, LWORK, INFO)
  ! For VECT='P' with square: K >= N => shift path
  call DORGBR('P', N, N, M, A, MAXN, TAUP, WORK, LWORK, INFO)
  QtQ = 0.0d0
  do i = 1, N
    do j = 1, N
      do k = 1, N
        QtQ(i, j) = QtQ(i, j) + A(i, k) * A(j, k)
      end do
    end do
  end do
  call begin_test('vect_p_square')
  call print_matrix('PT', A, MAXN, N, N)
  call print_matrix('PTtPT', QtQ, MAXN, N, N)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 7: M=0 quick return
  ! ---------------------------------------------------------------
  call begin_test('m_zero')
  call DORGBR('Q', 0, 0, 0, A, MAXN, TAUQ, WORK, LWORK, INFO)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 8: VECT='Q', M >= K, 5x4 original, K=4
  ! ---------------------------------------------------------------
  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 1.0d0; A(1,4) = 3.0d0
  A(2,1) = 4.0d0; A(2,2) = 1.0d0; A(2,3) = 3.0d0; A(2,4) = 1.0d0
  A(3,1) = 2.0d0; A(3,2) = 3.0d0; A(3,3) = 2.0d0; A(3,4) = 1.0d0
  A(4,1) = 1.0d0; A(4,2) = 1.0d0; A(4,3) = 4.0d0; A(4,4) = 2.0d0
  A(5,1) = 3.0d0; A(5,2) = 2.0d0; A(5,3) = 1.0d0; A(5,4) = 1.0d0
  M = 5; N = 4; KK = 4
  TAUQ = 0.0d0; TAUP = 0.0d0; WORK = 0.0d0
  call DGEBRD(M, N, A, MAXN, D, E, TAUQ, TAUP, WORK, LWORK, INFO)
  call DORGBR('Q', M, N, KK, A, MAXN, TAUQ, WORK, LWORK, INFO)
  QtQ = 0.0d0
  do i = 1, N
    do j = 1, N
      do k = 1, M
        QtQ(i, j) = QtQ(i, j) + A(k, i) * A(k, j)
      end do
    end do
  end do
  call begin_test('vect_q_5x4')
  call print_matrix('Q', A, MAXN, M, N)
  call print_matrix('QtQ', QtQ, MAXN, N, N)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 9: 1x1 matrix (edge case)
  ! ---------------------------------------------------------------
  A = 0.0d0
  A(1,1) = 5.0d0
  M = 1; N = 1; KK = 1
  TAUQ = 0.0d0; TAUP = 0.0d0; WORK = 0.0d0
  call DGEBRD(M, N, A, MAXN, D, E, TAUQ, TAUP, WORK, LWORK, INFO)
  call DORGBR('Q', M, N, KK, A, MAXN, TAUQ, WORK, LWORK, INFO)
  call begin_test('vect_q_1x1')
  call print_matrix('Q', A, MAXN, M, N)
  call print_int('INFO', INFO)
  call end_test()

  A = 0.0d0
  A(1,1) = 5.0d0
  M = 1; N = 1; KK = 1
  TAUQ = 0.0d0; TAUP = 0.0d0; WORK = 0.0d0
  call DGEBRD(M, N, A, MAXN, D, E, TAUQ, TAUP, WORK, LWORK, INFO)
  call DORGBR('P', N, N, M, A, MAXN, TAUP, WORK, LWORK, INFO)
  call begin_test('vect_p_1x1')
  call print_matrix('PT', A, MAXN, N, N)
  call print_int('INFO', INFO)
  call end_test()

end program
