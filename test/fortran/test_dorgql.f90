program test_dorgql
  use test_utils
  implicit none

  integer, parameter :: MAXM = 40, MAXN = 35, LWORK = 4096
  double precision :: A(MAXM, MAXN), TAU(MAXN), WORK(LWORK)
  double precision :: QtQ(MAXN, MAXN)
  double precision :: Asave(MAXM, MAXN)
  integer :: INFO, i, j, k, M, N, KK

  ! ---------------------------------------------------------------
  ! Test 1: 4x3, K=3 (M > N, full K)
  ! ---------------------------------------------------------------
  A = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0
  A(3,1) = 3.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0
  A(4,1) = 1.0d0; A(4,2) = 3.0d0; A(4,3) = 1.0d0
  TAU = 0.0d0; WORK = 0.0d0
  M = 4; N = 3; KK = 3
  call DGEQLF(M, N, A, MAXM, TAU, WORK, LWORK, INFO)
  ! Save the factored A and TAU for the JS test
  Asave(1:M, 1:N) = A(1:M, 1:N)
  call begin_test('4x3_k3_input')
  call print_matrix('A', Asave, MAXM, M, N)
  call print_array('TAU', TAU, KK)
  call print_int('M', M)
  call print_int('N', N)
  call print_int('K', KK)
  call end_test()

  call DORGQL(M, N, KK, A, MAXM, TAU, WORK, LWORK, INFO)
  call begin_test('4x3_k3')
  call print_matrix('Q', A, MAXM, M, N)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 2: 3x3, K=3 (square)
  ! ---------------------------------------------------------------
  A = 0.0d0
  A(1,1) = 4.0d0; A(1,2) = 1.0d0; A(1,3) = 2.0d0
  A(2,1) = 3.0d0; A(2,2) = 2.0d0; A(2,3) = 1.0d0
  A(3,1) = 1.0d0; A(3,2) = 5.0d0; A(3,3) = 3.0d0
  TAU = 0.0d0; WORK = 0.0d0
  M = 3; N = 3; KK = 3
  call DGEQLF(M, N, A, MAXM, TAU, WORK, LWORK, INFO)
  Asave(1:M, 1:N) = A(1:M, 1:N)
  call begin_test('3x3_k3_input')
  call print_matrix('A', Asave, MAXM, M, N)
  call print_array('TAU', TAU, KK)
  call print_int('M', M)
  call print_int('N', N)
  call print_int('K', KK)
  call end_test()

  call DORGQL(M, N, KK, A, MAXM, TAU, WORK, LWORK, INFO)
  call begin_test('3x3_k3')
  call print_matrix('Q', A, MAXM, M, N)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 3: 4x2, K=2
  ! ---------------------------------------------------------------
  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0
  A(2,1) = 3.0d0; A(2,2) = 4.0d0
  A(3,1) = 5.0d0; A(3,2) = 6.0d0
  A(4,1) = 7.0d0; A(4,2) = 8.0d0
  TAU = 0.0d0; WORK = 0.0d0
  M = 4; N = 2; KK = 2
  call DGEQLF(M, N, A, MAXM, TAU, WORK, LWORK, INFO)
  Asave(1:M, 1:N) = A(1:M, 1:N)
  call begin_test('4x2_k2_input')
  call print_matrix('A', Asave, MAXM, M, N)
  call print_array('TAU', TAU, KK)
  call print_int('M', M)
  call print_int('N', N)
  call print_int('K', KK)
  call end_test()

  call DORGQL(M, N, KK, A, MAXM, TAU, WORK, LWORK, INFO)
  call begin_test('4x2_k2')
  call print_matrix('Q', A, MAXM, M, N)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 4: K=0 (should produce identity-like columns)
  ! ---------------------------------------------------------------
  A = 0.0d0
  A(1,1) = 99.0d0; A(1,2) = 88.0d0
  A(2,1) = 77.0d0; A(2,2) = 66.0d0
  A(3,1) = 55.0d0; A(3,2) = 44.0d0
  TAU = 0.0d0; WORK = 0.0d0
  M = 3; N = 2; KK = 0
  call begin_test('k_zero_input')
  call print_matrix('A', A, MAXM, M, N)
  call print_int('M', M)
  call print_int('N', N)
  call print_int('K', KK)
  call end_test()

  call DORGQL(M, N, KK, A, MAXM, TAU, WORK, LWORK, INFO)
  call begin_test('k_zero')
  call print_matrix('Q', A, MAXM, M, N)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 5: N=0 quick return
  ! ---------------------------------------------------------------
  A = 0.0d0
  TAU = 0.0d0; WORK = 0.0d0
  M = 3; N = 0; KK = 0
  call DORGQL(M, N, KK, A, MAXM, TAU, WORK, LWORK, INFO)
  call begin_test('n_zero')
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 6: M=0, N=0 quick return
  ! ---------------------------------------------------------------
  A = 0.0d0
  TAU = 0.0d0; WORK = 0.0d0
  M = 0; N = 0; KK = 0
  call DORGQL(M, N, KK, A, MAXM, TAU, WORK, LWORK, INFO)
  call begin_test('m_zero')
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 7: 5x3, K=3, verify orthogonality Q^T * Q = I
  ! ---------------------------------------------------------------
  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 1.0d0
  A(2,1) = 4.0d0; A(2,2) = 1.0d0; A(2,3) = 3.0d0
  A(3,1) = 2.0d0; A(3,2) = 3.0d0; A(3,3) = 2.0d0
  A(4,1) = 1.0d0; A(4,2) = 1.0d0; A(4,3) = 4.0d0
  A(5,1) = 3.0d0; A(5,2) = 2.0d0; A(5,3) = 1.0d0
  TAU = 0.0d0; WORK = 0.0d0
  M = 5; N = 3; KK = 3
  call DGEQLF(M, N, A, MAXM, TAU, WORK, LWORK, INFO)
  Asave(1:M, 1:N) = A(1:M, 1:N)
  call begin_test('5x3_k3_input')
  call print_matrix('A', Asave, MAXM, M, N)
  call print_array('TAU', TAU, KK)
  call print_int('M', M)
  call print_int('N', N)
  call print_int('K', KK)
  call end_test()

  call DORGQL(M, N, KK, A, MAXM, TAU, WORK, LWORK, INFO)
  ! Compute Q^T * Q
  QtQ = 0.0d0
  do i = 1, N
    do j = 1, N
      do k = 1, M
        QtQ(i, j) = QtQ(i, j) + A(k, i) * A(k, j)
      end do
    end do
  end do
  call begin_test('5x3_orthogonal')
  call print_matrix('Q', A, MAXM, M, N)
  call print_matrix('QtQ', QtQ, MAXN, N, N)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 8: 6x4, K=4
  ! ---------------------------------------------------------------
  A = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0; A(1,4) = 1.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0; A(2,4) = 3.0d0
  A(3,1) = 3.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0; A(3,4) = 2.0d0
  A(4,1) = 1.0d0; A(4,2) = 3.0d0; A(4,3) = 1.0d0; A(4,4) = 4.0d0
  A(5,1) = 2.0d0; A(5,2) = 1.0d0; A(5,3) = 4.0d0; A(5,4) = 1.0d0
  A(6,1) = 1.0d0; A(6,2) = 2.0d0; A(6,3) = 1.0d0; A(6,4) = 3.0d0
  TAU = 0.0d0; WORK = 0.0d0
  M = 6; N = 4; KK = 4
  call DGEQLF(M, N, A, MAXM, TAU, WORK, LWORK, INFO)
  Asave(1:M, 1:N) = A(1:M, 1:N)
  call begin_test('6x4_k4_input')
  call print_matrix('A', Asave, MAXM, M, N)
  call print_array('TAU', TAU, KK)
  call print_int('M', M)
  call print_int('N', N)
  call print_int('K', KK)
  call end_test()

  call DORGQL(M, N, KK, A, MAXM, TAU, WORK, LWORK, INFO)
  call begin_test('6x4_k4')
  call print_matrix('Q', A, MAXM, M, N)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 9: 40x35, K=35 (exercises blocked path, NB=32)
  ! ---------------------------------------------------------------
  A = 0.0d0
  M = 40; N = 35; KK = 35
  do j = 1, N
    do i = 1, M
      A(i,j) = sin(dble(i*7 + j*13))
    end do
  end do
  TAU = 0.0d0; WORK = 0.0d0
  call DGEQLF(M, N, A, MAXM, TAU, WORK, LWORK, INFO)
  Asave(1:M, 1:N) = A(1:M, 1:N)
  call begin_test('40x35_input')
  call print_matrix('A', Asave, MAXM, M, N)
  call print_array('TAU', TAU, KK)
  call print_int('M', M)
  call print_int('N', N)
  call print_int('K', KK)
  call end_test()

  call DORGQL(M, N, KK, A, MAXM, TAU, WORK, LWORK, INFO)
  ! Compute Q^T * Q to verify orthogonality
  QtQ = 0.0d0
  do i = 1, N
    do j = 1, N
      do k = 1, M
        QtQ(i, j) = QtQ(i, j) + A(k, i) * A(k, j)
      end do
    end do
  end do
  call begin_test('40x35_blocked')
  call print_matrix('Q', A, MAXM, M, N)
  call print_matrix('QtQ', QtQ, MAXN, N, N)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 10: 40x35, K=34 (blocked path with N > K, exercises zero-fill)
  ! ---------------------------------------------------------------
  A = 0.0d0
  M = 40; N = 35; KK = 34
  do j = 1, N
    do i = 1, M
      A(i,j) = cos(dble(i*3 + j*11))
    end do
  end do
  TAU = 0.0d0; WORK = 0.0d0
  call DGEQLF(M, N, A, MAXM, TAU, WORK, LWORK, INFO)
  Asave(1:M, 1:N) = A(1:M, 1:N)
  call begin_test('40x35_k34_input')
  call print_matrix('A', Asave, MAXM, M, N)
  call print_array('TAU', TAU, KK)
  call print_int('M', M)
  call print_int('N', N)
  call print_int('K', KK)
  call end_test()

  call DORGQL(M, N, KK, A, MAXM, TAU, WORK, LWORK, INFO)
  ! Verify orthogonality
  QtQ = 0.0d0
  do i = 1, N
    do j = 1, N
      do k = 1, M
        QtQ(i, j) = QtQ(i, j) + A(k, i) * A(k, j)
      end do
    end do
  end do
  call begin_test('40x35_k34')
  call print_matrix('Q', A, MAXM, M, N)
  call print_matrix('QtQ', QtQ, MAXN, N, N)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 11: 4x3, K=1 (partial K, only 1 reflector)
  ! ---------------------------------------------------------------
  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 3.0d0
  A(2,1) = 4.0d0; A(2,2) = 5.0d0; A(2,3) = 6.0d0
  A(3,1) = 7.0d0; A(3,2) = 8.0d0; A(3,3) = 9.0d0
  A(4,1) = 10.0d0; A(4,2) = 11.0d0; A(4,3) = 12.0d0
  TAU = 0.0d0; WORK = 0.0d0
  M = 4; N = 3; KK = 3
  call DGEQLF(M, N, A, MAXM, TAU, WORK, LWORK, INFO)
  ! Now use only K=1 (just the last reflector)
  KK = 1
  Asave(1:M, 1:N) = A(1:M, 1:N)
  call begin_test('4x3_k1_input')
  call print_matrix('A', Asave, MAXM, M, N)
  call print_array('TAU', TAU, 3)
  call print_int('M', M)
  call print_int('N', N)
  call print_int('K', KK)
  call end_test()

  call DORGQL(M, N, KK, A, MAXM, TAU, WORK, LWORK, INFO)
  call begin_test('4x3_k1')
  call print_matrix('Q', A, MAXM, M, N)
  call print_int('INFO', INFO)
  call end_test()

end program
