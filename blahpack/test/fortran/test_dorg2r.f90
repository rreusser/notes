program test_dorg2r
  use test_utils
  implicit none

  double precision :: A(6, 4), Q(6, 4), TAU(4), WORK(4)
  double precision :: QtQ(4, 4)
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
  call DGEQR2(M, N, A, 6, TAU, WORK, INFO)
  call DORG2R(M, N, KK, A, 6, TAU, WORK, INFO)
  call begin_test('4x3_k3')
  call print_matrix('Q', A, 6, M, N)
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
  call DGEQR2(M, N, A, 6, TAU, WORK, INFO)
  call DORG2R(M, N, KK, A, 6, TAU, WORK, INFO)
  call begin_test('3x3_k3')
  call print_matrix('Q', A, 6, M, N)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 3: 4x2, K=1 (K < N, partial reflectors)
  ! ---------------------------------------------------------------
  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0
  A(2,1) = 3.0d0; A(2,2) = 4.0d0
  A(3,1) = 5.0d0; A(3,2) = 6.0d0
  A(4,1) = 7.0d0; A(4,2) = 8.0d0
  TAU = 0.0d0; WORK = 0.0d0
  M = 4; N = 2; KK = 1
  call DGEQR2(M, N, A, 6, TAU, WORK, INFO)
  call DORG2R(M, N, KK, A, 6, TAU, WORK, INFO)
  call begin_test('4x2_k1')
  call print_matrix('Q', A, 6, M, N)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 4: K=0 (should produce identity in first N columns)
  ! ---------------------------------------------------------------
  A = 0.0d0
  A(1,1) = 99.0d0; A(1,2) = 88.0d0
  A(2,1) = 77.0d0; A(2,2) = 66.0d0
  A(3,1) = 55.0d0; A(3,2) = 44.0d0
  TAU = 0.0d0; WORK = 0.0d0
  M = 3; N = 2; KK = 0
  call DORG2R(M, N, KK, A, 6, TAU, WORK, INFO)
  call begin_test('k_zero')
  call print_matrix('Q', A, 6, M, N)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 5: N=0 quick return
  ! ---------------------------------------------------------------
  A = 0.0d0
  TAU = 0.0d0; WORK = 0.0d0
  M = 3; N = 0; KK = 0
  call DORG2R(M, N, KK, A, 6, TAU, WORK, INFO)
  call begin_test('n_zero')
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 6: M=0, N=0 quick return
  ! ---------------------------------------------------------------
  A = 0.0d0
  TAU = 0.0d0; WORK = 0.0d0
  M = 0; N = 0; KK = 0
  call DORG2R(M, N, KK, A, 6, TAU, WORK, INFO)
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
  call DGEQR2(M, N, A, 6, TAU, WORK, INFO)
  call DORG2R(M, N, KK, A, 6, TAU, WORK, INFO)
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
  call print_matrix('Q', A, 6, M, N)
  call print_matrix('QtQ', QtQ, 4, N, N)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 8: 6x4, K=4 (larger matrix)
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
  call DGEQR2(M, N, A, 6, TAU, WORK, INFO)
  call DORG2R(M, N, KK, A, 6, TAU, WORK, INFO)
  call begin_test('6x4_k4')
  call print_matrix('Q', A, 6, M, N)
  call print_int('INFO', INFO)
  call end_test()

end program
