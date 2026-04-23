program test_dorgr2
  use test_utils
  implicit none

  double precision :: A(6, 8), TAU(6), WORK(6)
  double precision :: QQt(6, 6)
  integer :: INFO, i, j, k, M, N, KK

  ! ---------------------------------------------------------------
  ! Test 1: 3x5, K=3 (M < N, full K)
  ! ---------------------------------------------------------------
  A = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0; A(1,4) = 1.0d0; A(1,5) = 4.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0; A(2,4) = 3.0d0; A(2,5) = 2.0d0
  A(3,1) = 3.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0; A(3,4) = 2.0d0; A(3,5) = 1.0d0
  TAU = 0.0d0; WORK = 0.0d0
  M = 3; N = 5; KK = 3
  call DGERQF(M, N, A, 6, TAU, WORK, 6, INFO)
  call DORGR2(M, N, KK, A, 6, TAU, WORK, INFO)
  call begin_test('3x5_k3')
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
  call DGERQF(M, N, A, 6, TAU, WORK, 6, INFO)
  call DORGR2(M, N, KK, A, 6, TAU, WORK, INFO)
  call begin_test('3x3_k3')
  call print_matrix('Q', A, 6, M, N)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 3: 2x5, K=1 (K < M, partial reflectors)
  ! ---------------------------------------------------------------
  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 3.0d0; A(1,4) = 4.0d0; A(1,5) = 5.0d0
  A(2,1) = 6.0d0; A(2,2) = 7.0d0; A(2,3) = 8.0d0; A(2,4) = 9.0d0; A(2,5) = 10.0d0
  TAU = 0.0d0; WORK = 0.0d0
  M = 2; N = 5; KK = 1
  call DGERQF(M, N, A, 6, TAU, WORK, 6, INFO)
  call DORGR2(M, N, KK, A, 6, TAU, WORK, INFO)
  call begin_test('2x5_k1')
  call print_matrix('Q', A, 6, M, N)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 4: K=0 (should produce identity-like rows)
  ! ---------------------------------------------------------------
  A = 0.0d0
  A(1,1) = 99.0d0; A(1,2) = 88.0d0; A(1,3) = 77.0d0
  A(2,1) = 66.0d0; A(2,2) = 55.0d0; A(2,3) = 44.0d0
  TAU = 0.0d0; WORK = 0.0d0
  M = 2; N = 3; KK = 0
  call DORGR2(M, N, KK, A, 6, TAU, WORK, INFO)
  call begin_test('k_zero')
  call print_matrix('Q', A, 6, M, N)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 5: M=0 quick return
  ! ---------------------------------------------------------------
  A = 0.0d0
  TAU = 0.0d0; WORK = 0.0d0
  M = 0; N = 0; KK = 0
  call DORGR2(M, N, KK, A, 6, TAU, WORK, INFO)
  call begin_test('m_zero')
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 6: 3x6, K=3, verify orthogonality Q * Q^T = I
  ! ---------------------------------------------------------------
  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 1.0d0; A(1,4) = 3.0d0; A(1,5) = 2.0d0; A(1,6) = 1.0d0
  A(2,1) = 4.0d0; A(2,2) = 1.0d0; A(2,3) = 3.0d0; A(2,4) = 2.0d0; A(2,5) = 1.0d0; A(2,6) = 4.0d0
  A(3,1) = 2.0d0; A(3,2) = 3.0d0; A(3,3) = 2.0d0; A(3,4) = 1.0d0; A(3,5) = 4.0d0; A(3,6) = 2.0d0
  TAU = 0.0d0; WORK = 0.0d0
  M = 3; N = 6; KK = 3
  call DGERQF(M, N, A, 6, TAU, WORK, 6, INFO)
  call DORGR2(M, N, KK, A, 6, TAU, WORK, INFO)
  ! Compute Q * Q^T (rows are orthonormal, so Q*Q^T = I_M)
  QQt = 0.0d0
  do i = 1, M
    do j = 1, M
      do k = 1, N
        QQt(i, j) = QQt(i, j) + A(i, k) * A(j, k)
      end do
    end do
  end do
  call begin_test('3x6_orthogonal')
  call print_matrix('Q', A, 6, M, N)
  call print_matrix('QQt', QQt, 6, M, M)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 7: 4x6, K=4 (larger matrix)
  ! ---------------------------------------------------------------
  A = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0; A(1,4) = 1.0d0; A(1,5) = 2.0d0; A(1,6) = 1.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0; A(2,4) = 3.0d0; A(2,5) = 1.0d0; A(2,6) = 2.0d0
  A(3,1) = 3.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0; A(3,4) = 2.0d0; A(3,5) = 4.0d0; A(3,6) = 1.0d0
  A(4,1) = 1.0d0; A(4,2) = 3.0d0; A(4,3) = 1.0d0; A(4,4) = 4.0d0; A(4,5) = 2.0d0; A(4,6) = 3.0d0
  TAU = 0.0d0; WORK = 0.0d0
  M = 4; N = 6; KK = 4
  call DGERQF(M, N, A, 6, TAU, WORK, 6, INFO)
  call DORGR2(M, N, KK, A, 6, TAU, WORK, INFO)
  call begin_test('4x6_k4')
  call print_matrix('Q', A, 6, M, N)
  call print_int('INFO', INFO)
  call end_test()

end program
