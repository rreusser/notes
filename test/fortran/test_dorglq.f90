program test_dorglq
  use test_utils
  implicit none

  double precision :: A(1600), TAU(40), WORK(2000)
  double precision :: QQT(1600)
  integer :: info, lwork, i, j, M, N, K, LDA

  lwork = 2000

  ! Test 1: 3x4, K=3 (from actual LQ factorization)
  M = 3; N = 4; K = 3; LDA = 3
  A = 0.0d0
  A(1)  = 2.0d0; A(4)  = 1.0d0; A(7)  = 3.0d0; A(10) = 1.0d0
  A(2)  = 1.0d0; A(5)  = 4.0d0; A(8)  = 2.0d0; A(11) = 3.0d0
  A(3)  = 3.0d0; A(6)  = 2.0d0; A(9)  = 5.0d0; A(12) = 2.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGELQF(M, N, A, LDA, TAU, WORK, lwork, info)
  call DORGLQ(M, N, K, A, LDA, TAU, WORK, lwork, info)
  call begin_test('3x4_k3')
  call print_matrix('A', A, LDA, M, N)
  call print_int('info', info)
  QQT = 0.0d0
  call DGEMM('N', 'T', M, M, N, 1.0d0, A, LDA, A, LDA, 0.0d0, QQT, M)
  call print_matrix('QQT', QQT, M, M, M)
  call end_test()

  ! Test 2: 3x3 square, K=3
  M = 3; N = 3; K = 3; LDA = 3
  A = 0.0d0
  A(1) = 4.0d0; A(4) = 1.0d0; A(7) = 2.0d0
  A(2) = 1.0d0; A(5) = 3.0d0; A(8) = 1.0d0
  A(3) = 2.0d0; A(6) = 1.0d0; A(9) = 5.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGELQF(M, N, A, LDA, TAU, WORK, lwork, info)
  call DORGLQ(M, N, K, A, LDA, TAU, WORK, lwork, info)
  call begin_test('3x3_k3')
  call print_matrix('A', A, LDA, M, N)
  call print_int('info', info)
  QQT = 0.0d0
  call DGEMM('N', 'T', M, M, N, 1.0d0, A, LDA, A, LDA, 0.0d0, QQT, M)
  call print_matrix('QQT', QQT, M, M, M)
  call end_test()

  ! Test 3: 2x5, K=2 (rectangular, M < N)
  M = 2; N = 5; K = 2; LDA = 2
  A = 0.0d0
  A(1) = 1.0d0; A(3)  = 2.0d0; A(5)  = 3.0d0; A(7) = 4.0d0; A(9) = 5.0d0
  A(2) = 6.0d0; A(4)  = 7.0d0; A(6)  = 8.0d0; A(8) = 9.0d0; A(10) = 10.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGELQF(M, N, A, LDA, TAU, WORK, lwork, info)
  call DORGLQ(M, N, K, A, LDA, TAU, WORK, lwork, info)
  call begin_test('2x5_k2')
  call print_matrix('A', A, LDA, M, N)
  call print_int('info', info)
  QQT = 0.0d0
  call DGEMM('N', 'T', M, M, N, 1.0d0, A, LDA, A, LDA, 0.0d0, QQT, M)
  call print_matrix('QQT', QQT, M, M, M)
  call end_test()

  ! Test 4: K=0 (should produce identity)
  M = 3; N = 3; K = 0; LDA = 3
  A = 0.0d0
  A(1) = 9.0d0; A(4) = 9.0d0; A(7) = 9.0d0
  A(2) = 9.0d0; A(5) = 9.0d0; A(8) = 9.0d0
  A(3) = 9.0d0; A(6) = 9.0d0; A(9) = 9.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DORGLQ(M, N, K, A, LDA, TAU, WORK, lwork, info)
  call begin_test('k0_identity')
  call print_matrix('A', A, LDA, M, N)
  call print_int('info', info)
  call end_test()

  ! Test 5: M=0 quick return
  M = 0; N = 4; K = 0
  info = -99
  call DORGLQ(M, N, K, A, 1, TAU, WORK, lwork, info)
  call begin_test('m0_quick')
  call print_int('info', info)
  call end_test()

  ! Test 6: 1x1, K=1
  M = 1; N = 1; K = 1; LDA = 1
  A = 0.0d0
  A(1) = 7.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGELQF(M, N, A, LDA, TAU, WORK, lwork, info)
  call DORGLQ(M, N, K, A, LDA, TAU, WORK, lwork, info)
  call begin_test('1x1_k1')
  call print_matrix('A', A, LDA, M, N)
  call print_int('info', info)
  call end_test()

  ! Test 7: 3x4, K=2 (partial, K < M)
  M = 3; N = 4; K = 2; LDA = 3
  A = 0.0d0
  A(1)  = 2.0d0; A(4)  = 1.0d0; A(7)  = 3.0d0; A(10) = 1.0d0
  A(2)  = 1.0d0; A(5)  = 4.0d0; A(8)  = 2.0d0; A(11) = 3.0d0
  A(3)  = 3.0d0; A(6)  = 2.0d0; A(9)  = 5.0d0; A(12) = 2.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGELQF(M, N, A, LDA, TAU, WORK, lwork, info)
  call DORGLQ(M, N, K, A, LDA, TAU, WORK, lwork, info)
  call begin_test('3x4_k2')
  call print_matrix('A', A, LDA, M, N)
  call print_int('info', info)
  QQT = 0.0d0
  call DGEMM('N', 'T', M, M, N, 1.0d0, A, LDA, A, LDA, 0.0d0, QQT, M)
  call print_matrix('QQT', QQT, M, M, M)
  call end_test()

  ! Test 8: Large matrix 35x40, K=35 (exercises blocked path, NB=32)
  M = 35; N = 40; K = 35; LDA = 35
  A = 0.0d0
  do j = 1, N
    do i = 1, M
      A((j-1)*LDA + i) = dble(i+j)/dble(M+N) + 0.1d0*dble(mod(i*j,7))
    end do
  end do
  TAU = 0.0d0; WORK = 0.0d0
  call DGELQF(M, N, A, LDA, TAU, WORK, lwork, info)
  call DORGLQ(M, N, K, A, LDA, TAU, WORK, lwork, info)
  call begin_test('35x40_k35_blocked')
  call print_matrix('A', A, LDA, M, N)
  call print_int('info', info)
  QQT = 0.0d0
  call DGEMM('N', 'T', M, M, N, 1.0d0, A, LDA, A, LDA, 0.0d0, QQT, M)
  call print_matrix('QQT', QQT, M, M, M)
  call end_test()

  ! Test 9: 1x4, K=1 (single row, from LQ)
  M = 1; N = 4; K = 1; LDA = 1
  A = 0.0d0
  A(1) = 1.0d0; A(2) = 2.0d0; A(3) = 3.0d0; A(4) = 4.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGELQF(M, N, A, LDA, TAU, WORK, lwork, info)
  call DORGLQ(M, N, K, A, LDA, TAU, WORK, lwork, info)
  call begin_test('1x4_k1')
  call print_array('A', A, N)
  call print_int('info', info)
  call end_test()

  ! Test 10: N=0 quick return
  M = 0; N = 0; K = 0
  info = -99
  call DORGLQ(M, N, K, A, 1, TAU, WORK, lwork, info)
  call begin_test('n0_quick')
  call print_int('info', info)
  call end_test()

end program
