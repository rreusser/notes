program test_dorgl2
  use test_utils
  implicit none

  double precision :: A(36), TAU(6), WORK(6)
  double precision :: QQT(36)
  integer :: info, i, j, M, N, LDA

  ! Test 1: 3x4, K=3 (from actual LQ factorization)
  ! LDA=3, column-major
  A = 0.0d0
  A(1)  = 2.0d0; A(4)  = 1.0d0; A(7)  = 3.0d0; A(10) = 1.0d0
  A(2)  = 1.0d0; A(5)  = 4.0d0; A(8)  = 2.0d0; A(11) = 3.0d0
  A(3)  = 3.0d0; A(6)  = 2.0d0; A(9)  = 5.0d0; A(12) = 2.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGELQ2(3, 4, A, 3, TAU, WORK, info)
  call DORGL2(3, 4, 3, A, 3, TAU, WORK, info)
  call begin_test('3x4_k3')
  call print_matrix('A', A, 3, 3, 4)
  call print_int('info', info)
  ! Verify orthogonality: Q * Q^T = I (3x3)
  M = 3; N = 4; LDA = 3
  QQT = 0.0d0
  call DGEMM('N', 'T', M, M, N, 1.0d0, A, LDA, A, LDA, 0.0d0, QQT, M)
  call print_matrix('QQT', QQT, M, M, M)
  call end_test()

  ! Test 2: 3x3 square, K=3
  A = 0.0d0
  A(1) = 4.0d0; A(4) = 1.0d0; A(7) = 2.0d0
  A(2) = 1.0d0; A(5) = 3.0d0; A(8) = 1.0d0
  A(3) = 2.0d0; A(6) = 1.0d0; A(9) = 5.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGELQ2(3, 3, A, 3, TAU, WORK, info)
  call DORGL2(3, 3, 3, A, 3, TAU, WORK, info)
  call begin_test('3x3_k3')
  call print_matrix('A', A, 3, 3, 3)
  call print_int('info', info)
  M = 3; N = 3; LDA = 3
  QQT = 0.0d0
  call DGEMM('N', 'T', M, M, N, 1.0d0, A, LDA, A, LDA, 0.0d0, QQT, M)
  call print_matrix('QQT', QQT, M, M, M)
  call end_test()

  ! Test 3: 2x5, K=1 (K < M, partial generation)
  A = 0.0d0
  A(1)  = 1.0d0; A(3)  = 2.0d0; A(5)  = 3.0d0; A(7) = 4.0d0; A(9) = 5.0d0
  A(2)  = 6.0d0; A(4)  = 7.0d0; A(6)  = 8.0d0; A(8) = 9.0d0; A(10) = 10.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGELQ2(2, 5, A, 2, TAU, WORK, info)
  call DORGL2(2, 5, 1, A, 2, TAU, WORK, info)
  call begin_test('2x5_k1')
  call print_matrix('A', A, 2, 2, 5)
  call print_int('info', info)
  M = 2; N = 5; LDA = 2
  QQT = 0.0d0
  call DGEMM('N', 'T', M, M, N, 1.0d0, A, LDA, A, LDA, 0.0d0, QQT, M)
  call print_matrix('QQT', QQT, M, M, M)
  call end_test()

  ! Test 4: K=0 (should produce identity in first M rows)
  A = 0.0d0
  ! Set up some non-zero data in rows K+1..M (which is rows 1..3 since K=0)
  A(1)  = 9.0d0; A(4)  = 9.0d0; A(7) = 9.0d0
  A(2)  = 9.0d0; A(5)  = 9.0d0; A(8) = 9.0d0
  A(3)  = 9.0d0; A(6)  = 9.0d0; A(9) = 9.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DORGL2(3, 3, 0, A, 3, TAU, WORK, info)
  call begin_test('k0_identity')
  call print_matrix('A', A, 3, 3, 3)
  call print_int('info', info)
  call end_test()

  ! Test 5: M=0 quick return
  info = -99
  call DORGL2(0, 4, 0, A, 3, TAU, WORK, info)
  call begin_test('m0_quick')
  call print_int('info', info)
  call end_test()

  ! Test 6: 1x1, K=1
  A = 0.0d0
  A(1) = 7.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGELQ2(1, 1, A, 1, TAU, WORK, info)
  call DORGL2(1, 1, 1, A, 1, TAU, WORK, info)
  call begin_test('1x1_k1')
  call print_matrix('A', A, 1, 1, 1)
  call print_int('info', info)
  call end_test()

  ! Test 7: 1x4, K=1 (single row)
  A = 0.0d0
  A(1) = 1.0d0; A(2) = 2.0d0; A(3) = 3.0d0; A(4) = 4.0d0
  TAU = 0.0d0; WORK = 0.0d0
  ! LDA=1 for 1xN
  call DGELQ2(1, 4, A, 1, TAU, WORK, info)
  call DORGL2(1, 4, 1, A, 1, TAU, WORK, info)
  call begin_test('1x4_k1')
  call print_array('A', A, 4)
  call print_int('info', info)
  call end_test()

  ! Test 8: 2x5, K=2 (full K = M, from LQ)
  A = 0.0d0
  A(1)  = 1.0d0; A(3)  = 2.0d0; A(5)  = 3.0d0; A(7) = 4.0d0; A(9) = 5.0d0
  A(2)  = 6.0d0; A(4)  = 7.0d0; A(6)  = 8.0d0; A(8) = 9.0d0; A(10) = 10.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGELQ2(2, 5, A, 2, TAU, WORK, info)
  call DORGL2(2, 5, 2, A, 2, TAU, WORK, info)
  call begin_test('2x5_k2')
  call print_matrix('A', A, 2, 2, 5)
  call print_int('info', info)
  M = 2; N = 5; LDA = 2
  QQT = 0.0d0
  call DGEMM('N', 'T', M, M, N, 1.0d0, A, LDA, A, LDA, 0.0d0, QQT, M)
  call print_matrix('QQT', QQT, M, M, M)
  call end_test()

  ! Test 9: 3x4, K=2 (K < M, partial generation)
  A = 0.0d0
  A(1)  = 2.0d0; A(4)  = 1.0d0; A(7)  = 3.0d0; A(10) = 1.0d0
  A(2)  = 1.0d0; A(5)  = 4.0d0; A(8)  = 2.0d0; A(11) = 3.0d0
  A(3)  = 3.0d0; A(6)  = 2.0d0; A(9)  = 5.0d0; A(12) = 2.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGELQ2(3, 4, A, 3, TAU, WORK, info)
  call DORGL2(3, 4, 2, A, 3, TAU, WORK, info)
  call begin_test('3x4_k2')
  call print_matrix('A', A, 3, 3, 4)
  call print_int('info', info)
  M = 3; N = 4; LDA = 3
  QQT = 0.0d0
  call DGEMM('N', 'T', M, M, N, 1.0d0, A, LDA, A, LDA, 0.0d0, QQT, M)
  call print_matrix('QQT', QQT, M, M, M)
  call end_test()

end program
