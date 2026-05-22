program test_zungrq
  use test_utils
  implicit none

  integer :: info, lwork
  complex*16 :: A(1600), TAU(40), WORK(2000)
  double precision :: A_r(3200), TAU_r(80)
  equivalence (A, A_r)
  equivalence (TAU, TAU_r)
  integer :: i, j, M, N, K, LDA

  lwork = 2000

  ! Test 1: 3x4, K=3 (M < N, full K=M from RQ)
  M = 3; N = 4; K = 3; LDA = 3
  A(1:M*N) = (0.0d0, 0.0d0)
  A(1)  = ( 2.0d0,  1.0d0)
  A(4)  = ( 1.0d0, -0.5d0)
  A(7)  = ( 3.0d0,  0.7d0)
  A(10) = ( 1.0d0,  0.3d0)
  A(2)  = ( 1.0d0,  0.2d0)
  A(5)  = ( 4.0d0, -1.0d0)
  A(8)  = ( 2.0d0,  0.4d0)
  A(11) = ( 3.0d0, -0.2d0)
  A(3)  = ( 3.0d0,  0.8d0)
  A(6)  = ( 2.0d0,  0.1d0)
  A(9)  = ( 5.0d0,  0.6d0)
  A(12) = ( 2.0d0, -0.3d0)
  TAU = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  call ZGERQF(M, N, A, LDA, TAU, WORK, lwork, info)
  call ZUNGRQ(M, N, K, A, LDA, TAU, WORK, lwork, info)
  call begin_test('zungrq_3x4_k3')
  call print_int('info', info)
  call print_int('M', M)
  call print_int('N', N)
  call print_array('A', A_r, 2*M*N)
  call end_test()

  ! Test 2: 3x3 square, K=3
  M = 3; N = 3; K = 3; LDA = 3
  A(1:M*N) = (0.0d0, 0.0d0)
  A(1) = ( 4.0d0,  0.5d0); A(4) = ( 1.0d0, -0.2d0); A(7) = ( 2.0d0,  0.3d0)
  A(2) = ( 1.0d0,  0.1d0); A(5) = ( 3.0d0,  0.0d0); A(8) = ( 1.0d0,  0.4d0)
  A(3) = ( 2.0d0, -0.6d0); A(6) = ( 1.0d0,  0.8d0); A(9) = ( 5.0d0, -0.1d0)
  TAU = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  call ZGERQF(M, N, A, LDA, TAU, WORK, lwork, info)
  call ZUNGRQ(M, N, K, A, LDA, TAU, WORK, lwork, info)
  call begin_test('zungrq_3x3_k3')
  call print_int('info', info)
  call print_int('M', M)
  call print_int('N', N)
  call print_array('A', A_r, 2*M*N)
  call end_test()

  ! Test 3: 2x5, K=2 (rectangular, M < N)
  M = 2; N = 5; K = 2; LDA = 2
  A(1:M*N) = (0.0d0, 0.0d0)
  A(1) = ( 1.0d0,  0.5d0); A(3) = ( 2.0d0, -0.3d0); A(5) = ( 3.0d0,  0.2d0)
  A(7) = ( 4.0d0,  0.6d0); A(9) = ( 5.0d0, -0.1d0)
  A(2) = ( 6.0d0,  0.4d0); A(4) = ( 7.0d0,  0.1d0); A(6) = ( 8.0d0, -0.5d0)
  A(8) = ( 9.0d0,  0.3d0); A(10) = (10.0d0, -0.2d0)
  TAU = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  call ZGERQF(M, N, A, LDA, TAU, WORK, lwork, info)
  call ZUNGRQ(M, N, K, A, LDA, TAU, WORK, lwork, info)
  call begin_test('zungrq_2x5_k2')
  call print_int('info', info)
  call print_int('M', M)
  call print_int('N', N)
  call print_array('A', A_r, 2*M*N)
  call end_test()

  ! Test 4: K=0 (should produce identity in last M rows)
  M = 3; N = 3; K = 0; LDA = 3
  A(1:M*N) = (0.0d0, 0.0d0)
  A(1) = (9.0d0, 1.0d0); A(4) = (9.0d0, 2.0d0); A(7) = (9.0d0, 3.0d0)
  A(2) = (9.0d0, 4.0d0); A(5) = (9.0d0, 5.0d0); A(8) = (9.0d0, 6.0d0)
  A(3) = (9.0d0, 7.0d0); A(6) = (9.0d0, 8.0d0); A(9) = (9.0d0, 9.0d0)
  TAU = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  call ZUNGRQ(M, N, K, A, LDA, TAU, WORK, lwork, info)
  call begin_test('zungrq_k0_identity')
  call print_int('info', info)
  call print_int('M', M)
  call print_int('N', N)
  call print_array('A', A_r, 2*M*N)
  call end_test()

  ! Test 5: M=0 quick return
  M = 0; N = 4; K = 0
  info = -99
  call ZUNGRQ(M, N, K, A, 1, TAU, WORK, lwork, info)
  call begin_test('zungrq_m0_quick')
  call print_int('info', info)
  call end_test()

  ! Test 6: 1x1, K=1
  M = 1; N = 1; K = 1; LDA = 1
  A(1:M*N) = (0.0d0, 0.0d0)
  A(1) = (7.0d0, -2.0d0)
  TAU = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  call ZGERQF(M, N, A, LDA, TAU, WORK, lwork, info)
  call ZUNGRQ(M, N, K, A, LDA, TAU, WORK, lwork, info)
  call begin_test('zungrq_1x1_k1')
  call print_int('info', info)
  call print_int('M', M)
  call print_int('N', N)
  call print_array('A', A_r, 2*M*N)
  call end_test()

  ! Test 7: 1x4, K=1 (single row, from RQ)
  M = 1; N = 4; K = 1; LDA = 1
  A(1:M*N) = (0.0d0, 0.0d0)
  A(1) = (1.0d0, 0.5d0); A(2) = (2.0d0, -0.3d0); A(3) = (3.0d0, 0.2d0); A(4) = (4.0d0, 0.6d0)
  TAU = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  call ZGERQF(M, N, A, LDA, TAU, WORK, lwork, info)
  call ZUNGRQ(M, N, K, A, LDA, TAU, WORK, lwork, info)
  call begin_test('zungrq_1x4_k1')
  call print_int('info', info)
  call print_int('M', M)
  call print_int('N', N)
  call print_array('A', A_r, 2*M*N)
  call end_test()

  ! Test 8: Large matrix 35x40, K=35 (exercises blocked path, NB=32)
  M = 35; N = 40; K = 35; LDA = 35
  A(1:M*N) = (0.0d0, 0.0d0)
  do j = 1, N
    do i = 1, M
      A((j-1)*LDA + i) = dcmplx(dble(i+j)/dble(M+N) + 0.1d0*dble(mod(i*j,7)), &
                                dble(i-j)/dble(M+N))
    end do
  end do
  TAU = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  call ZGERQF(M, N, A, LDA, TAU, WORK, lwork, info)
  call ZUNGRQ(M, N, K, A, LDA, TAU, WORK, lwork, info)
  call begin_test('zungrq_35x40_k35_blocked')
  call print_int('info', info)
  call print_int('M', M)
  call print_int('N', N)
  call print_array('A', A_r, 2*M*N)
  call end_test()

  ! Test 9: N=0 quick return
  M = 0; N = 0; K = 0
  info = -99
  call ZUNGRQ(M, N, K, A, 1, TAU, WORK, lwork, info)
  call begin_test('zungrq_n0_quick')
  call print_int('info', info)
  call end_test()

  ! Test 10: From actual RQ factorization, 4x4 (full K=M=N)
  M = 4; N = 4; K = 4; LDA = 4
  A(1:M*N) = (0.0d0, 0.0d0)
  A(1)  = (1.0d0, 2.0d0)
  A(2)  = (3.0d0, 4.0d0)
  A(3)  = (5.0d0, 6.0d0)
  A(4)  = (7.0d0, 8.0d0)
  A(5)  = (2.0d0, -1.0d0)
  A(6)  = (1.0d0, 3.0d0)
  A(7)  = (4.0d0, -2.0d0)
  A(8)  = (0.0d0, 1.0d0)
  A(9)  = (1.0d0, 1.0d0)
  A(10) = (-1.0d0, 2.0d0)
  A(11) = (3.0d0, 0.0d0)
  A(12) = (2.0d0, -3.0d0)
  A(13) = (0.0d0, 3.0d0)
  A(14) = (2.0d0, -1.0d0)
  A(15) = (-1.0d0, 4.0d0)
  A(16) = (1.0d0, 1.0d0)
  call ZGERQF(M, N, A, LDA, TAU, WORK, lwork, info)
  call begin_test('zungrq_from_rq_4x4_input')
  call print_int('info', info)
  call print_int('M', M)
  call print_int('N', N)
  call print_int('K', K)
  call print_array('A', A_r, 2*M*N)
  call print_array('TAU', TAU_r, 2*K)
  call end_test()

  call ZUNGRQ(M, N, K, A, LDA, TAU, WORK, lwork, info)
  call begin_test('zungrq_from_rq_4x4')
  call print_int('info', info)
  call print_int('M', M)
  call print_int('N', N)
  call print_array('A', A_r, 2*M*N)
  call end_test()

  ! Test 11: 5x8, K=5 (rectangular, M < N, from RQ)
  M = 5; N = 8; K = 5; LDA = 5
  A(1:M*N) = (0.0d0, 0.0d0)
  do j = 1, N
    do i = 1, M
      A((j-1)*LDA + i) = dcmplx(dble(i*j)/10.0d0, dble(i-j)/5.0d0)
    end do
  end do
  call ZGERQF(M, N, A, LDA, TAU, WORK, lwork, info)
  call begin_test('zungrq_5x8_k5_input')
  call print_int('info', info)
  call print_int('M', M)
  call print_int('N', N)
  call print_int('K', K)
  call print_array('A', A_r, 2*M*N)
  call print_array('TAU', TAU_r, 2*K)
  call end_test()

  call ZUNGRQ(M, N, K, A, LDA, TAU, WORK, lwork, info)
  call begin_test('zungrq_5x8_k5')
  call print_int('info', info)
  call print_int('M', M)
  call print_int('N', N)
  call print_array('A', A_r, 2*M*N)
  call end_test()

  ! Test 12: 40x40, K=40 (large, blocked, K=M=N)
  M = 40; N = 40; K = 40; LDA = 40
  A(1:M*N) = (0.0d0, 0.0d0)
  do j = 1, N
    do i = 1, M
      A((j-1)*LDA + i) = dcmplx(dble(i+j)/dble(M+N), dble(i-j)/dble(M+N))
    end do
  end do
  call ZGERQF(M, N, A, LDA, TAU, WORK, lwork, info)
  call begin_test('zungrq_blocked_40x40_input')
  call print_int('info', info)
  call print_int('M', M)
  call print_int('N', N)
  call print_int('K', K)
  call print_array('A', A_r, 2*M*N)
  call print_array('TAU', TAU_r, 2*K)
  call end_test()

  call ZUNGRQ(M, N, K, A, LDA, TAU, WORK, lwork, info)
  call begin_test('zungrq_blocked_40x40')
  call print_int('info', info)
  call print_int('M', M)
  call print_int('N', N)
  call print_array('A', A_r, 2*M*N)
  call end_test()

end program
