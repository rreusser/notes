program test_zunglq
  use test_utils
  implicit none

  integer :: info, lwork
  complex*16 :: A(1600), TAU(40), WORK(2000)
  double precision :: A_r(3200), TAU_r(80)
  equivalence (A, A_r)
  equivalence (TAU, TAU_r)
  integer :: i, j, M, N, K

  lwork = 2000

  ! Test 1: 3x3, K=0 (identity)
  M = 3; N = 3; K = 0
  A(1:M*N) = (0.0d0, 0.0d0)
  TAU = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0)
  call zunglq(M, N, K, A, M, TAU, WORK, lwork, info)
  call begin_test('zunglq_identity_k0')
  call print_int('info', info)
  call print_int('M', M)
  call print_int('N', N)
  call print_array('A', A_r, 2*M*N)
  call end_test()

  ! Test 2: 3x3, K=2
  M = 3; N = 3; K = 2
  A(1:M*N) = (0.0d0, 0.0d0)
  ! Row 1: reflector 1 in columns 1..3
  A(1) = (1.0d0, 0.0d0)
  A(4) = (0.4d0, 0.2d0)
  A(7) = (0.1d0, -0.3d0)
  ! Row 2: reflector 2 in columns 2..3
  A(5) = (1.0d0, 0.0d0)
  A(8) = (0.6d0, 0.5d0)
  TAU(1) = (1.1d0, 0.2d0)
  TAU(2) = (0.9d0, -0.1d0)
  WORK = (0.0d0, 0.0d0)
  call zunglq(M, N, K, A, M, TAU, WORK, lwork, info)
  call begin_test('zunglq_3x3_k2')
  call print_int('info', info)
  call print_int('M', M)
  call print_int('N', N)
  call print_array('A', A_r, 2*M*N)
  call end_test()

  ! Test 3: 3x4, K=3 (rectangular, M < N)
  M = 3; N = 4; K = 3
  A(1:M*N) = (0.0d0, 0.0d0)
  A(1) = (1.0d0, 0.0d0)
  A(4) = (0.3d0, 0.1d0)
  A(7) = (0.2d0, -0.2d0)
  A(10) = (0.1d0, 0.05d0)
  A(5) = (1.0d0, 0.0d0)
  A(8) = (0.4d0, 0.3d0)
  A(11) = (-0.1d0, 0.2d0)
  A(9) = (1.0d0, 0.0d0)
  A(12) = (0.5d0, -0.1d0)
  TAU(1) = (1.05d0, 0.1d0)
  TAU(2) = (1.15d0, -0.2d0)
  TAU(3) = (0.8d0, 0.15d0)
  WORK = (0.0d0, 0.0d0)
  call zunglq(M, N, K, A, M, TAU, WORK, lwork, info)
  call begin_test('zunglq_3x4_k3')
  call print_int('info', info)
  call print_int('M', M)
  call print_int('N', N)
  call print_array('A', A_r, 2*M*N)
  call end_test()

  ! Test 4: M=0 quick return
  M = 0; N = 3; K = 0
  call zunglq(M, N, K, A, 1, TAU, WORK, lwork, info)
  call begin_test('zunglq_m0')
  call print_int('info', info)
  call end_test()

  ! Test 5: 1x1, K=1
  M = 1; N = 1; K = 1
  A(1) = (1.0d0, 0.0d0)
  TAU(1) = (0.5d0, 0.5d0)
  WORK = (0.0d0, 0.0d0)
  call zunglq(M, N, K, A, M, TAU, WORK, lwork, info)
  call begin_test('zunglq_1x1_k1')
  call print_int('info', info)
  call print_int('M', M)
  call print_int('N', N)
  call print_array('A', A_r, 2)
  call end_test()

  ! Test 6: From actual LQ factorization, 4x4
  M = 4; N = 4; K = 4
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
  call zgelqf(M, N, A, M, TAU, WORK, lwork, info)
  ! Output intermediate state
  call begin_test('zunglq_from_lq_4x4_input')
  call print_int('info', info)
  call print_int('M', M)
  call print_int('N', N)
  call print_int('K', K)
  call print_array('A', A_r, 2*M*N)
  call print_array('TAU', TAU_r, 2*K)
  call end_test()

  call zunglq(M, N, K, A, M, TAU, WORK, lwork, info)
  call begin_test('zunglq_from_lq_4x4')
  call print_int('info', info)
  call print_int('M', M)
  call print_int('N', N)
  call print_array('A', A_r, 2*M*N)
  call end_test()

  ! Test 7: Larger matrix to trigger blocking (K>32)
  M = 40; N = 40; K = 40
  A(1:M*N) = (0.0d0, 0.0d0)
  do j = 1, N
    do i = 1, M
      A((j-1)*M + i) = dcmplx(dble(i+j)/dble(M+N), dble(i-j)/dble(M+N))
    end do
  end do
  call zgelqf(M, N, A, M, TAU, WORK, lwork, info)
  ! Output intermediate state
  call begin_test('zunglq_blocked_40x40_input')
  call print_int('info', info)
  call print_int('M', M)
  call print_int('N', N)
  call print_int('K', K)
  call print_array('A', A_r, 2*M*N)
  call print_array('TAU', TAU_r, 2*K)
  call end_test()

  call zunglq(M, N, K, A, M, TAU, WORK, lwork, info)
  call begin_test('zunglq_blocked_40x40')
  call print_int('info', info)
  call print_int('M', M)
  call print_int('N', N)
  call print_array('A', A_r, 2*M*N)
  call end_test()

  ! Test 8: 5x8, K=5 (rectangular, M < N, from LQ)
  M = 5; N = 8; K = 5
  A(1:M*N) = (0.0d0, 0.0d0)
  do j = 1, N
    do i = 1, M
      A((j-1)*M + i) = dcmplx(dble(i*j)/10.0d0, dble(i-j)/5.0d0)
    end do
  end do
  call zgelqf(M, N, A, M, TAU, WORK, lwork, info)
  ! Output intermediate state
  call begin_test('zunglq_5x8_k5_input')
  call print_int('info', info)
  call print_int('M', M)
  call print_int('N', N)
  call print_int('K', K)
  call print_array('A', A_r, 2*M*N)
  call print_array('TAU', TAU_r, 2*K)
  call end_test()

  call zunglq(M, N, K, A, M, TAU, WORK, lwork, info)
  call begin_test('zunglq_5x8_k5')
  call print_int('info', info)
  call print_int('M', M)
  call print_int('N', N)
  call print_array('A', A_r, 2*M*N)
  call end_test()

end program
