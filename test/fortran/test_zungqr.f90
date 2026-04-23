program test_zungqr
  use test_utils
  implicit none

  integer :: info, lwork
  ! Large enough for blocked tests: max 40x40
  complex*16 :: A(1600), TAU(40), WORK(2000)
  double precision :: A_r(3200), TAU_r(80)
  equivalence (A, A_r)
  equivalence (TAU, TAU_r)
  integer :: i, j, M, N, K

  ! Test 1: 3x3, K=0 (identity, unblocked)
  M = 3; N = 3; K = 0
  A(1:M*N) = (0.0d0, 0.0d0)
  TAU = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0)
  lwork = 2000
  call zungqr(M, N, K, A, M, TAU, WORK, lwork, info)
  call begin_test('zungqr_identity_k0')
  call print_int('info', info)
  call print_int('M', M)
  call print_int('N', N)
  call print_array('A', A_r, 2*M*N)
  call end_test()

  ! Test 2: 3x3, K=2, unblocked
  M = 3; N = 3; K = 2
  A(1:M*N) = (0.0d0, 0.0d0)
  A(1) = (1.0d0, 0.0d0)
  A(2) = (0.4d0, 0.2d0)
  A(3) = (0.1d0, -0.3d0)
  A(5) = (1.0d0, 0.0d0)
  A(6) = (0.6d0, 0.5d0)
  TAU(1) = (1.1d0, 0.2d0)
  TAU(2) = (0.9d0, -0.1d0)
  WORK = (0.0d0, 0.0d0)
  call zungqr(M, N, K, A, M, TAU, WORK, lwork, info)
  call begin_test('zungqr_3x3_k2')
  call print_int('info', info)
  call print_int('M', M)
  call print_int('N', N)
  call print_array('A', A_r, 2*M*N)
  call end_test()

  ! Test 3: 4x3, K=3 (rectangular, unblocked)
  M = 4; N = 3; K = 3
  A(1:M*N) = (0.0d0, 0.0d0)
  A(1) = (1.0d0, 0.0d0)
  A(2) = (0.3d0, 0.1d0)
  A(3) = (0.2d0, -0.2d0)
  A(4) = (0.1d0, 0.05d0)
  A(6) = (1.0d0, 0.0d0)
  A(7) = (0.4d0, 0.3d0)
  A(8) = (-0.1d0, 0.2d0)
  A(11) = (1.0d0, 0.0d0)
  A(12) = (0.5d0, -0.1d0)
  TAU(1) = (1.05d0, 0.1d0)
  TAU(2) = (1.15d0, -0.2d0)
  TAU(3) = (0.8d0, 0.15d0)
  WORK = (0.0d0, 0.0d0)
  call zungqr(M, N, K, A, M, TAU, WORK, lwork, info)
  call begin_test('zungqr_4x3_k3')
  call print_int('info', info)
  call print_int('M', M)
  call print_int('N', N)
  call print_array('A', A_r, 2*M*N)
  call end_test()

  ! Test 4: N=0 quick return
  M = 3; N = 0; K = 0
  call zungqr(M, N, K, A, M, TAU, WORK, lwork, info)
  call begin_test('zungqr_n0')
  call print_int('info', info)
  call end_test()

  ! Test 5: 1x1, K=1
  M = 1; N = 1; K = 1
  A(1) = (1.0d0, 0.0d0)
  TAU(1) = (0.5d0, 0.5d0)
  WORK = (0.0d0, 0.0d0)
  call zungqr(M, N, K, A, M, TAU, WORK, lwork, info)
  call begin_test('zungqr_1x1_k1')
  call print_int('info', info)
  call print_int('M', M)
  call print_int('N', N)
  call print_array('A', A_r, 2)
  call end_test()

  ! Test 6: From actual QR factorization, 4x4
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
  call zgeqrf(M, N, A, M, TAU, WORK, lwork, info)
  ! Output the intermediate QR state for JS testing
  call begin_test('zungqr_from_qr_4x4_input')
  call print_int('info', info)
  call print_int('M', M)
  call print_int('N', N)
  call print_int('K', K)
  call print_array('A', A_r, 2*M*N)
  call print_array('TAU', TAU_r, 2*K)
  call end_test()

  call zungqr(M, N, K, A, M, TAU, WORK, lwork, info)
  call begin_test('zungqr_from_qr_4x4')
  call print_int('info', info)
  call print_int('M', M)
  call print_int('N', N)
  call print_array('A', A_r, 2*M*N)
  call end_test()

  ! Test 7: Larger matrix to trigger blocking (NB=32 typical, so K>32)
  ! 40x40, K=40 - this should use the blocked path
  ! First output the QR factored form, then the Q matrix
  M = 40; N = 40; K = 40
  A(1:M*N) = (0.0d0, 0.0d0)
  do j = 1, N
    do i = 1, M
      A((j-1)*M + i) = dcmplx(dble(i+j)/dble(M+N), dble(i-j)/dble(M+N))
    end do
  end do
  call zgeqrf(M, N, A, M, TAU, WORK, lwork, info)
  ! Output the intermediate QR state for JS testing
  call begin_test('zungqr_blocked_40x40_input')
  call print_int('info', info)
  call print_int('M', M)
  call print_int('N', N)
  call print_int('K', K)
  call print_array('A', A_r, 2*M*N)
  call print_array('TAU', TAU_r, 2*K)
  call end_test()

  call zungqr(M, N, K, A, M, TAU, WORK, lwork, info)
  call begin_test('zungqr_blocked_40x40')
  call print_int('info', info)
  call print_int('M', M)
  call print_int('N', N)
  call print_array('A', A_r, 2*M*N)
  call end_test()

  ! Test 8: M=8, N=5, K=5 (rectangular, unblocked, partial Q)
  M = 8; N = 5; K = 5
  A(1:M*N) = (0.0d0, 0.0d0)
  do j = 1, N
    do i = 1, M
      A((j-1)*M + i) = dcmplx(dble(i*j)/10.0d0, dble(i-j)/5.0d0)
    end do
  end do
  call zgeqrf(M, N, A, M, TAU, WORK, lwork, info)
  ! Output the intermediate QR state for JS testing
  call begin_test('zungqr_8x5_k5_input')
  call print_int('info', info)
  call print_int('M', M)
  call print_int('N', N)
  call print_int('K', K)
  call print_array('A', A_r, 2*M*N)
  call print_array('TAU', TAU_r, 2*K)
  call end_test()

  call zungqr(M, N, K, A, M, TAU, WORK, lwork, info)
  call begin_test('zungqr_8x5_k5')
  call print_int('info', info)
  call print_int('M', M)
  call print_int('N', N)
  call print_array('A', A_r, 2*M*N)
  call end_test()

end program
