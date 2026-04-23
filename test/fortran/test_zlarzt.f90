program test_zlarzt
  use test_utils
  implicit none

  integer, parameter :: NMAX = 8, KMAX = 4
  complex*16 :: V(KMAX, NMAX), T(KMAX, KMAX), TAU(KMAX)
  complex*16 :: Vpk(KMAX * NMAX), Tpk(KMAX * KMAX)
  double precision :: Vpk_r(2 * KMAX * NMAX), Tpk_r(2 * KMAX * KMAX)
  double precision :: TAU_r(2 * KMAX)
  equivalence (Vpk, Vpk_r)
  equivalence (Tpk, Tpk_r)
  equivalence (TAU, TAU_r)

  integer :: N, K, i, j

  ! ============================================================
  ! Test 1: K=1, N=5, backward/rowwise
  ! ============================================================
  K = 1
  N = 5
  V = (0.0d0, 0.0d0)
  T = (0.0d0, 0.0d0)

  ! V is K x N. Row 1 stores reflector 1.
  ! DIRECT='B': identity at the end, so V(1, N) = 1 (restored on exit)
  ! The first N-K columns store the reflector body
  V(1,1) = (0.3d0, 0.1d0)
  V(1,2) = (-0.5d0, 0.2d0)
  V(1,3) = (0.7d0, -0.4d0)
  V(1,4) = (0.1d0, 0.6d0)
  ! V(1,5) = 1.0 (identity part, handled internally)

  TAU(1) = (0.8d0, -0.3d0)

  call ZLARZT('B', 'R', N, K, V, KMAX, TAU, T, KMAX)

  call begin_test('k1_backward_rowwise')

  ! Pack V (K x N)
  do j = 1, N
    do i = 1, K
      Vpk(i + (j-1)*K) = V(i,j)
    end do
  end do
  call print_array('V', Vpk_r, 2*K*N)

  ! Pack T (K x K)
  do j = 1, K
    do i = 1, K
      Tpk(i + (j-1)*K) = T(i,j)
    end do
  end do
  call print_array('T', Tpk_r, 2*K*K)
  call print_array('TAU', TAU_r, 2*K)
  call end_test()

  ! ============================================================
  ! Test 2: K=3, N=8, backward/rowwise
  ! ============================================================
  K = 3
  N = 8
  V = (0.0d0, 0.0d0)
  T = (0.0d0, 0.0d0)

  ! V is K x N with DIRECT='B': identity at columns N-K+1 : N
  ! Reflector body in columns 1 : N-K
  ! Row 1 (reflector 1): V(1,6) = 1 (identity), body in cols 1-5
  V(1,1) = (0.2d0, 0.1d0)
  V(1,2) = (-0.3d0, 0.4d0)
  V(1,3) = (0.5d0, -0.2d0)
  V(1,4) = (0.1d0, 0.3d0)
  V(1,5) = (-0.4d0, 0.6d0)

  ! Row 2 (reflector 2): V(2,7) = 1 (identity), body in cols 1-5
  V(2,1) = (0.6d0, -0.1d0)
  V(2,2) = (0.3d0, 0.5d0)
  V(2,3) = (-0.2d0, 0.7d0)
  V(2,4) = (0.4d0, -0.3d0)
  V(2,5) = (0.1d0, 0.2d0)

  ! Row 3 (reflector 3): V(3,8) = 1 (identity), body in cols 1-5
  V(3,1) = (-0.1d0, 0.4d0)
  V(3,2) = (0.7d0, -0.6d0)
  V(3,3) = (0.3d0, 0.1d0)
  V(3,4) = (-0.5d0, 0.2d0)
  V(3,5) = (0.2d0, -0.8d0)

  TAU(1) = (0.5d0, -0.2d0)
  TAU(2) = (0.7d0, 0.1d0)
  TAU(3) = (0.3d0, -0.5d0)

  call ZLARZT('B', 'R', N, K, V, KMAX, TAU, T, KMAX)

  call begin_test('k3_backward_rowwise')

  ! Pack V (K x N)
  do j = 1, N
    do i = 1, K
      Vpk(i + (j-1)*K) = V(i,j)
    end do
  end do
  call print_array('V', Vpk_r, 2*K*N)

  ! Pack T (K x K)
  do j = 1, K
    do i = 1, K
      Tpk(i + (j-1)*K) = T(i,j)
    end do
  end do
  call print_array('T', Tpk_r, 2*K*K)
  call print_array('TAU', TAU_r, 2*K)
  call end_test()

  ! ============================================================
  ! Test 3: K=3, N=8, backward/rowwise, one zero tau
  ! ============================================================
  K = 3
  N = 8
  V = (0.0d0, 0.0d0)
  T = (0.0d0, 0.0d0)

  ! Same V entries as test 2
  V(1,1) = (0.2d0, 0.1d0)
  V(1,2) = (-0.3d0, 0.4d0)
  V(1,3) = (0.5d0, -0.2d0)
  V(1,4) = (0.1d0, 0.3d0)
  V(1,5) = (-0.4d0, 0.6d0)

  V(2,1) = (0.6d0, -0.1d0)
  V(2,2) = (0.3d0, 0.5d0)
  V(2,3) = (-0.2d0, 0.7d0)
  V(2,4) = (0.4d0, -0.3d0)
  V(2,5) = (0.1d0, 0.2d0)

  V(3,1) = (-0.1d0, 0.4d0)
  V(3,2) = (0.7d0, -0.6d0)
  V(3,3) = (0.3d0, 0.1d0)
  V(3,4) = (-0.5d0, 0.2d0)
  V(3,5) = (0.2d0, -0.8d0)

  ! Middle tau is zero -> H(2) = I
  TAU(1) = (0.5d0, -0.2d0)
  TAU(2) = (0.0d0, 0.0d0)
  TAU(3) = (0.3d0, -0.5d0)

  call ZLARZT('B', 'R', N, K, V, KMAX, TAU, T, KMAX)

  call begin_test('k3_zero_tau')

  ! Pack T (K x K)
  do j = 1, K
    do i = 1, K
      Tpk(i + (j-1)*K) = T(i,j)
    end do
  end do
  call print_array('T', Tpk_r, 2*K*K)
  call print_array('TAU', TAU_r, 2*K)
  call end_test()

end program
