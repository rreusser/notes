program test_dlarzt
  use test_utils
  implicit none

  integer, parameter :: NMAX = 10
  double precision :: V(NMAX, NMAX), T(NMAX, NMAX), TAU(NMAX)
  integer :: i, j, N, K

  ! =============================================
  ! Test 1: K=1, N=4, backward/rowwise
  ! Single reflector — T should just be TAU(1)
  ! =============================================
  N = 4
  K = 1
  do j = 1, N
    do i = 1, K
      V(i, j) = 0.0d0
    end do
  end do
  ! V is K-by-N rowwise: row 1 is the reflector vector
  V(1, 1) = 1.0d0
  V(1, 2) = 0.5d0
  V(1, 3) = -0.3d0
  V(1, 4) = 0.7d0
  TAU(1) = 0.8d0

  do j = 1, K
    do i = 1, K
      T(i, j) = 0.0d0
    end do
  end do

  call DLARZT('B', 'R', N, K, V, NMAX, TAU, T, NMAX)

  call begin_test('k1_n4')
  call print_array('T', T, K*K)
  call end_test()

  ! =============================================
  ! Test 2: K=3, N=5, backward/rowwise
  ! Multiple reflectors
  ! =============================================
  N = 5
  K = 3
  do j = 1, N
    do i = 1, K
      V(i, j) = 0.0d0
    end do
  end do
  ! Row 1 (reflector 1)
  V(1, 1) = 1.0d0
  V(1, 2) = 0.3d0
  V(1, 3) = -0.2d0
  V(1, 4) = 0.5d0
  V(1, 5) = 0.1d0
  ! Row 2 (reflector 2)
  V(2, 1) = 0.4d0
  V(2, 2) = 1.0d0
  V(2, 3) = -0.6d0
  V(2, 4) = 0.2d0
  V(2, 5) = 0.8d0
  ! Row 3 (reflector 3)
  V(3, 1) = -0.1d0
  V(3, 2) = 0.7d0
  V(3, 3) = 1.0d0
  V(3, 4) = -0.3d0
  V(3, 5) = 0.4d0

  TAU(1) = 0.5d0
  TAU(2) = 0.7d0
  TAU(3) = 0.9d0

  do j = 1, K
    do i = 1, K
      T(i, j) = 0.0d0
    end do
  end do

  call DLARZT('B', 'R', N, K, V, NMAX, TAU, T, NMAX)

  call begin_test('k3_n5')
  ! Print K-by-K submatrix of T packed column-major
  call print_array('T', T(1:K, 1:K), K*K)
  call end_test()

  ! =============================================
  ! Test 3: K=2, N=3, with a zero TAU
  ! One reflector is identity
  ! =============================================
  N = 3
  K = 2
  do j = 1, N
    do i = 1, K
      V(i, j) = 0.0d0
    end do
  end do
  V(1, 1) = 1.0d0
  V(1, 2) = 0.4d0
  V(1, 3) = -0.6d0
  V(2, 1) = 0.3d0
  V(2, 2) = 1.0d0
  V(2, 3) = 0.5d0

  TAU(1) = 0.0d0  ! zero tau — identity reflector
  TAU(2) = 0.6d0

  do j = 1, K
    do i = 1, K
      T(i, j) = 0.0d0
    end do
  end do

  call DLARZT('B', 'R', N, K, V, NMAX, TAU, T, NMAX)

  call begin_test('k2_n3_zero_tau')
  call print_array('T', T(1:K, 1:K), K*K)
  call end_test()

  ! =============================================
  ! Test 4: K=1, N=1
  ! Edge case: single element
  ! =============================================
  N = 1
  K = 1
  V(1, 1) = 1.0d0
  TAU(1) = 0.3d0
  T(1, 1) = 0.0d0

  call DLARZT('B', 'R', N, K, V, NMAX, TAU, T, NMAX)

  call begin_test('k1_n1')
  call print_array('T', T, 1)
  call end_test()

  ! =============================================
  ! Test 5: K=2, N=4, both TAUs zero
  ! =============================================
  N = 4
  K = 2
  do j = 1, N
    do i = 1, K
      V(i, j) = 0.0d0
    end do
  end do
  V(1, 1) = 1.0d0
  V(1, 2) = 0.2d0
  V(1, 3) = 0.3d0
  V(1, 4) = 0.4d0
  V(2, 1) = 0.5d0
  V(2, 2) = 1.0d0
  V(2, 3) = 0.6d0
  V(2, 4) = 0.7d0

  TAU(1) = 0.0d0
  TAU(2) = 0.0d0

  do j = 1, K
    do i = 1, K
      T(i, j) = 0.0d0
    end do
  end do

  call DLARZT('B', 'R', N, K, V, NMAX, TAU, T, NMAX)

  call begin_test('k2_n4_all_zero_tau')
  call print_array('T', T(1:K, 1:K), K*K)
  call end_test()

end program
