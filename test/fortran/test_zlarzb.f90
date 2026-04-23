program test_zlarzb
  use test_utils
  implicit none

  integer, parameter :: LDV = 6
  integer, parameter :: LDT = 6
  integer, parameter :: LDC = 8
  integer, parameter :: LDW = 8
  complex*16 :: V(LDV, LDV), T(LDT, LDT), C(LDC, LDC), WORK(LDW, LDW)
  complex*16 :: Cpk(LDC * LDC)
  double precision :: Cpk_r(2 * LDC * LDC)
  equivalence (Cpk, Cpk_r)

  integer :: M, N, K, L, i, j

  ! =============================================
  ! Test 1: SIDE='L', TRANS='N', M=5, N=4, K=2, L=3
  ! =============================================
  M = 5
  N = 4
  K = 2
  L = 3

  V = (0.0d0, 0.0d0)
  ! V is K-by-L
  V(1, 1) = (0.2d0, 0.1d0)
  V(1, 2) = (-0.1d0, 0.3d0)
  V(1, 3) = (0.3d0, -0.2d0)
  V(2, 1) = (0.4d0, -0.3d0)
  V(2, 2) = (0.5d0, 0.2d0)
  V(2, 3) = (-0.2d0, 0.4d0)

  T = (0.0d0, 0.0d0)
  ! T is K-by-K lower triangular
  T(1, 1) = (0.7d0, 0.1d0)
  T(2, 1) = (0.3d0, -0.2d0)
  T(2, 2) = (0.5d0, 0.3d0)

  C = (0.0d0, 0.0d0)
  do j = 1, N
    do i = 1, M
      C(i, j) = dcmplx(dble(i) + 0.1d0 * dble(j), 0.05d0 * dble(i*j))
    end do
  end do

  WORK = (0.0d0, 0.0d0)
  call ZLARZB('L', 'N', 'B', 'R', M, N, K, L, V, LDV, T, LDT, C, LDC, WORK, LDW)
  call begin_test('left_notrans_m5_n4_k2_l3')
  do j = 1, N
    do i = 1, M
      Cpk(i + (j-1)*M) = C(i, j)
    end do
  end do
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! =============================================
  ! Test 2: SIDE='L', TRANS='C', M=5, N=4, K=2, L=3
  ! =============================================
  M = 5
  N = 4
  K = 2
  L = 3

  V = (0.0d0, 0.0d0)
  V(1, 1) = (0.2d0, 0.1d0)
  V(1, 2) = (-0.1d0, 0.3d0)
  V(1, 3) = (0.3d0, -0.2d0)
  V(2, 1) = (0.4d0, -0.3d0)
  V(2, 2) = (0.5d0, 0.2d0)
  V(2, 3) = (-0.2d0, 0.4d0)

  T = (0.0d0, 0.0d0)
  T(1, 1) = (0.7d0, 0.1d0)
  T(2, 1) = (0.3d0, -0.2d0)
  T(2, 2) = (0.5d0, 0.3d0)

  C = (0.0d0, 0.0d0)
  do j = 1, N
    do i = 1, M
      C(i, j) = dcmplx(dble(i) + 0.1d0 * dble(j), 0.05d0 * dble(i*j))
    end do
  end do

  WORK = (0.0d0, 0.0d0)
  call ZLARZB('L', 'C', 'B', 'R', M, N, K, L, V, LDV, T, LDT, C, LDC, WORK, LDW)
  call begin_test('left_ctrans_m5_n4_k2_l3')
  do j = 1, N
    do i = 1, M
      Cpk(i + (j-1)*M) = C(i, j)
    end do
  end do
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! =============================================
  ! Test 3: SIDE='R', TRANS='N', M=4, N=5, K=2, L=3
  ! =============================================
  M = 4
  N = 5
  K = 2
  L = 3

  V = (0.0d0, 0.0d0)
  V(1, 1) = (0.2d0, 0.1d0)
  V(1, 2) = (-0.1d0, 0.3d0)
  V(1, 3) = (0.3d0, -0.2d0)
  V(2, 1) = (0.4d0, -0.3d0)
  V(2, 2) = (0.5d0, 0.2d0)
  V(2, 3) = (-0.2d0, 0.4d0)

  T = (0.0d0, 0.0d0)
  T(1, 1) = (0.7d0, 0.1d0)
  T(2, 1) = (0.3d0, -0.2d0)
  T(2, 2) = (0.5d0, 0.3d0)

  C = (0.0d0, 0.0d0)
  do j = 1, N
    do i = 1, M
      C(i, j) = dcmplx(dble(i) + 0.1d0 * dble(j), 0.05d0 * dble(i*j))
    end do
  end do

  WORK = (0.0d0, 0.0d0)
  call ZLARZB('R', 'N', 'B', 'R', M, N, K, L, V, LDV, T, LDT, C, LDC, WORK, LDW)
  call begin_test('right_notrans_m4_n5_k2_l3')
  do j = 1, N
    do i = 1, M
      Cpk(i + (j-1)*M) = C(i, j)
    end do
  end do
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! =============================================
  ! Test 4: SIDE='R', TRANS='C', M=4, N=5, K=2, L=3
  ! =============================================
  M = 4
  N = 5
  K = 2
  L = 3

  V = (0.0d0, 0.0d0)
  V(1, 1) = (0.2d0, 0.1d0)
  V(1, 2) = (-0.1d0, 0.3d0)
  V(1, 3) = (0.3d0, -0.2d0)
  V(2, 1) = (0.4d0, -0.3d0)
  V(2, 2) = (0.5d0, 0.2d0)
  V(2, 3) = (-0.2d0, 0.4d0)

  T = (0.0d0, 0.0d0)
  T(1, 1) = (0.7d0, 0.1d0)
  T(2, 1) = (0.3d0, -0.2d0)
  T(2, 2) = (0.5d0, 0.3d0)

  C = (0.0d0, 0.0d0)
  do j = 1, N
    do i = 1, M
      C(i, j) = dcmplx(dble(i) + 0.1d0 * dble(j), 0.05d0 * dble(i*j))
    end do
  end do

  WORK = (0.0d0, 0.0d0)
  call ZLARZB('R', 'C', 'B', 'R', M, N, K, L, V, LDV, T, LDT, C, LDC, WORK, LDW)
  call begin_test('right_ctrans_m4_n5_k2_l3')
  do j = 1, N
    do i = 1, M
      Cpk(i + (j-1)*M) = C(i, j)
    end do
  end do
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! =============================================
  ! Test 5: SIDE='L', TRANS='N', L=0 (no trailing part)
  ! =============================================
  M = 4
  N = 3
  K = 2
  L = 0

  V = (0.0d0, 0.0d0)
  T = (0.0d0, 0.0d0)
  T(1, 1) = (0.6d0, 0.1d0)
  T(2, 1) = (0.2d0, -0.1d0)
  T(2, 2) = (0.4d0, 0.2d0)

  C = (0.0d0, 0.0d0)
  do j = 1, N
    do i = 1, M
      C(i, j) = dcmplx(dble(i * 2) - dble(j), 0.1d0 * dble(i + j))
    end do
  end do

  WORK = (0.0d0, 0.0d0)
  call ZLARZB('L', 'N', 'B', 'R', M, N, K, L, V, LDV, T, LDT, C, LDC, WORK, LDW)
  call begin_test('left_notrans_l0')
  do j = 1, N
    do i = 1, M
      Cpk(i + (j-1)*M) = C(i, j)
    end do
  end do
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! =============================================
  ! Test 6: K=1 single reflector, SIDE='L', TRANS='N'
  ! =============================================
  M = 4
  N = 3
  K = 1
  L = 2

  V = (0.0d0, 0.0d0)
  V(1, 1) = (0.3d0, 0.1d0)
  V(1, 2) = (-0.4d0, 0.2d0)

  T = (0.0d0, 0.0d0)
  T(1, 1) = (0.8d0, -0.1d0)

  C = (0.0d0, 0.0d0)
  do j = 1, N
    do i = 1, M
      C(i, j) = dcmplx(dble(i) * 0.5d0 + dble(j) * 0.2d0, 0.05d0 * dble(i))
    end do
  end do

  WORK = (0.0d0, 0.0d0)
  call ZLARZB('L', 'N', 'B', 'R', M, N, K, L, V, LDV, T, LDT, C, LDC, WORK, LDW)
  call begin_test('left_notrans_k1')
  do j = 1, N
    do i = 1, M
      Cpk(i + (j-1)*M) = C(i, j)
    end do
  end do
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! =============================================
  ! Test 7: K=1, SIDE='R', TRANS='C'
  ! =============================================
  M = 3
  N = 4
  K = 1
  L = 2

  V = (0.0d0, 0.0d0)
  V(1, 1) = (0.3d0, 0.1d0)
  V(1, 2) = (-0.4d0, 0.2d0)

  T = (0.0d0, 0.0d0)
  T(1, 1) = (0.8d0, -0.1d0)

  C = (0.0d0, 0.0d0)
  do j = 1, N
    do i = 1, M
      C(i, j) = dcmplx(dble(i) * 0.5d0 + dble(j) * 0.2d0, 0.05d0 * dble(j))
    end do
  end do

  WORK = (0.0d0, 0.0d0)
  call ZLARZB('R', 'C', 'B', 'R', M, N, K, L, V, LDV, T, LDT, C, LDC, WORK, LDW)
  call begin_test('right_ctrans_k1')
  do j = 1, N
    do i = 1, M
      Cpk(i + (j-1)*M) = C(i, j)
    end do
  end do
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

end program
