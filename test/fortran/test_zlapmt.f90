program test_zlapmt
  use test_utils
  implicit none

  integer, parameter :: MMAX = 4, NMAX = 5
  integer :: M, N, I, J
  complex*16 :: X(MMAX, NMAX)
  double precision :: X_r(2*MMAX*NMAX)
  equivalence (X, X_r)
  double precision :: Xpk_r(2*MMAX*NMAX)
  complex*16 :: Xpk(MMAX*NMAX)
  equivalence (Xpk, Xpk_r)
  integer :: K(NMAX)

  ! =====================================================
  ! Test 1: FORWRD=.TRUE., 3x4 matrix
  ! Permutation K = [3, 1, 4, 2] (1-based)
  ! =====================================================
  M = 3
  N = 4

  X(1,1) = (1.0d0, 2.0d0);  X(2,1) = (3.0d0, 4.0d0);  X(3,1) = (5.0d0, 6.0d0)
  X(1,2) = (7.0d0, 8.0d0);  X(2,2) = (9.0d0, 10.0d0); X(3,2) = (11.0d0, 12.0d0)
  X(1,3) = (13.0d0, 14.0d0); X(2,3) = (15.0d0, 16.0d0); X(3,3) = (17.0d0, 18.0d0)
  X(1,4) = (19.0d0, 20.0d0); X(2,4) = (21.0d0, 22.0d0); X(3,4) = (23.0d0, 24.0d0)

  K(1) = 3; K(2) = 1; K(3) = 4; K(4) = 2

  call ZLAPMT(.TRUE., M, N, X, MMAX, K)

  ! Pack M-by-N submatrix for printing (avoid LDA stride mismatch)
  do j = 1, N
    do i = 1, M
      Xpk(i + (j-1)*M) = X(i, j)
    end do
  end do

  call begin_test('forward_3x4')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('X', Xpk_r, 2*M*N)
  call print_int_array('K', K, N)
  call end_test()

  ! =====================================================
  ! Test 2: FORWRD=.FALSE., 3x4 matrix
  ! =====================================================
  M = 3
  N = 4

  X(1,1) = (1.0d0, 2.0d0);  X(2,1) = (3.0d0, 4.0d0);  X(3,1) = (5.0d0, 6.0d0)
  X(1,2) = (7.0d0, 8.0d0);  X(2,2) = (9.0d0, 10.0d0); X(3,2) = (11.0d0, 12.0d0)
  X(1,3) = (13.0d0, 14.0d0); X(2,3) = (15.0d0, 16.0d0); X(3,3) = (17.0d0, 18.0d0)
  X(1,4) = (19.0d0, 20.0d0); X(2,4) = (21.0d0, 22.0d0); X(3,4) = (23.0d0, 24.0d0)

  K(1) = 3; K(2) = 1; K(3) = 4; K(4) = 2

  call ZLAPMT(.FALSE., M, N, X, MMAX, K)

  do j = 1, N
    do i = 1, M
      Xpk(i + (j-1)*M) = X(i, j)
    end do
  end do

  call begin_test('backward_3x4')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('X', Xpk_r, 2*M*N)
  call print_int_array('K', K, N)
  call end_test()

  ! =====================================================
  ! Test 3: Identity permutation (FORWRD=.TRUE.)
  ! K = [1, 2, 3] -- no change
  ! =====================================================
  M = 2
  N = 3

  X(1,1) = (10.0d0, 11.0d0); X(2,1) = (20.0d0, 21.0d0)
  X(1,2) = (30.0d0, 31.0d0); X(2,2) = (40.0d0, 41.0d0)
  X(1,3) = (50.0d0, 51.0d0); X(2,3) = (60.0d0, 61.0d0)

  K(1) = 1; K(2) = 2; K(3) = 3

  call ZLAPMT(.TRUE., M, N, X, MMAX, K)

  do j = 1, N
    do i = 1, M
      Xpk(i + (j-1)*M) = X(i, j)
    end do
  end do

  call begin_test('identity_2x3')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('X', Xpk_r, 2*M*N)
  call end_test()

  ! =====================================================
  ! Test 4: N=1 quick return
  ! =====================================================
  M = 3
  N = 1

  X(1,1) = (42.0d0, 43.0d0); X(2,1) = (44.0d0, 45.0d0); X(3,1) = (46.0d0, 47.0d0)
  K(1) = 1

  call ZLAPMT(.TRUE., M, N, X, MMAX, K)

  do j = 1, N
    do i = 1, M
      Xpk(i + (j-1)*M) = X(i, j)
    end do
  end do

  call begin_test('n1_quick')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('X', Xpk_r, 2*M*N)
  call end_test()

  ! =====================================================
  ! Test 5: Reverse permutation (FORWRD=.TRUE.)
  ! K = [4, 3, 2, 1] -- complete reversal
  ! =====================================================
  M = 2
  N = 4

  X(1,1) = (1.0d0, 0.5d0); X(2,1) = (2.0d0, 1.5d0)
  X(1,2) = (3.0d0, 2.5d0); X(2,2) = (4.0d0, 3.5d0)
  X(1,3) = (5.0d0, 4.5d0); X(2,3) = (6.0d0, 5.5d0)
  X(1,4) = (7.0d0, 6.5d0); X(2,4) = (8.0d0, 7.5d0)

  K(1) = 4; K(2) = 3; K(3) = 2; K(4) = 1

  call ZLAPMT(.TRUE., M, N, X, MMAX, K)

  do j = 1, N
    do i = 1, M
      Xpk(i + (j-1)*M) = X(i, j)
    end do
  end do

  call begin_test('reverse_fwd_2x4')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('X', Xpk_r, 2*M*N)
  call end_test()

  ! =====================================================
  ! Test 6: Reverse permutation (FORWRD=.FALSE.)
  ! =====================================================
  M = 2
  N = 4

  X(1,1) = (1.0d0, 0.5d0); X(2,1) = (2.0d0, 1.5d0)
  X(1,2) = (3.0d0, 2.5d0); X(2,2) = (4.0d0, 3.5d0)
  X(1,3) = (5.0d0, 4.5d0); X(2,3) = (6.0d0, 5.5d0)
  X(1,4) = (7.0d0, 6.5d0); X(2,4) = (8.0d0, 7.5d0)

  K(1) = 4; K(2) = 3; K(3) = 2; K(4) = 1

  call ZLAPMT(.FALSE., M, N, X, MMAX, K)

  do j = 1, N
    do i = 1, M
      Xpk(i + (j-1)*M) = X(i, j)
    end do
  end do

  call begin_test('reverse_bwd_2x4')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('X', Xpk_r, 2*M*N)
  call end_test()

  ! =====================================================
  ! Test 7: Cyclic permutation (FORWRD=.TRUE.)
  ! K = [2, 3, 4, 5, 1] -- rotate left
  ! =====================================================
  M = 2
  N = 5

  X(1,1) = (10.0d0, 1.0d0); X(2,1) = (11.0d0, 2.0d0)
  X(1,2) = (20.0d0, 3.0d0); X(2,2) = (21.0d0, 4.0d0)
  X(1,3) = (30.0d0, 5.0d0); X(2,3) = (31.0d0, 6.0d0)
  X(1,4) = (40.0d0, 7.0d0); X(2,4) = (41.0d0, 8.0d0)
  X(1,5) = (50.0d0, 9.0d0); X(2,5) = (51.0d0, 10.0d0)

  K(1) = 2; K(2) = 3; K(3) = 4; K(4) = 5; K(5) = 1

  call ZLAPMT(.TRUE., M, N, X, MMAX, K)

  do j = 1, N
    do i = 1, M
      Xpk(i + (j-1)*M) = X(i, j)
    end do
  end do

  call begin_test('cyclic_fwd_2x5')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('X', Xpk_r, 2*M*N)
  call end_test()

end program test_zlapmt
