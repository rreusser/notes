program test_zlapmr
  use test_utils
  implicit none

  integer, parameter :: MMAX = 5, NMAX = 4
  integer :: M, N, I, J
  complex*16 :: X(MMAX, NMAX)
  integer :: K(MMAX)

  ! For printing: pack M-by-N submatrix into contiguous array
  complex*16 :: Xpk(MMAX*NMAX)
  double precision :: Xpk_r(2*MMAX*NMAX)
  equivalence (Xpk, Xpk_r)

  ! =====================================================
  ! Test 1: FORWRD=.TRUE., 4x3 matrix
  ! Permutation K = [3, 1, 4, 2] (1-based)
  ! =====================================================
  M = 4
  N = 3

  X(1,1) = dcmplx(1.0d0, 2.0d0)
  X(2,1) = dcmplx(3.0d0, 4.0d0)
  X(3,1) = dcmplx(5.0d0, 6.0d0)
  X(4,1) = dcmplx(7.0d0, 8.0d0)
  X(1,2) = dcmplx(9.0d0, 10.0d0)
  X(2,2) = dcmplx(11.0d0, 12.0d0)
  X(3,2) = dcmplx(13.0d0, 14.0d0)
  X(4,2) = dcmplx(15.0d0, 16.0d0)
  X(1,3) = dcmplx(17.0d0, 18.0d0)
  X(2,3) = dcmplx(19.0d0, 20.0d0)
  X(3,3) = dcmplx(21.0d0, 22.0d0)
  X(4,3) = dcmplx(23.0d0, 24.0d0)

  K(1) = 3; K(2) = 1; K(3) = 4; K(4) = 2

  call ZLAPMR(.TRUE., M, N, X, MMAX, K)

  ! Pack M-by-N submatrix into contiguous storage for printing
  do j = 1, N
    do i = 1, M
      Xpk(i + (j-1)*M) = X(i, j)
    end do
  end do

  call begin_test('forward_4x3')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('X', Xpk_r, 2*M*N)
  call print_int_array('K', K, M)
  call end_test()

  ! =====================================================
  ! Test 2: FORWRD=.FALSE., 4x3 matrix
  ! Backward permutation
  ! =====================================================
  M = 4
  N = 3

  X(1,1) = dcmplx(1.0d0, 2.0d0)
  X(2,1) = dcmplx(3.0d0, 4.0d0)
  X(3,1) = dcmplx(5.0d0, 6.0d0)
  X(4,1) = dcmplx(7.0d0, 8.0d0)
  X(1,2) = dcmplx(9.0d0, 10.0d0)
  X(2,2) = dcmplx(11.0d0, 12.0d0)
  X(3,2) = dcmplx(13.0d0, 14.0d0)
  X(4,2) = dcmplx(15.0d0, 16.0d0)
  X(1,3) = dcmplx(17.0d0, 18.0d0)
  X(2,3) = dcmplx(19.0d0, 20.0d0)
  X(3,3) = dcmplx(21.0d0, 22.0d0)
  X(4,3) = dcmplx(23.0d0, 24.0d0)

  K(1) = 3; K(2) = 1; K(3) = 4; K(4) = 2

  call ZLAPMR(.FALSE., M, N, X, MMAX, K)

  do j = 1, N
    do i = 1, M
      Xpk(i + (j-1)*M) = X(i, j)
    end do
  end do

  call begin_test('backward_4x3')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('X', Xpk_r, 2*M*N)
  call print_int_array('K', K, M)
  call end_test()

  ! =====================================================
  ! Test 3: Identity permutation (FORWRD=.TRUE.)
  ! K = [1, 2, 3] -- no change
  ! =====================================================
  M = 3
  N = 2

  X(1,1) = dcmplx(10.0d0, 20.0d0)
  X(2,1) = dcmplx(30.0d0, 40.0d0)
  X(3,1) = dcmplx(50.0d0, 60.0d0)
  X(1,2) = dcmplx(70.0d0, 80.0d0)
  X(2,2) = dcmplx(90.0d0, 100.0d0)
  X(3,2) = dcmplx(110.0d0, 120.0d0)

  K(1) = 1; K(2) = 2; K(3) = 3

  call ZLAPMR(.TRUE., M, N, X, MMAX, K)

  do j = 1, N
    do i = 1, M
      Xpk(i + (j-1)*M) = X(i, j)
    end do
  end do

  call begin_test('identity_3x2')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('X', Xpk_r, 2*M*N)
  call end_test()

  ! =====================================================
  ! Test 4: Reverse permutation (FORWRD=.TRUE.)
  ! K = [4, 3, 2, 1] -- complete reversal
  ! =====================================================
  M = 4
  N = 2

  X(1,1) = dcmplx(1.0d0, 2.0d0)
  X(2,1) = dcmplx(3.0d0, 4.0d0)
  X(3,1) = dcmplx(5.0d0, 6.0d0)
  X(4,1) = dcmplx(7.0d0, 8.0d0)
  X(1,2) = dcmplx(9.0d0, 10.0d0)
  X(2,2) = dcmplx(11.0d0, 12.0d0)
  X(3,2) = dcmplx(13.0d0, 14.0d0)
  X(4,2) = dcmplx(15.0d0, 16.0d0)

  K(1) = 4; K(2) = 3; K(3) = 2; K(4) = 1

  call ZLAPMR(.TRUE., M, N, X, MMAX, K)

  do j = 1, N
    do i = 1, M
      Xpk(i + (j-1)*M) = X(i, j)
    end do
  end do

  call begin_test('reverse_fwd_4x2')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('X', Xpk_r, 2*M*N)
  call end_test()

  ! =====================================================
  ! Test 5: Reverse permutation (FORWRD=.FALSE.)
  ! =====================================================
  M = 4
  N = 2

  X(1,1) = dcmplx(1.0d0, 2.0d0)
  X(2,1) = dcmplx(3.0d0, 4.0d0)
  X(3,1) = dcmplx(5.0d0, 6.0d0)
  X(4,1) = dcmplx(7.0d0, 8.0d0)
  X(1,2) = dcmplx(9.0d0, 10.0d0)
  X(2,2) = dcmplx(11.0d0, 12.0d0)
  X(3,2) = dcmplx(13.0d0, 14.0d0)
  X(4,2) = dcmplx(15.0d0, 16.0d0)

  K(1) = 4; K(2) = 3; K(3) = 2; K(4) = 1

  call ZLAPMR(.FALSE., M, N, X, MMAX, K)

  do j = 1, N
    do i = 1, M
      Xpk(i + (j-1)*M) = X(i, j)
    end do
  end do

  call begin_test('reverse_bwd_4x2')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('X', Xpk_r, 2*M*N)
  call end_test()

  ! =====================================================
  ! Test 6: Cyclic permutation (FORWRD=.TRUE.)
  ! K = [2, 3, 4, 5, 1] -- rotate rows up
  ! =====================================================
  M = 5
  N = 2

  X(1,1) = dcmplx(10.0d0, 11.0d0)
  X(2,1) = dcmplx(20.0d0, 21.0d0)
  X(3,1) = dcmplx(30.0d0, 31.0d0)
  X(4,1) = dcmplx(40.0d0, 41.0d0)
  X(5,1) = dcmplx(50.0d0, 51.0d0)
  X(1,2) = dcmplx(12.0d0, 13.0d0)
  X(2,2) = dcmplx(22.0d0, 23.0d0)
  X(3,2) = dcmplx(32.0d0, 33.0d0)
  X(4,2) = dcmplx(42.0d0, 43.0d0)
  X(5,2) = dcmplx(52.0d0, 53.0d0)

  K(1) = 2; K(2) = 3; K(3) = 4; K(4) = 5; K(5) = 1

  call ZLAPMR(.TRUE., M, N, X, MMAX, K)

  do j = 1, N
    do i = 1, M
      Xpk(i + (j-1)*M) = X(i, j)
    end do
  end do

  call begin_test('cyclic_fwd_5x2')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('X', Xpk_r, 2*M*N)
  call end_test()

  ! =====================================================
  ! Test 7: Cyclic permutation (FORWRD=.FALSE.)
  ! K = [2, 3, 4, 5, 1]
  ! =====================================================
  M = 5
  N = 2

  X(1,1) = dcmplx(10.0d0, 11.0d0)
  X(2,1) = dcmplx(20.0d0, 21.0d0)
  X(3,1) = dcmplx(30.0d0, 31.0d0)
  X(4,1) = dcmplx(40.0d0, 41.0d0)
  X(5,1) = dcmplx(50.0d0, 51.0d0)
  X(1,2) = dcmplx(12.0d0, 13.0d0)
  X(2,2) = dcmplx(22.0d0, 23.0d0)
  X(3,2) = dcmplx(32.0d0, 33.0d0)
  X(4,2) = dcmplx(42.0d0, 43.0d0)
  X(5,2) = dcmplx(52.0d0, 53.0d0)

  K(1) = 2; K(2) = 3; K(3) = 4; K(4) = 5; K(5) = 1

  call ZLAPMR(.FALSE., M, N, X, MMAX, K)

  do j = 1, N
    do i = 1, M
      Xpk(i + (j-1)*M) = X(i, j)
    end do
  end do

  call begin_test('cyclic_bwd_5x2')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('X', Xpk_r, 2*M*N)
  call end_test()

end program test_zlapmr
