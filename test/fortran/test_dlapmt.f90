program test_dlapmt
  use test_utils
  implicit none

  integer, parameter :: MMAX = 4, NMAX = 5
  integer :: M, N, I, J
  double precision :: X(MMAX, NMAX)
  integer :: K(NMAX)

  ! =====================================================
  ! Test 1: FORWRD=.TRUE., 3x4 matrix
  ! Permutation K = [3, 1, 4, 2] (1-based)
  ! Column j goes to position K(j):
  !   col 1 -> pos 3, col 2 -> pos 1, col 3 -> pos 4, col 4 -> pos 2
  ! =====================================================
  M = 3
  N = 4

  ! Column-major matrix:
  ! [ 1  5  9  13 ]
  ! [ 2  6 10  14 ]
  ! [ 3  7 11  15 ]
  X(1,1) = 1.0d0;  X(2,1) = 2.0d0;  X(3,1) = 3.0d0
  X(1,2) = 5.0d0;  X(2,2) = 6.0d0;  X(3,2) = 7.0d0
  X(1,3) = 9.0d0;  X(2,3) = 10.0d0; X(3,3) = 11.0d0
  X(1,4) = 13.0d0; X(2,4) = 14.0d0; X(3,4) = 15.0d0

  K(1) = 3; K(2) = 1; K(3) = 4; K(4) = 2

  call DLAPMT(.TRUE., M, N, X, MMAX, K)

  call begin_test('forward_3x4')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('X', X, MMAX*N)
  call print_int_array('K', K, N)
  call end_test()

  ! =====================================================
  ! Test 2: FORWRD=.FALSE., 3x4 matrix
  ! Inverse permutation
  ! =====================================================
  M = 3
  N = 4

  X(1,1) = 1.0d0;  X(2,1) = 2.0d0;  X(3,1) = 3.0d0
  X(1,2) = 5.0d0;  X(2,2) = 6.0d0;  X(3,2) = 7.0d0
  X(1,3) = 9.0d0;  X(2,3) = 10.0d0; X(3,3) = 11.0d0
  X(1,4) = 13.0d0; X(2,4) = 14.0d0; X(3,4) = 15.0d0

  K(1) = 3; K(2) = 1; K(3) = 4; K(4) = 2

  call DLAPMT(.FALSE., M, N, X, MMAX, K)

  call begin_test('backward_3x4')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('X', X, MMAX*N)
  call print_int_array('K', K, N)
  call end_test()

  ! =====================================================
  ! Test 3: Identity permutation (FORWRD=.TRUE.)
  ! K = [1, 2, 3] -- no change
  ! =====================================================
  M = 2
  N = 3

  X(1,1) = 10.0d0; X(2,1) = 20.0d0
  X(1,2) = 30.0d0; X(2,2) = 40.0d0
  X(1,3) = 50.0d0; X(2,3) = 60.0d0

  K(1) = 1; K(2) = 2; K(3) = 3

  call DLAPMT(.TRUE., M, N, X, MMAX, K)

  call begin_test('identity_2x3')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('X', X, MMAX*N)
  call end_test()

  ! =====================================================
  ! Test 4: N=1 quick return
  ! =====================================================
  M = 3
  N = 1

  X(1,1) = 42.0d0; X(2,1) = 43.0d0; X(3,1) = 44.0d0
  K(1) = 1

  call DLAPMT(.TRUE., M, N, X, MMAX, K)

  call begin_test('n1_quick')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('X', X, MMAX*N)
  call end_test()

  ! =====================================================
  ! Test 5: N=0 quick return
  ! =====================================================
  M = 3
  N = 0

  call DLAPMT(.TRUE., M, N, X, MMAX, K)

  call begin_test('n0_quick')
  call print_int('M', M)
  call print_int('N', N)
  call end_test()

  ! =====================================================
  ! Test 6: Reverse permutation (FORWRD=.TRUE.)
  ! K = [4, 3, 2, 1] -- complete reversal
  ! =====================================================
  M = 2
  N = 4

  X(1,1) = 1.0d0; X(2,1) = 2.0d0
  X(1,2) = 3.0d0; X(2,2) = 4.0d0
  X(1,3) = 5.0d0; X(2,3) = 6.0d0
  X(1,4) = 7.0d0; X(2,4) = 8.0d0

  K(1) = 4; K(2) = 3; K(3) = 2; K(4) = 1

  call DLAPMT(.TRUE., M, N, X, MMAX, K)

  call begin_test('reverse_fwd_2x4')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('X', X, MMAX*N)
  call end_test()

  ! =====================================================
  ! Test 7: Reverse permutation (FORWRD=.FALSE.)
  ! =====================================================
  M = 2
  N = 4

  X(1,1) = 1.0d0; X(2,1) = 2.0d0
  X(1,2) = 3.0d0; X(2,2) = 4.0d0
  X(1,3) = 5.0d0; X(2,3) = 6.0d0
  X(1,4) = 7.0d0; X(2,4) = 8.0d0

  K(1) = 4; K(2) = 3; K(3) = 2; K(4) = 1

  call DLAPMT(.FALSE., M, N, X, MMAX, K)

  call begin_test('reverse_bwd_2x4')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('X', X, MMAX*N)
  call end_test()

  ! =====================================================
  ! Test 8: Cyclic permutation (FORWRD=.TRUE.)
  ! K = [2, 3, 4, 5, 1] -- rotate left
  ! =====================================================
  M = 2
  N = 5

  X(1,1) = 10.0d0; X(2,1) = 11.0d0
  X(1,2) = 20.0d0; X(2,2) = 21.0d0
  X(1,3) = 30.0d0; X(2,3) = 31.0d0
  X(1,4) = 40.0d0; X(2,4) = 41.0d0
  X(1,5) = 50.0d0; X(2,5) = 51.0d0

  K(1) = 2; K(2) = 3; K(3) = 4; K(4) = 5; K(5) = 1

  call DLAPMT(.TRUE., M, N, X, MMAX, K)

  call begin_test('cyclic_fwd_2x5')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('X', X, MMAX*N)
  call end_test()

end program test_dlapmt
