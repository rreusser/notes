program test_dlapmr
  use test_utils
  implicit none

  integer, parameter :: MMAX = 5, NMAX = 4
  integer :: M, N, I, J
  double precision :: X(MMAX, NMAX)
  integer :: K(MMAX)

  ! =====================================================
  ! Test 1: FORWRD=.TRUE., 4x3 matrix
  ! Permutation K = [3, 1, 4, 2] (1-based)
  ! Row i goes to position K(i):
  !   row 1 -> pos 3, row 2 -> pos 1, row 3 -> pos 4, row 4 -> pos 2
  ! =====================================================
  M = 4
  N = 3

  ! Column-major matrix (M=4, N=3):
  ! [ 1  5  9  ]
  ! [ 2  6 10  ]
  ! [ 3  7 11  ]
  ! [ 4  8 12  ]
  X(1,1) = 1.0d0;  X(2,1) = 2.0d0;  X(3,1) = 3.0d0;  X(4,1) = 4.0d0
  X(1,2) = 5.0d0;  X(2,2) = 6.0d0;  X(3,2) = 7.0d0;  X(4,2) = 8.0d0
  X(1,3) = 9.0d0;  X(2,3) = 10.0d0; X(3,3) = 11.0d0; X(4,3) = 12.0d0

  K(1) = 3; K(2) = 1; K(3) = 4; K(4) = 2

  call DLAPMR(.TRUE., M, N, X, MMAX, K)

  call begin_test('forward_4x3')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('X', X, MMAX*N)
  call print_int_array('K', K, M)
  call end_test()

  ! =====================================================
  ! Test 2: FORWRD=.FALSE., 4x3 matrix
  ! Backward permutation
  ! =====================================================
  M = 4
  N = 3

  X(1,1) = 1.0d0;  X(2,1) = 2.0d0;  X(3,1) = 3.0d0;  X(4,1) = 4.0d0
  X(1,2) = 5.0d0;  X(2,2) = 6.0d0;  X(3,2) = 7.0d0;  X(4,2) = 8.0d0
  X(1,3) = 9.0d0;  X(2,3) = 10.0d0; X(3,3) = 11.0d0; X(4,3) = 12.0d0

  K(1) = 3; K(2) = 1; K(3) = 4; K(4) = 2

  call DLAPMR(.FALSE., M, N, X, MMAX, K)

  call begin_test('backward_4x3')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('X', X, MMAX*N)
  call print_int_array('K', K, M)
  call end_test()

  ! =====================================================
  ! Test 3: Identity permutation (FORWRD=.TRUE.)
  ! K = [1, 2, 3] -- no change
  ! =====================================================
  M = 3
  N = 2

  X(1,1) = 10.0d0; X(2,1) = 20.0d0; X(3,1) = 30.0d0
  X(1,2) = 40.0d0; X(2,2) = 50.0d0; X(3,2) = 60.0d0

  K(1) = 1; K(2) = 2; K(3) = 3

  call DLAPMR(.TRUE., M, N, X, MMAX, K)

  call begin_test('identity_3x2')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('X', X, MMAX*N)
  call end_test()

  ! =====================================================
  ! Test 4: M=1 quick return
  ! =====================================================
  M = 1
  N = 3

  X(1,1) = 42.0d0; X(1,2) = 43.0d0; X(1,3) = 44.0d0
  K(1) = 1

  call DLAPMR(.TRUE., M, N, X, MMAX, K)

  call begin_test('m1_quick')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('X', X, MMAX*N)
  call end_test()

  ! =====================================================
  ! Test 5: M=0 quick return
  ! =====================================================
  M = 0
  N = 3

  call DLAPMR(.TRUE., M, N, X, MMAX, K)

  call begin_test('m0_quick')
  call print_int('M', M)
  call print_int('N', N)
  call end_test()

  ! =====================================================
  ! Test 6: Reverse permutation (FORWRD=.TRUE.)
  ! K = [4, 3, 2, 1] -- complete reversal
  ! =====================================================
  M = 4
  N = 2

  X(1,1) = 1.0d0; X(2,1) = 2.0d0; X(3,1) = 3.0d0; X(4,1) = 4.0d0
  X(1,2) = 5.0d0; X(2,2) = 6.0d0; X(3,2) = 7.0d0; X(4,2) = 8.0d0

  K(1) = 4; K(2) = 3; K(3) = 2; K(4) = 1

  call DLAPMR(.TRUE., M, N, X, MMAX, K)

  call begin_test('reverse_fwd_4x2')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('X', X, MMAX*N)
  call end_test()

  ! =====================================================
  ! Test 7: Reverse permutation (FORWRD=.FALSE.)
  ! =====================================================
  M = 4
  N = 2

  X(1,1) = 1.0d0; X(2,1) = 2.0d0; X(3,1) = 3.0d0; X(4,1) = 4.0d0
  X(1,2) = 5.0d0; X(2,2) = 6.0d0; X(3,2) = 7.0d0; X(4,2) = 8.0d0

  K(1) = 4; K(2) = 3; K(3) = 2; K(4) = 1

  call DLAPMR(.FALSE., M, N, X, MMAX, K)

  call begin_test('reverse_bwd_4x2')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('X', X, MMAX*N)
  call end_test()

  ! =====================================================
  ! Test 8: Cyclic permutation (FORWRD=.TRUE.)
  ! K = [2, 3, 4, 5, 1] -- rotate rows up
  ! =====================================================
  M = 5
  N = 2

  X(1,1) = 10.0d0; X(2,1) = 20.0d0; X(3,1) = 30.0d0; X(4,1) = 40.0d0; X(5,1) = 50.0d0
  X(1,2) = 11.0d0; X(2,2) = 21.0d0; X(3,2) = 31.0d0; X(4,2) = 41.0d0; X(5,2) = 51.0d0

  K(1) = 2; K(2) = 3; K(3) = 4; K(4) = 5; K(5) = 1

  call DLAPMR(.TRUE., M, N, X, MMAX, K)

  call begin_test('cyclic_fwd_5x2')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('X', X, MMAX*N)
  call end_test()

  ! =====================================================
  ! Test 9: Cyclic permutation (FORWRD=.FALSE.)
  ! K = [2, 3, 4, 5, 1] -- rotate rows
  ! =====================================================
  M = 5
  N = 2

  X(1,1) = 10.0d0; X(2,1) = 20.0d0; X(3,1) = 30.0d0; X(4,1) = 40.0d0; X(5,1) = 50.0d0
  X(1,2) = 11.0d0; X(2,2) = 21.0d0; X(3,2) = 31.0d0; X(4,2) = 41.0d0; X(5,2) = 51.0d0

  K(1) = 2; K(2) = 3; K(3) = 4; K(4) = 5; K(5) = 1

  call DLAPMR(.FALSE., M, N, X, MMAX, K)

  call begin_test('cyclic_bwd_5x2')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('X', X, MMAX*N)
  call end_test()

end program test_dlapmr
