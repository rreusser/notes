program test_dlarft
  use test_utils
  implicit none

  double precision :: V(6, 3), TAU(3), T(3, 3)

  ! Test 1: Forward, Columnwise, 5x3
  V = 0.0d0
  V(1,1) = 1.0d0
  V(2,1) = 0.5d0; V(2,2) = 1.0d0
  V(3,1) = 0.25d0; V(3,2) = 0.5d0; V(3,3) = 1.0d0
  V(4,1) = 0.125d0; V(4,2) = 0.25d0; V(4,3) = 0.5d0
  V(5,1) = 0.0625d0; V(5,2) = 0.125d0; V(5,3) = 0.25d0

  TAU(1) = 1.2d0; TAU(2) = 1.5d0; TAU(3) = 1.1d0
  T = 0.0d0
  call DLARFT('F', 'C', 5, 3, V, 6, TAU, T, 3)
  call begin_test('fwd_col_5x3')
  call print_matrix('T', T, 3, 3, 3)
  call end_test()

  ! Test 2: Forward, Columnwise, 3x2
  V = 0.0d0
  V(1,1) = 1.0d0
  V(2,1) = 2.0d0; V(2,2) = 1.0d0
  V(3,1) = 3.0d0; V(3,2) = 4.0d0

  TAU(1) = 0.8d0; TAU(2) = 1.2d0
  T = 0.0d0
  call DLARFT('F', 'C', 3, 2, V, 6, TAU, T, 3)
  call begin_test('fwd_col_3x2')
  call print_matrix('T', T, 3, 2, 2)
  call end_test()

  ! Test 3: Backward, Columnwise, 5x2
  V = 0.0d0
  V(1,1) = 0.5d0; V(1,2) = 0.25d0
  V(2,1) = 0.25d0; V(2,2) = 0.125d0
  V(3,1) = 0.125d0; V(3,2) = 0.0625d0
  V(4,1) = 1.0d0; V(4,2) = 0.0d0
  V(5,1) = 0.0d0; V(5,2) = 1.0d0

  TAU(1) = 1.5d0; TAU(2) = 0.9d0
  T = 0.0d0
  call DLARFT('B', 'C', 5, 2, V, 6, TAU, T, 3)
  call begin_test('bwd_col_5x2')
  call print_matrix('T', T, 3, 2, 2)
  call end_test()

  ! Test 4: tau=0 for one reflector
  V = 0.0d0
  V(1,1) = 1.0d0
  V(2,1) = 0.5d0; V(2,2) = 1.0d0
  V(3,1) = 0.25d0; V(3,2) = 0.5d0

  TAU(1) = 1.2d0; TAU(2) = 0.0d0
  T = 0.0d0
  call DLARFT('F', 'C', 3, 2, V, 6, TAU, T, 3)
  call begin_test('fwd_col_tau_zero')
  call print_matrix('T', T, 3, 2, 2)
  call end_test()

end program
