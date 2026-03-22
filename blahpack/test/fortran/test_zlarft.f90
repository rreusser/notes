program test_zlarft
  use test_utils
  implicit none

  complex*16 :: V(25), T(9), tau(5)
  double precision :: V_real(50), T_real(18), tau_real(10)
  equivalence (V, V_real)
  equivalence (T, T_real)
  equivalence (tau, tau_real)
  integer :: i

  ! Test 1: Forward, Columnwise, n=4, k=2
  ! V is 4x2 (unit lower triangular in first k rows):
  !   V = [1    0   ]
  !       [v1   1   ]
  !       [v1   v2  ]
  !       [v1   v2  ]
  V = (0.0d0, 0.0d0)
  V(1) = (1.0d0, 0.0d0)  ! V(1,1)
  V(2) = (0.3d0, 0.2d0)  ! V(2,1)
  V(3) = (-0.5d0, 0.1d0) ! V(3,1)
  V(4) = (0.4d0, -0.3d0) ! V(4,1)
  V(5) = (0.0d0, 0.0d0)  ! V(1,2)
  V(6) = (1.0d0, 0.0d0)  ! V(2,2)
  V(7) = (0.6d0, -0.4d0) ! V(3,2)
  V(8) = (-0.2d0, 0.5d0) ! V(4,2)
  tau(1) = (1.2d0, -0.3d0)
  tau(2) = (1.5d0, 0.4d0)
  T = (0.0d0, 0.0d0)
  call zlarft('F', 'C', 4, 2, V, 4, tau, T, 3)
  call begin_test('zlarft_fwd_col')
  call print_array('T', T_real, 12)
  call end_test()

  ! Test 2: Forward, Columnwise, n=5, k=3
  V = (0.0d0, 0.0d0)
  ! Column 1
  V(1) = (1.0d0, 0.0d0)
  V(2) = (0.2d0, 0.1d0)
  V(3) = (-0.3d0, 0.4d0)
  V(4) = (0.5d0, -0.2d0)
  V(5) = (0.1d0, 0.6d0)
  ! Column 2
  V(6) = (0.0d0, 0.0d0)
  V(7) = (1.0d0, 0.0d0)
  V(8) = (0.4d0, -0.5d0)
  V(9) = (-0.1d0, 0.3d0)
  V(10) = (0.7d0, 0.2d0)
  ! Column 3
  V(11) = (0.0d0, 0.0d0)
  V(12) = (0.0d0, 0.0d0)
  V(13) = (1.0d0, 0.0d0)
  V(14) = (0.3d0, 0.1d0)
  V(15) = (-0.2d0, 0.8d0)
  tau(1) = (1.1d0, 0.2d0)
  tau(2) = (1.3d0, -0.1d0)
  tau(3) = (1.6d0, 0.5d0)
  T = (0.0d0, 0.0d0)
  call zlarft('F', 'C', 5, 3, V, 5, tau, T, 3)
  call begin_test('zlarft_fwd_col_5x3')
  call print_array('T', T_real, 18)
  call end_test()

  ! Test 3: tau(1)=0 (first reflector is identity)
  V = (0.0d0, 0.0d0)
  V(1) = (1.0d0, 0.0d0)
  V(2) = (0.3d0, 0.2d0)
  V(3) = (-0.5d0, 0.1d0)
  V(4) = (0.4d0, -0.3d0)
  V(5) = (0.0d0, 0.0d0)
  V(6) = (1.0d0, 0.0d0)
  V(7) = (0.6d0, -0.4d0)
  V(8) = (-0.2d0, 0.5d0)
  tau(1) = (0.0d0, 0.0d0)
  tau(2) = (1.5d0, 0.4d0)
  T = (0.0d0, 0.0d0)
  call zlarft('F', 'C', 4, 2, V, 4, tau, T, 3)
  call begin_test('zlarft_tau_zero')
  call print_array('T', T_real, 12)
  call end_test()

  ! Test 4: Backward, Columnwise, n=4, k=2
  ! For backward:
  !   V = [v1   v2  ]
  !       [v1   v2  ]
  !       [ 1   v2  ]
  !       [ 0    1  ]
  V = (0.0d0, 0.0d0)
  V(1) = (0.3d0, 0.2d0)  ! V(1,1)
  V(2) = (-0.5d0, 0.1d0) ! V(2,1)
  V(3) = (1.0d0, 0.0d0)  ! V(3,1)
  V(4) = (0.0d0, 0.0d0)  ! V(4,1)
  V(5) = (0.6d0, -0.4d0) ! V(1,2)
  V(6) = (-0.2d0, 0.5d0) ! V(2,2)
  V(7) = (0.4d0, -0.3d0) ! V(3,2)
  V(8) = (1.0d0, 0.0d0)  ! V(4,2)
  tau(1) = (1.2d0, -0.3d0)
  tau(2) = (1.5d0, 0.4d0)
  T = (0.0d0, 0.0d0)
  call zlarft('B', 'C', 4, 2, V, 4, tau, T, 3)
  call begin_test('zlarft_bwd_col')
  call print_array('T', T_real, 12)
  call end_test()

  ! Test 5: Forward, Rowwise, n=4, k=2
  ! V is 2x4 (unit upper triangular in first k columns):
  !   V = [ 1   v1  v1  v1 ]
  !       [ 0    1  v2  v2 ]
  ! Stored col-major in memory with LDV=2
  V = (0.0d0, 0.0d0)
  V(1) = (1.0d0, 0.0d0)  ! V(1,1)
  V(2) = (0.0d0, 0.0d0)  ! V(2,1)
  V(3) = (0.3d0, 0.2d0)  ! V(1,2)
  V(4) = (1.0d0, 0.0d0)  ! V(2,2)
  V(5) = (-0.5d0, 0.1d0) ! V(1,3)
  V(6) = (0.6d0, -0.4d0) ! V(2,3)
  V(7) = (0.4d0, -0.3d0) ! V(1,4)
  V(8) = (-0.2d0, 0.5d0) ! V(2,4)
  tau(1) = (1.2d0, -0.3d0)
  tau(2) = (1.5d0, 0.4d0)
  T = (0.0d0, 0.0d0)
  call zlarft('F', 'R', 4, 2, V, 2, tau, T, 3)
  call begin_test('zlarft_fwd_row')
  call print_array('T', T_real, 12)
  call end_test()

  ! Test 6: Backward, Rowwise, n=4, k=2
  ! V is 2x4 (unit lower triangular in last k columns):
  !   V = [ v1  v1  1   0 ]
  !       [ v2  v2  v2  1 ]
  V = (0.0d0, 0.0d0)
  V(1) = (0.3d0, 0.2d0)   ! V(1,1)
  V(2) = (0.6d0, -0.4d0)  ! V(2,1)
  V(3) = (-0.5d0, 0.1d0)  ! V(1,2)
  V(4) = (-0.2d0, 0.5d0)  ! V(2,2)
  V(5) = (1.0d0, 0.0d0)   ! V(1,3)
  V(6) = (0.4d0, -0.3d0)  ! V(2,3)
  V(7) = (0.0d0, 0.0d0)   ! V(1,4)
  V(8) = (1.0d0, 0.0d0)   ! V(2,4)
  tau(1) = (1.2d0, -0.3d0)
  tau(2) = (1.5d0, 0.4d0)
  T = (0.0d0, 0.0d0)
  call zlarft('B', 'R', 4, 2, V, 2, tau, T, 3)
  call begin_test('zlarft_bwd_row')
  call print_array('T', T_real, 12)
  call end_test()

end program
