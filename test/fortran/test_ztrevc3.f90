program test_ztrevc3
  use test_utils
  implicit none
  integer, parameter :: NMAX = 4
  complex*16 :: T(NMAX, NMAX), VL(NMAX, NMAX), VR(NMAX, NMAX)
  complex*16 :: WORK(3*NMAX)
  double precision :: RWORK(NMAX)
  double precision :: T_r(2*NMAX*NMAX), VL_r(2*NMAX*NMAX), VR_r(2*NMAX*NMAX)
  logical :: SELECT(NMAX)
  integer :: INFO, M, N, LWORK
  equivalence (T, T_r)
  equivalence (VL, VL_r)
  equivalence (VR, VR_r)

  ! Test 1: SIDE='R', HOWMNY='A' - right eigenvectors, all, 3x3
  N = 3
  LWORK = 3*N
  T = (0.0d0, 0.0d0)
  T(1,1) = (1.0d0, 1.0d0)
  T(1,2) = (0.5d0, 0.0d0)
  T(1,3) = (0.0d0, 0.3d0)
  T(2,2) = (2.0d0, -1.0d0)
  T(2,3) = (0.2d0, 0.1d0)
  T(3,3) = (3.0d0, 0.0d0)
  VR = (0.0d0, 0.0d0)
  M = 0
  call ztrevc3('R', 'A', SELECT, N, T, NMAX, VL, NMAX, VR, NMAX, N, M, &
               WORK, LWORK, RWORK, N, INFO)
  call begin_test('right_all_3x3')
  call print_int('M', M)
  call print_int('INFO', INFO)
  ! Print NxN submatrix of VR using print_matrix with double LDA=2*NMAX
  call print_matrix('VR', VR_r, 2*NMAX, 2*N, N)
  call end_test()

  ! Test 2: SIDE='L', HOWMNY='A' - left eigenvectors, all, 3x3
  T = (0.0d0, 0.0d0)
  T(1,1) = (1.0d0, 1.0d0)
  T(1,2) = (0.5d0, 0.0d0)
  T(1,3) = (0.0d0, 0.3d0)
  T(2,2) = (2.0d0, -1.0d0)
  T(2,3) = (0.2d0, 0.1d0)
  T(3,3) = (3.0d0, 0.0d0)
  VL = (0.0d0, 0.0d0)
  M = 0
  call ztrevc3('L', 'A', SELECT, N, T, NMAX, VL, NMAX, VR, NMAX, N, M, &
               WORK, LWORK, RWORK, N, INFO)
  call begin_test('left_all_3x3')
  call print_int('M', M)
  call print_int('INFO', INFO)
  call print_matrix('VL', VL_r, 2*NMAX, 2*N, N)
  call end_test()

  ! Test 3: SIDE='B', HOWMNY='A' - both eigenvectors, 3x3
  T = (0.0d0, 0.0d0)
  T(1,1) = (1.0d0, 1.0d0)
  T(1,2) = (0.5d0, 0.0d0)
  T(1,3) = (0.0d0, 0.3d0)
  T(2,2) = (2.0d0, -1.0d0)
  T(2,3) = (0.2d0, 0.1d0)
  T(3,3) = (3.0d0, 0.0d0)
  VR = (0.0d0, 0.0d0)
  VL = (0.0d0, 0.0d0)
  M = 0
  call ztrevc3('B', 'A', SELECT, N, T, NMAX, VL, NMAX, VR, NMAX, N, M, &
               WORK, LWORK, RWORK, N, INFO)
  call begin_test('both_all_3x3')
  call print_int('M', M)
  call print_int('INFO', INFO)
  call print_matrix('VR', VR_r, 2*NMAX, 2*N, N)
  call print_matrix('VL', VL_r, 2*NMAX, 2*N, N)
  call end_test()

  ! Test 4: Diagonal matrix
  N = 3
  T = (0.0d0, 0.0d0)
  T(1,1) = (2.0d0, 1.0d0)
  T(2,2) = (3.0d0, -2.0d0)
  T(3,3) = (1.0d0, 0.0d0)
  VR = (0.0d0, 0.0d0)
  M = 0
  call ztrevc3('R', 'A', SELECT, N, T, NMAX, VL, NMAX, VR, NMAX, N, M, &
               WORK, LWORK, RWORK, N, INFO)
  call begin_test('right_all_diag')
  call print_int('M', M)
  call print_int('INFO', INFO)
  call print_matrix('VR', VR_r, 2*NMAX, 2*N, N)
  call end_test()

  ! Test 5: 4x4 matrix
  N = 4
  LWORK = 3*N
  T = (0.0d0, 0.0d0)
  T(1,1) = (1.0d0, 0.0d0)
  T(1,2) = (0.5d0, 0.2d0)
  T(1,3) = (0.1d0, 0.0d0)
  T(1,4) = (0.0d0, 0.3d0)
  T(2,2) = (2.0d0, 1.0d0)
  T(2,3) = (0.3d0, -0.1d0)
  T(2,4) = (0.0d0, 0.0d0)
  T(3,3) = (3.0d0, -1.0d0)
  T(3,4) = (0.4d0, 0.2d0)
  T(4,4) = (4.0d0, 0.0d0)
  VR = (0.0d0, 0.0d0)
  M = 0
  call ztrevc3('R', 'A', SELECT, N, T, NMAX, VL, NMAX, VR, NMAX, N, M, &
               WORK, LWORK, RWORK, N, INFO)
  call begin_test('right_all_4x4')
  call print_int('M', M)
  call print_int('INFO', INFO)
  call print_matrix('VR', VR_r, 2*NMAX, 2*N, N)
  call end_test()

  ! Test 6: N=1
  N = 1
  LWORK = 3
  T = (0.0d0, 0.0d0)
  T(1,1) = (5.0d0, -3.0d0)
  VR = (0.0d0, 0.0d0)
  M = 0
  call ztrevc3('R', 'A', SELECT, N, T, NMAX, VL, NMAX, VR, NMAX, N, M, &
               WORK, LWORK, RWORK, N, INFO)
  call begin_test('right_all_n1')
  call print_int('M', M)
  call print_int('INFO', INFO)
  call print_matrix('VR', VR_r, 2*NMAX, 2*N, N)
  call end_test()

  ! Test 7: Backtransform right (HOWMNY='B') with identity
  N = 3
  LWORK = 3*N
  T = (0.0d0, 0.0d0)
  T(1,1) = (1.0d0, 1.0d0)
  T(1,2) = (0.5d0, 0.0d0)
  T(1,3) = (0.0d0, 0.3d0)
  T(2,2) = (2.0d0, -1.0d0)
  T(2,3) = (0.2d0, 0.1d0)
  T(3,3) = (3.0d0, 0.0d0)
  VR = (0.0d0, 0.0d0)
  VR(1,1) = (1.0d0, 0.0d0)
  VR(2,2) = (1.0d0, 0.0d0)
  VR(3,3) = (1.0d0, 0.0d0)
  M = 0
  call ztrevc3('R', 'B', SELECT, N, T, NMAX, VL, NMAX, VR, NMAX, N, M, &
               WORK, LWORK, RWORK, N, INFO)
  call begin_test('right_backtransform_3x3')
  call print_int('M', M)
  call print_int('INFO', INFO)
  call print_matrix('VR', VR_r, 2*NMAX, 2*N, N)
  call end_test()

  ! Test 8: Backtransform left (HOWMNY='B') with identity
  T = (0.0d0, 0.0d0)
  T(1,1) = (1.0d0, 1.0d0)
  T(1,2) = (0.5d0, 0.0d0)
  T(1,3) = (0.0d0, 0.3d0)
  T(2,2) = (2.0d0, -1.0d0)
  T(2,3) = (0.2d0, 0.1d0)
  T(3,3) = (3.0d0, 0.0d0)
  VL = (0.0d0, 0.0d0)
  VL(1,1) = (1.0d0, 0.0d0)
  VL(2,2) = (1.0d0, 0.0d0)
  VL(3,3) = (1.0d0, 0.0d0)
  M = 0
  call ztrevc3('L', 'B', SELECT, N, T, NMAX, VL, NMAX, VR, NMAX, N, M, &
               WORK, LWORK, RWORK, N, INFO)
  call begin_test('left_backtransform_3x3')
  call print_int('M', M)
  call print_int('INFO', INFO)
  call print_matrix('VL', VL_r, 2*NMAX, 2*N, N)
  call end_test()

end program
