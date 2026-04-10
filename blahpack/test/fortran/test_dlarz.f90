program test_dlarz
  use test_utils
  implicit none

  double precision :: C(6, 6), v(4), WORK(6), tau

  ! Test 1: Left, M=4, N=4, L=2
  C = 0.0d0
  C(1,1) = 1.0d0; C(1,2) = 2.0d0; C(1,3) = 3.0d0; C(1,4) = 4.0d0
  C(2,1) = 5.0d0; C(2,2) = 6.0d0; C(2,3) = 7.0d0; C(2,4) = 8.0d0
  C(3,1) = 9.0d0; C(3,2) = 10.0d0; C(3,3) = 11.0d0; C(3,4) = 12.0d0
  C(4,1) = 13.0d0; C(4,2) = 14.0d0; C(4,3) = 15.0d0; C(4,4) = 16.0d0
  v(1) = 0.5d0; v(2) = 0.25d0
  tau = 1.5d0
  call DLARZ('L', 4, 4, 2, v, 1, tau, C, 6, WORK)
  call begin_test('left_4x4_l2')
  call print_matrix('C', C, 6, 4, 4)
  call end_test()

  ! Test 2: Right, M=4, N=4, L=2
  C = 0.0d0
  C(1,1) = 1.0d0; C(1,2) = 2.0d0; C(1,3) = 3.0d0; C(1,4) = 4.0d0
  C(2,1) = 5.0d0; C(2,2) = 6.0d0; C(2,3) = 7.0d0; C(2,4) = 8.0d0
  C(3,1) = 9.0d0; C(3,2) = 10.0d0; C(3,3) = 11.0d0; C(3,4) = 12.0d0
  C(4,1) = 13.0d0; C(4,2) = 14.0d0; C(4,3) = 15.0d0; C(4,4) = 16.0d0
  v(1) = 0.5d0; v(2) = 0.25d0
  tau = 1.5d0
  call DLARZ('R', 4, 4, 2, v, 1, tau, C, 6, WORK)
  call begin_test('right_4x4_l2')
  call print_matrix('C', C, 6, 4, 4)
  call end_test()

  ! Test 3: tau = 0
  C = 0.0d0
  C(1,1) = 1.0d0; C(1,2) = 2.0d0; C(1,3) = 3.0d0
  C(2,1) = 4.0d0; C(2,2) = 5.0d0; C(2,3) = 6.0d0
  C(3,1) = 7.0d0; C(3,2) = 8.0d0; C(3,3) = 9.0d0
  v(1) = 1.0d0
  tau = 0.0d0
  call DLARZ('L', 3, 3, 1, v, 1, tau, C, 6, WORK)
  call begin_test('tau_zero')
  call print_matrix('C', C, 6, 3, 3)
  call end_test()

  ! Test 4: L = 0 (degenerate, still does DCOPY/DAXPY)
  C = 0.0d0
  C(1,1) = 1.0d0; C(1,2) = 2.0d0; C(1,3) = 3.0d0
  C(2,1) = 4.0d0; C(2,2) = 5.0d0; C(2,3) = 6.0d0
  C(3,1) = 7.0d0; C(3,2) = 8.0d0; C(3,3) = 9.0d0
  v(1) = 0.0d0
  tau = 0.5d0
  call DLARZ('L', 3, 3, 0, v, 1, tau, C, 6, WORK)
  call begin_test('left_l0')
  call print_matrix('C', C, 6, 3, 3)
  call end_test()

  ! Test 5: Left with M=5, N=3, L=3
  C = 0.0d0
  C(1,1) = 1.0d0; C(1,2) = 2.0d0; C(1,3) = 3.0d0
  C(2,1) = 4.0d0; C(2,2) = 5.0d0; C(2,3) = 6.0d0
  C(3,1) = 7.0d0; C(3,2) = 8.0d0; C(3,3) = 9.0d0
  C(4,1) = 10.0d0; C(4,2) = 11.0d0; C(4,3) = 12.0d0
  C(5,1) = 13.0d0; C(5,2) = 14.0d0; C(5,3) = 15.0d0
  v(1) = 0.1d0; v(2) = 0.2d0; v(3) = 0.3d0
  tau = 2.0d0
  call DLARZ('L', 5, 3, 3, v, 1, tau, C, 6, WORK)
  call begin_test('left_5x3_l3')
  call print_matrix('C', C, 6, 5, 3)
  call end_test()

  ! Test 6: Right with M=3, N=5, L=3, negative v stride
  C = 0.0d0
  C(1,1) = 1.0d0; C(1,2) = 2.0d0; C(1,3) = 3.0d0; C(1,4) = 4.0d0; C(1,5) = 5.0d0
  C(2,1) = 6.0d0; C(2,2) = 7.0d0; C(2,3) = 8.0d0; C(2,4) = 9.0d0; C(2,5) = 10.0d0
  C(3,1) = 11.0d0; C(3,2) = 12.0d0; C(3,3) = 13.0d0; C(3,4) = 14.0d0; C(3,5) = 15.0d0
  v(1) = 0.3d0; v(2) = 0.2d0; v(3) = 0.1d0
  tau = -0.5d0
  call DLARZ('R', 3, 5, 3, v, 1, tau, C, 6, WORK)
  call begin_test('right_3x5_l3')
  call print_matrix('C', C, 6, 3, 5)
  call end_test()

end program
