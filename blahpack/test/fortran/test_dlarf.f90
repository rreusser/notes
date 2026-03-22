program test_dlarf
  use test_utils
  implicit none

  double precision :: C(4, 4), v(4), WORK(4), tau

  ! Test 1: Left, 3x3 matrix
  C = 0.0d0
  C(1,1) = 1.0d0; C(1,2) = 2.0d0; C(1,3) = 3.0d0
  C(2,1) = 4.0d0; C(2,2) = 5.0d0; C(2,3) = 6.0d0
  C(3,1) = 7.0d0; C(3,2) = 8.0d0; C(3,3) = 9.0d0
  v(1) = 1.0d0; v(2) = 0.5d0; v(3) = 0.25d0
  tau = 1.5d0
  call DLARF('L', 3, 3, v, 1, tau, C, 4, WORK)
  call begin_test('left_3x3')
  call print_matrix('C', C, 4, 3, 3)
  call end_test()

  ! Test 2: Right, 3x3 matrix
  C = 0.0d0
  C(1,1) = 1.0d0; C(1,2) = 2.0d0; C(1,3) = 3.0d0
  C(2,1) = 4.0d0; C(2,2) = 5.0d0; C(2,3) = 6.0d0
  C(3,1) = 7.0d0; C(3,2) = 8.0d0; C(3,3) = 9.0d0
  v(1) = 1.0d0; v(2) = 0.5d0; v(3) = 0.25d0
  tau = 1.5d0
  call DLARF('R', 3, 3, v, 1, tau, C, 4, WORK)
  call begin_test('right_3x3')
  call print_matrix('C', C, 4, 3, 3)
  call end_test()

  ! Test 3: tau=0 (identity)
  C = 0.0d0
  C(1,1) = 1.0d0; C(1,2) = 2.0d0
  C(2,1) = 3.0d0; C(2,2) = 4.0d0
  v(1) = 1.0d0; v(2) = 0.5d0
  tau = 0.0d0
  call DLARF('L', 2, 2, v, 1, tau, C, 4, WORK)
  call begin_test('tau_zero')
  call print_matrix('C', C, 4, 2, 2)
  call end_test()

  ! Test 4: Left, 2x3 matrix
  C = 0.0d0
  C(1,1) = 1.0d0; C(1,2) = 2.0d0; C(1,3) = 3.0d0
  C(2,1) = 4.0d0; C(2,2) = 5.0d0; C(2,3) = 6.0d0
  v(1) = 1.0d0; v(2) = 2.0d0
  tau = 0.8d0
  call DLARF('L', 2, 3, v, 1, tau, C, 4, WORK)
  call begin_test('left_2x3')
  call print_matrix('C', C, 4, 2, 3)
  call end_test()

end program
