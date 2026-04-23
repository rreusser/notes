program test_dlarfy
  use test_utils
  implicit none

  double precision :: C(5, 5), v(5), WORK(5), tau

  ! Test 1: Upper, 3x3, tau=1.0
  C = 0.0d0
  C(1,1) = 4.0d0; C(1,2) = 1.0d0; C(1,3) = 2.0d0
  C(2,1) = 1.0d0; C(2,2) = 5.0d0; C(2,3) = 3.0d0
  C(3,1) = 2.0d0; C(3,2) = 3.0d0; C(3,3) = 6.0d0
  v(1) = 1.0d0; v(2) = 0.5d0; v(3) = 0.25d0
  tau = 1.0d0
  call DLARFY('U', 3, v, 1, tau, C, 5, WORK)
  call begin_test('upper_3x3_tau1')
  call print_matrix('C', C, 5, 3, 3)
  call end_test()

  ! Test 2: Lower, 3x3, tau=1.0
  C = 0.0d0
  C(1,1) = 4.0d0; C(1,2) = 1.0d0; C(1,3) = 2.0d0
  C(2,1) = 1.0d0; C(2,2) = 5.0d0; C(2,3) = 3.0d0
  C(3,1) = 2.0d0; C(3,2) = 3.0d0; C(3,3) = 6.0d0
  v(1) = 1.0d0; v(2) = 0.5d0; v(3) = 0.25d0
  tau = 1.0d0
  call DLARFY('L', 3, v, 1, tau, C, 5, WORK)
  call begin_test('lower_3x3_tau1')
  call print_matrix('C', C, 5, 3, 3)
  call end_test()

  ! Test 3: tau=0 quick return
  C = 0.0d0
  C(1,1) = 4.0d0; C(1,2) = 1.0d0; C(1,3) = 2.0d0
  C(2,1) = 1.0d0; C(2,2) = 5.0d0; C(2,3) = 3.0d0
  C(3,1) = 2.0d0; C(3,2) = 3.0d0; C(3,3) = 6.0d0
  v(1) = 1.0d0; v(2) = 0.5d0; v(3) = 0.25d0
  tau = 0.0d0
  call DLARFY('U', 3, v, 1, tau, C, 5, WORK)
  call begin_test('tau_zero')
  call print_matrix('C', C, 5, 3, 3)
  call end_test()

  ! Test 4: Upper, N=2
  C = 0.0d0
  C(1,1) = 2.0d0; C(1,2) = 1.0d0
  C(2,1) = 1.0d0; C(2,2) = 3.0d0
  v(1) = 1.0d0; v(2) = 2.0d0
  tau = 0.5d0
  call DLARFY('U', 2, v, 1, tau, C, 5, WORK)
  call begin_test('upper_2x2')
  call print_matrix('C', C, 5, 2, 2)
  call end_test()

  ! Test 5: Lower, N=5 with tau=0.7
  C = 0.0d0
  C(1,1) = 10.0d0; C(1,2) = 1.0d0; C(1,3) = 2.0d0; C(1,4) = 3.0d0; C(1,5) = 4.0d0
  C(2,1) = 1.0d0;  C(2,2) = 11.0d0; C(2,3) = 5.0d0; C(2,4) = 6.0d0; C(2,5) = 7.0d0
  C(3,1) = 2.0d0;  C(3,2) = 5.0d0; C(3,3) = 12.0d0; C(3,4) = 8.0d0; C(3,5) = 9.0d0
  C(4,1) = 3.0d0;  C(4,2) = 6.0d0; C(4,3) = 8.0d0; C(4,4) = 13.0d0; C(4,5) = 1.5d0
  C(5,1) = 4.0d0;  C(5,2) = 7.0d0; C(5,3) = 9.0d0; C(5,4) = 1.5d0; C(5,5) = 14.0d0
  v(1) = 1.0d0; v(2) = 0.4d0; v(3) = 0.3d0; v(4) = 0.2d0; v(5) = 0.1d0
  tau = 0.7d0
  call DLARFY('L', 5, v, 1, tau, C, 5, WORK)
  call begin_test('lower_5x5')
  call print_matrix('C', C, 5, 5, 5)
  call end_test()

  ! Test 6: N=1 edge case
  C = 0.0d0
  C(1,1) = 5.0d0
  v(1) = 1.0d0
  tau = 1.0d0
  call DLARFY('U', 1, v, 1, tau, C, 5, WORK)
  call begin_test('n1')
  call print_matrix('C', C, 5, 1, 1)
  call end_test()

end program
