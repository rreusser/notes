program test_zsymv
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4
  complex*16 :: A(NMAX, NMAX), x(NMAX), y(NMAX), alpha, beta
  double precision :: y_r(2*NMAX)
  equivalence (y, y_r)
  integer :: n

  ! Test 1: Upper, alpha=1, beta=0
  n = 4
  alpha = (1.0d0, 0.0d0)
  beta = (0.0d0, 0.0d0)
  A = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 1.0d0)
  A(1,2) = (1.0d0, 2.0d0)
  A(1,3) = (3.0d0, -1.0d0)
  A(1,4) = (0.5d0, 0.5d0)
  A(2,2) = (5.0d0, -1.0d0)
  A(2,3) = (2.0d0, 1.0d0)
  A(2,4) = (1.0d0, -2.0d0)
  A(3,3) = (4.0d0, 2.0d0)
  A(3,4) = (3.0d0, 0.0d0)
  A(4,4) = (6.0d0, -3.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (0.0d0, 1.0d0)
  x(3) = (2.0d0, -1.0d0)
  x(4) = (1.0d0, 1.0d0)
  y = (0.0d0, 0.0d0)
  call ZSYMV('U', n, alpha, A, NMAX, x, 1, beta, y, 1)
  call begin_test('upper_a1_b0')
  call print_int('n', n)
  call print_array('y', y_r, 2*n)
  call end_test()

  ! Test 2: Lower, alpha=(2,1), beta=(0.5,0)
  alpha = (2.0d0, 1.0d0)
  beta = (0.5d0, 0.0d0)
  A = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 1.0d0)
  A(2,1) = (1.0d0, 2.0d0)
  A(2,2) = (5.0d0, -1.0d0)
  A(3,1) = (3.0d0, -1.0d0)
  A(3,2) = (2.0d0, 1.0d0)
  A(3,3) = (4.0d0, 2.0d0)
  A(4,1) = (0.5d0, 0.5d0)
  A(4,2) = (1.0d0, -2.0d0)
  A(4,3) = (3.0d0, 0.0d0)
  A(4,4) = (6.0d0, -3.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (0.0d0, 1.0d0)
  x(3) = (2.0d0, -1.0d0)
  x(4) = (1.0d0, 1.0d0)
  ! y still has result from test 1
  call ZSYMV('L', n, alpha, A, NMAX, x, 1, beta, y, 1)
  call begin_test('lower_a2_b05')
  call print_int('n', n)
  call print_array('y', y_r, 2*n)
  call end_test()

  ! Test 3: N=0
  y = (99.0d0, 99.0d0)
  call ZSYMV('U', 0, alpha, A, NMAX, x, 1, beta, y, 1)
  call begin_test('n0')
  call print_array('y', y_r, 2)
  call end_test()

  ! Test 4: alpha=0, beta=1 (quick return)
  alpha = (0.0d0, 0.0d0)
  beta = (1.0d0, 0.0d0)
  y(1) = (7.0d0, 3.0d0)
  y(2) = (2.0d0, 5.0d0)
  call ZSYMV('U', 2, alpha, A, NMAX, x, 1, beta, y, 1)
  call begin_test('a0_b1')
  call print_array('y', y_r, 4)
  call end_test()

end program
