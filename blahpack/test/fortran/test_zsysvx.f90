program test_zsysvx
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4, LWMAX = 256
  complex*16 :: A(NMAX, NMAX), AF(NMAX, NMAX), B(NMAX, 2), X(NMAX, 2)
  complex*16 :: WORK(LWMAX)
  double precision :: FERR(2), BERR(2), RWORK(NMAX), RCOND
  double precision :: X_r(2*NMAX*2)
  equivalence (X, X_r)
  integer :: IPIV(NMAX), INFO, n, nrhs

  ! Test 1: Not-factored, upper, 4x4, 1 RHS
  n = 4
  nrhs = 1
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
  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (2.0d0, 1.0d0)
  B(3,1) = (-1.0d0, 3.0d0)
  B(4,1) = (0.5d0, -0.5d0)
  call ZSYSVX('N', 'U', n, nrhs, A, NMAX, AF, NMAX, IPIV, B, NMAX, &
               X, NMAX, RCOND, FERR, BERR, WORK, LWMAX, RWORK, INFO)
  call begin_test('upper_4x4_1rhs')
  call print_int('n', n)
  call print_int('info', INFO)
  call print_array('X', X_r, 2*n*nrhs)
  call print_scalar('rcond', RCOND)
  call print_array('ferr', FERR, nrhs)
  call print_array('berr', BERR, nrhs)
  call end_test()

  ! Test 2: Not-factored, lower, 4x4, 2 RHS
  n = 4
  nrhs = 2
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
  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (2.0d0, 1.0d0)
  B(3,1) = (-1.0d0, 3.0d0)
  B(4,1) = (0.5d0, -0.5d0)
  B(1,2) = (0.0d0, 1.0d0)
  B(2,2) = (1.0d0, 0.0d0)
  B(3,2) = (2.0d0, -1.0d0)
  B(4,2) = (-1.0d0, 2.0d0)
  call ZSYSVX('N', 'L', n, nrhs, A, NMAX, AF, NMAX, IPIV, B, NMAX, &
               X, NMAX, RCOND, FERR, BERR, WORK, LWMAX, RWORK, INFO)
  call begin_test('lower_4x4_2rhs')
  call print_int('n', n)
  call print_int('info', INFO)
  call print_array('X', X_r, 2*NMAX*nrhs)
  call print_scalar('rcond', RCOND)
  call print_array('ferr', FERR, nrhs)
  call print_array('berr', BERR, nrhs)
  call end_test()

  ! Test 3: N=0
  call ZSYSVX('N', 'U', 0, 1, A, NMAX, AF, NMAX, IPIV, B, NMAX, &
               X, NMAX, RCOND, FERR, BERR, WORK, LWMAX, RWORK, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call end_test()

end program
