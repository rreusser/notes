program test_zsyrfs
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4, LWMAX = 256
  complex*16 :: A(NMAX, NMAX), AF(NMAX, NMAX), B(NMAX, 1), X(NMAX, 1)
  complex*16 :: WORK(LWMAX)
  double precision :: FERR(1), BERR(1), RWORK(NMAX)
  double precision :: X_r(2*NMAX)
  equivalence (X, X_r)
  integer :: IPIV(NMAX), INFO, n, nrhs

  ! Test 1: Upper, 4x4 symmetric (not Hermitian), 1 RHS
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
  AF = A
  call ZSYTRF('U', n, AF, NMAX, IPIV, WORK, LWMAX, INFO)
  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (2.0d0, 1.0d0)
  B(3,1) = (-1.0d0, 3.0d0)
  B(4,1) = (0.5d0, -0.5d0)
  X = B
  call ZSYTRS('U', n, nrhs, AF, NMAX, IPIV, X, NMAX, INFO)
  call ZSYRFS('U', n, nrhs, A, NMAX, AF, NMAX, IPIV, B, NMAX, X, NMAX, &
              FERR, BERR, WORK, RWORK, INFO)
  call begin_test('upper_4x4')
  call print_int('info', INFO)
  call print_array('X', X_r, 2*n)
  call print_array('ferr', FERR, 1)
  call print_array('berr', BERR, 1)
  call end_test()

end program
