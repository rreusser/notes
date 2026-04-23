program test_zherfs
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4, LWMAX = 256
  complex*16 :: A(NMAX, NMAX), AF(NMAX, NMAX), B(NMAX, 2), X(NMAX, 2)
  complex*16 :: WORK(LWMAX)
  double precision :: FERR(2), BERR(2), RWORK(NMAX)
  double precision :: X_r(2*NMAX*2)
  equivalence (X, X_r)
  integer :: IPIV(NMAX), INFO, n, nrhs

  ! Test 1: Upper, Hermitian, 4x4, 1 RHS
  n = 4
  nrhs = 1
  A = (0.0d0, 0.0d0)
  ! Hermitian: diag is real, A(i,j) = conj(A(j,i))
  A(1,1) = (4.0d0, 0.0d0)
  A(1,2) = (1.0d0, 2.0d0)
  A(1,3) = (3.0d0, -1.0d0)
  A(1,4) = (0.5d0, 0.5d0)
  A(2,2) = (6.0d0, 0.0d0)
  A(2,3) = (2.0d0, 1.0d0)
  A(2,4) = (1.0d0, -2.0d0)
  A(3,3) = (5.0d0, 0.0d0)
  A(3,4) = (3.0d0, 0.5d0)
  A(4,4) = (7.0d0, 0.0d0)
  AF = A
  call ZHETRF('U', n, AF, NMAX, IPIV, WORK, LWMAX, INFO)
  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (2.0d0, 1.0d0)
  B(3,1) = (-1.0d0, 3.0d0)
  B(4,1) = (0.5d0, -0.5d0)
  X(:,1) = B(:,1)
  call ZHETRS('U', n, nrhs, AF, NMAX, IPIV, X, NMAX, INFO)
  call ZHERFS('U', n, nrhs, A, NMAX, AF, NMAX, IPIV, B, NMAX, X, NMAX, &
              FERR, BERR, WORK, RWORK, INFO)
  call begin_test('upper_4x4')
  call print_int('info', INFO)
  call print_array('X', X_r, 2*n)
  call print_array('ferr', FERR, 1)
  call print_array('berr', BERR, 1)
  call end_test()

  ! Test 2: Lower, Hermitian, 4x4, 1 RHS
  n = 4
  nrhs = 1
  A = (0.0d0, 0.0d0)
  A(1,1) = (4.0d0, 0.0d0)
  A(2,1) = (1.0d0, -2.0d0)
  A(2,2) = (6.0d0, 0.0d0)
  A(3,1) = (3.0d0, 1.0d0)
  A(3,2) = (2.0d0, -1.0d0)
  A(3,3) = (5.0d0, 0.0d0)
  A(4,1) = (0.5d0, -0.5d0)
  A(4,2) = (1.0d0, 2.0d0)
  A(4,3) = (3.0d0, -0.5d0)
  A(4,4) = (7.0d0, 0.0d0)
  AF = A
  call ZHETRF('L', n, AF, NMAX, IPIV, WORK, LWMAX, INFO)
  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (2.0d0, 1.0d0)
  B(3,1) = (-1.0d0, 3.0d0)
  B(4,1) = (0.5d0, -0.5d0)
  X(:,1) = B(:,1)
  call ZHETRS('L', n, nrhs, AF, NMAX, IPIV, X, NMAX, INFO)
  call ZHERFS('L', n, nrhs, A, NMAX, AF, NMAX, IPIV, B, NMAX, X, NMAX, &
              FERR, BERR, WORK, RWORK, INFO)
  call begin_test('lower_4x4')
  call print_int('info', INFO)
  call print_array('X', X_r, 2*n)
  call print_array('ferr', FERR, 1)
  call print_array('berr', BERR, 1)
  call end_test()

  ! Test 3: Upper, 4x4, 2 RHS
  n = 4
  nrhs = 2
  A = (0.0d0, 0.0d0)
  A(1,1) = (4.0d0, 0.0d0)
  A(1,2) = (1.0d0, 2.0d0)
  A(1,3) = (3.0d0, -1.0d0)
  A(1,4) = (0.5d0, 0.5d0)
  A(2,2) = (6.0d0, 0.0d0)
  A(2,3) = (2.0d0, 1.0d0)
  A(2,4) = (1.0d0, -2.0d0)
  A(3,3) = (5.0d0, 0.0d0)
  A(3,4) = (3.0d0, 0.5d0)
  A(4,4) = (7.0d0, 0.0d0)
  AF = A
  call ZHETRF('U', n, AF, NMAX, IPIV, WORK, LWMAX, INFO)
  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (2.0d0, 1.0d0)
  B(3,1) = (-1.0d0, 3.0d0)
  B(4,1) = (0.5d0, -0.5d0)
  B(1,2) = (0.0d0, 1.0d0)
  B(2,2) = (1.0d0, 0.0d0)
  B(3,2) = (2.0d0, -1.0d0)
  B(4,2) = (-1.0d0, 2.0d0)
  X(:,1) = B(:,1)
  X(:,2) = B(:,2)
  call ZHETRS('U', n, nrhs, AF, NMAX, IPIV, X, NMAX, INFO)
  call ZHERFS('U', n, nrhs, A, NMAX, AF, NMAX, IPIV, B, NMAX, X, NMAX, &
              FERR, BERR, WORK, RWORK, INFO)
  call begin_test('upper_4x4_2rhs')
  call print_int('info', INFO)
  call print_array('X', X_r, 2*NMAX*nrhs)
  call print_array('ferr', FERR, nrhs)
  call print_array('berr', BERR, nrhs)
  call end_test()

  ! Test 4: N=0
  call ZHERFS('U', 0, 1, A, NMAX, AF, NMAX, IPIV, B, NMAX, X, NMAX, &
              FERR, BERR, WORK, RWORK, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call print_array('ferr', FERR, 1)
  call print_array('berr', BERR, 1)
  call end_test()

  ! Test 5: N=1
  n = 1
  nrhs = 1
  A(1,1) = (5.0d0, 0.0d0)
  AF(1,1) = (5.0d0, 0.0d0)
  IPIV(1) = 1
  B(1,1) = (10.0d0, 5.0d0)
  X(1,1) = (2.0d0, 1.0d0)
  call ZHERFS('U', n, nrhs, A, NMAX, AF, NMAX, IPIV, B, NMAX, X, NMAX, &
              FERR, BERR, WORK, RWORK, INFO)
  call begin_test('n1')
  call print_int('info', INFO)
  call print_array('X', X_r, 2*n)
  call print_array('ferr', FERR, 1)
  call print_array('berr', BERR, 1)
  call end_test()

end program
