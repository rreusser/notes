program test_zla_syrcond_x
  use test_utils
  implicit none

  integer, parameter :: NMAX = 3
  complex*16 :: A(NMAX, NMAX), AF(NMAX, NMAX), X(NMAX), WORK(2*NMAX)
  double precision :: A_r(2*NMAX*NMAX), AF_r(2*NMAX*NMAX), X_r(2*NMAX)
  equivalence (A, A_r)
  equivalence (AF, AF_r)
  equivalence (X, X_r)
  double precision :: RWORK(NMAX), tmp
  integer :: IPIV(NMAX), INFO, N
  double precision :: ZLA_SYRCOND_X
  external :: ZLA_SYRCOND_X, ZSYTRF

  ! ===== Test 1: UPLO='U', uniform real X =====
  N = 3
  A(1,1) = (2.0d0, 1.0d0); A(1,2) = (1.0d0, 0.0d0); A(1,3) = (0.0d0, -1.0d0)
  A(2,1) = (0.0d0, 0.0d0); A(2,2) = (3.0d0, 0.5d0); A(2,3) = (1.0d0, 1.0d0)
  A(3,1) = (0.0d0, 0.0d0); A(3,2) = (0.0d0, 0.0d0); A(3,3) = (4.0d0, 0.0d0)
  AF = A
  call ZSYTRF('U', N, AF, NMAX, IPIV, WORK, 2*NMAX, INFO)
  X(1) = (1.0d0, 0.0d0); X(2) = (1.0d0, 0.0d0); X(3) = (1.0d0, 0.0d0)

  tmp = ZLA_SYRCOND_X('U', N, A, NMAX, AF, NMAX, IPIV, X, INFO, WORK, RWORK)
  call begin_test('uplo_U_uniform_real')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 2: UPLO='U', complex X =====
  X(1) = (2.0d0, 1.0d0); X(2) = (0.5d0, -0.5d0); X(3) = (3.0d0, 2.0d0)
  tmp = ZLA_SYRCOND_X('U', N, A, NMAX, AF, NMAX, IPIV, X, INFO, WORK, RWORK)
  call begin_test('uplo_U_complex_x')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 3: UPLO='L', uniform real X =====
  ! Build symmetric matrix stored in lower triangle
  A(1,1) = (2.0d0, 1.0d0); A(1,2) = (0.0d0, 0.0d0); A(1,3) = (0.0d0, 0.0d0)
  A(2,1) = (1.0d0, 0.0d0); A(2,2) = (3.0d0, 0.5d0); A(2,3) = (0.0d0, 0.0d0)
  A(3,1) = (0.0d0, -1.0d0); A(3,2) = (1.0d0, 1.0d0); A(3,3) = (4.0d0, 0.0d0)
  AF = A
  call ZSYTRF('L', N, AF, NMAX, IPIV, WORK, 2*NMAX, INFO)
  X(1) = (1.0d0, 0.0d0); X(2) = (1.0d0, 0.0d0); X(3) = (1.0d0, 0.0d0)
  tmp = ZLA_SYRCOND_X('L', N, A, NMAX, AF, NMAX, IPIV, X, INFO, WORK, RWORK)
  call begin_test('uplo_L_uniform_real')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 4: UPLO='L', complex X =====
  X(1) = (2.0d0, 1.0d0); X(2) = (0.5d0, -0.5d0); X(3) = (3.0d0, 2.0d0)
  tmp = ZLA_SYRCOND_X('L', N, A, NMAX, AF, NMAX, IPIV, X, INFO, WORK, RWORK)
  call begin_test('uplo_L_complex_x')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 5: N=1 =====
  N = 1
  A(1,1) = (5.0d0, 2.0d0)
  AF(1,1) = (5.0d0, 2.0d0)
  IPIV(1) = 1
  X(1) = (1.0d0, 1.0d0)
  tmp = ZLA_SYRCOND_X('U', N, A, NMAX, AF, NMAX, IPIV, X, INFO, WORK, RWORK)
  call begin_test('n1_uplo_U')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 6: N=0 =====
  N = 0
  tmp = ZLA_SYRCOND_X('U', N, A, NMAX, AF, NMAX, IPIV, X, INFO, WORK, RWORK)
  call begin_test('n0')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

end program
