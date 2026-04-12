program test_zla_gercond_x
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
  double precision :: ZLA_GERCOND_X
  external :: ZLA_GERCOND_X, ZGETRF

  ! ===== Test 1: trans='N', X = (1,0) in all entries =====
  N = 3
  A(1,1) = (2.0d0, 1.0d0); A(1,2) = (1.0d0, 0.0d0); A(1,3) = (0.0d0, -1.0d0)
  A(2,1) = (1.0d0, -1.0d0); A(2,2) = (3.0d0, 0.0d0); A(2,3) = (1.0d0, 1.0d0)
  A(3,1) = (0.0d0, 1.0d0); A(3,2) = (1.0d0, -0.5d0); A(3,3) = (4.0d0, 0.0d0)
  AF = A
  call ZGETRF(N, N, AF, NMAX, IPIV, INFO)
  X(1) = (1.0d0, 0.0d0); X(2) = (1.0d0, 0.0d0); X(3) = (1.0d0, 0.0d0)

  tmp = ZLA_GERCOND_X('N', N, A, NMAX, AF, NMAX, IPIV, X, INFO, WORK, RWORK)
  call begin_test('trans_N_uniform_real')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 2: trans='C', X = (1,0) in all entries =====
  tmp = ZLA_GERCOND_X('C', N, A, NMAX, AF, NMAX, IPIV, X, INFO, WORK, RWORK)
  call begin_test('trans_C_uniform_real')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 3: trans='N', complex X =====
  X(1) = (2.0d0, 1.0d0); X(2) = (0.5d0, -0.5d0); X(3) = (3.0d0, 2.0d0)
  tmp = ZLA_GERCOND_X('N', N, A, NMAX, AF, NMAX, IPIV, X, INFO, WORK, RWORK)
  call begin_test('trans_N_complex_x')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 4: trans='C', complex X =====
  tmp = ZLA_GERCOND_X('C', N, A, NMAX, AF, NMAX, IPIV, X, INFO, WORK, RWORK)
  call begin_test('trans_C_complex_x')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 5: N=1 =====
  N = 1
  A(1,1) = (5.0d0, 2.0d0)
  AF(1,1) = (5.0d0, 2.0d0)
  IPIV(1) = 1
  X(1) = (1.0d0, 1.0d0)
  tmp = ZLA_GERCOND_X('N', N, A, NMAX, AF, NMAX, IPIV, X, INFO, WORK, RWORK)
  call begin_test('n1_trans_N')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 6: N=0 =====
  N = 0
  tmp = ZLA_GERCOND_X('N', N, A, NMAX, AF, NMAX, IPIV, X, INFO, WORK, RWORK)
  call begin_test('n0')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

end program
