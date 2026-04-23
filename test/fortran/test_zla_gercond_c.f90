program test_zla_gercond_c
  use test_utils
  implicit none

  integer, parameter :: NMAX = 3
  complex*16 :: A(NMAX, NMAX), AF(NMAX, NMAX), WORK(2*NMAX)
  double precision :: A_r(2*NMAX*NMAX), AF_r(2*NMAX*NMAX)
  double precision :: C(NMAX), RWORK(NMAX), tmp
  integer :: IPIV(NMAX), INFO, N
  double precision :: ZLA_GERCOND_C
  external :: ZLA_GERCOND_C, ZGETRF
  equivalence (A, A_r)
  equivalence (AF, AF_r)

  ! ===== Setup 3x3 complex matrix =====
  N = 3
  A(1,1) = (2.0d0, 1.0d0); A(1,2) = (1.0d0, 0.0d0); A(1,3) = (0.0d0, -1.0d0)
  A(2,1) = (1.0d0, 2.0d0); A(2,2) = (3.0d0, 0.0d0); A(2,3) = (1.0d0, 1.0d0)
  A(3,1) = (0.0d0, 1.0d0); A(3,2) = (1.0d0, -1.0d0); A(3,3) = (4.0d0, 0.0d0)
  AF = A
  call ZGETRF(N, N, AF, NMAX, IPIV, INFO)
  C(1) = 1.0d0; C(2) = 1.0d0; C(3) = 1.0d0

  ! ===== Test 1: trans='N', CAPPLY=.true., uniform C =====
  tmp = ZLA_GERCOND_C('N', N, A, NMAX, AF, NMAX, IPIV, C, .true., INFO, WORK, RWORK)
  call begin_test('trans_N_capply_uniform')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 2: trans='C', CAPPLY=.true., uniform C =====
  tmp = ZLA_GERCOND_C('C', N, A, NMAX, AF, NMAX, IPIV, C, .true., INFO, WORK, RWORK)
  call begin_test('trans_C_capply_uniform')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 3: trans='N', CAPPLY=.true., non-uniform C =====
  C(1) = 2.0d0; C(2) = 0.5d0; C(3) = 3.0d0
  tmp = ZLA_GERCOND_C('N', N, A, NMAX, AF, NMAX, IPIV, C, .true., INFO, WORK, RWORK)
  call begin_test('trans_N_capply_nonuniform')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 4: trans='C', CAPPLY=.true., non-uniform C =====
  tmp = ZLA_GERCOND_C('C', N, A, NMAX, AF, NMAX, IPIV, C, .true., INFO, WORK, RWORK)
  call begin_test('trans_C_capply_nonuniform')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 5: trans='N', CAPPLY=.false. =====
  tmp = ZLA_GERCOND_C('N', N, A, NMAX, AF, NMAX, IPIV, C, .false., INFO, WORK, RWORK)
  call begin_test('trans_N_nocapply')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 6: trans='C', CAPPLY=.false. =====
  tmp = ZLA_GERCOND_C('C', N, A, NMAX, AF, NMAX, IPIV, C, .false., INFO, WORK, RWORK)
  call begin_test('trans_C_nocapply')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 7: N=1 edge case =====
  N = 1
  A(1,1) = (5.0d0, 2.0d0)
  AF(1,1) = (5.0d0, 2.0d0)
  IPIV(1) = 1
  C(1) = 2.0d0
  tmp = ZLA_GERCOND_C('N', N, A, NMAX, AF, NMAX, IPIV, C, .true., INFO, WORK, RWORK)
  call begin_test('n1_trans_N')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 8: N=0 edge case =====
  N = 0
  tmp = ZLA_GERCOND_C('N', N, A, NMAX, AF, NMAX, IPIV, C, .true., INFO, WORK, RWORK)
  call begin_test('n0')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

end program
