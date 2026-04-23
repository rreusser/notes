program test_dla_gercond
  use test_utils
  implicit none

  integer, parameter :: NMAX = 3
  double precision :: A(NMAX, NMAX), AF(NMAX, NMAX)
  double precision :: C(NMAX), WORK(3*NMAX), tmp
  integer :: IPIV(NMAX), IWORK(NMAX), INFO, N
  double precision :: DLA_GERCOND
  external :: DLA_GERCOND, DGETRF

  ! ===== Test 1: trans='N', cmode=1, C = [1,1,1] (uniform scaling) =====
  N = 3
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 0.0d0
  A(2,1) = 1.0d0; A(2,2) = 3.0d0; A(2,3) = 1.0d0
  A(3,1) = 0.0d0; A(3,2) = 1.0d0; A(3,3) = 4.0d0
  AF = A
  call DGETRF(N, N, AF, NMAX, IPIV, INFO)
  C(1) = 1.0d0; C(2) = 1.0d0; C(3) = 1.0d0

  tmp = DLA_GERCOND('N', N, A, NMAX, AF, NMAX, IPIV, 1, C, INFO, WORK, IWORK)
  call begin_test('trans_N_cmode1_uniform')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 2: trans='T', cmode=1, C = [1,1,1] =====
  tmp = DLA_GERCOND('T', N, A, NMAX, AF, NMAX, IPIV, 1, C, INFO, WORK, IWORK)
  call begin_test('trans_T_cmode1_uniform')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 3: trans='N', cmode=1, non-uniform C =====
  C(1) = 2.0d0; C(2) = 0.5d0; C(3) = 3.0d0
  tmp = DLA_GERCOND('N', N, A, NMAX, AF, NMAX, IPIV, 1, C, INFO, WORK, IWORK)
  call begin_test('trans_N_cmode1_nonuniform')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 4: trans='T', cmode=1, non-uniform C =====
  tmp = DLA_GERCOND('T', N, A, NMAX, AF, NMAX, IPIV, 1, C, INFO, WORK, IWORK)
  call begin_test('trans_T_cmode1_nonuniform')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 5: trans='N', cmode=0 (no scaling) =====
  tmp = DLA_GERCOND('N', N, A, NMAX, AF, NMAX, IPIV, 0, C, INFO, WORK, IWORK)
  call begin_test('trans_N_cmode0')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 6: trans='T', cmode=0 =====
  tmp = DLA_GERCOND('T', N, A, NMAX, AF, NMAX, IPIV, 0, C, INFO, WORK, IWORK)
  call begin_test('trans_T_cmode0')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 7: trans='N', cmode=-1, non-uniform C =====
  tmp = DLA_GERCOND('N', N, A, NMAX, AF, NMAX, IPIV, -1, C, INFO, WORK, IWORK)
  call begin_test('trans_N_cmode_neg1')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 8: trans='T', cmode=-1, non-uniform C =====
  tmp = DLA_GERCOND('T', N, A, NMAX, AF, NMAX, IPIV, -1, C, INFO, WORK, IWORK)
  call begin_test('trans_T_cmode_neg1')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 9: N=1 edge case =====
  N = 1
  A(1,1) = 5.0d0
  AF(1,1) = 5.0d0
  IPIV(1) = 1
  C(1) = 2.0d0
  tmp = DLA_GERCOND('N', N, A, NMAX, AF, NMAX, IPIV, 1, C, INFO, WORK, IWORK)
  call begin_test('n1_trans_N')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 10: N=0 edge case =====
  N = 0
  tmp = DLA_GERCOND('N', N, A, NMAX, AF, NMAX, IPIV, 1, C, INFO, WORK, IWORK)
  call begin_test('n0')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

end program
