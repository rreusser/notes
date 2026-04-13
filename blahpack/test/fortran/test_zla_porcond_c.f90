program test_zla_porcond_c
  use test_utils
  implicit none

  integer, parameter :: NMAX = 3
  complex*16 :: A(NMAX, NMAX), AF(NMAX, NMAX), WORK(2*NMAX)
  double precision :: A_r(2*NMAX*NMAX), AF_r(2*NMAX*NMAX)
  double precision :: C(NMAX), RWORK(NMAX), tmp
  integer :: INFO, N
  double precision :: ZLA_PORCOND_C
  external :: ZLA_PORCOND_C, ZPOTRF
  equivalence (A, A_r)
  equivalence (AF, AF_r)

  ! ===== Setup 3x3 Hermitian positive-definite matrix =====
  ! Diagonally dominant with real diagonal. Both triangles populated (Hermitian).
  N = 3
  A(1,1) = ( 6.0d0, 0.0d0); A(1,2) = ( 1.0d0, 1.0d0); A(1,3) = ( 0.0d0,-1.0d0)
  A(2,1) = ( 1.0d0,-1.0d0); A(2,2) = ( 7.0d0, 0.0d0); A(2,3) = ( 2.0d0, 0.5d0)
  A(3,1) = ( 0.0d0, 1.0d0); A(3,2) = ( 2.0d0,-0.5d0); A(3,3) = ( 8.0d0, 0.0d0)

  ! ===== Test 1: UPLO='U', CAPPLY=.true., uniform C =====
  AF = A
  call ZPOTRF('U', N, AF, NMAX, INFO)
  C(1) = 1.0d0; C(2) = 1.0d0; C(3) = 1.0d0
  tmp = ZLA_PORCOND_C('U', N, A, NMAX, AF, NMAX, C, .true., INFO, WORK, RWORK)
  call begin_test('uplo_U_capply_uniform')
  call print_array('A', A_r, 2*NMAX*NMAX)
  call print_array('AF_U', AF_r, 2*NMAX*NMAX)
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 2: UPLO='L', CAPPLY=.true., uniform C =====
  AF = A
  call ZPOTRF('L', N, AF, NMAX, INFO)
  tmp = ZLA_PORCOND_C('L', N, A, NMAX, AF, NMAX, C, .true., INFO, WORK, RWORK)
  call begin_test('uplo_L_capply_uniform')
  call print_array('AF_L', AF_r, 2*NMAX*NMAX)
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 3: UPLO='U', CAPPLY=.true., non-uniform C =====
  AF = A
  call ZPOTRF('U', N, AF, NMAX, INFO)
  C(1) = 2.0d0; C(2) = 0.5d0; C(3) = 3.0d0
  tmp = ZLA_PORCOND_C('U', N, A, NMAX, AF, NMAX, C, .true., INFO, WORK, RWORK)
  call begin_test('uplo_U_capply_nonuniform')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 4: UPLO='L', CAPPLY=.true., non-uniform C =====
  AF = A
  call ZPOTRF('L', N, AF, NMAX, INFO)
  tmp = ZLA_PORCOND_C('L', N, A, NMAX, AF, NMAX, C, .true., INFO, WORK, RWORK)
  call begin_test('uplo_L_capply_nonuniform')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 5: UPLO='U', CAPPLY=.false. =====
  AF = A
  call ZPOTRF('U', N, AF, NMAX, INFO)
  tmp = ZLA_PORCOND_C('U', N, A, NMAX, AF, NMAX, C, .false., INFO, WORK, RWORK)
  call begin_test('uplo_U_nocapply')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 6: UPLO='L', CAPPLY=.false. =====
  AF = A
  call ZPOTRF('L', N, AF, NMAX, INFO)
  tmp = ZLA_PORCOND_C('L', N, A, NMAX, AF, NMAX, C, .false., INFO, WORK, RWORK)
  call begin_test('uplo_L_nocapply')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 7: N=1 edge case =====
  N = 1
  A(1,1) = (4.0d0, 0.0d0)
  AF(1,1) = (2.0d0, 0.0d0)
  C(1) = 2.0d0
  tmp = ZLA_PORCOND_C('U', N, A, NMAX, AF, NMAX, C, .true., INFO, WORK, RWORK)
  call begin_test('n1_uplo_U')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 8: N=0 edge case =====
  N = 0
  tmp = ZLA_PORCOND_C('U', N, A, NMAX, AF, NMAX, C, .true., INFO, WORK, RWORK)
  call begin_test('n0')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

end program
