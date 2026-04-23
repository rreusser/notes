program test_zla_hercond_c
  use test_utils
  implicit none

  integer, parameter :: NMAX = 3
  integer, parameter :: LWORK = 64
  complex*16 :: A(NMAX, NMAX), AF(NMAX, NMAX), WORK(2*NMAX), FWORK(LWORK)
  double precision :: A_r(2*NMAX*NMAX), AF_r(2*NMAX*NMAX)
  double precision :: C(NMAX), RWORK(NMAX), tmp
  integer :: IPIV(NMAX), INFO, N
  double precision :: ZLA_HERCOND_C
  external :: ZLA_HERCOND_C, ZHETRF
  equivalence (A, A_r)
  equivalence (AF, AF_r)

  ! ===== Setup 3x3 Hermitian indefinite matrix =====
  ! Upper triangle stored. Diagonal must be real for Hermitian.
  N = 3
  A(1,1) = ( 2.0d0, 0.0d0); A(1,2) = ( 1.0d0, 1.0d0); A(1,3) = ( 0.0d0,-1.0d0)
  A(2,1) = ( 1.0d0,-1.0d0); A(2,2) = (-3.0d0, 0.0d0); A(2,3) = ( 2.0d0, 0.5d0)
  A(3,1) = ( 0.0d0, 1.0d0); A(3,2) = ( 2.0d0,-0.5d0); A(3,3) = ( 4.0d0, 0.0d0)

  ! ===== Test 1: UPLO='U', CAPPLY=.true., uniform C =====
  AF = A
  call ZHETRF('U', N, AF, NMAX, IPIV, FWORK, LWORK, INFO)
  C(1) = 1.0d0; C(2) = 1.0d0; C(3) = 1.0d0
  tmp = ZLA_HERCOND_C('U', N, A, NMAX, AF, NMAX, IPIV, C, .true., INFO, WORK, RWORK)
  call begin_test('uplo_U_capply_uniform')
  call print_array('A', A_r, 2*NMAX*NMAX)
  call print_array('AF_U', AF_r, 2*NMAX*NMAX)
  call print_int_array('IPIV_U', IPIV, NMAX)
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 2: UPLO='L', CAPPLY=.true., uniform C =====
  AF = A
  call ZHETRF('L', N, AF, NMAX, IPIV, FWORK, LWORK, INFO)
  tmp = ZLA_HERCOND_C('L', N, A, NMAX, AF, NMAX, IPIV, C, .true., INFO, WORK, RWORK)
  call begin_test('uplo_L_capply_uniform')
  call print_array('AF_L', AF_r, 2*NMAX*NMAX)
  call print_int_array('IPIV_L', IPIV, NMAX)
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 3: UPLO='U', CAPPLY=.true., non-uniform C =====
  AF = A
  call ZHETRF('U', N, AF, NMAX, IPIV, FWORK, LWORK, INFO)
  C(1) = 2.0d0; C(2) = 0.5d0; C(3) = 3.0d0
  tmp = ZLA_HERCOND_C('U', N, A, NMAX, AF, NMAX, IPIV, C, .true., INFO, WORK, RWORK)
  call begin_test('uplo_U_capply_nonuniform')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 4: UPLO='L', CAPPLY=.true., non-uniform C =====
  AF = A
  call ZHETRF('L', N, AF, NMAX, IPIV, FWORK, LWORK, INFO)
  tmp = ZLA_HERCOND_C('L', N, A, NMAX, AF, NMAX, IPIV, C, .true., INFO, WORK, RWORK)
  call begin_test('uplo_L_capply_nonuniform')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 5: UPLO='U', CAPPLY=.false. =====
  AF = A
  call ZHETRF('U', N, AF, NMAX, IPIV, FWORK, LWORK, INFO)
  tmp = ZLA_HERCOND_C('U', N, A, NMAX, AF, NMAX, IPIV, C, .false., INFO, WORK, RWORK)
  call begin_test('uplo_U_nocapply')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 6: UPLO='L', CAPPLY=.false. =====
  AF = A
  call ZHETRF('L', N, AF, NMAX, IPIV, FWORK, LWORK, INFO)
  tmp = ZLA_HERCOND_C('L', N, A, NMAX, AF, NMAX, IPIV, C, .false., INFO, WORK, RWORK)
  call begin_test('uplo_L_nocapply')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 7: N=1 edge case =====
  N = 1
  A(1,1) = (5.0d0, 0.0d0)
  AF(1,1) = (5.0d0, 0.0d0)
  IPIV(1) = 1
  C(1) = 2.0d0
  tmp = ZLA_HERCOND_C('U', N, A, NMAX, AF, NMAX, IPIV, C, .true., INFO, WORK, RWORK)
  call begin_test('n1_uplo_U')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 8: N=0 edge case =====
  N = 0
  tmp = ZLA_HERCOND_C('U', N, A, NMAX, AF, NMAX, IPIV, C, .true., INFO, WORK, RWORK)
  call begin_test('n0')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

end program
