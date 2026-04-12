program test_zla_syrcond_c
  use test_utils
  implicit none

  integer, parameter :: NMAX = 3
  complex*16 :: A(NMAX, NMAX), AF(NMAX, NMAX), WORK(2*NMAX), WORKF(NMAX)
  complex*16 :: Apk(NMAX*NMAX), AFpk(NMAX*NMAX)
  double precision :: Apk_r(2*NMAX*NMAX), AFpk_r(2*NMAX*NMAX)
  double precision :: C(NMAX), RWORK(NMAX), tmp
  integer :: IPIV(NMAX), INFO, N, I, J
  double precision :: ZLA_SYRCOND_C
  external :: ZLA_SYRCOND_C, ZSYTRF
  equivalence (Apk, Apk_r)
  equivalence (AFpk, AFpk_r)

  ! ===== Setup 3x3 complex symmetric matrix =====
  N = 3
  A = (0.0D+0, 0.0D+0)
  A(1,1) = (2.0D+0, 1.0D+0)
  A(1,2) = (1.0D+0, 0.5D+0)
  A(1,3) = (0.5D+0, -0.5D+0)
  A(2,2) = (3.0D+0, -0.5D+0)
  A(2,3) = (-0.5D+0, 0.25D+0)
  A(3,3) = (4.0D+0, 0.0D+0)
  ! Mirror to lower (symmetric, not hermitian)
  A(2,1) = A(1,2)
  A(3,1) = A(1,3)
  A(3,2) = A(2,3)

  C(1) = 1.0D+0; C(2) = 2.0D+0; C(3) = 0.5D+0

  ! Factor with upper
  AF = A
  call ZSYTRF('U', N, AF, NMAX, IPIV, WORKF, NMAX, INFO)
  if ( INFO .ne. 0 ) then
    print *, 'ZSYTRF U failed with INFO=', INFO
    stop 1
  end if

  ! ===== Test 1: UPLO='U', capply=true =====
  tmp = ZLA_SYRCOND_C('U', N, A, NMAX, AF, NMAX, IPIV, C, .true., INFO, WORK, RWORK)
  do j = 1, N
    do i = 1, N
      Apk(i + (j-1)*N) = A(i, j)
      AFpk(i + (j-1)*N) = AF(i, j)
    end do
  end do
  call begin_test('upper_capply')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call print_array('A', Apk_r, 2*N*N)
  call print_array('AF', AFpk_r, 2*N*N)
  call print_int_array('IPIV', IPIV, N)
  call print_array('C', C, N)
  call end_test()

  ! ===== Test 2: UPLO='U', capply=false =====
  tmp = ZLA_SYRCOND_C('U', N, A, NMAX, AF, NMAX, IPIV, C, .false., INFO, WORK, RWORK)
  call begin_test('upper_nocapply')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 3: UPLO='U', capply=true, non-uniform C =====
  C(1) = 3.0D+0; C(2) = 0.1D+0; C(3) = 2.0D+0
  tmp = ZLA_SYRCOND_C('U', N, A, NMAX, AF, NMAX, IPIV, C, .true., INFO, WORK, RWORK)
  call begin_test('upper_capply_nonuniform')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call print_array('C', C, N)
  call end_test()

  ! Reset C
  C(1) = 1.0D+0; C(2) = 2.0D+0; C(3) = 0.5D+0

  ! Factor with lower
  AF = A
  call ZSYTRF('L', N, AF, NMAX, IPIV, WORKF, NMAX, INFO)
  if ( INFO .ne. 0 ) then
    print *, 'ZSYTRF L failed with INFO=', INFO
    stop 1
  end if

  ! ===== Test 4: UPLO='L', capply=true =====
  tmp = ZLA_SYRCOND_C('L', N, A, NMAX, AF, NMAX, IPIV, C, .true., INFO, WORK, RWORK)
  do j = 1, N
    do i = 1, N
      AFpk(i + (j-1)*N) = AF(i, j)
    end do
  end do
  call begin_test('lower_capply')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call print_array('AF', AFpk_r, 2*N*N)
  call print_int_array('IPIV', IPIV, N)
  call end_test()

  ! ===== Test 5: UPLO='L', capply=false =====
  tmp = ZLA_SYRCOND_C('L', N, A, NMAX, AF, NMAX, IPIV, C, .false., INFO, WORK, RWORK)
  call begin_test('lower_nocapply')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 6: UPLO='L', capply=true, non-uniform C =====
  C(1) = 3.0D+0; C(2) = 0.1D+0; C(3) = 2.0D+0
  tmp = ZLA_SYRCOND_C('L', N, A, NMAX, AF, NMAX, IPIV, C, .true., INFO, WORK, RWORK)
  call begin_test('lower_capply_nonuniform')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call print_array('C', C, N)
  call end_test()

  ! ===== Test 7: N=1 edge case =====
  N = 1
  A(1,1) = (5.0D+0, 2.0D+0)
  AF(1,1) = (5.0D+0, 2.0D+0)
  IPIV(1) = 1
  C(1) = 2.0D+0
  tmp = ZLA_SYRCOND_C('U', N, A, NMAX, AF, NMAX, IPIV, C, .true., INFO, WORK, RWORK)
  call begin_test('n1_upper')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 8: N=0 edge case =====
  N = 0
  tmp = ZLA_SYRCOND_C('U', N, A, NMAX, AF, NMAX, IPIV, C, .true., INFO, WORK, RWORK)
  call begin_test('n0')
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

end program
