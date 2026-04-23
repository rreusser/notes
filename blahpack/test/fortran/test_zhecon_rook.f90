program test_zhecon_rook
  use test_utils
  implicit none

  integer, parameter :: NMAX = 6
  complex*16 :: A(NMAX, NMAX), WORK(NMAX*32)
  double precision :: A_r(2*NMAX*NMAX), W_r(2*NMAX*32)
  equivalence (A, A_r)
  equivalence (WORK, W_r)
  integer :: IPIV(NMAX), IWORK(NMAX), INFO, n, LWORK
  double precision :: anorm, rcond
  double precision, external :: zlanhe

  LWORK = NMAX * 32

  ! Test 1: 4x4 Hermitian (upper)
  n = 4
  A = (0.0d0, 0.0d0)
  A(1,1) = (4.0d0, 0.0d0)
  A(1,2) = (1.0d0, 2.0d0)
  A(1,3) = (3.0d0, -1.0d0)
  A(1,4) = (0.5d0, 0.5d0)
  A(2,2) = (5.0d0, 0.0d0)
  A(2,3) = (2.0d0, 1.0d0)
  A(2,4) = (1.0d0, -2.0d0)
  A(3,3) = (6.0d0, 0.0d0)
  A(3,4) = (3.0d0, 0.0d0)
  A(4,4) = (8.0d0, 0.0d0)
  anorm = zlanhe('1', 'U', n, A, NMAX, W_r)
  call ZHETRF_ROOK('U', n, A, NMAX, IPIV, WORK, LWORK, INFO)
  call ZHECON_ROOK('U', n, A, NMAX, IPIV, anorm, rcond, WORK, INFO)
  call begin_test('upper_4x4')
  call print_int('lda', NMAX)
  call print_array('A', A_r, 2*NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', INFO)
  call end_test()

  ! Test 2: 4x4 Hermitian (lower)
  n = 4
  A = (0.0d0, 0.0d0)
  A(1,1) = (4.0d0, 0.0d0)
  A(2,1) = (1.0d0, -2.0d0)
  A(2,2) = (5.0d0, 0.0d0)
  A(3,1) = (3.0d0, 1.0d0)
  A(3,2) = (2.0d0, -1.0d0)
  A(3,3) = (6.0d0, 0.0d0)
  A(4,1) = (0.5d0, -0.5d0)
  A(4,2) = (1.0d0, 2.0d0)
  A(4,3) = (3.0d0, 0.0d0)
  A(4,4) = (8.0d0, 0.0d0)
  anorm = zlanhe('1', 'L', n, A, NMAX, W_r)
  call ZHETRF_ROOK('L', n, A, NMAX, IPIV, WORK, LWORK, INFO)
  call ZHECON_ROOK('L', n, A, NMAX, IPIV, anorm, rcond, WORK, INFO)
  call begin_test('lower_4x4')
  call print_int('lda', NMAX)
  call print_array('A', A_r, 2*NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', INFO)
  call end_test()

  ! Test 3: 3x3 identity (upper, rcond=1)
  n = 3
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0)
  A(2,2) = (1.0d0, 0.0d0)
  A(3,3) = (1.0d0, 0.0d0)
  anorm = zlanhe('1', 'U', n, A, NMAX, W_r)
  call ZHETRF_ROOK('U', n, A, NMAX, IPIV, WORK, LWORK, INFO)
  call ZHECON_ROOK('U', n, A, NMAX, IPIV, anorm, rcond, WORK, INFO)
  call begin_test('identity_upper')
  call print_int('lda', NMAX)
  call print_array('A', A_r, 2*NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', INFO)
  call end_test()

  ! Test 4: N=1
  n = 1
  A = (0.0d0, 0.0d0)
  A(1,1) = (5.0d0, 0.0d0)
  anorm = 5.0d0
  call ZHETRF_ROOK('U', n, A, NMAX, IPIV, WORK, LWORK, INFO)
  call ZHECON_ROOK('U', n, A, NMAX, IPIV, anorm, rcond, WORK, INFO)
  call begin_test('n_one_upper')
  call print_int('lda', NMAX)
  call print_array('A', A_r, 2*NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', INFO)
  call end_test()

  ! Test 5: 3x3 indefinite Hermitian (lower) — exercises 2x2 pivots
  n = 3
  A = (0.0d0, 0.0d0)
  A(1,1) = (0.0d0, 0.0d0)
  A(2,1) = (1.0d0, 1.0d0)
  A(2,2) = (0.0d0, 0.0d0)
  A(3,1) = (2.0d0, -1.0d0)
  A(3,2) = (1.0d0, 0.5d0)
  A(3,3) = (3.0d0, 0.0d0)
  anorm = zlanhe('1', 'L', n, A, NMAX, W_r)
  call ZHETRF_ROOK('L', n, A, NMAX, IPIV, WORK, LWORK, INFO)
  call ZHECON_ROOK('L', n, A, NMAX, IPIV, anorm, rcond, WORK, INFO)
  call begin_test('indef_lower')
  call print_int('lda', NMAX)
  call print_array('A', A_r, 2*NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', INFO)
  call end_test()

end program
