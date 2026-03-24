program test_dsycon
  use test_utils
  implicit none
  double precision :: a(100), work(100), anorm, rcond
  integer :: ipiv(10), iwork(10), info, n, lwork
  double precision :: dlansy

  ! Test 1: 3x3 well-conditioned symmetric matrix (upper)
  ! A = [[4, 1, 1], [1, 3, 1], [1, 1, 2]] (symmetric, diag-dominant)
  n = 3
  a = 0.0d0
  a(1) = 4.0d0; a(2) = 1.0d0; a(3) = 1.0d0
  a(4) = 1.0d0; a(5) = 3.0d0; a(6) = 1.0d0
  a(7) = 1.0d0; a(8) = 1.0d0; a(9) = 2.0d0
  anorm = dlansy('1', 'U', n, a, n, work)
  lwork = 64 * n
  call dsytrf('U', n, a, n, ipiv, work, lwork, info)
  call dsycon('U', n, a, n, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('upper_well_cond')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call print_int_array('ipiv', ipiv, n)
  call end_test()

  ! Test 2: same matrix, lower triangular
  n = 3
  a = 0.0d0
  a(1) = 4.0d0; a(2) = 1.0d0; a(3) = 1.0d0
  a(4) = 1.0d0; a(5) = 3.0d0; a(6) = 1.0d0
  a(7) = 1.0d0; a(8) = 1.0d0; a(9) = 2.0d0
  anorm = dlansy('1', 'L', n, a, n, work)
  lwork = 64 * n
  call dsytrf('L', n, a, n, ipiv, work, lwork, info)
  call dsycon('L', n, a, n, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('lower_well_cond')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call print_int_array('ipiv', ipiv, n)
  call end_test()

  ! Test 3: 3x3 identity (upper, rcond = 1)
  n = 3
  a = 0.0d0
  a(1) = 1.0d0; a(5) = 1.0d0; a(9) = 1.0d0
  anorm = dlansy('1', 'U', n, a, n, work)
  lwork = 64 * n
  call dsytrf('U', n, a, n, ipiv, work, lwork, info)
  call dsycon('U', n, a, n, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('identity_upper')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 4: 3x3 identity (lower, rcond = 1)
  n = 3
  a = 0.0d0
  a(1) = 1.0d0; a(5) = 1.0d0; a(9) = 1.0d0
  anorm = dlansy('1', 'L', n, a, n, work)
  lwork = 64 * n
  call dsytrf('L', n, a, n, ipiv, work, lwork, info)
  call dsycon('L', n, a, n, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('identity_lower')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 5: ill-conditioned matrix (upper)
  ! A = [[1, 0, 0], [0, 1, 0], [0, 0, 1e-15]] (symmetric)
  n = 3
  a = 0.0d0
  a(1) = 1.0d0; a(5) = 1.0d0; a(9) = 1.0d-15
  anorm = dlansy('1', 'U', n, a, n, work)
  lwork = 64 * n
  call dsytrf('U', n, a, n, ipiv, work, lwork, info)
  call dsycon('U', n, a, n, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('ill_cond_upper')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 6: singular matrix (upper)
  ! A = [[1, 2, 3], [2, 4, 6], [3, 6, 9]] (rank 1)
  n = 3
  a = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0
  a(4) = 2.0d0; a(5) = 4.0d0; a(6) = 6.0d0
  a(7) = 3.0d0; a(8) = 6.0d0; a(9) = 9.0d0
  anorm = dlansy('1', 'U', n, a, n, work)
  lwork = 64 * n
  call dsytrf('U', n, a, n, ipiv, work, lwork, info)
  call dsycon('U', n, a, n, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('singular_upper')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 7: N=0 (upper)
  call dsycon('U', 0, a, 1, ipiv, 0.0d0, rcond, work, iwork, info)
  call begin_test('n_zero')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 8: 4x4 symmetric positive definite (upper)
  n = 4
  a = 0.0d0
  a(1) = 10.0d0; a(2) = 1.0d0; a(3) = 2.0d0; a(4) = 0.0d0
  a(5) = 1.0d0; a(6) = 8.0d0; a(7) = 1.0d0; a(8) = 1.0d0
  a(9) = 2.0d0; a(10) = 1.0d0; a(11) = 6.0d0; a(12) = 1.0d0
  a(13) = 0.0d0; a(14) = 1.0d0; a(15) = 1.0d0; a(16) = 5.0d0
  anorm = dlansy('1', 'U', n, a, n, work)
  lwork = 64 * n
  call dsytrf('U', n, a, n, ipiv, work, lwork, info)
  call dsycon('U', n, a, n, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('4x4_upper')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 9: 4x4 symmetric positive definite (lower)
  n = 4
  a = 0.0d0
  a(1) = 10.0d0; a(2) = 1.0d0; a(3) = 2.0d0; a(4) = 0.0d0
  a(5) = 1.0d0; a(6) = 8.0d0; a(7) = 1.0d0; a(8) = 1.0d0
  a(9) = 2.0d0; a(10) = 1.0d0; a(11) = 6.0d0; a(12) = 1.0d0
  a(13) = 0.0d0; a(14) = 1.0d0; a(15) = 1.0d0; a(16) = 5.0d0
  anorm = dlansy('1', 'L', n, a, n, work)
  lwork = 64 * n
  call dsytrf('L', n, a, n, ipiv, work, lwork, info)
  call dsycon('L', n, a, n, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('4x4_lower')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 10: N=1 (upper)
  n = 1
  a(1) = 5.0d0
  anorm = 5.0d0
  lwork = 64
  call dsytrf('U', n, a, n, ipiv, work, lwork, info)
  call dsycon('U', n, a, n, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('n_one_upper')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

end program
