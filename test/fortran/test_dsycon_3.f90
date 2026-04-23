program test_dsycon_3
  use test_utils
  implicit none
  double precision :: a(100), e(10), work(400), anorm, rcond
  integer :: ipiv(10), iwork(10), info, n, lwork
  double precision :: dlansy

  ! Test 1: 4x4 well-conditioned symmetric (upper)
  n = 4
  a = 0.0d0
  a(1) = 10.0d0
  a(5) = 1.0d0;  a(6) = 8.0d0
  a(9) = 2.0d0;  a(10) = 1.0d0; a(11) = 6.0d0
  a(13) = 0.0d0; a(14) = 1.0d0; a(15) = 1.0d0; a(16) = 5.0d0
  anorm = dlansy('1', 'U', n, a, n, work)
  lwork = 400
  call dsytrf_rk('U', n, a, n, e, ipiv, work, lwork, info)
  call dsycon_3('U', n, a, n, e, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('4x4_upper_well')
  call print_array('a', a, n*n)
  call print_array('e', e, n)
  call print_int_array('ipiv', ipiv, n)
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 2: same matrix, lower triangular
  n = 4
  a = 0.0d0
  a(1) = 10.0d0; a(2) = 1.0d0; a(3) = 2.0d0; a(4) = 0.0d0
  a(6) = 8.0d0;  a(7) = 1.0d0; a(8) = 1.0d0
  a(11) = 6.0d0; a(12) = 1.0d0
  a(16) = 5.0d0
  anorm = dlansy('1', 'L', n, a, n, work)
  lwork = 400
  call dsytrf_rk('L', n, a, n, e, ipiv, work, lwork, info)
  call dsycon_3('L', n, a, n, e, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('4x4_lower_well')
  call print_array('a', a, n*n)
  call print_array('e', e, n)
  call print_int_array('ipiv', ipiv, n)
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 3: 3x3 identity (upper)
  n = 3
  a = 0.0d0
  a(1) = 1.0d0; a(5) = 1.0d0; a(9) = 1.0d0
  anorm = dlansy('1', 'U', n, a, n, work)
  lwork = 400
  call dsytrf_rk('U', n, a, n, e, ipiv, work, lwork, info)
  call dsycon_3('U', n, a, n, e, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('identity_upper')
  call print_array('a', a, n*n)
  call print_array('e', e, n)
  call print_int_array('ipiv', ipiv, n)
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 4: 3x3 identity (lower)
  n = 3
  a = 0.0d0
  a(1) = 1.0d0; a(5) = 1.0d0; a(9) = 1.0d0
  anorm = dlansy('1', 'L', n, a, n, work)
  lwork = 400
  call dsytrf_rk('L', n, a, n, e, ipiv, work, lwork, info)
  call dsycon_3('L', n, a, n, e, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('identity_lower')
  call print_array('a', a, n*n)
  call print_array('e', e, n)
  call print_int_array('ipiv', ipiv, n)
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 5: ill-conditioned (upper)
  n = 3
  a = 0.0d0
  a(1) = 1.0d0; a(5) = 1.0d0; a(9) = 1.0d-12
  anorm = dlansy('1', 'U', n, a, n, work)
  lwork = 400
  call dsytrf_rk('U', n, a, n, e, ipiv, work, lwork, info)
  call dsycon_3('U', n, a, n, e, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('ill_cond_upper')
  call print_array('a', a, n*n)
  call print_array('e', e, n)
  call print_int_array('ipiv', ipiv, n)
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 6: indefinite forces 2x2 pivots (lower)
  n = 4
  a = 0.0d0
  a(1) = 0.0d0; a(2) = 1.0d0; a(3) = 2.0d0; a(4) = 3.0d0
  a(6) = 0.0d0; a(7) = 4.0d0; a(8) = 5.0d0
  a(11) = 0.0d0; a(12) = 6.0d0
  a(16) = 0.0d0
  anorm = dlansy('1', 'L', n, a, n, work)
  lwork = 400
  call dsytrf_rk('L', n, a, n, e, ipiv, work, lwork, info)
  call dsycon_3('L', n, a, n, e, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('indef_lower')
  call print_array('a', a, n*n)
  call print_array('e', e, n)
  call print_int_array('ipiv', ipiv, n)
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 7: indefinite (upper)
  n = 4
  a = 0.0d0
  a(1) = 0.0d0
  a(5) = 1.0d0; a(6) = 0.0d0
  a(9) = 2.0d0; a(10) = 4.0d0; a(11) = 0.0d0
  a(13) = 3.0d0; a(14) = 5.0d0; a(15) = 6.0d0; a(16) = 0.0d0
  anorm = dlansy('1', 'U', n, a, n, work)
  lwork = 400
  call dsytrf_rk('U', n, a, n, e, ipiv, work, lwork, info)
  call dsycon_3('U', n, a, n, e, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('indef_upper')
  call print_array('a', a, n*n)
  call print_array('e', e, n)
  call print_int_array('ipiv', ipiv, n)
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 8: N=0
  n = 0
  call dsycon_3('U', n, a, 1, e, ipiv, 0.0d0, rcond, work, iwork, info)
  call begin_test('n_zero')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 9: N=1, upper
  n = 1
  a(1) = 5.0d0
  e(1) = 0.0d0
  anorm = 5.0d0
  lwork = 400
  call dsytrf_rk('U', n, a, n, e, ipiv, work, lwork, info)
  call dsycon_3('U', n, a, n, e, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('n_one_upper')
  call print_array('a', a, n*n)
  call print_array('e', e, n)
  call print_int_array('ipiv', ipiv, n)
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 10: anorm = 0
  n = 3
  a = 0.0d0
  a(1) = 1.0d0; a(5) = 1.0d0; a(9) = 1.0d0
  anorm = 0.0d0
  lwork = 400
  call dsytrf_rk('U', n, a, n, e, ipiv, work, lwork, info)
  call dsycon_3('U', n, a, n, e, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('anorm_zero')
  call print_array('a', a, n*n)
  call print_array('e', e, n)
  call print_int_array('ipiv', ipiv, n)
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

end program
