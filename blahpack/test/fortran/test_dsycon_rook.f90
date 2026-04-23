program test_dsycon_rook
  use test_utils
  implicit none
  double precision :: a(100), work(200), anorm, rcond
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
  call dsytrf_rook('U', n, a, n, ipiv, work, lwork, info)
  call dsycon_rook('U', n, a, n, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('upper_well_cond')
  call print_matrix('A', a, n, n, n)
  call print_int_array('ipiv', ipiv, n)
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 2: same matrix, lower triangular
  n = 3
  a = 0.0d0
  a(1) = 4.0d0; a(2) = 1.0d0; a(3) = 1.0d0
  a(4) = 1.0d0; a(5) = 3.0d0; a(6) = 1.0d0
  a(7) = 1.0d0; a(8) = 1.0d0; a(9) = 2.0d0
  anorm = dlansy('1', 'L', n, a, n, work)
  lwork = 64 * n
  call dsytrf_rook('L', n, a, n, ipiv, work, lwork, info)
  call dsycon_rook('L', n, a, n, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('lower_well_cond')
  call print_matrix('A', a, n, n, n)
  call print_int_array('ipiv', ipiv, n)
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 3: 4x4 indefinite (upper) — exercises 2x2 pivots
  ! A = [[0, 1, 2, 3], [1, 0, 4, 5], [2, 4, 0, 6], [3, 5, 6, 0]]
  n = 4
  a = 0.0d0
  a(1) = 0.0d0
  a(5) = 1.0d0; a(6) = 0.0d0
  a(9) = 2.0d0; a(10) = 4.0d0; a(11) = 0.0d0
  a(13) = 3.0d0; a(14) = 5.0d0; a(15) = 6.0d0; a(16) = 0.0d0
  anorm = dlansy('1', 'U', n, a, n, work)
  lwork = 64 * n
  call dsytrf_rook('U', n, a, n, ipiv, work, lwork, info)
  call dsycon_rook('U', n, a, n, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('4x4_indef_upper')
  call print_matrix('A', a, n, n, n)
  call print_int_array('ipiv', ipiv, n)
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 4: 4x4 indefinite (lower)
  n = 4
  a = 0.0d0
  a(1) = 0.0d0; a(2) = 1.0d0; a(3) = 2.0d0; a(4) = 3.0d0
  a(5) = 0.0d0; a(6) = 0.0d0; a(7) = 4.0d0; a(8) = 5.0d0
  a(9) = 0.0d0; a(10) = 0.0d0; a(11) = 0.0d0; a(12) = 6.0d0
  a(13) = 0.0d0; a(14) = 0.0d0; a(15) = 0.0d0; a(16) = 0.0d0
  anorm = dlansy('1', 'L', n, a, n, work)
  lwork = 64 * n
  call dsytrf_rook('L', n, a, n, ipiv, work, lwork, info)
  call dsycon_rook('L', n, a, n, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('4x4_indef_lower')
  call print_matrix('A', a, n, n, n)
  call print_int_array('ipiv', ipiv, n)
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 5: 3x3 identity (upper, rcond=1)
  n = 3
  a = 0.0d0
  a(1) = 1.0d0; a(5) = 1.0d0; a(9) = 1.0d0
  anorm = dlansy('1', 'U', n, a, n, work)
  lwork = 64 * n
  call dsytrf_rook('U', n, a, n, ipiv, work, lwork, info)
  call dsycon_rook('U', n, a, n, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('identity_upper')
  call print_matrix('A', a, n, n, n)
  call print_int_array('ipiv', ipiv, n)
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 6: ill-conditioned matrix (upper)
  n = 3
  a = 0.0d0
  a(1) = 1.0d0; a(5) = 1.0d0; a(9) = 1.0d-15
  anorm = dlansy('1', 'U', n, a, n, work)
  lwork = 64 * n
  call dsytrf_rook('U', n, a, n, ipiv, work, lwork, info)
  call dsycon_rook('U', n, a, n, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('ill_cond_upper')
  call print_matrix('A', a, n, n, n)
  call print_int_array('ipiv', ipiv, n)
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 7: N=1 (upper)
  n = 1
  a = 0.0d0
  a(1) = 5.0d0
  anorm = 5.0d0
  lwork = 64
  call dsytrf_rook('U', n, a, n, ipiv, work, lwork, info)
  call dsycon_rook('U', n, a, n, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('n_one_upper')
  call print_matrix('A', a, n, n, n)
  call print_int_array('ipiv', ipiv, n)
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 8: 4x4 SPD (lower) — only 1x1 pivots
  n = 4
  a = 0.0d0
  a(1) = 10.0d0; a(2) = 1.0d0; a(3) = 2.0d0; a(4) = 0.0d0
  a(6) = 8.0d0; a(7) = 1.0d0; a(8) = 1.0d0
  a(11) = 6.0d0; a(12) = 1.0d0
  a(16) = 5.0d0
  anorm = dlansy('1', 'L', n, a, n, work)
  lwork = 64 * n
  call dsytrf_rook('L', n, a, n, ipiv, work, lwork, info)
  call dsycon_rook('L', n, a, n, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('4x4_spd_lower')
  call print_matrix('A', a, n, n, n)
  call print_int_array('ipiv', ipiv, n)
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

end program
