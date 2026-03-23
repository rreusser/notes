program test_dpocon
  use test_utils
  implicit none
  double precision :: a(100), work(100), anorm, rcond
  integer :: iwork(10), info, n
  double precision :: dlange, dlansy

  ! Test 1: 3x3 identity, upper (rcond = 1)
  n = 3
  a = 0.0d0
  a(1) = 1.0d0; a(5) = 1.0d0; a(9) = 1.0d0
  anorm = dlansy('1', 'U', n, a, n, work)
  call dpotrf('U', n, a, n, info)
  call dpocon('U', n, a, n, anorm, rcond, work, iwork, info)
  call begin_test('identity_upper')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 2: 3x3 identity, lower (rcond = 1)
  a = 0.0d0
  a(1) = 1.0d0; a(5) = 1.0d0; a(9) = 1.0d0
  anorm = dlansy('1', 'L', n, a, n, work)
  call dpotrf('L', n, a, n, info)
  call dpocon('L', n, a, n, anorm, rcond, work, iwork, info)
  call begin_test('identity_lower')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 3: Well-conditioned SPD 3x3, upper
  ! A = [[4, 2, 1], [2, 5, 2], [1, 2, 6]]
  n = 3
  a = 0.0d0
  a(1) = 4.0d0; a(2) = 2.0d0; a(3) = 1.0d0
  a(4) = 2.0d0; a(5) = 5.0d0; a(6) = 2.0d0
  a(7) = 1.0d0; a(8) = 2.0d0; a(9) = 6.0d0
  anorm = dlansy('1', 'U', n, a, n, work)
  call dpotrf('U', n, a, n, info)
  call dpocon('U', n, a, n, anorm, rcond, work, iwork, info)
  call begin_test('well_cond_upper')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 4: Same matrix, lower
  a = 0.0d0
  a(1) = 4.0d0; a(2) = 2.0d0; a(3) = 1.0d0
  a(4) = 2.0d0; a(5) = 5.0d0; a(6) = 2.0d0
  a(7) = 1.0d0; a(8) = 2.0d0; a(9) = 6.0d0
  anorm = dlansy('1', 'L', n, a, n, work)
  call dpotrf('L', n, a, n, info)
  call dpocon('L', n, a, n, anorm, rcond, work, iwork, info)
  call begin_test('well_cond_lower')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 5: Ill-conditioned SPD: diag(1, 1, 1e-15)
  n = 3
  a = 0.0d0
  a(1) = 1.0d0; a(5) = 1.0d0; a(9) = 1.0d-15
  anorm = dlansy('1', 'U', n, a, n, work)
  call dpotrf('U', n, a, n, info)
  call dpocon('U', n, a, n, anorm, rcond, work, iwork, info)
  call begin_test('ill_cond')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 6: N=0
  call dpocon('U', 0, a, 1, 0.0d0, rcond, work, iwork, info)
  call begin_test('n_zero')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 7: anorm = 0
  n = 3
  a = 0.0d0
  a(1) = 1.0d0; a(5) = 1.0d0; a(9) = 1.0d0
  call dpotrf('U', n, a, n, info)
  call dpocon('U', n, a, n, 0.0d0, rcond, work, iwork, info)
  call begin_test('anorm_zero')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 8: 4x4 SPD matrix, upper
  n = 4
  a = 0.0d0
  a(1) = 10.0d0; a(2) = 1.0d0; a(3) = 0.0d0; a(4) = 0.0d0
  a(5) = 1.0d0; a(6) = 8.0d0; a(7) = 1.0d0; a(8) = 0.0d0
  a(9) = 0.0d0; a(10) = 1.0d0; a(11) = 6.0d0; a(12) = 1.0d0
  a(13) = 0.0d0; a(14) = 0.0d0; a(15) = 1.0d0; a(16) = 4.0d0
  anorm = dlansy('1', 'U', n, a, n, work)
  call dpotrf('U', n, a, n, info)
  call dpocon('U', n, a, n, anorm, rcond, work, iwork, info)
  call begin_test('4x4_upper')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

end program
