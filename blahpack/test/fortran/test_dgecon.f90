program test_dgecon
  use test_utils
  implicit none
  double precision :: a(100), work(100), anorm, rcond
  integer :: ipiv(10), iwork(10), info, n
  double precision :: dlange

  ! Test 1: 3x3 well-conditioned matrix (1-norm)
  ! A = [[4, 1, 1], [1, 3, 1], [1, 1, 2]] (symmetric, diag-dominant)
  n = 3
  a = 0.0d0
  a(1) = 4.0d0; a(2) = 1.0d0; a(3) = 1.0d0
  a(4) = 1.0d0; a(5) = 3.0d0; a(6) = 1.0d0
  a(7) = 1.0d0; a(8) = 1.0d0; a(9) = 2.0d0
  anorm = dlange('1', n, n, a, n, work)
  call dgetrf(n, n, a, n, ipiv, info)
  call dgecon('1', n, a, n, anorm, rcond, work, iwork, info)
  call begin_test('well_cond_1norm')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 2: same matrix with infinity-norm
  a = 0.0d0
  a(1) = 4.0d0; a(2) = 1.0d0; a(3) = 1.0d0
  a(4) = 1.0d0; a(5) = 3.0d0; a(6) = 1.0d0
  a(7) = 1.0d0; a(8) = 1.0d0; a(9) = 2.0d0
  anorm = dlange('I', n, n, a, n, work)
  call dgetrf(n, n, a, n, ipiv, info)
  call dgecon('I', n, a, n, anorm, rcond, work, iwork, info)
  call begin_test('well_cond_Inorm')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 3: 3x3 identity (rcond = 1)
  n = 3
  a = 0.0d0
  a(1) = 1.0d0; a(5) = 1.0d0; a(9) = 1.0d0
  anorm = dlange('1', n, n, a, n, work)
  call dgetrf(n, n, a, n, ipiv, info)
  call dgecon('1', n, a, n, anorm, rcond, work, iwork, info)
  call begin_test('identity')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 4: 3x3 ill-conditioned matrix
  ! Nearly singular: [[1, 0, 0], [0, 1, 0], [0, 0, 1e-15]]
  n = 3
  a = 0.0d0
  a(1) = 1.0d0; a(5) = 1.0d0; a(9) = 1.0d-15
  anorm = dlange('1', n, n, a, n, work)
  call dgetrf(n, n, a, n, ipiv, info)
  call dgecon('1', n, a, n, anorm, rcond, work, iwork, info)
  call begin_test('ill_cond')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 5: singular matrix (rcond = 0)
  n = 3
  a = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0
  a(4) = 2.0d0; a(5) = 4.0d0; a(6) = 6.0d0
  a(7) = 3.0d0; a(8) = 6.0d0; a(9) = 9.0d0
  anorm = dlange('1', n, n, a, n, work)
  call dgetrf(n, n, a, n, ipiv, info)
  call dgecon('1', n, a, n, anorm, rcond, work, iwork, info)
  call begin_test('singular')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 6: N=0 (rcond = 1)
  call dgecon('1', 0, a, 1, 0.0d0, rcond, work, iwork, info)
  call begin_test('n_zero')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 7: anorm = 0 (rcond = 0)
  n = 3
  a = 0.0d0
  a(1) = 1.0d0; a(5) = 1.0d0; a(9) = 1.0d0
  call dgetrf(n, n, a, n, ipiv, info)
  call dgecon('1', n, a, n, 0.0d0, rcond, work, iwork, info)
  call begin_test('anorm_zero')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 8: 4x4 matrix
  n = 4
  a = 0.0d0
  a(1) = 5.0d0; a(2) = 1.0d0; a(3) = 0.0d0; a(4) = 0.0d0
  a(5) = 1.0d0; a(6) = 4.0d0; a(7) = 1.0d0; a(8) = 0.0d0
  a(9) = 0.0d0; a(10) = 1.0d0; a(11) = 3.0d0; a(12) = 1.0d0
  a(13) = 0.0d0; a(14) = 0.0d0; a(15) = 1.0d0; a(16) = 2.0d0
  anorm = dlange('1', n, n, a, n, work)
  call dgetrf(n, n, a, n, ipiv, info)
  call dgecon('1', n, a, n, anorm, rcond, work, iwork, info)
  call begin_test('4x4_1norm')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

end program
