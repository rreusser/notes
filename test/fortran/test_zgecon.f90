program test_zgecon
  use test_utils
  implicit none

  integer, parameter :: MAXN = 10
  complex*16 :: a(MAXN*MAXN), work(2*MAXN)
  double precision :: a_r(2*MAXN*MAXN), work_r(4*MAXN)
  equivalence (a, a_r)
  equivalence (work, work_r)
  double precision :: rwork(2*MAXN), anorm, rcond
  integer :: ipiv(MAXN), info, n
  double precision :: zlange

  ! Test 1: 3x3 well-conditioned hermitian diag-dominant (1-norm)
  ! A = [[4+0i, 1+1i, 0], [1-1i, 3+0i, 1+0i], [0, 1+0i, 2+0i]]
  n = 3
  a = (0.0d0, 0.0d0)
  a(1) = (4.0d0, 0.0d0); a(2) = (1.0d0, -1.0d0); a(3) = (0.0d0, 0.0d0)
  a(4) = (1.0d0, 1.0d0); a(5) = (3.0d0, 0.0d0);  a(6) = (1.0d0, 0.0d0)
  a(7) = (0.0d0, 0.0d0); a(8) = (1.0d0, 0.0d0);  a(9) = (2.0d0, 0.0d0)
  anorm = zlange('1', n, n, a, n, rwork)
  call zgetrf(n, n, a, n, ipiv, info)
  call zgecon('1', n, a, n, anorm, rcond, work, rwork, info)
  call begin_test('well_cond_1norm')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 2: same matrix with infinity-norm
  a = (0.0d0, 0.0d0)
  a(1) = (4.0d0, 0.0d0); a(2) = (1.0d0, -1.0d0); a(3) = (0.0d0, 0.0d0)
  a(4) = (1.0d0, 1.0d0); a(5) = (3.0d0, 0.0d0);  a(6) = (1.0d0, 0.0d0)
  a(7) = (0.0d0, 0.0d0); a(8) = (1.0d0, 0.0d0);  a(9) = (2.0d0, 0.0d0)
  anorm = zlange('I', n, n, a, n, rwork)
  call zgetrf(n, n, a, n, ipiv, info)
  call zgecon('I', n, a, n, anorm, rcond, work, rwork, info)
  call begin_test('well_cond_Inorm')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 3: 3x3 identity (rcond=1)
  n = 3
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0); a(5) = (1.0d0, 0.0d0); a(9) = (1.0d0, 0.0d0)
  anorm = zlange('1', n, n, a, n, rwork)
  call zgetrf(n, n, a, n, ipiv, info)
  call zgecon('1', n, a, n, anorm, rcond, work, rwork, info)
  call begin_test('identity')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 4: 3x3 ill-conditioned
  n = 3
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0); a(5) = (1.0d0, 0.0d0); a(9) = (1.0d-15, 0.0d0)
  anorm = zlange('1', n, n, a, n, rwork)
  call zgetrf(n, n, a, n, ipiv, info)
  call zgecon('1', n, a, n, anorm, rcond, work, rwork, info)
  call begin_test('ill_cond')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 5: N=0 (rcond=1)
  call zgecon('1', 0, a, 1, 0.0d0, rcond, work, rwork, info)
  call begin_test('n_zero')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 6: anorm=0 (rcond=0)
  n = 3
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0); a(5) = (1.0d0, 0.0d0); a(9) = (1.0d0, 0.0d0)
  call zgetrf(n, n, a, n, ipiv, info)
  call zgecon('1', n, a, n, 0.0d0, rcond, work, rwork, info)
  call begin_test('anorm_zero')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 7: 4x4 complex matrix (1-norm)
  n = 4
  a = (0.0d0, 0.0d0)
  a(1)  = (5.0d0, 1.0d0);  a(2)  = (1.0d0, 0.0d0);  a(3)  = (0.0d0, 0.0d0);  a(4)  = (0.0d0, 0.0d0)
  a(5)  = (1.0d0, 0.0d0);  a(6)  = (4.0d0, -1.0d0); a(7)  = (1.0d0, 1.0d0);  a(8)  = (0.0d0, 0.0d0)
  a(9)  = (0.0d0, 0.0d0);  a(10) = (1.0d0, -1.0d0); a(11) = (3.0d0, 0.0d0);  a(12) = (1.0d0, 0.0d0)
  a(13) = (0.0d0, 0.0d0);  a(14) = (0.0d0, 0.0d0);  a(15) = (1.0d0, 0.0d0);  a(16) = (2.0d0, 1.0d0)
  anorm = zlange('1', n, n, a, n, rwork)
  call zgetrf(n, n, a, n, ipiv, info)
  call zgecon('1', n, a, n, anorm, rcond, work, rwork, info)
  call begin_test('4x4_1norm')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 8: 4x4 complex matrix (inf-norm)
  a = (0.0d0, 0.0d0)
  a(1)  = (5.0d0, 1.0d0);  a(2)  = (1.0d0, 0.0d0);  a(3)  = (0.0d0, 0.0d0);  a(4)  = (0.0d0, 0.0d0)
  a(5)  = (1.0d0, 0.0d0);  a(6)  = (4.0d0, -1.0d0); a(7)  = (1.0d0, 1.0d0);  a(8)  = (0.0d0, 0.0d0)
  a(9)  = (0.0d0, 0.0d0);  a(10) = (1.0d0, -1.0d0); a(11) = (3.0d0, 0.0d0);  a(12) = (1.0d0, 0.0d0)
  a(13) = (0.0d0, 0.0d0);  a(14) = (0.0d0, 0.0d0);  a(15) = (1.0d0, 0.0d0);  a(16) = (2.0d0, 1.0d0)
  anorm = zlange('I', n, n, a, n, rwork)
  call zgetrf(n, n, a, n, ipiv, info)
  call zgecon('I', n, a, n, anorm, rcond, work, rwork, info)
  call begin_test('4x4_Inorm')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

end program
