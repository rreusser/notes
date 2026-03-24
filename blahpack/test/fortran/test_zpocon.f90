program test_zpocon
  use test_utils
  implicit none

  integer, parameter :: MAXN = 10
  complex*16 :: a(MAXN*MAXN), work(2*MAXN)
  double precision :: a_r(2*MAXN*MAXN), work_r(4*MAXN)
  equivalence (a, a_r)
  equivalence (work, work_r)
  double precision :: rwork(MAXN), anorm, rcond
  integer :: info, n
  double precision :: zlanhe

  ! Test 1: 3x3 HPD matrix, upper, 1-norm
  ! A = [[4, 1+i, 0], [1-i, 3, 1], [0, 1, 2]]
  n = 3
  a = (0.0d0, 0.0d0)
  a(1) = (4.0d0, 0.0d0); a(4) = (1.0d0, 1.0d0); a(7) = (0.0d0, 0.0d0)
  a(2) = (1.0d0, -1.0d0); a(5) = (3.0d0, 0.0d0); a(8) = (1.0d0, 0.0d0)
  a(3) = (0.0d0, 0.0d0); a(6) = (1.0d0, 0.0d0); a(9) = (2.0d0, 0.0d0)
  anorm = zlanhe('1', 'U', n, a, n, rwork)
  call zpotrf('U', n, a, n, info)
  call zpocon('U', n, a, n, anorm, rcond, work, rwork, info)
  call begin_test('upper_1norm')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 2: same matrix, lower storage
  a = (0.0d0, 0.0d0)
  a(1) = (4.0d0, 0.0d0); a(4) = (1.0d0, 1.0d0); a(7) = (0.0d0, 0.0d0)
  a(2) = (1.0d0, -1.0d0); a(5) = (3.0d0, 0.0d0); a(8) = (1.0d0, 0.0d0)
  a(3) = (0.0d0, 0.0d0); a(6) = (1.0d0, 0.0d0); a(9) = (2.0d0, 0.0d0)
  anorm = zlanhe('1', 'L', n, a, n, rwork)
  call zpotrf('L', n, a, n, info)
  call zpocon('L', n, a, n, anorm, rcond, work, rwork, info)
  call begin_test('lower_1norm')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 3: 3x3 identity (rcond=1)
  n = 3
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0); a(5) = (1.0d0, 0.0d0); a(9) = (1.0d0, 0.0d0)
  anorm = zlanhe('1', 'U', n, a, n, rwork)
  call zpotrf('U', n, a, n, info)
  call zpocon('U', n, a, n, anorm, rcond, work, rwork, info)
  call begin_test('identity')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 4: N=0 (rcond=1)
  call zpocon('U', 0, a, 1, 0.0d0, rcond, work, rwork, info)
  call begin_test('n_zero')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 5: anorm=0 (rcond=0)
  n = 3
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0); a(5) = (1.0d0, 0.0d0); a(9) = (1.0d0, 0.0d0)
  call zpotrf('U', n, a, n, info)
  call zpocon('U', n, a, n, 0.0d0, rcond, work, rwork, info)
  call begin_test('anorm_zero')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 6: 4x4 HPD matrix, upper
  ! A = [[5, 1+i, 0, 0], [1-i, 4, 1+i, 0], [0, 1-i, 3, 1], [0, 0, 1, 2]]
  n = 4
  a = (0.0d0, 0.0d0)
  a(1)  = (5.0d0, 0.0d0); a(5)  = (1.0d0, 1.0d0);  a(9)  = (0.0d0, 0.0d0);  a(13) = (0.0d0, 0.0d0)
  a(2)  = (1.0d0,-1.0d0); a(6)  = (4.0d0, 0.0d0);  a(10) = (1.0d0, 1.0d0);  a(14) = (0.0d0, 0.0d0)
  a(3)  = (0.0d0, 0.0d0); a(7)  = (1.0d0,-1.0d0);  a(11) = (3.0d0, 0.0d0);  a(15) = (1.0d0, 0.0d0)
  a(4)  = (0.0d0, 0.0d0); a(8)  = (0.0d0, 0.0d0);  a(12) = (1.0d0, 0.0d0);  a(16) = (2.0d0, 0.0d0)
  anorm = zlanhe('1', 'U', n, a, n, rwork)
  call zpotrf('U', n, a, n, info)
  call zpocon('U', n, a, n, anorm, rcond, work, rwork, info)
  call begin_test('4x4_upper')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 7: 4x4 HPD matrix, lower
  a = (0.0d0, 0.0d0)
  a(1)  = (5.0d0, 0.0d0); a(5)  = (1.0d0, 1.0d0);  a(9)  = (0.0d0, 0.0d0);  a(13) = (0.0d0, 0.0d0)
  a(2)  = (1.0d0,-1.0d0); a(6)  = (4.0d0, 0.0d0);  a(10) = (1.0d0, 1.0d0);  a(14) = (0.0d0, 0.0d0)
  a(3)  = (0.0d0, 0.0d0); a(7)  = (1.0d0,-1.0d0);  a(11) = (3.0d0, 0.0d0);  a(15) = (1.0d0, 0.0d0)
  a(4)  = (0.0d0, 0.0d0); a(8)  = (0.0d0, 0.0d0);  a(12) = (1.0d0, 0.0d0);  a(16) = (2.0d0, 0.0d0)
  anorm = zlanhe('1', 'L', n, a, n, rwork)
  call zpotrf('L', n, a, n, info)
  call zpocon('L', n, a, n, anorm, rcond, work, rwork, info)
  call begin_test('4x4_lower')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

end program
