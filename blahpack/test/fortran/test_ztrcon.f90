program test_ztrcon
  use test_utils
  implicit none

  integer, parameter :: MAXN = 10
  complex*16 :: a(MAXN*MAXN), work(2*MAXN)
  double precision :: a_r(2*MAXN*MAXN), work_r(4*MAXN)
  equivalence (a, a_r)
  equivalence (work, work_r)
  double precision :: rwork(MAXN), rcond
  integer :: info, n

  ! Test 1: 3x3 upper triangular, non-unit, 1-norm
  ! A = [[4+i, 1+i, 0.5], [0, 3, 1-i], [0, 0, 2+i]]
  n = 3
  a = (0.0d0, 0.0d0)
  a(1) = (4.0d0, 1.0d0); a(4) = (1.0d0, 1.0d0); a(7) = (0.5d0, 0.0d0)
  a(5) = (3.0d0, 0.0d0); a(8) = (1.0d0, -1.0d0)
  a(9) = (2.0d0, 1.0d0)
  call ztrcon('1', 'U', 'N', n, a, n, rcond, work, rwork, info)
  call begin_test('upper_nonunit_1norm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 2: same matrix, inf-norm
  a = (0.0d0, 0.0d0)
  a(1) = (4.0d0, 1.0d0); a(4) = (1.0d0, 1.0d0); a(7) = (0.5d0, 0.0d0)
  a(5) = (3.0d0, 0.0d0); a(8) = (1.0d0, -1.0d0)
  a(9) = (2.0d0, 1.0d0)
  call ztrcon('I', 'U', 'N', n, a, n, rcond, work, rwork, info)
  call begin_test('upper_nonunit_Inorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 3: 3x3 lower triangular, non-unit, 1-norm
  ! A = [[3+i, 0, 0], [1, 4-i, 0], [0.5+i, 1-i, 2]]
  n = 3
  a = (0.0d0, 0.0d0)
  a(1) = (3.0d0, 1.0d0)
  a(2) = (1.0d0, 0.0d0); a(5) = (4.0d0, -1.0d0)
  a(3) = (0.5d0, 1.0d0); a(6) = (1.0d0, -1.0d0); a(9) = (2.0d0, 0.0d0)
  call ztrcon('1', 'L', 'N', n, a, n, rcond, work, rwork, info)
  call begin_test('lower_nonunit_1norm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 4: 3x3 upper triangular, unit diagonal, 1-norm
  ! A = [[1, 1+i, 0.5], [0, 1, 1-i], [0, 0, 1]] (unit diag)
  n = 3
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0); a(4) = (1.0d0, 1.0d0); a(7) = (0.5d0, 0.0d0)
  a(5) = (1.0d0, 0.0d0); a(8) = (1.0d0, -1.0d0)
  a(9) = (1.0d0, 0.0d0)
  call ztrcon('1', 'U', 'U', n, a, n, rcond, work, rwork, info)
  call begin_test('upper_unit_1norm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 5: 3x3 identity (rcond=1)
  n = 3
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0); a(5) = (1.0d0, 0.0d0); a(9) = (1.0d0, 0.0d0)
  call ztrcon('1', 'U', 'N', n, a, n, rcond, work, rwork, info)
  call begin_test('identity')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 6: N=0 (rcond=1)
  call ztrcon('1', 'U', 'N', 0, a, 1, rcond, work, rwork, info)
  call begin_test('n_zero')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 7: 4x4 lower triangular, inf-norm
  n = 4
  a = (0.0d0, 0.0d0)
  a(1) = (5.0d0, 1.0d0)
  a(2) = (1.0d0, 0.0d0); a(6) = (4.0d0, -1.0d0)
  a(3) = (0.0d0, 0.0d0); a(7) = (1.0d0, 1.0d0);  a(11) = (3.0d0, 0.0d0)
  a(4) = (0.0d0, 0.0d0); a(8) = (0.0d0, 0.0d0);  a(12) = (1.0d0, 0.0d0); a(16) = (2.0d0, 1.0d0)
  call ztrcon('I', 'L', 'N', n, a, n, rcond, work, rwork, info)
  call begin_test('4x4_lower_Inorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 8: lower, unit diagonal, inf-norm
  n = 3
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0)
  a(2) = (0.5d0, 0.5d0); a(5) = (1.0d0, 0.0d0)
  a(3) = (0.0d0, 0.0d0); a(6) = (0.5d0, -0.5d0); a(9) = (1.0d0, 0.0d0)
  call ztrcon('I', 'L', 'U', n, a, n, rcond, work, rwork, info)
  call begin_test('lower_unit_Inorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

end program
