program test_zpoequ
  use test_utils
  implicit none
  integer, parameter :: MAXN = 10
  complex*16 :: a(MAXN*MAXN)
  double precision :: a_r(2*MAXN*MAXN)
  equivalence (a, a_r)
  double precision :: s(MAXN)
  double precision :: scond, amax
  integer :: info, n

  ! Test 1: 3x3 HPD matrix
  ! A = [[4, 1+i, 0], [1-i, 9, 1], [0, 1, 16]]
  n = 3
  a = (0.0d0, 0.0d0)
  a(1) = (4.0d0, 0.0d0);  a(4) = (1.0d0, 1.0d0);  a(7) = (0.0d0, 0.0d0)
  a(2) = (1.0d0, -1.0d0); a(5) = (9.0d0, 0.0d0);  a(8) = (1.0d0, 0.0d0)
  a(3) = (0.0d0, 0.0d0);  a(6) = (1.0d0, 0.0d0);  a(9) = (16.0d0, 0.0d0)
  call zpoequ(n, a, n, s, scond, amax, info)
  call begin_test('basic')
  call print_array('s', s, n)
  call print_scalar('scond', scond)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

  ! Test 2: diagonal matrix with varied scales
  n = 3
  a = (0.0d0, 0.0d0)
  a(1) = (100.0d0, 0.0d0); a(5) = (1.0d0, 0.0d0); a(9) = (0.25d0, 0.0d0)
  call zpoequ(n, a, n, s, scond, amax, info)
  call begin_test('diagonal_varied')
  call print_array('s', s, n)
  call print_scalar('scond', scond)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

  ! Test 3: non-positive diagonal (info > 0)
  n = 3
  a = (0.0d0, 0.0d0)
  a(1) = (4.0d0, 0.0d0); a(5) = (-1.0d0, 0.0d0); a(9) = (9.0d0, 0.0d0)
  call zpoequ(n, a, n, s, scond, amax, info)
  call begin_test('non_positive_diag')
  call print_int('info', info)
  call end_test()

  ! Test 4: zero diagonal element
  n = 3
  a = (0.0d0, 0.0d0)
  a(1) = (4.0d0, 0.0d0); a(5) = (0.0d0, 0.0d0); a(9) = (9.0d0, 0.0d0)
  call zpoequ(n, a, n, s, scond, amax, info)
  call begin_test('zero_diag')
  call print_int('info', info)
  call end_test()

  ! Test 5: n=0
  n = 0
  call zpoequ(n, a, 1, s, scond, amax, info)
  call begin_test('n_zero')
  call print_scalar('scond', scond)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

  ! Test 6: n=1
  n = 1
  a(1) = (25.0d0, 0.0d0)
  call zpoequ(n, a, n, s, scond, amax, info)
  call begin_test('n_one')
  call print_array('s', s, n)
  call print_scalar('scond', scond)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

  ! Test 7: identity matrix
  n = 3
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0); a(5) = (1.0d0, 0.0d0); a(9) = (1.0d0, 0.0d0)
  call zpoequ(n, a, n, s, scond, amax, info)
  call begin_test('identity')
  call print_array('s', s, n)
  call print_scalar('scond', scond)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

end program
