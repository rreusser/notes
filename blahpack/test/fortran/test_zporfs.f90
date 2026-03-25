program test_zporfs
  use test_utils
  implicit none

  integer, parameter :: MAXN = 10
  complex*16 :: a(MAXN*MAXN), af(MAXN*MAXN), b(MAXN*MAXN), x(MAXN*MAXN)
  complex*16 :: work(2*MAXN)
  double precision :: a_r(2*MAXN*MAXN), af_r(2*MAXN*MAXN)
  double precision :: b_r(2*MAXN*MAXN), x_r(2*MAXN*MAXN)
  equivalence (a, a_r)
  equivalence (af, af_r)
  equivalence (b, b_r)
  equivalence (x, x_r)
  double precision :: ferr(MAXN), berr(MAXN), rwork(MAXN)
  integer :: info, n, nrhs

  ! ============================================================
  ! Test 1: basic 3x3 upper, single RHS
  ! A = [[4, 1+i, 0], [1-i, 3, 1], [0, 1, 2]]  (HPD)
  ! ============================================================
  n = 3
  nrhs = 1
  a = (0.0d0, 0.0d0)
  a(1) = (4.0d0, 0.0d0);  a(4) = (1.0d0, 1.0d0);  a(7) = (0.0d0, 0.0d0)
  a(2) = (1.0d0, -1.0d0); a(5) = (3.0d0, 0.0d0);  a(8) = (1.0d0, 0.0d0)
  a(3) = (0.0d0, 0.0d0);  a(6) = (1.0d0, 0.0d0);  a(9) = (2.0d0, 0.0d0)

  b(1) = (1.0d0, 0.0d0); b(2) = (1.0d0, 0.0d0); b(3) = (1.0d0, 0.0d0)

  af = a
  call zpotrf('U', n, af, n, info)
  if (info /= 0) stop 'zpotrf failed'

  x(1:n) = b(1:n)
  call zpotrs('U', n, nrhs, af, n, x, n, info)
  if (info /= 0) stop 'zpotrs failed'

  call zporfs('U', n, nrhs, a, n, af, n, b, n, x, n, ferr, berr, &
              work, rwork, info)
  call begin_test('basic_upper_3x3')
  call print_int('info', info)
  call print_array('x', x_r, 2*n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call end_test()

  ! ============================================================
  ! Test 2: basic 3x3 lower, single RHS
  ! ============================================================
  n = 3
  nrhs = 1
  a = (0.0d0, 0.0d0)
  a(1) = (4.0d0, 0.0d0);  a(4) = (1.0d0, 1.0d0);  a(7) = (0.0d0, 0.0d0)
  a(2) = (1.0d0, -1.0d0); a(5) = (3.0d0, 0.0d0);  a(8) = (1.0d0, 0.0d0)
  a(3) = (0.0d0, 0.0d0);  a(6) = (1.0d0, 0.0d0);  a(9) = (2.0d0, 0.0d0)

  b(1) = (1.0d0, 0.0d0); b(2) = (1.0d0, 0.0d0); b(3) = (1.0d0, 0.0d0)

  af = a
  call zpotrf('L', n, af, n, info)
  if (info /= 0) stop 'zpotrf failed'

  x(1:n) = b(1:n)
  call zpotrs('L', n, nrhs, af, n, x, n, info)
  if (info /= 0) stop 'zpotrs failed'

  call zporfs('L', n, nrhs, a, n, af, n, b, n, x, n, ferr, berr, &
              work, rwork, info)
  call begin_test('basic_lower_3x3')
  call print_int('info', info)
  call print_array('x', x_r, 2*n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call end_test()

  ! ============================================================
  ! Test 3: multiple RHS (3x3 upper, 2 columns)
  ! ============================================================
  n = 3
  nrhs = 2
  a = (0.0d0, 0.0d0)
  a(1) = (4.0d0, 0.0d0);  a(4) = (1.0d0, 1.0d0);  a(7) = (0.0d0, 0.0d0)
  a(2) = (1.0d0, -1.0d0); a(5) = (3.0d0, 0.0d0);  a(8) = (1.0d0, 0.0d0)
  a(3) = (0.0d0, 0.0d0);  a(6) = (1.0d0, 0.0d0);  a(9) = (2.0d0, 0.0d0)

  b(1) = (1.0d0, 0.0d0); b(2) = (2.0d0, 1.0d0); b(3) = (3.0d0, 0.0d0)
  b(4) = (4.0d0, 0.0d0); b(5) = (5.0d0, -1.0d0); b(6) = (6.0d0, 0.0d0)

  af = a
  call zpotrf('U', n, af, n, info)
  if (info /= 0) stop 'zpotrf failed'

  x = b
  call zpotrs('U', n, nrhs, af, n, x, n, info)
  if (info /= 0) stop 'zpotrs failed'

  call zporfs('U', n, nrhs, a, n, af, n, b, n, x, n, ferr, berr, &
              work, rwork, info)
  call begin_test('multi_rhs_3x3')
  call print_int('info', info)
  call print_array('x', x_r, 2*n*nrhs)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call end_test()

  ! ============================================================
  ! Test 4: N=0 quick return
  ! ============================================================
  call zporfs('U', 0, 1, a, 1, af, 1, b, 1, x, 1, ferr, berr, &
              work, rwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

  ! ============================================================
  ! Test 5: NRHS=0 quick return
  ! ============================================================
  call zporfs('U', 3, 0, a, 3, af, 3, b, 3, x, 3, ferr, berr, &
              work, rwork, info)
  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

end program
