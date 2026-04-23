program test_dlarfg
  use test_utils
  implicit none

  double precision :: x(10), alpha, tau

  ! Test 1: basic
  alpha = 3.0d0
  x(1) = 4.0d0; x(2) = 0.0d0; x(3) = 0.0d0
  tau = 0.0d0
  call DLARFG(4, alpha, x, 1, tau)
  call begin_test('basic')
  call print_scalar('alpha', alpha)
  call print_scalar('tau', tau)
  call print_array('x', x, 3)
  call end_test()

  ! Test 2: alpha=0 (x nonzero)
  alpha = 0.0d0
  x(1) = 3.0d0; x(2) = 4.0d0
  tau = 0.0d0
  call DLARFG(3, alpha, x, 1, tau)
  call begin_test('alpha_zero')
  call print_scalar('alpha', alpha)
  call print_scalar('tau', tau)
  call print_array('x', x, 2)
  call end_test()

  ! Test 3: n=1 (tau=0)
  alpha = 5.0d0
  tau = 0.0d0
  call DLARFG(1, alpha, x, 1, tau)
  call begin_test('n_one')
  call print_scalar('alpha', alpha)
  call print_scalar('tau', tau)
  call end_test()

  ! Test 4: x all zero (tau=0)
  alpha = 5.0d0
  x(1) = 0.0d0; x(2) = 0.0d0
  tau = 0.0d0
  call DLARFG(3, alpha, x, 1, tau)
  call begin_test('x_all_zero')
  call print_scalar('alpha', alpha)
  call print_scalar('tau', tau)
  call end_test()

  ! Test 5: negative alpha
  alpha = -3.0d0
  x(1) = 4.0d0
  tau = 0.0d0
  call DLARFG(2, alpha, x, 1, tau)
  call begin_test('negative_alpha')
  call print_scalar('alpha', alpha)
  call print_scalar('tau', tau)
  call print_array('x', x, 1)
  call end_test()

  ! Test 6: n=0
  alpha = 5.0d0
  tau = 99.0d0
  call DLARFG(0, alpha, x, 1, tau)
  call begin_test('n_zero')
  call print_scalar('alpha', alpha)
  call print_scalar('tau', tau)
  call end_test()

end program
