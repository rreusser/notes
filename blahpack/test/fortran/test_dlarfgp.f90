program test_dlarfgp
  use test_utils
  implicit none

  double precision :: x(10), alpha, tau

  ! Test 1: basic
  alpha = 3.0d0
  x(1) = 4.0d0; x(2) = 0.0d0; x(3) = 0.0d0
  tau = 0.0d0
  call DLARFGP(4, alpha, x, 1, tau)
  call begin_test('basic')
  call print_scalar('alpha', alpha)
  call print_scalar('tau', tau)
  call print_array('x', x, 3)
  call end_test()

  ! Test 2: alpha=0 (x nonzero)
  alpha = 0.0d0
  x(1) = 3.0d0; x(2) = 4.0d0
  tau = 0.0d0
  call DLARFGP(3, alpha, x, 1, tau)
  call begin_test('alpha_zero')
  call print_scalar('alpha', alpha)
  call print_scalar('tau', tau)
  call print_array('x', x, 2)
  call end_test()

  ! Test 3: n=1 (tau=0)
  alpha = 5.0d0
  tau = 0.0d0
  call DLARFGP(1, alpha, x, 1, tau)
  call begin_test('n_one')
  call print_scalar('alpha', alpha)
  call print_scalar('tau', tau)
  call end_test()

  ! Test 4: x all zero, alpha positive (tau=0)
  alpha = 5.0d0
  x(1) = 0.0d0; x(2) = 0.0d0
  tau = 0.0d0
  call DLARFGP(3, alpha, x, 1, tau)
  call begin_test('x_zero_pos_alpha')
  call print_scalar('alpha', alpha)
  call print_scalar('tau', tau)
  call end_test()

  ! Test 5: x all zero, alpha negative (tau=2, alpha negated)
  alpha = -5.0d0
  x(1) = 0.0d0; x(2) = 0.0d0
  tau = 0.0d0
  call DLARFGP(3, alpha, x, 1, tau)
  call begin_test('x_zero_neg_alpha')
  call print_scalar('alpha', alpha)
  call print_scalar('tau', tau)
  call end_test()

  ! Test 6: negative alpha, x nonzero
  alpha = -3.0d0
  x(1) = 4.0d0
  tau = 0.0d0
  call DLARFGP(2, alpha, x, 1, tau)
  call begin_test('negative_alpha')
  call print_scalar('alpha', alpha)
  call print_scalar('tau', tau)
  call print_array('x', x, 1)
  call end_test()

  ! Test 7: n=0
  alpha = 5.0d0
  tau = 99.0d0
  call DLARFGP(0, alpha, x, 1, tau)
  call begin_test('n_zero')
  call print_scalar('alpha', alpha)
  call print_scalar('tau', tau)
  call end_test()

  ! Test 8: stride 2
  alpha = 2.0d0
  x(1) = 1.0d0; x(2) = 99.0d0; x(3) = 2.0d0; x(4) = 99.0d0; x(5) = 2.0d0
  tau = 0.0d0
  call DLARFGP(4, alpha, x, 2, tau)
  call begin_test('stride2')
  call print_scalar('alpha', alpha)
  call print_scalar('tau', tau)
  call print_array('x', x, 5)
  call end_test()

  ! Test 9: larger n
  alpha = 1.5d0
  x(1) = 0.5d0; x(2) = -0.25d0; x(3) = 0.75d0; x(4) = -0.125d0; x(5) = 0.0625d0
  tau = 0.0d0
  call DLARFGP(6, alpha, x, 1, tau)
  call begin_test('larger_n')
  call print_scalar('alpha', alpha)
  call print_scalar('tau', tau)
  call print_array('x', x, 5)
  call end_test()

end program
