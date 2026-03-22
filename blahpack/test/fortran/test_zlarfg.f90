program test_zlarfg
  use test_utils
  implicit none

  complex*16 :: alpha, tau, x(10)
  double precision :: alpha_real(2), tau_real(2), x_real(20)
  equivalence (alpha, alpha_real)
  equivalence (tau, tau_real)
  equivalence (x, x_real)

  ! Test 1: basic case, n=3, real alpha, real x
  alpha = (3.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 0.0d0)
  tau = (0.0d0, 0.0d0)
  call zlarfg(3, alpha, x, 1, tau)
  call begin_test('zlarfg_basic_real')
  call print_array('alpha', alpha_real, 2)
  call print_array('tau', tau_real, 2)
  call print_array('x', x_real, 4)
  call end_test()

  ! Test 2: complex alpha and x, n=3
  alpha = (2.0d0, 1.0d0)
  x(1) = (1.0d0, -1.0d0)
  x(2) = (0.5d0, 0.5d0)
  tau = (0.0d0, 0.0d0)
  call zlarfg(3, alpha, x, 1, tau)
  call begin_test('zlarfg_complex')
  call print_array('alpha', alpha_real, 2)
  call print_array('tau', tau_real, 2)
  call print_array('x', x_real, 4)
  call end_test()

  ! Test 3: n=1 (no x vector)
  alpha = (5.0d0, 3.0d0)
  tau = (0.0d0, 0.0d0)
  call zlarfg(1, alpha, x, 1, tau)
  call begin_test('zlarfg_n_one')
  call print_array('alpha', alpha_real, 2)
  call print_array('tau', tau_real, 2)
  call end_test()

  ! Test 4: n=0 (quick return)
  alpha = (5.0d0, 3.0d0)
  tau = (99.0d0, 99.0d0)
  call zlarfg(0, alpha, x, 1, tau)
  call begin_test('zlarfg_n_zero')
  call print_array('alpha', alpha_real, 2)
  call print_array('tau', tau_real, 2)
  call end_test()

  ! Test 5: x is zero, alpha is real => tau=0
  alpha = (4.0d0, 0.0d0)
  x(1) = (0.0d0, 0.0d0)
  x(2) = (0.0d0, 0.0d0)
  tau = (0.0d0, 0.0d0)
  call zlarfg(3, alpha, x, 1, tau)
  call begin_test('zlarfg_x_zero_alpha_real')
  call print_array('alpha', alpha_real, 2)
  call print_array('tau', tau_real, 2)
  call end_test()

  ! Test 6: x is zero, alpha has imaginary part => non-trivial
  alpha = (4.0d0, 3.0d0)
  x(1) = (0.0d0, 0.0d0)
  x(2) = (0.0d0, 0.0d0)
  tau = (0.0d0, 0.0d0)
  call zlarfg(3, alpha, x, 1, tau)
  call begin_test('zlarfg_x_zero_alpha_complex')
  call print_array('alpha', alpha_real, 2)
  call print_array('tau', tau_real, 2)
  call end_test()

  ! Test 7: stride=2
  x(1) = (1.0d0, 2.0d0)
  x(2) = (99.0d0, 99.0d0)
  x(3) = (3.0d0, 4.0d0)
  x(4) = (99.0d0, 99.0d0)
  alpha = (2.0d0, -1.0d0)
  tau = (0.0d0, 0.0d0)
  call zlarfg(3, alpha, x, 2, tau)
  call begin_test('zlarfg_stride2')
  call print_array('alpha', alpha_real, 2)
  call print_array('tau', tau_real, 2)
  call print_array('x', x_real, 8)
  call end_test()

  ! Test 8: larger case n=5
  alpha = (1.0d0, 1.0d0)
  x(1) = (2.0d0, 3.0d0)
  x(2) = (4.0d0, 5.0d0)
  x(3) = (6.0d0, 7.0d0)
  x(4) = (8.0d0, 9.0d0)
  tau = (0.0d0, 0.0d0)
  call zlarfg(5, alpha, x, 1, tau)
  call begin_test('zlarfg_larger')
  call print_array('alpha', alpha_real, 2)
  call print_array('tau', tau_real, 2)
  call print_array('x', x_real, 8)
  call end_test()

  ! Test 9: very small values that trigger the rescaling loop
  ! safmin = dlamch('S')/dlamch('E') ~ 2.2e-308 / 1.1e-16 ~ 2.0e-292
  ! beta needs to be < safmin to trigger, so use alpha ~ 1e-310
  alpha = (1.0d-310, 0.0d0)
  x(1) = (1.0d-310, 0.0d0)
  x(2) = (1.0d-310, 0.0d0)
  tau = (0.0d0, 0.0d0)
  call zlarfg(3, alpha, x, 1, tau)
  call begin_test('zlarfg_rescaling')
  call print_array('alpha', alpha_real, 2)
  call print_array('tau', tau_real, 2)
  call print_array('x', x_real, 4)
  call end_test()

end program
