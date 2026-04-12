program test_dlartgs
  use test_utils
  implicit none
  double precision :: x, y, sigma, cs, sn
  double precision :: out(2)

  ! Test 1: nominal case
  x = 3.0d0
  y = 4.0d0
  sigma = 1.5d0
  call dlartgs(x, y, sigma, cs, sn)
  out(1) = cs
  out(2) = sn
  call begin_test('nominal')
  call print_array('out', out, 2)
  call end_test()

  ! Test 2: sigma = 0, positive x
  x = 2.0d0
  y = 3.0d0
  sigma = 0.0d0
  call dlartgs(x, y, sigma, cs, sn)
  out(1) = cs
  out(2) = sn
  call begin_test('sigma_zero_pos')
  call print_array('out', out, 2)
  call end_test()

  ! Test 3: sigma = 0, negative x
  x = -2.0d0
  y = 3.0d0
  sigma = 0.0d0
  call dlartgs(x, y, sigma, cs, sn)
  out(1) = cs
  out(2) = sn
  call begin_test('sigma_zero_neg')
  call print_array('out', out, 2)
  call end_test()

  ! Test 4: |x| < thresh (x effectively zero)
  x = 1.0d-20
  y = 2.0d0
  sigma = 1.5d0
  call dlartgs(x, y, sigma, cs, sn)
  out(1) = cs
  out(2) = sn
  call begin_test('x_tiny')
  call print_array('out', out, 2)
  call end_test()

  ! Test 5: sigma = 0 AND |x| < thresh (first branch, both zero)
  x = 1.0d-20
  y = 3.0d0
  sigma = 0.0d0
  call dlartgs(x, y, sigma, cs, sn)
  out(1) = cs
  out(2) = sn
  call begin_test('sigma_zero_x_tiny')
  call print_array('out', out, 2)
  call end_test()

  ! Test 6: |x| == sigma and y == 0 (first branch, rotation by PI/2)
  x = 2.5d0
  y = 0.0d0
  sigma = 2.5d0
  call dlartgs(x, y, sigma, cs, sn)
  out(1) = cs
  out(2) = sn
  call begin_test('x_eq_sigma_y_zero')
  call print_array('out', out, 2)
  call end_test()

  ! Test 7: negative x with nonzero sigma (else branch, s = -1)
  x = -3.0d0
  y = 2.0d0
  sigma = 1.0d0
  call dlartgs(x, y, sigma, cs, sn)
  out(1) = cs
  out(2) = sn
  call begin_test('neg_x_sigma')
  call print_array('out', out, 2)
  call end_test()

  ! Test 8: positive x with nonzero sigma (else branch, s = 1)
  x = 5.0d0
  y = 1.0d0
  sigma = 2.0d0
  call dlartgs(x, y, sigma, cs, sn)
  out(1) = cs
  out(2) = sn
  call begin_test('pos_x_sigma')
  call print_array('out', out, 2)
  call end_test()

  ! Test 9: sigma = 0, negative x with negative y
  x = -4.0d0
  y = -3.0d0
  sigma = 0.0d0
  call dlartgs(x, y, sigma, cs, sn)
  out(1) = cs
  out(2) = sn
  call begin_test('sigma_zero_negxy')
  call print_array('out', out, 2)
  call end_test()

end program
