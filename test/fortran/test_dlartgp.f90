program test_dlartgp
  use test_utils
  implicit none
  double precision :: f, g, cs, sn, r
  double precision :: out(3)

  ! Test 1: basic rotation, f=3, g=4
  f = 3.0d0
  g = 4.0d0
  call dlartgp(f, g, cs, sn, r)
  out(1) = cs
  out(2) = sn
  out(3) = r
  call begin_test('basic')
  call print_array('out', out, 3)
  call end_test()

  ! Test 2: g = 0
  f = 2.5d0
  g = 0.0d0
  call dlartgp(f, g, cs, sn, r)
  out(1) = cs
  out(2) = sn
  out(3) = r
  call begin_test('g_zero')
  call print_array('out', out, 3)
  call end_test()

  ! Test 3: g = 0 with negative f
  f = -2.5d0
  g = 0.0d0
  call dlartgp(f, g, cs, sn, r)
  out(1) = cs
  out(2) = sn
  out(3) = r
  call begin_test('g_zero_neg_f')
  call print_array('out', out, 3)
  call end_test()

  ! Test 4: f = 0
  f = 0.0d0
  g = 7.0d0
  call dlartgp(f, g, cs, sn, r)
  out(1) = cs
  out(2) = sn
  out(3) = r
  call begin_test('f_zero')
  call print_array('out', out, 3)
  call end_test()

  ! Test 5: f = 0 with negative g
  f = 0.0d0
  g = -7.0d0
  call dlartgp(f, g, cs, sn, r)
  out(1) = cs
  out(2) = sn
  out(3) = r
  call begin_test('f_zero_neg_g')
  call print_array('out', out, 3)
  call end_test()

  ! Test 6: negative f and g (R must be non-negative)
  f = -3.0d0
  g = -4.0d0
  call dlartgp(f, g, cs, sn, r)
  out(1) = cs
  out(2) = sn
  out(3) = r
  call begin_test('neg_f_neg_g')
  call print_array('out', out, 3)
  call end_test()

  ! Test 7: negative f, positive g
  f = -3.0d0
  g = 4.0d0
  call dlartgp(f, g, cs, sn, r)
  out(1) = cs
  out(2) = sn
  out(3) = r
  call begin_test('neg_f_pos_g')
  call print_array('out', out, 3)
  call end_test()

  ! Test 8: very large f (overflow scale branch)
  f = 1.0d200
  g = 2.0d200
  call dlartgp(f, g, cs, sn, r)
  out(1) = cs
  out(2) = sn
  out(3) = r
  call begin_test('large')
  call print_array('out', out, 3)
  call end_test()

  ! Test 9: very small values (underflow scale branch)
  f = 1.0d-200
  g = 2.0d-200
  call dlartgp(f, g, cs, sn, r)
  out(1) = cs
  out(2) = sn
  out(3) = r
  call begin_test('small')
  call print_array('out', out, 3)
  call end_test()

  ! Test 10: mixed magnitude
  f = 1.0d-150
  g = 1.0d150
  call dlartgp(f, g, cs, sn, r)
  out(1) = cs
  out(2) = sn
  out(3) = r
  call begin_test('mixed')
  call print_array('out', out, 3)
  call end_test()

  ! Test 11: f == g
  f = 1.0d0
  g = 1.0d0
  call dlartgp(f, g, cs, sn, r)
  out(1) = cs
  out(2) = sn
  out(3) = r
  call begin_test('equal')
  call print_array('out', out, 3)
  call end_test()

end program
