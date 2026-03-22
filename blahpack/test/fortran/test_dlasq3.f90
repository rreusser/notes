program test_dlasq3
  use test_utils
  implicit none

  double precision :: z(80), dmin, dmin1, dmin2, dn, dn1, dn2
  double precision :: sigma, desig, qmax, tau, g
  integer :: i0, n0, pp, nfail, iter, ndiv, ttype
  logical :: ieee

  ! ============================================================
  ! Test 1: N0 < I0 — immediate return
  ! ============================================================
  z = 0.0d0
  z(1) = 4.0d0; z(2) = 1.0d0; z(3) = 4.0d0; z(4) = 1.0d0

  i0 = 2; n0 = 1; pp = 0
  dmin = 0.5d0; sigma = 0.0d0; desig = 0.0d0; qmax = 4.0d0
  nfail = 0; iter = 0; ndiv = 0; ieee = .true.
  ttype = 0; dmin1 = 0.5d0; dmin2 = 0.5d0
  dn = 0.5d0; dn1 = 0.5d0; dn2 = 0.5d0; g = 0.0d0; tau = 0.0d0

  call dlasq3(i0, n0, z, pp, dmin, sigma, desig, qmax, nfail, &
              iter, ndiv, ieee, ttype, dmin1, dmin2, dn, dn1, dn2, g, tau)

  call begin_test('n0_lt_i0')
  call print_int('n0', n0)
  call print_int('pp', pp)
  call print_scalar('sigma', sigma)
  call print_scalar('desig', desig)
  call print_int('nfail', nfail)
  call print_int('iter', iter)
  call print_int('ndiv', ndiv)
  call end_test()

  ! ============================================================
  ! Test 2: N0 == I0 — deflate single eigenvalue
  ! ============================================================
  z = 0.0d0
  z(1) = 4.0d0; z(2) = 1.0d0; z(3) = 4.0d0; z(4) = 1.0d0

  i0 = 1; n0 = 1; pp = 0
  dmin = 0.5d0; sigma = 1.0d0; desig = 0.0d0; qmax = 4.0d0
  nfail = 0; iter = 0; ndiv = 0; ieee = .true.
  ttype = 0; dmin1 = 0.5d0; dmin2 = 0.5d0
  dn = 0.5d0; dn1 = 0.5d0; dn2 = 0.5d0; g = 0.0d0; tau = 0.0d0

  call dlasq3(i0, n0, z, pp, dmin, sigma, desig, qmax, nfail, &
              iter, ndiv, ieee, ttype, dmin1, dmin2, dn, dn1, dn2, g, tau)

  call begin_test('n0_eq_i0')
  call print_int('n0', n0)
  call print_scalar('z1', z(1))
  call print_scalar('sigma', sigma)
  call print_scalar('desig', desig)
  call print_int('iter', iter)
  call end_test()

  ! ============================================================
  ! Test 3: N0 == I0+1 — 2-eigenvalue deflation
  ! ============================================================
  z = 0.0d0
  ! Set up a 2-element qd array (pp=0)
  z(1) = 4.0d0; z(2) = 0.5d0; z(3) = 4.0d0; z(4) = 0.5d0
  z(5) = 3.0d0; z(6) = 0.0d0; z(7) = 3.0d0; z(8) = 0.0d0

  i0 = 1; n0 = 2; pp = 0
  dmin = 0.5d0; sigma = 1.0d0; desig = 0.0d0; qmax = 4.0d0
  nfail = 0; iter = 0; ndiv = 0; ieee = .true.
  ttype = 0; dmin1 = 0.5d0; dmin2 = 0.5d0
  dn = 3.0d0; dn1 = 4.0d0; dn2 = 0.5d0; g = 0.0d0; tau = 0.0d0

  call dlasq3(i0, n0, z, pp, dmin, sigma, desig, qmax, nfail, &
              iter, ndiv, ieee, ttype, dmin1, dmin2, dn, dn1, dn2, g, tau)

  call begin_test('n0_eq_i0_plus_1')
  call print_int('n0', n0)
  call print_scalar('sigma', sigma)
  call print_scalar('desig', desig)
  call print_array('z', z, 8)
  call print_int('iter', iter)
  call end_test()

  ! ============================================================
  ! Test 4: Basic N=4 with IEEE arithmetic, pp=0, positive data
  ! This exercises the main dqds path: label 50 → dlasq4 → dlasq5
  ! ============================================================
  z = 0.0d0
  z(1)  = 4.0d0;  z(2)  = 1.0d0;  z(3)  = 4.0d0;  z(4)  = 1.0d0
  z(5)  = 3.0d0;  z(6)  = 0.5d0;  z(7)  = 3.0d0;  z(8)  = 0.5d0
  z(9)  = 2.0d0;  z(10) = 0.3d0;  z(11) = 2.0d0;  z(12) = 0.3d0
  z(13) = 1.0d0;  z(14) = 0.0d0;  z(15) = 1.0d0;  z(16) = 0.0d0

  i0 = 1; n0 = 4; pp = 0
  dmin = 1.0d0; sigma = 0.0d0; desig = 0.0d0; qmax = 4.0d0
  nfail = 0; iter = 0; ndiv = 0; ieee = .true.
  ttype = 0; dmin1 = 1.0d0; dmin2 = 2.0d0
  dn = 1.0d0; dn1 = 2.0d0; dn2 = 3.0d0; g = 0.0d0; tau = 0.0d0

  call dlasq3(i0, n0, z, pp, dmin, sigma, desig, qmax, nfail, &
              iter, ndiv, ieee, ttype, dmin1, dmin2, dn, dn1, dn2, g, tau)

  call begin_test('basic_n4_pp0')
  call print_int('n0', n0)
  call print_int('pp', pp)
  call print_scalar('dmin', dmin)
  call print_scalar('sigma', sigma)
  call print_scalar('desig', desig)
  call print_scalar('qmax', qmax)
  call print_int('nfail', nfail)
  call print_int('iter', iter)
  call print_int('ndiv', ndiv)
  call print_int('ttype', ttype)
  call print_scalar('dmin1', dmin1)
  call print_scalar('dmin2', dmin2)
  call print_scalar('dn', dn)
  call print_scalar('dn1', dn1)
  call print_scalar('dn2', dn2)
  call print_scalar('g', g)
  call print_scalar('tau', tau)
  call print_array('z', z, 16)
  call end_test()

  ! ============================================================
  ! Test 5: N=5 with pp=0, larger problem
  ! ============================================================
  z = 0.0d0
  z(1)  = 4.0d0;  z(2)  = 1.0d0;  z(3)  = 4.0d0;  z(4)  = 1.0d0
  z(5)  = 3.0d0;  z(6)  = 0.5d0;  z(7)  = 3.0d0;  z(8)  = 0.5d0
  z(9)  = 5.0d0;  z(10) = 0.3d0;  z(11) = 5.0d0;  z(12) = 0.3d0
  z(13) = 2.0d0;  z(14) = 0.2d0;  z(15) = 2.0d0;  z(16) = 0.2d0
  z(17) = 1.5d0;  z(18) = 0.0d0;  z(19) = 1.5d0;  z(20) = 0.0d0

  i0 = 1; n0 = 5; pp = 0
  dmin = 1.5d0; sigma = 0.0d0; desig = 0.0d0; qmax = 5.0d0
  nfail = 0; iter = 0; ndiv = 0; ieee = .true.
  ttype = 0; dmin1 = 1.5d0; dmin2 = 2.0d0
  dn = 1.5d0; dn1 = 2.0d0; dn2 = 5.0d0; g = 0.0d0; tau = 0.0d0

  call dlasq3(i0, n0, z, pp, dmin, sigma, desig, qmax, nfail, &
              iter, ndiv, ieee, ttype, dmin1, dmin2, dn, dn1, dn2, g, tau)

  call begin_test('basic_n5_pp0')
  call print_int('n0', n0)
  call print_int('pp', pp)
  call print_scalar('dmin', dmin)
  call print_scalar('sigma', sigma)
  call print_scalar('desig', desig)
  call print_scalar('qmax', qmax)
  call print_int('nfail', nfail)
  call print_int('iter', iter)
  call print_int('ndiv', ndiv)
  call print_int('ttype', ttype)
  call print_scalar('dmin1', dmin1)
  call print_scalar('dmin2', dmin2)
  call print_scalar('dn', dn)
  call print_scalar('dn1', dn1)
  call print_scalar('dn2', dn2)
  call print_scalar('g', g)
  call print_scalar('tau', tau)
  call print_array('z', z, 20)
  call end_test()

  ! ============================================================
  ! Test 6: pp=2 gets reset to 0
  ! ============================================================
  z = 0.0d0
  z(1)  = 4.0d0;  z(2)  = 1.0d0;  z(3)  = 4.0d0;  z(4)  = 1.0d0
  z(5)  = 3.0d0;  z(6)  = 0.5d0;  z(7)  = 3.0d0;  z(8)  = 0.5d0
  z(9)  = 2.0d0;  z(10) = 0.3d0;  z(11) = 2.0d0;  z(12) = 0.3d0
  z(13) = 1.0d0;  z(14) = 0.0d0;  z(15) = 1.0d0;  z(16) = 0.0d0

  i0 = 1; n0 = 4; pp = 2
  dmin = 1.0d0; sigma = 0.0d0; desig = 0.0d0; qmax = 4.0d0
  nfail = 0; iter = 0; ndiv = 0; ieee = .true.
  ttype = 0; dmin1 = 1.0d0; dmin2 = 2.0d0
  dn = 1.0d0; dn1 = 2.0d0; dn2 = 3.0d0; g = 0.0d0; tau = 0.0d0

  call dlasq3(i0, n0, z, pp, dmin, sigma, desig, qmax, nfail, &
              iter, ndiv, ieee, ttype, dmin1, dmin2, dn, dn1, dn2, g, tau)

  call begin_test('pp2_reset')
  call print_int('n0', n0)
  call print_int('pp', pp)
  call print_scalar('dmin', dmin)
  call print_scalar('sigma', sigma)
  call print_scalar('desig', desig)
  call print_int('nfail', nfail)
  call print_int('iter', iter)
  call print_int('ndiv', ndiv)
  call print_int('ttype', ttype)
  call print_scalar('tau', tau)
  call print_array('z', z, 16)
  call end_test()

  ! ============================================================
  ! Test 7: dmin <= 0 triggers reversal path
  ! ============================================================
  z = 0.0d0
  z(1)  = 1.0d0;  z(2)  = 0.5d0;  z(3)  = 1.0d0;  z(4)  = 0.5d0
  z(5)  = 2.0d0;  z(6)  = 0.3d0;  z(7)  = 2.0d0;  z(8)  = 0.3d0
  z(9)  = 3.0d0;  z(10) = 0.2d0;  z(11) = 3.0d0;  z(12) = 0.2d0
  z(13) = 4.0d0;  z(14) = 0.0d0;  z(15) = 4.0d0;  z(16) = 0.0d0

  i0 = 1; n0 = 4; pp = 0
  dmin = -0.1d0; sigma = 0.0d0; desig = 0.0d0; qmax = 4.0d0
  nfail = 0; iter = 0; ndiv = 0; ieee = .true.
  ttype = 0; dmin1 = 1.0d0; dmin2 = 2.0d0
  dn = 4.0d0; dn1 = 3.0d0; dn2 = 2.0d0; g = 0.0d0; tau = 0.0d0

  call dlasq3(i0, n0, z, pp, dmin, sigma, desig, qmax, nfail, &
              iter, ndiv, ieee, ttype, dmin1, dmin2, dn, dn1, dn2, g, tau)

  call begin_test('dmin_negative_reversal')
  call print_int('n0', n0)
  call print_int('pp', pp)
  call print_scalar('dmin', dmin)
  call print_scalar('sigma', sigma)
  call print_scalar('desig', desig)
  call print_scalar('qmax', qmax)
  call print_int('nfail', nfail)
  call print_int('iter', iter)
  call print_int('ndiv', ndiv)
  call print_int('ttype', ttype)
  call print_scalar('dmin1', dmin1)
  call print_scalar('dmin2', dmin2)
  call print_scalar('dn', dn)
  call print_scalar('dn1', dn1)
  call print_scalar('dn2', dn2)
  call print_scalar('tau', tau)
  call print_array('z', z, 16)
  call end_test()

  ! ============================================================
  ! Test 8: non-IEEE arithmetic
  ! ============================================================
  z = 0.0d0
  z(1)  = 4.0d0;  z(2)  = 1.0d0;  z(3)  = 4.0d0;  z(4)  = 1.0d0
  z(5)  = 3.0d0;  z(6)  = 0.5d0;  z(7)  = 3.0d0;  z(8)  = 0.5d0
  z(9)  = 2.0d0;  z(10) = 0.3d0;  z(11) = 2.0d0;  z(12) = 0.3d0
  z(13) = 1.0d0;  z(14) = 0.0d0;  z(15) = 1.0d0;  z(16) = 0.0d0

  i0 = 1; n0 = 4; pp = 0
  dmin = 1.0d0; sigma = 0.0d0; desig = 0.0d0; qmax = 4.0d0
  nfail = 0; iter = 0; ndiv = 0; ieee = .false.
  ttype = 0; dmin1 = 1.0d0; dmin2 = 2.0d0
  dn = 1.0d0; dn1 = 2.0d0; dn2 = 3.0d0; g = 0.0d0; tau = 0.0d0

  call dlasq3(i0, n0, z, pp, dmin, sigma, desig, qmax, nfail, &
              iter, ndiv, ieee, ttype, dmin1, dmin2, dn, dn1, dn2, g, tau)

  call begin_test('non_ieee')
  call print_int('n0', n0)
  call print_int('pp', pp)
  call print_scalar('dmin', dmin)
  call print_scalar('sigma', sigma)
  call print_scalar('desig', desig)
  call print_int('nfail', nfail)
  call print_int('iter', iter)
  call print_int('ndiv', ndiv)
  call print_int('ttype', ttype)
  call print_scalar('tau', tau)
  call print_array('z', z, 16)
  call end_test()

  ! ============================================================
  ! Test 9: deflation of last eigenvalue (E(N0-1) negligible)
  ! ============================================================
  z = 0.0d0
  z(1)  = 4.0d0;  z(2)  = 1.0d0;  z(3)  = 4.0d0;  z(4)  = 1.0d0
  z(5)  = 3.0d0;  z(6)  = 0.5d0;  z(7)  = 3.0d0;  z(8)  = 0.5d0
  z(9)  = 2.0d0;  z(10) = 0.3d0;  z(11) = 2.0d0;  z(12) = 0.3d0
  ! Make E(N0-1) = z(14) = z(nn-5) very small to trigger deflation
  z(13) = 5.0d0;  z(14) = 1.0d-30;  z(15) = 5.0d0;  z(16) = 1.0d-30
  z(17) = 1.0d0;  z(18) = 0.0d0;  z(19) = 1.0d0;  z(20) = 0.0d0

  i0 = 1; n0 = 5; pp = 0
  dmin = 1.0d0; sigma = 0.0d0; desig = 0.0d0; qmax = 5.0d0
  nfail = 0; iter = 0; ndiv = 0; ieee = .true.
  ttype = 0; dmin1 = 1.0d0; dmin2 = 2.0d0
  dn = 1.0d0; dn1 = 5.0d0; dn2 = 2.0d0; g = 0.0d0; tau = 0.0d0

  call dlasq3(i0, n0, z, pp, dmin, sigma, desig, qmax, nfail, &
              iter, ndiv, ieee, ttype, dmin1, dmin2, dn, dn1, dn2, g, tau)

  call begin_test('deflation_last')
  call print_int('n0', n0)
  call print_int('pp', pp)
  call print_scalar('dmin', dmin)
  call print_scalar('sigma', sigma)
  call print_scalar('desig', desig)
  call print_int('nfail', nfail)
  call print_int('iter', iter)
  call print_int('ndiv', ndiv)
  call print_int('ttype', ttype)
  call print_scalar('tau', tau)
  call print_array('z', z, 20)
  call end_test()

  ! ============================================================
  ! Test 10: N=3 minimal problem with deflation first
  ! ============================================================
  z = 0.0d0
  z(1)  = 4.0d0;  z(2)  = 1.0d0;  z(3)  = 4.0d0;  z(4)  = 1.0d0
  z(5)  = 3.0d0;  z(6)  = 0.5d0;  z(7)  = 3.0d0;  z(8)  = 0.5d0
  z(9)  = 2.0d0;  z(10) = 0.0d0;  z(11) = 2.0d0;  z(12) = 0.0d0

  i0 = 1; n0 = 3; pp = 0
  dmin = 2.0d0; sigma = 0.0d0; desig = 0.0d0; qmax = 4.0d0
  nfail = 0; iter = 0; ndiv = 0; ieee = .true.
  ttype = 0; dmin1 = 2.0d0; dmin2 = 3.0d0
  dn = 2.0d0; dn1 = 3.0d0; dn2 = 4.0d0; g = 0.0d0; tau = 0.0d0

  call dlasq3(i0, n0, z, pp, dmin, sigma, desig, qmax, nfail, &
              iter, ndiv, ieee, ttype, dmin1, dmin2, dn, dn1, dn2, g, tau)

  call begin_test('n3_minimal')
  call print_int('n0', n0)
  call print_scalar('dmin', dmin)
  call print_scalar('sigma', sigma)
  call print_scalar('desig', desig)
  call print_int('nfail', nfail)
  call print_int('iter', iter)
  call print_int('ndiv', ndiv)
  call print_int('ttype', ttype)
  call print_scalar('tau', tau)
  call print_array('z', z, 12)
  call end_test()

end program
