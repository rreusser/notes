program test_dlasq5
  use test_utils
  implicit none

  double precision :: z(40), dmin, dmin1, dmin2, dn, dnm1, dnm2
  double precision :: tau, sigma, eps
  integer :: i

  ! Machine epsilon
  eps = 2.220446049250313D-016

  ! Test 1: basic operation, n=5, pp=0, tau=0.1, ieee=.true.
  ! Z has 4*5=20 entries for i0=1, n0=5
  ! q values in Z(1), Z(5), Z(9), Z(13), Z(17) (at 4*k-3 for k=1..5)
  ! e values in Z(2), Z(6), Z(10), Z(14)          (at 4*k-2 for k=1..4)
  ! plus qhat/ehat in the other slots
  z = 0.0d0
  z(1)  = 4.0d0;  z(2)  = 1.0d0;  z(3)  = 4.0d0;  z(4)  = 1.0d0
  z(5)  = 3.0d0;  z(6)  = 0.5d0;  z(7)  = 3.0d0;  z(8)  = 0.5d0
  z(9)  = 2.0d0;  z(10) = 0.3d0;  z(11) = 2.0d0;  z(12) = 0.3d0
  z(13) = 5.0d0;  z(14) = 0.2d0;  z(15) = 5.0d0;  z(16) = 0.2d0
  z(17) = 1.0d0;  z(18) = 0.0d0;  z(19) = 1.0d0;  z(20) = 0.0d0

  tau = 0.1d0
  sigma = 1.0d0

  call dlasq5(1, 5, z, 0, tau, sigma, dmin, dmin1, dmin2, dn, &
              dnm1, dnm2, .true., eps)

  call begin_test('basic_ieee_pp0')
  call print_array('z', z, 20)
  call print_scalar('dmin', dmin)
  call print_scalar('dmin1', dmin1)
  call print_scalar('dmin2', dmin2)
  call print_scalar('dn', dn)
  call print_scalar('dnm1', dnm1)
  call print_scalar('dnm2', dnm2)
  call end_test()

  ! Test 2: pp=1, ieee=.true.
  z = 0.0d0
  z(1)  = 4.0d0;  z(2)  = 1.0d0;  z(3)  = 4.0d0;  z(4)  = 1.0d0
  z(5)  = 3.0d0;  z(6)  = 0.5d0;  z(7)  = 3.0d0;  z(8)  = 0.5d0
  z(9)  = 2.0d0;  z(10) = 0.3d0;  z(11) = 2.0d0;  z(12) = 0.3d0
  z(13) = 5.0d0;  z(14) = 0.2d0;  z(15) = 5.0d0;  z(16) = 0.2d0
  z(17) = 1.0d0;  z(18) = 0.0d0;  z(19) = 1.0d0;  z(20) = 0.0d0

  tau = 0.1d0
  sigma = 1.0d0

  call dlasq5(1, 5, z, 1, tau, sigma, dmin, dmin1, dmin2, dn, &
              dnm1, dnm2, .true., eps)

  call begin_test('basic_ieee_pp1')
  call print_array('z', z, 20)
  call print_scalar('dmin', dmin)
  call print_scalar('dmin1', dmin1)
  call print_scalar('dmin2', dmin2)
  call print_scalar('dn', dn)
  call print_scalar('dnm1', dnm1)
  call print_scalar('dnm2', dnm2)
  call end_test()

  ! Test 3: quick return (n0 - i0 - 1 <= 0)
  z = 0.0d0
  z(1) = 1.0d0; z(2) = 0.5d0; z(3) = 1.0d0; z(4) = 0.5d0
  tau = 0.1d0; sigma = 1.0d0
  dmin = -999.0d0; dmin1 = -999.0d0; dmin2 = -999.0d0
  dn = -999.0d0; dnm1 = -999.0d0; dnm2 = -999.0d0

  call dlasq5(1, 2, z, 0, tau, sigma, dmin, dmin1, dmin2, dn, &
              dnm1, dnm2, .true., eps)

  call begin_test('quick_return')
  call print_scalar('dmin', dmin)
  call print_scalar('dn', dn)
  call end_test()

  ! Test 4: tau=0 (tau < dthresh*half forces tau=0), pp=0, ieee=.true.
  z = 0.0d0
  z(1)  = 4.0d0;  z(2)  = 1.0d0;  z(3)  = 4.0d0;  z(4)  = 1.0d0
  z(5)  = 3.0d0;  z(6)  = 0.5d0;  z(7)  = 3.0d0;  z(8)  = 0.5d0
  z(9)  = 2.0d0;  z(10) = 0.3d0;  z(11) = 2.0d0;  z(12) = 0.3d0
  z(13) = 5.0d0;  z(14) = 0.2d0;  z(15) = 5.0d0;  z(16) = 0.2d0
  z(17) = 1.0d0;  z(18) = 0.0d0;  z(19) = 1.0d0;  z(20) = 0.0d0

  tau = 0.0d0
  sigma = 1.0d0

  call dlasq5(1, 5, z, 0, tau, sigma, dmin, dmin1, dmin2, dn, &
              dnm1, dnm2, .true., eps)

  call begin_test('tau_zero_ieee_pp0')
  call print_array('z', z, 20)
  call print_scalar('dmin', dmin)
  call print_scalar('dmin1', dmin1)
  call print_scalar('dmin2', dmin2)
  call print_scalar('dn', dn)
  call print_scalar('dnm1', dnm1)
  call print_scalar('dnm2', dnm2)
  call end_test()

  ! Test 5: non-IEEE, pp=0
  z = 0.0d0
  z(1)  = 4.0d0;  z(2)  = 1.0d0;  z(3)  = 4.0d0;  z(4)  = 1.0d0
  z(5)  = 3.0d0;  z(6)  = 0.5d0;  z(7)  = 3.0d0;  z(8)  = 0.5d0
  z(9)  = 2.0d0;  z(10) = 0.3d0;  z(11) = 2.0d0;  z(12) = 0.3d0
  z(13) = 5.0d0;  z(14) = 0.2d0;  z(15) = 5.0d0;  z(16) = 0.2d0
  z(17) = 1.0d0;  z(18) = 0.0d0;  z(19) = 1.0d0;  z(20) = 0.0d0

  tau = 0.1d0
  sigma = 1.0d0

  call dlasq5(1, 5, z, 0, tau, sigma, dmin, dmin1, dmin2, dn, &
              dnm1, dnm2, .false., eps)

  call begin_test('non_ieee_pp0')
  call print_array('z', z, 20)
  call print_scalar('dmin', dmin)
  call print_scalar('dmin1', dmin1)
  call print_scalar('dmin2', dmin2)
  call print_scalar('dn', dn)
  call print_scalar('dnm1', dnm1)
  call print_scalar('dnm2', dnm2)
  call end_test()

  ! Test 6: tau_zero, non-IEEE, pp=0
  z = 0.0d0
  z(1)  = 4.0d0;  z(2)  = 1.0d0;  z(3)  = 4.0d0;  z(4)  = 1.0d0
  z(5)  = 3.0d0;  z(6)  = 0.5d0;  z(7)  = 3.0d0;  z(8)  = 0.5d0
  z(9)  = 2.0d0;  z(10) = 0.3d0;  z(11) = 2.0d0;  z(12) = 0.3d0
  z(13) = 5.0d0;  z(14) = 0.2d0;  z(15) = 5.0d0;  z(16) = 0.2d0
  z(17) = 1.0d0;  z(18) = 0.0d0;  z(19) = 1.0d0;  z(20) = 0.0d0

  tau = 0.0d0
  sigma = 1.0d0

  call dlasq5(1, 5, z, 0, tau, sigma, dmin, dmin1, dmin2, dn, &
              dnm1, dnm2, .false., eps)

  call begin_test('tau_zero_non_ieee_pp0')
  call print_array('z', z, 20)
  call print_scalar('dmin', dmin)
  call print_scalar('dmin1', dmin1)
  call print_scalar('dmin2', dmin2)
  call print_scalar('dn', dn)
  call print_scalar('dnm1', dnm1)
  call print_scalar('dnm2', dnm2)
  call end_test()

  ! Test 7: tau_zero, non-IEEE, pp=1
  z = 0.0d0
  z(1)  = 4.0d0;  z(2)  = 1.0d0;  z(3)  = 4.0d0;  z(4)  = 1.0d0
  z(5)  = 3.0d0;  z(6)  = 0.5d0;  z(7)  = 3.0d0;  z(8)  = 0.5d0
  z(9)  = 2.0d0;  z(10) = 0.3d0;  z(11) = 2.0d0;  z(12) = 0.3d0
  z(13) = 5.0d0;  z(14) = 0.2d0;  z(15) = 5.0d0;  z(16) = 0.2d0
  z(17) = 1.0d0;  z(18) = 0.0d0;  z(19) = 1.0d0;  z(20) = 0.0d0

  tau = 0.0d0
  sigma = 1.0d0

  call dlasq5(1, 5, z, 1, tau, sigma, dmin, dmin1, dmin2, dn, &
              dnm1, dnm2, .false., eps)

  call begin_test('tau_zero_non_ieee_pp1')
  call print_array('z', z, 20)
  call print_scalar('dmin', dmin)
  call print_scalar('dmin1', dmin1)
  call print_scalar('dmin2', dmin2)
  call print_scalar('dn', dn)
  call print_scalar('dnm1', dnm1)
  call print_scalar('dnm2', dnm2)
  call end_test()

  ! Test 8: n0=4, smaller problem
  z = 0.0d0
  z(1)  = 4.0d0;  z(2)  = 1.0d0;  z(3)  = 4.0d0;  z(4)  = 1.0d0
  z(5)  = 3.0d0;  z(6)  = 0.5d0;  z(7)  = 3.0d0;  z(8)  = 0.5d0
  z(9)  = 2.0d0;  z(10) = 0.3d0;  z(11) = 2.0d0;  z(12) = 0.3d0
  z(13) = 5.0d0;  z(14) = 0.2d0;  z(15) = 5.0d0;  z(16) = 0.2d0

  tau = 0.2d0
  sigma = 0.5d0

  call dlasq5(1, 4, z, 0, tau, sigma, dmin, dmin1, dmin2, dn, &
              dnm1, dnm2, .true., eps)

  call begin_test('n4_ieee_pp0')
  call print_array('z', z, 16)
  call print_scalar('dmin', dmin)
  call print_scalar('dmin1', dmin1)
  call print_scalar('dmin2', dmin2)
  call print_scalar('dn', dn)
  call print_scalar('dnm1', dnm1)
  call print_scalar('dnm2', dnm2)
  call end_test()

  ! Test 9: n0=3, minimal problem
  z = 0.0d0
  z(1)  = 4.0d0;  z(2)  = 1.0d0;  z(3)  = 4.0d0;  z(4)  = 1.0d0
  z(5)  = 3.0d0;  z(6)  = 0.5d0;  z(7)  = 3.0d0;  z(8)  = 0.5d0
  z(9)  = 2.0d0;  z(10) = 0.3d0;  z(11) = 2.0d0;  z(12) = 0.3d0

  tau = 0.1d0
  sigma = 0.5d0

  call dlasq5(1, 3, z, 0, tau, sigma, dmin, dmin1, dmin2, dn, &
              dnm1, dnm2, .true., eps)

  call begin_test('n3_ieee_pp0')
  call print_array('z', z, 12)
  call print_scalar('dmin', dmin)
  call print_scalar('dmin1', dmin1)
  call print_scalar('dmin2', dmin2)
  call print_scalar('dn', dn)
  call print_scalar('dnm1', dnm1)
  call print_scalar('dnm2', dnm2)
  call end_test()

end program
