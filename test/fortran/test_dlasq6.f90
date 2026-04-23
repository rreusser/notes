program test_dlasq6
  use test_utils
  implicit none

  double precision :: z(40), dmin, dmin1, dmin2, dn, dnm1, dnm2
  integer :: i

  ! Test 1: basic operation, n=5, pp=0
  z = 0.0d0
  z(1)  = 4.0d0;  z(2)  = 1.0d0;  z(3)  = 4.0d0;  z(4)  = 1.0d0
  z(5)  = 3.0d0;  z(6)  = 0.5d0;  z(7)  = 3.0d0;  z(8)  = 0.5d0
  z(9)  = 2.0d0;  z(10) = 0.3d0;  z(11) = 2.0d0;  z(12) = 0.3d0
  z(13) = 5.0d0;  z(14) = 0.2d0;  z(15) = 5.0d0;  z(16) = 0.2d0
  z(17) = 1.0d0;  z(18) = 0.0d0;  z(19) = 1.0d0;  z(20) = 0.0d0

  call dlasq6(1, 5, z, 0, dmin, dmin1, dmin2, dn, dnm1, dnm2)

  call begin_test('basic_pp0')
  call print_array('z', z, 20)
  call print_scalar('dmin', dmin)
  call print_scalar('dmin1', dmin1)
  call print_scalar('dmin2', dmin2)
  call print_scalar('dn', dn)
  call print_scalar('dnm1', dnm1)
  call print_scalar('dnm2', dnm2)
  call end_test()

  ! Test 2: pp=1
  z = 0.0d0
  z(1)  = 4.0d0;  z(2)  = 1.0d0;  z(3)  = 4.0d0;  z(4)  = 1.0d0
  z(5)  = 3.0d0;  z(6)  = 0.5d0;  z(7)  = 3.0d0;  z(8)  = 0.5d0
  z(9)  = 2.0d0;  z(10) = 0.3d0;  z(11) = 2.0d0;  z(12) = 0.3d0
  z(13) = 5.0d0;  z(14) = 0.2d0;  z(15) = 5.0d0;  z(16) = 0.2d0
  z(17) = 1.0d0;  z(18) = 0.0d0;  z(19) = 1.0d0;  z(20) = 0.0d0

  call dlasq6(1, 5, z, 1, dmin, dmin1, dmin2, dn, dnm1, dnm2)

  call begin_test('basic_pp1')
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
  dmin = -999.0d0; dmin1 = -999.0d0; dmin2 = -999.0d0
  dn = -999.0d0; dnm1 = -999.0d0; dnm2 = -999.0d0

  call dlasq6(1, 2, z, 0, dmin, dmin1, dmin2, dn, dnm1, dnm2)

  call begin_test('quick_return')
  call print_scalar('dmin', dmin)
  call print_scalar('dn', dn)
  call end_test()

  ! Test 4: n=4, pp=0
  z = 0.0d0
  z(1)  = 4.0d0;  z(2)  = 1.0d0;  z(3)  = 4.0d0;  z(4)  = 1.0d0
  z(5)  = 3.0d0;  z(6)  = 0.5d0;  z(7)  = 3.0d0;  z(8)  = 0.5d0
  z(9)  = 2.0d0;  z(10) = 0.3d0;  z(11) = 2.0d0;  z(12) = 0.3d0
  z(13) = 5.0d0;  z(14) = 0.2d0;  z(15) = 5.0d0;  z(16) = 0.2d0

  call dlasq6(1, 4, z, 0, dmin, dmin1, dmin2, dn, dnm1, dnm2)

  call begin_test('n4_pp0')
  call print_array('z', z, 16)
  call print_scalar('dmin', dmin)
  call print_scalar('dmin1', dmin1)
  call print_scalar('dmin2', dmin2)
  call print_scalar('dn', dn)
  call print_scalar('dnm1', dnm1)
  call print_scalar('dnm2', dnm2)
  call end_test()

  ! Test 5: n=3, minimal problem
  z = 0.0d0
  z(1)  = 4.0d0;  z(2)  = 1.0d0;  z(3)  = 4.0d0;  z(4)  = 1.0d0
  z(5)  = 3.0d0;  z(6)  = 0.5d0;  z(7)  = 3.0d0;  z(8)  = 0.5d0
  z(9)  = 2.0d0;  z(10) = 0.3d0;  z(11) = 2.0d0;  z(12) = 0.3d0

  call dlasq6(1, 3, z, 0, dmin, dmin1, dmin2, dn, dnm1, dnm2)

  call begin_test('n3_pp0')
  call print_array('z', z, 12)
  call print_scalar('dmin', dmin)
  call print_scalar('dmin1', dmin1)
  call print_scalar('dmin2', dmin2)
  call print_scalar('dn', dn)
  call print_scalar('dnm1', dnm1)
  call print_scalar('dnm2', dnm2)
  call end_test()

  ! Test 6: with a zero element to trigger zero-denominator branch
  z = 0.0d0
  z(1)  = 4.0d0;  z(2)  = 0.0d0;  z(3)  = 4.0d0;  z(4)  = 0.0d0
  z(5)  = 3.0d0;  z(6)  = 0.5d0;  z(7)  = 3.0d0;  z(8)  = 0.5d0
  z(9)  = 2.0d0;  z(10) = 0.3d0;  z(11) = 2.0d0;  z(12) = 0.3d0
  z(13) = 5.0d0;  z(14) = 0.2d0;  z(15) = 5.0d0;  z(16) = 0.2d0
  z(17) = 1.0d0;  z(18) = 0.0d0;  z(19) = 1.0d0;  z(20) = 0.0d0

  call dlasq6(1, 5, z, 0, dmin, dmin1, dmin2, dn, dnm1, dnm2)

  call begin_test('zero_element_pp0')
  call print_array('z', z, 20)
  call print_scalar('dmin', dmin)
  call print_scalar('dmin1', dmin1)
  call print_scalar('dmin2', dmin2)
  call print_scalar('dn', dn)
  call print_scalar('dnm1', dnm1)
  call print_scalar('dnm2', dnm2)
  call end_test()

  ! Test 7: larger values
  z = 0.0d0
  z(1)  = 10.0d0; z(2)  = 2.0d0;  z(3)  = 10.0d0; z(4)  = 2.0d0
  z(5)  = 8.0d0;  z(6)  = 1.5d0;  z(7)  = 8.0d0;  z(8)  = 1.5d0
  z(9)  = 6.0d0;  z(10) = 1.0d0;  z(11) = 6.0d0;  z(12) = 1.0d0
  z(13) = 4.0d0;  z(14) = 0.5d0;  z(15) = 4.0d0;  z(16) = 0.5d0
  z(17) = 2.0d0;  z(18) = 0.0d0;  z(19) = 2.0d0;  z(20) = 0.0d0

  call dlasq6(1, 5, z, 0, dmin, dmin1, dmin2, dn, dnm1, dnm2)

  call begin_test('larger_values_pp0')
  call print_array('z', z, 20)
  call print_scalar('dmin', dmin)
  call print_scalar('dmin1', dmin1)
  call print_scalar('dmin2', dmin2)
  call print_scalar('dn', dn)
  call print_scalar('dnm1', dnm1)
  call print_scalar('dnm2', dnm2)
  call end_test()

  ! Test 8: very small values to test unsafe computation branch
  ! When safmin*z(j4+1) >= z(j4-2) (the unsafe branch)
  z = 0.0d0
  z(1)  = 1.0d-300;  z(2)  = 1.0d-300;  z(3)  = 1.0d-300;  z(4)  = 1.0d-300
  z(5)  = 1.0d-300;  z(6)  = 1.0d-300;  z(7)  = 1.0d-300;  z(8)  = 1.0d-300
  z(9)  = 1.0d-300;  z(10) = 1.0d-300;  z(11) = 1.0d-300;  z(12) = 1.0d-300
  z(13) = 1.0d-300;  z(14) = 1.0d-300;  z(15) = 1.0d-300;  z(16) = 1.0d-300
  z(17) = 1.0d-300;  z(18) = 0.0d0;     z(19) = 1.0d-300;  z(20) = 0.0d0

  call dlasq6(1, 5, z, 0, dmin, dmin1, dmin2, dn, dnm1, dnm2)

  call begin_test('very_small_pp0')
  call print_array('z', z, 20)
  call print_scalar('dmin', dmin)
  call print_scalar('dmin1', dmin1)
  call print_scalar('dmin2', dmin2)
  call print_scalar('dn', dn)
  call print_scalar('dnm1', dnm1)
  call print_scalar('dnm2', dnm2)
  call end_test()

  ! Test 9: zero denominator in unrolled step
  z = 0.0d0
  z(1)  = 4.0d0;  z(2)  = 1.0d0;  z(3)  = 4.0d0;  z(4)  = 1.0d0
  z(5)  = 3.0d0;  z(6)  = 0.5d0;  z(7)  = 3.0d0;  z(8)  = 0.5d0
  z(9)  = 0.0d0;  z(10) = 0.0d0;  z(11) = 0.0d0;  z(12) = 0.0d0

  call dlasq6(1, 3, z, 0, dmin, dmin1, dmin2, dn, dnm1, dnm2)

  call begin_test('zero_denom_unrolled')
  call print_array('z', z, 12)
  call print_scalar('dmin', dmin)
  call print_scalar('dmin1', dmin1)
  call print_scalar('dmin2', dmin2)
  call print_scalar('dn', dn)
  call print_scalar('dnm1', dnm1)
  call print_scalar('dnm2', dnm2)
  call end_test()

end program
