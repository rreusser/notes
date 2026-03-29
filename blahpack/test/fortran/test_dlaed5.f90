program test_dlaed5
  use test_utils
  implicit none

  double precision :: D(2), Z(2), DELTA(2), RHO, DLAM

  ! Test 1: I=1, W > 0 branch
  ! Need: 1 + 2*RHO*(Z(2)^2 - Z(1)^2) / DEL > 0
  ! With D = [1, 5], Z = [0.6, 0.8], RHO = 1.0:
  !   DEL = 4, W = 1 + 2*1.0*(0.64 - 0.36)/4 = 1 + 0.14 = 1.14 > 0
  D(1) = 1.0D0
  D(2) = 5.0D0
  Z(1) = 0.6D0
  Z(2) = 0.8D0
  RHO = 1.0D0
  call DLAED5(1, D, Z, DELTA, RHO, DLAM)
  call begin_test('i1_w_positive')
  call print_array('D', D, 2)
  call print_array('Z', Z, 2)
  call print_scalar('RHO', RHO)
  call print_array('DELTA', DELTA, 2)
  call print_scalar('DLAM', DLAM)
  call end_test()

  ! Test 2: I=1, W <= 0 branch, B > 0
  ! Need: W = 1 + 2*RHO*(Z(2)^2 - Z(1)^2) / DEL <= 0
  ! With D = [1, 2], Z = [0.99, 0.14], RHO = 10.0:
  !   DEL = 1, W = 1 + 2*10*(0.0196 - 0.9801)/1 = 1 + 20*(-0.9605) = 1 - 19.21 = -18.21 < 0
  !   B = -DEL + RHO*(Z(1)^2 + Z(2)^2) = -1 + 10*(0.9801 + 0.0196) = -1 + 9.997 = 8.997 > 0
  D(1) = 1.0D0
  D(2) = 2.0D0
  Z(1) = 0.99D0
  Z(2) = 0.14D0
  RHO = 10.0D0
  call DLAED5(1, D, Z, DELTA, RHO, DLAM)
  call begin_test('i1_w_neg_b_positive')
  call print_array('D', D, 2)
  call print_array('Z', Z, 2)
  call print_scalar('RHO', RHO)
  call print_array('DELTA', DELTA, 2)
  call print_scalar('DLAM', DLAM)
  call end_test()

  ! Test 3: I=1, W <= 0 branch, B <= 0
  ! Need: W <= 0, B = -DEL + RHO*(Z(1)^2 + Z(2)^2) <= 0
  ! B <= 0 means RHO*(Z(1)^2 + Z(2)^2) <= DEL
  ! With D = [1, 100], Z = [0.99, 0.14], RHO = 0.5:
  !   DEL = 99, W = 1 + 2*0.5*(0.0196 - 0.9801)/99 = 1 + (-0.9605/99) = 1 - 0.0097 = 0.9903 > 0!
  ! Need bigger Z(2) relative to Z(1) to make W negative, or small RHO with big DEL.
  ! Try D = [1, 1.001], Z = [0.9999, 0.01], RHO = 0.01:
  !   DEL = 0.001, W = 1 + 2*0.01*(0.0001 - 0.99980001)/0.001 = 1 + 0.02*(-0.99970001)/0.001 = 1 - 19.994 < 0
  !   B = -0.001 + 0.01*(0.99980001 + 0.0001) = -0.001 + 0.0099990001 = 0.0089990001 > 0!
  ! It is very hard to get B <= 0 with W <= 0. Let me think more carefully.
  ! For B <= 0: -DEL + RHO*(Z1^2 + Z2^2) <= 0, so RHO*||Z||^2 <= DEL
  ! For W <= 0: 1 + 2*RHO*(Z2^2 - Z1^2)/DEL <= 0, so Z2^2 - Z1^2 <= -DEL/(2*RHO)
  ! i.e. Z1^2 - Z2^2 >= DEL/(2*RHO)
  ! Since ||Z||^2 = Z1^2 + Z2^2, we need RHO*(Z1^2 + Z2^2) <= DEL and Z1^2 - Z2^2 >= DEL/(2*RHO)
  ! From second: Z1^2 >= Z2^2 + DEL/(2*RHO)
  ! Sub into first: RHO*(2*Z2^2 + DEL/(2*RHO)) <= DEL => 2*RHO*Z2^2 + DEL/2 <= DEL => 2*RHO*Z2^2 <= DEL/2
  ! So Z2^2 <= DEL/(4*RHO)
  ! With DEL = 4, RHO = 0.25: Z2^2 <= 4, Z1^2 >= Z2^2 + 8
  ! Try Z1 = 3.0, Z2 = 0.5 (not unit norm but routine says ||Z||=1, still it should work algebraically)
  ! Actually the routine doesn't check ||Z||=1, it just assumes it. Let's use Z1=0.95, Z2=0.3, RHO=0.1, D=[1,5]
  !   DEL=4, W = 1 + 2*0.1*(0.09 - 0.9025)/4 = 1 + 0.2*(-0.8125)/4 = 1 - 0.040625 = 0.959... > 0
  ! Tricky. Let me try large Z1, small Z2, small RHO, small DEL:
  ! D = [1, 1.01], Z1=0.9, Z2=0.1, RHO=0.01
  !   DEL=0.01, W = 1 + 2*0.01*(0.01 - 0.81)/0.01 = 1 + 0.02*(-0.8)/0.01 = 1 - 1.6 = -0.6 < 0
  !   B = -0.01 + 0.01*(0.81 + 0.01) = -0.01 + 0.0082 = -0.0018 < 0
  D(1) = 1.0D0
  D(2) = 1.01D0
  Z(1) = 0.9D0
  Z(2) = 0.1D0
  RHO = 0.01D0
  call DLAED5(1, D, Z, DELTA, RHO, DLAM)
  call begin_test('i1_w_neg_b_neg')
  call print_array('D', D, 2)
  call print_array('Z', Z, 2)
  call print_scalar('RHO', RHO)
  call print_array('DELTA', DELTA, 2)
  call print_scalar('DLAM', DLAM)
  call end_test()

  ! Test 4: I=2, B > 0 branch
  ! B = -DEL + RHO*(Z(1)^2 + Z(2)^2) > 0
  ! With D = [1, 2], Z = [0.6, 0.8], RHO = 5.0:
  !   DEL = 1, B = -1 + 5*(0.36 + 0.64) = -1 + 5 = 4 > 0
  D(1) = 1.0D0
  D(2) = 2.0D0
  Z(1) = 0.6D0
  Z(2) = 0.8D0
  RHO = 5.0D0
  call DLAED5(2, D, Z, DELTA, RHO, DLAM)
  call begin_test('i2_b_positive')
  call print_array('D', D, 2)
  call print_array('Z', Z, 2)
  call print_scalar('RHO', RHO)
  call print_array('DELTA', DELTA, 2)
  call print_scalar('DLAM', DLAM)
  call end_test()

  ! Test 5: I=2, B <= 0 branch
  ! B = -DEL + RHO*(Z(1)^2 + Z(2)^2) <= 0 means RHO*||Z||^2 <= DEL
  ! With D = [1, 10], Z = [0.6, 0.8], RHO = 1.0:
  !   DEL = 9, B = -9 + 1*(0.36 + 0.64) = -9 + 1 = -8 < 0
  D(1) = 1.0D0
  D(2) = 10.0D0
  Z(1) = 0.6D0
  Z(2) = 0.8D0
  RHO = 1.0D0
  call DLAED5(2, D, Z, DELTA, RHO, DLAM)
  call begin_test('i2_b_neg')
  call print_array('D', D, 2)
  call print_array('Z', Z, 2)
  call print_scalar('RHO', RHO)
  call print_array('DELTA', DELTA, 2)
  call print_scalar('DLAM', DLAM)
  call end_test()

  ! Test 6: Unit norm Z vectors (as specified in the documentation)
  ! I=1, another configuration
  D(1) = 2.0D0
  D(2) = 8.0D0
  Z(1) = 0.8D0
  Z(2) = 0.6D0
  RHO = 3.0D0
  call DLAED5(1, D, Z, DELTA, RHO, DLAM)
  call begin_test('i1_rho3')
  call print_array('D', D, 2)
  call print_array('Z', Z, 2)
  call print_scalar('RHO', RHO)
  call print_array('DELTA', DELTA, 2)
  call print_scalar('DLAM', DLAM)
  call end_test()

  ! Test 7: I=2 with same data as test 6
  D(1) = 2.0D0
  D(2) = 8.0D0
  Z(1) = 0.8D0
  Z(2) = 0.6D0
  RHO = 3.0D0
  call DLAED5(2, D, Z, DELTA, RHO, DLAM)
  call begin_test('i2_rho3')
  call print_array('D', D, 2)
  call print_array('Z', Z, 2)
  call print_scalar('RHO', RHO)
  call print_array('DELTA', DELTA, 2)
  call print_scalar('DLAM', DLAM)
  call end_test()

  ! Test 8: Very small RHO (near-zero perturbation)
  D(1) = 3.0D0
  D(2) = 7.0D0
  Z(1) = 0.5D0
  Z(2) = 0.5D0
  RHO = 1.0D-10
  call DLAED5(1, D, Z, DELTA, RHO, DLAM)
  call begin_test('i1_small_rho')
  call print_array('D', D, 2)
  call print_array('Z', Z, 2)
  call print_scalar('RHO', RHO)
  call print_array('DELTA', DELTA, 2)
  call print_scalar('DLAM', DLAM)
  call end_test()

  ! Test 9: Very small RHO, I=2
  D(1) = 3.0D0
  D(2) = 7.0D0
  Z(1) = 0.5D0
  Z(2) = 0.5D0
  RHO = 1.0D-10
  call DLAED5(2, D, Z, DELTA, RHO, DLAM)
  call begin_test('i2_small_rho')
  call print_array('D', D, 2)
  call print_array('Z', Z, 2)
  call print_scalar('RHO', RHO)
  call print_array('DELTA', DELTA, 2)
  call print_scalar('DLAM', DLAM)
  call end_test()

  ! Test 10: Large RHO
  D(1) = 1.0D0
  D(2) = 2.0D0
  Z(1) = 0.7071067811865476D0
  Z(2) = 0.7071067811865476D0
  RHO = 100.0D0
  call DLAED5(1, D, Z, DELTA, RHO, DLAM)
  call begin_test('i1_large_rho')
  call print_array('D', D, 2)
  call print_array('Z', Z, 2)
  call print_scalar('RHO', RHO)
  call print_array('DELTA', DELTA, 2)
  call print_scalar('DLAM', DLAM)
  call end_test()

  ! Test 11: Large RHO, I=2
  D(1) = 1.0D0
  D(2) = 2.0D0
  Z(1) = 0.7071067811865476D0
  Z(2) = 0.7071067811865476D0
  RHO = 100.0D0
  call DLAED5(2, D, Z, DELTA, RHO, DLAM)
  call begin_test('i2_large_rho')
  call print_array('D', D, 2)
  call print_array('Z', Z, 2)
  call print_scalar('RHO', RHO)
  call print_array('DELTA', DELTA, 2)
  call print_scalar('DLAM', DLAM)
  call end_test()

end program
