program test_dlaed6
  use test_utils
  implicit none

  double precision :: D(3), Z(3), TAU, FINIT, RHO
  integer :: KNITER, INFO

  ! Test 1: orgati=true, kniter=1, finit < 0
  ! Root between d(2) and d(3)
  D(1) = 1.0D0
  D(2) = 2.0D0
  D(3) = 5.0D0
  Z(1) = 0.5D0
  Z(2) = 0.6D0
  Z(3) = 0.7D0
  RHO = 1.0D0
  ! f(x) = rho + z1/(d1-x) + z2/(d2-x) + z3/(d3-x)
  ! f(0) = 1 + 0.5/1 + 0.6/2 + 0.7/5 = 1 + 0.5 + 0.3 + 0.14 = 1.94
  FINIT = 1.94D0
  call DLAED6(1, .TRUE., RHO, D, Z, FINIT, TAU, INFO)
  call begin_test('orgati_true_kniter1_finit_pos')
  call print_array('D', D, 3)
  call print_array('Z', Z, 3)
  call print_scalar('RHO', RHO)
  call print_scalar('FINIT', FINIT)
  call print_scalar('TAU', TAU)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 2: orgati=false, kniter=1, finit < 0
  ! Root between d(1) and d(2)
  D(1) = 1.0D0
  D(2) = 3.0D0
  D(3) = 6.0D0
  Z(1) = 0.4D0
  Z(2) = 0.5D0
  Z(3) = 0.3D0
  RHO = -2.0D0
  ! f(0) = -2 + 0.4/1 + 0.5/3 + 0.3/6 = -2 + 0.4 + 0.1667 + 0.05 = -1.3833
  FINIT = -1.3833333333333333D0
  call DLAED6(1, .FALSE., RHO, D, Z, FINIT, TAU, INFO)
  call begin_test('orgati_false_kniter1_finit_neg')
  call print_array('D', D, 3)
  call print_array('Z', Z, 3)
  call print_scalar('RHO', RHO)
  call print_scalar('FINIT', FINIT)
  call print_scalar('TAU', TAU)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 3: orgati=true, kniter=2
  D(1) = 1.0D0
  D(2) = 3.0D0
  D(3) = 7.0D0
  Z(1) = 0.3D0
  Z(2) = 0.5D0
  Z(3) = 0.8D0
  RHO = 0.5D0
  FINIT = 0.5D0 + 0.3D0/1.0D0 + 0.5D0/3.0D0 + 0.8D0/7.0D0
  call DLAED6(2, .TRUE., RHO, D, Z, FINIT, TAU, INFO)
  call begin_test('orgati_true_kniter2')
  call print_array('D', D, 3)
  call print_array('Z', Z, 3)
  call print_scalar('RHO', RHO)
  call print_scalar('FINIT', FINIT)
  call print_scalar('TAU', TAU)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 4: orgati=false, kniter=2
  D(1) = 2.0D0
  D(2) = 4.0D0
  D(3) = 9.0D0
  Z(1) = 0.6D0
  Z(2) = 0.4D0
  Z(3) = 0.2D0
  RHO = -1.5D0
  FINIT = -1.5D0 + 0.6D0/2.0D0 + 0.4D0/4.0D0 + 0.2D0/9.0D0
  call DLAED6(2, .FALSE., RHO, D, Z, FINIT, TAU, INFO)
  call begin_test('orgati_false_kniter2')
  call print_array('D', D, 3)
  call print_array('Z', Z, 3)
  call print_scalar('RHO', RHO)
  call print_scalar('FINIT', FINIT)
  call print_scalar('TAU', TAU)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 5: orgati=true, finit negative (root goes up)
  D(1) = 0.5D0
  D(2) = 1.0D0
  D(3) = 2.0D0
  Z(1) = 0.3D0
  Z(2) = 0.4D0
  Z(3) = 0.5D0
  RHO = -3.0D0
  ! f(0) = -3 + 0.3/0.5 + 0.4/1.0 + 0.5/2.0 = -3 + 0.6 + 0.4 + 0.25 = -1.75
  FINIT = -1.75D0
  call DLAED6(1, .TRUE., RHO, D, Z, FINIT, TAU, INFO)
  call begin_test('orgati_true_finit_neg')
  call print_array('D', D, 3)
  call print_array('Z', Z, 3)
  call print_scalar('RHO', RHO)
  call print_scalar('FINIT', FINIT)
  call print_scalar('TAU', TAU)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 6: orgati=false, finit positive
  D(1) = 1.0D0
  D(2) = 5.0D0
  D(3) = 10.0D0
  Z(1) = 1.0D0
  Z(2) = 2.0D0
  Z(3) = 3.0D0
  RHO = 2.0D0
  FINIT = 2.0D0 + 1.0D0/1.0D0 + 2.0D0/5.0D0 + 3.0D0/10.0D0
  call DLAED6(1, .FALSE., RHO, D, Z, FINIT, TAU, INFO)
  call begin_test('orgati_false_finit_pos')
  call print_array('D', D, 3)
  call print_array('Z', Z, 3)
  call print_scalar('RHO', RHO)
  call print_scalar('FINIT', FINIT)
  call print_scalar('TAU', TAU)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 7: orgati=true, kniter=2, finit negative (kniter=2 with A <= 0 branch)
  D(1) = 0.1D0
  D(2) = 0.5D0
  D(3) = 1.0D0
  Z(1) = 0.2D0
  Z(2) = 0.3D0
  Z(3) = 0.4D0
  RHO = -5.0D0
  FINIT = -5.0D0 + 0.2D0/0.1D0 + 0.3D0/0.5D0 + 0.4D0/1.0D0
  call DLAED6(2, .TRUE., RHO, D, Z, FINIT, TAU, INFO)
  call begin_test('orgati_true_kniter2_finit_neg')
  call print_array('D', D, 3)
  call print_array('Z', Z, 3)
  call print_scalar('RHO', RHO)
  call print_scalar('FINIT', FINIT)
  call print_scalar('TAU', TAU)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 8: Small values to trigger scaling path
  D(1) = 1.0D-150
  D(2) = 2.0D-150
  D(3) = 3.0D-150
  Z(1) = 0.5D-150
  Z(2) = 0.6D-150
  Z(3) = 0.7D-150
  RHO = 1.0D0
  FINIT = 1.0D0 + 0.5D-150/(1.0D-150) + 0.6D-150/(2.0D-150) + 0.7D-150/(3.0D-150)
  call DLAED6(1, .TRUE., RHO, D, Z, FINIT, TAU, INFO)
  call begin_test('small_values_scaling')
  call print_array('D', D, 3)
  call print_array('Z', Z, 3)
  call print_scalar('RHO', RHO)
  call print_scalar('FINIT', FINIT)
  call print_scalar('TAU', TAU)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 9: kniter=2, orgati=false, finit positive with C=0 branch
  ! We need C = RHO + Z(3)/((D(3)-D(2)) - TEMP) where TEMP = (D(1)-D(2))/2
  ! to get C=0. Hard to engineer exactly, so test kniter=2 orgati=false with
  ! values that exercise a different quadratic root selection.
  D(1) = 0.5D0
  D(2) = 2.0D0
  D(3) = 8.0D0
  Z(1) = 0.25D0
  Z(2) = 0.5D0
  Z(3) = 0.75D0
  RHO = 1.0D0
  FINIT = 1.0D0 + 0.25D0/0.5D0 + 0.5D0/2.0D0 + 0.75D0/8.0D0
  call DLAED6(2, .FALSE., RHO, D, Z, FINIT, TAU, INFO)
  call begin_test('orgati_false_kniter2_alternate')
  call print_array('D', D, 3)
  call print_array('Z', Z, 3)
  call print_scalar('RHO', RHO)
  call print_scalar('FINIT', FINIT)
  call print_scalar('TAU', TAU)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 10: Very close D values (nearly degenerate)
  D(1) = 1.0D0
  D(2) = 1.0D0 + 1.0D-8
  D(3) = 2.0D0
  Z(1) = 0.3D0
  Z(2) = 0.4D0
  Z(3) = 0.5D0
  RHO = 0.1D0
  FINIT = 0.1D0 + 0.3D0/1.0D0 + 0.4D0/(1.0D0+1.0D-8) + 0.5D0/2.0D0
  call DLAED6(1, .TRUE., RHO, D, Z, FINIT, TAU, INFO)
  call begin_test('close_d_values')
  call print_array('D', D, 3)
  call print_array('Z', Z, 3)
  call print_scalar('RHO', RHO)
  call print_scalar('FINIT', FINIT)
  call print_scalar('TAU', TAU)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 11: Large rho, kniter=2, orgati=true
  D(1) = 1.0D0
  D(2) = 3.0D0
  D(3) = 5.0D0
  Z(1) = 0.5D0
  Z(2) = 0.5D0
  Z(3) = 0.5D0
  RHO = 100.0D0
  FINIT = 100.0D0 + 0.5D0/1.0D0 + 0.5D0/3.0D0 + 0.5D0/5.0D0
  call DLAED6(2, .TRUE., RHO, D, Z, FINIT, TAU, INFO)
  call begin_test('large_rho_kniter2')
  call print_array('D', D, 3)
  call print_array('Z', Z, 3)
  call print_scalar('RHO', RHO)
  call print_scalar('FINIT', FINIT)
  call print_scalar('TAU', TAU)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 12: negative rho, orgati=false, kniter=2
  D(1) = 2.0D0
  D(2) = 5.0D0
  D(3) = 10.0D0
  Z(1) = 0.7D0
  Z(2) = 0.3D0
  Z(3) = 0.1D0
  RHO = -3.0D0
  FINIT = -3.0D0 + 0.7D0/2.0D0 + 0.3D0/5.0D0 + 0.1D0/10.0D0
  call DLAED6(2, .FALSE., RHO, D, Z, FINIT, TAU, INFO)
  call begin_test('neg_rho_orgati_false_kniter2')
  call print_array('D', D, 3)
  call print_array('Z', Z, 3)
  call print_scalar('RHO', RHO)
  call print_scalar('FINIT', FINIT)
  call print_scalar('TAU', TAU)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 13: Very small values to trigger SMALL2 scaling path
  ! Need temp <= SMALL2 ~ 2e-205
  D(1) = 1.0D-210
  D(2) = 2.0D-210
  D(3) = 3.0D-210
  Z(1) = 0.5D-210
  Z(2) = 0.6D-210
  Z(3) = 0.7D-210
  RHO = 1.0D0
  FINIT = 1.0D0 + 0.5D-210/(1.0D-210) + 0.6D-210/(2.0D-210) + 0.7D-210/(3.0D-210)
  call DLAED6(1, .TRUE., RHO, D, Z, FINIT, TAU, INFO)
  call begin_test('very_small_scaling2')
  call print_array('D', D, 3)
  call print_array('Z', Z, 3)
  call print_scalar('RHO', RHO)
  call print_scalar('FINIT', FINIT)
  call print_scalar('TAU', TAU)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 14: orgati=false, small values scaling
  D(1) = 1.0D-150
  D(2) = 2.0D-150
  D(3) = 3.0D-150
  Z(1) = 0.5D-150
  Z(2) = 0.6D-150
  Z(3) = 0.7D-150
  RHO = 1.0D0
  FINIT = 1.0D0 + 0.5D-150/(1.0D-150) + 0.6D-150/(2.0D-150) + 0.7D-150/(3.0D-150)
  call DLAED6(1, .FALSE., RHO, D, Z, FINIT, TAU, INFO)
  call begin_test('orgati_false_small_values')
  call print_array('D', D, 3)
  call print_array('Z', Z, 3)
  call print_scalar('RHO', RHO)
  call print_scalar('FINIT', FINIT)
  call print_scalar('TAU', TAU)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 15: kniter=2, orgati=true with values designed to trigger tau=0
  ! When |finit| <= |temp| after initial quadratic, tau resets to 0
  D(1) = 1.0D0
  D(2) = 2.0D0
  D(3) = 3.0D0
  Z(1) = 0.1D0
  Z(2) = 0.1D0
  Z(3) = 0.1D0
  RHO = 10.0D0
  FINIT = 10.0D0 + 0.1D0/1.0D0 + 0.1D0/2.0D0 + 0.1D0/3.0D0
  call DLAED6(2, .TRUE., RHO, D, Z, FINIT, TAU, INFO)
  call begin_test('kniter2_orgati_large_rho')
  call print_array('D', D, 3)
  call print_array('Z', Z, 3)
  call print_scalar('RHO', RHO)
  call print_scalar('FINIT', FINIT)
  call print_scalar('TAU', TAU)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 16: kniter=2, orgati=false, negative finit
  D(1) = 1.0D0
  D(2) = 4.0D0
  D(3) = 8.0D0
  Z(1) = 0.5D0
  Z(2) = 0.3D0
  Z(3) = 0.2D0
  RHO = -5.0D0
  FINIT = -5.0D0 + 0.5D0/1.0D0 + 0.3D0/4.0D0 + 0.2D0/8.0D0
  call DLAED6(2, .FALSE., RHO, D, Z, FINIT, TAU, INFO)
  call begin_test('kniter2_orgati_false_neg_finit')
  call print_array('D', D, 3)
  call print_array('Z', Z, 3)
  call print_scalar('RHO', RHO)
  call print_scalar('FINIT', FINIT)
  call print_scalar('TAU', TAU)
  call print_int('INFO', INFO)
  call end_test()

end program
