program test_dlasd5
  use test_utils
  implicit none

  double precision :: D(2), Z(2), DELTA(2), WORK(2)
  double precision :: RHO, DSIGMA

  ! Test case 1: I=1, W>0 branch
  ! D = [1.0, 3.0], Z = [0.6, 0.8], RHO = 2.0
  D(1) = 1.0D0
  D(2) = 3.0D0
  Z(1) = 0.6D0
  Z(2) = 0.8D0
  RHO = 2.0D0

  call DLASD5(1, D, Z, DELTA, RHO, DSIGMA, WORK)

  call begin_test('i1_w_positive')
  call print_array('D', D, 2)
  call print_array('Z', Z, 2)
  call print_scalar('rho', RHO)
  call print_array('delta', DELTA, 2)
  call print_scalar('dsigma', DSIGMA)
  call print_array('work', WORK, 2)
  call end_test()

  ! Test case 2: I=1, W<=0 branch
  ! Use D values close together and Z(1) large relative to Z(2)
  D(1) = 2.0D0
  D(2) = 2.5D0
  Z(1) = 0.95D0
  Z(2) = 0.05D0
  RHO = 5.0D0

  call DLASD5(1, D, Z, DELTA, RHO, DSIGMA, WORK)

  call begin_test('i1_w_nonpositive')
  call print_array('D', D, 2)
  call print_array('Z', Z, 2)
  call print_scalar('rho', RHO)
  call print_array('delta', DELTA, 2)
  call print_scalar('dsigma', DSIGMA)
  call print_array('work', WORK, 2)
  call end_test()

  ! Test case 3: I=2, B>0 branch
  D(1) = 1.0D0
  D(2) = 3.0D0
  Z(1) = 0.6D0
  Z(2) = 0.8D0
  RHO = 2.0D0

  call DLASD5(2, D, Z, DELTA, RHO, DSIGMA, WORK)

  call begin_test('i2_b_positive')
  call print_array('D', D, 2)
  call print_array('Z', Z, 2)
  call print_scalar('rho', RHO)
  call print_array('delta', DELTA, 2)
  call print_scalar('dsigma', DSIGMA)
  call print_array('work', WORK, 2)
  call end_test()

  ! Test case 4: I=2, B<=0 branch
  ! Use small RHO relative to D values to get B<=0
  D(1) = 1.0D0
  D(2) = 5.0D0
  Z(1) = 0.3D0
  Z(2) = 0.9D0
  RHO = 0.5D0

  call DLASD5(2, D, Z, DELTA, RHO, DSIGMA, WORK)

  call begin_test('i2_b_nonpositive')
  call print_array('D', D, 2)
  call print_array('Z', Z, 2)
  call print_scalar('rho', RHO)
  call print_array('delta', DELTA, 2)
  call print_scalar('dsigma', DSIGMA)
  call print_array('work', WORK, 2)
  call end_test()

  ! Test case 5: I=1, W<=0, B<=0 sub-branch
  ! Need W<=0 and B<=0 simultaneously
  D(1) = 1.0D0
  D(2) = 2.0D0
  Z(1) = 1.0D0
  Z(2) = 0.0001D0
  RHO = 2.0D0

  call DLASD5(1, D, Z, DELTA, RHO, DSIGMA, WORK)

  call begin_test('i1_w_nonpos_b_nonpos')
  call print_array('D', D, 2)
  call print_array('Z', Z, 2)
  call print_scalar('rho', RHO)
  call print_array('delta', DELTA, 2)
  call print_scalar('dsigma', DSIGMA)
  call print_array('work', WORK, 2)
  call end_test()

  ! Test case 6: D(1)=0 edge case
  D(1) = 0.0D0
  D(2) = 2.0D0
  Z(1) = 0.5D0
  Z(2) = 0.5D0
  RHO = 1.0D0

  call DLASD5(1, D, Z, DELTA, RHO, DSIGMA, WORK)

  call begin_test('d1_zero_i1')
  call print_array('D', D, 2)
  call print_array('Z', Z, 2)
  call print_scalar('rho', RHO)
  call print_array('delta', DELTA, 2)
  call print_scalar('dsigma', DSIGMA)
  call print_array('work', WORK, 2)
  call end_test()

  ! Test case 7: D(1)=0, I=2
  D(1) = 0.0D0
  D(2) = 2.0D0
  Z(1) = 0.5D0
  Z(2) = 0.5D0
  RHO = 1.0D0

  call DLASD5(2, D, Z, DELTA, RHO, DSIGMA, WORK)

  call begin_test('d1_zero_i2')
  call print_array('D', D, 2)
  call print_array('Z', Z, 2)
  call print_scalar('rho', RHO)
  call print_array('delta', DELTA, 2)
  call print_scalar('dsigma', DSIGMA)
  call print_array('work', WORK, 2)
  call end_test()

  ! Test case 8: Large RHO
  D(1) = 1.0D0
  D(2) = 2.0D0
  Z(1) = 0.7071067811865476D0
  Z(2) = 0.7071067811865476D0
  RHO = 100.0D0

  call DLASD5(1, D, Z, DELTA, RHO, DSIGMA, WORK)

  call begin_test('large_rho_i1')
  call print_array('D', D, 2)
  call print_array('Z', Z, 2)
  call print_scalar('rho', RHO)
  call print_array('delta', DELTA, 2)
  call print_scalar('dsigma', DSIGMA)
  call print_array('work', WORK, 2)
  call end_test()

  ! Test case 9: Large RHO, I=2
  D(1) = 1.0D0
  D(2) = 2.0D0
  Z(1) = 0.7071067811865476D0
  Z(2) = 0.7071067811865476D0
  RHO = 100.0D0

  call DLASD5(2, D, Z, DELTA, RHO, DSIGMA, WORK)

  call begin_test('large_rho_i2')
  call print_array('D', D, 2)
  call print_array('Z', Z, 2)
  call print_scalar('rho', RHO)
  call print_array('delta', DELTA, 2)
  call print_scalar('dsigma', DSIGMA)
  call print_array('work', WORK, 2)
  call end_test()

end program
