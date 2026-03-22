program test_dlasq4
  use test_utils
  implicit none

  ! dlasq4 operates on a Z array of size 4*N0+PP (at least)
  ! Z layout: groups of 4 per element (d_i, e_i, d_i', e_i') for ping-pong
  ! The routine reads from specific Z indices: NN-3..NN-15 etc.
  ! where NN = 4*N0 + PP

  double precision :: z(100), tau, g
  integer :: i0, n0, pp, n0in, ttype
  double precision :: dmin, dmin1, dmin2, dn, dn1, dn2
  integer :: i

  ! ================================================================
  ! Test 1: dmin <= 0 (Case 1, ttype = -1)
  ! ================================================================
  do i = 1, 100
    z(i) = 1.0d0
  end do
  i0 = 1; n0 = 5; pp = 0; n0in = 5
  dmin = -0.5d0; dmin1 = 1.0d0; dmin2 = 1.0d0
  dn = 1.0d0; dn1 = 1.0d0; dn2 = 1.0d0
  tau = 0.0d0; ttype = 0; g = 0.0d0

  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('dmin_negative')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 2: dmin = 0 (Case 1, ttype = -1)
  ! ================================================================
  dmin = 0.0d0
  tau = 0.0d0; ttype = 0; g = 0.0d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('dmin_zero')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 3: Case 2 — n0in=n0, dmin=dn, dmin1=dn1
  ! This exercises the gap1>0 && gap1>b1 branch (ttype=-2)
  ! ================================================================
  ! Set up Z so that:
  ! NN = 4*5 + 0 = 20
  ! Z(17)=Z(NN-3), Z(15)=Z(NN-5), Z(13)=Z(NN-7), Z(11)=Z(NN-9)
  do i = 1, 100
    z(i) = 4.0d0
  end do
  ! Make specific values to get ttype=-2
  z(17) = 1.0d0   ! Z(NN-3)
  z(15) = 1.0d0   ! Z(NN-5)
  z(13) = 1.0d0   ! Z(NN-7)
  z(11) = 1.0d0   ! Z(NN-9)
  i0 = 1; n0 = 5; pp = 0; n0in = 5
  dn = 0.5d0; dn1 = 2.0d0; dn2 = 3.0d0
  dmin = 0.5d0; dmin1 = 0.5d0; dmin2 = 3.0d0
  tau = 0.0d0; ttype = 0; g = 0.0d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('case2_ttype_neg2')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 4: Case 3 — n0in=n0, dmin=dn, dmin1=dn1, but gap1<=0 (ttype=-3)
  ! ================================================================
  do i = 1, 100
    z(i) = 4.0d0
  end do
  z(17) = 2.0d0
  z(15) = 2.0d0
  z(13) = 2.0d0
  z(11) = 2.0d0
  i0 = 1; n0 = 5; pp = 0; n0in = 5
  dn = 3.5d0; dn1 = 3.5d0; dn2 = 4.5d0
  dmin = 3.5d0; dmin1 = 3.5d0; dmin2 = 4.5d0
  tau = 0.0d0; ttype = 0; g = 0.0d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('case3_ttype_neg3')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 5: Case 4 — n0in=n0, dmin=dn, dmin!=dmin1 (ttype=-4)
  ! ================================================================
  do i = 1, 100
    z(i) = 4.0d0
  end do
  z(17) = 1.0d0
  z(15) = 1.0d0
  z(13) = 2.0d0
  z(11) = 1.0d0
  z(9)  = 1.0d0
  z(7)  = 1.5d0
  z(5)  = 1.0d0
  z(3)  = 1.2d0
  i0 = 1; n0 = 5; pp = 0; n0in = 5
  dn = 0.5d0; dn1 = 1.0d0; dn2 = 2.0d0
  dmin = 0.5d0; dmin1 = 1.5d0; dmin2 = 2.0d0
  tau = 0.0d0; ttype = 0; g = 0.0d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('case4_dmin_eq_dn')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 6: Case 4 — dmin=dn1 (not dn), ttype=-4
  ! ================================================================
  do i = 1, 100
    z(i) = 4.0d0
  end do
  z(20) = 1.0d0
  z(18) = 1.5d0
  z(16) = 1.0d0
  z(14) = 2.0d0
  z(12) = 1.0d0
  z(11) = 1.0d0
  z(9)  = 1.0d0
  z(7)  = 1.5d0
  z(5)  = 1.0d0
  z(3)  = 1.2d0
  i0 = 1; n0 = 5; pp = 0; n0in = 5
  dn = 1.0d0; dn1 = 0.5d0; dn2 = 2.0d0
  dmin = 0.5d0; dmin1 = 1.5d0; dmin2 = 2.0d0
  tau = 0.0d0; ttype = 0; g = 0.0d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('case4_dmin_eq_dn1')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 7: Case 5 — dmin=dn2 (ttype=-5)
  ! ================================================================
  do i = 1, 100
    z(i) = 4.0d0
  end do
  z(20) = 1.0d0
  z(18) = 1.0d0
  z(16) = 1.0d0
  z(14) = 1.0d0
  z(12) = 1.0d0
  z(10) = 1.0d0
  z(8)  = 1.5d0
  z(6)  = 1.0d0
  z(5)  = 1.0d0
  z(4)  = 1.0d0
  z(3)  = 1.0d0
  z(2)  = 1.0d0
  z(1)  = 1.0d0
  i0 = 1; n0 = 5; pp = 0; n0in = 5
  dn = 1.5d0; dn1 = 2.0d0; dn2 = 0.5d0
  dmin = 0.5d0; dmin1 = 1.5d0; dmin2 = 1.0d0
  tau = 0.0d0; ttype = 0; g = 0.0d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('case5_dmin_eq_dn2')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 8: Case 6 — n0in=n0, dmin != dn, dn1, dn2 (ttype=-6)
  ! ================================================================
  do i = 1, 100
    z(i) = 4.0d0
  end do
  i0 = 1; n0 = 5; pp = 0; n0in = 5
  dn = 2.0d0; dn1 = 3.0d0; dn2 = 4.0d0
  dmin = 1.0d0; dmin1 = 2.0d0; dmin2 = 3.0d0
  tau = 0.0d0; ttype = 0; g = 0.0d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('case6_no_info')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 9: Case 6 with incoming ttype=-6 (g update path)
  ! ================================================================
  do i = 1, 100
    z(i) = 4.0d0
  end do
  i0 = 1; n0 = 5; pp = 0; n0in = 5
  dn = 2.0d0; dn1 = 3.0d0; dn2 = 4.0d0
  dmin = 1.0d0; dmin1 = 2.0d0; dmin2 = 3.0d0
  tau = 0.0d0; ttype = -6; g = 0.5d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('case6_ttype_neg6')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 10: Case 6 with incoming ttype=-18
  ! ================================================================
  do i = 1, 100
    z(i) = 4.0d0
  end do
  i0 = 1; n0 = 5; pp = 0; n0in = 5
  dn = 2.0d0; dn1 = 3.0d0; dn2 = 4.0d0
  dmin = 1.0d0; dmin1 = 2.0d0; dmin2 = 3.0d0
  tau = 0.0d0; ttype = -18; g = 0.5d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('case6_ttype_neg18')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 11: Case 7/8 — n0in = n0+1, dmin1=dn1, dmin2=dn2
  ! ================================================================
  do i = 1, 100
    z(i) = 4.0d0
  end do
  z(17) = 1.0d0
  z(15) = 2.0d0
  z(13) = 1.0d0
  z(11) = 1.5d0
  z(9) = 1.0d0
  z(7) = 2.0d0
  z(5) = 1.0d0
  z(3) = 1.5d0
  i0 = 1; n0 = 5; pp = 0; n0in = 6
  dn = 1.0d0; dn1 = 0.8d0; dn2 = 1.5d0
  dmin = 0.8d0; dmin1 = 0.8d0; dmin2 = 1.5d0
  tau = 0.0d0; ttype = 0; g = 0.0d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('case7_one_deflated')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 12: Case 9 — n0in = n0+1, dmin1!=dn1 or dmin2!=dn2
  ! ================================================================
  i0 = 1; n0 = 5; pp = 0; n0in = 6
  dn = 1.0d0; dn1 = 0.8d0; dn2 = 1.5d0
  dmin = 0.8d0; dmin1 = 0.9d0; dmin2 = 1.5d0
  tau = 0.0d0; ttype = 0; g = 0.0d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('case9_dmin1_ne_dn1')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 13: Case 9 — n0in = n0+1, dmin1=dn1 (s=half*dmin1)
  ! ================================================================
  i0 = 1; n0 = 5; pp = 0; n0in = 6
  dn = 1.0d0; dn1 = 0.8d0; dn2 = 1.5d0
  dmin = 0.8d0; dmin1 = 0.8d0; dmin2 = 1.6d0
  tau = 0.0d0; ttype = 0; g = 0.0d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('case9_dmin1_eq_dn1')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 14: Case 10 — n0in = n0+2, dmin2=dn2, 2*Z(NN-5)<Z(NN-7)
  ! ================================================================
  do i = 1, 100
    z(i) = 4.0d0
  end do
  ! NN = 4*5 + 0 = 20
  ! Z(15)=Z(NN-5) should be small, Z(13)=Z(NN-7) should be large enough
  z(15) = 0.3d0   ! Z(NN-5)
  z(13) = 2.0d0   ! Z(NN-7)
  z(11) = 1.0d0   ! Z(NN-9)
  z(9)  = 1.0d0   ! Z(NN-11)
  z(7)  = 1.5d0
  z(5)  = 1.0d0
  z(3)  = 1.2d0
  i0 = 1; n0 = 5; pp = 0; n0in = 7
  dn = 1.0d0; dn1 = 0.8d0; dn2 = 0.5d0
  dmin = 0.5d0; dmin1 = 0.8d0; dmin2 = 0.5d0
  tau = 0.0d0; ttype = 0; g = 0.0d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('case10_two_deflated')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 15: Case 11 — n0in = n0+2, but condition not met (ttype=-11)
  ! ================================================================
  do i = 1, 100
    z(i) = 4.0d0
  end do
  z(15) = 2.0d0   ! Z(NN-5) — 2*Z(NN-5) = 4 NOT < Z(NN-7) = 4
  z(13) = 4.0d0   ! Z(NN-7)
  i0 = 1; n0 = 5; pp = 0; n0in = 7
  dn = 1.0d0; dn1 = 0.8d0; dn2 = 0.5d0
  dmin = 0.5d0; dmin1 = 0.8d0; dmin2 = 0.5d0
  tau = 0.0d0; ttype = 0; g = 0.0d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('case11_two_deflated_fallback')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 16: Case 12 — n0in > n0+2 (ttype=-12)
  ! ================================================================
  do i = 1, 100
    z(i) = 4.0d0
  end do
  i0 = 1; n0 = 5; pp = 0; n0in = 8
  dn = 1.0d0; dn1 = 0.8d0; dn2 = 0.5d0
  dmin = 0.5d0; dmin1 = 0.8d0; dmin2 = 0.5d0
  tau = 0.0d0; ttype = 0; g = 0.0d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('case12_many_deflated')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 17: PP=1 (pong) test — Case 2
  ! ================================================================
  do i = 1, 100
    z(i) = 4.0d0
  end do
  ! NN = 4*5 + 1 = 21
  z(18) = 1.0d0   ! Z(NN-3)
  z(16) = 1.0d0   ! Z(NN-5)
  z(14) = 1.0d0   ! Z(NN-7)
  z(12) = 1.0d0   ! Z(NN-9)
  i0 = 1; n0 = 5; pp = 1; n0in = 5
  dn = 0.5d0; dn1 = 2.0d0; dn2 = 3.0d0
  dmin = 0.5d0; dmin1 = 0.5d0; dmin2 = 3.0d0
  tau = 0.0d0; ttype = 0; g = 0.0d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('pp1_case2')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 18: Case 4 early return (Z(NN-5) > Z(NN-7))
  ! ================================================================
  do i = 1, 100
    z(i) = 4.0d0
  end do
  z(17) = 1.0d0
  z(15) = 5.0d0   ! Z(NN-5) > Z(NN-7) triggers early return
  z(13) = 2.0d0   ! Z(NN-7)
  z(11) = 1.0d0
  i0 = 1; n0 = 5; pp = 0; n0in = 5
  dn = 0.5d0; dn1 = 1.0d0; dn2 = 2.0d0
  dmin = 0.5d0; dmin1 = 1.5d0; dmin2 = 2.0d0
  tau = 0.0d0; ttype = 0; g = 0.0d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('case4_early_return')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 19: Case 5 early return (Z(NP-8) > B2)
  ! ================================================================
  do i = 1, 100
    z(i) = 4.0d0
  end do
  ! NN=20, NP = NN - 2*PP = 20 - 0 = 20
  ! Z(NP-2)=Z(18), Z(NP-6)=Z(14), Z(NP-8)=Z(12), Z(NP-4)=Z(16)
  z(18) = 1.0d0   ! b1
  z(14) = 0.5d0   ! b2
  z(12) = 10.0d0  ! Z(NP-8) > b2 triggers return
  z(16) = 0.5d0
  i0 = 1; n0 = 5; pp = 0; n0in = 5
  dn = 1.5d0; dn1 = 2.0d0; dn2 = 0.5d0
  dmin = 0.5d0; dmin1 = 1.5d0; dmin2 = 1.0d0
  tau = 0.0d0; ttype = 0; g = 0.0d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('case5_early_return')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 20: Case 3 with dn > b1 path
  ! ================================================================
  do i = 1, 100
    z(i) = 4.0d0
  end do
  z(17) = 0.01d0
  z(15) = 0.01d0
  z(13) = 0.01d0
  z(11) = 0.01d0
  i0 = 1; n0 = 5; pp = 0; n0in = 5
  dn = 0.5d0; dn1 = 0.5d0; dn2 = 0.5d0
  dmin = 0.5d0; dmin1 = 0.5d0; dmin2 = 0.5d0
  tau = 0.0d0; ttype = 0; g = 0.0d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('case3_dn_gt_b1')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 21: Case 7 with b2=0 (early exit in loop)
  ! ================================================================
  do i = 1, 100
    z(i) = 4.0d0
  end do
  z(15) = 0.0d0   ! Z(NN-5) = 0 => b1=0, b2=0
  z(13) = 2.0d0   ! Z(NN-7) nonzero
  i0 = 1; n0 = 5; pp = 0; n0in = 6
  dn = 1.0d0; dn1 = 0.8d0; dn2 = 1.5d0
  dmin = 0.8d0; dmin1 = 0.8d0; dmin2 = 1.5d0
  tau = 0.0d0; ttype = 0; g = 0.0d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('case7_b2_zero')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 22: Case 7 with gap2 <= 0 (ttype=-8 path)
  ! ================================================================
  do i = 1, 100
    z(i) = 4.0d0
  end do
  z(17) = 1.0d0
  z(15) = 1.0d0
  z(13) = 2.0d0
  z(11) = 1.5d0
  z(9) = 1.0d0
  z(7) = 2.0d0
  z(5) = 1.0d0
  z(3) = 1.5d0
  i0 = 1; n0 = 5; pp = 0; n0in = 6
  dn = 1.0d0; dn1 = 0.8d0; dn2 = 0.1d0
  dmin = 0.1d0; dmin1 = 0.8d0; dmin2 = 0.1d0
  tau = 0.0d0; ttype = 0; g = 0.0d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('case8_gap2_negative')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 23: Case 10 with b2=0 path
  ! ================================================================
  do i = 1, 100
    z(i) = 4.0d0
  end do
  z(15) = 0.0d0   ! Z(NN-5) = 0
  z(13) = 2.0d0   ! Z(NN-7) large enough so 2*0 < 2
  z(11) = 1.0d0
  z(9)  = 1.0d0
  i0 = 1; n0 = 5; pp = 0; n0in = 7
  dn = 1.0d0; dn1 = 0.8d0; dn2 = 0.5d0
  dmin = 0.5d0; dmin1 = 0.8d0; dmin2 = 0.5d0
  tau = 0.0d0; ttype = 0; g = 0.0d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('case10_b2_zero')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 24: Case 2 with gap2 <= 0 path
  ! ================================================================
  do i = 1, 100
    z(i) = 4.0d0
  end do
  z(17) = 1.0d0
  z(15) = 1.0d0
  z(13) = 1.0d0
  z(11) = 1.0d0
  i0 = 1; n0 = 5; pp = 0; n0in = 5
  dn = 0.5d0; dn1 = 0.5d0; dn2 = 1.0d0
  dmin = 0.5d0; dmin1 = 0.5d0; dmin2 = 1.0d0
  tau = 0.0d0; ttype = 0; g = 0.0d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('case2_gap2_neg')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 25: Case 5 with n0-i0 <= 2 (skip inner loop)
  ! ================================================================
  do i = 1, 100
    z(i) = 4.0d0
  end do
  ! i0=3, n0=5 => n0-i0=2 (not > 2, so skip loop)
  ! NN=20, NP=20
  z(18) = 1.0d0
  z(14) = 0.5d0
  z(12) = 0.3d0
  z(16) = 0.5d0
  i0 = 3; n0 = 5; pp = 0; n0in = 5
  dn = 1.5d0; dn1 = 2.0d0; dn2 = 0.5d0
  dmin = 0.5d0; dmin1 = 1.5d0; dmin2 = 1.0d0
  tau = 0.0d0; ttype = 0; g = 0.0d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('case5_short_array')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 26: TRUE Case 2 (ttype=-2) — gap2>0, gap1>0, gap1>b1
  ! Need: n0in=n0, dmin=dn, dmin1=dn1
  ! gap2 = dmin2 - a2 - dmin2*0.25 > 0 and > b2
  ! gap1 = a2 - dn - (b2/gap2)*b2 > 0 and > b1
  ! ================================================================
  do i = 1, 100
    z(i) = 4.0d0
  end do
  ! NN=20. Z(17),Z(15),Z(13),Z(11) set small so b1,b2 are small
  z(17) = 0.01d0
  z(15) = 0.01d0
  z(13) = 0.01d0
  z(11) = 0.01d0
  i0 = 1; n0 = 5; pp = 0; n0in = 5
  ! a2 = Z(13)+Z(15) = 0.02
  ! b1 = sqrt(0.01)*sqrt(0.01) = 0.01
  ! b2 = sqrt(0.01)*sqrt(0.01) = 0.01
  ! gap2 = dmin2 - 0.02 - dmin2*0.25 = 10*0.75 - 0.02 = 7.48 > 0
  ! gap1 = 0.02 - dn - (0.01/7.48)*0.01 = 0.02 - 0.005 - tiny ~ 0.015 > b1=0.01 YES!
  dn = 0.005d0; dn1 = 0.005d0; dn2 = 10.0d0
  dmin = 0.005d0; dmin1 = 0.005d0; dmin2 = 10.0d0
  tau = 0.0d0; ttype = 0; g = 0.0d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('true_case2')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 27: Case 3 with a2 > b1+b2 path (s = min(s, a2-(b1+b2)))
  ! Need: dmin=dn, dmin1=dn1, gap1<=0 (so ttype=-3)
  ! And a2 > b1+b2 and dn > b1
  ! ================================================================
  do i = 1, 100
    z(i) = 4.0d0
  end do
  z(17) = 0.01d0
  z(15) = 0.01d0
  z(13) = 0.5d0
  z(11) = 0.01d0
  i0 = 1; n0 = 5; pp = 0; n0in = 5
  ! a2 = Z(13)+Z(15) = 0.51
  ! b1 = sqrt(0.01)*sqrt(0.01) = 0.01
  ! b2 = sqrt(0.5)*sqrt(0.01) = 0.0707...
  ! gap2 = dmin2 - 0.51 - dmin2*0.25 = 0.6*0.75 - 0.51 = -0.06 < 0
  ! gap1 = 0.51 - dn - (0.01 + 0.0707) = 0.51 - 0.3 - 0.0807 = 0.129 > b1=0.01 YES
  ! But we need gap1 NOT > b1 for ttype=-3... so make dn bigger
  ! gap1 = 0.51 - 0.45 - 0.0807 = -0.02 <= 0 => ttype=-3
  ! Now dn > b1: 0.45 > 0.01 YES
  ! a2 > b1+b2: 0.51 > 0.0807 YES
  dn = 0.45d0; dn1 = 0.45d0; dn2 = 0.6d0
  dmin = 0.45d0; dmin1 = 0.45d0; dmin2 = 0.6d0
  tau = 0.0d0; ttype = 0; g = 0.0d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('case3_a2_gt_b1b2')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 28: Case 4 with dmin!=dn (dn1 branch) — Z(nn-9) <= Z(nn-11), loop runs
  ! Need: dmin=dn1 (not dn), all Z ratios favorable
  ! ================================================================
  do i = 1, 100
    z(i) = 2.0d0
  end do
  ! NN=20, PP=0
  ! Need Z(NN-5)=Z(15) <= Z(NN-7)=Z(13) for dmin=dn case
  ! But dmin != dn, so it goes to dn1 branch
  ! NP = NN - 2*PP = 20
  ! Z(NP-4)=Z(16) <= Z(NP-2)=Z(18)
  ! Z(NN-9)=Z(11) <= Z(NN-11)=Z(9)
  z(18) = 3.0d0   ! Z(NP-2)
  z(16) = 1.0d0   ! Z(NP-4)
  z(11) = 1.0d0   ! Z(NN-9)
  z(9) = 2.0d0    ! Z(NN-11)
  z(7) = 1.0d0    ! NP=7, loop needs Z(7) <= Z(5)
  z(5) = 2.0d0
  z(3) = 1.0d0
  z(1) = 2.0d0
  z(17) = 1.0d0
  z(15) = 1.0d0
  z(13) = 2.0d0
  i0 = 1; n0 = 5; pp = 0; n0in = 5
  dn = 1.0d0; dn1 = 0.3d0; dn2 = 2.0d0
  dmin = 0.3d0; dmin1 = 1.5d0; dmin2 = 2.0d0
  tau = 0.0d0; ttype = 0; g = 0.0d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('case4_dn1_branch')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 29: Case 4 where a2 < CNST1 (Rayleigh bound triggers)
  ! Need: dmin=dn, Z(NN-5)<=Z(NN-7), accumulate small a2
  ! ================================================================
  do i = 1, 100
    z(i) = 10.0d0
  end do
  ! NN=20
  z(17) = 1.0d0
  z(15) = 0.1d0   ! Z(NN-5) < Z(NN-7)
  z(13) = 1.0d0   ! Z(NN-7)
  z(11) = 0.1d0   ! NP = 11, Z(11) <= Z(9)
  z(9) = 10.0d0
  z(7) = 0.1d0
  z(5) = 10.0d0
  z(3) = 0.1d0
  z(1) = 10.0d0
  i0 = 1; n0 = 5; pp = 0; n0in = 5
  dn = 0.1d0; dn1 = 1.0d0; dn2 = 2.0d0
  dmin = 0.1d0; dmin1 = 1.5d0; dmin2 = 2.0d0
  tau = 0.0d0; ttype = 0; g = 0.0d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('case4_rayleigh')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 30: Case 7 with gap2>0 (ttype=-7 with better shift)
  ! Need: n0in=n0+1, dmin1=dn1, dmin2=dn2, loop runs successfully
  ! gap2 = half*dmin2 - a2 > 0 and > b2*a2
  ! ================================================================
  do i = 1, 100
    z(i) = 4.0d0
  end do
  z(15) = 0.5d0   ! Z(NN-5) < Z(NN-7)=4.0
  z(13) = 4.0d0
  z(11) = 0.5d0
  z(9) = 4.0d0
  z(7) = 0.5d0
  z(5) = 4.0d0
  z(3) = 0.5d0
  i0 = 1; n0 = 5; pp = 0; n0in = 6
  dn = 1.0d0; dn1 = 0.5d0; dn2 = 10.0d0
  dmin = 0.5d0; dmin1 = 0.5d0; dmin2 = 10.0d0
  tau = 0.0d0; ttype = 0; g = 0.0d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('case7_gap2_positive')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 31: Case 10 where gap2>0 path executes
  ! Need: n0in=n0+2, dmin2=dn2, 2*Z(NN-5)<Z(NN-7), loop converges
  ! gap2 = Z(NN-7)+Z(NN-9)-sqrt(Z(NN-11))*sqrt(Z(NN-9))-a2 > 0
  ! ================================================================
  do i = 1, 100
    z(i) = 4.0d0
  end do
  z(15) = 0.1d0   ! Z(NN-5), 2*0.1=0.2 < Z(NN-7)=4.0
  z(13) = 4.0d0   ! Z(NN-7)
  z(11) = 1.0d0   ! Z(NN-9)
  z(9) = 1.0d0    ! Z(NN-11)
  z(7) = 0.5d0
  z(5) = 4.0d0
  z(3) = 0.5d0
  i0 = 1; n0 = 5; pp = 0; n0in = 7
  dn = 1.0d0; dn1 = 0.8d0; dn2 = 2.0d0
  dmin = 0.5d0; dmin1 = 0.8d0; dmin2 = 2.0d0
  tau = 0.0d0; ttype = 0; g = 0.0d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('case10_gap2_positive')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 32: Case 5 with loop running and a2 < CNST1
  ! Need: n0in=n0, dmin=dn2, n0-i0>2, Z ratios all < 1, small a2
  ! ================================================================
  do i = 1, 100
    z(i) = 10.0d0
  end do
  ! NN=20, NP=20
  z(18) = 10.0d0  ! b1=Z(NP-2)
  z(14) = 10.0d0  ! b2=Z(NP-6)
  z(12) = 1.0d0   ! Z(NP-8) < b2=10 OK
  z(16) = 1.0d0   ! Z(NP-4) < b1=10 OK
  ! a2 = (1/10)*(1+1/10) = 0.11
  z(7) = 0.1d0    ! Z(NN-13)
  z(5) = 10.0d0   ! Z(NN-15)
  ! b2 = 0.1/10 = 0.01, a2 = 0.11 + 0.01 = 0.12
  z(3) = 0.1d0    ! loop: Z(3) < Z(1)=10 OK
  z(1) = 10.0d0
  i0 = 1; n0 = 5; pp = 0; n0in = 5
  dn = 5.0d0; dn1 = 8.0d0; dn2 = 0.3d0
  dmin = 0.3d0; dmin1 = 5.0d0; dmin2 = 3.0d0
  tau = 0.0d0; ttype = 0; g = 0.0d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('case5_rayleigh')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 33: Case 4 loop — Z(I4) > Z(I4-2) triggers early return
  ! Need: dmin=dn (Case 4 dn path), Z(NN-5)<=Z(NN-7), then Z ratio fails in loop
  ! ================================================================
  do i = 1, 100
    z(i) = 2.0d0
  end do
  z(17) = 1.0d0
  z(15) = 1.0d0  ! Z(NN-5) = 1 <= Z(NN-7) = 2
  z(13) = 2.0d0  ! Z(NN-7)
  z(11) = 1.0d0  ! NP=NN-9=11, b2=Z(15)/Z(13)=0.5
  ! loop i4=11: Z(11)=1 <= Z(9)=2 OK
  z(9) = 2.0d0
  ! i4=7: Z(7)=5 > Z(5)=2 triggers RETURN
  z(7) = 5.0d0
  z(5) = 2.0d0
  i0 = 1; n0 = 5; pp = 0; n0in = 5
  dn = 0.3d0; dn1 = 1.0d0; dn2 = 2.0d0
  dmin = 0.3d0; dmin1 = 1.5d0; dmin2 = 2.0d0
  tau = 0.0d0; ttype = 0; g = 0.0d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('case4_loop_return')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 34: Case 5 loop — Z(I4) > Z(I4-2) triggers early return
  ! Need: dmin=dn2, n0-i0>2, initial Z ratios OK, then ratio fails
  ! ================================================================
  do i = 1, 100
    z(i) = 2.0d0
  end do
  ! NN=20, NP=20
  z(18) = 2.0d0  ! b1
  z(14) = 2.0d0  ! b2
  z(12) = 1.0d0  ! Z(NP-8)=1 < b2=2 OK
  z(16) = 1.0d0  ! Z(NP-4)=1 < b1=2 OK
  z(7) = 1.0d0   ! Z(NN-13)
  z(5) = 2.0d0   ! Z(NN-15)
  ! i4=NN-17=3: Z(3)=5 > Z(1)=2 triggers RETURN
  z(3) = 5.0d0
  z(1) = 2.0d0
  i0 = 1; n0 = 5; pp = 0; n0in = 5
  dn = 5.0d0; dn1 = 8.0d0; dn2 = 0.3d0
  dmin = 0.3d0; dmin1 = 5.0d0; dmin2 = 3.0d0
  tau = 0.0d0; ttype = 0; g = 0.0d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('case5_loop_return')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 35: Case 10 loop — Z(I4) > Z(I4-2) triggers early return
  ! ================================================================
  do i = 1, 100
    z(i) = 2.0d0
  end do
  z(15) = 0.1d0  ! 2*0.1=0.2 < Z(NN-7)=2
  z(13) = 2.0d0
  z(11) = 2.0d0
  ! i4=4*5-9+0=11: Z(11)=2 >= Z(9)=2 triggers RETURN
  z(9) = 1.0d0   ! Z(I4-2)=Z(9)=1: Z(11)=2 > 1 => RETURN
  i0 = 1; n0 = 5; pp = 0; n0in = 7
  dn = 1.0d0; dn1 = 0.8d0; dn2 = 0.5d0
  dmin = 0.5d0; dmin1 = 0.8d0; dmin2 = 0.5d0
  tau = 0.0d0; ttype = 0; g = 0.0d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('case10_loop_return')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

  ! ================================================================
  ! Test 36: Case 10 with gap2<=0 (else branch)
  ! ================================================================
  do i = 1, 100
    z(i) = 4.0d0
  end do
  z(15) = 0.1d0   ! 2*0.1 < 4
  z(13) = 4.0d0
  z(11) = 0.1d0   ! Z(NN-9) small => gap2 small
  z(9) = 0.1d0    ! Z(NN-11) small
  z(7) = 0.5d0
  z(5) = 4.0d0
  z(3) = 0.5d0
  i0 = 1; n0 = 5; pp = 0; n0in = 7
  dn = 1.0d0; dn1 = 0.8d0; dn2 = 0.5d0
  dmin = 0.5d0; dmin1 = 0.8d0; dmin2 = 0.5d0
  tau = 0.0d0; ttype = 0; g = 0.0d0
  call dlasq4(i0, n0, z, pp, n0in, dmin, dmin1, dmin2, dn, dn1, dn2, tau, ttype, g)
  call begin_test('case10_gap2_neg')
  call print_scalar('tau', tau)
  call print_int('ttype', ttype)
  call print_scalar('g', g)
  call end_test()

end program
