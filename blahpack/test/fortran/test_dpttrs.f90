program test_dpttrs
  use test_utils
  implicit none
  double precision :: d(10), e(10), b(100)
  integer :: info

  ! ---------------------------------------------------------------
  ! Test 1: Basic 5x5, single RHS
  ! Using same factored system as dptts2 tests:
  ! D = [4, 3, 2, 3, 4], E = [0.5, -0.5, 0.25, -0.25]
  ! b = A*x where x = [1, 2, 3, 4, 5]
  d(1) = 4.0d0; d(2) = 3.0d0; d(3) = 2.0d0; d(4) = 3.0d0; d(5) = 4.0d0
  e(1) = 0.5d0; e(2) = -0.5d0; e(3) = 0.25d0; e(4) = -0.25d0

  b(1) = 8.0d0
  b(2) = 5.5d0
  b(3) = 7.25d0
  b(4) = 10.25d0
  b(5) = 17.9375d0

  call dpttrs(5, 1, d, e, b, 5, info)

  call begin_test('basic_5x5_single_rhs')
  call print_array('x', b, 5)
  call print_int('info', info)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 2: Multiple RHS (NRHS=3)
  ! x1 = [1, 2, 3, 4, 5]
  ! x2 = [5, 4, 3, 2, 1]
  ! x3 = [2, 0, -1, 3, 1]
  !
  ! Recompute b = A*x for each RHS:
  ! A diagonal: [4, 4, 2.75, 3.125, 4.1875]
  ! A off-diag: [2, -1.5, 0.5, -0.75]
  !
  ! b1 = [8, 5.5, 7.25, 10.25, 17.9375] (same as test 1)
  ! b2: b2_1 = 4*5+2*4=28, b2_2 = 2*5+4*4+(-1.5)*3=21.5
  !     b2_3 = (-1.5)*4+2.75*3+0.5*2=3.25
  !     b2_4 = 0.5*3+3.125*2+(-0.75)*1=7.0
  !     b2_5 = (-0.75)*2+4.1875*1=2.6875
  ! b3: b3_1 = 4*2+2*0=8, b3_2 = 2*2+4*0+(-1.5)*(-1)=5.5
  !     b3_3 = (-1.5)*0+2.75*(-1)+0.5*3=-1.25
  !     b3_4 = 0.5*(-1)+3.125*3+(-0.75)*1=8.125
  !     b3_5 = (-0.75)*3+4.1875*1=1.9375

  d(1) = 4.0d0; d(2) = 3.0d0; d(3) = 2.0d0; d(4) = 3.0d0; d(5) = 4.0d0
  e(1) = 0.5d0; e(2) = -0.5d0; e(3) = 0.25d0; e(4) = -0.25d0

  ! Column 1 (indices 1-5)
  b(1) = 8.0d0
  b(2) = 5.5d0
  b(3) = 7.25d0
  b(4) = 10.25d0
  b(5) = 17.9375d0
  ! Column 2 (indices 6-10)
  b(6) = 28.0d0
  b(7) = 21.5d0
  b(8) = 3.25d0
  b(9) = 7.0d0
  b(10) = 2.6875d0
  ! Column 3 (indices 11-15)
  b(11) = 8.0d0
  b(12) = 5.5d0
  b(13) = -1.25d0
  b(14) = 8.125d0
  b(15) = 1.9375d0

  call dpttrs(5, 3, d, e, b, 5, info)

  call begin_test('multi_rhs_3')
  call print_array('x', b, 15)
  call print_int('info', info)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 3: N=1
  d(1) = 3.0d0
  b(1) = 9.0d0

  call dpttrs(1, 1, d, e, b, 1, info)

  call begin_test('n_eq_1')
  call print_array('x', b, 1)
  call print_int('info', info)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 4: N=0 (quick return)
  b(1) = 42.0d0

  call dpttrs(0, 1, d, e, b, 1, info)

  call begin_test('n_eq_0')
  call print_int('info', info)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 5: NRHS=0 (quick return)
  d(1) = 4.0d0; d(2) = 3.0d0
  e(1) = 0.5d0
  b(1) = 42.0d0

  call dpttrs(2, 0, d, e, b, 2, info)

  call begin_test('nrhs_eq_0')
  call print_int('info', info)
  call end_test()

end program
