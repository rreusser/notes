program test_ztgevc
  use test_utils
  implicit none

  complex*16 :: S(4, 4), PP(4, 4), VL(4, 4), VR(4, 4), WORK(20)
  double precision :: S_r(32), PP_r(32), VL_r(32), VR_r(32), RWORK(20)
  equivalence (S, S_r)
  equivalence (PP, PP_r)
  equivalence (VL, VL_r)
  equivalence (VR, VR_r)
  logical :: SELECTV(4)
  integer :: info, M, i

  ! Setup: a 3x3 upper triangular pair (S, P) with P having real positive diagonal
  ! Test 1: Right eigenvectors only (SIDE='R', HOWMNY='A')
  S = (0.0d0, 0.0d0)
  S(1,1) = (2.0d0, 1.0d0); S(1,2) = (1.0d0, 0.5d0); S(1,3) = (0.5d0, 0.0d0)
  S(2,2) = (3.0d0, -1.0d0); S(2,3) = (0.0d0, 1.0d0)
  S(3,3) = (1.0d0, 2.0d0)
  PP = (0.0d0, 0.0d0)
  PP(1,1) = (1.0d0, 0.0d0); PP(1,2) = (0.5d0, 0.0d0); PP(1,3) = (0.0d0, 0.0d0)
  PP(2,2) = (2.0d0, 0.0d0); PP(2,3) = (0.25d0, 0.0d0)
  PP(3,3) = (1.5d0, 0.0d0)
  VR = (0.0d0, 0.0d0)
  VL = (0.0d0, 0.0d0)
  call ztgevc('R', 'A', SELECTV, 3, S, 4, PP, 4, VL, 4, VR, 4, 3, M, WORK, RWORK, info)
  call begin_test('right_all')
  call print_int('info', info)
  call print_int('m', M)
  call print_array('vr', VR_r, 24)
  call end_test()

  ! Test 2: Left eigenvectors only (SIDE='L', HOWMNY='A')
  VL = (0.0d0, 0.0d0)
  VR = (0.0d0, 0.0d0)
  call ztgevc('L', 'A', SELECTV, 3, S, 4, PP, 4, VL, 4, VR, 4, 3, M, WORK, RWORK, info)
  call begin_test('left_all')
  call print_int('info', info)
  call print_int('m', M)
  call print_array('vl', VL_r, 24)
  call end_test()

  ! Test 3: Both eigenvectors (SIDE='B', HOWMNY='A')
  VL = (0.0d0, 0.0d0)
  VR = (0.0d0, 0.0d0)
  call ztgevc('B', 'A', SELECTV, 3, S, 4, PP, 4, VL, 4, VR, 4, 3, M, WORK, RWORK, info)
  call begin_test('both_all')
  call print_int('info', info)
  call print_int('m', M)
  call print_array('vl', VL_r, 24)
  call print_array('vr', VR_r, 24)
  call end_test()

  ! Test 4: Selected eigenvectors (HOWMNY='S', right only)
  VR = (0.0d0, 0.0d0)
  SELECTV(1) = .TRUE.
  SELECTV(2) = .FALSE.
  SELECTV(3) = .TRUE.
  call ztgevc('R', 'S', SELECTV, 3, S, 4, PP, 4, VL, 4, VR, 4, 2, M, WORK, RWORK, info)
  call begin_test('right_selected')
  call print_int('info', info)
  call print_int('m', M)
  call print_array('vr', VR_r, 24)
  call end_test()

  ! Test 5: Backtransform (HOWMNY='B', right)
  ! VR = identity as input (backtransform matrix)
  VR = (0.0d0, 0.0d0)
  do i = 1, 3
    VR(i, i) = (1.0d0, 0.0d0)
  end do
  call ztgevc('R', 'B', SELECTV, 3, S, 4, PP, 4, VL, 4, VR, 4, 3, M, WORK, RWORK, info)
  call begin_test('right_backtransform')
  call print_int('info', info)
  call print_int('m', M)
  call print_array('vr', VR_r, 24)
  call end_test()

  ! Test 6: N=0 quick return
  call ztgevc('R', 'A', SELECTV, 0, S, 4, PP, 4, VL, 4, VR, 4, 0, M, WORK, RWORK, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call print_int('m', M)
  call end_test()

  ! Test 7: 2x2 system
  S = (0.0d0, 0.0d0)
  S(1,1) = (1.0d0, 0.0d0); S(1,2) = (0.5d0, 0.5d0)
  S(2,2) = (2.0d0, 0.0d0)
  PP = (0.0d0, 0.0d0)
  PP(1,1) = (1.0d0, 0.0d0); PP(1,2) = (0.0d0, 0.0d0)
  PP(2,2) = (1.0d0, 0.0d0)
  VL = (0.0d0, 0.0d0)
  VR = (0.0d0, 0.0d0)
  call ztgevc('B', 'A', SELECTV, 2, S, 4, PP, 4, VL, 4, VR, 4, 2, M, WORK, RWORK, info)
  call begin_test('both_2x2')
  call print_int('info', info)
  call print_int('m', M)
  call print_array('vl', VL_r, 8)
  call print_array('vr', VR_r, 8)
  call end_test()

end program
