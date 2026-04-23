program test_dlantp
  use test_utils
  implicit none
  double precision :: ap(15)
  double precision :: work(10)
  double precision :: result
  double precision :: dlantp
  external :: dlantp
  integer :: n

  ! ============================================================
  ! 3x3 upper triangular matrix (non-unit diagonal):
  !   A = [  2.0   3.0  -1.0 ]
  !       [  0.0   5.0   2.0 ]
  !       [  0.0   0.0   7.0 ]
  !
  ! Upper packed: (1,1),(1,2),(2,2),(1,3),(2,3),(3,3) = 2,3,5,-1,2,7
  ! ============================================================

  n = 3

  ! --- Upper triangle, non-unit diagonal tests ---
  ap(1) = 2.0d0
  ap(2) = 3.0d0
  ap(3) = 5.0d0
  ap(4) = -1.0d0
  ap(5) = 2.0d0
  ap(6) = 7.0d0

  result = dlantp('M', 'U', 'N', n, ap, work)
  call begin_test('dlantp_3x3_max_U_N')
  call print_scalar('result', result)
  call end_test()

  result = dlantp('1', 'U', 'N', n, ap, work)
  call begin_test('dlantp_3x3_one_U_N')
  call print_scalar('result', result)
  call end_test()

  result = dlantp('I', 'U', 'N', n, ap, work)
  call begin_test('dlantp_3x3_inf_U_N')
  call print_scalar('result', result)
  call end_test()

  result = dlantp('F', 'U', 'N', n, ap, work)
  call begin_test('dlantp_3x3_frob_U_N')
  call print_scalar('result', result)
  call end_test()

  ! --- Upper triangle, unit diagonal tests ---
  result = dlantp('M', 'U', 'U', n, ap, work)
  call begin_test('dlantp_3x3_max_U_U')
  call print_scalar('result', result)
  call end_test()

  result = dlantp('1', 'U', 'U', n, ap, work)
  call begin_test('dlantp_3x3_one_U_U')
  call print_scalar('result', result)
  call end_test()

  result = dlantp('I', 'U', 'U', n, ap, work)
  call begin_test('dlantp_3x3_inf_U_U')
  call print_scalar('result', result)
  call end_test()

  result = dlantp('F', 'U', 'U', n, ap, work)
  call begin_test('dlantp_3x3_frob_U_U')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! 3x3 lower triangular matrix (non-unit diagonal):
  !   A = [  2.0   0.0   0.0 ]
  !       [  3.0   5.0   0.0 ]
  !       [ -1.0   2.0   7.0 ]
  !
  ! Lower packed: (1,1),(2,1),(3,1),(2,2),(3,2),(3,3) = 2,3,-1,5,2,7
  ! ============================================================

  ap(1) = 2.0d0
  ap(2) = 3.0d0
  ap(3) = -1.0d0
  ap(4) = 5.0d0
  ap(5) = 2.0d0
  ap(6) = 7.0d0

  ! --- Lower triangle, non-unit diagonal tests ---
  result = dlantp('M', 'L', 'N', n, ap, work)
  call begin_test('dlantp_3x3_max_L_N')
  call print_scalar('result', result)
  call end_test()

  result = dlantp('1', 'L', 'N', n, ap, work)
  call begin_test('dlantp_3x3_one_L_N')
  call print_scalar('result', result)
  call end_test()

  result = dlantp('I', 'L', 'N', n, ap, work)
  call begin_test('dlantp_3x3_inf_L_N')
  call print_scalar('result', result)
  call end_test()

  result = dlantp('F', 'L', 'N', n, ap, work)
  call begin_test('dlantp_3x3_frob_L_N')
  call print_scalar('result', result)
  call end_test()

  ! --- Lower triangle, unit diagonal tests ---
  result = dlantp('M', 'L', 'U', n, ap, work)
  call begin_test('dlantp_3x3_max_L_U')
  call print_scalar('result', result)
  call end_test()

  result = dlantp('1', 'L', 'U', n, ap, work)
  call begin_test('dlantp_3x3_one_L_U')
  call print_scalar('result', result)
  call end_test()

  result = dlantp('I', 'L', 'U', n, ap, work)
  call begin_test('dlantp_3x3_inf_L_U')
  call print_scalar('result', result)
  call end_test()

  result = dlantp('F', 'L', 'U', n, ap, work)
  call begin_test('dlantp_3x3_frob_L_U')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! 4x4 upper triangular matrix (non-unit diagonal):
  !   A = [  2.0   3.0  -1.0   4.0 ]
  !       [  0.0   5.0   2.0  -6.0 ]
  !       [  0.0   0.0   7.0   1.0 ]
  !       [  0.0   0.0   0.0   8.0 ]
  !
  ! Upper packed: 2,3,5,-1,2,7,4,-6,1,8
  ! ============================================================

  n = 4

  ap(1)  = 2.0d0
  ap(2)  = 3.0d0
  ap(3)  = 5.0d0
  ap(4)  = -1.0d0
  ap(5)  = 2.0d0
  ap(6)  = 7.0d0
  ap(7)  = 4.0d0
  ap(8)  = -6.0d0
  ap(9)  = 1.0d0
  ap(10) = 8.0d0

  result = dlantp('M', 'U', 'N', n, ap, work)
  call begin_test('dlantp_4x4_max_U_N')
  call print_scalar('result', result)
  call end_test()

  result = dlantp('1', 'U', 'N', n, ap, work)
  call begin_test('dlantp_4x4_one_U_N')
  call print_scalar('result', result)
  call end_test()

  result = dlantp('I', 'U', 'N', n, ap, work)
  call begin_test('dlantp_4x4_inf_U_N')
  call print_scalar('result', result)
  call end_test()

  result = dlantp('F', 'U', 'N', n, ap, work)
  call begin_test('dlantp_4x4_frob_U_N')
  call print_scalar('result', result)
  call end_test()

  ! --- Upper triangle, unit diagonal tests ---
  result = dlantp('M', 'U', 'U', n, ap, work)
  call begin_test('dlantp_4x4_max_U_U')
  call print_scalar('result', result)
  call end_test()

  result = dlantp('1', 'U', 'U', n, ap, work)
  call begin_test('dlantp_4x4_one_U_U')
  call print_scalar('result', result)
  call end_test()

  result = dlantp('I', 'U', 'U', n, ap, work)
  call begin_test('dlantp_4x4_inf_U_U')
  call print_scalar('result', result)
  call end_test()

  result = dlantp('F', 'U', 'U', n, ap, work)
  call begin_test('dlantp_4x4_frob_U_U')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! 4x4 lower triangular matrix (non-unit diagonal):
  !   A = [  2.0   0.0   0.0   0.0 ]
  !       [  3.0   5.0   0.0   0.0 ]
  !       [ -1.0   2.0   7.0   0.0 ]
  !       [  4.0  -6.0   1.0   8.0 ]
  !
  ! Lower packed: 2,3,-1,4,5,2,-6,7,1,8
  ! ============================================================

  ap(1)  = 2.0d0
  ap(2)  = 3.0d0
  ap(3)  = -1.0d0
  ap(4)  = 4.0d0
  ap(5)  = 5.0d0
  ap(6)  = 2.0d0
  ap(7)  = -6.0d0
  ap(8)  = 7.0d0
  ap(9)  = 1.0d0
  ap(10) = 8.0d0

  result = dlantp('M', 'L', 'N', n, ap, work)
  call begin_test('dlantp_4x4_max_L_N')
  call print_scalar('result', result)
  call end_test()

  result = dlantp('1', 'L', 'N', n, ap, work)
  call begin_test('dlantp_4x4_one_L_N')
  call print_scalar('result', result)
  call end_test()

  result = dlantp('I', 'L', 'N', n, ap, work)
  call begin_test('dlantp_4x4_inf_L_N')
  call print_scalar('result', result)
  call end_test()

  result = dlantp('F', 'L', 'N', n, ap, work)
  call begin_test('dlantp_4x4_frob_L_N')
  call print_scalar('result', result)
  call end_test()

  ! --- Lower triangle, unit diagonal tests ---
  result = dlantp('M', 'L', 'U', n, ap, work)
  call begin_test('dlantp_4x4_max_L_U')
  call print_scalar('result', result)
  call end_test()

  result = dlantp('1', 'L', 'U', n, ap, work)
  call begin_test('dlantp_4x4_one_L_U')
  call print_scalar('result', result)
  call end_test()

  result = dlantp('I', 'L', 'U', n, ap, work)
  call begin_test('dlantp_4x4_inf_L_U')
  call print_scalar('result', result)
  call end_test()

  result = dlantp('F', 'L', 'U', n, ap, work)
  call begin_test('dlantp_4x4_frob_L_U')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! N=0 quick return
  ! ============================================================
  result = dlantp('M', 'U', 'N', 0, ap, work)
  call begin_test('dlantp_n0')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! N=1 matrix, single element = -5.5 (non-unit)
  ! ============================================================
  n = 1
  ap(1) = -5.5d0

  result = dlantp('M', 'U', 'N', n, ap, work)
  call begin_test('dlantp_1x1_max_N')
  call print_scalar('result', result)
  call end_test()

  result = dlantp('1', 'U', 'N', n, ap, work)
  call begin_test('dlantp_1x1_one_N')
  call print_scalar('result', result)
  call end_test()

  result = dlantp('I', 'U', 'N', n, ap, work)
  call begin_test('dlantp_1x1_inf_N')
  call print_scalar('result', result)
  call end_test()

  result = dlantp('F', 'U', 'N', n, ap, work)
  call begin_test('dlantp_1x1_frob_N')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! N=1 matrix, unit diagonal (diagonal assumed 1.0)
  ! ============================================================
  result = dlantp('M', 'U', 'U', n, ap, work)
  call begin_test('dlantp_1x1_max_U')
  call print_scalar('result', result)
  call end_test()

  result = dlantp('1', 'U', 'U', n, ap, work)
  call begin_test('dlantp_1x1_one_U')
  call print_scalar('result', result)
  call end_test()

  result = dlantp('I', 'U', 'U', n, ap, work)
  call begin_test('dlantp_1x1_inf_U')
  call print_scalar('result', result)
  call end_test()

  result = dlantp('F', 'U', 'U', n, ap, work)
  call begin_test('dlantp_1x1_frob_U')
  call print_scalar('result', result)
  call end_test()

end program
