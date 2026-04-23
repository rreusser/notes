program test_dlansp
  use test_utils
  implicit none
  double precision :: ap(15)
  double precision :: work(10)
  double precision :: result
  double precision :: dlansp
  external :: dlansp
  integer :: n

  ! ============================================================
  ! 3x3 symmetric matrix:
  !   A = [  2.0   3.0  -1.0 ]
  !       [  3.0   5.0   2.0 ]
  !       [ -1.0   2.0   7.0 ]
  !
  ! Upper packed: (1,1),(1,2),(2,2),(1,3),(2,3),(3,3) = 2,3,5,-1,2,7
  ! Lower packed: (1,1),(2,1),(3,1),(2,2),(3,2),(3,3) = 2,3,-1,5,2,7
  ! ============================================================

  n = 3

  ! --- Upper triangle tests ---
  ap(1) = 2.0d0
  ap(2) = 3.0d0
  ap(3) = 5.0d0
  ap(4) = -1.0d0
  ap(5) = 2.0d0
  ap(6) = 7.0d0

  ! Test 1: max norm, upper
  result = dlansp('M', 'U', n, ap, work)
  call begin_test('dlansp_3x3_max_U')
  call print_scalar('result', result)
  call end_test()

  ! Test 2: one norm, upper
  result = dlansp('1', 'U', n, ap, work)
  call begin_test('dlansp_3x3_one_U')
  call print_scalar('result', result)
  call end_test()

  ! Test 3: inf norm, upper
  result = dlansp('I', 'U', n, ap, work)
  call begin_test('dlansp_3x3_inf_U')
  call print_scalar('result', result)
  call end_test()

  ! Test 4: frobenius norm, upper
  result = dlansp('F', 'U', n, ap, work)
  call begin_test('dlansp_3x3_frob_U')
  call print_scalar('result', result)
  call end_test()

  ! --- Lower triangle tests ---
  ap(1) = 2.0d0
  ap(2) = 3.0d0
  ap(3) = -1.0d0
  ap(4) = 5.0d0
  ap(5) = 2.0d0
  ap(6) = 7.0d0

  ! Test 5: max norm, lower
  result = dlansp('M', 'L', n, ap, work)
  call begin_test('dlansp_3x3_max_L')
  call print_scalar('result', result)
  call end_test()

  ! Test 6: one norm, lower
  result = dlansp('1', 'L', n, ap, work)
  call begin_test('dlansp_3x3_one_L')
  call print_scalar('result', result)
  call end_test()

  ! Test 7: inf norm, lower
  result = dlansp('I', 'L', n, ap, work)
  call begin_test('dlansp_3x3_inf_L')
  call print_scalar('result', result)
  call end_test()

  ! Test 8: frobenius norm, lower
  result = dlansp('F', 'L', n, ap, work)
  call begin_test('dlansp_3x3_frob_L')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! 4x4 symmetric matrix:
  !   A = [  2.0   3.0  -1.0   4.0 ]
  !       [  3.0   5.0   2.0  -6.0 ]
  !       [ -1.0   2.0   7.0   1.0 ]
  !       [  4.0  -6.0   1.0   8.0 ]
  !
  ! Upper packed: 2,3,5,-1,2,7,4,-6,1,8
  ! Lower packed: 2,3,-1,4,5,2,-6,7,1,8
  ! ============================================================

  n = 4

  ! --- Upper triangle tests ---
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

  ! Test 9: max norm, upper
  result = dlansp('M', 'U', n, ap, work)
  call begin_test('dlansp_4x4_max_U')
  call print_scalar('result', result)
  call end_test()

  ! Test 10: one norm, upper
  result = dlansp('1', 'U', n, ap, work)
  call begin_test('dlansp_4x4_one_U')
  call print_scalar('result', result)
  call end_test()

  ! Test 11: inf norm, upper
  result = dlansp('I', 'U', n, ap, work)
  call begin_test('dlansp_4x4_inf_U')
  call print_scalar('result', result)
  call end_test()

  ! Test 12: frobenius norm, upper
  result = dlansp('F', 'U', n, ap, work)
  call begin_test('dlansp_4x4_frob_U')
  call print_scalar('result', result)
  call end_test()

  ! --- Lower triangle tests ---
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

  ! Test 13: max norm, lower
  result = dlansp('M', 'L', n, ap, work)
  call begin_test('dlansp_4x4_max_L')
  call print_scalar('result', result)
  call end_test()

  ! Test 14: one norm, lower
  result = dlansp('1', 'L', n, ap, work)
  call begin_test('dlansp_4x4_one_L')
  call print_scalar('result', result)
  call end_test()

  ! Test 15: inf norm, lower
  result = dlansp('I', 'L', n, ap, work)
  call begin_test('dlansp_4x4_inf_L')
  call print_scalar('result', result)
  call end_test()

  ! Test 16: frobenius norm, lower
  result = dlansp('F', 'L', n, ap, work)
  call begin_test('dlansp_4x4_frob_L')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! N=0 quick return
  ! ============================================================
  result = dlansp('M', 'U', 0, ap, work)
  call begin_test('dlansp_n0')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! N=1 matrix, single element = -5.5
  ! ============================================================
  n = 1
  ap(1) = -5.5d0

  result = dlansp('M', 'U', n, ap, work)
  call begin_test('dlansp_1x1_max')
  call print_scalar('result', result)
  call end_test()

  result = dlansp('1', 'U', n, ap, work)
  call begin_test('dlansp_1x1_one')
  call print_scalar('result', result)
  call end_test()

  result = dlansp('I', 'U', n, ap, work)
  call begin_test('dlansp_1x1_inf')
  call print_scalar('result', result)
  call end_test()

  result = dlansp('F', 'U', n, ap, work)
  call begin_test('dlansp_1x1_frob')
  call print_scalar('result', result)
  call end_test()

end program
