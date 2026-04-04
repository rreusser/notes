program test_zlansp
  use test_utils
  implicit none
  complex*16 :: ap(15)
  double precision :: work(10)
  double precision :: result
  double precision :: zlansp
  external :: zlansp
  integer :: n

  ! ============================================================
  ! 3x3 complex symmetric matrix:
  !   A = [ (2,1)   (1,2)   (3,-1) ]
  !       [ (1,2)   (5,-1)  (2,1)  ]
  !       [ (3,-1)  (2,1)   (4,2)  ]
  !
  ! Upper packed: (1,1),(1,2),(2,2),(1,3),(2,3),(3,3)
  !   = (2,1),(1,2),(5,-1),(3,-1),(2,1),(4,2)
  ! Lower packed: (1,1),(2,1),(3,1),(2,2),(3,2),(3,3)
  !   = (2,1),(1,2),(3,-1),(5,-1),(2,1),(4,2)
  ! ============================================================

  n = 3

  ! --- Upper triangle tests ---
  ap(1) = (2.0d0, 1.0d0)
  ap(2) = (1.0d0, 2.0d0)
  ap(3) = (5.0d0, -1.0d0)
  ap(4) = (3.0d0, -1.0d0)
  ap(5) = (2.0d0, 1.0d0)
  ap(6) = (4.0d0, 2.0d0)

  ! Test 1: max norm, upper
  result = zlansp('M', 'U', n, ap, work)
  call begin_test('zlansp_3x3_max_U')
  call print_scalar('result', result)
  call end_test()

  ! Test 2: one norm, upper
  work = 0.0d0
  result = zlansp('1', 'U', n, ap, work)
  call begin_test('zlansp_3x3_one_U')
  call print_scalar('result', result)
  call end_test()

  ! Test 3: inf norm, upper
  work = 0.0d0
  result = zlansp('I', 'U', n, ap, work)
  call begin_test('zlansp_3x3_inf_U')
  call print_scalar('result', result)
  call end_test()

  ! Test 4: frobenius norm, upper
  result = zlansp('F', 'U', n, ap, work)
  call begin_test('zlansp_3x3_frob_U')
  call print_scalar('result', result)
  call end_test()

  ! --- Lower triangle tests ---
  ap(1) = (2.0d0, 1.0d0)
  ap(2) = (1.0d0, 2.0d0)
  ap(3) = (3.0d0, -1.0d0)
  ap(4) = (5.0d0, -1.0d0)
  ap(5) = (2.0d0, 1.0d0)
  ap(6) = (4.0d0, 2.0d0)

  ! Test 5: max norm, lower
  result = zlansp('M', 'L', n, ap, work)
  call begin_test('zlansp_3x3_max_L')
  call print_scalar('result', result)
  call end_test()

  ! Test 6: one norm, lower
  work = 0.0d0
  result = zlansp('1', 'L', n, ap, work)
  call begin_test('zlansp_3x3_one_L')
  call print_scalar('result', result)
  call end_test()

  ! Test 7: inf norm, lower
  work = 0.0d0
  result = zlansp('I', 'L', n, ap, work)
  call begin_test('zlansp_3x3_inf_L')
  call print_scalar('result', result)
  call end_test()

  ! Test 8: frobenius norm, lower
  result = zlansp('F', 'L', n, ap, work)
  call begin_test('zlansp_3x3_frob_L')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! 4x4 complex symmetric matrix:
  !   A = [ (2,1)   (1,2)   (3,-1)  (0.5,0.5) ]
  !       [ (1,2)   (5,-1)  (2,1)   (1,-2)    ]
  !       [ (3,-1)  (2,1)   (4,2)   (3,0)     ]
  !       [ (0.5,0.5) (1,-2) (3,0)  (6,-3)    ]
  !
  ! Upper packed: (1,1),(1,2),(2,2),(1,3),(2,3),(3,3),(1,4),(2,4),(3,4),(4,4)
  ! Lower packed: (1,1),(2,1),(3,1),(4,1),(2,2),(3,2),(4,2),(3,3),(4,3),(4,4)
  ! ============================================================

  n = 4

  ! --- Upper triangle tests ---
  ap(1)  = (2.0d0, 1.0d0)
  ap(2)  = (1.0d0, 2.0d0)
  ap(3)  = (5.0d0, -1.0d0)
  ap(4)  = (3.0d0, -1.0d0)
  ap(5)  = (2.0d0, 1.0d0)
  ap(6)  = (4.0d0, 2.0d0)
  ap(7)  = (0.5d0, 0.5d0)
  ap(8)  = (1.0d0, -2.0d0)
  ap(9)  = (3.0d0, 0.0d0)
  ap(10) = (6.0d0, -3.0d0)

  ! Test 9: max norm, upper
  result = zlansp('M', 'U', n, ap, work)
  call begin_test('zlansp_4x4_max_U')
  call print_scalar('result', result)
  call end_test()

  ! Test 10: one norm, upper
  work = 0.0d0
  result = zlansp('1', 'U', n, ap, work)
  call begin_test('zlansp_4x4_one_U')
  call print_scalar('result', result)
  call end_test()

  ! Test 11: inf norm, upper
  work = 0.0d0
  result = zlansp('I', 'U', n, ap, work)
  call begin_test('zlansp_4x4_inf_U')
  call print_scalar('result', result)
  call end_test()

  ! Test 12: frobenius norm, upper
  result = zlansp('F', 'U', n, ap, work)
  call begin_test('zlansp_4x4_frob_U')
  call print_scalar('result', result)
  call end_test()

  ! --- Lower triangle tests ---
  ap(1)  = (2.0d0, 1.0d0)
  ap(2)  = (1.0d0, 2.0d0)
  ap(3)  = (3.0d0, -1.0d0)
  ap(4)  = (0.5d0, 0.5d0)
  ap(5)  = (5.0d0, -1.0d0)
  ap(6)  = (2.0d0, 1.0d0)
  ap(7)  = (1.0d0, -2.0d0)
  ap(8)  = (4.0d0, 2.0d0)
  ap(9)  = (3.0d0, 0.0d0)
  ap(10) = (6.0d0, -3.0d0)

  ! Test 13: max norm, lower
  result = zlansp('M', 'L', n, ap, work)
  call begin_test('zlansp_4x4_max_L')
  call print_scalar('result', result)
  call end_test()

  ! Test 14: one norm, lower
  work = 0.0d0
  result = zlansp('1', 'L', n, ap, work)
  call begin_test('zlansp_4x4_one_L')
  call print_scalar('result', result)
  call end_test()

  ! Test 15: inf norm, lower
  work = 0.0d0
  result = zlansp('I', 'L', n, ap, work)
  call begin_test('zlansp_4x4_inf_L')
  call print_scalar('result', result)
  call end_test()

  ! Test 16: frobenius norm, lower
  result = zlansp('F', 'L', n, ap, work)
  call begin_test('zlansp_4x4_frob_L')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! N=0 quick return
  ! ============================================================
  result = zlansp('M', 'U', 0, ap, work)
  call begin_test('zlansp_n0')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! N=1 matrix, single element = (3.0, 4.0)  => |z| = 5.0
  ! ============================================================
  n = 1
  ap(1) = (3.0d0, 4.0d0)

  result = zlansp('M', 'U', n, ap, work)
  call begin_test('zlansp_1x1_max')
  call print_scalar('result', result)
  call end_test()

  result = zlansp('1', 'U', n, ap, work)
  call begin_test('zlansp_1x1_one')
  call print_scalar('result', result)
  call end_test()

  result = zlansp('I', 'U', n, ap, work)
  call begin_test('zlansp_1x1_inf')
  call print_scalar('result', result)
  call end_test()

  result = zlansp('F', 'U', n, ap, work)
  call begin_test('zlansp_1x1_frob')
  call print_scalar('result', result)
  call end_test()

end program
