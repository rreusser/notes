program test_zlanhp
  use test_utils
  implicit none
  complex*16 :: ap(15)
  double precision :: work(10)
  double precision :: result
  double precision :: zlanhp
  external :: zlanhp
  integer :: n

  ! ============================================================
  ! 3x3 Hermitian matrix:
  !   A = [  2.0+0i       (1.0+2.0i)   (-1.0+3.0i) ]
  !       [ (1.0-2.0i)     5.0+0i       (0.5-1.5i)  ]
  !       [ (-1.0-3.0i)   (0.5+1.5i)    7.0+0i      ]
  !
  ! Upper packed (column-major): A(1,1), A(1,2), A(2,2), A(1,3), A(2,3), A(3,3)
  !   = (2,0), (1,2), (5,0), (-1,3), (0.5,-1.5), (7,0)
  !
  ! Lower packed (column-major): A(1,1), A(2,1), A(3,1), A(2,2), A(3,2), A(3,3)
  !   = (2,0), (1,-2), (-1,-3), (5,0), (0.5,1.5), (7,0)
  ! ============================================================

  n = 3

  ! --- Upper triangle tests ---
  ap(1) = (2.0d0, 0.0d0)
  ap(2) = (1.0d0, 2.0d0)
  ap(3) = (5.0d0, 0.0d0)
  ap(4) = (-1.0d0, 3.0d0)
  ap(5) = (0.5d0, -1.5d0)
  ap(6) = (7.0d0, 0.0d0)

  result = zlanhp('M', 'U', n, ap, work)
  call begin_test('zlanhp_3x3_max_U')
  call print_scalar('result', result)
  call end_test()

  result = zlanhp('1', 'U', n, ap, work)
  call begin_test('zlanhp_3x3_one_U')
  call print_scalar('result', result)
  call end_test()

  result = zlanhp('I', 'U', n, ap, work)
  call begin_test('zlanhp_3x3_inf_U')
  call print_scalar('result', result)
  call end_test()

  result = zlanhp('F', 'U', n, ap, work)
  call begin_test('zlanhp_3x3_frob_U')
  call print_scalar('result', result)
  call end_test()

  ! --- Lower triangle tests ---
  ap(1) = (2.0d0, 0.0d0)
  ap(2) = (1.0d0, -2.0d0)
  ap(3) = (-1.0d0, -3.0d0)
  ap(4) = (5.0d0, 0.0d0)
  ap(5) = (0.5d0, 1.5d0)
  ap(6) = (7.0d0, 0.0d0)

  result = zlanhp('M', 'L', n, ap, work)
  call begin_test('zlanhp_3x3_max_L')
  call print_scalar('result', result)
  call end_test()

  result = zlanhp('1', 'L', n, ap, work)
  call begin_test('zlanhp_3x3_one_L')
  call print_scalar('result', result)
  call end_test()

  result = zlanhp('I', 'L', n, ap, work)
  call begin_test('zlanhp_3x3_inf_L')
  call print_scalar('result', result)
  call end_test()

  result = zlanhp('F', 'L', n, ap, work)
  call begin_test('zlanhp_3x3_frob_L')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! 4x4 Hermitian matrix:
  !   A = [  2.0+0i       (3.0+1.0i)   (-1.0+2.0i)   (4.0-1.0i) ]
  !       [ (3.0-1.0i)     5.0+0i       (2.0+0.5i)   (-6.0+3.0i)]
  !       [ (-1.0-2.0i)   (2.0-0.5i)    7.0+0i        (1.0+1.0i) ]
  !       [ (4.0+1.0i)   (-6.0-3.0i)   (1.0-1.0i)     8.0+0i     ]
  !
  ! Upper packed: A(1,1),A(1,2),A(2,2),A(1,3),A(2,3),A(3,3),A(1,4),A(2,4),A(3,4),A(4,4)
  ! Lower packed: A(1,1),A(2,1),A(3,1),A(4,1),A(2,2),A(3,2),A(4,2),A(3,3),A(4,3),A(4,4)
  ! ============================================================

  n = 4

  ! --- Upper triangle ---
  ap(1)  = (2.0d0, 0.0d0)
  ap(2)  = (3.0d0, 1.0d0)
  ap(3)  = (5.0d0, 0.0d0)
  ap(4)  = (-1.0d0, 2.0d0)
  ap(5)  = (2.0d0, 0.5d0)
  ap(6)  = (7.0d0, 0.0d0)
  ap(7)  = (4.0d0, -1.0d0)
  ap(8)  = (-6.0d0, 3.0d0)
  ap(9)  = (1.0d0, 1.0d0)
  ap(10) = (8.0d0, 0.0d0)

  result = zlanhp('M', 'U', n, ap, work)
  call begin_test('zlanhp_4x4_max_U')
  call print_scalar('result', result)
  call end_test()

  result = zlanhp('1', 'U', n, ap, work)
  call begin_test('zlanhp_4x4_one_U')
  call print_scalar('result', result)
  call end_test()

  result = zlanhp('I', 'U', n, ap, work)
  call begin_test('zlanhp_4x4_inf_U')
  call print_scalar('result', result)
  call end_test()

  result = zlanhp('F', 'U', n, ap, work)
  call begin_test('zlanhp_4x4_frob_U')
  call print_scalar('result', result)
  call end_test()

  ! --- Lower triangle ---
  ap(1)  = (2.0d0, 0.0d0)
  ap(2)  = (3.0d0, -1.0d0)
  ap(3)  = (-1.0d0, -2.0d0)
  ap(4)  = (4.0d0, 1.0d0)
  ap(5)  = (5.0d0, 0.0d0)
  ap(6)  = (2.0d0, -0.5d0)
  ap(7)  = (-6.0d0, -3.0d0)
  ap(8)  = (7.0d0, 0.0d0)
  ap(9)  = (1.0d0, -1.0d0)
  ap(10) = (8.0d0, 0.0d0)

  result = zlanhp('M', 'L', n, ap, work)
  call begin_test('zlanhp_4x4_max_L')
  call print_scalar('result', result)
  call end_test()

  result = zlanhp('1', 'L', n, ap, work)
  call begin_test('zlanhp_4x4_one_L')
  call print_scalar('result', result)
  call end_test()

  result = zlanhp('I', 'L', n, ap, work)
  call begin_test('zlanhp_4x4_inf_L')
  call print_scalar('result', result)
  call end_test()

  result = zlanhp('F', 'L', n, ap, work)
  call begin_test('zlanhp_4x4_frob_L')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! N=0 quick return
  ! ============================================================
  result = zlanhp('M', 'U', 0, ap, work)
  call begin_test('zlanhp_n0')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! N=1 matrix, single element = (3.5, 0.0) (diagonal is real)
  ! ============================================================
  n = 1
  ap(1) = (3.5d0, 0.0d0)

  result = zlanhp('M', 'U', n, ap, work)
  call begin_test('zlanhp_1x1_max')
  call print_scalar('result', result)
  call end_test()

  result = zlanhp('1', 'U', n, ap, work)
  call begin_test('zlanhp_1x1_one')
  call print_scalar('result', result)
  call end_test()

  result = zlanhp('I', 'U', n, ap, work)
  call begin_test('zlanhp_1x1_inf')
  call print_scalar('result', result)
  call end_test()

  result = zlanhp('F', 'U', n, ap, work)
  call begin_test('zlanhp_1x1_frob')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! N=1 matrix with negative diagonal = (-5.5, 0.0)
  ! ============================================================
  n = 1
  ap(1) = (-5.5d0, 0.0d0)

  result = zlanhp('M', 'U', n, ap, work)
  call begin_test('zlanhp_1x1_neg_max')
  call print_scalar('result', result)
  call end_test()

  result = zlanhp('F', 'U', n, ap, work)
  call begin_test('zlanhp_1x1_neg_frob')
  call print_scalar('result', result)
  call end_test()

end program
