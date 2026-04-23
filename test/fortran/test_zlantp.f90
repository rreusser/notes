program test_zlantp
  use test_utils
  implicit none
  complex*16 :: ap(15)
  double precision :: work(10)
  double precision :: result
  double precision :: zlantp
  external :: zlantp
  integer :: n

  ! ============================================================
  ! 3x3 upper triangular matrix:
  !   A = [ (1+2i) (3+4i) (5+6i) ]
  !       [     0  (7+8i) (9+1i) ]
  !       [     0      0  (2+3i) ]
  !
  ! Upper packed (column-major): A(1,1), A(1,2), A(2,2), A(1,3), A(2,3), A(3,3)
  !   = (1,2), (3,4), (7,8), (5,6), (9,1), (2,3)
  ! ============================================================

  n = 3

  ! --- Upper, non-unit ---
  ap(1) = (1.0d0, 2.0d0)
  ap(2) = (3.0d0, 4.0d0)
  ap(3) = (7.0d0, 8.0d0)
  ap(4) = (5.0d0, 6.0d0)
  ap(5) = (9.0d0, 1.0d0)
  ap(6) = (2.0d0, 3.0d0)

  result = zlantp('M', 'U', 'N', n, ap, work)
  call begin_test('max_upper_nonunit')
  call print_scalar('result', result)
  call end_test()

  result = zlantp('1', 'U', 'N', n, ap, work)
  call begin_test('one_upper_nonunit')
  call print_scalar('result', result)
  call end_test()

  result = zlantp('I', 'U', 'N', n, ap, work)
  call begin_test('inf_upper_nonunit')
  call print_scalar('result', result)
  call end_test()

  result = zlantp('F', 'U', 'N', n, ap, work)
  call begin_test('frob_upper_nonunit')
  call print_scalar('result', result)
  call end_test()

  ! --- Upper, unit ---
  result = zlantp('M', 'U', 'U', n, ap, work)
  call begin_test('max_upper_unit')
  call print_scalar('result', result)
  call end_test()

  result = zlantp('1', 'U', 'U', n, ap, work)
  call begin_test('one_upper_unit')
  call print_scalar('result', result)
  call end_test()

  result = zlantp('I', 'U', 'U', n, ap, work)
  call begin_test('inf_upper_unit')
  call print_scalar('result', result)
  call end_test()

  result = zlantp('F', 'U', 'U', n, ap, work)
  call begin_test('frob_upper_unit')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! 3x3 lower triangular matrix:
  !   A = [ (1+2i)     0      0  ]
  !       [ (3+4i) (7+8i)     0  ]
  !       [ (5+6i) (9+1i) (2+3i) ]
  !
  ! Lower packed (column-major): A(1,1), A(2,1), A(3,1), A(2,2), A(3,2), A(3,3)
  !   = (1,2), (3,4), (5,6), (7,8), (9,1), (2,3)
  ! ============================================================

  ap(1) = (1.0d0, 2.0d0)
  ap(2) = (3.0d0, 4.0d0)
  ap(3) = (5.0d0, 6.0d0)
  ap(4) = (7.0d0, 8.0d0)
  ap(5) = (9.0d0, 1.0d0)
  ap(6) = (2.0d0, 3.0d0)

  ! --- Lower, non-unit ---
  result = zlantp('M', 'L', 'N', n, ap, work)
  call begin_test('max_lower_nonunit')
  call print_scalar('result', result)
  call end_test()

  result = zlantp('1', 'L', 'N', n, ap, work)
  call begin_test('one_lower_nonunit')
  call print_scalar('result', result)
  call end_test()

  result = zlantp('I', 'L', 'N', n, ap, work)
  call begin_test('inf_lower_nonunit')
  call print_scalar('result', result)
  call end_test()

  result = zlantp('F', 'L', 'N', n, ap, work)
  call begin_test('frob_lower_nonunit')
  call print_scalar('result', result)
  call end_test()

  ! --- Lower, unit ---
  result = zlantp('M', 'L', 'U', n, ap, work)
  call begin_test('max_lower_unit')
  call print_scalar('result', result)
  call end_test()

  result = zlantp('1', 'L', 'U', n, ap, work)
  call begin_test('one_lower_unit')
  call print_scalar('result', result)
  call end_test()

  result = zlantp('I', 'L', 'U', n, ap, work)
  call begin_test('inf_lower_unit')
  call print_scalar('result', result)
  call end_test()

  result = zlantp('F', 'L', 'U', n, ap, work)
  call begin_test('frob_lower_unit')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! 4x4 upper triangular matrix:
  !   A = [ (1+1i) (2-1i) (3+2i) (4-3i) ]
  !       [     0  (5+1i) (1-2i) (2+4i) ]
  !       [     0      0  (6-1i) (3+1i) ]
  !       [     0      0      0  (7+2i) ]
  !
  ! Upper packed: (1,1),(1,2),(2,2),(1,3),(2,3),(3,3),(1,4),(2,4),(3,4),(4,4)
  ! ============================================================

  n = 4

  ap(1)  = (1.0d0, 1.0d0)
  ap(2)  = (2.0d0, -1.0d0)
  ap(3)  = (5.0d0, 1.0d0)
  ap(4)  = (3.0d0, 2.0d0)
  ap(5)  = (1.0d0, -2.0d0)
  ap(6)  = (6.0d0, -1.0d0)
  ap(7)  = (4.0d0, -3.0d0)
  ap(8)  = (2.0d0, 4.0d0)
  ap(9)  = (3.0d0, 1.0d0)
  ap(10) = (7.0d0, 2.0d0)

  result = zlantp('M', 'U', 'N', n, ap, work)
  call begin_test('4x4_max_upper_nonunit')
  call print_scalar('result', result)
  call end_test()

  result = zlantp('1', 'U', 'N', n, ap, work)
  call begin_test('4x4_one_upper_nonunit')
  call print_scalar('result', result)
  call end_test()

  result = zlantp('I', 'U', 'N', n, ap, work)
  call begin_test('4x4_inf_upper_nonunit')
  call print_scalar('result', result)
  call end_test()

  result = zlantp('F', 'U', 'N', n, ap, work)
  call begin_test('4x4_frob_upper_nonunit')
  call print_scalar('result', result)
  call end_test()

  result = zlantp('M', 'U', 'U', n, ap, work)
  call begin_test('4x4_max_upper_unit')
  call print_scalar('result', result)
  call end_test()

  result = zlantp('1', 'U', 'U', n, ap, work)
  call begin_test('4x4_one_upper_unit')
  call print_scalar('result', result)
  call end_test()

  result = zlantp('I', 'U', 'U', n, ap, work)
  call begin_test('4x4_inf_upper_unit')
  call print_scalar('result', result)
  call end_test()

  result = zlantp('F', 'U', 'U', n, ap, work)
  call begin_test('4x4_frob_upper_unit')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! 4x4 lower triangular matrix:
  !   A = [ (1+1i)     0      0      0  ]
  !       [ (2-1i) (5+1i)     0      0  ]
  !       [ (3+2i) (1-2i) (6-1i)     0  ]
  !       [ (4-3i) (2+4i) (3+1i) (7+2i) ]
  !
  ! Lower packed: (1,1),(2,1),(3,1),(4,1),(2,2),(3,2),(4,2),(3,3),(4,3),(4,4)
  ! ============================================================

  ap(1)  = (1.0d0, 1.0d0)
  ap(2)  = (2.0d0, -1.0d0)
  ap(3)  = (3.0d0, 2.0d0)
  ap(4)  = (4.0d0, -3.0d0)
  ap(5)  = (5.0d0, 1.0d0)
  ap(6)  = (1.0d0, -2.0d0)
  ap(7)  = (2.0d0, 4.0d0)
  ap(8)  = (6.0d0, -1.0d0)
  ap(9)  = (3.0d0, 1.0d0)
  ap(10) = (7.0d0, 2.0d0)

  result = zlantp('M', 'L', 'N', n, ap, work)
  call begin_test('4x4_max_lower_nonunit')
  call print_scalar('result', result)
  call end_test()

  result = zlantp('1', 'L', 'N', n, ap, work)
  call begin_test('4x4_one_lower_nonunit')
  call print_scalar('result', result)
  call end_test()

  result = zlantp('I', 'L', 'N', n, ap, work)
  call begin_test('4x4_inf_lower_nonunit')
  call print_scalar('result', result)
  call end_test()

  result = zlantp('F', 'L', 'N', n, ap, work)
  call begin_test('4x4_frob_lower_nonunit')
  call print_scalar('result', result)
  call end_test()

  result = zlantp('M', 'L', 'U', n, ap, work)
  call begin_test('4x4_max_lower_unit')
  call print_scalar('result', result)
  call end_test()

  result = zlantp('1', 'L', 'U', n, ap, work)
  call begin_test('4x4_one_lower_unit')
  call print_scalar('result', result)
  call end_test()

  result = zlantp('I', 'L', 'U', n, ap, work)
  call begin_test('4x4_inf_lower_unit')
  call print_scalar('result', result)
  call end_test()

  result = zlantp('F', 'L', 'U', n, ap, work)
  call begin_test('4x4_frob_lower_unit')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! N=0 quick return
  ! ============================================================
  result = zlantp('M', 'U', 'N', 0, ap, work)
  call begin_test('n_zero')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! N=1 matrix, single element = (3.0, 4.0)
  ! ============================================================
  n = 1
  ap(1) = (3.0d0, 4.0d0)

  result = zlantp('M', 'U', 'N', n, ap, work)
  call begin_test('1x1_max_nonunit')
  call print_scalar('result', result)
  call end_test()

  result = zlantp('1', 'U', 'N', n, ap, work)
  call begin_test('1x1_one_nonunit')
  call print_scalar('result', result)
  call end_test()

  result = zlantp('I', 'U', 'N', n, ap, work)
  call begin_test('1x1_inf_nonunit')
  call print_scalar('result', result)
  call end_test()

  result = zlantp('F', 'U', 'N', n, ap, work)
  call begin_test('1x1_frob_nonunit')
  call print_scalar('result', result)
  call end_test()

  ! N=1, unit diagonal
  result = zlantp('M', 'U', 'U', n, ap, work)
  call begin_test('1x1_max_unit')
  call print_scalar('result', result)
  call end_test()

  result = zlantp('F', 'U', 'U', n, ap, work)
  call begin_test('1x1_frob_unit')
  call print_scalar('result', result)
  call end_test()

end program
