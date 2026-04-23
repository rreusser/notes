program test_dlanhs
  use test_utils
  implicit none
  double precision :: dlanhs, result, work(10)
  double precision :: a(25)

  ! 3x3 upper Hessenberg matrix (column-major, LDA=3):
  ! Row\Col   1     2     3
  !   1      1.0   2.0   3.0
  !   2      4.0   5.0   6.0
  !   3      0.0   7.0   8.0
  ! (Hessenberg: only one subdiagonal, so a(3,1)=0)
  a = 0.0d0
  a(1) = 1.0d0;  a(2) = 4.0d0;  a(3) = 0.0d0
  a(4) = 2.0d0;  a(5) = 5.0d0;  a(6) = 7.0d0
  a(7) = 3.0d0;  a(8) = 6.0d0;  a(9) = 8.0d0

  result = dlanhs('M', 3, a, 3, work)
  call begin_test('max_norm')
  call print_scalar('result', result)
  call end_test()

  result = dlanhs('1', 3, a, 3, work)
  call begin_test('one_norm')
  call print_scalar('result', result)
  call end_test()

  result = dlanhs('O', 3, a, 3, work)
  call begin_test('one_norm_O')
  call print_scalar('result', result)
  call end_test()

  result = dlanhs('I', 3, a, 3, work)
  call begin_test('inf_norm')
  call print_scalar('result', result)
  call end_test()

  result = dlanhs('F', 3, a, 3, work)
  call begin_test('frob_norm')
  call print_scalar('result', result)
  call end_test()

  result = dlanhs('E', 3, a, 3, work)
  call begin_test('frob_norm_E')
  call print_scalar('result', result)
  call end_test()

  ! N=0 quick return
  result = dlanhs('M', 0, a, 3, work)
  call begin_test('n_zero')
  call print_scalar('result', result)
  call end_test()

  ! 1x1 matrix
  a = 0.0d0
  a(1) = -5.5d0
  result = dlanhs('M', 1, a, 1, work)
  call begin_test('one_by_one_max')
  call print_scalar('result', result)
  call end_test()

  result = dlanhs('1', 1, a, 1, work)
  call begin_test('one_by_one_one')
  call print_scalar('result', result)
  call end_test()

  result = dlanhs('I', 1, a, 1, work)
  call begin_test('one_by_one_inf')
  call print_scalar('result', result)
  call end_test()

  result = dlanhs('F', 1, a, 1, work)
  call begin_test('one_by_one_frob')
  call print_scalar('result', result)
  call end_test()

  ! 4x4 upper Hessenberg matrix (column-major, LDA=4):
  ! Row\Col   1     2     3     4
  !   1      2.0   4.0  -7.0   1.0
  !   2     -1.0  -6.0   2.0   0.0
  !   3      0.0   1.0   8.0  -3.0
  !   4      0.0   0.0  -4.0   5.0
  a = 0.0d0
  a(1) =  2.0d0;  a(2) = -1.0d0;  a(3) = 0.0d0;   a(4) = 0.0d0
  a(5) =  4.0d0;  a(6) = -6.0d0;  a(7) = 1.0d0;   a(8) = 0.0d0
  a(9) = -7.0d0;  a(10) = 2.0d0;  a(11) = 8.0d0;  a(12) = -4.0d0
  a(13) = 1.0d0;  a(14) = 0.0d0;  a(15) = -3.0d0; a(16) = 5.0d0

  result = dlanhs('M', 4, a, 4, work)
  call begin_test('big_max')
  call print_scalar('result', result)
  call end_test()

  result = dlanhs('1', 4, a, 4, work)
  call begin_test('big_one')
  call print_scalar('result', result)
  call end_test()

  result = dlanhs('I', 4, a, 4, work)
  call begin_test('big_inf')
  call print_scalar('result', result)
  call end_test()

  result = dlanhs('F', 4, a, 4, work)
  call begin_test('big_frob')
  call print_scalar('result', result)
  call end_test()

end program
