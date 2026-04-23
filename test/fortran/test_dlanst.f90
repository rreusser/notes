program test_dlanst
  use test_utils
  implicit none
  double precision :: d(10), e(10), anorm
  double precision :: dlanst
  external :: dlanst

  ! Test 1: N=0 (quick return)
  anorm = dlanst('M', 0, d, e)
  call begin_test('n_zero')
  call print_scalar('anorm', anorm)
  call end_test()

  ! Test 2: N=1, max norm (only diagonal)
  d(1) = -3.0d0
  anorm = dlanst('M', 1, d, e)
  call begin_test('n1_max')
  call print_scalar('d1', d(1))
  call print_scalar('anorm', anorm)
  call end_test()

  ! Test 3: N=1, one-norm
  d(1) = -3.0d0
  anorm = dlanst('1', 1, d, e)
  call begin_test('n1_one')
  call print_scalar('d1', d(1))
  call print_scalar('anorm', anorm)
  call end_test()

  ! Test 4: N=1, infinity-norm
  d(1) = -3.0d0
  anorm = dlanst('I', 1, d, e)
  call begin_test('n1_inf')
  call print_scalar('d1', d(1))
  call print_scalar('anorm', anorm)
  call end_test()

  ! Test 5: N=1, Frobenius norm
  d(1) = -3.0d0
  anorm = dlanst('F', 1, d, e)
  call begin_test('n1_frob')
  call print_scalar('d1', d(1))
  call print_scalar('anorm', anorm)
  call end_test()

  ! Set up a general tridiagonal matrix for tests 6-9:
  ! d = [2, -4, 6, -1, 3]
  ! e = [1, -2, 3, 5]
  d(1) = 2.0d0
  d(2) = -4.0d0
  d(3) = 6.0d0
  d(4) = -1.0d0
  d(5) = 3.0d0
  e(1) = 1.0d0
  e(2) = -2.0d0
  e(3) = 3.0d0
  e(4) = 5.0d0

  ! Test 6: max norm, N=5
  anorm = dlanst('M', 5, d, e)
  call begin_test('n5_max')
  call print_array('d', d, 5)
  call print_array('e', e, 4)
  call print_scalar('anorm', anorm)
  call end_test()

  ! Test 7: one-norm, N=5
  anorm = dlanst('O', 5, d, e)
  call begin_test('n5_one')
  call print_array('d', d, 5)
  call print_array('e', e, 4)
  call print_scalar('anorm', anorm)
  call end_test()

  ! Test 8: infinity-norm (same as one-norm for symmetric), N=5
  anorm = dlanst('I', 5, d, e)
  call begin_test('n5_inf')
  call print_array('d', d, 5)
  call print_array('e', e, 4)
  call print_scalar('anorm', anorm)
  call end_test()

  ! Test 9: Frobenius norm, N=5
  anorm = dlanst('F', 5, d, e)
  call begin_test('n5_frob')
  call print_array('d', d, 5)
  call print_array('e', e, 4)
  call print_scalar('anorm', anorm)
  call end_test()

  ! Test 10: N=2 (boundary case for one-norm)
  d(1) = 1.0d0
  d(2) = 2.0d0
  e(1) = 3.0d0
  anorm = dlanst('1', 2, d, e)
  call begin_test('n2_one')
  call print_scalar('d1', d(1))
  call print_scalar('d2', d(2))
  call print_scalar('e1', e(1))
  call print_scalar('anorm', anorm)
  call end_test()

  ! Test 11: N=2 Frobenius norm
  anorm = dlanst('F', 2, d, e)
  call begin_test('n2_frob')
  call print_scalar('d1', d(1))
  call print_scalar('d2', d(2))
  call print_scalar('e1', e(1))
  call print_scalar('anorm', anorm)
  call end_test()

  ! Test 12: 'E' norm type (should be same as 'F')
  anorm = dlanst('E', 2, d, e)
  call begin_test('n2_e_norm')
  call print_scalar('d1', d(1))
  call print_scalar('d2', d(2))
  call print_scalar('e1', e(1))
  call print_scalar('anorm', anorm)
  call end_test()

  ! Test 13: max norm N=5, all positive
  d(1) = 1.0d0
  d(2) = 2.0d0
  d(3) = 3.0d0
  d(4) = 4.0d0
  d(5) = 5.0d0
  e(1) = 0.5d0
  e(2) = 1.5d0
  e(3) = 2.5d0
  e(4) = 3.5d0
  anorm = dlanst('M', 5, d, e)
  call begin_test('n5_max_positive')
  call print_array('d', d, 5)
  call print_array('e', e, 4)
  call print_scalar('anorm', anorm)
  call end_test()

end program
