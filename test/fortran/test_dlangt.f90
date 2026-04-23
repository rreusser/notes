program test_dlangt
  use test_utils
  implicit none
  double precision :: dl(10), d(10), du(10)
  double precision :: dlangt
  external dlangt
  double precision :: result

  ! Test 1: max-norm ('M') with 4x4 tridiagonal
  ! A = [2  -1   0   0]
  !     [3   4  -2   0]
  !     [0   1   5  -3]
  !     [0   0   2   6]
  dl(1) = 3.0d0; dl(2) = 1.0d0; dl(3) = 2.0d0
  d(1) = 2.0d0; d(2) = 4.0d0; d(3) = 5.0d0; d(4) = 6.0d0
  du(1) = -1.0d0; du(2) = -2.0d0; du(3) = -3.0d0
  result = dlangt('M', 4, dl, d, du)
  call begin_test('max_norm_4x4')
  call print_scalar('result', result)
  call end_test()

  ! Test 2: one-norm ('1') with 4x4
  result = dlangt('1', 4, dl, d, du)
  call begin_test('one_norm_4x4')
  call print_scalar('result', result)
  call end_test()

  ! Test 3: one-norm ('O') alias
  result = dlangt('O', 4, dl, d, du)
  call begin_test('one_norm_O_4x4')
  call print_scalar('result', result)
  call end_test()

  ! Test 4: infinity-norm ('I') with 4x4
  result = dlangt('I', 4, dl, d, du)
  call begin_test('inf_norm_4x4')
  call print_scalar('result', result)
  call end_test()

  ! Test 5: Frobenius norm ('F') with 4x4
  result = dlangt('F', 4, dl, d, du)
  call begin_test('frob_norm_4x4')
  call print_scalar('result', result)
  call end_test()

  ! Test 6: Frobenius norm with 'E' alias
  result = dlangt('E', 4, dl, d, du)
  call begin_test('frob_norm_E_4x4')
  call print_scalar('result', result)
  call end_test()

  ! Test 7: N=1
  d(1) = -7.0d0
  result = dlangt('M', 1, dl, d, du)
  call begin_test('max_norm_n1')
  call print_scalar('result', result)
  call end_test()

  result = dlangt('1', 1, dl, d, du)
  call begin_test('one_norm_n1')
  call print_scalar('result', result)
  call end_test()

  result = dlangt('I', 1, dl, d, du)
  call begin_test('inf_norm_n1')
  call print_scalar('result', result)
  call end_test()

  result = dlangt('F', 1, dl, d, du)
  call begin_test('frob_norm_n1')
  call print_scalar('result', result)
  call end_test()

  ! Test 8: N=0
  result = dlangt('M', 0, dl, d, du)
  call begin_test('n_zero')
  call print_scalar('result', result)
  call end_test()

  ! Test 9: 5x5 with larger values
  dl(1) = 1.0d0; dl(2) = 2.0d0; dl(3) = 3.0d0; dl(4) = 4.0d0
  d(1) = 10.0d0; d(2) = 20.0d0; d(3) = 30.0d0; d(4) = 40.0d0; d(5) = 50.0d0
  du(1) = 5.0d0; du(2) = 6.0d0; du(3) = 7.0d0; du(4) = 8.0d0
  result = dlangt('M', 5, dl, d, du)
  call begin_test('max_norm_5x5')
  call print_scalar('result', result)
  call end_test()

  result = dlangt('1', 5, dl, d, du)
  call begin_test('one_norm_5x5')
  call print_scalar('result', result)
  call end_test()

  result = dlangt('I', 5, dl, d, du)
  call begin_test('inf_norm_5x5')
  call print_scalar('result', result)
  call end_test()

  result = dlangt('F', 5, dl, d, du)
  call begin_test('frob_norm_5x5')
  call print_scalar('result', result)
  call end_test()

  ! Test 10: N=2 edge case
  dl(1) = 0.5d0
  d(1) = 3.0d0; d(2) = 4.0d0
  du(1) = 1.5d0
  result = dlangt('M', 2, dl, d, du)
  call begin_test('max_norm_n2')
  call print_scalar('result', result)
  call end_test()

  result = dlangt('1', 2, dl, d, du)
  call begin_test('one_norm_n2')
  call print_scalar('result', result)
  call end_test()

  result = dlangt('I', 2, dl, d, du)
  call begin_test('inf_norm_n2')
  call print_scalar('result', result)
  call end_test()

  result = dlangt('F', 2, dl, d, du)
  call begin_test('frob_norm_n2')
  call print_scalar('result', result)
  call end_test()

end program
