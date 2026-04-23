program test_dlae2
  use test_utils
  implicit none
  double precision :: a, b, c, rt1, rt2

  ! Test 1: diagonal matrix (b=0), a > c
  ! eigenvalues of [[3,0],[0,1]] are 3 and 1
  a = 3.0d0
  b = 0.0d0
  c = 1.0d0
  call dlae2(a, b, c, rt1, rt2)
  call begin_test('diagonal_a_gt_c')
  call print_scalar('a', a)
  call print_scalar('b', b)
  call print_scalar('c', c)
  call print_scalar('rt1', rt1)
  call print_scalar('rt2', rt2)
  call end_test()

  ! Test 2: diagonal matrix (b=0), c > a
  ! eigenvalues of [[1,0],[0,3]] are 3 and 1
  a = 1.0d0
  b = 0.0d0
  c = 3.0d0
  call dlae2(a, b, c, rt1, rt2)
  call begin_test('diagonal_c_gt_a')
  call print_scalar('a', a)
  call print_scalar('b', b)
  call print_scalar('c', c)
  call print_scalar('rt1', rt1)
  call print_scalar('rt2', rt2)
  call end_test()

  ! Test 3: off-diagonal only (a=c=0)
  ! eigenvalues of [[0,2],[2,0]] are 2 and -2
  a = 0.0d0
  b = 2.0d0
  c = 0.0d0
  call dlae2(a, b, c, rt1, rt2)
  call begin_test('off_diagonal')
  call print_scalar('a', a)
  call print_scalar('b', b)
  call print_scalar('c', c)
  call print_scalar('rt1', rt1)
  call print_scalar('rt2', rt2)
  call end_test()

  ! Test 4: equal diagonal elements
  ! eigenvalues of [[2,1],[1,2]] are 3 and 1
  a = 2.0d0
  b = 1.0d0
  c = 2.0d0
  call dlae2(a, b, c, rt1, rt2)
  call begin_test('equal_diagonal')
  call print_scalar('a', a)
  call print_scalar('b', b)
  call print_scalar('c', c)
  call print_scalar('rt1', rt1)
  call print_scalar('rt2', rt2)
  call end_test()

  ! Test 5: general 2x2 matrix
  ! [[4, 3], [3, 2]]
  a = 4.0d0
  b = 3.0d0
  c = 2.0d0
  call dlae2(a, b, c, rt1, rt2)
  call begin_test('general')
  call print_scalar('a', a)
  call print_scalar('b', b)
  call print_scalar('c', c)
  call print_scalar('rt1', rt1)
  call print_scalar('rt2', rt2)
  call end_test()

  ! Test 6: negative diagonal, sm < 0 path
  ! [[-5, 1], [1, -3]]
  a = -5.0d0
  b = 1.0d0
  c = -3.0d0
  call dlae2(a, b, c, rt1, rt2)
  call begin_test('negative_diagonal')
  call print_scalar('a', a)
  call print_scalar('b', b)
  call print_scalar('c', c)
  call print_scalar('rt1', rt1)
  call print_scalar('rt2', rt2)
  call end_test()

  ! Test 7: sm = 0 path (a = -c)
  ! [[3, 2], [2, -3]]
  a = 3.0d0
  b = 2.0d0
  c = -3.0d0
  call dlae2(a, b, c, rt1, rt2)
  call begin_test('sm_zero')
  call print_scalar('a', a)
  call print_scalar('b', b)
  call print_scalar('c', c)
  call print_scalar('rt1', rt1)
  call print_scalar('rt2', rt2)
  call end_test()

  ! Test 8: identity matrix
  a = 1.0d0
  b = 0.0d0
  c = 1.0d0
  call dlae2(a, b, c, rt1, rt2)
  call begin_test('identity')
  call print_scalar('a', a)
  call print_scalar('b', b)
  call print_scalar('c', c)
  call print_scalar('rt1', rt1)
  call print_scalar('rt2', rt2)
  call end_test()

  ! Test 9: adf < ab path (small diagonal difference, large off-diagonal)
  a = 1.0d0
  b = 10.0d0
  c = 1.0d0
  call dlae2(a, b, c, rt1, rt2)
  call begin_test('adf_lt_ab')
  call print_scalar('a', a)
  call print_scalar('b', b)
  call print_scalar('c', c)
  call print_scalar('rt1', rt1)
  call print_scalar('rt2', rt2)
  call end_test()

  ! Test 10: adf = ab path (equal adf and ab)
  a = 2.0d0
  b = 0.5d0
  c = 1.0d0
  call dlae2(a, b, c, rt1, rt2)
  call begin_test('adf_eq_ab')
  call print_scalar('a', a)
  call print_scalar('b', b)
  call print_scalar('c', c)
  call print_scalar('rt1', rt1)
  call print_scalar('rt2', rt2)
  call end_test()

end program
