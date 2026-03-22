program test_dlaev2
  use test_utils
  implicit none
  double precision :: a, b, c, rt1, rt2, cs1, sn1

  ! Test 1: identity matrix [[1,0],[0,1]]
  a = 1.0d0
  b = 0.0d0
  c = 1.0d0
  call dlaev2(a, b, c, rt1, rt2, cs1, sn1)
  call begin_test('identity')
  call print_scalar('a', a)
  call print_scalar('b', b)
  call print_scalar('c', c)
  call print_scalar('rt1', rt1)
  call print_scalar('rt2', rt2)
  call print_scalar('cs1', cs1)
  call print_scalar('sn1', sn1)
  call end_test()

  ! Test 2: diagonal matrix [[3,0],[0,1]]
  a = 3.0d0
  b = 0.0d0
  c = 1.0d0
  call dlaev2(a, b, c, rt1, rt2, cs1, sn1)
  call begin_test('diagonal')
  call print_scalar('a', a)
  call print_scalar('b', b)
  call print_scalar('c', c)
  call print_scalar('rt1', rt1)
  call print_scalar('rt2', rt2)
  call print_scalar('cs1', cs1)
  call print_scalar('sn1', sn1)
  call end_test()

  ! Test 3: off-diagonal only [[0,2],[2,0]]
  a = 0.0d0
  b = 2.0d0
  c = 0.0d0
  call dlaev2(a, b, c, rt1, rt2, cs1, sn1)
  call begin_test('off_diagonal')
  call print_scalar('a', a)
  call print_scalar('b', b)
  call print_scalar('c', c)
  call print_scalar('rt1', rt1)
  call print_scalar('rt2', rt2)
  call print_scalar('cs1', cs1)
  call print_scalar('sn1', sn1)
  call end_test()

  ! Test 4: general case [[4,3],[3,2]]
  a = 4.0d0
  b = 3.0d0
  c = 2.0d0
  call dlaev2(a, b, c, rt1, rt2, cs1, sn1)
  call begin_test('general')
  call print_scalar('a', a)
  call print_scalar('b', b)
  call print_scalar('c', c)
  call print_scalar('rt1', rt1)
  call print_scalar('rt2', rt2)
  call print_scalar('cs1', cs1)
  call print_scalar('sn1', sn1)
  call end_test()

  ! Test 5: negative diagonal [[-5,1],[1,-3]]
  a = -5.0d0
  b = 1.0d0
  c = -3.0d0
  call dlaev2(a, b, c, rt1, rt2, cs1, sn1)
  call begin_test('negative_diagonal')
  call print_scalar('a', a)
  call print_scalar('b', b)
  call print_scalar('c', c)
  call print_scalar('rt1', rt1)
  call print_scalar('rt2', rt2)
  call print_scalar('cs1', cs1)
  call print_scalar('sn1', sn1)
  call end_test()

  ! Test 6: sm = 0 path [[3,2],[2,-3]]
  a = 3.0d0
  b = 2.0d0
  c = -3.0d0
  call dlaev2(a, b, c, rt1, rt2, cs1, sn1)
  call begin_test('sm_zero')
  call print_scalar('a', a)
  call print_scalar('b', b)
  call print_scalar('c', c)
  call print_scalar('rt1', rt1)
  call print_scalar('rt2', rt2)
  call print_scalar('cs1', cs1)
  call print_scalar('sn1', sn1)
  call end_test()

  ! Test 7: equal diagonal [[2,1],[1,2]]
  a = 2.0d0
  b = 1.0d0
  c = 2.0d0
  call dlaev2(a, b, c, rt1, rt2, cs1, sn1)
  call begin_test('equal_diagonal')
  call print_scalar('a', a)
  call print_scalar('b', b)
  call print_scalar('c', c)
  call print_scalar('rt1', rt1)
  call print_scalar('rt2', rt2)
  call print_scalar('cs1', cs1)
  call print_scalar('sn1', sn1)
  call end_test()

  ! Test 8: negative off-diagonal [[2,-3],[-3,5]]
  a = 2.0d0
  b = -3.0d0
  c = 5.0d0
  call dlaev2(a, b, c, rt1, rt2, cs1, sn1)
  call begin_test('negative_offdiag')
  call print_scalar('a', a)
  call print_scalar('b', b)
  call print_scalar('c', c)
  call print_scalar('rt1', rt1)
  call print_scalar('rt2', rt2)
  call print_scalar('cs1', cs1)
  call print_scalar('sn1', sn1)
  call end_test()

  ! Test 9: df < 0 path (a < c) [[1,2],[2,5]]
  a = 1.0d0
  b = 2.0d0
  c = 5.0d0
  call dlaev2(a, b, c, rt1, rt2, cs1, sn1)
  call begin_test('df_negative')
  call print_scalar('a', a)
  call print_scalar('b', b)
  call print_scalar('c', c)
  call print_scalar('rt1', rt1)
  call print_scalar('rt2', rt2)
  call print_scalar('cs1', cs1)
  call print_scalar('sn1', sn1)
  call end_test()

  ! Test 10: b=0 and a < c (acs > ab with ab=0)
  a = 1.0d0
  b = 0.0d0
  c = 3.0d0
  call dlaev2(a, b, c, rt1, rt2, cs1, sn1)
  call begin_test('b_zero_a_lt_c')
  call print_scalar('a', a)
  call print_scalar('b', b)
  call print_scalar('c', c)
  call print_scalar('rt1', rt1)
  call print_scalar('rt2', rt2)
  call print_scalar('cs1', cs1)
  call print_scalar('sn1', sn1)
  call end_test()

end program
