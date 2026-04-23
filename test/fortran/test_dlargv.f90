program test_dlargv
  use test_utils
  implicit none
  double precision :: x(20), y(20), c(20)
  integer :: i

  ! Test 1: basic — mixed cases in one call
  ! x = [3, 0, 5, 1], y = [0, 4, 3, 2]
  ! Entry 1: g=0, Entry 2: f=0, Entry 3: |f|>|g|, Entry 4: |f|<|g|
  x = 0.0d0; y = 0.0d0; c = 0.0d0
  x(1) = 3.0d0; x(2) = 0.0d0; x(3) = 5.0d0; x(4) = 1.0d0
  y(1) = 0.0d0; y(2) = 4.0d0; y(3) = 3.0d0; y(4) = 2.0d0
  call dlargv(4, x, 1, y, 1, c, 1)
  call begin_test('basic')
  call print_array('x', x, 4)
  call print_array('y', y, 4)
  call print_array('c', c, 4)
  call end_test()

  ! Test 2: n=0 (quick return, no-op)
  x = 0.0d0; y = 0.0d0; c = 0.0d0
  x(1) = 99.0d0; y(1) = 99.0d0; c(1) = 99.0d0
  call dlargv(0, x, 1, y, 1, c, 1)
  call begin_test('n_zero')
  call print_array('x', x, 1)
  call print_array('y', y, 1)
  call print_array('c', c, 1)
  call end_test()

  ! Test 3: n=1 with |f|>|g|
  x = 0.0d0; y = 0.0d0; c = 0.0d0
  x(1) = 4.0d0
  y(1) = 3.0d0
  call dlargv(1, x, 1, y, 1, c, 1)
  call begin_test('n_one_f_gt_g')
  call print_array('x', x, 1)
  call print_array('y', y, 1)
  call print_array('c', c, 1)
  call end_test()

  ! Test 4: n=1 with |f|<|g|
  x = 0.0d0; y = 0.0d0; c = 0.0d0
  x(1) = 3.0d0
  y(1) = 4.0d0
  call dlargv(1, x, 1, y, 1, c, 1)
  call begin_test('n_one_f_lt_g')
  call print_array('x', x, 1)
  call print_array('y', y, 1)
  call print_array('c', c, 1)
  call end_test()

  ! Test 5: all g=0 (cosines should all be 1)
  x = 0.0d0; y = 0.0d0; c = 0.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0
  y(1) = 0.0d0; y(2) = 0.0d0; y(3) = 0.0d0
  call dlargv(3, x, 1, y, 1, c, 1)
  call begin_test('all_g_zero')
  call print_array('x', x, 3)
  call print_array('y', y, 3)
  call print_array('c', c, 3)
  call end_test()

  ! Test 6: all f=0
  x = 0.0d0; y = 0.0d0; c = 0.0d0
  x(1) = 0.0d0; x(2) = 0.0d0; x(3) = 0.0d0
  y(1) = 5.0d0; y(2) = 6.0d0; y(3) = 7.0d0
  call dlargv(3, x, 1, y, 1, c, 1)
  call begin_test('all_f_zero')
  call print_array('x', x, 3)
  call print_array('y', y, 3)
  call print_array('c', c, 3)
  call end_test()

  ! Test 7: non-unit strides (incx=2, incy=3, incc=2)
  x = 0.0d0; y = 0.0d0; c = 0.0d0
  x(1) = 3.0d0; x(3) = 0.0d0; x(5) = 5.0d0
  y(1) = 4.0d0; y(4) = 7.0d0; y(7) = 12.0d0
  call dlargv(3, x, 2, y, 3, c, 2)
  call begin_test('stride')
  call print_array('x', x, 6)
  call print_array('y', y, 9)
  call print_array('c', c, 6)
  call end_test()

  ! Test 8: negative values of f and g
  x = 0.0d0; y = 0.0d0; c = 0.0d0
  x(1) = -3.0d0; x(2) = -5.0d0; x(3) = 4.0d0
  y(1) = 4.0d0; y(2) = -12.0d0; y(3) = -3.0d0
  call dlargv(3, x, 1, y, 1, c, 1)
  call begin_test('negative')
  call print_array('x', x, 3)
  call print_array('y', y, 3)
  call print_array('c', c, 3)
  call end_test()

  ! Test 9: equal magnitudes |f| == |g|
  x = 0.0d0; y = 0.0d0; c = 0.0d0
  x(1) = 1.0d0; x(2) = -1.0d0; x(3) = 3.0d0
  y(1) = 1.0d0; y(2) = 1.0d0; y(3) = -3.0d0
  call dlargv(3, x, 1, y, 1, c, 1)
  call begin_test('equal_mag')
  call print_array('x', x, 3)
  call print_array('y', y, 3)
  call print_array('c', c, 3)
  call end_test()

  ! Test 10: large values
  x = 0.0d0; y = 0.0d0; c = 0.0d0
  x(1) = 1.0d200; x(2) = 1.0d-200
  y(1) = 1.0d200; y(2) = 1.0d-200
  call dlargv(2, x, 1, y, 1, c, 1)
  call begin_test('large_values')
  call print_array('x', x, 2)
  call print_array('y', y, 2)
  call print_array('c', c, 2)
  call end_test()

end program
