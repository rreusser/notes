program test_zlargv
  use test_utils
  implicit none

  integer, parameter :: NMAX = 20
  complex*16 :: x(NMAX), y(NMAX)
  double precision :: x_r(2*NMAX), y_r(2*NMAX), c(NMAX)
  equivalence (x, x_r)
  equivalence (y, y_r)

  ! Test 1: basic — mixed cases
  ! x = (3+1i, 0+0i, 1+2i, 0+1i), y = (0, 4+0i, 3+1i, 2-1i)
  x = (0.0d0, 0.0d0); y = (0.0d0, 0.0d0); c = 0.0d0
  x(1) = (3.0d0, 1.0d0)
  x(2) = (0.0d0, 0.0d0)
  x(3) = (1.0d0, 2.0d0)
  x(4) = (0.0d0, 1.0d0)
  y(1) = (0.0d0, 0.0d0)
  y(2) = (4.0d0, 0.0d0)
  y(3) = (3.0d0, 1.0d0)
  y(4) = (2.0d0, -1.0d0)
  call zlargv(4, x, 1, y, 1, c, 1)
  call begin_test('basic')
  call print_array('x', x_r, 8)
  call print_array('y', y_r, 8)
  call print_array('c', c, 4)
  call end_test()

  ! Test 2: n=0 (quick return)
  x = (0.0d0, 0.0d0); y = (0.0d0, 0.0d0); c = 0.0d0
  x(1) = (99.0d0, 99.0d0)
  y(1) = (99.0d0, 99.0d0)
  c(1) = 99.0d0
  call zlargv(0, x, 1, y, 1, c, 1)
  call begin_test('n_zero')
  call print_array('x', x_r, 2)
  call print_array('y', y_r, 2)
  call print_array('c', c, 1)
  call end_test()

  ! Test 3: all y=0 (cosines should be 1, sines should be 0)
  x = (0.0d0, 0.0d0); y = (0.0d0, 0.0d0); c = 0.0d0
  x(1) = (1.0d0, 2.0d0)
  x(2) = (3.0d0, 4.0d0)
  x(3) = (5.0d0, 0.0d0)
  y(1) = (0.0d0, 0.0d0)
  y(2) = (0.0d0, 0.0d0)
  y(3) = (0.0d0, 0.0d0)
  call zlargv(3, x, 1, y, 1, c, 1)
  call begin_test('all_y_zero')
  call print_array('x', x_r, 6)
  call print_array('y', y_r, 6)
  call print_array('c', c, 3)
  call end_test()

  ! Test 4: all x=0
  x = (0.0d0, 0.0d0); y = (0.0d0, 0.0d0); c = 0.0d0
  x(1) = (0.0d0, 0.0d0)
  x(2) = (0.0d0, 0.0d0)
  x(3) = (0.0d0, 0.0d0)
  y(1) = (5.0d0, 0.0d0)
  y(2) = (3.0d0, 4.0d0)
  y(3) = (0.0d0, 7.0d0)
  call zlargv(3, x, 1, y, 1, c, 1)
  call begin_test('all_x_zero')
  call print_array('x', x_r, 6)
  call print_array('y', y_r, 6)
  call print_array('c', c, 3)
  call end_test()

  ! Test 5: non-unit strides (incx=2, incy=2, incc=2)
  x = (0.0d0, 0.0d0); y = (0.0d0, 0.0d0); c = 0.0d0
  x(1) = (3.0d0, 1.0d0)
  x(3) = (0.0d0, 0.0d0)
  x(5) = (1.0d0, 2.0d0)
  y(1) = (4.0d0, 0.0d0)
  y(3) = (7.0d0, 0.0d0)
  y(5) = (3.0d0, 1.0d0)
  call zlargv(3, x, 2, y, 2, c, 2)
  call begin_test('stride')
  call print_array('x', x_r, 10)
  call print_array('y', y_r, 10)
  call print_array('c', c, 6)
  call end_test()

  ! Test 6: x with only imaginary parts
  x = (0.0d0, 0.0d0); y = (0.0d0, 0.0d0); c = 0.0d0
  x(1) = (0.0d0, 3.0d0)
  x(2) = (0.0d0, 5.0d0)
  y(1) = (4.0d0, 0.0d0)
  y(2) = (12.0d0, 0.0d0)
  call zlargv(2, x, 1, y, 1, c, 1)
  call begin_test('imag_only_x')
  call print_array('x', x_r, 4)
  call print_array('y', y_r, 4)
  call print_array('c', c, 2)
  call end_test()

  ! Test 7: large values (near overflow rescaling)
  x = (0.0d0, 0.0d0); y = (0.0d0, 0.0d0); c = 0.0d0
  x(1) = (1.0d150, 1.0d150)
  x(2) = (1.0d-150, 1.0d-150)
  y(1) = (1.0d150, 0.0d0)
  y(2) = (1.0d-150, 0.0d0)
  call zlargv(2, x, 1, y, 1, c, 1)
  call begin_test('large_values')
  call print_array('x', x_r, 4)
  call print_array('y', y_r, 4)
  call print_array('c', c, 2)
  call end_test()

  ! Test 8: small values (near underflow rescaling)
  x = (0.0d0, 0.0d0); y = (0.0d0, 0.0d0); c = 0.0d0
  x(1) = (1.0d-300, 2.0d-300)
  y(1) = (3.0d-300, 1.0d-300)
  call zlargv(1, x, 1, y, 1, c, 1)
  call begin_test('small_values')
  call print_array('x', x_r, 2)
  call print_array('y', y_r, 2)
  call print_array('c', c, 1)
  call end_test()

  ! Test 9: x and y both zero
  x = (0.0d0, 0.0d0); y = (0.0d0, 0.0d0); c = 0.0d0
  x(1) = (0.0d0, 0.0d0)
  y(1) = (0.0d0, 0.0d0)
  call zlargv(1, x, 1, y, 1, c, 1)
  call begin_test('both_zero')
  call print_array('x', x_r, 2)
  call print_array('y', y_r, 2)
  call print_array('c', c, 1)
  call end_test()

  ! Test 10: |f| small relative to |g| (f2 <= max(g2, 1)*safmin path, f nonzero)
  x = (0.0d0, 0.0d0); y = (0.0d0, 0.0d0); c = 0.0d0
  x(1) = (1.0d-200, 1.0d-200)
  y(1) = (1.0d0, 0.0d0)
  call zlargv(1, x, 1, y, 1, c, 1)
  call begin_test('f_small_vs_g')
  call print_array('x', x_r, 2)
  call print_array('y', y_r, 2)
  call print_array('c', c, 1)
  call end_test()

  ! Test 11: |f| >> |g| (normal path, f2 > max(g2, 1)*safmin)
  x = (0.0d0, 0.0d0); y = (0.0d0, 0.0d0); c = 0.0d0
  x(1) = (10.0d0, 5.0d0)
  y(1) = (1.0d0, 0.0d0)
  call zlargv(1, x, 1, y, 1, c, 1)
  call begin_test('f_large_vs_g')
  call print_array('x', x_r, 2)
  call print_array('y', y_r, 2)
  call print_array('c', c, 1)
  call end_test()

  ! Test 12: n=1 with negative real y
  x = (0.0d0, 0.0d0); y = (0.0d0, 0.0d0); c = 0.0d0
  x(1) = (2.0d0, 3.0d0)
  y(1) = (-4.0d0, 2.0d0)
  call zlargv(1, x, 1, y, 1, c, 1)
  call begin_test('negative_y')
  call print_array('x', x_r, 2)
  call print_array('y', y_r, 2)
  call print_array('c', c, 1)
  call end_test()

  ! Test 13: f=0, g complex (tests the f==0 branch with complex g)
  x = (0.0d0, 0.0d0); y = (0.0d0, 0.0d0); c = 0.0d0
  x(1) = (0.0d0, 0.0d0)
  y(1) = (3.0d0, 4.0d0)
  call zlargv(1, x, 1, y, 1, c, 1)
  call begin_test('f_zero_g_complex')
  call print_array('x', x_r, 2)
  call print_array('y', y_r, 2)
  call print_array('c', c, 1)
  call end_test()

  ! Test 14: |f| > 1 in the small-f path (ABS1(F) > ONE branch)
  ! Need: f2 <= max(g2,1)*safmin after scaling AND ABS1(f) > 1
  ! f=(2,0), g=(1e200,0): after 1 overflow scaling, fs ~ 3e-154, gs ~ 1.5e46
  ! f2 ~ 9e-308 << max(g2,1)*safmin ~ 5e-216, AND ABS1(f) = 2 > 1
  x = (0.0d0, 0.0d0); y = (0.0d0, 0.0d0); c = 0.0d0
  x(1) = (2.0d0, 0.0d0)
  y(1) = (1.0d200, 0.0d0)
  call zlargv(1, x, 1, y, 1, c, 1)
  call begin_test('f_small_abs1_gt_one')
  call print_array('x', x_r, 2)
  call print_array('y', y_r, 2)
  call print_array('c', c, 1)
  call end_test()

  ! Test 15: |f| < 1 in the small-f path (ABS1(F) <= ONE branch, f nonzero)
  ! Need: f2 <= max(g2,1)*safmin AND ABS1(f) <= 1 AND f != 0
  ! f=(0.5,0.3), g=(1e200,0): after overflow scaling the condition is met
  x = (0.0d0, 0.0d0); y = (0.0d0, 0.0d0); c = 0.0d0
  x(1) = (0.5d0, 0.3d0)
  y(1) = (1.0d200, 0.0d0)
  call zlargv(1, x, 1, y, 1, c, 1)
  call begin_test('f_small_abs1_le_one')
  call print_array('x', x_r, 2)
  call print_array('y', y_r, 2)
  call print_array('c', c, 1)
  call end_test()

  ! Test 16: very large values — trigger overflow scaling (scale >= SAFMX2)
  ! SAFMX2 ~ 6.7e153, so we need ABS1 > 6.7e153
  x = (0.0d0, 0.0d0); y = (0.0d0, 0.0d0); c = 0.0d0
  x(1) = (1.0d200, 1.0d200)
  y(1) = (1.0d200, 1.0d200)
  call zlargv(1, x, 1, y, 1, c, 1)
  call begin_test('very_large')
  call print_array('x', x_r, 2)
  call print_array('y', y_r, 2)
  call print_array('c', c, 1)
  call end_test()

  ! Test 17: trigger count > 0 in common path: both x and y very large,
  ! but x dominant (so we end up in the normal path after scaling)
  x = (0.0d0, 0.0d0); y = (0.0d0, 0.0d0); c = 0.0d0
  x(1) = (1.0d250, 0.0d0)
  y(1) = (1.0d200, 0.0d0)
  call zlargv(1, x, 1, y, 1, c, 1)
  call begin_test('overflow_common_path')
  call print_array('x', x_r, 2)
  call print_array('y', y_r, 2)
  call print_array('c', c, 1)
  call end_test()

end program
