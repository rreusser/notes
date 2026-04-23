program test_zaxpy
  use test_utils
  implicit none
  complex*16 :: zx(10), zy(10)
  double precision :: zx_r(20), zy_r(20)
  equivalence (zx, zx_r)
  equivalence (zy, zy_r)
  complex*16 :: za
  integer :: n

  ! Test 1: basic, n=3, za=(2,3), incx=1, incy=1
  ! y(1) = y(1) + (2+3i)*(1+2i) = (10+20i) + (-4+7i) = (6+27i)
  ! y(2) = y(2) + (2+3i)*(3+4i) = (30+40i) + (-6+17i) = (24+57i)
  ! y(3) = y(3) + (2+3i)*(5+6i) = (50+60i) + (-8+27i) = (42+87i)
  zx = (0.0d0, 0.0d0)
  zy = (0.0d0, 0.0d0)
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (3.0d0, 4.0d0)
  zx(3) = (5.0d0, 6.0d0)
  zy(1) = (10.0d0, 20.0d0)
  zy(2) = (30.0d0, 40.0d0)
  zy(3) = (50.0d0, 60.0d0)
  n = 3
  za = (2.0d0, 3.0d0)
  call zaxpy(n, za, zx, 1, zy, 1)
  call begin_test('zaxpy_basic')
  call print_array('zy', zy_r, 2*n)
  call print_array('zx', zx_r, 2*n)
  call end_test()

  ! Test 2: za=(0,0) — should be no-op
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (3.0d0, 4.0d0)
  zy(1) = (10.0d0, 20.0d0)
  zy(2) = (30.0d0, 40.0d0)
  n = 2
  za = (0.0d0, 0.0d0)
  call zaxpy(n, za, zx, 1, zy, 1)
  call begin_test('zaxpy_alpha_zero')
  call print_array('zy', zy_r, 2*n)
  call end_test()

  ! Test 3: za=(1,0) — y += x
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (3.0d0, 4.0d0)
  zx(3) = (5.0d0, 6.0d0)
  zy(1) = (10.0d0, 20.0d0)
  zy(2) = (30.0d0, 40.0d0)
  zy(3) = (50.0d0, 60.0d0)
  n = 3
  za = (1.0d0, 0.0d0)
  call zaxpy(n, za, zx, 1, zy, 1)
  call begin_test('zaxpy_alpha_one')
  call print_array('zy', zy_r, 2*n)
  call end_test()

  ! Test 4: n=0 (no-op)
  zx(1) = (1.0d0, 2.0d0)
  zy(1) = (10.0d0, 20.0d0)
  za = (5.0d0, 6.0d0)
  call zaxpy(0, za, zx, 1, zy, 1)
  call begin_test('zaxpy_n_zero')
  call print_array('zy', zy_r, 2)
  call end_test()

  ! Test 5: n=1
  zx(1) = (2.0d0, 3.0d0)
  zy(1) = (4.0d0, 5.0d0)
  n = 1
  za = (1.0d0, 1.0d0)
  call zaxpy(n, za, zx, 1, zy, 1)
  call begin_test('zaxpy_n_one')
  call print_array('zy', zy_r, 2)
  call end_test()

  ! Test 6: non-unit strides, incx=2, incy=2
  zx = (0.0d0, 0.0d0)
  zy = (0.0d0, 0.0d0)
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (99.0d0, 99.0d0)
  zx(3) = (3.0d0, 4.0d0)
  zx(4) = (99.0d0, 99.0d0)
  zx(5) = (5.0d0, 6.0d0)
  zy(1) = (10.0d0, 20.0d0)
  zy(2) = (88.0d0, 88.0d0)
  zy(3) = (30.0d0, 40.0d0)
  zy(4) = (88.0d0, 88.0d0)
  zy(5) = (50.0d0, 60.0d0)
  n = 3
  za = (2.0d0, 3.0d0)
  call zaxpy(n, za, zx, 2, zy, 2)
  call begin_test('zaxpy_stride')
  call print_array('zy', zy_r, 10)
  call print_array('zx', zx_r, 10)
  call end_test()

  ! Test 7: negative stride (incx=-1, incy=1)
  ! With negative INCX, Fortran starts at index (-N+1)*INCX+1 = (-3+1)*(-1)+1 = 3
  ! So it reads x(3), x(2), x(1) and writes to y(1), y(2), y(3)
  zx = (0.0d0, 0.0d0)
  zy = (0.0d0, 0.0d0)
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (3.0d0, 4.0d0)
  zx(3) = (5.0d0, 6.0d0)
  zy(1) = (10.0d0, 20.0d0)
  zy(2) = (30.0d0, 40.0d0)
  zy(3) = (50.0d0, 60.0d0)
  n = 3
  za = (1.0d0, 0.0d0)
  call zaxpy(n, za, zx, -1, zy, 1)
  call begin_test('zaxpy_neg_stride')
  call print_array('zy', zy_r, 2*n)
  call end_test()

  ! Test 8: purely imaginary alpha
  zx = (0.0d0, 0.0d0)
  zy = (0.0d0, 0.0d0)
  zx(1) = (1.0d0, 0.0d0)
  zx(2) = (0.0d0, 1.0d0)
  zx(3) = (1.0d0, 1.0d0)
  zy(1) = (0.0d0, 0.0d0)
  zy(2) = (0.0d0, 0.0d0)
  zy(3) = (0.0d0, 0.0d0)
  n = 3
  za = (0.0d0, 2.0d0)
  call zaxpy(n, za, zx, 1, zy, 1)
  call begin_test('zaxpy_imag_alpha')
  call print_array('zy', zy_r, 2*n)
  call end_test()

end program
