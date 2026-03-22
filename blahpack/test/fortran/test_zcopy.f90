program test_zcopy
  use test_utils
  implicit none
  complex*16 :: zx(10), zy(10)
  double precision :: zx_r(20), zy_r(20)
  equivalence (zx, zx_r)
  equivalence (zy, zy_r)
  integer :: n

  ! Test 1: basic copy, n=3, incx=1, incy=1
  zx = (0.0d0, 0.0d0)
  zy = (0.0d0, 0.0d0)
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (3.0d0, 4.0d0)
  zx(3) = (5.0d0, 6.0d0)
  n = 3
  call zcopy(n, zx, 1, zy, 1)
  call begin_test('zcopy_basic')
  call print_array('zx', zx_r, 2*n)
  call print_array('zy', zy_r, 2*n)
  call end_test()

  ! Test 2: n=0 (no-op)
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (3.0d0, 4.0d0)
  zy(1) = (99.0d0, 88.0d0)
  zy(2) = (77.0d0, 66.0d0)
  n = 2
  call zcopy(0, zx, 1, zy, 1)
  call begin_test('zcopy_n_zero')
  call print_array('zy', zy_r, 2*n)
  call end_test()

  ! Test 3: non-unit stride, incx=2, incy=2
  zx = (0.0d0, 0.0d0)
  zy = (0.0d0, 0.0d0)
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (99.0d0, 99.0d0)
  zx(3) = (3.0d0, 4.0d0)
  zx(4) = (99.0d0, 99.0d0)
  zx(5) = (5.0d0, 6.0d0)
  zy(1) = (77.0d0, 77.0d0)
  zy(2) = (88.0d0, 88.0d0)
  zy(3) = (77.0d0, 77.0d0)
  zy(4) = (88.0d0, 88.0d0)
  zy(5) = (77.0d0, 77.0d0)
  n = 3
  call zcopy(n, zx, 2, zy, 2)
  call begin_test('zcopy_stride')
  call print_array('zx', zx_r, 10)
  call print_array('zy', zy_r, 10)
  call end_test()

  ! Test 4: mixed strides, incx=1, incy=2
  zx = (0.0d0, 0.0d0)
  zy = (0.0d0, 0.0d0)
  zx(1) = (10.0d0, 20.0d0)
  zx(2) = (30.0d0, 40.0d0)
  zx(3) = (50.0d0, 60.0d0)
  zy(1) = (0.0d0, 0.0d0)
  zy(2) = (88.0d0, 88.0d0)
  zy(3) = (0.0d0, 0.0d0)
  zy(4) = (88.0d0, 88.0d0)
  zy(5) = (0.0d0, 0.0d0)
  n = 3
  call zcopy(n, zx, 1, zy, 2)
  call begin_test('zcopy_mixed_stride')
  call print_array('zx', zx_r, 6)
  call print_array('zy', zy_r, 10)
  call end_test()

end program
