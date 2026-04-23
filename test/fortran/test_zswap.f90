program test_zswap
  use test_utils
  implicit none
  complex*16 :: zx(10), zy(10)
  double precision :: zx_real(20), zy_real(20)
  equivalence (zx, zx_real)
  equivalence (zy, zy_real)
  integer :: n

  ! Test 1: basic swap, n=3, incx=1, incy=1
  zx = (0.0d0, 0.0d0)
  zy = (0.0d0, 0.0d0)
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (3.0d0, 4.0d0)
  zx(3) = (5.0d0, 6.0d0)
  zy(1) = (7.0d0, 8.0d0)
  zy(2) = (9.0d0, 10.0d0)
  zy(3) = (11.0d0, 12.0d0)
  n = 3
  call zswap(n, zx, 1, zy, 1)
  call begin_test('basic')
  call print_array('zx', zx_real, 2*n)
  call print_array('zy', zy_real, 2*n)
  call end_test()

  ! Test 2: n=0 (no-op)
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (3.0d0, 4.0d0)
  zy(1) = (5.0d0, 6.0d0)
  zy(2) = (7.0d0, 8.0d0)
  n = 2
  call zswap(0, zx, 1, zy, 1)
  call begin_test('n_zero')
  call print_array('zx', zx_real, 2*n)
  call print_array('zy', zy_real, 2*n)
  call end_test()

  ! Test 3: n=1
  zx(1) = (10.0d0, 20.0d0)
  zy(1) = (30.0d0, 40.0d0)
  call zswap(1, zx, 1, zy, 1)
  call begin_test('n_one')
  call print_array('zx', zx_real, 2)
  call print_array('zy', zy_real, 2)
  call end_test()

  ! Test 4: non-unit strides, incx=2, incy=2
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
  call zswap(n, zx, 2, zy, 2)
  call begin_test('stride')
  call print_array('zx', zx_real, 10)
  call print_array('zy', zy_real, 10)
  call end_test()

  ! Test 5: mixed strides, incx=1, incy=2
  zx = (0.0d0, 0.0d0)
  zy = (0.0d0, 0.0d0)
  zx(1) = (1.0d0, 1.0d0)
  zx(2) = (2.0d0, 2.0d0)
  zx(3) = (3.0d0, 3.0d0)
  zy(1) = (10.0d0, 10.0d0)
  zy(2) = (77.0d0, 77.0d0)
  zy(3) = (20.0d0, 20.0d0)
  zy(4) = (77.0d0, 77.0d0)
  zy(5) = (30.0d0, 30.0d0)
  n = 3
  call zswap(n, zx, 1, zy, 2)
  call begin_test('mixed_stride')
  call print_array('zx', zx_real, 6)
  call print_array('zy', zy_real, 10)
  call end_test()

end program
