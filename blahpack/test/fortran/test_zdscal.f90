program test_zdscal
  use test_utils
  implicit none
  complex*16 :: zx(10)
  double precision :: zx_real(20)
  equivalence (zx, zx_real)
  double precision :: da
  integer :: n

  ! Test 1: basic scaling, n=3, da=2.0, incx=1
  zx = (0.0d0, 0.0d0)
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (3.0d0, 4.0d0)
  zx(3) = (5.0d0, 6.0d0)
  n = 3
  da = 2.0d0
  call zdscal(n, da, zx, 1)
  call begin_test('basic')
  call print_array('zx', zx_real, 2*n)
  call end_test()

  ! Test 2: da=0 (should zero out the vector)
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (3.0d0, 4.0d0)
  zx(3) = (5.0d0, 6.0d0)
  n = 3
  da = 0.0d0
  call zdscal(n, da, zx, 1)
  call begin_test('da_zero')
  call print_array('zx', zx_real, 2*n)
  call end_test()

  ! Test 3: da=1 (no-op, zdscal returns early for da=1)
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (3.0d0, 4.0d0)
  zx(3) = (5.0d0, 6.0d0)
  n = 3
  da = 1.0d0
  call zdscal(n, da, zx, 1)
  call begin_test('da_one')
  call print_array('zx', zx_real, 2*n)
  call end_test()

  ! Test 4: n=0 (no-op)
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (3.0d0, 4.0d0)
  n = 2
  call zdscal(0, 5.0d0, zx, 1)
  call begin_test('n_zero')
  call print_array('zx', zx_real, 2*n)
  call end_test()

  ! Test 5: n=1
  zx(1) = (7.0d0, 3.0d0)
  call zdscal(1, 3.0d0, zx, 1)
  call begin_test('n_one')
  call print_array('zx', zx_real, 2)
  call end_test()

  ! Test 6: non-unit stride (incx=2)
  zx = (0.0d0, 0.0d0)
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (99.0d0, 99.0d0)
  zx(3) = (3.0d0, 4.0d0)
  zx(4) = (99.0d0, 99.0d0)
  zx(5) = (5.0d0, 6.0d0)
  n = 3
  da = 4.0d0
  call zdscal(n, da, zx, 2)
  call begin_test('stride')
  call print_array('zx', zx_real, 10)
  call end_test()

  ! Test 7: negative da
  zx(1) = (2.0d0, 3.0d0)
  zx(2) = (4.0d0, 5.0d0)
  n = 2
  da = -2.0d0
  call zdscal(n, da, zx, 1)
  call begin_test('neg_da')
  call print_array('zx', zx_real, 2*n)
  call end_test()

end program
