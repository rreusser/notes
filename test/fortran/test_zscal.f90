program test_zscal
  use test_utils
  implicit none
  complex*16 :: zx(10)
  double precision :: zx_r(20)
  equivalence (zx, zx_r)
  complex*16 :: za
  integer :: n

  ! Test 1: basic scaling, n=3, za=(2,3), incx=1
  ! (2+3i)*(1+2i) = (2*1-3*2) + (2*2+3*1)i = -4+7i
  ! (2+3i)*(3+4i) = (6-12) + (8+9)i = -6+17i
  ! (2+3i)*(5+6i) = (10-18) + (12+15)i = -8+27i
  zx = (0.0d0, 0.0d0)
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (3.0d0, 4.0d0)
  zx(3) = (5.0d0, 6.0d0)
  n = 3
  za = (2.0d0, 3.0d0)
  call zscal(n, za, zx, 1)
  call begin_test('zscal_basic')
  call print_array('zx', zx_r, 2*n)
  call end_test()

  ! Test 2: n=0 (no-op)
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (3.0d0, 4.0d0)
  n = 2
  za = (5.0d0, 6.0d0)
  call zscal(0, za, zx, 1)
  call begin_test('zscal_n_zero')
  call print_array('zx', zx_r, 2*n)
  call end_test()

  ! Test 3: za=(0,0) — should zero out the vector
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (3.0d0, 4.0d0)
  zx(3) = (5.0d0, 6.0d0)
  n = 3
  za = (0.0d0, 0.0d0)
  call zscal(n, za, zx, 1)
  call begin_test('zscal_za_zero')
  call print_array('zx', zx_r, 2*n)
  call end_test()

  ! Test 4: non-unit stride, incx=2
  zx = (0.0d0, 0.0d0)
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (99.0d0, 99.0d0)
  zx(3) = (3.0d0, 4.0d0)
  zx(4) = (99.0d0, 99.0d0)
  zx(5) = (5.0d0, 6.0d0)
  n = 3
  za = (0.0d0, 1.0d0)
  call zscal(n, za, zx, 2)
  call begin_test('zscal_stride')
  call print_array('zx', zx_r, 10)
  call end_test()

  ! Test 5: za=(1,0) — identity, should be a no-op
  zx(1) = (7.0d0, 8.0d0)
  zx(2) = (9.0d0, 10.0d0)
  n = 2
  za = (1.0d0, 0.0d0)
  call zscal(n, za, zx, 1)
  call begin_test('zscal_za_one')
  call print_array('zx', zx_r, 2*n)
  call end_test()

end program
