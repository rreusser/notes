program test_zdrscl
  use test_utils
  implicit none

  integer, parameter :: MAXN = 20
  double precision :: zx_r(MAXN)
  complex*16 :: zx(MAXN/2)
  equivalence (zx, zx_r)
  integer :: n

  ! Test 1: basic scaling by 1/2 (sa=2)
  n = 3
  zx(1) = (2.0d0, 4.0d0)
  zx(2) = (6.0d0, 8.0d0)
  zx(3) = (10.0d0, 12.0d0)
  call zdrscl(n, 2.0d0, zx, 1)
  call begin_test('basic_scale_2')
  call print_int('n', n)
  call print_array('x', zx_r, 2*n)
  call end_test()

  ! Test 2: scaling by 1/1 (identity)
  n = 3
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (3.0d0, 4.0d0)
  zx(3) = (5.0d0, 6.0d0)
  call zdrscl(n, 1.0d0, zx, 1)
  call begin_test('identity_scale')
  call print_int('n', n)
  call print_array('x', zx_r, 2*n)
  call end_test()

  ! Test 3: scaling by 1/0.5 (multiply by 2)
  n = 2
  zx(1) = (1.0d0, 3.0d0)
  zx(2) = (5.0d0, 7.0d0)
  call zdrscl(n, 0.5d0, zx, 1)
  call begin_test('scale_half')
  call print_int('n', n)
  call print_array('x', zx_r, 2*n)
  call end_test()

  ! Test 4: N=0 (quick return)
  zx(1) = (99.0d0, 88.0d0)
  call zdrscl(0, 2.0d0, zx, 1)
  call begin_test('n_zero')
  call print_array('x', zx_r, 2)
  call end_test()

  ! Test 5: N=1
  n = 1
  zx(1) = (4.0d0, -6.0d0)
  call zdrscl(n, 2.0d0, zx, 1)
  call begin_test('n_one')
  call print_int('n', n)
  call print_array('x', zx_r, 2*n)
  call end_test()

  ! Test 6: large scalar (overflow protection path)
  n = 2
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (3.0d0, 4.0d0)
  call zdrscl(n, 1.0d+300, zx, 1)
  call begin_test('large_scalar')
  call print_int('n', n)
  call print_array('x', zx_r, 2*n)
  call end_test()

  ! Test 7: small scalar (underflow protection path)
  n = 2
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (3.0d0, 4.0d0)
  call zdrscl(n, 1.0d-300, zx, 1)
  call begin_test('small_scalar')
  call print_int('n', n)
  call print_array('x', zx_r, 2*n)
  call end_test()

  ! Test 8: non-unit stride (incx=2)
  n = 2
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (99.0d0, 99.0d0)
  zx(3) = (3.0d0, 4.0d0)
  zx(4) = (99.0d0, 99.0d0)
  call zdrscl(n, 4.0d0, zx, 2)
  call begin_test('stride_2')
  call print_int('n', n)
  call print_array('x', zx_r, 8)
  call end_test()

end program
