program test_zrscl
  use test_utils
  implicit none

  integer, parameter :: MAXN = 20
  double precision :: zx_r(MAXN)
  complex*16 :: zx(MAXN/2)
  equivalence (zx, zx_r)
  complex*16 :: a
  integer :: n

  ! Test 1: purely real scalar a = (2, 0) => delegates to ZDRSCL
  n = 3
  zx(1) = (2.0d0, 4.0d0)
  zx(2) = (6.0d0, 8.0d0)
  zx(3) = (10.0d0, 12.0d0)
  a = (2.0d0, 0.0d0)
  call zrscl(n, a, zx, 1)
  call begin_test('real_scalar')
  call print_array('x', zx_r, 2*n)
  call end_test()

  ! Test 2: purely imaginary scalar a = (0, 2) => AR==0 normal branch
  n = 3
  zx(1) = (2.0d0, 4.0d0)
  zx(2) = (6.0d0, 8.0d0)
  zx(3) = (10.0d0, 12.0d0)
  a = (0.0d0, 2.0d0)
  call zrscl(n, a, zx, 1)
  call begin_test('imag_scalar')
  call print_array('x', zx_r, 2*n)
  call end_test()

  ! Test 3: general complex scalar a = (3, 4) => general branch, normal path
  n = 3
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (3.0d0, 4.0d0)
  zx(3) = (5.0d0, 6.0d0)
  a = (3.0d0, 4.0d0)
  call zrscl(n, a, zx, 1)
  call begin_test('general_complex')
  call print_array('x', zx_r, 2*n)
  call end_test()

  ! Test 4: N=0 quick return
  zx(1) = (99.0d0, 88.0d0)
  a = (1.0d0, 1.0d0)
  call zrscl(0, a, zx, 1)
  call begin_test('n_zero')
  call print_array('x', zx_r, 2)
  call end_test()

  ! Test 5: N=1
  n = 1
  zx(1) = (4.0d0, -6.0d0)
  a = (2.0d0, 1.0d0)
  call zrscl(n, a, zx, 1)
  call begin_test('n_one')
  call print_array('x', zx_r, 2*n)
  call end_test()

  ! Test 6: non-unit stride (incx=2)
  n = 2
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (99.0d0, 99.0d0)
  zx(3) = (3.0d0, 4.0d0)
  zx(4) = (99.0d0, 99.0d0)
  a = (2.0d0, 3.0d0)
  call zrscl(n, a, zx, 2)
  call begin_test('stride_2')
  call print_array('x', zx_r, 8)
  call end_test()

  ! Test 7: purely imaginary, moderate |AI| (normal path, was mislabeled)
  n = 2
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (3.0d0, 4.0d0)
  a = (0.0d0, 1.0d+300)
  call zrscl(n, a, zx, 1)
  call begin_test('imag_moderate')
  call print_array('x', zx_r, 2*n)
  call end_test()

  ! Test 7b: purely imaginary, truly large |AI| > SAFMAX (~4.5e307)
  n = 2
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (3.0d0, 4.0d0)
  a = (0.0d0, 1.0d+308)
  call zrscl(n, a, zx, 1)
  call begin_test('imag_very_large')
  call print_array('x', zx_r, 2*n)
  call end_test()

  ! Test 8: purely imaginary, small |AI| < SAFMIN
  n = 2
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (3.0d0, 4.0d0)
  a = (0.0d0, 1.0d-310)
  call zrscl(n, a, zx, 1)
  call begin_test('imag_small')
  call print_array('x', zx_r, 2*n)
  call end_test()

  ! Test 9: general complex with very small components => UR/UI < SAFMIN
  n = 2
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (3.0d0, 4.0d0)
  a = (1.0d-310, 1.0d-310)
  call zrscl(n, a, zx, 1)
  call begin_test('general_small')
  call print_array('x', zx_r, 2*n)
  call end_test()

  ! Test 10: general complex with large components but UR/UI < SAFMAX
  ! (1e300 < SAFMAX ~ 4.5e307, so this hits the normal path)
  n = 2
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (3.0d0, 4.0d0)
  a = (1.0d+300, 1.0d+300)
  call zrscl(n, a, zx, 1)
  call begin_test('general_large')
  call print_array('x', zx_r, 2*n)
  call end_test()

  ! Test 10b: general complex with UR/UI > SAFMAX but UR/UI <= OV
  ! a=(3e307, 3e307) => UR=UI=6e307 > SAFMAX, <= OV => line 124 branch
  n = 2
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (3.0d0, 4.0d0)
  a = (3.0d+307, 3.0d+307)
  call zrscl(n, a, zx, 1)
  call begin_test('general_safmax_not_ov')
  call print_array('x', zx_r, 2*n)
  call end_test()

  ! Test 11: identity (a = (1, 0))
  n = 3
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (3.0d0, 4.0d0)
  zx(3) = (5.0d0, 6.0d0)
  a = (1.0d0, 0.0d0)
  call zrscl(n, a, zx, 1)
  call begin_test('identity')
  call print_array('x', zx_r, 2*n)
  call end_test()

  ! Test 12: a = (0, 1) => dividing by i should give x * (-i)
  n = 2
  zx(1) = (1.0d0, 0.0d0)
  zx(2) = (0.0d0, 1.0d0)
  a = (0.0d0, 1.0d0)
  call zrscl(n, a, zx, 1)
  call begin_test('div_by_i')
  call print_array('x', zx_r, 2*n)
  call end_test()

  ! Test 13: a = (0, -1) => dividing by -i should give x * i
  n = 2
  zx(1) = (1.0d0, 0.0d0)
  zx(2) = (0.0d0, 1.0d0)
  a = (0.0d0, -1.0d0)
  call zrscl(n, a, zx, 1)
  call begin_test('div_by_neg_i')
  call print_array('x', zx_r, 2*n)
  call end_test()

  ! Test 14: negative real scalar a = (-3, 0)
  n = 2
  zx(1) = (6.0d0, 9.0d0)
  zx(2) = (-3.0d0, 12.0d0)
  a = (-3.0d0, 0.0d0)
  call zrscl(n, a, zx, 1)
  call begin_test('neg_real_scalar')
  call print_array('x', zx_r, 2*n)
  call end_test()

  ! Test 15: general complex, UR/UI overflow (AR small, AI large => AI*(AI/AR) overflows)
  ! This triggers: UR/UI > SAFMAX, absr <= OV, after zdscal(SAFMIN), |UR|>OV, absr < absi
  n = 2
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (3.0d0, 4.0d0)
  a = (1.0d0, 1.0d+200)
  call zrscl(n, a, zx, 1)
  call begin_test('general_overflow_absr_lt_absi')
  call print_array('x', zx_r, 2*n)
  call end_test()

  ! Test 16: general complex, UR/UI overflow, absr >= absi
  ! AR large, AI moderate => AR*(AR/AI) overflows
  n = 2
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (3.0d0, 4.0d0)
  a = (1.0d+200, 1.0d0)
  call zrscl(n, a, zx, 1)
  call begin_test('general_overflow_absr_ge_absi')
  call print_array('x', zx_r, 2*n)
  call end_test()

end program
