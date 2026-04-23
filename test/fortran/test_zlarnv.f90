program test_zlarnv
  use test_utils
  implicit none

  integer :: iseed(4)
  complex*16 :: x(10)
  double precision :: xr(20)
  equivalence (x, xr)

  ! Test 1: IDIST=1, uniform real/imag in (0,1)
  iseed = (/ 1, 1, 1, 1 /)
  call ZLARNV(1, iseed, 5, x)

  call begin_test('idist1_uniform_01')
  call print_int_array('iseed_out', iseed, 4)
  call print_array('x', xr, 10)
  call end_test()

  ! Test 2: IDIST=2, uniform real/imag in (-1,1)
  iseed = (/ 1, 1, 1, 1 /)
  call ZLARNV(2, iseed, 5, x)

  call begin_test('idist2_uniform_m1_1')
  call print_int_array('iseed_out', iseed, 4)
  call print_array('x', xr, 10)
  call end_test()

  ! Test 3: IDIST=3, normal(0,1) on real and imag
  iseed = (/ 1, 1, 1, 1 /)
  call ZLARNV(3, iseed, 5, x)

  call begin_test('idist3_normal')
  call print_int_array('iseed_out', iseed, 4)
  call print_array('x', xr, 10)
  call end_test()

  ! Test 4: IDIST=4, uniform on unit disc
  iseed = (/ 1, 1, 1, 1 /)
  call ZLARNV(4, iseed, 5, x)

  call begin_test('idist4_disc')
  call print_int_array('iseed_out', iseed, 4)
  call print_array('x', xr, 10)
  call end_test()

  ! Test 5: IDIST=5, uniform on unit circle
  iseed = (/ 1, 1, 1, 1 /)
  call ZLARNV(5, iseed, 5, x)

  call begin_test('idist5_circle')
  call print_int_array('iseed_out', iseed, 4)
  call print_array('x', xr, 10)
  call end_test()

  ! Test 6: N=0 edge case
  iseed = (/ 1, 1, 1, 1 /)
  call ZLARNV(1, iseed, 0, x)

  call begin_test('n_zero')
  call print_int_array('iseed_out', iseed, 4)
  call end_test()

  ! Test 7: Larger N to exercise batching (N > LV/2 = 64)
  ! We'll test with N=10 and a different seed to get variety
  iseed = (/ 42, 7, 13, 99 /)
  call ZLARNV(1, iseed, 10, x)

  call begin_test('idist1_large_seed')
  call print_int_array('iseed_out', iseed, 4)
  call print_array('x', xr, 20)
  call end_test()

  ! Test 8: IDIST=4 with different seed
  iseed = (/ 42, 7, 13, 99 /)
  call ZLARNV(4, iseed, 5, x)

  call begin_test('idist4_alt_seed')
  call print_int_array('iseed_out', iseed, 4)
  call print_array('x', xr, 10)
  call end_test()

  ! Test 9: IDIST=5 with different seed
  iseed = (/ 42, 7, 13, 99 /)
  call ZLARNV(5, iseed, 5, x)

  call begin_test('idist5_alt_seed')
  call print_int_array('iseed_out', iseed, 4)
  call print_array('x', xr, 10)
  call end_test()

end program
