program test_dlaneg
  use test_utils
  implicit none

  integer :: dlaneg
  external :: dlaneg
  double precision :: d(200), lld(200), sigma, pivmin
  integer :: n, r, negcnt, i

  pivmin = 1.0d-30

  ! Test 1: N=1, r=1, sigma below d(1) => no negative pivots
  n = 1
  r = 1
  d(1) = 2.0d0
  sigma = 0.0d0
  negcnt = dlaneg(n, d, lld, sigma, pivmin, r)
  call begin_test('n1_sigma_below')
  call print_int('negcnt', negcnt)
  call end_test()

  ! Test 2: N=1, r=1, sigma above d(1) => 1 negative pivot
  n = 1
  r = 1
  d(1) = 2.0d0
  sigma = 5.0d0
  negcnt = dlaneg(n, d, lld, sigma, pivmin, r)
  call begin_test('n1_sigma_above')
  call print_int('negcnt', negcnt)
  call end_test()

  ! Test 3: N=5, small positive LDL^T, sigma=0
  n = 5
  d(1) = 4.0d0
  d(2) = 3.0d0
  d(3) = 2.0d0
  d(4) = 1.0d0
  d(5) = 5.0d0
  lld(1) = 0.5d0
  lld(2) = 0.5d0
  lld(3) = 0.5d0
  lld(4) = 0.5d0
  sigma = 0.0d0
  r = 3
  negcnt = dlaneg(n, d, lld, sigma, pivmin, r)
  call begin_test('n5_sigma_zero_r3')
  call print_int('negcnt', negcnt)
  call end_test()

  ! Test 4: same as 3 but r=1 (all downward sweep not used), r=N
  r = 5
  negcnt = dlaneg(n, d, lld, sigma, pivmin, r)
  call begin_test('n5_sigma_zero_r5')
  call print_int('negcnt', negcnt)
  call end_test()

  ! Test 5: r=1 (all upward sweep from N)
  r = 1
  negcnt = dlaneg(n, d, lld, sigma, pivmin, r)
  call begin_test('n5_sigma_zero_r1')
  call print_int('negcnt', negcnt)
  call end_test()

  ! Test 6: sigma large enough to flip signs
  sigma = 10.0d0
  r = 3
  negcnt = dlaneg(n, d, lld, sigma, pivmin, r)
  call begin_test('n5_sigma_large_r3')
  call print_int('negcnt', negcnt)
  call end_test()

  ! Test 7: mixed sign diag
  n = 4
  d(1) = -1.0d0
  d(2) = 2.0d0
  d(3) = -3.0d0
  d(4) = 4.0d0
  lld(1) = 0.25d0
  lld(2) = 0.25d0
  lld(3) = 0.25d0
  sigma = 0.0d0
  r = 2
  negcnt = dlaneg(n, d, lld, sigma, pivmin, r)
  call begin_test('n4_mixed_r2')
  call print_int('negcnt', negcnt)
  call end_test()

  ! Test 8: negative sigma
  sigma = -1.0d0
  negcnt = dlaneg(n, d, lld, sigma, pivmin, r)
  call begin_test('n4_mixed_neg_sigma')
  call print_int('negcnt', negcnt)
  call end_test()

  ! Test 9: Larger N=150 (exceeds BLKLEN=128)
  n = 150
  do i = 1, n
    d(i) = dble(i)
  end do
  do i = 1, n-1
    lld(i) = 0.1d0
  end do
  sigma = 0.0d0
  r = 75
  negcnt = dlaneg(n, d, lld, sigma, pivmin, r)
  call begin_test('n150_sigma_zero')
  call print_int('negcnt', negcnt)
  call end_test()

  ! Test 10: Larger N=150, sigma shifts
  sigma = 50.5d0
  negcnt = dlaneg(n, d, lld, sigma, pivmin, r)
  call begin_test('n150_sigma_50')
  call print_int('negcnt', negcnt)
  call end_test()

  ! Test 11: r=N for larger N
  r = 150
  sigma = 0.0d0
  negcnt = dlaneg(n, d, lld, sigma, pivmin, r)
  call begin_test('n150_r_n')
  call print_int('negcnt', negcnt)
  call end_test()

  ! Test 12: r=1 for larger N
  r = 1
  negcnt = dlaneg(n, d, lld, sigma, pivmin, r)
  call begin_test('n150_r_1')
  call print_int('negcnt', negcnt)
  call end_test()

  ! Test 13: N=2
  n = 2
  d(1) = 1.0d0
  d(2) = 4.0d0
  lld(1) = 1.0d0
  sigma = 0.5d0
  r = 1
  negcnt = dlaneg(n, d, lld, sigma, pivmin, r)
  call begin_test('n2_r1')
  call print_int('negcnt', negcnt)
  call end_test()

  r = 2
  negcnt = dlaneg(n, d, lld, sigma, pivmin, r)
  call begin_test('n2_r2')
  call print_int('negcnt', negcnt)
  call end_test()

end program
