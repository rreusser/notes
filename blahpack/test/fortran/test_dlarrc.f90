program test_dlarrc
  use test_utils
  implicit none
  double precision :: d(10), e(10), pivmin
  integer :: eigcnt, lcnt, rcnt, info, n

  ! Test 1: N=0, quick return
  n = 0
  pivmin = 1.0d-16
  call dlarrc('T', n, -1.0d0, 1.0d0, d, e, pivmin, eigcnt, lcnt, rcnt, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call print_int('eigcnt', eigcnt)
  call print_int('lcnt', lcnt)
  call print_int('rcnt', rcnt)
  call end_test()

  ! Test 2: N=1, JOBT='T', eigenvalue inside interval
  ! T = [2.0], interval (0, 3]
  ! Sturm: pivot_L = 2 - 0 = 2 > 0 => lcnt=0
  !         pivot_R = 2 - 3 = -1 <= 0 => rcnt=1
  ! eigcnt = 1 - 0 = 1
  n = 1
  d(1) = 2.0d0
  call dlarrc('T', n, 0.0d0, 3.0d0, d, e, pivmin, eigcnt, lcnt, rcnt, info)
  call begin_test('n1_t_inside')
  call print_int('info', info)
  call print_int('eigcnt', eigcnt)
  call print_int('lcnt', lcnt)
  call print_int('rcnt', rcnt)
  call end_test()

  ! Test 3: N=1, JOBT='T', eigenvalue outside interval
  ! T = [2.0], interval (3, 5]
  n = 1
  d(1) = 2.0d0
  call dlarrc('T', n, 3.0d0, 5.0d0, d, e, pivmin, eigcnt, lcnt, rcnt, info)
  call begin_test('n1_t_outside')
  call print_int('info', info)
  call print_int('eigcnt', eigcnt)
  call print_int('lcnt', lcnt)
  call print_int('rcnt', rcnt)
  call end_test()

  ! Test 4: N=5, JOBT='T', tridiagonal Sturm sequence
  ! d = [4, 3, 2, 1, 5], e = [1, 1, 1, 1]
  ! Interval (-10, 10] should capture all 5 eigenvalues
  n = 5
  d(1) = 4.0d0
  d(2) = 3.0d0
  d(3) = 2.0d0
  d(4) = 1.0d0
  d(5) = 5.0d0
  e(1) = 1.0d0
  e(2) = 1.0d0
  e(3) = 1.0d0
  e(4) = 1.0d0
  call dlarrc('T', n, -10.0d0, 10.0d0, d, e, pivmin, eigcnt, lcnt, rcnt, info)
  call begin_test('n5_t_all')
  call print_int('info', info)
  call print_int('eigcnt', eigcnt)
  call print_int('lcnt', lcnt)
  call print_int('rcnt', rcnt)
  call end_test()

  ! Test 5: N=5, JOBT='T', narrow interval
  ! Same matrix, interval (1, 4]
  call dlarrc('T', n, 1.0d0, 4.0d0, d, e, pivmin, eigcnt, lcnt, rcnt, info)
  call begin_test('n5_t_narrow')
  call print_int('info', info)
  call print_int('eigcnt', eigcnt)
  call print_int('lcnt', lcnt)
  call print_int('rcnt', rcnt)
  call end_test()

  ! Test 6: N=1, JOBT='L', eigenvalue inside
  ! LDL^T with D=[2], interval (0, 3]
  n = 1
  d(1) = 2.0d0
  call dlarrc('L', n, 0.0d0, 3.0d0, d, e, pivmin, eigcnt, lcnt, rcnt, info)
  call begin_test('n1_l_inside')
  call print_int('info', info)
  call print_int('eigcnt', eigcnt)
  call print_int('lcnt', lcnt)
  call print_int('rcnt', rcnt)
  call end_test()

  ! Test 7: N=5, JOBT='L', LDL^T Sturm sequence
  ! d = [4, 3, 2, 1, 5], e = [0.5, 0.5, 0.5, 0.5]
  ! Wide interval (-10, 10]
  n = 5
  d(1) = 4.0d0
  d(2) = 3.0d0
  d(3) = 2.0d0
  d(4) = 1.0d0
  d(5) = 5.0d0
  e(1) = 0.5d0
  e(2) = 0.5d0
  e(3) = 0.5d0
  e(4) = 0.5d0
  call dlarrc('L', n, -10.0d0, 10.0d0, d, e, pivmin, eigcnt, lcnt, rcnt, info)
  call begin_test('n5_l_all')
  call print_int('info', info)
  call print_int('eigcnt', eigcnt)
  call print_int('lcnt', lcnt)
  call print_int('rcnt', rcnt)
  call end_test()

  ! Test 8: N=5, JOBT='L', narrow interval
  call dlarrc('L', n, 1.0d0, 4.0d0, d, e, pivmin, eigcnt, lcnt, rcnt, info)
  call begin_test('n5_l_narrow')
  call print_int('info', info)
  call print_int('eigcnt', eigcnt)
  call print_int('lcnt', lcnt)
  call print_int('rcnt', rcnt)
  call end_test()

  ! Test 9: N=3, JOBT='T', eigenvalue at boundary
  ! d = [1, 2, 3], e = [0, 0], diagonal matrix with eigenvalues 1,2,3
  ! interval (1, 3] should have 2 eigenvalues (2 and 3)
  n = 3
  d(1) = 1.0d0
  d(2) = 2.0d0
  d(3) = 3.0d0
  e(1) = 0.0d0
  e(2) = 0.0d0
  call dlarrc('T', n, 1.0d0, 3.0d0, d, e, pivmin, eigcnt, lcnt, rcnt, info)
  call begin_test('n3_t_boundary')
  call print_int('info', info)
  call print_int('eigcnt', eigcnt)
  call print_int('lcnt', lcnt)
  call print_int('rcnt', rcnt)
  call end_test()

  ! Test 10: N=4, JOBT='T', all negative eigenvalues
  ! d = [-5, -3, -7, -1], e = [0.5, 0.5, 0.5]
  ! interval (-8, -2]
  n = 4
  d(1) = -5.0d0
  d(2) = -3.0d0
  d(3) = -7.0d0
  d(4) = -1.0d0
  e(1) = 0.5d0
  e(2) = 0.5d0
  e(3) = 0.5d0
  call dlarrc('T', n, -8.0d0, -2.0d0, d, e, pivmin, eigcnt, lcnt, rcnt, info)
  call begin_test('n4_t_negative')
  call print_int('info', info)
  call print_int('eigcnt', eigcnt)
  call print_int('lcnt', lcnt)
  call print_int('rcnt', rcnt)
  call end_test()

  ! Test 11: N=4, JOBT='L', all negative eigenvalues
  n = 4
  d(1) = -5.0d0
  d(2) = -3.0d0
  d(3) = -7.0d0
  d(4) = -1.0d0
  e(1) = 0.5d0
  e(2) = 0.5d0
  e(3) = 0.5d0
  call dlarrc('L', n, -8.0d0, -2.0d0, d, e, pivmin, eigcnt, lcnt, rcnt, info)
  call begin_test('n4_l_negative')
  call print_int('info', info)
  call print_int('eigcnt', eigcnt)
  call print_int('lcnt', lcnt)
  call print_int('rcnt', rcnt)
  call end_test()

  ! Test 12: N=2, JOBT='T'
  ! d = [1, 4], e = [1]
  ! Eigenvalues are (5 +/- sqrt(5))/2 ~ 0.382 and 4.618
  ! Interval (0, 5]
  n = 2
  d(1) = 1.0d0
  d(2) = 4.0d0
  e(1) = 1.0d0
  call dlarrc('T', n, 0.0d0, 5.0d0, d, e, pivmin, eigcnt, lcnt, rcnt, info)
  call begin_test('n2_t_both')
  call print_int('info', info)
  call print_int('eigcnt', eigcnt)
  call print_int('lcnt', lcnt)
  call print_int('rcnt', rcnt)
  call end_test()

  ! Test 13: N=2, JOBT='L'
  n = 2
  d(1) = 1.0d0
  d(2) = 4.0d0
  e(1) = 1.0d0
  call dlarrc('L', n, 0.0d0, 5.0d0, d, e, pivmin, eigcnt, lcnt, rcnt, info)
  call begin_test('n2_l_both')
  call print_int('info', info)
  call print_int('eigcnt', eigcnt)
  call print_int('lcnt', lcnt)
  call print_int('rcnt', rcnt)
  call end_test()

end program
