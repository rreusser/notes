program test_dlasq1
  use test_utils
  implicit none

  double precision :: d(20), e(20), work(100)
  integer :: info, n

  ! ============================================================
  ! Test 1: N=0 — quick return
  ! ============================================================
  n = 0
  d = 0.0d0
  e = 0.0d0
  work = 0.0d0
  call dlasq1(n, d, e, work, info)

  call begin_test('n0')
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 2: N=1
  ! ============================================================
  n = 1
  d = 0.0d0
  e = 0.0d0
  work = 0.0d0
  d(1) = 5.0d0
  call dlasq1(n, d, e, work, info)

  call begin_test('n1')
  call print_int('info', info)
  call print_array('d', d, 1)
  call end_test()

  ! ============================================================
  ! Test 3: N=1 negative diagonal
  ! ============================================================
  n = 1
  d = 0.0d0
  e = 0.0d0
  work = 0.0d0
  d(1) = -3.0d0
  call dlasq1(n, d, e, work, info)

  call begin_test('n1_neg')
  call print_int('info', info)
  call print_array('d', d, 1)
  call end_test()

  ! ============================================================
  ! Test 4: N=2
  ! ============================================================
  n = 2
  d = 0.0d0
  e = 0.0d0
  work = 0.0d0
  d(1) = 4.0d0
  d(2) = 3.0d0
  e(1) = 1.0d0
  call dlasq1(n, d, e, work, info)

  call begin_test('n2')
  call print_int('info', info)
  call print_array('d', d, 2)
  call end_test()

  ! ============================================================
  ! Test 5: N=2 with negative diagonal values
  ! ============================================================
  n = 2
  d = 0.0d0
  e = 0.0d0
  work = 0.0d0
  d(1) = -4.0d0
  d(2) = -3.0d0
  e(1) = 2.0d0
  call dlasq1(n, d, e, work, info)

  call begin_test('n2_neg')
  call print_int('info', info)
  call print_array('d', d, 2)
  call end_test()

  ! ============================================================
  ! Test 6: N=3 basic
  ! ============================================================
  n = 3
  d = 0.0d0
  e = 0.0d0
  work = 0.0d0
  d(1) = 4.0d0
  d(2) = 3.0d0
  d(3) = 2.0d0
  e(1) = 1.0d0
  e(2) = 0.5d0
  call dlasq1(n, d, e, work, info)

  call begin_test('n3_basic')
  call print_int('info', info)
  call print_array('d', d, 3)
  call end_test()

  ! ============================================================
  ! Test 7: N=4 basic
  ! ============================================================
  n = 4
  d = 0.0d0
  e = 0.0d0
  work = 0.0d0
  d(1) = 5.0d0
  d(2) = 4.0d0
  d(3) = 3.0d0
  d(4) = 2.0d0
  e(1) = 1.0d0
  e(2) = 0.5d0
  e(3) = 0.3d0
  call dlasq1(n, d, e, work, info)

  call begin_test('n4_basic')
  call print_int('info', info)
  call print_array('d', d, 4)
  call end_test()

  ! ============================================================
  ! Test 8: N=3 diagonal (e=0)
  ! ============================================================
  n = 3
  d = 0.0d0
  e = 0.0d0
  work = 0.0d0
  d(1) = 1.0d0
  d(2) = 5.0d0
  d(3) = 3.0d0
  call dlasq1(n, d, e, work, info)

  call begin_test('n3_diag')
  call print_int('info', info)
  call print_array('d', d, 3)
  call end_test()

  ! ============================================================
  ! Test 9: N=5 larger case
  ! ============================================================
  n = 5
  d = 0.0d0
  e = 0.0d0
  work = 0.0d0
  d(1) = 6.0d0
  d(2) = 5.0d0
  d(3) = 4.0d0
  d(4) = 3.0d0
  d(5) = 2.0d0
  e(1) = 1.0d0
  e(2) = 0.8d0
  e(3) = 0.5d0
  e(4) = 0.3d0
  call dlasq1(n, d, e, work, info)

  call begin_test('n5_basic')
  call print_int('info', info)
  call print_array('d', d, 5)
  call end_test()

  ! ============================================================
  ! Test 10: N=2 with zero e — diagonal case
  ! ============================================================
  n = 2
  d = 0.0d0
  e = 0.0d0
  work = 0.0d0
  d(1) = 3.0d0
  d(2) = 7.0d0
  call dlasq1(n, d, e, work, info)

  call begin_test('n2_diag')
  call print_int('info', info)
  call print_array('d', d, 2)
  call end_test()

  ! ============================================================
  ! Test 11: N=3 with negative diags and large e
  ! ============================================================
  n = 3
  d = 0.0d0
  e = 0.0d0
  work = 0.0d0
  d(1) = -3.0d0
  d(2) = -5.0d0
  d(3) = -2.0d0
  e(1) = 4.0d0
  e(2) = 3.0d0
  call dlasq1(n, d, e, work, info)

  call begin_test('n3_neg_large_e')
  call print_int('info', info)
  call print_array('d', d, 3)
  call end_test()

  ! ============================================================
  ! Test 12: N=4 identity-like (d=1, small e)
  ! ============================================================
  n = 4
  d = 0.0d0
  e = 0.0d0
  work = 0.0d0
  d(1) = 1.0d0
  d(2) = 1.0d0
  d(3) = 1.0d0
  d(4) = 1.0d0
  e(1) = 0.01d0
  e(2) = 0.01d0
  e(3) = 0.01d0
  call dlasq1(n, d, e, work, info)

  call begin_test('n4_identity_like')
  call print_int('info', info)
  call print_array('d', d, 4)
  call end_test()

end program
