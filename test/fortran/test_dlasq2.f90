program test_dlasq2
  use test_utils
  implicit none

  double precision :: z(100)
  integer :: info, n, k

  ! ============================================================
  ! Test 1: N=0 — quick return
  ! ============================================================
  n = 0
  z = 0.0d0
  call dlasq2(n, z, info)

  call begin_test('n0')
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 2: N=1
  ! ============================================================
  n = 1
  z = 0.0d0
  z(1) = 4.0d0
  call dlasq2(n, z, info)

  call begin_test('n1')
  call print_int('info', info)
  call print_array('z', z, 4)
  call end_test()

  ! ============================================================
  ! Test 3: N=2 — 2-by-2 case
  ! ============================================================
  n = 2
  z = 0.0d0
  z(1) = 4.0d0
  z(2) = 1.0d0
  z(3) = 3.0d0
  call dlasq2(n, z, info)

  call begin_test('n2')
  call print_int('info', info)
  call print_array('z', z, 8)
  call end_test()

  ! ============================================================
  ! Test 4: N=2 where z(3) > z(1) — swap case
  ! ============================================================
  n = 2
  z = 0.0d0
  z(1) = 2.0d0
  z(2) = 1.0d0
  z(3) = 5.0d0
  call dlasq2(n, z, info)

  call begin_test('n2_swap')
  call print_int('info', info)
  call print_array('z', z, 8)
  call end_test()

  ! ============================================================
  ! Test 5: N=3 — basic case
  ! ============================================================
  n = 3
  z = 0.0d0
  z(1) = 4.0d0
  z(2) = 1.0d0
  z(3) = 3.0d0
  z(4) = 0.5d0
  z(5) = 2.0d0
  call dlasq2(n, z, info)

  call begin_test('n3_basic')
  call print_int('info', info)
  call print_array('z', z, 12)
  call end_test()

  ! ============================================================
  ! Test 6: N=4 — larger case
  ! ============================================================
  n = 4
  z = 0.0d0
  z(1) = 4.0d0
  z(2) = 1.0d0
  z(3) = 3.0d0
  z(4) = 0.5d0
  z(5) = 5.0d0
  z(6) = 0.3d0
  z(7) = 2.0d0
  call dlasq2(n, z, info)

  call begin_test('n4_basic')
  call print_int('info', info)
  call print_array('z', z, 16)
  call end_test()

  ! ============================================================
  ! Test 7: N=3 diagonal case (all e = 0)
  ! ============================================================
  n = 3
  z = 0.0d0
  z(1) = 4.0d0
  z(2) = 0.0d0
  z(3) = 3.0d0
  z(4) = 0.0d0
  z(5) = 2.0d0
  call dlasq2(n, z, info)

  call begin_test('n3_diagonal')
  call print_int('info', info)
  call print_array('z', z, 10)
  call end_test()

  ! ============================================================
  ! Test 10: N=5 — larger case
  ! ============================================================
  n = 5
  z = 0.0d0
  z(1) = 9.0d0
  z(2) = 1.0d0
  z(3) = 8.0d0
  z(4) = 0.5d0
  z(5) = 7.0d0
  z(6) = 0.3d0
  z(7) = 6.0d0
  z(8) = 0.2d0
  z(9) = 5.0d0
  call dlasq2(n, z, info)

  call begin_test('n5_basic')
  call print_int('info', info)
  call print_array('z', z, 20)
  call end_test()

  ! ============================================================
  ! Test 11: N=3 with Z values that trigger reversal
  ! (small first, large last)
  ! ============================================================
  n = 3
  z = 0.0d0
  z(1) = 1.0d0
  z(2) = 0.5d0
  z(3) = 2.0d0
  z(4) = 0.3d0
  z(5) = 10.0d0
  call dlasq2(n, z, info)

  call begin_test('n3_reversal')
  call print_int('info', info)
  call print_array('z', z, 12)
  call end_test()

  ! ============================================================
  ! Test 12: N=4 with identity-like bidiagonal
  ! ============================================================
  n = 4
  z = 0.0d0
  z(1) = 1.0d0
  z(2) = 0.1d0
  z(3) = 1.0d0
  z(4) = 0.1d0
  z(5) = 1.0d0
  z(6) = 0.1d0
  z(7) = 1.0d0
  call dlasq2(n, z, info)

  call begin_test('n4_identity_like')
  call print_int('info', info)
  call print_array('z', z, 16)
  call end_test()

  ! ============================================================
  ! Test 13: N=2 with z(3)=0, z(2)=0 — edge case
  ! ============================================================
  n = 2
  z = 0.0d0
  z(1) = 4.0d0
  z(2) = 0.0d0
  z(3) = 3.0d0
  call dlasq2(n, z, info)

  call begin_test('n2_zero_offdiag')
  call print_int('info', info)
  call print_array('z', z, 8)
  call end_test()

end program
