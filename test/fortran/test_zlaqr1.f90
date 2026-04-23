program test_zlaqr1
  use test_utils
  implicit none

  integer, parameter :: NMAX = 3
  complex*16 :: H(NMAX, NMAX), V(NMAX)
  complex*16 :: s1, s2
  double precision :: H_r(2*NMAX*NMAX), V_r(2*NMAX)
  equivalence (H, H_r)
  equivalence (V, V_r)
  integer :: n, ldh

  ldh = NMAX

  ! ==================================================================
  ! Test 1: 2x2 with real shifts
  ! ==================================================================
  n = 2
  H = (0.0d0, 0.0d0)
  V = (0.0d0, 0.0d0)

  H(1,1) = (4.0d0, 0.0d0)
  H(1,2) = (2.0d0, 0.0d0)
  H(2,1) = (1.0d0, 0.0d0)
  H(2,2) = (3.0d0, 0.0d0)

  s1 = (5.0d0, 0.0d0)
  s2 = (2.0d0, 0.0d0)

  call ZLAQR1(n, H, ldh, s1, s2, V)

  call begin_test('2x2_real_shifts')
  call print_array('v', V_r, 2*n)
  call end_test()

  ! ==================================================================
  ! Test 2: 2x2 with complex shifts
  ! ==================================================================
  n = 2
  H = (0.0d0, 0.0d0)
  V = (0.0d0, 0.0d0)

  H(1,1) = (4.0d0, 1.0d0)
  H(1,2) = (2.0d0, -1.0d0)
  H(2,1) = (1.0d0, 0.5d0)
  H(2,2) = (3.0d0, -0.5d0)

  s1 = (3.0d0, 2.0d0)
  s2 = (1.0d0, -1.0d0)

  call ZLAQR1(n, H, ldh, s1, s2, V)

  call begin_test('2x2_complex_shifts')
  call print_array('v', V_r, 2*n)
  call end_test()

  ! ==================================================================
  ! Test 3: 3x3 with real shifts
  ! ==================================================================
  n = 3
  H = (0.0d0, 0.0d0)
  V = (0.0d0, 0.0d0)

  H(1,1) = (5.0d0, 0.0d0)
  H(1,2) = (2.0d0, 0.0d0)
  H(1,3) = (1.0d0, 0.0d0)
  H(2,1) = (1.0d0, 0.0d0)
  H(2,2) = (4.0d0, 0.0d0)
  H(2,3) = (3.0d0, 0.0d0)
  H(3,1) = (0.0d0, 0.0d0)
  H(3,2) = (2.0d0, 0.0d0)
  H(3,3) = (3.0d0, 0.0d0)

  s1 = (6.0d0, 0.0d0)
  s2 = (2.0d0, 0.0d0)

  call ZLAQR1(n, H, ldh, s1, s2, V)

  call begin_test('3x3_real_shifts')
  call print_array('v', V_r, 2*n)
  call end_test()

  ! ==================================================================
  ! Test 4: 3x3 with complex shifts
  ! ==================================================================
  n = 3
  H = (0.0d0, 0.0d0)
  V = (0.0d0, 0.0d0)

  H(1,1) = (3.0d0, 1.0d0)
  H(1,2) = (1.0d0, 0.5d0)
  H(1,3) = (0.5d0, -0.5d0)
  H(2,1) = (2.0d0, -1.0d0)
  H(2,2) = (4.0d0, 0.0d0)
  H(2,3) = (1.0d0, 1.0d0)
  H(3,1) = (0.0d0, 0.0d0)
  H(3,2) = (1.5d0, 0.5d0)
  H(3,3) = (2.0d0, -1.0d0)

  s1 = (2.0d0, 3.0d0)
  s2 = (1.0d0, -2.0d0)

  call ZLAQR1(n, H, ldh, s1, s2, V)

  call begin_test('3x3_complex_shifts')
  call print_array('v', V_r, 2*n)
  call end_test()

  ! ==================================================================
  ! Test 5: 3x3 with conjugate pair shifts (common in QR algorithm)
  ! ==================================================================
  n = 3
  H = (0.0d0, 0.0d0)
  V = (0.0d0, 0.0d0)

  H(1,1) = (6.0d0, 0.5d0)
  H(1,2) = (3.0d0, -1.0d0)
  H(1,3) = (1.0d0, 0.0d0)
  H(2,1) = (1.0d0, 0.0d0)
  H(2,2) = (5.0d0, -0.5d0)
  H(2,3) = (2.0d0, 1.0d0)
  H(3,1) = (0.0d0, 0.0d0)
  H(3,2) = (0.5d0, 0.25d0)
  H(3,3) = (4.0d0, 0.0d0)

  s1 = (3.0d0, 1.5d0)
  s2 = (3.0d0, -1.5d0)

  call ZLAQR1(n, H, ldh, s1, s2, V)

  call begin_test('3x3_conjugate_shifts')
  call print_array('v', V_r, 2*n)
  call end_test()

  ! ==================================================================
  ! Test 6: 2x2 with identical shifts
  ! ==================================================================
  n = 2
  H = (0.0d0, 0.0d0)
  V = (0.0d0, 0.0d0)

  H(1,1) = (3.0d0, 1.0d0)
  H(1,2) = (1.0d0, 0.0d0)
  H(2,1) = (0.5d0, -0.5d0)
  H(2,2) = (2.0d0, -1.0d0)

  s1 = (3.0d0, 1.0d0)
  s2 = (3.0d0, 1.0d0)

  call ZLAQR1(n, H, ldh, s1, s2, V)

  call begin_test('2x2_identical_shifts')
  call print_array('v', V_r, 2*n)
  call end_test()

end program
