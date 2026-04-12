program test_dlaic1
  use test_utils
  implicit none

  integer :: j
  double precision :: sest, gamma_val, sestpr, s, c_val
  double precision :: x(10), w(10)

  ! =============================================
  ! JOB=1 tests (largest singular value estimate)
  ! =============================================

  ! Test 1: JOB=1, J=3, basic case
  j = 3
  x(1) = 0.6d0; x(2) = 0.8d0; x(3) = 0.0d0
  w(1) = 1.0d0; w(2) = 2.0d0; w(3) = 3.0d0
  sest = 5.0d0
  gamma_val = 2.0d0
  call dlaic1( 1, j, x, sest, w, gamma_val, sestpr, s, c_val )
  call begin_test('job1_j3_basic')
  call print_scalar('sestpr', sestpr)
  call print_scalar('s', s)
  call print_scalar('c', c_val)
  call end_test()

  ! Test 2: JOB=1, J=5
  j = 5
  x(1) = 0.2d0; x(2) = 0.4d0; x(3) = 0.5d0; x(4) = 0.3d0; x(5) = 0.1d0
  w(1) = 1.5d0; w(2) = 0.5d0; w(3) = 2.0d0; w(4) = 1.0d0; w(5) = 0.8d0
  sest = 3.0d0
  gamma_val = 1.5d0
  call dlaic1( 1, j, x, sest, w, gamma_val, sestpr, s, c_val )
  call begin_test('job1_j5')
  call print_scalar('sestpr', sestpr)
  call print_scalar('s', s)
  call print_scalar('c', c_val)
  call end_test()

  ! Test 3: JOB=1, J=1
  j = 1
  x(1) = 1.0d0
  w(1) = 0.5d0
  sest = 2.0d0
  gamma_val = 0.3d0
  call dlaic1( 1, j, x, sest, w, gamma_val, sestpr, s, c_val )
  call begin_test('job1_j1')
  call print_scalar('sestpr', sestpr)
  call print_scalar('s', s)
  call print_scalar('c', c_val)
  call end_test()

  ! Test 4: JOB=1, SEST=0 (edge case)
  j = 3
  x(1) = 0.6d0; x(2) = 0.8d0; x(3) = 0.0d0
  w(1) = 1.0d0; w(2) = 2.0d0; w(3) = 3.0d0
  sest = 0.0d0
  gamma_val = 2.0d0
  call dlaic1( 1, j, x, sest, w, gamma_val, sestpr, s, c_val )
  call begin_test('job1_sest0')
  call print_scalar('sestpr', sestpr)
  call print_scalar('s', s)
  call print_scalar('c', c_val)
  call end_test()

  ! Test 5: JOB=1, SEST=0, alpha=0, gamma=0
  j = 1
  x(1) = 0.0d0
  w(1) = 0.0d0
  sest = 0.0d0
  gamma_val = 0.0d0
  call dlaic1( 1, j, x, sest, w, gamma_val, sestpr, s, c_val )
  call begin_test('job1_sest0_zero')
  call print_scalar('sestpr', sestpr)
  call print_scalar('s', s)
  call print_scalar('c', c_val)
  call end_test()

  ! Test 6: JOB=1, gamma near zero (absgam <= eps*absest)
  j = 2
  x(1) = 1.0d0; x(2) = 0.0d0
  w(1) = 0.5d0; w(2) = 0.3d0
  sest = 10.0d0
  gamma_val = 1.0d-20
  call dlaic1( 1, j, x, sest, w, gamma_val, sestpr, s, c_val )
  call begin_test('job1_gamma_tiny')
  call print_scalar('sestpr', sestpr)
  call print_scalar('s', s)
  call print_scalar('c', c_val)
  call end_test()

  ! Test 7: JOB=1, alpha near zero (absalp <= eps*absest)
  j = 2
  x(1) = 1.0d-20; x(2) = 0.0d0
  w(1) = 1.0d0; w(2) = 0.0d0
  sest = 10.0d0
  gamma_val = 5.0d0
  call dlaic1( 1, j, x, sest, w, gamma_val, sestpr, s, c_val )
  call begin_test('job1_alpha_tiny')
  call print_scalar('sestpr', sestpr)
  call print_scalar('s', s)
  call print_scalar('c', c_val)
  call end_test()

  ! Test 8: JOB=1, sest tiny (absest <= eps*absalp)
  j = 1
  x(1) = 1.0d0
  w(1) = 1.0d10
  sest = 1.0d-20
  gamma_val = 5.0d0
  call dlaic1( 1, j, x, sest, w, gamma_val, sestpr, s, c_val )
  call begin_test('job1_sest_tiny')
  call print_scalar('sestpr', sestpr)
  call print_scalar('s', s)
  call print_scalar('c', c_val)
  call end_test()

  ! =============================================
  ! JOB=2 tests (smallest singular value estimate)
  ! =============================================

  ! Test 9: JOB=2, J=3, basic case
  j = 3
  x(1) = 0.6d0; x(2) = 0.8d0; x(3) = 0.0d0
  w(1) = 1.0d0; w(2) = 2.0d0; w(3) = 3.0d0
  sest = 5.0d0
  gamma_val = 2.0d0
  call dlaic1( 2, j, x, sest, w, gamma_val, sestpr, s, c_val )
  call begin_test('job2_j3_basic')
  call print_scalar('sestpr', sestpr)
  call print_scalar('s', s)
  call print_scalar('c', c_val)
  call end_test()

  ! Test 10: JOB=2, J=5
  j = 5
  x(1) = 0.2d0; x(2) = 0.4d0; x(3) = 0.5d0; x(4) = 0.3d0; x(5) = 0.1d0
  w(1) = 1.5d0; w(2) = 0.5d0; w(3) = 2.0d0; w(4) = 1.0d0; w(5) = 0.8d0
  sest = 3.0d0
  gamma_val = 1.5d0
  call dlaic1( 2, j, x, sest, w, gamma_val, sestpr, s, c_val )
  call begin_test('job2_j5')
  call print_scalar('sestpr', sestpr)
  call print_scalar('s', s)
  call print_scalar('c', c_val)
  call end_test()

  ! Test 11: JOB=2, J=1
  j = 1
  x(1) = 1.0d0
  w(1) = 0.5d0
  sest = 2.0d0
  gamma_val = 0.3d0
  call dlaic1( 2, j, x, sest, w, gamma_val, sestpr, s, c_val )
  call begin_test('job2_j1')
  call print_scalar('sestpr', sestpr)
  call print_scalar('s', s)
  call print_scalar('c', c_val)
  call end_test()

  ! Test 12: JOB=2, SEST=0
  j = 3
  x(1) = 0.6d0; x(2) = 0.8d0; x(3) = 0.0d0
  w(1) = 1.0d0; w(2) = 2.0d0; w(3) = 3.0d0
  sest = 0.0d0
  gamma_val = 2.0d0
  call dlaic1( 2, j, x, sest, w, gamma_val, sestpr, s, c_val )
  call begin_test('job2_sest0')
  call print_scalar('sestpr', sestpr)
  call print_scalar('s', s)
  call print_scalar('c', c_val)
  call end_test()

  ! Test 13: JOB=2, SEST=0, alpha=0, gamma=0
  j = 1
  x(1) = 0.0d0
  w(1) = 0.0d0
  sest = 0.0d0
  gamma_val = 0.0d0
  call dlaic1( 2, j, x, sest, w, gamma_val, sestpr, s, c_val )
  call begin_test('job2_sest0_zero')
  call print_scalar('sestpr', sestpr)
  call print_scalar('s', s)
  call print_scalar('c', c_val)
  call end_test()

  ! Test 14: JOB=2, gamma near zero
  j = 2
  x(1) = 1.0d0; x(2) = 0.0d0
  w(1) = 0.5d0; w(2) = 0.3d0
  sest = 10.0d0
  gamma_val = 1.0d-20
  call dlaic1( 2, j, x, sest, w, gamma_val, sestpr, s, c_val )
  call begin_test('job2_gamma_tiny')
  call print_scalar('sestpr', sestpr)
  call print_scalar('s', s)
  call print_scalar('c', c_val)
  call end_test()

  ! Test 15: JOB=2, alpha near zero
  j = 2
  x(1) = 1.0d-20; x(2) = 0.0d0
  w(1) = 1.0d0; w(2) = 0.0d0
  sest = 10.0d0
  gamma_val = 5.0d0
  call dlaic1( 2, j, x, sest, w, gamma_val, sestpr, s, c_val )
  call begin_test('job2_alpha_tiny')
  call print_scalar('sestpr', sestpr)
  call print_scalar('s', s)
  call print_scalar('c', c_val)
  call end_test()

  ! Test 16: JOB=2, sest tiny
  j = 1
  x(1) = 1.0d0
  w(1) = 1.0d10
  sest = 1.0d-20
  gamma_val = 5.0d0
  call dlaic1( 2, j, x, sest, w, gamma_val, sestpr, s, c_val )
  call begin_test('job2_sest_tiny')
  call print_scalar('sestpr', sestpr)
  call print_scalar('s', s)
  call print_scalar('c', c_val)
  call end_test()

  ! Test 17: JOB=1, alpha tiny, s1 > s2 (absgam > absest)
  j = 1
  x(1) = 1.0d-20
  w(1) = 1.0d0
  sest = 3.0d0
  gamma_val = 10.0d0
  call dlaic1( 1, j, x, sest, w, gamma_val, sestpr, s, c_val )
  call begin_test('job1_alpha_tiny_s1gts2')
  call print_scalar('sestpr', sestpr)
  call print_scalar('s', s)
  call print_scalar('c', c_val)
  call end_test()

  ! Test 18: JOB=1, sest tiny, s1 > s2 (absgam > absalp)
  j = 1
  x(1) = 1.0d0
  w(1) = 2.0d0
  sest = 1.0d-20
  gamma_val = 100.0d0
  call dlaic1( 1, j, x, sest, w, gamma_val, sestpr, s, c_val )
  call begin_test('job1_sest_tiny_gam_large')
  call print_scalar('sestpr', sestpr)
  call print_scalar('s', s)
  call print_scalar('c', c_val)
  call end_test()

  ! Test 19: JOB=1, normal case with b <= 0
  j = 1
  x(1) = 1.0d0
  w(1) = 3.0d0
  sest = 1.0d0
  gamma_val = 3.0d0
  call dlaic1( 1, j, x, sest, w, gamma_val, sestpr, s, c_val )
  call begin_test('job1_normal_b_neg')
  call print_scalar('sestpr', sestpr)
  call print_scalar('s', s)
  call print_scalar('c', c_val)
  call end_test()

  ! Test 20: JOB=2, alpha tiny, s1 > s2 (absgam > absest)
  j = 1
  x(1) = 1.0d-20
  w(1) = 1.0d0
  sest = 3.0d0
  gamma_val = 10.0d0
  call dlaic1( 2, j, x, sest, w, gamma_val, sestpr, s, c_val )
  call begin_test('job2_alpha_tiny_s1gts2')
  call print_scalar('sestpr', sestpr)
  call print_scalar('s', s)
  call print_scalar('c', c_val)
  call end_test()

  ! Test 21: JOB=2, sest tiny, s1 > s2 (absgam > absalp)
  j = 1
  x(1) = 1.0d0
  w(1) = 2.0d0
  sest = 1.0d-20
  gamma_val = 100.0d0
  call dlaic1( 2, j, x, sest, w, gamma_val, sestpr, s, c_val )
  call begin_test('job2_sest_tiny_gam_large')
  call print_scalar('sestpr', sestpr)
  call print_scalar('s', s)
  call print_scalar('c', c_val)
  call end_test()

  ! Test 22: JOB=2, normal case with test < 0 (zeta2 >> zeta1)
  ! Need: 1 + 2*(zeta1^2 - zeta2^2) < 0 => zeta2^2 - zeta1^2 > 0.5
  ! alpha = dot(x,w), zeta1=alpha/sest, zeta2=gamma/sest
  ! alpha = 0.1, sest=1, gamma=2 => zeta1=0.1, zeta2=2
  ! test = 1 + 2*(0.01 - 4) = 1 - 7.98 = -6.98 < 0 => YES
  j = 1
  x(1) = 1.0d0
  w(1) = 0.1d0
  sest = 1.0d0
  gamma_val = 2.0d0
  call dlaic1( 2, j, x, sest, w, gamma_val, sestpr, s, c_val )
  call begin_test('job2_normal_test_neg')
  call print_scalar('sestpr', sestpr)
  call print_scalar('s', s)
  call print_scalar('c', c_val)
  call end_test()

  ! Test 23: JOB=2, normal case with test < 0 and b < 0
  ! b = (zeta2^2 + zeta1^2 - 1)/2
  ! With zeta1=0.1, zeta2=0.5: b=(0.25+0.01-1)/2 = -0.37 < 0
  ! test = 1 + 2*(0.01-0.25) = 1 - 0.48 = 0.52 >= 0 -- NO
  ! Need bigger zeta2. zeta1=0.1, zeta2=1.5: test=1+2*(0.01-2.25)=-3.48 < 0
  ! b = (2.25+0.01-1)/2 = 0.63 >= 0 -- takes b>=0 branch
  ! For b < 0: need zeta1^2+zeta2^2 < 1 but zeta2^2-zeta1^2 > 0.5
  ! zeta1=0.1, zeta2=0.8: test=1+2*(0.01-0.64)=-0.26 < 0
  ! b=(0.64+0.01-1)/2=-0.175 < 0 => YES
  j = 1
  x(1) = 1.0d0
  w(1) = 0.1d0
  sest = 1.0d0
  gamma_val = 0.8d0
  call dlaic1( 2, j, x, sest, w, gamma_val, sestpr, s, c_val )
  call begin_test('job2_normal_test_neg_b_neg')
  call print_scalar('sestpr', sestpr)
  call print_scalar('s', s)
  call print_scalar('c', c_val)
  call end_test()

end program test_dlaic1
