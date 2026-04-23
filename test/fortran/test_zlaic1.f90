program test_zlaic1
  use test_utils
  implicit none

  integer, parameter :: NMAX = 10
  complex*16 :: x(NMAX), w(NMAX)
  complex*16 :: gamma_val, s_val, c_val, alpha_val
  double precision :: sest, sestpr
  double precision :: s_r(2), c_r(2)
  equivalence (s_val, s_r)
  equivalence (c_val, c_r)
  integer :: j

  ! ---- Test 1: JOB=1, N=3, normal case ----
  j = 3
  x(1) = dcmplx(0.6d0, 0.1d0)
  x(2) = dcmplx(0.5d0, -0.2d0)
  x(3) = dcmplx(0.4d0, 0.3d0)
  w(1) = dcmplx(0.3d0, 0.4d0)
  w(2) = dcmplx(0.7d0, -0.1d0)
  w(3) = dcmplx(0.2d0, 0.5d0)
  sest = 2.5d0
  gamma_val = dcmplx(1.0d0, 0.5d0)

  call ZLAIC1(1, j, x, sest, w, gamma_val, sestpr, s_val, c_val)

  call begin_test('job1_n3_normal')
  call print_scalar('sestpr', sestpr)
  call print_array('s', s_r, 2)
  call print_array('c', c_r, 2)
  call end_test()

  ! ---- Test 2: JOB=2, N=3, normal case ----
  j = 3
  x(1) = dcmplx(0.6d0, 0.1d0)
  x(2) = dcmplx(0.5d0, -0.2d0)
  x(3) = dcmplx(0.4d0, 0.3d0)
  w(1) = dcmplx(0.3d0, 0.4d0)
  w(2) = dcmplx(0.7d0, -0.1d0)
  w(3) = dcmplx(0.2d0, 0.5d0)
  sest = 2.5d0
  gamma_val = dcmplx(1.0d0, 0.5d0)

  call ZLAIC1(2, j, x, sest, w, gamma_val, sestpr, s_val, c_val)

  call begin_test('job2_n3_normal')
  call print_scalar('sestpr', sestpr)
  call print_array('s', s_r, 2)
  call print_array('c', c_r, 2)
  call end_test()

  ! ---- Test 3: JOB=1, SEST=0 ----
  j = 2
  x(1) = dcmplx(0.7d0, 0.0d0)
  x(2) = dcmplx(0.0d0, 0.7d0)
  w(1) = dcmplx(1.0d0, 0.0d0)
  w(2) = dcmplx(0.0d0, 1.0d0)
  sest = 0.0d0
  gamma_val = dcmplx(0.5d0, 0.3d0)

  call ZLAIC1(1, j, x, sest, w, gamma_val, sestpr, s_val, c_val)

  call begin_test('job1_sest0')
  call print_scalar('sestpr', sestpr)
  call print_array('s', s_r, 2)
  call print_array('c', c_r, 2)
  call end_test()

  ! ---- Test 4: JOB=2, SEST=0 ----
  j = 2
  x(1) = dcmplx(0.7d0, 0.0d0)
  x(2) = dcmplx(0.0d0, 0.7d0)
  w(1) = dcmplx(1.0d0, 0.0d0)
  w(2) = dcmplx(0.0d0, 1.0d0)
  sest = 0.0d0
  gamma_val = dcmplx(0.5d0, 0.3d0)

  call ZLAIC1(2, j, x, sest, w, gamma_val, sestpr, s_val, c_val)

  call begin_test('job2_sest0')
  call print_scalar('sestpr', sestpr)
  call print_array('s', s_r, 2)
  call print_array('c', c_r, 2)
  call end_test()

  ! ---- Test 5: JOB=1, N=1 ----
  j = 1
  x(1) = dcmplx(1.0d0, 0.0d0)
  w(1) = dcmplx(0.5d0, 0.5d0)
  sest = 3.0d0
  gamma_val = dcmplx(2.0d0, -1.0d0)

  call ZLAIC1(1, j, x, sest, w, gamma_val, sestpr, s_val, c_val)

  call begin_test('job1_n1')
  call print_scalar('sestpr', sestpr)
  call print_array('s', s_r, 2)
  call print_array('c', c_r, 2)
  call end_test()

  ! ---- Test 6: JOB=2, N=1 ----
  j = 1
  x(1) = dcmplx(1.0d0, 0.0d0)
  w(1) = dcmplx(0.5d0, 0.5d0)
  sest = 3.0d0
  gamma_val = dcmplx(2.0d0, -1.0d0)

  call ZLAIC1(2, j, x, sest, w, gamma_val, sestpr, s_val, c_val)

  call begin_test('job2_n1')
  call print_scalar('sestpr', sestpr)
  call print_array('s', s_r, 2)
  call print_array('c', c_r, 2)
  call end_test()

  ! ---- Test 7: JOB=1, SEST=0, alpha=0, gamma=0 ----
  j = 1
  x(1) = dcmplx(1.0d0, 0.0d0)
  w(1) = dcmplx(0.0d0, 0.0d0)
  sest = 0.0d0
  gamma_val = dcmplx(0.0d0, 0.0d0)

  call ZLAIC1(1, j, x, sest, w, gamma_val, sestpr, s_val, c_val)

  call begin_test('job1_sest0_zero')
  call print_scalar('sestpr', sestpr)
  call print_array('s', s_r, 2)
  call print_array('c', c_r, 2)
  call end_test()

  ! ---- Test 8: JOB=2, SEST=0, alpha=0, gamma=0 ----
  j = 1
  x(1) = dcmplx(1.0d0, 0.0d0)
  w(1) = dcmplx(0.0d0, 0.0d0)
  sest = 0.0d0
  gamma_val = dcmplx(0.0d0, 0.0d0)

  call ZLAIC1(2, j, x, sest, w, gamma_val, sestpr, s_val, c_val)

  call begin_test('job2_sest0_zero')
  call print_scalar('sestpr', sestpr)
  call print_array('s', s_r, 2)
  call print_array('c', c_r, 2)
  call end_test()

  ! ---- Test 9: JOB=1, small gamma (absgam <= eps*absest) ----
  j = 2
  x(1) = dcmplx(0.6d0, 0.1d0)
  x(2) = dcmplx(0.5d0, -0.2d0)
  w(1) = dcmplx(0.3d0, 0.4d0)
  w(2) = dcmplx(0.7d0, -0.1d0)
  sest = 1.0d10
  gamma_val = dcmplx(1.0d-20, 0.0d0)

  call ZLAIC1(1, j, x, sest, w, gamma_val, sestpr, s_val, c_val)

  call begin_test('job1_small_gamma')
  call print_scalar('sestpr', sestpr)
  call print_array('s', s_r, 2)
  call print_array('c', c_r, 2)
  call end_test()

  ! ---- Test 10: JOB=2, small gamma ----
  j = 2
  x(1) = dcmplx(0.6d0, 0.1d0)
  x(2) = dcmplx(0.5d0, -0.2d0)
  w(1) = dcmplx(0.3d0, 0.4d0)
  w(2) = dcmplx(0.7d0, -0.1d0)
  sest = 1.0d10
  gamma_val = dcmplx(1.0d-20, 0.0d0)

  call ZLAIC1(2, j, x, sest, w, gamma_val, sestpr, s_val, c_val)

  call begin_test('job2_small_gamma')
  call print_scalar('sestpr', sestpr)
  call print_array('s', s_r, 2)
  call print_array('c', c_r, 2)
  call end_test()

  ! ---- Test 11: JOB=1, small alpha (absalp <= eps*absest) ----
  j = 1
  x(1) = dcmplx(1.0d0, 0.0d0)
  w(1) = dcmplx(1.0d-20, 0.0d0)
  sest = 1.0d10
  gamma_val = dcmplx(5.0d0, 3.0d0)

  call ZLAIC1(1, j, x, sest, w, gamma_val, sestpr, s_val, c_val)

  call begin_test('job1_small_alpha')
  call print_scalar('sestpr', sestpr)
  call print_array('s', s_r, 2)
  call print_array('c', c_r, 2)
  call end_test()

  ! ---- Test 12: JOB=2, small alpha ----
  j = 1
  x(1) = dcmplx(1.0d0, 0.0d0)
  w(1) = dcmplx(1.0d-20, 0.0d0)
  sest = 1.0d10
  gamma_val = dcmplx(5.0d0, 3.0d0)

  call ZLAIC1(2, j, x, sest, w, gamma_val, sestpr, s_val, c_val)

  call begin_test('job2_small_alpha')
  call print_scalar('sestpr', sestpr)
  call print_array('s', s_r, 2)
  call print_array('c', c_r, 2)
  call end_test()

  ! ---- Test 13: JOB=1, tiny sest (absest <= eps*absalp) ----
  j = 1
  x(1) = dcmplx(1.0d0, 0.0d0)
  w(1) = dcmplx(3.0d0, 4.0d0)
  sest = 1.0d-20
  gamma_val = dcmplx(2.0d0, 1.0d0)

  call ZLAIC1(1, j, x, sest, w, gamma_val, sestpr, s_val, c_val)

  call begin_test('job1_tiny_sest')
  call print_scalar('sestpr', sestpr)
  call print_array('s', s_r, 2)
  call print_array('c', c_r, 2)
  call end_test()

  ! ---- Test 14: JOB=2, tiny sest ----
  j = 1
  x(1) = dcmplx(1.0d0, 0.0d0)
  w(1) = dcmplx(3.0d0, 4.0d0)
  sest = 1.0d-20
  gamma_val = dcmplx(2.0d0, 1.0d0)

  call ZLAIC1(2, j, x, sest, w, gamma_val, sestpr, s_val, c_val)

  call begin_test('job2_tiny_sest')
  call print_scalar('sestpr', sestpr)
  call print_array('s', s_r, 2)
  call print_array('c', c_r, 2)
  call end_test()

  ! ---- Test 15: JOB=1, small alpha, s1 > s2 (absgam > absest) ----
  j = 1
  x(1) = dcmplx(1.0d0, 0.0d0)
  w(1) = dcmplx(1.0d-20, 0.0d0)
  sest = 1.0d0
  gamma_val = dcmplx(5.0d0, 3.0d0)

  call ZLAIC1(1, j, x, sest, w, gamma_val, sestpr, s_val, c_val)

  call begin_test('job1_small_alpha_s1_gt_s2')
  call print_scalar('sestpr', sestpr)
  call print_array('s', s_r, 2)
  call print_array('c', c_r, 2)
  call end_test()

  ! ---- Test 16: JOB=1, tiny sest, s1 > s2 (absgam > absalp) ----
  j = 1
  x(1) = dcmplx(1.0d0, 0.0d0)
  w(1) = dcmplx(1.0d0, 0.0d0)
  sest = 1.0d-20
  gamma_val = dcmplx(5.0d0, 3.0d0)

  call ZLAIC1(1, j, x, sest, w, gamma_val, sestpr, s_val, c_val)

  call begin_test('job1_tiny_sest_s1_gt_s2')
  call print_scalar('sestpr', sestpr)
  call print_array('s', s_r, 2)
  call print_array('c', c_r, 2)
  call end_test()

  ! ---- Test 17: JOB=2, small alpha, s1 > s2 (absgam > absest) ----
  j = 1
  x(1) = dcmplx(1.0d0, 0.0d0)
  w(1) = dcmplx(1.0d-20, 0.0d0)
  sest = 1.0d0
  gamma_val = dcmplx(5.0d0, 3.0d0)

  call ZLAIC1(2, j, x, sest, w, gamma_val, sestpr, s_val, c_val)

  call begin_test('job2_small_alpha_s1_gt_s2')
  call print_scalar('sestpr', sestpr)
  call print_array('s', s_r, 2)
  call print_array('c', c_r, 2)
  call end_test()

  ! ---- Test 18: JOB=2, tiny sest, s1 > s2 (absgam > absalp) ----
  j = 1
  x(1) = dcmplx(1.0d0, 0.0d0)
  w(1) = dcmplx(1.0d0, 0.0d0)
  sest = 1.0d-20
  gamma_val = dcmplx(5.0d0, 3.0d0)

  call ZLAIC1(2, j, x, sest, w, gamma_val, sestpr, s_val, c_val)

  call begin_test('job2_tiny_sest_s1_gt_s2')
  call print_scalar('sestpr', sestpr)
  call print_array('s', s_r, 2)
  call print_array('c', c_r, 2)
  call end_test()

  ! ---- Test 19: JOB=2, normal case with test < 0 (zeta2 > zeta1), b < 0 ----
  ! Need: absalp/absest small, absgam/absest moderate so zeta2 > zeta1
  ! and zeta2^2 + zeta1^2 < 1 (for b < 0 in the test<0 branch)
  j = 1
  x(1) = dcmplx(1.0d0, 0.0d0)
  w(1) = dcmplx(0.3d0, 0.1d0)
  sest = 1.0d0
  gamma_val = dcmplx(2.0d0, 1.5d0)

  call ZLAIC1(2, j, x, sest, w, gamma_val, sestpr, s_val, c_val)

  call begin_test('job2_normal_test_neg')
  call print_scalar('sestpr', sestpr)
  call print_array('s', s_r, 2)
  call print_array('c', c_r, 2)
  call end_test()

  ! ---- Test 20: JOB=1, normal case b <= 0 (large zeta1 and zeta2) ----
  j = 1
  x(1) = dcmplx(1.0d0, 0.0d0)
  w(1) = dcmplx(2.0d0, 1.0d0)
  sest = 1.0d0
  gamma_val = dcmplx(2.0d0, 1.5d0)

  call ZLAIC1(1, j, x, sest, w, gamma_val, sestpr, s_val, c_val)

  call begin_test('job1_normal_b_neg')
  call print_scalar('sestpr', sestpr)
  call print_array('s', s_r, 2)
  call print_array('c', c_r, 2)
  call end_test()

  ! ---- Test 21: JOB=2, normal case with test >= 0, b >= 0 branch ----
  j = 2
  x(1) = dcmplx(0.8d0, 0.1d0)
  x(2) = dcmplx(0.3d0, -0.1d0)
  w(1) = dcmplx(0.5d0, 0.2d0)
  w(2) = dcmplx(0.4d0, -0.3d0)
  sest = 5.0d0
  gamma_val = dcmplx(1.0d0, 0.5d0)

  call ZLAIC1(2, j, x, sest, w, gamma_val, sestpr, s_val, c_val)

  call begin_test('job2_normal_test_pos')
  call print_scalar('sestpr', sestpr)
  call print_array('s', s_r, 2)
  call print_array('c', c_r, 2)
  call end_test()

  ! ---- Test 22: JOB=2, normal case test < 0 AND b < 0 ----
  ! zeta1 = absalp/absest = 0.1, zeta2 = absgam/absest = 0.8
  ! test = 1+2*(0.01-0.64) = -0.26 < 0
  ! b = (0.64+0.01-1)/2 = -0.175 < 0
  j = 1
  x(1) = dcmplx(1.0d0, 0.0d0)
  w(1) = dcmplx(1.0d0, 0.0d0)
  sest = 10.0d0
  gamma_val = dcmplx(6.0d0, 5.291502622d0)

  call ZLAIC1(2, j, x, sest, w, gamma_val, sestpr, s_val, c_val)

  call begin_test('job2_normal_test_neg_b_neg')
  call print_scalar('sestpr', sestpr)
  call print_array('s', s_r, 2)
  call print_array('c', c_r, 2)
  call end_test()

end program test_zlaic1
