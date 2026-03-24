program test_drotmg
  use test_utils
  implicit none
  double precision :: dd1, dd2, dx1, dy1, dparam(5)

  ! Test 1: basic case |q1| > |q2|, flag=0
  dd1 = 2.0d0; dd2 = 1.0d0; dx1 = 3.0d0; dy1 = 4.0d0
  dparam = 0.0d0
  call drotmg(dd1, dd2, dx1, dy1, dparam)
  call begin_test('basic_q1_gt_q2')
  call print_scalar('dd1', dd1)
  call print_scalar('dd2', dd2)
  call print_scalar('dx1', dx1)
  call print_array('dparam', dparam, 5)
  call end_test()

  ! Test 2: |q2| > |q1|, flag=1
  dd1 = 1.0d0; dd2 = 2.0d0; dx1 = 1.0d0; dy1 = 3.0d0
  dparam = 0.0d0
  call drotmg(dd1, dd2, dx1, dy1, dparam)
  call begin_test('q2_gt_q1')
  call print_scalar('dd1', dd1)
  call print_scalar('dd2', dd2)
  call print_scalar('dx1', dx1)
  call print_array('dparam', dparam, 5)
  call end_test()

  ! Test 3: dd1 < 0 (error case)
  dd1 = -1.0d0; dd2 = 1.0d0; dx1 = 1.0d0; dy1 = 1.0d0
  dparam = 0.0d0
  call drotmg(dd1, dd2, dx1, dy1, dparam)
  call begin_test('dd1_negative')
  call print_scalar('dd1', dd1)
  call print_scalar('dd2', dd2)
  call print_scalar('dx1', dx1)
  call print_array('dparam', dparam, 5)
  call end_test()

  ! Test 4: dy1 = 0 (quick return with flag=-2)
  dd1 = 1.0d0; dd2 = 1.0d0; dx1 = 1.0d0; dy1 = 0.0d0
  dparam = 0.0d0
  call drotmg(dd1, dd2, dx1, dy1, dparam)
  call begin_test('dy1_zero')
  call print_scalar('dd1', dd1)
  call print_scalar('dd2', dd2)
  call print_scalar('dx1', dx1)
  call print_array('dparam', dparam, 5)
  call end_test()

  ! Test 5: dd2 = 0 (p2 = 0, quick return)
  dd1 = 1.0d0; dd2 = 0.0d0; dx1 = 1.0d0; dy1 = 1.0d0
  dparam = 0.0d0
  call drotmg(dd1, dd2, dx1, dy1, dparam)
  call begin_test('dd2_zero')
  call print_scalar('dd1', dd1)
  call print_scalar('dd2', dd2)
  call print_scalar('dx1', dx1)
  call print_array('dparam', dparam, 5)
  call end_test()

  ! Test 6: q2 < 0 (negative definite case)
  dd1 = 1.0d0; dd2 = -1.0d0; dx1 = 1.0d0; dy1 = 2.0d0
  dparam = 0.0d0
  call drotmg(dd1, dd2, dx1, dy1, dparam)
  call begin_test('q2_negative')
  call print_scalar('dd1', dd1)
  call print_scalar('dd2', dd2)
  call print_scalar('dx1', dx1)
  call print_array('dparam', dparam, 5)
  call end_test()

  ! Test 7: rescaling (very small dd1)
  dd1 = 1.0d-10; dd2 = 1.0d0; dx1 = 1.0d0; dy1 = 1.0d0
  dparam = 0.0d0
  call drotmg(dd1, dd2, dx1, dy1, dparam)
  call begin_test('rescale_small')
  call print_scalar('dd1', dd1)
  call print_scalar('dd2', dd2)
  call print_scalar('dx1', dx1)
  call print_array('dparam', dparam, 5)
  call end_test()

  ! Test 8: rescaling (very large dd1)
  dd1 = 1.0d10; dd2 = 1.0d0; dx1 = 1.0d0; dy1 = 1.0d0
  dparam = 0.0d0
  call drotmg(dd1, dd2, dx1, dy1, dparam)
  call begin_test('rescale_large')
  call print_scalar('dd1', dd1)
  call print_scalar('dd2', dd2)
  call print_scalar('dx1', dx1)
  call print_array('dparam', dparam, 5)
  call end_test()

  ! Test 9: dd1 = 0 (degenerate, q1 = 0)
  dd1 = 0.0d0; dd2 = 3.0d0; dx1 = 5.0d0; dy1 = 4.0d0
  dparam = 0.0d0
  call drotmg(dd1, dd2, dx1, dy1, dparam)
  call begin_test('dd1_zero')
  call print_scalar('dd1', dd1)
  call print_scalar('dd2', dd2)
  call print_scalar('dx1', dx1)
  call print_array('dparam', dparam, 5)
  call end_test()

  ! Test 10: dx1 = 0 (q1 = 0, swap)
  dd1 = 2.0d0; dd2 = 3.0d0; dx1 = 0.0d0; dy1 = 4.0d0
  dparam = 0.0d0
  call drotmg(dd1, dd2, dx1, dy1, dparam)
  call begin_test('dx1_zero')
  call print_scalar('dd1', dd1)
  call print_scalar('dd2', dd2)
  call print_scalar('dx1', dx1)
  call print_array('dparam', dparam, 5)
  call end_test()

  ! Test 11: negative dy1
  dd1 = 2.0d0; dd2 = 1.0d0; dx1 = 5.0d0; dy1 = -3.0d0
  dparam = 0.0d0
  call drotmg(dd1, dd2, dx1, dy1, dparam)
  call begin_test('negative_dy1')
  call print_scalar('dd1', dd1)
  call print_scalar('dd2', dd2)
  call print_scalar('dx1', dx1)
  call print_array('dparam', dparam, 5)
  call end_test()

  ! Test 12: equal d values
  dd1 = 1.0d0; dd2 = 1.0d0; dx1 = 3.0d0; dy1 = 4.0d0
  dparam = 0.0d0
  call drotmg(dd1, dd2, dx1, dy1, dparam)
  call begin_test('equal_d')
  call print_scalar('dd1', dd1)
  call print_scalar('dd2', dd2)
  call print_scalar('dx1', dx1)
  call print_array('dparam', dparam, 5)
  call end_test()

  ! Test 13: rescaling dd2 very small
  dd1 = 1.0d0; dd2 = 1.0d-10; dx1 = 1.0d0; dy1 = 1.0d5
  dparam = 0.0d0
  call drotmg(dd1, dd2, dx1, dy1, dparam)
  call begin_test('rescale_dd2_small')
  call print_scalar('dd1', dd1)
  call print_scalar('dd2', dd2)
  call print_scalar('dx1', dx1)
  call print_array('dparam', dparam, 5)
  call end_test()

  ! Test 14: rescaling dd2 very large
  dd1 = 1.0d0; dd2 = 1.0d10; dx1 = 1.0d0; dy1 = 1.0d-5
  dparam = 0.0d0
  call drotmg(dd1, dd2, dx1, dy1, dparam)
  call begin_test('rescale_dd2_large')
  call print_scalar('dd1', dd1)
  call print_scalar('dd2', dd2)
  call print_scalar('dx1', dx1)
  call print_array('dparam', dparam, 5)
  call end_test()

end program
