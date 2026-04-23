program test_zgeru
  use test_utils
  implicit none
  double precision :: A_r(20), x_r(10), y_r(10)
  complex*16 :: A(2,3), x(3), y(3), alpha
  equivalence (A, A_r)
  equivalence (x, x_r)
  equivalence (y, y_r)

  ! ============================================================
  ! Test 1: Basic 2x3 rank-1 update with alpha=(1,0)
  ! A = [1+2i, 3+4i, 5+6i; 7+8i, 9+10i, 11+12i]
  ! x = [1+1i, 2+2i]
  ! y = [1+0i, 0+1i, 1+1i]
  A(1,1) = (1.0d0, 2.0d0); A(2,1) = (7.0d0, 8.0d0)
  A(1,2) = (3.0d0, 4.0d0); A(2,2) = (9.0d0, 10.0d0)
  A(1,3) = (5.0d0, 6.0d0); A(2,3) = (11.0d0, 12.0d0)
  x(1) = (1.0d0, 1.0d0); x(2) = (2.0d0, 2.0d0)
  y(1) = (1.0d0, 0.0d0); y(2) = (0.0d0, 1.0d0); y(3) = (1.0d0, 1.0d0)
  alpha = (1.0d0, 0.0d0)
  call ZGERU(2, 3, alpha, x, 1, y, 1, A, 2)
  call begin_test('basic_2x3')
  call print_array('A', A_r, 12)
  call end_test()

  ! ============================================================
  ! Test 2: alpha = (2, -1)
  A(1,1) = (1.0d0, 0.0d0); A(2,1) = (0.0d0, 0.0d0)
  A(1,2) = (0.0d0, 0.0d0); A(2,2) = (1.0d0, 0.0d0)
  x(1) = (1.0d0, 2.0d0); x(2) = (3.0d0, 4.0d0)
  y(1) = (2.0d0, 1.0d0); y(2) = (1.0d0, -1.0d0)
  alpha = (2.0d0, -1.0d0)
  call ZGERU(2, 2, alpha, x, 1, y, 1, A, 2)
  call begin_test('alpha_2_neg1')
  call print_array('A', A_r, 8)
  call end_test()

  ! ============================================================
  ! Test 3: alpha = 0 (no update)
  A(1,1) = (5.0d0, 6.0d0); A(2,1) = (7.0d0, 8.0d0)
  x(1) = (1.0d0, 1.0d0); x(2) = (2.0d0, 2.0d0)
  y(1) = (3.0d0, 3.0d0); y(2) = (4.0d0, 4.0d0)
  alpha = (0.0d0, 0.0d0)
  call ZGERU(2, 2, alpha, x, 1, y, 1, A, 2)
  call begin_test('alpha_zero')
  call print_array('A', A_r, 8)
  call end_test()

  ! ============================================================
  ! Test 4: N=0 quick return
  A(1,1) = (5.0d0, 6.0d0)
  alpha = (1.0d0, 0.0d0)
  call ZGERU(2, 0, alpha, x, 1, y, 1, A, 2)
  call begin_test('n_zero')
  call print_array('A', A_r, 2)
  call end_test()

  ! ============================================================
  ! Test 5: M=0 quick return
  call ZGERU(0, 2, alpha, x, 1, y, 1, A, 2)
  call begin_test('m_zero')
  call print_array('A', A_r, 2)
  call end_test()

  ! ============================================================
  ! Test 6: Non-unit strides, incx=2, incy=2
  A(1,1) = (0.0d0, 0.0d0); A(2,1) = (0.0d0, 0.0d0)
  A(1,2) = (0.0d0, 0.0d0); A(2,2) = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0); x(2) = (99.0d0, 99.0d0); x(3) = (0.0d0, 1.0d0)
  y(1) = (2.0d0, 0.0d0); y(2) = (88.0d0, 88.0d0); y(3) = (0.0d0, 2.0d0)
  alpha = (1.0d0, 0.0d0)
  call ZGERU(2, 2, alpha, x, 2, y, 2, A, 2)
  call begin_test('stride_2')
  call print_array('A', A_r, 8)
  call end_test()

  ! ============================================================
  ! Test 7: Negative stride incy=-1
  A(1,1) = (0.0d0, 0.0d0); A(2,1) = (0.0d0, 0.0d0)
  A(1,2) = (0.0d0, 0.0d0); A(2,2) = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 1.0d0); x(2) = (2.0d0, 0.0d0)
  y(1) = (3.0d0, 0.0d0); y(2) = (0.0d0, 3.0d0)
  alpha = (1.0d0, 0.0d0)
  call ZGERU(2, 2, alpha, x, 1, y, -1, A, 2)
  call begin_test('neg_incy')
  call print_array('A', A_r, 8)
  call end_test()

  ! ============================================================
  ! Test 8: 1x1 matrix
  A(1,1) = (1.0d0, 1.0d0)
  x(1) = (2.0d0, 3.0d0)
  y(1) = (4.0d0, 5.0d0)
  alpha = (1.0d0, 0.0d0)
  call ZGERU(1, 1, alpha, x, 1, y, 1, A, 1)
  call begin_test('one_by_one')
  call print_array('A', A_r, 2)
  call end_test()

end program
