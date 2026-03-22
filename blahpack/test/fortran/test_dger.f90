program test_dger
  use test_utils
  implicit none

  double precision :: A(16), x(10), y(10)

  ! Test 1: basic 3x2, alpha=1
  ! x = [1, 2, 3], y = [4, 5]
  ! A := 1*x*y^T + 0 = [4 5; 8 10; 12 15]
  A = 0.0d0; x = 0.0d0; y = 0.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0
  y(1) = 4.0d0; y(2) = 5.0d0
  call dger(3, 2, 1.0d0, x, 1, y, 1, A, 3)
  call begin_test('basic')
  call print_matrix('A', A, 3, 3, 2)
  call end_test()

  ! Test 2: alpha=2
  A = 0.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0
  y(1) = 4.0d0; y(2) = 5.0d0
  call dger(3, 2, 2.0d0, x, 1, y, 1, A, 3)
  call begin_test('alpha_two')
  call print_matrix('A', A, 3, 3, 2)
  call end_test()

  ! Test 3: add to existing matrix
  A = 0.0d0
  A(1) = 10.0d0; A(5) = 20.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0
  y(1) = 4.0d0; y(2) = 5.0d0
  call dger(3, 2, 1.0d0, x, 1, y, 1, A, 3)
  call begin_test('add_existing')
  call print_matrix('A', A, 3, 3, 2)
  call end_test()

  ! Test 4: alpha=0 (no change)
  A = 0.0d0
  A(1) = 99.0d0
  call dger(3, 2, 0.0d0, x, 1, y, 1, A, 3)
  call begin_test('alpha_zero')
  call print_matrix('A', A, 3, 3, 2)
  call end_test()

  ! Test 5: M=0
  A = 0.0d0; A(1) = 99.0d0
  call dger(0, 2, 1.0d0, x, 1, y, 1, A, 1)
  call begin_test('m_zero')
  call print_array('A', A, 1)
  call end_test()

  ! Test 6: N=0
  A = 0.0d0; A(1) = 99.0d0
  call dger(3, 0, 1.0d0, x, 1, y, 1, A, 3)
  call begin_test('n_zero')
  call print_array('A', A, 1)
  call end_test()

  ! Test 7: non-unit stride incx=2
  A = 0.0d0; x = 0.0d0; y = 0.0d0
  x(1) = 1.0d0; x(3) = 2.0d0; x(5) = 3.0d0
  y(1) = 4.0d0; y(2) = 5.0d0
  call dger(3, 2, 1.0d0, x, 2, y, 1, A, 3)
  call begin_test('stride_x')
  call print_matrix('A', A, 3, 3, 2)
  call end_test()

  ! Test 8: negative stride incy=-1
  A = 0.0d0; x = 0.0d0; y = 0.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0
  y(1) = 4.0d0; y(2) = 5.0d0
  call dger(3, 2, 1.0d0, x, 1, y, -1, A, 3)
  call begin_test('neg_stride_y')
  call print_matrix('A', A, 3, 3, 2)
  call end_test()

end program
