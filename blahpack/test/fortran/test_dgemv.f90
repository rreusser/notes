program test_dgemv
  use test_utils
  implicit none
  double precision :: a(20), x(20), y(20)

  ! Test 1: basic — trans='N', m=3, n=2, alpha=1, beta=0, incx=1, incy=1
  ! A = [1 4; 2 5; 3 6] (column-major: 1,2,3,4,5,6)
  ! x = [1, 2], y = [0, 0, 0]
  ! y = 1 * A * x + 0 * y = [1*1+4*2, 2*1+5*2, 3*1+6*2] = [9, 12, 15]
  a = 0.0d0; x = 0.0d0; y = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0
  a(4) = 4.0d0; a(5) = 5.0d0; a(6) = 6.0d0
  x(1) = 1.0d0; x(2) = 2.0d0
  call dgemv('N', 3, 2, 1.0d0, a, 3, x, 1, 0.0d0, y, 1)
  call begin_test('basic')
  call print_array('y', y, 3)
  call end_test()

  ! Test 2: transpose — trans='T', m=3, n=2, same A
  ! y (length 2) = alpha * A^T * x (length 3) + beta * y
  ! A^T = [1 2 3; 4 5 6], x = [1, 2, 3]
  ! y = [1*1+2*2+3*3, 4*1+5*2+6*3] = [14, 32]
  a = 0.0d0; x = 0.0d0; y = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0
  a(4) = 4.0d0; a(5) = 5.0d0; a(6) = 6.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0
  call dgemv('T', 3, 2, 1.0d0, a, 3, x, 1, 0.0d0, y, 1)
  call begin_test('transpose')
  call print_array('y', y, 2)
  call end_test()

  ! Test 3: alpha_beta — alpha=2.0, beta=3.0, trans='N'
  ! y = 2 * A * x + 3 * y_initial
  a = 0.0d0; x = 0.0d0; y = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0
  a(4) = 4.0d0; a(5) = 5.0d0; a(6) = 6.0d0
  x(1) = 1.0d0; x(2) = 2.0d0
  y(1) = 10.0d0; y(2) = 20.0d0; y(3) = 30.0d0
  call dgemv('N', 3, 2, 2.0d0, a, 3, x, 1, 3.0d0, y, 1)
  call begin_test('alpha_beta')
  call print_array('y', y, 3)
  call end_test()

  ! Test 4: n_zero — quick return
  y(1) = 99.0d0
  call dgemv('N', 3, 0, 1.0d0, a, 3, x, 1, 0.0d0, y, 1)
  call begin_test('n_zero')
  call print_array('y', y, 1)
  call end_test()

  ! Test 5: m_zero — quick return
  y(1) = 99.0d0
  call dgemv('N', 0, 2, 1.0d0, a, 1, x, 1, 0.0d0, y, 1)
  call begin_test('m_zero')
  call print_array('y', y, 1)
  call end_test()

  ! Test 6: non-unit strides — incx=2, incy=2, trans='N'
  ! A = [1 4; 2 5; 3 6] (3x2), x at stride 2: x(1)=1, x(3)=2
  ! y at stride 2: y(1), y(3), y(5)
  a = 0.0d0; x = 0.0d0; y = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0
  a(4) = 4.0d0; a(5) = 5.0d0; a(6) = 6.0d0
  x(1) = 1.0d0; x(3) = 2.0d0
  y(1) = 10.0d0; y(3) = 20.0d0; y(5) = 30.0d0
  call dgemv('N', 3, 2, 1.0d0, a, 3, x, 2, 1.0d0, y, 2)
  call begin_test('stride')
  call print_array('y', y, 6)
  call end_test()

  ! Test 7: transpose with alpha and beta
  ! trans='T', A(3x2), x length 3, y length 2
  ! y = 2 * A^T * x + 3 * y
  a = 0.0d0; x = 0.0d0; y = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0
  a(4) = 4.0d0; a(5) = 5.0d0; a(6) = 6.0d0
  x(1) = 1.0d0; x(2) = 1.0d0; x(3) = 1.0d0
  y(1) = 5.0d0; y(2) = 10.0d0
  call dgemv('T', 3, 2, 2.0d0, a, 3, x, 1, 3.0d0, y, 1)
  call begin_test('transpose_alpha_beta')
  call print_array('y', y, 2)
  call end_test()

  ! Test 8: alpha=0 — should just scale y by beta
  y = 0.0d0
  y(1) = 10.0d0; y(2) = 20.0d0; y(3) = 30.0d0
  call dgemv('N', 3, 2, 0.0d0, a, 3, x, 1, 2.0d0, y, 1)
  call begin_test('alpha_zero')
  call print_array('y', y, 3)
  call end_test()

end program
