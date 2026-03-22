program test_zhemv
  use test_utils
  implicit none
  complex*16 :: A(25), x(10), y(10)
  double precision :: A_r(50), x_r(20), y_r(20)
  equivalence (A, A_r)
  equivalence (x, x_r)
  equivalence (y, y_r)
  complex*16 :: alpha, beta

  ! Test 1: upper triangle, 3x3, alpha=(1,0), beta=(0,0)
  ! Hermitian A (upper stored):
  !   A = [ 2    (1+1i) (2-1i) ]
  !       [ .     3     (1+2i) ]
  !       [ .     .      4     ]
  ! x = [(1,0), (0,1), (1,1)]
  ! y := alpha * A * x + beta * y = A * x
  A = (0.0d0, 0.0d0)
  A(1) = (2.0d0, 0.0d0)   ! A(1,1) = 2 (real diagonal)
  A(4) = (1.0d0, 1.0d0)   ! A(1,2) = 1+i
  A(5) = (3.0d0, 0.0d0)   ! A(2,2) = 3 (real diagonal)
  A(7) = (2.0d0, -1.0d0)  ! A(1,3) = 2-i
  A(8) = (1.0d0, 2.0d0)   ! A(2,3) = 1+2i
  A(9) = (4.0d0, 0.0d0)   ! A(3,3) = 4 (real diagonal)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (0.0d0, 1.0d0)
  x(3) = (1.0d0, 1.0d0)
  y = (0.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  beta = (0.0d0, 0.0d0)
  call zhemv('U', 3, alpha, A, 3, x, 1, beta, y, 1)
  call begin_test('upper_basic')
  call print_array('y', y_r, 6)
  call end_test()

  ! Test 2: lower triangle, 3x3, alpha=(1,0), beta=(0,0)
  ! Same Hermitian matrix but stored in lower triangle:
  !   A = [ 2     .      .    ]
  !       [(1-1i) 3      .    ]
  !       [(2+1i) (1-2i) 4    ]
  A = (0.0d0, 0.0d0)
  A(1) = (2.0d0, 0.0d0)   ! A(1,1) = 2
  A(2) = (1.0d0, -1.0d0)  ! A(2,1) = 1-i (conj of upper A(1,2))
  A(3) = (2.0d0, 1.0d0)   ! A(3,1) = 2+i (conj of upper A(1,3))
  A(5) = (3.0d0, 0.0d0)   ! A(2,2) = 3
  A(6) = (1.0d0, -2.0d0)  ! A(3,2) = 1-2i (conj of upper A(2,3))
  A(9) = (4.0d0, 0.0d0)   ! A(3,3) = 4
  x(1) = (1.0d0, 0.0d0)
  x(2) = (0.0d0, 1.0d0)
  x(3) = (1.0d0, 1.0d0)
  y = (0.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  beta = (0.0d0, 0.0d0)
  call zhemv('L', 3, alpha, A, 3, x, 1, beta, y, 1)
  call begin_test('lower_basic')
  call print_array('y', y_r, 6)
  call end_test()

  ! Test 3: N=0 quick return
  y(1) = (99.0d0, 88.0d0)
  alpha = (1.0d0, 0.0d0)
  beta = (0.0d0, 0.0d0)
  call zhemv('U', 0, alpha, A, 1, x, 1, beta, y, 1)
  call begin_test('n_zero')
  call print_array('y', y_r, 2)
  call end_test()

  ! Test 4: alpha=0, beta=1 quick return
  y(1) = (5.0d0, 6.0d0)
  y(2) = (7.0d0, 8.0d0)
  alpha = (0.0d0, 0.0d0)
  beta = (1.0d0, 0.0d0)
  call zhemv('U', 2, alpha, A, 3, x, 1, beta, y, 1)
  call begin_test('alpha_zero_beta_one')
  call print_array('y', y_r, 4)
  call end_test()

  ! Test 5: alpha=0, beta=(2,1) — scale y only
  y(1) = (1.0d0, 0.0d0)
  y(2) = (0.0d0, 1.0d0)
  alpha = (0.0d0, 0.0d0)
  beta = (2.0d0, 1.0d0)
  call zhemv('U', 2, alpha, A, 3, x, 1, beta, y, 1)
  call begin_test('alpha_zero_beta_scale')
  call print_array('y', y_r, 4)
  call end_test()

  ! Test 6: N=1
  A(1) = (5.0d0, 0.0d0)  ! Hermitian 1x1: diagonal must be real
  x(1) = (2.0d0, 3.0d0)
  y(1) = (1.0d0, 1.0d0)
  alpha = (1.0d0, 0.0d0)
  beta = (1.0d0, 0.0d0)
  call zhemv('U', 1, alpha, A, 1, x, 1, beta, y, 1)
  call begin_test('n_one')
  call print_array('y', y_r, 2)
  call end_test()

  ! Test 7: upper, non-unit stride incx=2, incy=2
  A = (0.0d0, 0.0d0)
  A(1) = (2.0d0, 0.0d0)
  A(4) = (1.0d0, 1.0d0)
  A(5) = (3.0d0, 0.0d0)
  A(7) = (2.0d0, -1.0d0)
  A(8) = (1.0d0, 2.0d0)
  A(9) = (4.0d0, 0.0d0)
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(3) = (0.0d0, 1.0d0)
  x(5) = (1.0d0, 1.0d0)
  y = (0.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  beta = (0.0d0, 0.0d0)
  call zhemv('U', 3, alpha, A, 3, x, 2, beta, y, 2)
  call begin_test('upper_stride')
  call print_array('y', y_r, 10)
  call end_test()

  ! Test 8: lower, non-unit stride incx=2, incy=2
  A = (0.0d0, 0.0d0)
  A(1) = (2.0d0, 0.0d0)
  A(2) = (1.0d0, -1.0d0)
  A(3) = (2.0d0, 1.0d0)
  A(5) = (3.0d0, 0.0d0)
  A(6) = (1.0d0, -2.0d0)
  A(9) = (4.0d0, 0.0d0)
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(3) = (0.0d0, 1.0d0)
  x(5) = (1.0d0, 1.0d0)
  y = (0.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  beta = (0.0d0, 0.0d0)
  call zhemv('L', 3, alpha, A, 3, x, 2, beta, y, 2)
  call begin_test('lower_stride')
  call print_array('y', y_r, 10)
  call end_test()

  ! Test 9: complex alpha and beta
  A = (0.0d0, 0.0d0)
  A(1) = (2.0d0, 0.0d0)
  A(4) = (1.0d0, 1.0d0)
  A(5) = (3.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (1.0d0, 0.0d0)
  y(1) = (1.0d0, 0.0d0)
  y(2) = (0.0d0, 1.0d0)
  alpha = (2.0d0, 1.0d0)
  beta = (1.0d0, -1.0d0)
  call zhemv('U', 2, alpha, A, 2, x, 1, beta, y, 1)
  call begin_test('complex_alpha_beta')
  call print_array('y', y_r, 4)
  call end_test()

  ! Test 10: beta=0 (zero y first)
  A = (0.0d0, 0.0d0)
  A(1) = (1.0d0, 0.0d0)
  A(3) = (1.0d0, 1.0d0)
  A(4) = (2.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (1.0d0, 0.0d0)
  y(1) = (99.0d0, 99.0d0)
  y(2) = (99.0d0, 99.0d0)
  alpha = (1.0d0, 0.0d0)
  beta = (0.0d0, 0.0d0)
  call zhemv('U', 2, alpha, A, 2, x, 1, beta, y, 1)
  call begin_test('beta_zero')
  call print_array('y', y_r, 4)
  call end_test()

end program
