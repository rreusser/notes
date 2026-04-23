program test_zgemv
  use test_utils
  implicit none
  complex*16 :: a(20), x(10), y(10)
  double precision :: a_r(40), x_r(20), y_r(20)
  equivalence (a, a_r)
  equivalence (x, x_r)
  equivalence (y, y_r)
  complex*16 :: alpha, beta

  ! Test 1: basic trans='N', m=2, n=2, alpha=(1,0), beta=(0,0)
  ! A = [ (1+1i) (3+3i) ]   column-major: a(1)=(1,1), a(2)=(2,2), a(3)=(3,3), a(4)=(4,4)
  !     [ (2+2i) (4+4i) ]
  ! x = [ (1,0), (1,0) ]
  ! y = alpha * A * x = A * x = [(1+1i)+(3+3i), (2+2i)+(4+4i)] = [(4,4), (6,6)]
  a = (0.0d0, 0.0d0)
  x = (0.0d0, 0.0d0)
  y = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 1.0d0)
  a(2) = (2.0d0, 2.0d0)
  a(3) = (3.0d0, 3.0d0)
  a(4) = (4.0d0, 4.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  beta = (0.0d0, 0.0d0)
  call zgemv('N', 2, 2, alpha, a, 2, x, 1, beta, y, 1)
  call begin_test('zgemv_basic')
  call print_array('y', y_r, 4)
  call end_test()

  ! Test 2: trans='C' (conjugate transpose), m=2, n=2
  ! A^H * x: uses conjugate of A
  ! A^H = [ conj(1+1i) conj(2+2i) ] = [ (1,-1) (2,-2) ]
  !       [ conj(3+3i) conj(4+4i) ]   [ (3,-3) (4,-4) ]
  ! x = [(1,1), (1,1)]
  ! y = A^H * x
  ! y(1) = (1-1i)*(1+1i) + (2-2i)*(1+1i) = (1+1-1+1) + (2+2-2+2) = 2 + 4 = (6, 0)
  !        (1-i)*(1+i) = 1+i-i-i^2 = 1+1 = 2
  !        (2-2i)*(1+i) = 2+2i-2i-2i^2 = 2+2 = 4
  ! y(2) = (3-3i)*(1+i) + (4-4i)*(1+i) = (3+3)+(4+4) = 6 + 8 = (14, 0)
  a = (0.0d0, 0.0d0)
  x = (0.0d0, 0.0d0)
  y = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 1.0d0)
  a(2) = (2.0d0, 2.0d0)
  a(3) = (3.0d0, 3.0d0)
  a(4) = (4.0d0, 4.0d0)
  x(1) = (1.0d0, 1.0d0)
  x(2) = (1.0d0, 1.0d0)
  alpha = (1.0d0, 0.0d0)
  beta = (0.0d0, 0.0d0)
  call zgemv('C', 2, 2, alpha, a, 2, x, 1, beta, y, 1)
  call begin_test('zgemv_conj_trans')
  call print_array('y', y_r, 4)
  call end_test()

  ! Test 3: alpha and beta scaling, trans='N'
  ! y = alpha * A * x + beta * y
  ! alpha = (2,1), beta = (1,1)
  a = (0.0d0, 0.0d0)
  x = (0.0d0, 0.0d0)
  y = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0)
  a(2) = (0.0d0, 1.0d0)
  a(3) = (2.0d0, 0.0d0)
  a(4) = (0.0d0, 2.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (1.0d0, 0.0d0)
  y(1) = (1.0d0, 0.0d0)
  y(2) = (0.0d0, 1.0d0)
  alpha = (2.0d0, 1.0d0)
  beta = (1.0d0, 1.0d0)
  call zgemv('N', 2, 2, alpha, a, 2, x, 1, beta, y, 1)
  call begin_test('zgemv_alpha_beta')
  call print_array('y', y_r, 4)
  call end_test()

  ! Test 4: m=0, n=0 — quick return
  y(1) = (99.0d0, 88.0d0)
  alpha = (1.0d0, 0.0d0)
  beta = (0.0d0, 0.0d0)
  call zgemv('N', 0, 0, alpha, a, 1, x, 1, beta, y, 1)
  call begin_test('zgemv_zero_dim')
  call print_array('y', y_r, 2)
  call end_test()

  ! Test 5: trans='T' (transpose, no conjugate)
  ! A^T * x (no conjugation)
  a = (0.0d0, 0.0d0)
  x = (0.0d0, 0.0d0)
  y = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 1.0d0)
  a(2) = (2.0d0, 2.0d0)
  a(3) = (3.0d0, 3.0d0)
  a(4) = (4.0d0, 4.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (0.0d0, 1.0d0)
  alpha = (1.0d0, 0.0d0)
  beta = (0.0d0, 0.0d0)
  call zgemv('T', 2, 2, alpha, a, 2, x, 1, beta, y, 1)
  call begin_test('zgemv_trans')
  call print_array('y', y_r, 4)
  call end_test()

  ! Test 6: non-unit stride, trans='N', m=2, n=2, incx=2, incy=2
  a = (0.0d0, 0.0d0)
  x = (0.0d0, 0.0d0)
  y = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0)
  a(2) = (2.0d0, 0.0d0)
  a(3) = (3.0d0, 0.0d0)
  a(4) = (4.0d0, 0.0d0)
  x(1) = (1.0d0, 1.0d0)
  x(2) = (99.0d0, 99.0d0)
  x(3) = (2.0d0, 2.0d0)
  y(1) = (0.0d0, 0.0d0)
  y(2) = (88.0d0, 88.0d0)
  y(3) = (0.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  beta = (0.0d0, 0.0d0)
  call zgemv('N', 2, 2, alpha, a, 2, x, 2, beta, y, 2)
  call begin_test('zgemv_stride')
  call print_array('y', y_r, 6)
  call end_test()

end program
