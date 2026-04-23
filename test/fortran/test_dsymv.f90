program test_dsymv
  use test_utils
  implicit none
  double precision :: a(25), x(20), y(20)

  ! Test 1: uplo='U', basic — N=4, alpha=1, beta=0, incx=1, incy=1
  ! Symmetric matrix (upper triangle stored):
  !   A = [ 1  2  3  4 ]
  !       [ .  5  6  7 ]
  !       [ .  .  8  9 ]
  !       [ .  .  . 10 ]
  ! Full symmetric:
  !   [ 1  2  3  4 ]
  !   [ 2  5  6  7 ]
  !   [ 3  6  8  9 ]
  !   [ 4  7  9 10 ]
  ! x = [1, 2, 3, 4]
  ! y = A*x = [1+4+9+16, 2+10+18+28, 3+12+24+36, 4+14+27+40] = [30, 58, 75, 85]
  a = 0.0d0; x = 0.0d0; y = 0.0d0
  a(1) = 1.0d0; a(2) = 0.0d0; a(3) = 0.0d0; a(4) = 0.0d0
  a(5) = 2.0d0; a(6) = 5.0d0; a(7) = 0.0d0; a(8) = 0.0d0
  a(9) = 3.0d0; a(10) = 6.0d0; a(11) = 8.0d0; a(12) = 0.0d0
  a(13) = 4.0d0; a(14) = 7.0d0; a(15) = 9.0d0; a(16) = 10.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0; x(4) = 4.0d0
  call dsymv('U', 4, 1.0d0, a, 4, x, 1, 0.0d0, y, 1)
  call begin_test('upper_basic')
  call print_array('y', y, 4)
  call end_test()

  ! Test 2: uplo='L', basic — same symmetric matrix, lower triangle stored
  ! Column-major lower triangle:
  ! col1: [1, 2, 3, 4], col2: [_, 5, 6, 7], col3: [_, _, 8, 9], col4: [_, _, _, 10]
  a = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0; a(4) = 4.0d0
  a(5) = 0.0d0; a(6) = 5.0d0; a(7) = 6.0d0; a(8) = 7.0d0
  a(9) = 0.0d0; a(10) = 0.0d0; a(11) = 8.0d0; a(12) = 9.0d0
  a(13) = 0.0d0; a(14) = 0.0d0; a(15) = 0.0d0; a(16) = 10.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0; x(4) = 4.0d0
  y = 0.0d0
  call dsymv('L', 4, 1.0d0, a, 4, x, 1, 0.0d0, y, 1)
  call begin_test('lower_basic')
  call print_array('y', y, 4)
  call end_test()

  ! Test 3: alpha=2, beta=0.5
  ! y_new = 2*A*x + 0.5*y_initial
  a = 0.0d0
  a(1) = 1.0d0; a(5) = 2.0d0; a(9) = 3.0d0; a(13) = 4.0d0
  a(6) = 5.0d0; a(10) = 6.0d0; a(14) = 7.0d0
  a(11) = 8.0d0; a(15) = 9.0d0
  a(16) = 10.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0; x(4) = 4.0d0
  y(1) = 10.0d0; y(2) = 20.0d0; y(3) = 30.0d0; y(4) = 40.0d0
  call dsymv('U', 4, 2.0d0, a, 4, x, 1, 0.5d0, y, 1)
  call begin_test('alpha_beta')
  call print_array('y', y, 4)
  call end_test()

  ! Test 4: N=0 quick return
  y(1) = 99.0d0
  call dsymv('U', 0, 1.0d0, a, 1, x, 1, 0.0d0, y, 1)
  call begin_test('n_zero')
  call print_array('y', y, 1)
  call end_test()

  ! Test 5: N=1
  a = 0.0d0; x = 0.0d0; y = 0.0d0
  a(1) = 3.0d0
  x(1) = 5.0d0
  y(1) = 7.0d0
  call dsymv('U', 1, 2.0d0, a, 1, x, 1, 3.0d0, y, 1)
  call begin_test('n_one')
  call print_array('y', y, 1)
  call end_test()

  ! Test 6: alpha=0, should just scale y by beta
  y(1) = 10.0d0; y(2) = 20.0d0; y(3) = 30.0d0; y(4) = 40.0d0
  call dsymv('U', 4, 0.0d0, a, 4, x, 1, 2.0d0, y, 1)
  call begin_test('alpha_zero')
  call print_array('y', y, 4)
  call end_test()

  ! Test 7: non-unit strides — incx=2, incy=2, uplo='U'
  ! Same symmetric matrix upper triangle
  a = 0.0d0
  a(1) = 1.0d0; a(5) = 2.0d0; a(9) = 3.0d0
  a(6) = 4.0d0; a(10) = 5.0d0
  a(11) = 6.0d0
  ! N=3, x at stride 2: x(1)=1, x(3)=2, x(5)=3
  ! y at stride 2: y(1), y(3), y(5)
  x = 0.0d0; y = 0.0d0
  x(1) = 1.0d0; x(3) = 2.0d0; x(5) = 3.0d0
  y(1) = 1.0d0; y(3) = 2.0d0; y(5) = 3.0d0
  call dsymv('U', 3, 1.0d0, a, 3, x, 2, 1.0d0, y, 2)
  call begin_test('stride')
  call print_array('y', y, 6)
  call end_test()

  ! Test 8: uplo='L' with non-unit strides and alpha=2, beta=0.5
  a = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0
  a(5) = 0.0d0; a(6) = 4.0d0; a(7) = 5.0d0
  a(9) = 0.0d0; a(10) = 0.0d0; a(11) = 6.0d0
  x = 0.0d0; y = 0.0d0
  x(1) = 1.0d0; x(3) = 2.0d0; x(5) = 3.0d0
  y(1) = 10.0d0; y(3) = 20.0d0; y(5) = 30.0d0
  call dsymv('L', 3, 2.0d0, a, 3, x, 2, 0.5d0, y, 2)
  call begin_test('lower_stride_alpha_beta')
  call print_array('y', y, 6)
  call end_test()

  ! Test 9: negative stride — incx=-1, incy=-1, uplo='U'
  a = 0.0d0
  a(1) = 1.0d0; a(4) = 2.0d0; a(7) = 3.0d0
  a(5) = 4.0d0; a(8) = 5.0d0
  a(9) = 6.0d0
  x = 0.0d0; y = 0.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0
  y(1) = 0.0d0; y(2) = 0.0d0; y(3) = 0.0d0
  call dsymv('U', 3, 1.0d0, a, 3, x, -1, 0.0d0, y, -1)
  call begin_test('negative_stride')
  call print_array('y', y, 3)
  call end_test()

end program
