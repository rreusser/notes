program test_zhpmv
  use test_utils
  implicit none
  complex*16 :: ap(15), x(8), y(8)
  double precision :: ap_r(30), x_r(16), y_r(16)
  equivalence (ap, ap_r)
  equivalence (x, x_r)
  equivalence (y, y_r)

  ! 3x3 Hermitian matrix:
  ! A = [2    1+i   3-2i]
  !     [1-i  4     2+i ]
  !     [3+2i 2-i   5   ]
  ! Packed upper: A(1,1)=2, A(1,2)=1+i, A(2,2)=4, A(1,3)=3-2i, A(2,3)=2+i, A(3,3)=5

  ! Test 1: UPLO='U', basic
  ap = (0.0d0, 0.0d0)
  ap(1) = (2.0d0, 0.0d0)
  ap(2) = (1.0d0, 1.0d0)
  ap(3) = (4.0d0, 0.0d0)
  ap(4) = (3.0d0, -2.0d0)
  ap(5) = (2.0d0, 1.0d0)
  ap(6) = (5.0d0, 0.0d0)

  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.5d0)
  x(2) = (2.0d0, -1.0d0)
  x(3) = (3.0d0, 1.0d0)

  y = (0.0d0, 0.0d0)
  call zhpmv('U', 3, (1.0d0,0.0d0), ap, x, 1, (0.0d0,0.0d0), y, 1)
  call begin_test('upper_basic')
  call print_array('y', y_r, 6)
  call end_test()

  ! Test 2: UPLO='L', same Hermitian matrix stored in lower
  ! Packed lower: A(1,1)=2, A(2,1)=1-i, A(3,1)=3+2i, A(2,2)=4, A(3,2)=2-i, A(3,3)=5
  ap = (0.0d0, 0.0d0)
  ap(1) = (2.0d0, 0.0d0)
  ap(2) = (1.0d0, -1.0d0)
  ap(3) = (3.0d0, 2.0d0)
  ap(4) = (4.0d0, 0.0d0)
  ap(5) = (2.0d0, -1.0d0)
  ap(6) = (5.0d0, 0.0d0)

  y = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.5d0)
  x(2) = (2.0d0, -1.0d0)
  x(3) = (3.0d0, 1.0d0)
  call zhpmv('L', 3, (1.0d0,0.0d0), ap, x, 1, (0.0d0,0.0d0), y, 1)
  call begin_test('lower_basic')
  call print_array('y', y_r, 6)
  call end_test()

  ! Test 3: complex alpha and beta
  y = (0.0d0, 0.0d0)
  y(1) = (1.0d0, 1.0d0)
  y(2) = (2.0d0, -1.0d0)
  y(3) = (0.5d0, 0.5d0)
  ! Use upper storage
  ap = (0.0d0, 0.0d0)
  ap(1) = (2.0d0, 0.0d0)
  ap(2) = (1.0d0, 1.0d0)
  ap(3) = (4.0d0, 0.0d0)
  ap(4) = (3.0d0, -2.0d0)
  ap(5) = (2.0d0, 1.0d0)
  ap(6) = (5.0d0, 0.0d0)
  x(1) = (1.0d0, 0.5d0)
  x(2) = (2.0d0, -1.0d0)
  x(3) = (3.0d0, 1.0d0)
  call zhpmv('U', 3, (2.0d0,1.0d0), ap, x, 1, (0.5d0,-0.5d0), y, 1)
  call begin_test('complex_alpha_beta')
  call print_array('y', y_r, 6)
  call end_test()

  ! Test 4: alpha=0, beta=(2,0)
  y = (0.0d0, 0.0d0)
  y(1) = (1.0d0, 2.0d0)
  y(2) = (3.0d0, 4.0d0)
  y(3) = (5.0d0, 6.0d0)
  call zhpmv('U', 3, (0.0d0,0.0d0), ap, x, 1, (2.0d0,0.0d0), y, 1)
  call begin_test('alpha_zero')
  call print_array('y', y_r, 6)
  call end_test()

  ! Test 5: N=0 quick return
  y(1) = (99.0d0, 0.0d0)
  call zhpmv('U', 0, (1.0d0,0.0d0), ap, x, 1, (0.0d0,0.0d0), y, 1)
  call begin_test('n_zero')
  call print_array('y', y_r, 2)
  call end_test()

  ! Test 6: alpha=0, beta=0 -- zero y
  y = (0.0d0, 0.0d0)
  y(1) = (99.0d0, 88.0d0)
  y(2) = (77.0d0, 66.0d0)
  y(3) = (55.0d0, 44.0d0)
  call zhpmv('U', 3, (0.0d0,0.0d0), ap, x, 1, (0.0d0,0.0d0), y, 1)
  call begin_test('alpha_zero_beta_zero')
  call print_array('y', y_r, 6)
  call end_test()

  ! Test 7: non-unit strides incx=2, incy=2
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.5d0)
  x(3) = (2.0d0, -1.0d0)
  x(5) = (3.0d0, 1.0d0)
  y = (0.0d0, 0.0d0)
  call zhpmv('U', 3, (1.0d0,0.0d0), ap, x, 2, (0.0d0,0.0d0), y, 2)
  call begin_test('stride_2')
  call print_array('y', y_r, 12)
  call end_test()

  ! Test 8: 1x1 scalar
  ap = (0.0d0, 0.0d0)
  x = (0.0d0, 0.0d0)
  y = (0.0d0, 0.0d0)
  ap(1) = (3.0d0, 0.0d0)
  x(1) = (5.0d0, 2.0d0)
  call zhpmv('U', 1, (2.0d0,1.0d0), ap, x, 1, (0.0d0,0.0d0), y, 1)
  call begin_test('scalar')
  call print_array('y', y_r, 2)
  call end_test()

  ! Test 9: lower with nonzero beta
  ap = (0.0d0, 0.0d0)
  ap(1) = (2.0d0, 0.0d0)
  ap(2) = (1.0d0, -1.0d0)
  ap(3) = (3.0d0, 2.0d0)
  ap(4) = (4.0d0, 0.0d0)
  ap(5) = (2.0d0, -1.0d0)
  ap(6) = (5.0d0, 0.0d0)
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.5d0)
  x(2) = (2.0d0, -1.0d0)
  x(3) = (3.0d0, 1.0d0)
  y = (0.0d0, 0.0d0)
  y(1) = (1.0d0, 1.0d0)
  y(2) = (2.0d0, -1.0d0)
  y(3) = (0.5d0, 0.5d0)
  call zhpmv('L', 3, (1.0d0,0.0d0), ap, x, 1, (0.5d0,0.0d0), y, 1)
  call begin_test('lower_nonzero_beta')
  call print_array('y', y_r, 6)
  call end_test()

end program
