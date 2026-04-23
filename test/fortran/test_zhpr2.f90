program test_zhpr2
  use test_utils
  implicit none
  complex*16 :: ap(15), x(8), y(8)
  double precision :: ap_r(30), x_r(16), y_r(16)
  equivalence (ap, ap_r)
  equivalence (x, x_r)
  equivalence (y, y_r)

  ! Test 1: UPLO='U', N=3
  ap = (0.0d0, 0.0d0)
  ap(1) = (2.0d0, 0.0d0); ap(2) = (1.0d0, 1.0d0); ap(3) = (4.0d0, 0.0d0)
  ap(4) = (3.0d0, -2.0d0); ap(5) = (2.0d0, 1.0d0); ap(6) = (5.0d0, 0.0d0)
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.5d0); x(2) = (2.0d0, -1.0d0); x(3) = (3.0d0, 1.0d0)
  y = (0.0d0, 0.0d0)
  y(1) = (0.5d0, 1.0d0); y(2) = (1.5d0, -0.5d0); y(3) = (2.5d0, 0.0d0)
  call zhpr2('U', 3, (1.0d0,0.0d0), x, 1, y, 1, ap)
  call begin_test('upper_basic')
  call print_array('AP', ap_r, 12)
  call end_test()

  ! Test 2: UPLO='L', N=3
  ap = (0.0d0, 0.0d0)
  ap(1) = (2.0d0, 0.0d0); ap(2) = (1.0d0, -1.0d0); ap(3) = (3.0d0, 2.0d0)
  ap(4) = (4.0d0, 0.0d0); ap(5) = (2.0d0, -1.0d0); ap(6) = (5.0d0, 0.0d0)
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.5d0); x(2) = (2.0d0, -1.0d0); x(3) = (3.0d0, 1.0d0)
  y = (0.0d0, 0.0d0)
  y(1) = (0.5d0, 1.0d0); y(2) = (1.5d0, -0.5d0); y(3) = (2.5d0, 0.0d0)
  call zhpr2('L', 3, (1.0d0,0.0d0), x, 1, y, 1, ap)
  call begin_test('lower_basic')
  call print_array('AP', ap_r, 12)
  call end_test()

  ! Test 3: complex alpha=(2,1)
  ap = (0.0d0, 0.0d0)
  ap(1) = (2.0d0, 0.0d0); ap(2) = (1.0d0, 1.0d0); ap(3) = (4.0d0, 0.0d0)
  ap(4) = (3.0d0, -2.0d0); ap(5) = (2.0d0, 1.0d0); ap(6) = (5.0d0, 0.0d0)
  call zhpr2('U', 3, (2.0d0,1.0d0), x, 1, y, 1, ap)
  call begin_test('complex_alpha')
  call print_array('AP', ap_r, 12)
  call end_test()

  ! Test 4: alpha=0 (no-op)
  ap = (0.0d0, 0.0d0)
  ap(1) = (2.0d0, 0.0d0); ap(2) = (1.0d0, 1.0d0); ap(3) = (4.0d0, 0.0d0)
  call zhpr2('U', 3, (0.0d0,0.0d0), x, 1, y, 1, ap)
  call begin_test('alpha_zero')
  call print_array('AP', ap_r, 6)
  call end_test()

  ! Test 5: N=0 quick return
  ap(1) = (99.0d0, 0.0d0)
  call zhpr2('U', 0, (1.0d0,0.0d0), x, 1, y, 1, ap)
  call begin_test('n_zero')
  call print_array('AP', ap_r, 2)
  call end_test()

  ! Test 6: N=1 scalar
  ap = (0.0d0, 0.0d0); ap(1) = (3.0d0, 0.0d0)
  x = (0.0d0, 0.0d0); x(1) = (2.0d0, 1.0d0)
  y = (0.0d0, 0.0d0); y(1) = (1.0d0, -0.5d0)
  call zhpr2('U', 1, (1.0d0,0.5d0), x, 1, y, 1, ap)
  call begin_test('scalar')
  call print_array('AP', ap_r, 2)
  call end_test()

  ! Test 7: non-unit strides incx=2, incy=2
  ap = (0.0d0, 0.0d0)
  ap(1) = (2.0d0, 0.0d0); ap(2) = (1.0d0, 1.0d0); ap(3) = (4.0d0, 0.0d0)
  ap(4) = (3.0d0, -2.0d0); ap(5) = (2.0d0, 1.0d0); ap(6) = (5.0d0, 0.0d0)
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.5d0); x(3) = (2.0d0, -1.0d0); x(5) = (3.0d0, 1.0d0)
  y = (0.0d0, 0.0d0)
  y(1) = (0.5d0, 1.0d0); y(3) = (1.5d0, -0.5d0); y(5) = (2.5d0, 0.0d0)
  call zhpr2('U', 3, (1.0d0,0.0d0), x, 2, y, 2, ap)
  call begin_test('stride_2')
  call print_array('AP', ap_r, 12)
  call end_test()

  ! Test 8: x or y has zero elements
  ap = (0.0d0, 0.0d0)
  ap(1) = (2.0d0, 0.0d0); ap(2) = (1.0d0, 1.0d0); ap(3) = (4.0d0, 0.0d0)
  ap(4) = (3.0d0, -2.0d0); ap(5) = (2.0d0, 1.0d0); ap(6) = (5.0d0, 0.0d0)
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.5d0); x(2) = (0.0d0, 0.0d0); x(3) = (3.0d0, 1.0d0)
  y = (0.0d0, 0.0d0)
  y(1) = (0.5d0, 1.0d0); y(2) = (0.0d0, 0.0d0); y(3) = (2.5d0, 0.0d0)
  call zhpr2('U', 3, (1.0d0,0.0d0), x, 1, y, 1, ap)
  call begin_test('zero_elements')
  call print_array('AP', ap_r, 12)
  call end_test()

  ! Test 9: lower with non-unit stride
  ap = (0.0d0, 0.0d0)
  ap(1) = (2.0d0, 0.0d0); ap(2) = (1.0d0, -1.0d0); ap(3) = (3.0d0, 2.0d0)
  ap(4) = (4.0d0, 0.0d0); ap(5) = (2.0d0, -1.0d0); ap(6) = (5.0d0, 0.0d0)
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.5d0); x(3) = (2.0d0, -1.0d0); x(5) = (3.0d0, 1.0d0)
  y = (0.0d0, 0.0d0)
  y(1) = (0.5d0, 1.0d0); y(3) = (1.5d0, -0.5d0); y(5) = (2.5d0, 0.0d0)
  call zhpr2('L', 3, (1.0d0,0.0d0), x, 2, y, 2, ap)
  call begin_test('lower_stride_2')
  call print_array('AP', ap_r, 12)
  call end_test()

end program
