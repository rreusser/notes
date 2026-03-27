program test_zhpr
  use test_utils
  implicit none
  complex*16 :: ap(15), x(8)
  double precision :: ap_r(30), x_r(16)
  equivalence (ap, ap_r)
  equivalence (x, x_r)

  ! Test 1: UPLO='U', N=3
  ! A packed upper: [2, 1+i, 4, 3-2i, 2+i, 5]
  ap = (0.0d0, 0.0d0)
  ap(1) = (2.0d0, 0.0d0); ap(2) = (1.0d0, 1.0d0); ap(3) = (4.0d0, 0.0d0)
  ap(4) = (3.0d0, -2.0d0); ap(5) = (2.0d0, 1.0d0); ap(6) = (5.0d0, 0.0d0)
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.5d0); x(2) = (2.0d0, -1.0d0); x(3) = (3.0d0, 1.0d0)
  call zhpr('U', 3, 2.0d0, x, 1, ap)
  call begin_test('upper_basic')
  call print_array('AP', ap_r, 12)
  call end_test()

  ! Test 2: UPLO='L', N=3
  ap = (0.0d0, 0.0d0)
  ap(1) = (2.0d0, 0.0d0); ap(2) = (1.0d0, -1.0d0); ap(3) = (3.0d0, 2.0d0)
  ap(4) = (4.0d0, 0.0d0); ap(5) = (2.0d0, -1.0d0); ap(6) = (5.0d0, 0.0d0)
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.5d0); x(2) = (2.0d0, -1.0d0); x(3) = (3.0d0, 1.0d0)
  call zhpr('L', 3, 2.0d0, x, 1, ap)
  call begin_test('lower_basic')
  call print_array('AP', ap_r, 12)
  call end_test()

  ! Test 3: alpha=0 (no-op, diagonal stays real)
  ap = (0.0d0, 0.0d0)
  ap(1) = (2.0d0, 0.0d0); ap(2) = (1.0d0, 1.0d0); ap(3) = (4.0d0, 0.0d0)
  ap(4) = (3.0d0, -2.0d0); ap(5) = (2.0d0, 1.0d0); ap(6) = (5.0d0, 0.0d0)
  call zhpr('U', 3, 0.0d0, x, 1, ap)
  call begin_test('alpha_zero')
  call print_array('AP', ap_r, 12)
  call end_test()

  ! Test 4: N=0 quick return
  ap(1) = (99.0d0, 0.0d0)
  call zhpr('U', 0, 1.0d0, x, 1, ap)
  call begin_test('n_zero')
  call print_array('AP', ap_r, 2)
  call end_test()

  ! Test 5: N=1 scalar case
  ap = (0.0d0, 0.0d0)
  ap(1) = (3.0d0, 0.0d0)
  x = (0.0d0, 0.0d0)
  x(1) = (2.0d0, 1.0d0)
  call zhpr('U', 1, 1.5d0, x, 1, ap)
  call begin_test('scalar')
  call print_array('AP', ap_r, 2)
  call end_test()

  ! Test 6: non-unit stride incx=2
  ap = (0.0d0, 0.0d0)
  ap(1) = (2.0d0, 0.0d0); ap(2) = (1.0d0, 1.0d0); ap(3) = (4.0d0, 0.0d0)
  ap(4) = (3.0d0, -2.0d0); ap(5) = (2.0d0, 1.0d0); ap(6) = (5.0d0, 0.0d0)
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.5d0); x(3) = (2.0d0, -1.0d0); x(5) = (3.0d0, 1.0d0)
  call zhpr('U', 3, 2.0d0, x, 2, ap)
  call begin_test('upper_stride_2')
  call print_array('AP', ap_r, 12)
  call end_test()

  ! Test 7: x has zero element (exercises branch where X(J).NE.ZERO skips)
  ap = (0.0d0, 0.0d0)
  ap(1) = (2.0d0, 0.0d0); ap(2) = (1.0d0, 1.0d0); ap(3) = (4.0d0, 0.0d0)
  ap(4) = (3.0d0, -2.0d0); ap(5) = (2.0d0, 1.0d0); ap(6) = (5.0d0, 0.0d0)
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.5d0); x(2) = (0.0d0, 0.0d0); x(3) = (3.0d0, 1.0d0)
  call zhpr('U', 3, 2.0d0, x, 1, ap)
  call begin_test('zero_element')
  call print_array('AP', ap_r, 12)
  call end_test()

  ! Test 8: lower with non-unit stride
  ap = (0.0d0, 0.0d0)
  ap(1) = (2.0d0, 0.0d0); ap(2) = (1.0d0, -1.0d0); ap(3) = (3.0d0, 2.0d0)
  ap(4) = (4.0d0, 0.0d0); ap(5) = (2.0d0, -1.0d0); ap(6) = (5.0d0, 0.0d0)
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.5d0); x(3) = (2.0d0, -1.0d0); x(5) = (3.0d0, 1.0d0)
  call zhpr('L', 3, 2.0d0, x, 2, ap)
  call begin_test('lower_stride_2')
  call print_array('AP', ap_r, 12)
  call end_test()

end program
