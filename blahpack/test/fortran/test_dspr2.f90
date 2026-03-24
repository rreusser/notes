program test_dspr2
  use test_utils
  implicit none

  double precision :: AP(15), x(10), y(10)

  ! Test 1: upper triangle, 3x3, alpha=1, AP starts with known values
  AP = 0.0d0; x = 0.0d0; y = 0.0d0
  AP(1) = 1.0d0; AP(2) = 2.0d0; AP(3) = 5.0d0
  AP(4) = 3.0d0; AP(5) = 6.0d0; AP(6) = 9.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0
  y(1) = 4.0d0; y(2) = 5.0d0; y(3) = 6.0d0
  call dspr2('U', 3, 1.0d0, x, 1, y, 1, AP)
  call begin_test('upper_basic')
  call print_array('AP', AP, 6)
  call end_test()

  ! Test 2: lower triangle, 3x3, alpha=1
  AP = 0.0d0
  AP(1) = 1.0d0; AP(2) = 2.0d0; AP(3) = 3.0d0
  AP(4) = 5.0d0; AP(5) = 6.0d0; AP(6) = 9.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0
  y(1) = 4.0d0; y(2) = 5.0d0; y(3) = 6.0d0
  call dspr2('L', 3, 1.0d0, x, 1, y, 1, AP)
  call begin_test('lower_basic')
  call print_array('AP', AP, 6)
  call end_test()

  ! Test 3: upper triangle, alpha=2.5
  AP = 0.0d0
  AP(1) = 1.0d0; AP(2) = 2.0d0; AP(3) = 5.0d0
  AP(4) = 3.0d0; AP(5) = 6.0d0; AP(6) = 9.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0
  y(1) = 0.5d0; y(2) = 1.5d0; y(3) = 2.5d0
  call dspr2('U', 3, 2.5d0, x, 1, y, 1, AP)
  call begin_test('upper_alpha')
  call print_array('AP', AP, 6)
  call end_test()

  ! Test 4: lower triangle, alpha=0.5
  AP = 0.0d0
  AP(1) = 1.0d0; AP(2) = 2.0d0; AP(3) = 3.0d0
  AP(4) = 5.0d0; AP(5) = 6.0d0; AP(6) = 9.0d0
  x(1) = 2.0d0; x(2) = 3.0d0; x(3) = 4.0d0
  y(1) = 1.0d0; y(2) = -1.0d0; y(3) = 2.0d0
  call dspr2('L', 3, 0.5d0, x, 1, y, 1, AP)
  call begin_test('lower_alpha')
  call print_array('AP', AP, 6)
  call end_test()

  ! Test 5: N=0 quick return
  AP = 0.0d0; AP(1) = 99.0d0
  call dspr2('U', 0, 1.0d0, x, 1, y, 1, AP)
  call begin_test('n_zero')
  call print_array('AP', AP, 1)
  call end_test()

  ! Test 6: alpha=0 quick return
  AP = 0.0d0; AP(1) = 99.0d0
  call dspr2('U', 3, 0.0d0, x, 1, y, 1, AP)
  call begin_test('alpha_zero')
  call print_array('AP', AP, 1)
  call end_test()

  ! Test 7: N=1
  AP = 0.0d0; AP(1) = 5.0d0
  x(1) = 3.0d0; y(1) = 2.0d0
  call dspr2('U', 1, 1.0d0, x, 1, y, 1, AP)
  call begin_test('n_one')
  call print_array('AP', AP, 1)
  call end_test()

  ! Test 8: upper, non-unit stride incx=2, incy=2
  AP = 0.0d0
  AP(1) = 1.0d0; AP(2) = 2.0d0; AP(3) = 5.0d0
  AP(4) = 3.0d0; AP(5) = 6.0d0; AP(6) = 9.0d0
  x = 0.0d0; y = 0.0d0
  x(1) = 1.0d0; x(3) = 2.0d0; x(5) = 3.0d0
  y(1) = 4.0d0; y(3) = 5.0d0; y(5) = 6.0d0
  call dspr2('U', 3, 1.0d0, x, 2, y, 2, AP)
  call begin_test('upper_stride')
  call print_array('AP', AP, 6)
  call end_test()

  ! Test 9: lower, non-unit stride incx=2, incy=3
  AP = 0.0d0
  AP(1) = 1.0d0; AP(2) = 2.0d0; AP(3) = 3.0d0
  AP(4) = 5.0d0; AP(5) = 6.0d0; AP(6) = 9.0d0
  x = 0.0d0; y = 0.0d0
  x(1) = 1.0d0; x(3) = 2.0d0; x(5) = 3.0d0
  y(1) = 4.0d0; y(4) = 5.0d0; y(7) = 6.0d0
  call dspr2('L', 3, 1.0d0, x, 2, y, 3, AP)
  call begin_test('lower_stride')
  call print_array('AP', AP, 6)
  call end_test()

  ! Test 10: upper, 4x4 matrix (10 packed elements)
  AP = 0.0d0
  AP(1) = 1.0d0; AP(2) = 2.0d0; AP(3) = 5.0d0; AP(4) = 3.0d0
  AP(5) = 6.0d0; AP(6) = 8.0d0; AP(7) = 4.0d0; AP(8) = 7.0d0
  AP(9) = 9.0d0; AP(10) = 10.0d0
  x = 0.0d0; y = 0.0d0
  x(1) = 1.0d0; x(2) = -1.0d0; x(3) = 2.0d0; x(4) = -2.0d0
  y(1) = 3.0d0; y(2) = 0.5d0; y(3) = -1.0d0; y(4) = 1.5d0
  call dspr2('U', 4, 1.0d0, x, 1, y, 1, AP)
  call begin_test('upper_4x4')
  call print_array('AP', AP, 10)
  call end_test()

  ! Test 11: lower, 4x4 matrix (10 packed elements)
  AP = 0.0d0
  AP(1) = 1.0d0; AP(2) = 2.0d0; AP(3) = 3.0d0; AP(4) = 4.0d0
  AP(5) = 5.0d0; AP(6) = 6.0d0; AP(7) = 7.0d0; AP(8) = 8.0d0
  AP(9) = 9.0d0; AP(10) = 10.0d0
  x = 0.0d0; y = 0.0d0
  x(1) = 1.0d0; x(2) = -1.0d0; x(3) = 2.0d0; x(4) = -2.0d0
  y(1) = 3.0d0; y(2) = 0.5d0; y(3) = -1.0d0; y(4) = 1.5d0
  call dspr2('L', 4, 1.0d0, x, 1, y, 1, AP)
  call begin_test('lower_4x4')
  call print_array('AP', AP, 10)
  call end_test()

  ! Test 12: upper with zero elements in x and y (tests skip branch)
  AP = 0.0d0
  AP(1) = 1.0d0; AP(2) = 2.0d0; AP(3) = 5.0d0
  AP(4) = 3.0d0; AP(5) = 6.0d0; AP(6) = 9.0d0
  x = 0.0d0; y = 0.0d0
  x(1) = 0.0d0; x(2) = 2.0d0; x(3) = 0.0d0
  y(1) = 0.0d0; y(2) = 5.0d0; y(3) = 0.0d0
  call dspr2('U', 3, 1.0d0, x, 1, y, 1, AP)
  call begin_test('upper_zeros')
  call print_array('AP', AP, 6)
  call end_test()

end program
