program test_dsyr2
  use test_utils
  implicit none

  double precision :: A(25), x(10), y(10)

  ! Test 1: upper triangle, 3x3, alpha=1
  ! A = [1 2 3; 2 5 6; 3 6 9], x = [1, 2, 3], y = [4, 5, 6]
  ! A := 1*x*y' + 1*y*x' + A
  A = 0.0d0; x = 0.0d0; y = 0.0d0
  A(1) = 1.0d0; A(4) = 2.0d0; A(7) = 3.0d0
  A(2) = 2.0d0; A(5) = 5.0d0; A(8) = 6.0d0
  A(3) = 3.0d0; A(6) = 6.0d0; A(9) = 9.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0
  y(1) = 4.0d0; y(2) = 5.0d0; y(3) = 6.0d0
  call dsyr2('U', 3, 1.0d0, x, 1, y, 1, A, 3)
  call begin_test('upper_basic')
  call print_matrix('A', A, 3, 3, 3)
  call end_test()

  ! Test 2: lower triangle, 3x3, alpha=1
  A = 0.0d0
  A(1) = 1.0d0; A(4) = 2.0d0; A(7) = 3.0d0
  A(2) = 2.0d0; A(5) = 5.0d0; A(8) = 6.0d0
  A(3) = 3.0d0; A(6) = 6.0d0; A(9) = 9.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0
  y(1) = 4.0d0; y(2) = 5.0d0; y(3) = 6.0d0
  call dsyr2('L', 3, 1.0d0, x, 1, y, 1, A, 3)
  call begin_test('lower_basic')
  call print_matrix('A', A, 3, 3, 3)
  call end_test()

  ! Test 3: upper triangle, alpha=2.5
  A = 0.0d0
  A(1) = 1.0d0; A(4) = 2.0d0; A(7) = 3.0d0
  A(2) = 0.0d0; A(5) = 5.0d0; A(8) = 6.0d0
  A(3) = 0.0d0; A(6) = 0.0d0; A(9) = 9.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0
  y(1) = 0.5d0; y(2) = 1.5d0; y(3) = 2.5d0
  call dsyr2('U', 3, 2.5d0, x, 1, y, 1, A, 3)
  call begin_test('upper_alpha')
  call print_matrix('A', A, 3, 3, 3)
  call end_test()

  ! Test 4: lower triangle, alpha=0.5
  A = 0.0d0
  A(1) = 1.0d0; A(4) = 0.0d0; A(7) = 0.0d0
  A(2) = 2.0d0; A(5) = 5.0d0; A(8) = 0.0d0
  A(3) = 3.0d0; A(6) = 6.0d0; A(9) = 9.0d0
  x(1) = 2.0d0; x(2) = 3.0d0; x(3) = 4.0d0
  y(1) = 1.0d0; y(2) = -1.0d0; y(3) = 2.0d0
  call dsyr2('L', 3, 0.5d0, x, 1, y, 1, A, 3)
  call begin_test('lower_alpha')
  call print_matrix('A', A, 3, 3, 3)
  call end_test()

  ! Test 5: N=0 quick return
  A = 0.0d0; A(1) = 99.0d0
  call dsyr2('U', 0, 1.0d0, x, 1, y, 1, A, 1)
  call begin_test('n_zero')
  call print_array('A', A, 1)
  call end_test()

  ! Test 6: alpha=0 quick return
  A = 0.0d0; A(1) = 99.0d0
  call dsyr2('U', 3, 0.0d0, x, 1, y, 1, A, 3)
  call begin_test('alpha_zero')
  call print_array('A', A, 1)
  call end_test()

  ! Test 7: N=1
  A = 0.0d0; A(1) = 5.0d0
  x(1) = 3.0d0; y(1) = 2.0d0
  call dsyr2('U', 1, 1.0d0, x, 1, y, 1, A, 1)
  call begin_test('n_one')
  call print_array('A', A, 1)
  call end_test()

  ! Test 8: upper, non-unit stride incx=2, incy=2
  A = 0.0d0
  A(1) = 1.0d0; A(4) = 2.0d0; A(7) = 3.0d0
  A(5) = 5.0d0; A(8) = 6.0d0
  A(9) = 9.0d0
  x = 0.0d0; y = 0.0d0
  x(1) = 1.0d0; x(3) = 2.0d0; x(5) = 3.0d0
  y(1) = 4.0d0; y(3) = 5.0d0; y(5) = 6.0d0
  call dsyr2('U', 3, 1.0d0, x, 2, y, 2, A, 3)
  call begin_test('upper_stride')
  call print_matrix('A', A, 3, 3, 3)
  call end_test()

  ! Test 9: lower, non-unit stride incx=2, incy=3
  A = 0.0d0
  A(1) = 1.0d0
  A(2) = 2.0d0; A(5) = 5.0d0
  A(3) = 3.0d0; A(6) = 6.0d0; A(9) = 9.0d0
  x = 0.0d0; y = 0.0d0
  x(1) = 1.0d0; x(3) = 2.0d0; x(5) = 3.0d0
  y(1) = 4.0d0; y(4) = 5.0d0; y(7) = 6.0d0
  call dsyr2('L', 3, 1.0d0, x, 2, y, 3, A, 3)
  call begin_test('lower_stride')
  call print_matrix('A', A, 3, 3, 3)
  call end_test()

  ! Test 10: upper, 4x4 matrix
  A = 0.0d0
  A(1)  = 1.0d0;  A(5)  = 2.0d0;  A(9)  = 3.0d0;  A(13) = 4.0d0
                   A(6)  = 5.0d0;  A(10) = 6.0d0;  A(14) = 7.0d0
                                    A(11) = 8.0d0;  A(15) = 9.0d0
                                                     A(16) = 10.0d0
  x = 0.0d0; y = 0.0d0
  x(1) = 1.0d0; x(2) = -1.0d0; x(3) = 2.0d0; x(4) = -2.0d0
  y(1) = 3.0d0; y(2) = 0.5d0; y(3) = -1.0d0; y(4) = 1.5d0
  call dsyr2('U', 4, 1.0d0, x, 1, y, 1, A, 4)
  call begin_test('upper_4x4')
  call print_matrix('A', A, 4, 4, 4)
  call end_test()

  ! Test 11: lower, 4x4 matrix
  A = 0.0d0
  A(1)  = 1.0d0
  A(2)  = 2.0d0;  A(6)  = 5.0d0
  A(3)  = 3.0d0;  A(7)  = 6.0d0;  A(11) = 8.0d0
  A(4)  = 4.0d0;  A(8)  = 7.0d0;  A(12) = 9.0d0;  A(16) = 10.0d0
  x = 0.0d0; y = 0.0d0
  x(1) = 1.0d0; x(2) = -1.0d0; x(3) = 2.0d0; x(4) = -2.0d0
  y(1) = 3.0d0; y(2) = 0.5d0; y(3) = -1.0d0; y(4) = 1.5d0
  call dsyr2('L', 4, 1.0d0, x, 1, y, 1, A, 4)
  call begin_test('lower_4x4')
  call print_matrix('A', A, 4, 4, 4)
  call end_test()

  ! Test 12: x has zero elements (tests skip branch)
  A = 0.0d0
  A(1) = 1.0d0; A(4) = 2.0d0; A(7) = 3.0d0
  A(5) = 5.0d0; A(8) = 6.0d0
  A(9) = 9.0d0
  x = 0.0d0; y = 0.0d0
  x(1) = 0.0d0; x(2) = 2.0d0; x(3) = 0.0d0
  y(1) = 0.0d0; y(2) = 5.0d0; y(3) = 0.0d0
  call dsyr2('U', 3, 1.0d0, x, 1, y, 1, A, 3)
  call begin_test('upper_zeros')
  call print_matrix('A', A, 3, 3, 3)
  call end_test()

end program
