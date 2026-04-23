program test_zlacgv
  use test_utils
  implicit none

  complex*16 :: x(10)
  double precision :: x_real(20)
  equivalence (x, x_real)

  ! Test 1: basic conjugation, n=3, incx=1
  x(1) = (1.0d0, 2.0d0)
  x(2) = (3.0d0, -4.0d0)
  x(3) = (5.0d0, 0.0d0)
  call zlacgv(3, x, 1)
  call begin_test('zlacgv_basic')
  call print_array('x', x_real, 6)
  call end_test()

  ! Test 2: n=0 (no-op)
  x(1) = (1.0d0, 2.0d0)
  x(2) = (3.0d0, 4.0d0)
  call zlacgv(0, x, 1)
  call begin_test('zlacgv_n_zero')
  call print_array('x', x_real, 4)
  call end_test()

  ! Test 3: n=1
  x(1) = (7.0d0, -3.0d0)
  call zlacgv(1, x, 1)
  call begin_test('zlacgv_n_one')
  call print_array('x', x_real, 2)
  call end_test()

  ! Test 4: non-unit stride, incx=2
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 2.0d0)
  x(2) = (99.0d0, 99.0d0)
  x(3) = (3.0d0, 4.0d0)
  x(4) = (99.0d0, 99.0d0)
  x(5) = (5.0d0, 6.0d0)
  call zlacgv(3, x, 2)
  call begin_test('zlacgv_stride2')
  call print_array('x', x_real, 10)
  call end_test()

  ! Test 5: negative stride, incx=-1
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 2.0d0)
  x(2) = (3.0d0, 4.0d0)
  x(3) = (5.0d0, 6.0d0)
  call zlacgv(3, x, -1)
  call begin_test('zlacgv_neg_stride')
  call print_array('x', x_real, 6)
  call end_test()

  ! Test 6: all zeros
  x(1) = (0.0d0, 0.0d0)
  x(2) = (0.0d0, 0.0d0)
  call zlacgv(2, x, 1)
  call begin_test('zlacgv_zeros')
  call print_array('x', x_real, 4)
  call end_test()

  ! Test 7: pure imaginary
  x(1) = (0.0d0, 5.0d0)
  x(2) = (0.0d0, -3.0d0)
  call zlacgv(2, x, 1)
  call begin_test('zlacgv_pure_imag')
  call print_array('x', x_real, 4)
  call end_test()

end program
