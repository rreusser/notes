program test_dswap
  use test_utils
  implicit none

  integer, parameter :: MAXN = 20
  double precision :: x(MAXN), y(MAXN)

  ! Test 1: basic swap, N=5, incx=1, incy=1
  x = 0.0d0; y = 0.0d0
  x(1:5) = (/ 1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0 /)
  y(1:5) = (/ 6.0d0, 7.0d0, 8.0d0, 9.0d0, 10.0d0 /)

  call dswap(5, x, 1, y, 1)
  call begin_test('basic')
  call print_array('x', x, 5)
  call print_array('y', y, 5)
  call end_test()

  ! Test 2: negative stride, N=3, incx=2, incy=-1
  x = 0.0d0; y = 0.0d0
  x(1) = 1.0d0; x(3) = 2.0d0; x(5) = 3.0d0
  y(1:3) = (/ 4.0d0, 5.0d0, 6.0d0 /)

  call dswap(3, x, 2, y, -1)
  call begin_test('negative_stride')
  call print_array('x', x, 5)
  call print_array('y', y, 3)
  call end_test()

  ! Test 3: N=0 (quick return, vectors unchanged)
  x = 0.0d0; y = 0.0d0
  x(1:3) = (/ 1.0d0, 2.0d0, 3.0d0 /)
  y(1:3) = (/ 4.0d0, 5.0d0, 6.0d0 /)

  call dswap(0, x, 1, y, 1)
  call begin_test('n_zero')
  call print_array('x', x, 3)
  call print_array('y', y, 3)
  call end_test()

  ! Test 4: N=1
  x = 0.0d0; y = 0.0d0
  x(1) = 42.0d0
  y(1) = 99.0d0

  call dswap(1, x, 1, y, 1)
  call begin_test('n_one')
  call print_array('x', x, 1)
  call print_array('y', y, 1)
  call end_test()

end program
