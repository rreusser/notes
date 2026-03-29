program test_dla_wwaddw
  use test_utils
  implicit none
  double precision :: x(10), y(10), w(10)
  integer :: n

  ! Test 1: basic, n=5
  n = 5
  x = 0.0d0
  y = 0.0d0
  w = 0.0d0
  x(1:5) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0/)
  y(1:5) = (/0.1d0, 0.2d0, 0.3d0, 0.4d0, 0.5d0/)
  w(1:5) = (/10.0d0, 20.0d0, 30.0d0, 40.0d0, 50.0d0/)
  call DLA_WWADDW(n, x, y, w)
  call begin_test('basic')
  call print_array('x', x, n)
  call print_array('y', y, n)
  call end_test()

  ! Test 2: n=0 (quick return, arrays unchanged)
  n = 0
  x(1:5) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0/)
  y(1:5) = (/0.1d0, 0.2d0, 0.3d0, 0.4d0, 0.5d0/)
  w(1:5) = (/10.0d0, 20.0d0, 30.0d0, 40.0d0, 50.0d0/)
  call DLA_WWADDW(n, x, y, w)
  call begin_test('n_zero')
  call print_array('x', x, 5)
  call print_array('y', y, 5)
  call end_test()

  ! Test 3: n=1
  n = 1
  x(1) = 1.0d0
  y(1) = 0.1d0
  w(1) = 10.0d0
  call DLA_WWADDW(n, x, y, w)
  call begin_test('n_one')
  call print_array('x', x, 1)
  call print_array('y', y, 1)
  call end_test()

  ! Test 4: negative values
  n = 4
  x(1:4) = (/-1.0d0, -2.0d0, -3.0d0, -4.0d0/)
  y(1:4) = (/0.01d0, -0.02d0, 0.03d0, -0.04d0/)
  w(1:4) = (/0.5d0, -0.5d0, 1.5d0, -1.5d0/)
  call DLA_WWADDW(n, x, y, w)
  call begin_test('negative')
  call print_array('x', x, n)
  call print_array('y', y, n)
  call end_test()

  ! Test 5: large values (test precision behavior)
  n = 3
  x(1:3) = (/1.0d15, 2.0d15, 3.0d15/)
  y(1:3) = (/1.0d0, 2.0d0, 3.0d0/)
  w(1:3) = (/1.0d0, 2.0d0, 3.0d0/)
  call DLA_WWADDW(n, x, y, w)
  call begin_test('large_values')
  call print_array('x', x, n)
  call print_array('y', y, n)
  call end_test()

  ! Test 6: zeros
  n = 3
  x(1:3) = (/0.0d0, 0.0d0, 0.0d0/)
  y(1:3) = (/0.0d0, 0.0d0, 0.0d0/)
  w(1:3) = (/0.0d0, 0.0d0, 0.0d0/)
  call DLA_WWADDW(n, x, y, w)
  call begin_test('zeros')
  call print_array('x', x, n)
  call print_array('y', y, n)
  call end_test()

end program
