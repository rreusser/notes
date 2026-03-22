program test_drot
  use test_utils
  implicit none
  double precision :: dx(20), dy(20)
  double precision :: c, s
  double precision, parameter :: PI = 3.141592653589793d0

  ! Test 1: basic rotation with c=cos(pi/4), s=sin(pi/4)
  dx = 0.0d0; dy = 0.0d0
  dx(1:5) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0/)
  dy(1:5) = (/6.0d0, 7.0d0, 8.0d0, 9.0d0, 10.0d0/)
  c = cos(PI / 4.0d0)
  s = sin(PI / 4.0d0)
  call drot(5, dx, 1, dy, 1, c, s)
  call begin_test('basic')
  call print_array('dx', dx, 5)
  call print_array('dy', dy, 5)
  call end_test()

  ! Test 2: identity rotation (c=1, s=0) — should leave vectors unchanged
  dx(1:5) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0/)
  dy(1:5) = (/6.0d0, 7.0d0, 8.0d0, 9.0d0, 10.0d0/)
  call drot(5, dx, 1, dy, 1, 1.0d0, 0.0d0)
  call begin_test('identity')
  call print_array('dx', dx, 5)
  call print_array('dy', dy, 5)
  call end_test()

  ! Test 3: full swap rotation (c=0, s=1)
  dx(1:5) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0/)
  dy(1:5) = (/6.0d0, 7.0d0, 8.0d0, 9.0d0, 10.0d0/)
  call drot(5, dx, 1, dy, 1, 0.0d0, 1.0d0)
  call begin_test('swap')
  call print_array('dx', dx, 5)
  call print_array('dy', dy, 5)
  call end_test()

  ! Test 4: n=0 (no-op)
  dx(1:5) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0/)
  dy(1:5) = (/6.0d0, 7.0d0, 8.0d0, 9.0d0, 10.0d0/)
  call drot(0, dx, 1, dy, 1, c, s)
  call begin_test('n_zero')
  call print_array('dx', dx, 5)
  call print_array('dy', dy, 5)
  call end_test()

  ! Test 5: n=1
  dx(1) = 3.0d0
  dy(1) = 4.0d0
  call drot(1, dx, 1, dy, 1, 0.6d0, 0.8d0)
  call begin_test('n_one')
  call print_array('dx', dx, 1)
  call print_array('dy', dy, 1)
  call end_test()

  ! Test 6: non-unit strides (incx=2, incy=3)
  dx = 0.0d0; dy = 0.0d0
  dx(1) = 1.0d0; dx(3) = 2.0d0; dx(5) = 3.0d0
  dy(1) = 10.0d0; dy(4) = 20.0d0; dy(7) = 30.0d0
  call drot(3, dx, 2, dy, 3, 0.6d0, 0.8d0)
  call begin_test('stride')
  call print_array('dx', dx, 6)
  call print_array('dy', dy, 9)
  call end_test()

  ! Test 7: negative stride (incx=-1)
  dx = 0.0d0; dy = 0.0d0
  dx(1:4) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0/)
  dy(1:4) = (/10.0d0, 20.0d0, 30.0d0, 40.0d0/)
  call drot(4, dx, -1, dy, 1, 0.6d0, 0.8d0)
  call begin_test('neg_stride')
  call print_array('dx', dx, 4)
  call print_array('dy', dy, 4)
  call end_test()

  ! Test 8: 180-degree rotation (c=-1, s=0) — negates both vectors
  dx(1:3) = (/1.0d0, 2.0d0, 3.0d0/)
  dy(1:3) = (/4.0d0, 5.0d0, 6.0d0/)
  call drot(3, dx, 1, dy, 1, -1.0d0, 0.0d0)
  call begin_test('negate')
  call print_array('dx', dx, 3)
  call print_array('dy', dy, 3)
  call end_test()

end program
