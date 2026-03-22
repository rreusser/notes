program test_daxpy
  use test_utils
  implicit none
  double precision :: dx(20), dy(20)
  integer :: i

  ! Test 1: basic, inc=1
  dx = 0.0d0; dy = 0.0d0
  dx(1:5) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0/)
  dy(1:5) = (/10.0d0, 20.0d0, 30.0d0, 40.0d0, 50.0d0/)
  call daxpy(5, 2.0d0, dx, 1, dy, 1)
  call begin_test('basic')
  call print_array('dy', dy, 5)
  call end_test()

  ! Test 2: da=0 (should be no-op)
  dx(1:5) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0/)
  dy(1:5) = (/10.0d0, 20.0d0, 30.0d0, 40.0d0, 50.0d0/)
  call daxpy(5, 0.0d0, dx, 1, dy, 1)
  call begin_test('da_zero')
  call print_array('dy', dy, 5)
  call end_test()

  ! Test 3: n=0 (should be no-op)
  dy(1:5) = (/10.0d0, 20.0d0, 30.0d0, 40.0d0, 50.0d0/)
  call daxpy(0, 2.0d0, dx, 1, dy, 1)
  call begin_test('n_zero')
  call print_array('dy', dy, 5)
  call end_test()

  ! Test 4: n=1
  dy(1) = 7.0d0
  dx(1) = 3.0d0
  call daxpy(1, 5.0d0, dx, 1, dy, 1)
  call begin_test('n_one')
  call print_array('dy', dy, 1)
  call end_test()

  ! Test 5: incx=2, incy=3
  dx = 0.0d0; dy = 0.0d0
  dx(1) = 1.0d0; dx(3) = 2.0d0; dx(5) = 3.0d0
  dy(1) = 10.0d0; dy(4) = 20.0d0; dy(7) = 30.0d0
  call daxpy(3, 2.0d0, dx, 2, dy, 3)
  call begin_test('stride')
  call print_array('dy', dy, 9)
  call end_test()

  ! Test 6: negative incx
  dx = 0.0d0; dy = 0.0d0
  dx(1:3) = (/1.0d0, 2.0d0, 3.0d0/)
  dy(1:3) = (/10.0d0, 20.0d0, 30.0d0/)
  call daxpy(3, 1.0d0, dx, -1, dy, 1)
  call begin_test('neg_incx')
  call print_array('dy', dy, 3)
  call end_test()

  ! Test 7: larger n (exercises 4-unrolled loop)
  do i = 1, 10
    dx(i) = dble(i)
    dy(i) = dble(i * 10)
  end do
  call daxpy(10, 3.0d0, dx, 1, dy, 1)
  call begin_test('unrolled')
  call print_array('dy', dy, 10)
  call end_test()

end program
