program test_ddot
  use test_utils
  implicit none
  double precision :: dx(20), dy(20), result
  double precision :: ddot
  integer :: i

  ! Test 1: basic dot product, incx=1, incy=1
  dx = 0.0d0; dy = 0.0d0
  dx(1:5) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0/)
  dy(1:5) = (/2.0d0, 3.0d0, 4.0d0, 5.0d0, 6.0d0/)
  result = ddot(5, dx, 1, dy, 1)
  call begin_test('basic')
  call print_scalar('result', result)
  call end_test()

  ! Test 2: n=0 (should return 0)
  result = ddot(0, dx, 1, dy, 1)
  call begin_test('n_zero')
  call print_scalar('result', result)
  call end_test()

  ! Test 3: n=1
  dx(1) = 3.0d0
  dy(1) = 7.0d0
  result = ddot(1, dx, 1, dy, 1)
  call begin_test('n_one')
  call print_scalar('result', result)
  call end_test()

  ! Test 4: non-unit strides, incx=2, incy=2
  dx = 0.0d0; dy = 0.0d0
  dx(1) = 1.0d0; dx(3) = 2.0d0; dx(5) = 3.0d0
  dy(1) = 4.0d0; dy(3) = 5.0d0; dy(5) = 6.0d0
  result = ddot(3, dx, 2, dy, 2)
  call begin_test('stride')
  call print_scalar('result', result)
  call end_test()

  ! Test 5: negative strides
  dx = 0.0d0; dy = 0.0d0
  dx(1:3) = (/1.0d0, 2.0d0, 3.0d0/)
  dy(1:3) = (/4.0d0, 5.0d0, 6.0d0/)
  result = ddot(3, dx, -1, dy, 1)
  call begin_test('neg_inc')
  call print_scalar('result', result)
  call end_test()

  ! Test 6: orthogonal vectors (result should be 0)
  dx = 0.0d0; dy = 0.0d0
  dx(1:2) = (/1.0d0, 0.0d0/)
  dy(1:2) = (/0.0d0, 1.0d0/)
  result = ddot(2, dx, 1, dy, 1)
  call begin_test('orthogonal')
  call print_scalar('result', result)
  call end_test()

  ! Test 7: larger n=12 to exercise 5-unrolled loop
  dx = 0.0d0; dy = 0.0d0
  do i = 1, 12
    dx(i) = dble(i)
    dy(i) = dble(i * 2)
  end do
  result = ddot(12, dx, 1, dy, 1)
  call begin_test('unrolled')
  call print_scalar('result', result)
  call end_test()

end program
