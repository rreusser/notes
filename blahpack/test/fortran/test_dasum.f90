program test_dasum
  use test_utils
  implicit none
  double precision :: dx(20), result
  double precision, external :: dasum
  integer :: i

  ! Test 1: basic, n=5, incx=1, positive values
  dx = 0.0d0
  dx(1:5) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0/)
  result = dasum(5, dx, 1)
  call begin_test('basic')
  call print_scalar('result', result)
  call end_test()

  ! Test 2: mixed signs
  dx = 0.0d0
  dx(1:5) = (/-1.0d0, 2.0d0, -3.0d0, 4.0d0, -5.0d0/)
  result = dasum(5, dx, 1)
  call begin_test('mixed_signs')
  call print_scalar('result', result)
  call end_test()

  ! Test 3: n=0 (should return 0)
  result = dasum(0, dx, 1)
  call begin_test('n_zero')
  call print_scalar('result', result)
  call end_test()

  ! Test 4: n=1
  dx(1) = 42.0d0
  result = dasum(1, dx, 1)
  call begin_test('n_one')
  call print_scalar('result', result)
  call end_test()

  ! Test 5: non-unit stride, incx=2
  dx = 0.0d0
  dx(1) = 1.0d0; dx(3) = 2.0d0; dx(5) = 3.0d0
  result = dasum(3, dx, 2)
  call begin_test('stride')
  call print_scalar('result', result)
  call end_test()

  ! Test 6: larger n=13 to exercise the 6-unrolled loop
  dx = 0.0d0
  do i = 1, 13
    dx(i) = dble(i)
  end do
  result = dasum(13, dx, 1)
  call begin_test('unrolled')
  call print_scalar('result', result)
  call end_test()

end program
