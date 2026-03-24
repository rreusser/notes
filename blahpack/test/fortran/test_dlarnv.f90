program test_dlarnv
  use test_utils
  implicit none

  integer :: iseed(4)
  double precision :: x(10)

  ! Test 1: Uniform (0,1)
  iseed = (/ 1, 1, 1, 1 /)
  call DLARNV(1, iseed, 5, x)

  call begin_test('uniform_01')
  call print_int_array('iseed_out', iseed, 4)
  call print_array('x', x, 5)
  call end_test()

  ! Test 2: Uniform (-1,1)
  iseed = (/ 1, 1, 1, 1 /)
  call DLARNV(2, iseed, 5, x)

  call begin_test('uniform_m1_1')
  call print_int_array('iseed_out', iseed, 4)
  call print_array('x', x, 5)
  call end_test()

  ! Test 3: Normal (0,1)
  iseed = (/ 1, 1, 1, 1 /)
  call DLARNV(3, iseed, 5, x)

  call begin_test('normal_01')
  call print_int_array('iseed_out', iseed, 4)
  call print_array('x', x, 5)
  call end_test()

end program
