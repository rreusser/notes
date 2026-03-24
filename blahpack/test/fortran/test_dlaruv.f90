program test_dlaruv
  use test_utils
  implicit none

  integer :: iseed(4)
  double precision :: x(10)

  ! Test 1: Generate 5 random numbers
  iseed(1) = 1
  iseed(2) = 1
  iseed(3) = 1
  iseed(4) = 1

  call DLARUV(iseed, 5, x)

  call begin_test('basic_5')
  call print_int_array('iseed_out', iseed, 4)
  call print_array('x', x, 5)
  call end_test()

  ! Test 2: Generate 10 random numbers with different seed
  iseed(1) = 123
  iseed(2) = 456
  iseed(3) = 789
  iseed(4) = 1011

  call DLARUV(iseed, 10, x)

  call begin_test('basic_10')
  call print_int_array('iseed_out', iseed, 4)
  call print_array('x', x, 10)
  call end_test()

  ! Test 3: N=0
  iseed(1) = 1
  iseed(2) = 1
  iseed(3) = 1
  iseed(4) = 1
  call DLARUV(iseed, 0, x)

  call begin_test('n_equals_0')
  call print_int_array('iseed_out', iseed, 4)
  call end_test()

end program
