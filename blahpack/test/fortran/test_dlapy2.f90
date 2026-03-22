program test_dlapy2
  use test_utils
  implicit none

  double precision :: DLAPY2, result

  ! Test 1: basic 3,4 -> 5
  result = DLAPY2(3.0d0, 4.0d0)
  call begin_test('basic')
  call print_scalar('result', result)
  call end_test()

  ! Test 2: x=0
  result = DLAPY2(0.0d0, 5.0d0)
  call begin_test('x_zero')
  call print_scalar('result', result)
  call end_test()

  ! Test 3: y=0
  result = DLAPY2(7.0d0, 0.0d0)
  call begin_test('y_zero')
  call print_scalar('result', result)
  call end_test()

  ! Test 4: both zero
  result = DLAPY2(0.0d0, 0.0d0)
  call begin_test('both_zero')
  call print_scalar('result', result)
  call end_test()

  ! Test 5: large values
  result = DLAPY2(1.0d+154, 1.0d+154)
  call begin_test('large')
  call print_scalar('result', result)
  call end_test()

  ! Test 6: negative values
  result = DLAPY2(-3.0d0, -4.0d0)
  call begin_test('negative')
  call print_scalar('result', result)
  call end_test()

end program
