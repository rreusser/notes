program test_dcabs1
  use test_utils
  implicit none
  double precision :: dcabs1, result
  complex*16 :: z

  z = (3.0d0, 4.0d0)
  result = dcabs1(z)
  call begin_test('basic')
  call print_scalar('result', result)
  call end_test()

  z = (0.0d0, 0.0d0)
  result = dcabs1(z)
  call begin_test('zero')
  call print_scalar('result', result)
  call end_test()

  z = (-5.0d0, 12.0d0)
  result = dcabs1(z)
  call begin_test('negative')
  call print_scalar('result', result)
  call end_test()

  z = (1.0d-300, -2.0d-300)
  result = dcabs1(z)
  call begin_test('tiny')
  call print_scalar('result', result)
  call end_test()

end program
