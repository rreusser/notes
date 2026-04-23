program test_zlanhs
  use test_utils
  implicit none

  double precision :: zlanhs, result, work(10)
  complex*16 :: a(25)

  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 1.0d0); a(2) = (4.0d0, 2.0d0); a(3) = (0.0d0, 0.0d0)
  a(4) = (2.0d0, 0.0d0); a(5) = (5.0d0, 1.0d0); a(6) = (7.0d0, 3.0d0)
  a(7) = (3.0d0, 0.0d0); a(8) = (6.0d0, 0.0d0); a(9) = (8.0d0, 2.0d0)

  result = zlanhs('M', 3, a, 3, work)
  call begin_test('max_norm')
  call print_scalar('result', result)
  call end_test()

  result = zlanhs('1', 3, a, 3, work)
  call begin_test('one_norm')
  call print_scalar('result', result)
  call end_test()

  result = zlanhs('I', 3, a, 3, work)
  call begin_test('inf_norm')
  call print_scalar('result', result)
  call end_test()

  result = zlanhs('F', 3, a, 3, work)
  call begin_test('frob_norm')
  call print_scalar('result', result)
  call end_test()

  result = zlanhs('M', 0, a, 3, work)
  call begin_test('n_zero')
  call print_scalar('result', result)
  call end_test()

end program
