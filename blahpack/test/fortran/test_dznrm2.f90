program test_dznrm2
  use test_utils
  implicit none
  double precision :: dznrm2, result
  complex*16 :: x(10)

  ! Test 1: basic — |(3+4i)| = 5, so norm of single element = 5
  x(1) = (3.0d0, 4.0d0)
  result = dznrm2(1, x, 1)
  call begin_test('single')
  call print_scalar('result', result)
  call end_test()

  ! Test 2: two elements — |(1+0i), (0+1i)| = sqrt(2)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (0.0d0, 1.0d0)
  result = dznrm2(2, x, 1)
  call begin_test('two_elements')
  call print_scalar('result', result)
  call end_test()

  ! Test 3: n=0 → 0
  result = dznrm2(0, x, 1)
  call begin_test('n_zero')
  call print_scalar('result', result)
  call end_test()

  ! Test 4: three elements
  x(1) = (1.0d0, 2.0d0)
  x(2) = (3.0d0, 4.0d0)
  x(3) = (5.0d0, 6.0d0)
  result = dznrm2(3, x, 1)
  call begin_test('three_elements')
  call print_scalar('result', result)
  call end_test()

  ! Test 5: stride=2
  x(1) = (1.0d0, 0.0d0)
  x(2) = (99.0d0, 99.0d0)
  x(3) = (0.0d0, 1.0d0)
  result = dznrm2(2, x, 2)
  call begin_test('stride_2')
  call print_scalar('result', result)
  call end_test()

end program
