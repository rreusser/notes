program test_dnrm2
  use test_utils
  implicit none

  double precision :: x(10), result
  double precision :: DNRM2

  ! Test 1: basic - norm of [3, 4] = 5
  x(1) = 3.0d0
  x(2) = 4.0d0
  result = DNRM2(2, x, 1)
  call begin_test('basic')
  call print_scalar('result', result)
  call end_test()

  ! Test 2: n=0
  result = DNRM2(0, x, 1)
  call begin_test('n_zero')
  call print_scalar('result', result)
  call end_test()

  ! Test 3: n=1
  x(1) = 7.0d0
  result = DNRM2(1, x, 1)
  call begin_test('n_one')
  call print_scalar('result', result)
  call end_test()

  ! Test 4: stride=2
  x(1) = 3.0d0
  x(2) = 999.0d0
  x(3) = 4.0d0
  result = DNRM2(2, x, 2)
  call begin_test('stride2')
  call print_scalar('result', result)
  call end_test()

  ! Test 5: large values (overflow protection)
  x(1) = 1.0d+154
  x(2) = 1.0d+154
  result = DNRM2(2, x, 1)
  call begin_test('large_values')
  call print_scalar('result', result)
  call end_test()

  ! Test 6: small values (underflow protection)
  x(1) = 1.0d-160
  x(2) = 1.0d-160
  result = DNRM2(2, x, 1)
  call begin_test('small_values')
  call print_scalar('result', result)
  call end_test()

  ! Test 7: mixed values
  x(1) = 1.0d0
  x(2) = 2.0d0
  x(3) = 3.0d0
  x(4) = 4.0d0
  x(5) = 5.0d0
  result = DNRM2(5, x, 1)
  call begin_test('five_elements')
  call print_scalar('result', result)
  call end_test()

  ! Test 8: all zeros
  x(1) = 0.0d0
  x(2) = 0.0d0
  x(3) = 0.0d0
  result = DNRM2(3, x, 1)
  call begin_test('all_zeros')
  call print_scalar('result', result)
  call end_test()

end program
