program test_zladiv
  use test_utils
  implicit none
  complex*16 :: x, y, result
  complex*16 :: zladiv
  double precision :: result_r(2)
  equivalence (result, result_r)

  ! Test 1: (4+2i)/(1+1i) = 3-1i
  x = (4.0d0, 2.0d0)
  y = (1.0d0, 1.0d0)
  result = zladiv(x, y)
  call begin_test('zladiv_basic')
  call print_array('result', result_r, 2)
  call end_test()

  ! Test 2: (1+0i)/(0+1i) = 0-1i
  x = (1.0d0, 0.0d0)
  y = (0.0d0, 1.0d0)
  result = zladiv(x, y)
  call begin_test('zladiv_pure_imag_denom')
  call print_array('result', result_r, 2)
  call end_test()

  ! Test 3: (1+0i)/(1+0i) = 1+0i
  x = (1.0d0, 0.0d0)
  y = (1.0d0, 0.0d0)
  result = zladiv(x, y)
  call begin_test('zladiv_real_div')
  call print_array('result', result_r, 2)
  call end_test()

  ! Test 4: (0+0i)/(1+1i) = 0+0i
  x = (0.0d0, 0.0d0)
  y = (1.0d0, 1.0d0)
  result = zladiv(x, y)
  call begin_test('zladiv_zero_numer')
  call print_array('result', result_r, 2)
  call end_test()

  ! Test 5: (3+4i)/(1-2i) = -1+2i
  x = (3.0d0, 4.0d0)
  y = (1.0d0, -2.0d0)
  result = zladiv(x, y)
  call begin_test('zladiv_neg_denom')
  call print_array('result', result_r, 2)
  call end_test()

  ! Test 6: large values
  x = (1.0d300, 1.0d300)
  y = (1.0d300, 1.0d300)
  result = zladiv(x, y)
  call begin_test('zladiv_large')
  call print_array('result', result_r, 2)
  call end_test()

  ! Test 7: small values
  x = (1.0d-300, 1.0d-300)
  y = (1.0d-300, 1.0d-300)
  result = zladiv(x, y)
  call begin_test('zladiv_small')
  call print_array('result', result_r, 2)
  call end_test()

end program
