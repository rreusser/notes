program test_idamax
  use test_utils
  implicit none
  double precision :: dx(20)
  integer :: idamax, result

  ! Test 1: basic, n=5, incx=1
  ! |values|: 1.0, 3.0, 2.0, 5.0, 4.0
  ! Max is element 4 (1-based), value = 5.0
  dx = 0.0d0
  dx(1) = 1.0d0
  dx(2) = -3.0d0
  dx(3) = 2.0d0
  dx(4) = 5.0d0
  dx(5) = -4.0d0
  result = idamax(5, dx, 1)
  call begin_test('basic')
  call print_int('result', result)
  call end_test()

  ! Test 2: n=0 (should return 0)
  result = idamax(0, dx, 1)
  call begin_test('n_zero')
  call print_int('result', result)
  call end_test()

  ! Test 3: n=1 (should return 1)
  result = idamax(1, dx, 1)
  call begin_test('n_one')
  call print_int('result', result)
  call end_test()

  ! Test 4: negative values, n=4, incx=1
  ! |-2|=2, |-7|=7, |-1|=1, |-3|=3
  ! Max is element 2 (1-based), value = 7.0
  dx = 0.0d0
  dx(1) = -2.0d0
  dx(2) = -7.0d0
  dx(3) = -1.0d0
  dx(4) = -3.0d0
  result = idamax(4, dx, 1)
  call begin_test('negative')
  call print_int('result', result)
  call end_test()

  ! Test 5: non-unit stride, incx=2
  ! Elements accessed: dx(1)=1, dx(3)=10, dx(5)=2
  ! Max is element 2 (1-based, |10|=10)
  dx = 0.0d0
  dx(1) = 1.0d0
  dx(2) = 99.0d0
  dx(3) = 10.0d0
  dx(4) = 99.0d0
  dx(5) = 2.0d0
  result = idamax(3, dx, 2)
  call begin_test('stride')
  call print_int('result', result)
  call end_test()

  ! Test 6: first element is max
  dx = 0.0d0
  dx(1) = 100.0d0
  dx(2) = 1.0d0
  dx(3) = 2.0d0
  result = idamax(3, dx, 1)
  call begin_test('first_max')
  call print_int('result', result)
  call end_test()

  ! Test 7: last element is max
  dx = 0.0d0
  dx(1) = 1.0d0
  dx(2) = 2.0d0
  dx(3) = 100.0d0
  result = idamax(3, dx, 1)
  call begin_test('last_max')
  call print_int('result', result)
  call end_test()

end program
