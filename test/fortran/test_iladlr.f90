program test_iladlr
  use test_utils
  implicit none

  double precision :: A(4, 4)
  integer :: ILADLR, result

  ! Test 1: 3x4 matrix, last non-zero row = 3
  A = 0.0d0
  A(1,1) = 1.0d0
  A(2,2) = 2.0d0
  A(3,3) = 3.0d0
  result = ILADLR(3, 4, A, 4)
  call begin_test('basic_3x4')
  call print_int('result', result)
  call end_test()

  ! Test 2: all zeros
  A = 0.0d0
  result = ILADLR(3, 4, A, 4)
  call begin_test('all_zeros')
  call print_int('result', result)
  call end_test()

  ! Test 3: last row non-zero (quick return path)
  A = 0.0d0
  A(3,1) = 5.0d0
  result = ILADLR(3, 4, A, 4)
  call begin_test('last_row_nonzero')
  call print_int('result', result)
  call end_test()

  ! Test 4: only first row non-zero
  A = 0.0d0
  A(1,1) = 1.0d0
  A(1,3) = 2.0d0
  result = ILADLR(3, 4, A, 4)
  call begin_test('first_row_only')
  call print_int('result', result)
  call end_test()

  ! Test 5: M=0
  result = ILADLR(0, 4, A, 4)
  call begin_test('m_zero')
  call print_int('result', result)
  call end_test()

  ! Test 6: bottom-right corner non-zero
  A = 0.0d0
  A(3,4) = 9.0d0
  result = ILADLR(3, 4, A, 4)
  call begin_test('bottom_right')
  call print_int('result', result)
  call end_test()

end program
