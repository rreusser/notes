program test_iladlc
  use test_utils
  implicit none

  double precision :: A(4, 4)
  integer :: ILADLC, result

  ! Test 1: 3x4 matrix, last non-zero in col 3
  A = 0.0d0
  A(1,1) = 1.0d0
  A(2,2) = 2.0d0
  A(3,3) = 3.0d0
  result = ILADLC(3, 4, A, 4)
  call begin_test('basic_3x4')
  call print_int('result', result)
  call end_test()

  ! Test 2: all zeros
  A = 0.0d0
  result = ILADLC(3, 4, A, 4)
  call begin_test('all_zeros')
  call print_int('result', result)
  call end_test()

  ! Test 3: last column non-zero (quick return path)
  A = 0.0d0
  A(1,4) = 5.0d0
  result = ILADLC(3, 4, A, 4)
  call begin_test('last_col_nonzero')
  call print_int('result', result)
  call end_test()

  ! Test 4: only first column non-zero
  A = 0.0d0
  A(1,1) = 1.0d0
  A(2,1) = 2.0d0
  result = ILADLC(3, 4, A, 4)
  call begin_test('first_col_only')
  call print_int('result', result)
  call end_test()

  ! Test 5: N=0
  result = ILADLC(3, 0, A, 4)
  call begin_test('n_zero')
  call print_int('result', result)
  call end_test()

  ! Test 6: bottom-right corner non-zero
  A = 0.0d0
  A(3,4) = 9.0d0
  result = ILADLC(3, 4, A, 4)
  call begin_test('bottom_right')
  call print_int('result', result)
  call end_test()

end program
