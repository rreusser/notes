program test_ilazlc
  use test_utils
  implicit none

  integer :: ilazlc
  external :: ilazlc

  complex*16 :: a(4, 4)
  integer :: result

  ! Test 1: 3x3 matrix with last non-zero column = 3
  ! col 1: (1+0i, 0, 0), col 2: (0, 2+0i, 0), col 3: (0, 0, 3+0i)
  a = (0.0d0, 0.0d0)
  a(1, 1) = (1.0d0, 0.0d0)
  a(2, 2) = (2.0d0, 0.0d0)
  a(3, 3) = (3.0d0, 0.0d0)
  result = ilazlc(3, 3, a, 4)
  call begin_test('ilazlc_diag')
  call print_int('result', result)
  call end_test()

  ! Test 2: last column is all zeros -> last non-zero is col 2
  a = (0.0d0, 0.0d0)
  a(1, 1) = (1.0d0, 0.0d0)
  a(2, 2) = (2.0d0, 0.0d0)
  result = ilazlc(3, 3, a, 4)
  call begin_test('ilazlc_col2')
  call print_int('result', result)
  call end_test()

  ! Test 3: all zeros -> returns 0
  a = (0.0d0, 0.0d0)
  result = ilazlc(3, 3, a, 4)
  call begin_test('ilazlc_zeros')
  call print_int('result', result)
  call end_test()

  ! Test 4: n=0 -> returns 0
  result = ilazlc(3, 0, a, 4)
  call begin_test('ilazlc_n_zero')
  call print_int('result', result)
  call end_test()

  ! Test 5: only imaginary part non-zero in last column
  a = (0.0d0, 0.0d0)
  a(2, 3) = (0.0d0, 5.0d0)
  result = ilazlc(3, 3, a, 4)
  call begin_test('ilazlc_imag')
  call print_int('result', result)
  call end_test()

  ! Test 6: full matrix (all non-zero)
  a(1,1) = (1.0d0, 1.0d0)
  a(2,1) = (2.0d0, 2.0d0)
  a(3,1) = (3.0d0, 3.0d0)
  a(1,2) = (4.0d0, 4.0d0)
  a(2,2) = (5.0d0, 5.0d0)
  a(3,2) = (6.0d0, 6.0d0)
  a(1,3) = (7.0d0, 7.0d0)
  a(2,3) = (8.0d0, 8.0d0)
  a(3,3) = (9.0d0, 9.0d0)
  result = ilazlc(3, 3, a, 4)
  call begin_test('ilazlc_full')
  call print_int('result', result)
  call end_test()

  ! Test 7: 1x1 non-zero
  a = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 0.0d0)
  result = ilazlc(1, 1, a, 4)
  call begin_test('ilazlc_1x1')
  call print_int('result', result)
  call end_test()

  ! Test 8: 1x1 zero
  a(1,1) = (0.0d0, 0.0d0)
  result = ilazlc(1, 1, a, 4)
  call begin_test('ilazlc_1x1_zero')
  call print_int('result', result)
  call end_test()

end program
