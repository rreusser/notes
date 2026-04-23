program test_ilazlr
  use test_utils
  implicit none

  integer :: ilazlr
  external :: ilazlr

  complex*16 :: a(4, 4)
  integer :: result

  ! Test 1: 3x3 diagonal matrix -> last non-zero row = 3
  a = (0.0d0, 0.0d0)
  a(1, 1) = (1.0d0, 0.0d0)
  a(2, 2) = (2.0d0, 0.0d0)
  a(3, 3) = (3.0d0, 0.0d0)
  result = ilazlr(3, 3, a, 4)
  call begin_test('ilazlr_diag')
  call print_int('result', result)
  call end_test()

  ! Test 2: last row is all zeros -> last non-zero is row 2
  a = (0.0d0, 0.0d0)
  a(1, 1) = (1.0d0, 0.0d0)
  a(2, 2) = (2.0d0, 0.0d0)
  result = ilazlr(3, 3, a, 4)
  call begin_test('ilazlr_row2')
  call print_int('result', result)
  call end_test()

  ! Test 3: all zeros -> returns 0
  a = (0.0d0, 0.0d0)
  result = ilazlr(3, 3, a, 4)
  call begin_test('ilazlr_zeros')
  call print_int('result', result)
  call end_test()

  ! Test 4: m=0 -> returns 0
  result = ilazlr(0, 3, a, 4)
  call begin_test('ilazlr_m_zero')
  call print_int('result', result)
  call end_test()

  ! Test 5: only imaginary part non-zero in last row
  a = (0.0d0, 0.0d0)
  a(3, 2) = (0.0d0, 5.0d0)
  result = ilazlr(3, 3, a, 4)
  call begin_test('ilazlr_imag')
  call print_int('result', result)
  call end_test()

  ! Test 6: full matrix
  a(1,1) = (1.0d0, 1.0d0)
  a(2,1) = (2.0d0, 2.0d0)
  a(3,1) = (3.0d0, 3.0d0)
  a(1,2) = (4.0d0, 4.0d0)
  a(2,2) = (5.0d0, 5.0d0)
  a(3,2) = (6.0d0, 6.0d0)
  a(1,3) = (7.0d0, 7.0d0)
  a(2,3) = (8.0d0, 8.0d0)
  a(3,3) = (9.0d0, 9.0d0)
  result = ilazlr(3, 3, a, 4)
  call begin_test('ilazlr_full')
  call print_int('result', result)
  call end_test()

  ! Test 7: 1x1 non-zero
  a = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 0.0d0)
  result = ilazlr(1, 1, a, 4)
  call begin_test('ilazlr_1x1')
  call print_int('result', result)
  call end_test()

  ! Test 8: 1x1 zero
  a(1,1) = (0.0d0, 0.0d0)
  result = ilazlr(1, 1, a, 4)
  call begin_test('ilazlr_1x1_zero')
  call print_int('result', result)
  call end_test()

end program
