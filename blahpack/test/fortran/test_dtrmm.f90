program test_dtrmm
  use test_utils
  implicit none
  double precision :: a(16), b(16)

  ! Test 1: left, upper, no-trans, non-unit, 3x2
  ! A (upper tri 3x3): [2 3 4; 0 5 6; 0 0 7]
  ! B (3x2): [1 4; 2 5; 3 6]
  ! B := 1.0 * A * B
  a = 0.0d0; b = 0.0d0
  a(1) = 2.0d0; a(4) = 3.0d0; a(7) = 4.0d0
  a(5) = 5.0d0; a(8) = 6.0d0
  a(9) = 7.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  b(4) = 4.0d0; b(5) = 5.0d0; b(6) = 6.0d0
  call dtrmm('L', 'U', 'N', 'N', 3, 2, 1.0d0, a, 3, b, 3)
  call begin_test('left_upper_n')
  call print_matrix('b', b, 3, 3, 2)
  call end_test()

  ! Test 2: left, lower, no-trans, non-unit, 3x2
  ! A (lower tri 3x3): [2 0 0; 3 5 0; 4 6 7]
  a = 0.0d0; b = 0.0d0
  a(1) = 2.0d0; a(2) = 3.0d0; a(3) = 4.0d0
  a(5) = 5.0d0; a(6) = 6.0d0
  a(9) = 7.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  b(4) = 4.0d0; b(5) = 5.0d0; b(6) = 6.0d0
  call dtrmm('L', 'L', 'N', 'N', 3, 2, 1.0d0, a, 3, b, 3)
  call begin_test('left_lower_n')
  call print_matrix('b', b, 3, 3, 2)
  call end_test()

  ! Test 3: left, upper, transpose, non-unit, 3x2
  a = 0.0d0; b = 0.0d0
  a(1) = 2.0d0; a(4) = 3.0d0; a(7) = 4.0d0
  a(5) = 5.0d0; a(8) = 6.0d0
  a(9) = 7.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  b(4) = 4.0d0; b(5) = 5.0d0; b(6) = 6.0d0
  call dtrmm('L', 'U', 'T', 'N', 3, 2, 1.0d0, a, 3, b, 3)
  call begin_test('left_upper_t')
  call print_matrix('b', b, 3, 3, 2)
  call end_test()

  ! Test 4: left, lower, transpose, non-unit, 3x2
  a = 0.0d0; b = 0.0d0
  a(1) = 2.0d0; a(2) = 3.0d0; a(3) = 4.0d0
  a(5) = 5.0d0; a(6) = 6.0d0
  a(9) = 7.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  b(4) = 4.0d0; b(5) = 5.0d0; b(6) = 6.0d0
  call dtrmm('L', 'L', 'T', 'N', 3, 2, 1.0d0, a, 3, b, 3)
  call begin_test('left_lower_t')
  call print_matrix('b', b, 3, 3, 2)
  call end_test()

  ! Test 5: right, upper, no-trans, non-unit, 3x2
  ! A (upper tri 2x2): [2 3; 0 5]
  ! B (3x2): [1 4; 2 5; 3 6]
  a = 0.0d0; b = 0.0d0
  a(1) = 2.0d0; a(3) = 3.0d0
  a(4) = 5.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  b(4) = 4.0d0; b(5) = 5.0d0; b(6) = 6.0d0
  call dtrmm('R', 'U', 'N', 'N', 3, 2, 1.0d0, a, 2, b, 3)
  call begin_test('right_upper_n')
  call print_matrix('b', b, 3, 3, 2)
  call end_test()

  ! Test 6: right, lower, no-trans, non-unit, 3x2
  ! A (lower tri 2x2): [2 0; 3 5]
  a = 0.0d0; b = 0.0d0
  a(1) = 2.0d0; a(2) = 3.0d0
  a(4) = 5.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  b(4) = 4.0d0; b(5) = 5.0d0; b(6) = 6.0d0
  call dtrmm('R', 'L', 'N', 'N', 3, 2, 1.0d0, a, 2, b, 3)
  call begin_test('right_lower_n')
  call print_matrix('b', b, 3, 3, 2)
  call end_test()

  ! Test 7: right, upper, transpose, non-unit, 3x2
  a = 0.0d0; b = 0.0d0
  a(1) = 2.0d0; a(3) = 3.0d0
  a(4) = 5.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  b(4) = 4.0d0; b(5) = 5.0d0; b(6) = 6.0d0
  call dtrmm('R', 'U', 'T', 'N', 3, 2, 1.0d0, a, 2, b, 3)
  call begin_test('right_upper_t')
  call print_matrix('b', b, 3, 3, 2)
  call end_test()

  ! Test 8: right, lower, transpose, non-unit, 3x2
  a = 0.0d0; b = 0.0d0
  a(1) = 2.0d0; a(2) = 3.0d0
  a(4) = 5.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  b(4) = 4.0d0; b(5) = 5.0d0; b(6) = 6.0d0
  call dtrmm('R', 'L', 'T', 'N', 3, 2, 1.0d0, a, 2, b, 3)
  call begin_test('right_lower_t')
  call print_matrix('b', b, 3, 3, 2)
  call end_test()

  ! Test 9: alpha=0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  b(4) = 4.0d0; b(5) = 5.0d0; b(6) = 6.0d0
  call dtrmm('L', 'U', 'N', 'N', 3, 2, 0.0d0, a, 3, b, 3)
  call begin_test('alpha_zero')
  call print_matrix('b', b, 3, 3, 2)
  call end_test()

  ! Test 10: M=0 quick return
  b(1) = 99.0d0
  call dtrmm('L', 'U', 'N', 'N', 0, 2, 1.0d0, a, 1, b, 1)
  call begin_test('m_zero')
  call print_array('b', b, 1)
  call end_test()

  ! Test 11: unit diag, left, upper, no-trans
  a = 0.0d0; b = 0.0d0
  a(1) = 99.0d0; a(4) = 3.0d0; a(7) = 4.0d0
  a(5) = 99.0d0; a(8) = 6.0d0
  a(9) = 99.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  b(4) = 4.0d0; b(5) = 5.0d0; b(6) = 6.0d0
  call dtrmm('L', 'U', 'N', 'U', 3, 2, 1.0d0, a, 3, b, 3)
  call begin_test('unit_diag')
  call print_matrix('b', b, 3, 3, 2)
  call end_test()

  ! Test 12: alpha=2, left, upper, no-trans
  a = 0.0d0; b = 0.0d0
  a(1) = 2.0d0; a(4) = 3.0d0; a(7) = 4.0d0
  a(5) = 5.0d0; a(8) = 6.0d0
  a(9) = 7.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  b(4) = 4.0d0; b(5) = 5.0d0; b(6) = 6.0d0
  call dtrmm('L', 'U', 'N', 'N', 3, 2, 2.0d0, a, 3, b, 3)
  call begin_test('alpha_two')
  call print_matrix('b', b, 3, 3, 2)
  call end_test()

  ! Test 13: alpha=2, left, lower, no-trans
  a = 0.0d0; b = 0.0d0
  a(1) = 2.0d0; a(2) = 3.0d0; a(3) = 4.0d0
  a(5) = 5.0d0; a(6) = 6.0d0
  a(9) = 7.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  b(4) = 4.0d0; b(5) = 5.0d0; b(6) = 6.0d0
  call dtrmm('L', 'L', 'N', 'N', 3, 2, 2.0d0, a, 3, b, 3)
  call begin_test('alpha_two_left_lower')
  call print_matrix('b', b, 3, 3, 2)
  call end_test()

  ! Test 14: alpha=2, right, upper, no-trans
  a = 0.0d0; b = 0.0d0
  a(1) = 2.0d0; a(3) = 3.0d0
  a(4) = 5.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  b(4) = 4.0d0; b(5) = 5.0d0; b(6) = 6.0d0
  call dtrmm('R', 'U', 'N', 'N', 3, 2, 2.0d0, a, 2, b, 3)
  call begin_test('alpha_two_right_upper')
  call print_matrix('b', b, 3, 3, 2)
  call end_test()

  ! Test 15: alpha=2, right, lower, no-trans
  a = 0.0d0; b = 0.0d0
  a(1) = 2.0d0; a(2) = 3.0d0
  a(4) = 5.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  b(4) = 4.0d0; b(5) = 5.0d0; b(6) = 6.0d0
  call dtrmm('R', 'L', 'N', 'N', 3, 2, 2.0d0, a, 2, b, 3)
  call begin_test('alpha_two_right_lower')
  call print_matrix('b', b, 3, 3, 2)
  call end_test()

  ! Test 16: alpha=2, right, upper, transpose
  a = 0.0d0; b = 0.0d0
  a(1) = 2.0d0; a(3) = 3.0d0
  a(4) = 5.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  b(4) = 4.0d0; b(5) = 5.0d0; b(6) = 6.0d0
  call dtrmm('R', 'U', 'T', 'N', 3, 2, 2.0d0, a, 2, b, 3)
  call begin_test('alpha_two_right_upper_t')
  call print_matrix('b', b, 3, 3, 2)
  call end_test()

  ! Test 17: alpha=2, right, lower, transpose
  a = 0.0d0; b = 0.0d0
  a(1) = 2.0d0; a(2) = 3.0d0
  a(4) = 5.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  b(4) = 4.0d0; b(5) = 5.0d0; b(6) = 6.0d0
  call dtrmm('R', 'L', 'T', 'N', 3, 2, 2.0d0, a, 2, b, 3)
  call begin_test('alpha_two_right_lower_t')
  call print_matrix('b', b, 3, 3, 2)
  call end_test()

end program
