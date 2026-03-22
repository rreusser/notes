program test_dtrsm
  use test_utils
  implicit none
  double precision :: a(20), b(20)

  ! Test 1: Left, Upper, No-trans, Non-unit diagonal — basic 2x2
  ! Solve A*X = alpha*B where A is upper triangular
  ! A = [2 3; 0 4] (col-major: 2, 0, 3, 4), B = [8 10; 4 12]
  ! alpha = 1.0
  ! Solution: from A*X = B
  ! Row 2: 4*x21 = 4 => x21 = 1; 4*x22 = 12 => x22 = 3
  ! Row 1: 2*x11 + 3*x21 = 8 => x11 = (8-3)/2 = 2.5; 2*x12 + 3*x22 = 10 => x12 = (10-9)/2 = 0.5
  a = 0.0d0; b = 0.0d0
  a(1) = 2.0d0; a(2) = 0.0d0; a(3) = 3.0d0; a(4) = 4.0d0
  b(1) = 8.0d0; b(2) = 4.0d0; b(3) = 10.0d0; b(4) = 12.0d0
  call dtrsm('L', 'U', 'N', 'N', 2, 2, 1.0d0, a, 2, b, 2)
  call begin_test('left_upper_n_n')
  call print_matrix('B', b, 2, 2, 2)
  call end_test()

  ! Test 2: Left, Lower, No-trans, Non-unit
  ! A = [3 0; 2 5] (col-major: 3, 2, 0, 5), B = [6 9; 14 25]
  ! Solve A*X = B
  ! Row 1: 3*x11 = 6 => x11 = 2; 3*x12 = 9 => x12 = 3
  ! Row 2: 2*x11 + 5*x21 = 14 => x21 = (14-4)/5 = 2; 2*x12 + 5*x22 = 25 => x22 = (25-6)/5 = 3.8
  a = 0.0d0; b = 0.0d0
  a(1) = 3.0d0; a(2) = 2.0d0; a(3) = 0.0d0; a(4) = 5.0d0
  b(1) = 6.0d0; b(2) = 14.0d0; b(3) = 9.0d0; b(4) = 25.0d0
  call dtrsm('L', 'L', 'N', 'N', 2, 2, 1.0d0, a, 2, b, 2)
  call begin_test('left_lower_n_n')
  call print_matrix('B', b, 2, 2, 2)
  call end_test()

  ! Test 3: Right, Upper, No-trans, Non-unit
  ! Solve X*A = alpha*B where A is upper triangular
  ! A = [2 3; 0 4], B = [4 11; 6 15]
  a = 0.0d0; b = 0.0d0
  a(1) = 2.0d0; a(2) = 0.0d0; a(3) = 3.0d0; a(4) = 4.0d0
  b(1) = 4.0d0; b(2) = 6.0d0; b(3) = 11.0d0; b(4) = 15.0d0
  call dtrsm('R', 'U', 'N', 'N', 2, 2, 1.0d0, a, 2, b, 2)
  call begin_test('right_upper_n_n')
  call print_matrix('B', b, 2, 2, 2)
  call end_test()

  ! Test 4: Unit diagonal — Left, Upper, No-trans, Unit
  ! A = [1 3; 0 1] (diag='U' means diagonal treated as 1)
  ! Actual stored: [99 0; 3 99] but diag elements ignored
  ! Solve A*X = B where A = [1 3; 0 1]
  ! B = [7 10; 1 2]
  ! Row 2: x21 = 1; x22 = 2
  ! Row 1: x11 + 3*1 = 7 => x11 = 4; x12 + 3*2 = 10 => x12 = 4
  a = 0.0d0; b = 0.0d0
  a(1) = 99.0d0; a(2) = 0.0d0; a(3) = 3.0d0; a(4) = 99.0d0
  b(1) = 7.0d0; b(2) = 1.0d0; b(3) = 10.0d0; b(4) = 2.0d0
  call dtrsm('L', 'U', 'N', 'U', 2, 2, 1.0d0, a, 2, b, 2)
  call begin_test('unit_diag')
  call print_matrix('B', b, 2, 2, 2)
  call end_test()

  ! Test 5: Alpha scaling
  ! Same as test 1 but alpha=2.0
  ! Solve A*X = 2*B
  a = 0.0d0; b = 0.0d0
  a(1) = 2.0d0; a(2) = 0.0d0; a(3) = 3.0d0; a(4) = 4.0d0
  b(1) = 8.0d0; b(2) = 4.0d0; b(3) = 10.0d0; b(4) = 12.0d0
  call dtrsm('L', 'U', 'N', 'N', 2, 2, 2.0d0, a, 2, b, 2)
  call begin_test('alpha_scale')
  call print_matrix('B', b, 2, 2, 2)
  call end_test()

  ! Test 6: alpha=0 — B should be zeroed
  b = 0.0d0
  b(1) = 5.0d0; b(2) = 6.0d0; b(3) = 7.0d0; b(4) = 8.0d0
  call dtrsm('L', 'U', 'N', 'N', 2, 2, 0.0d0, a, 2, b, 2)
  call begin_test('alpha_zero')
  call print_matrix('B', b, 2, 2, 2)
  call end_test()

  ! Test 7: Left, Upper, Transpose, Non-unit
  ! Solve A^T*X = B
  ! A = [2 3; 0 4], A^T = [2 0; 3 4]
  ! B = [4 2; 11 14]
  ! Row 1: 2*x11 = 4 => x11 = 2; 2*x12 = 2 => x12 = 1
  ! Row 2: 3*x11 + 4*x21 = 11 => x21 = (11-6)/4 = 1.25
  a = 0.0d0; b = 0.0d0
  a(1) = 2.0d0; a(2) = 0.0d0; a(3) = 3.0d0; a(4) = 4.0d0
  b(1) = 4.0d0; b(2) = 11.0d0; b(3) = 2.0d0; b(4) = 14.0d0
  call dtrsm('L', 'U', 'T', 'N', 2, 2, 1.0d0, a, 2, b, 2)
  call begin_test('left_upper_t_n')
  call print_matrix('B', b, 2, 2, 2)
  call end_test()

  ! Test 8: Left, Lower, Transpose, Non-unit
  ! A = [3 0; 2 5], A^T = [3 2; 0 5]
  ! Solve A^T*X = B
  ! B = [9 15; 10 19]
  a = 0.0d0; b = 0.0d0
  a(1) = 3.0d0; a(2) = 2.0d0; a(3) = 0.0d0; a(4) = 5.0d0
  b(1) = 9.0d0; b(2) = 10.0d0; b(3) = 15.0d0; b(4) = 19.0d0
  call dtrsm('L', 'L', 'T', 'N', 2, 2, 1.0d0, a, 2, b, 2)
  call begin_test('left_lower_t_n')
  call print_matrix('B', b, 2, 2, 2)
  call end_test()

  ! Test 9: Right, Lower, No-trans, Non-unit
  ! Solve X*A = B where A is lower triangular
  ! A = [3 0; 2 5], B = [3 10; 6 22]
  a = 0.0d0; b = 0.0d0
  a(1) = 3.0d0; a(2) = 2.0d0; a(3) = 0.0d0; a(4) = 5.0d0
  b(1) = 3.0d0; b(2) = 6.0d0; b(3) = 10.0d0; b(4) = 22.0d0
  call dtrsm('R', 'L', 'N', 'N', 2, 2, 1.0d0, a, 2, b, 2)
  call begin_test('right_lower_n_n')
  call print_matrix('B', b, 2, 2, 2)
  call end_test()

  ! Test 10: Right, Upper, Transpose, Non-unit
  ! Solve X*A^T = B where A is upper triangular
  ! A = [2 3; 0 4], A^T = [2 0; 3 4]
  a = 0.0d0; b = 0.0d0
  a(1) = 2.0d0; a(2) = 0.0d0; a(3) = 3.0d0; a(4) = 4.0d0
  b(1) = 4.0d0; b(2) = 6.0d0; b(3) = 14.0d0; b(4) = 22.0d0
  call dtrsm('R', 'U', 'T', 'N', 2, 2, 1.0d0, a, 2, b, 2)
  call begin_test('right_upper_t_n')
  call print_matrix('B', b, 2, 2, 2)
  call end_test()

  ! Test 11: Right, Lower, Transpose, Non-unit
  ! Solve X*A^T = B where A is lower triangular
  ! A = [3 0; 2 5], A^T = [3 2; 0 5]
  a = 0.0d0; b = 0.0d0
  a(1) = 3.0d0; a(2) = 2.0d0; a(3) = 0.0d0; a(4) = 5.0d0
  b(1) = 6.0d0; b(2) = 9.0d0; b(3) = 10.0d0; b(4) = 25.0d0
  call dtrsm('R', 'L', 'T', 'N', 2, 2, 1.0d0, a, 2, b, 2)
  call begin_test('right_lower_t_n')
  call print_matrix('B', b, 2, 2, 2)
  call end_test()

  ! Test 12: M=0 quick return
  b(1) = 99.0d0
  call dtrsm('L', 'U', 'N', 'N', 0, 2, 1.0d0, a, 1, b, 1)
  call begin_test('m_zero')
  call print_matrix('B', b, 1, 1, 1)
  call end_test()

end program
