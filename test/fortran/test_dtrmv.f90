program test_dtrmv
  use test_utils
  implicit none
  double precision :: a(16), x(10)

  ! Test 1: upper, no-transpose, non-unit diag, N=3
  ! A (upper triangular, col-major):
  !   [2  3  4]
  !   [0  5  6]
  !   [0  0  7]
  ! x = [1, 2, 3]
  ! x := A*x => x(1) = 2*1+3*2+4*3 = 20, x(2) = 5*2+6*3 = 28, x(3) = 7*3 = 21
  a = 0.0d0; x = 0.0d0
  a(1) = 2.0d0; a(4) = 3.0d0; a(7) = 4.0d0
  a(5) = 5.0d0; a(8) = 6.0d0
  a(9) = 7.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0
  call dtrmv('U', 'N', 'N', 3, a, 3, x, 1)
  call begin_test('upper_n_nonunit')
  call print_array('x', x, 3)
  call end_test()

  ! Test 2: lower, no-transpose, non-unit diag, N=3
  ! A (lower triangular, col-major):
  !   [2  0  0]
  !   [3  5  0]
  !   [4  6  7]
  ! x = [1, 2, 3]
  ! x := A*x => x(1) = 2*1 = 2, x(2) = 3*1+5*2 = 13, x(3) = 4*1+6*2+7*3 = 37
  a = 0.0d0; x = 0.0d0
  a(1) = 2.0d0; a(2) = 3.0d0; a(3) = 4.0d0
  a(5) = 5.0d0; a(6) = 6.0d0
  a(9) = 7.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0
  call dtrmv('L', 'N', 'N', 3, a, 3, x, 1)
  call begin_test('lower_n_nonunit')
  call print_array('x', x, 3)
  call end_test()

  ! Test 3: upper, transpose, non-unit diag, N=3
  ! A^T * x where A is upper triangular
  ! A^T = [2 0 0; 3 5 0; 4 6 7]
  ! x = [1, 2, 3]
  ! x := A^T*x => x(1) = 2*1 = 2, x(2) = 3*1+5*2 = 13, x(3) = 4*1+6*2+7*3 = 37
  a = 0.0d0; x = 0.0d0
  a(1) = 2.0d0; a(4) = 3.0d0; a(7) = 4.0d0
  a(5) = 5.0d0; a(8) = 6.0d0
  a(9) = 7.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0
  call dtrmv('U', 'T', 'N', 3, a, 3, x, 1)
  call begin_test('upper_t_nonunit')
  call print_array('x', x, 3)
  call end_test()

  ! Test 4: lower, transpose, non-unit diag, N=3
  ! A^T = [2 3 4; 0 5 6; 0 0 7]
  ! x = [1, 2, 3]
  ! x := A^T*x => x(1) = 2*1+3*2+4*3 = 20, x(2) = 5*2+6*3 = 28, x(3) = 7*3 = 21
  a = 0.0d0; x = 0.0d0
  a(1) = 2.0d0; a(2) = 3.0d0; a(3) = 4.0d0
  a(5) = 5.0d0; a(6) = 6.0d0
  a(9) = 7.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0
  call dtrmv('L', 'T', 'N', 3, a, 3, x, 1)
  call begin_test('lower_t_nonunit')
  call print_array('x', x, 3)
  call end_test()

  ! Test 5: upper, no-transpose, unit diag, N=3
  ! Unit diag means diagonal is treated as 1
  ! A = [1 3 4; 0 1 6; 0 0 1]
  ! x = [1, 2, 3]
  ! x := A*x => x(1) = 1*1+3*2+4*3 = 19, x(2) = 1*2+6*3 = 20, x(3) = 1*3 = 3
  a = 0.0d0; x = 0.0d0
  a(1) = 99.0d0; a(4) = 3.0d0; a(7) = 4.0d0
  a(5) = 99.0d0; a(8) = 6.0d0
  a(9) = 99.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0
  call dtrmv('U', 'N', 'U', 3, a, 3, x, 1)
  call begin_test('upper_n_unit')
  call print_array('x', x, 3)
  call end_test()

  ! Test 6: lower, transpose, unit diag, N=3
  ! Unit diag: A = [1 0 0; 3 1 0; 4 6 1]
  ! A^T = [1 3 4; 0 1 6; 0 0 1]
  ! x = [1, 2, 3]
  ! x := A^T*x => x(1) = 1+6+12 = 19, x(2) = 2+18 = 20, x(3) = 3
  a = 0.0d0; x = 0.0d0
  a(1) = 99.0d0; a(2) = 3.0d0; a(3) = 4.0d0
  a(5) = 99.0d0; a(6) = 6.0d0
  a(9) = 99.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0
  call dtrmv('L', 'T', 'U', 3, a, 3, x, 1)
  call begin_test('lower_t_unit')
  call print_array('x', x, 3)
  call end_test()

  ! Test 7: N=0 quick return
  x(1) = 99.0d0
  call dtrmv('U', 'N', 'N', 0, a, 1, x, 1)
  call begin_test('n_zero')
  call print_array('x', x, 1)
  call end_test()

  ! Test 8: N=1, non-unit diag
  a(1) = 5.0d0
  x(1) = 3.0d0
  call dtrmv('U', 'N', 'N', 1, a, 1, x, 1)
  call begin_test('n_one')
  call print_array('x', x, 1)
  call end_test()

  ! Test 9: non-unit stride incx=2, upper, no-trans, non-unit diag
  ! A same upper 3x3:  [2 3 4; 0 5 6; 0 0 7]
  ! x at stride 2: x(1)=1, x(3)=2, x(5)=3
  a = 0.0d0; x = 0.0d0
  a(1) = 2.0d0; a(4) = 3.0d0; a(7) = 4.0d0
  a(5) = 5.0d0; a(8) = 6.0d0
  a(9) = 7.0d0
  x(1) = 1.0d0; x(3) = 2.0d0; x(5) = 3.0d0
  call dtrmv('U', 'N', 'N', 3, a, 3, x, 2)
  call begin_test('stride')
  call print_array('x', x, 6)
  call end_test()

  ! Test 10: negative stride incx=-1, lower, no-trans, non-unit diag
  a = 0.0d0; x = 0.0d0
  a(1) = 2.0d0; a(2) = 3.0d0; a(3) = 4.0d0
  a(5) = 5.0d0; a(6) = 6.0d0
  a(9) = 7.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0
  call dtrmv('L', 'N', 'N', 3, a, 3, x, -1)
  call begin_test('neg_stride')
  call print_array('x', x, 3)
  call end_test()

end program
