program test_dtrsv
  use test_utils
  implicit none
  double precision :: a(16), x(10)

  ! Test 1: upper, no-transpose, non-unit diag, N=3
  ! A (upper triangular, col-major):
  !   [2  3  4]
  !   [0  5  6]
  !   [0  0  7]
  ! b = [20, 28, 21] (= A * [1,2,3])
  ! Solve A*x = b => x = [1, 2, 3]
  a = 0.0d0; x = 0.0d0
  a(1) = 2.0d0; a(4) = 3.0d0; a(7) = 4.0d0
  a(5) = 5.0d0; a(8) = 6.0d0
  a(9) = 7.0d0
  x(1) = 20.0d0; x(2) = 28.0d0; x(3) = 21.0d0
  call dtrsv('U', 'N', 'N', 3, a, 3, x, 1)
  call begin_test('upper_n_nonunit')
  call print_array('x', x, 3)
  call end_test()

  ! Test 2: lower, no-transpose, non-unit diag, N=3
  ! A (lower triangular, col-major):
  !   [2  0  0]
  !   [3  5  0]
  !   [4  6  7]
  ! b = [2, 13, 37] (= A * [1,2,3])
  ! Solve A*x = b => x = [1, 2, 3]
  a = 0.0d0; x = 0.0d0
  a(1) = 2.0d0; a(2) = 3.0d0; a(3) = 4.0d0
  a(5) = 5.0d0; a(6) = 6.0d0
  a(9) = 7.0d0
  x(1) = 2.0d0; x(2) = 13.0d0; x(3) = 37.0d0
  call dtrsv('L', 'N', 'N', 3, a, 3, x, 1)
  call begin_test('lower_n_nonunit')
  call print_array('x', x, 3)
  call end_test()

  ! Test 3: upper, transpose, non-unit diag, N=3
  ! A^T = [2 0 0; 3 5 0; 4 6 7]
  ! b = [2, 13, 37] (= A^T * [1,2,3])
  ! Solve A^T*x = b => x = [1, 2, 3]
  a = 0.0d0; x = 0.0d0
  a(1) = 2.0d0; a(4) = 3.0d0; a(7) = 4.0d0
  a(5) = 5.0d0; a(8) = 6.0d0
  a(9) = 7.0d0
  x(1) = 2.0d0; x(2) = 13.0d0; x(3) = 37.0d0
  call dtrsv('U', 'T', 'N', 3, a, 3, x, 1)
  call begin_test('upper_t_nonunit')
  call print_array('x', x, 3)
  call end_test()

  ! Test 4: lower, transpose, non-unit diag, N=3
  ! A = [2 0 0; 3 5 0; 4 6 7]
  ! A^T = [2 3 4; 0 5 6; 0 0 7]
  ! b = [20, 28, 21] (= A^T * [1,2,3])
  ! Solve A^T*x = b => x = [1, 2, 3]
  a = 0.0d0; x = 0.0d0
  a(1) = 2.0d0; a(2) = 3.0d0; a(3) = 4.0d0
  a(5) = 5.0d0; a(6) = 6.0d0
  a(9) = 7.0d0
  x(1) = 20.0d0; x(2) = 28.0d0; x(3) = 21.0d0
  call dtrsv('L', 'T', 'N', 3, a, 3, x, 1)
  call begin_test('lower_t_nonunit')
  call print_array('x', x, 3)
  call end_test()

  ! Test 5: upper, no-transpose, unit diag, N=3
  ! Unit diag: A = [1 3 4; 0 1 6; 0 0 1]
  ! b = [19, 20, 3] (= A * [1,2,3])
  ! Solve A*x = b => x = [1, 2, 3]
  a = 0.0d0; x = 0.0d0
  a(1) = 99.0d0; a(4) = 3.0d0; a(7) = 4.0d0
  a(5) = 99.0d0; a(8) = 6.0d0
  a(9) = 99.0d0
  x(1) = 19.0d0; x(2) = 20.0d0; x(3) = 3.0d0
  call dtrsv('U', 'N', 'U', 3, a, 3, x, 1)
  call begin_test('upper_n_unit')
  call print_array('x', x, 3)
  call end_test()

  ! Test 6: lower, no-transpose, unit diag, N=3
  ! Unit diag: A = [1 0 0; 3 1 0; 4 6 1]
  ! b = [1, 5, 19] (= A * [1,2,3])
  ! Solve A*x = b => x = [1, 2, 3]
  a = 0.0d0; x = 0.0d0
  a(1) = 99.0d0; a(2) = 3.0d0; a(3) = 4.0d0
  a(5) = 99.0d0; a(6) = 6.0d0
  a(9) = 99.0d0
  x(1) = 1.0d0; x(2) = 5.0d0; x(3) = 19.0d0
  call dtrsv('L', 'N', 'U', 3, a, 3, x, 1)
  call begin_test('lower_n_unit')
  call print_array('x', x, 3)
  call end_test()

  ! Test 7: upper, transpose, unit diag, N=3
  ! Unit diag A = [1 3 4; 0 1 6; 0 0 1]
  ! A^T = [1 0 0; 3 1 0; 4 6 1]
  ! b = [1, 5, 19] (= A^T * [1,2,3])
  ! Solve A^T*x = b => x = [1, 2, 3]
  a = 0.0d0; x = 0.0d0
  a(1) = 99.0d0; a(4) = 3.0d0; a(7) = 4.0d0
  a(5) = 99.0d0; a(8) = 6.0d0
  a(9) = 99.0d0
  x(1) = 1.0d0; x(2) = 5.0d0; x(3) = 19.0d0
  call dtrsv('U', 'T', 'U', 3, a, 3, x, 1)
  call begin_test('upper_t_unit')
  call print_array('x', x, 3)
  call end_test()

  ! Test 8: lower, transpose, unit diag, N=3
  ! Unit diag A = [1 0 0; 3 1 0; 4 6 1]
  ! A^T = [1 3 4; 0 1 6; 0 0 1]
  ! b = [19, 20, 3] (= A^T * [1,2,3])
  ! Solve A^T*x = b => x = [1, 2, 3]
  a = 0.0d0; x = 0.0d0
  a(1) = 99.0d0; a(2) = 3.0d0; a(3) = 4.0d0
  a(5) = 99.0d0; a(6) = 6.0d0
  a(9) = 99.0d0
  x(1) = 19.0d0; x(2) = 20.0d0; x(3) = 3.0d0
  call dtrsv('L', 'T', 'U', 3, a, 3, x, 1)
  call begin_test('lower_t_unit')
  call print_array('x', x, 3)
  call end_test()

  ! Test 9: N=0 quick return
  x(1) = 99.0d0
  call dtrsv('U', 'N', 'N', 0, a, 1, x, 1)
  call begin_test('n_zero')
  call print_array('x', x, 1)
  call end_test()

  ! Test 10: N=1, non-unit diag
  a(1) = 5.0d0
  x(1) = 15.0d0
  call dtrsv('U', 'N', 'N', 1, a, 1, x, 1)
  call begin_test('n_one')
  call print_array('x', x, 1)
  call end_test()

  ! Test 11: non-unit stride incx=2, upper, no-trans, non-unit diag
  ! A same upper 3x3:  [2 3 4; 0 5 6; 0 0 7]
  ! b = [20, 28, 21] at stride 2: x(1)=20, x(3)=28, x(5)=21
  ! Solve => x(1)=1, x(3)=2, x(5)=3
  a = 0.0d0; x = 0.0d0
  a(1) = 2.0d0; a(4) = 3.0d0; a(7) = 4.0d0
  a(5) = 5.0d0; a(8) = 6.0d0
  a(9) = 7.0d0
  x(1) = 20.0d0; x(3) = 28.0d0; x(5) = 21.0d0
  call dtrsv('U', 'N', 'N', 3, a, 3, x, 2)
  call begin_test('stride')
  call print_array('x', x, 6)
  call end_test()

  ! Test 12: negative stride incx=-1, lower, no-trans, non-unit diag
  ! A (lower): [2 0 0; 3 5 0; 4 6 7]
  ! b = [2, 13, 37] => x = [1, 2, 3]
  ! With incx=-1, x stored in reverse: x(3)=b(1), x(2)=b(2), x(1)=b(3)
  a = 0.0d0; x = 0.0d0
  a(1) = 2.0d0; a(2) = 3.0d0; a(3) = 4.0d0
  a(5) = 5.0d0; a(6) = 6.0d0
  a(9) = 7.0d0
  x(1) = 37.0d0; x(2) = 13.0d0; x(3) = 2.0d0
  call dtrsv('L', 'N', 'N', 3, a, 3, x, -1)
  call begin_test('neg_stride')
  call print_array('x', x, 3)
  call end_test()

  ! Test 13: larger 4x4 upper, no-transpose, non-unit
  ! A (upper triangular, col-major):
  !   [1  2  3  4]
  !   [0  5  6  7]
  !   [0  0  8  9]
  !   [0  0  0 10]
  ! x = [1, 1, 1, 1] => b = A*x = [10, 18, 17, 10]
  a = 0.0d0; x = 0.0d0
  a(1) = 1.0d0; a(5) = 2.0d0; a(9) = 3.0d0; a(13) = 4.0d0
  a(6) = 5.0d0; a(10) = 6.0d0; a(14) = 7.0d0
  a(11) = 8.0d0; a(15) = 9.0d0
  a(16) = 10.0d0
  x(1) = 10.0d0; x(2) = 18.0d0; x(3) = 17.0d0; x(4) = 10.0d0
  call dtrsv('U', 'N', 'N', 4, a, 4, x, 1)
  call begin_test('upper_n_nonunit_4x4')
  call print_array('x', x, 4)
  call end_test()

  ! Test 14: N=1, unit diag
  a(1) = 99.0d0
  x(1) = 7.0d0
  call dtrsv('L', 'T', 'U', 1, a, 1, x, 1)
  call begin_test('n_one_unit')
  call print_array('x', x, 1)
  call end_test()

  ! Test 15: b has zeros — tests the x(j)!=0 branch skipping
  ! Upper, no-trans, non-unit, N=3
  ! A: [2 3 4; 0 5 6; 0 0 7]
  ! b = [0, 0, 21] => x(3) = 21/7 = 3, x(2) = (0 - 6*3)/5 = -3.6, x(1) = (0 - 3*(-3.6) - 4*3)/2 = -0.6
  a = 0.0d0; x = 0.0d0
  a(1) = 2.0d0; a(4) = 3.0d0; a(7) = 4.0d0
  a(5) = 5.0d0; a(8) = 6.0d0
  a(9) = 7.0d0
  x(1) = 0.0d0; x(2) = 0.0d0; x(3) = 21.0d0
  call dtrsv('U', 'N', 'N', 3, a, 3, x, 1)
  call begin_test('upper_n_zeros')
  call print_array('x', x, 3)
  call end_test()

end program
