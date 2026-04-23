program test_dtpsv
  use test_utils
  implicit none
  double precision :: ap(10), x(10)

  ! =============================================
  ! 4x4 upper triangular matrix A:
  !   [2  3  4  5]
  !   [0  6  7  8]
  !   [0  0  9 10]
  !   [0  0  0 11]
  !
  ! Upper packed (column-major):
  !   col1: 2
  !   col2: 3, 6
  !   col3: 4, 7, 9
  !   col4: 5, 8, 10, 11
  ! AP = [2, 3, 6, 4, 7, 9, 5, 8, 10, 11]
  !
  ! x_known = [1, 2, 3, 4]
  ! b = A * x_known:
  !   b(1) = 2*1 + 3*2 + 4*3 + 5*4 = 40
  !   b(2) = 6*2 + 7*3 + 8*4 = 65
  !   b(3) = 9*3 + 10*4 = 67
  !   b(4) = 11*4 = 44
  ! =============================================

  ! Test 1: upper, no-transpose, non-unit, N=4
  ap(1)=2.0d0; ap(2)=3.0d0; ap(3)=6.0d0; ap(4)=4.0d0; ap(5)=7.0d0
  ap(6)=9.0d0; ap(7)=5.0d0; ap(8)=8.0d0; ap(9)=10.0d0; ap(10)=11.0d0
  x(1)=40.0d0; x(2)=65.0d0; x(3)=67.0d0; x(4)=44.0d0
  call dtpsv('U', 'N', 'N', 4, ap, x, 1)
  call begin_test('upper_n_nonunit')
  call print_array('x', x, 4)
  call end_test()

  ! =============================================
  ! 4x4 lower triangular matrix A:
  !   [2  0  0  0]
  !   [3  6  0  0]
  !   [4  7  9  0]
  !   [5  8 10 11]
  !
  ! Lower packed (column-major):
  !   col1: 2, 3, 4, 5
  !   col2: 6, 7, 8
  !   col3: 9, 10
  !   col4: 11
  ! AP = [2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
  !
  ! x_known = [1, 2, 3, 4]
  ! b = A * x_known:
  !   b(1) = 2*1 = 2
  !   b(2) = 3*1 + 6*2 = 15
  !   b(3) = 4*1 + 7*2 + 9*3 = 45
  !   b(4) = 5*1 + 8*2 + 10*3 + 11*4 = 95
  ! =============================================

  ! Test 2: lower, no-transpose, non-unit, N=4
  ap(1)=2.0d0; ap(2)=3.0d0; ap(3)=4.0d0; ap(4)=5.0d0; ap(5)=6.0d0
  ap(6)=7.0d0; ap(7)=8.0d0; ap(8)=9.0d0; ap(9)=10.0d0; ap(10)=11.0d0
  x(1)=2.0d0; x(2)=15.0d0; x(3)=45.0d0; x(4)=95.0d0
  call dtpsv('L', 'N', 'N', 4, ap, x, 1)
  call begin_test('lower_n_nonunit')
  call print_array('x', x, 4)
  call end_test()

  ! Test 3: upper, transpose, non-unit, N=4
  ! b = A^T * [1,2,3,4]:
  !   b(1) = 2*1 = 2
  !   b(2) = 3*1 + 6*2 = 15
  !   b(3) = 4*1 + 7*2 + 9*3 = 45
  !   b(4) = 5*1 + 8*2 + 10*3 + 11*4 = 95
  ap(1)=2.0d0; ap(2)=3.0d0; ap(3)=6.0d0; ap(4)=4.0d0; ap(5)=7.0d0
  ap(6)=9.0d0; ap(7)=5.0d0; ap(8)=8.0d0; ap(9)=10.0d0; ap(10)=11.0d0
  x(1)=2.0d0; x(2)=15.0d0; x(3)=45.0d0; x(4)=95.0d0
  call dtpsv('U', 'T', 'N', 4, ap, x, 1)
  call begin_test('upper_t_nonunit')
  call print_array('x', x, 4)
  call end_test()

  ! Test 4: lower, transpose, non-unit, N=4
  ! b = A^T * [1,2,3,4] where A is the lower matrix:
  !   b(1) = 2*1 + 3*2 + 4*3 + 5*4 = 40
  !   b(2) = 6*2 + 7*3 + 8*4 = 65
  !   b(3) = 9*3 + 10*4 = 67
  !   b(4) = 11*4 = 44
  ap(1)=2.0d0; ap(2)=3.0d0; ap(3)=4.0d0; ap(4)=5.0d0; ap(5)=6.0d0
  ap(6)=7.0d0; ap(7)=8.0d0; ap(8)=9.0d0; ap(9)=10.0d0; ap(10)=11.0d0
  x(1)=40.0d0; x(2)=65.0d0; x(3)=67.0d0; x(4)=44.0d0
  call dtpsv('L', 'T', 'N', 4, ap, x, 1)
  call begin_test('lower_t_nonunit')
  call print_array('x', x, 4)
  call end_test()

  ! Test 5: upper, no-transpose, unit, N=4
  ! Unit diag A = [1 3 4 5; 0 1 7 8; 0 0 1 10; 0 0 0 1]
  ! b = A * [1,2,3,4]:
  !   b(1) = 1 + 6 + 12 + 20 = 39
  !   b(2) = 2 + 21 + 32 = 55
  !   b(3) = 3 + 40 = 43
  !   b(4) = 4
  ap(1)=99.0d0; ap(2)=3.0d0; ap(3)=99.0d0; ap(4)=4.0d0; ap(5)=7.0d0
  ap(6)=99.0d0; ap(7)=5.0d0; ap(8)=8.0d0; ap(9)=10.0d0; ap(10)=99.0d0
  x(1)=39.0d0; x(2)=55.0d0; x(3)=43.0d0; x(4)=4.0d0
  call dtpsv('U', 'N', 'U', 4, ap, x, 1)
  call begin_test('upper_n_unit')
  call print_array('x', x, 4)
  call end_test()

  ! Test 6: lower, no-transpose, unit, N=4
  ! Unit diag A = [1 0 0 0; 3 1 0 0; 4 7 1 0; 5 8 10 1]
  ! b = A * [1,2,3,4]:
  !   b(1) = 1
  !   b(2) = 3 + 2 = 5
  !   b(3) = 4 + 14 + 3 = 21
  !   b(4) = 5 + 16 + 30 + 4 = 55
  ap(1)=99.0d0; ap(2)=3.0d0; ap(3)=4.0d0; ap(4)=5.0d0; ap(5)=99.0d0
  ap(6)=7.0d0; ap(7)=8.0d0; ap(8)=99.0d0; ap(9)=10.0d0; ap(10)=99.0d0
  x(1)=1.0d0; x(2)=5.0d0; x(3)=21.0d0; x(4)=55.0d0
  call dtpsv('L', 'N', 'U', 4, ap, x, 1)
  call begin_test('lower_n_unit')
  call print_array('x', x, 4)
  call end_test()

  ! Test 7: upper, transpose, unit, N=4
  ! Unit diag A = [1 3 4 5; 0 1 7 8; 0 0 1 10; 0 0 0 1]
  ! b = A^T * [1,2,3,4]:
  !   b(1) = 1
  !   b(2) = 3 + 2 = 5
  !   b(3) = 4 + 14 + 3 = 21
  !   b(4) = 5 + 16 + 30 + 4 = 55
  ap(1)=99.0d0; ap(2)=3.0d0; ap(3)=99.0d0; ap(4)=4.0d0; ap(5)=7.0d0
  ap(6)=99.0d0; ap(7)=5.0d0; ap(8)=8.0d0; ap(9)=10.0d0; ap(10)=99.0d0
  x(1)=1.0d0; x(2)=5.0d0; x(3)=21.0d0; x(4)=55.0d0
  call dtpsv('U', 'T', 'U', 4, ap, x, 1)
  call begin_test('upper_t_unit')
  call print_array('x', x, 4)
  call end_test()

  ! Test 8: lower, transpose, unit, N=4
  ! Unit diag A = [1 0 0 0; 3 1 0 0; 4 7 1 0; 5 8 10 1]
  ! b = A^T * [1,2,3,4]:
  !   b(1) = 1 + 6 + 12 + 20 = 39
  !   b(2) = 2 + 21 + 32 = 55
  !   b(3) = 3 + 40 = 43
  !   b(4) = 4
  ap(1)=99.0d0; ap(2)=3.0d0; ap(3)=4.0d0; ap(4)=5.0d0; ap(5)=99.0d0
  ap(6)=7.0d0; ap(7)=8.0d0; ap(8)=99.0d0; ap(9)=10.0d0; ap(10)=99.0d0
  x(1)=39.0d0; x(2)=55.0d0; x(3)=43.0d0; x(4)=4.0d0
  call dtpsv('L', 'T', 'U', 4, ap, x, 1)
  call begin_test('lower_t_unit')
  call print_array('x', x, 4)
  call end_test()

  ! Test 9: N=0 quick return
  x(1) = 99.0d0
  call dtpsv('U', 'N', 'N', 0, ap, x, 1)
  call begin_test('n_zero')
  call print_array('x', x, 1)
  call end_test()

  ! Test 10: N=1, non-unit diag
  ap(1) = 5.0d0
  x(1) = 15.0d0
  call dtpsv('U', 'N', 'N', 1, ap, x, 1)
  call begin_test('n_one')
  call print_array('x', x, 1)
  call end_test()

  ! Test 11: N=1, unit diag
  ap(1) = 99.0d0
  x(1) = 7.0d0
  call dtpsv('L', 'T', 'U', 1, ap, x, 1)
  call begin_test('n_one_unit')
  call print_array('x', x, 1)
  call end_test()

  ! Test 12: non-unit stride incx=2, upper, no-trans, non-unit, N=4
  ap(1)=2.0d0; ap(2)=3.0d0; ap(3)=6.0d0; ap(4)=4.0d0; ap(5)=7.0d0
  ap(6)=9.0d0; ap(7)=5.0d0; ap(8)=8.0d0; ap(9)=10.0d0; ap(10)=11.0d0
  x = 0.0d0
  x(1)=40.0d0; x(3)=65.0d0; x(5)=67.0d0; x(7)=44.0d0
  call dtpsv('U', 'N', 'N', 4, ap, x, 2)
  call begin_test('stride_2')
  call print_array('x', x, 8)
  call end_test()

  ! Test 13: negative stride incx=-1, lower, no-trans, non-unit, N=4
  ap(1)=2.0d0; ap(2)=3.0d0; ap(3)=4.0d0; ap(4)=5.0d0; ap(5)=6.0d0
  ap(6)=7.0d0; ap(7)=8.0d0; ap(8)=9.0d0; ap(9)=10.0d0; ap(10)=11.0d0
  x = 0.0d0
  x(1)=95.0d0; x(2)=45.0d0; x(3)=15.0d0; x(4)=2.0d0
  call dtpsv('L', 'N', 'N', 4, ap, x, -1)
  call begin_test('neg_stride')
  call print_array('x', x, 4)
  call end_test()

  ! Test 14: upper, no-transpose with zero RHS
  ap(1)=2.0d0; ap(2)=3.0d0; ap(3)=6.0d0; ap(4)=4.0d0; ap(5)=7.0d0
  ap(6)=9.0d0; ap(7)=5.0d0; ap(8)=8.0d0; ap(9)=10.0d0; ap(10)=11.0d0
  x(1)=0.0d0; x(2)=0.0d0; x(3)=0.0d0; x(4)=44.0d0
  call dtpsv('U', 'N', 'N', 4, ap, x, 1)
  call begin_test('upper_n_zeros')
  call print_array('x', x, 4)
  call end_test()

  ! Test 15: lower, transpose, non-unit, stride=2, N=4
  ap(1)=2.0d0; ap(2)=3.0d0; ap(3)=4.0d0; ap(4)=5.0d0; ap(5)=6.0d0
  ap(6)=7.0d0; ap(7)=8.0d0; ap(8)=9.0d0; ap(9)=10.0d0; ap(10)=11.0d0
  x = 0.0d0
  x(1)=40.0d0; x(3)=65.0d0; x(5)=67.0d0; x(7)=44.0d0
  call dtpsv('L', 'T', 'N', 4, ap, x, 2)
  call begin_test('lower_t_stride_2')
  call print_array('x', x, 8)
  call end_test()

end program
