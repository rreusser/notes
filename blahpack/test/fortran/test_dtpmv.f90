program test_dtpmv
  use test_utils
  implicit none
  double precision :: ap(20), x(20)

  ! 4x4 upper triangular matrix:
  !   [ 2  3  5  8 ]
  !   [ 0  4  6  9 ]
  !   [ 0  0  7 10 ]
  !   [ 0  0  0 11 ]
  ! Upper packed: 2, 3, 4, 5, 6, 7, 8, 9, 10, 11

  ! Test 1: upper, no-transpose, non-unit
  ap(1) = 2.0d0; ap(2) = 3.0d0; ap(3) = 4.0d0
  ap(4) = 5.0d0; ap(5) = 6.0d0; ap(6) = 7.0d0
  ap(7) = 8.0d0; ap(8) = 9.0d0; ap(9) = 10.0d0; ap(10) = 11.0d0
  x(1:4) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0/)
  call dtpmv('U', 'N', 'N', 4, ap, x, 1)
  call begin_test('upper_notrans_nonunit')
  call print_array('x', x, 4)
  call end_test()

  ! Test 2: upper, transpose, non-unit
  x(1:4) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0/)
  call dtpmv('U', 'T', 'N', 4, ap, x, 1)
  call begin_test('upper_trans_nonunit')
  call print_array('x', x, 4)
  call end_test()

  ! Test 3: upper, no-transpose, unit diagonal
  x(1:4) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0/)
  call dtpmv('U', 'N', 'U', 4, ap, x, 1)
  call begin_test('upper_notrans_unit')
  call print_array('x', x, 4)
  call end_test()

  ! 4x4 lower triangular matrix:
  !   [ 2  0  0  0 ]
  !   [ 3  5  0  0 ]
  !   [ 4  6  8  0 ]
  !   [ 7  9 10 11 ]
  ! Lower packed: 2, 3, 4, 7, 5, 6, 9, 8, 10, 11
  ap(1) = 2.0d0; ap(2) = 3.0d0; ap(3) = 4.0d0; ap(4) = 7.0d0
  ap(5) = 5.0d0; ap(6) = 6.0d0; ap(7) = 9.0d0
  ap(8) = 8.0d0; ap(9) = 10.0d0
  ap(10) = 11.0d0

  ! Test 4: lower, no-transpose, non-unit
  x(1:4) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0/)
  call dtpmv('L', 'N', 'N', 4, ap, x, 1)
  call begin_test('lower_notrans_nonunit')
  call print_array('x', x, 4)
  call end_test()

  ! Test 5: lower, transpose, non-unit
  x(1:4) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0/)
  call dtpmv('L', 'T', 'N', 4, ap, x, 1)
  call begin_test('lower_trans_nonunit')
  call print_array('x', x, 4)
  call end_test()

  ! Test 6: n=0
  x(1) = 99.0d0
  call dtpmv('U', 'N', 'N', 0, ap, x, 1)
  call begin_test('n_zero')
  call print_array('x', x, 1)
  call end_test()

  ! Test 7: stride=2
  ap(1) = 2.0d0; ap(2) = 3.0d0; ap(3) = 4.0d0
  ap(4) = 5.0d0; ap(5) = 6.0d0; ap(6) = 7.0d0
  ap(7) = 8.0d0; ap(8) = 9.0d0; ap(9) = 10.0d0; ap(10) = 11.0d0
  x = 0.0d0
  x(1) = 1.0d0; x(3) = 2.0d0; x(5) = 3.0d0; x(7) = 4.0d0
  call dtpmv('U', 'N', 'N', 4, ap, x, 2)
  call begin_test('stride')
  call print_array('x', x, 8)
  call end_test()

end program
