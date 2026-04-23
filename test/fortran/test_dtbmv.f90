program test_dtbmv
  use test_utils
  implicit none
  double precision :: a(20), x(20)

  ! 4x4 triangular band matrix with K=1
  ! Upper triangle (band storage LDA=2):
  !   [ 2  3  0  0 ]  band: row0=superdiag, row1=diag
  !   [ 0  4  5  0 ]
  !   [ 0  0  6  7 ]
  !   [ 0  0  0  8 ]

  ! Test 1: upper, no-transpose, non-unit
  a = 0.0d0
  a(2) = 2.0d0  ! col1 diag
  a(3) = 3.0d0; a(4) = 4.0d0  ! col2
  a(5) = 5.0d0; a(6) = 6.0d0  ! col3
  a(7) = 7.0d0; a(8) = 8.0d0  ! col4
  x(1:4) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0/)
  call dtbmv('U', 'N', 'N', 4, 1, a, 2, x, 1)
  call begin_test('upper_notrans_nonunit')
  call print_array('x', x, 4)
  call end_test()

  ! Test 2: upper, transpose, non-unit
  x(1:4) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0/)
  call dtbmv('U', 'T', 'N', 4, 1, a, 2, x, 1)
  call begin_test('upper_trans_nonunit')
  call print_array('x', x, 4)
  call end_test()

  ! Test 3: upper, no-transpose, unit diagonal
  x(1:4) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0/)
  call dtbmv('U', 'N', 'U', 4, 1, a, 2, x, 1)
  call begin_test('upper_notrans_unit')
  call print_array('x', x, 4)
  call end_test()

  ! Lower triangle (band storage LDA=2):
  !   [ 2  0  0  0 ]  band: row0=diag, row1=subdiag
  !   [ 3  4  0  0 ]
  !   [ 0  5  6  0 ]
  !   [ 0  0  7  8 ]
  a = 0.0d0
  a(1) = 2.0d0; a(2) = 3.0d0  ! col1
  a(3) = 4.0d0; a(4) = 5.0d0  ! col2
  a(5) = 6.0d0; a(6) = 7.0d0  ! col3
  a(7) = 8.0d0                  ! col4

  ! Test 4: lower, no-transpose, non-unit
  x(1:4) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0/)
  call dtbmv('L', 'N', 'N', 4, 1, a, 2, x, 1)
  call begin_test('lower_notrans_nonunit')
  call print_array('x', x, 4)
  call end_test()

  ! Test 5: lower, transpose, non-unit
  x(1:4) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0/)
  call dtbmv('L', 'T', 'N', 4, 1, a, 2, x, 1)
  call begin_test('lower_trans_nonunit')
  call print_array('x', x, 4)
  call end_test()

  ! Test 6: n=0
  x(1) = 99.0d0
  call dtbmv('U', 'N', 'N', 0, 1, a, 2, x, 1)
  call begin_test('n_zero')
  call print_array('x', x, 1)
  call end_test()

  ! Test 7: stride=2, upper, no-transpose, non-unit
  a = 0.0d0
  a(2) = 2.0d0
  a(3) = 3.0d0; a(4) = 4.0d0
  a(5) = 5.0d0; a(6) = 6.0d0
  a(7) = 7.0d0; a(8) = 8.0d0
  x = 0.0d0
  x(1) = 1.0d0; x(3) = 2.0d0; x(5) = 3.0d0; x(7) = 4.0d0
  call dtbmv('U', 'N', 'N', 4, 1, a, 2, x, 2)
  call begin_test('stride')
  call print_array('x', x, 8)
  call end_test()

end program
