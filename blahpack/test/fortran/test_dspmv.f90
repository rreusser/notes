program test_dspmv
  use test_utils
  implicit none
  double precision :: ap(20), x(20), y(20)

  ! 4x4 symmetric matrix:
  !   [ 1  2  3  4 ]
  !   [ 2  5  6  7 ]
  !   [ 3  6  8  9 ]
  !   [ 4  7  9 10 ]

  ! Test 1: upper packed storage, alpha=1, beta=0
  ! Upper packed: col-major upper triangle
  ! col 1: 1
  ! col 2: 2, 5
  ! col 3: 3, 6, 8
  ! col 4: 4, 7, 9, 10
  ap = 0.0d0; x = 0.0d0; y = 0.0d0
  ap(1) = 1.0d0
  ap(2) = 2.0d0; ap(3) = 5.0d0
  ap(4) = 3.0d0; ap(5) = 6.0d0; ap(6) = 8.0d0
  ap(7) = 4.0d0; ap(8) = 7.0d0; ap(9) = 9.0d0; ap(10) = 10.0d0
  x(1:4) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0/)
  y = 0.0d0
  call dspmv('U', 4, 1.0d0, ap, x, 1, 0.0d0, y, 1)
  call begin_test('upper_basic')
  call print_array('y', y, 4)
  call end_test()

  ! Test 2: lower packed storage
  ! Lower packed: col-major lower triangle
  ! col 1: 1, 2, 3, 4
  ! col 2: 5, 6, 7
  ! col 3: 8, 9
  ! col 4: 10
  ap = 0.0d0
  ap(1) = 1.0d0; ap(2) = 2.0d0; ap(3) = 3.0d0; ap(4) = 4.0d0
  ap(5) = 5.0d0; ap(6) = 6.0d0; ap(7) = 7.0d0
  ap(8) = 8.0d0; ap(9) = 9.0d0
  ap(10) = 10.0d0
  x(1:4) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0/)
  y = 0.0d0
  call dspmv('L', 4, 1.0d0, ap, x, 1, 0.0d0, y, 1)
  call begin_test('lower_basic')
  call print_array('y', y, 4)
  call end_test()

  ! Test 3: alpha=2, beta=0.5
  ap = 0.0d0
  ap(1) = 1.0d0
  ap(2) = 2.0d0; ap(3) = 5.0d0
  ap(4) = 3.0d0; ap(5) = 6.0d0; ap(6) = 8.0d0
  ap(7) = 4.0d0; ap(8) = 7.0d0; ap(9) = 9.0d0; ap(10) = 10.0d0
  x(1:4) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0/)
  y(1:4) = (/10.0d0, 20.0d0, 30.0d0, 40.0d0/)
  call dspmv('U', 4, 2.0d0, ap, x, 1, 0.5d0, y, 1)
  call begin_test('alpha_beta')
  call print_array('y', y, 4)
  call end_test()

  ! Test 4: n=0
  y(1) = 99.0d0
  call dspmv('U', 0, 1.0d0, ap, x, 1, 0.0d0, y, 1)
  call begin_test('n_zero')
  call print_array('y', y, 1)
  call end_test()

  ! Test 5: n=1
  ap = 0.0d0; x = 0.0d0; y = 0.0d0
  ap(1) = 3.0d0; x(1) = 5.0d0; y(1) = 7.0d0
  call dspmv('U', 1, 2.0d0, ap, x, 1, 3.0d0, y, 1)
  call begin_test('n_one')
  call print_array('y', y, 1)
  call end_test()

  ! Test 6: alpha=0 (just scales y by beta)
  y(1) = 10.0d0; y(2) = 20.0d0; y(3) = 30.0d0; y(4) = 40.0d0
  call dspmv('U', 4, 0.0d0, ap, x, 1, 2.0d0, y, 1)
  call begin_test('alpha_zero')
  call print_array('y', y, 4)
  call end_test()

  ! Test 7: beta=0, uplo='L'
  ap = 0.0d0
  ap(1) = 1.0d0; ap(2) = 2.0d0; ap(3) = 3.0d0
  ap(4) = 4.0d0; ap(5) = 5.0d0
  ap(6) = 6.0d0
  x(1) = 1.0d0; x(2) = 1.0d0; x(3) = 1.0d0
  y(1) = 99.0d0; y(2) = 88.0d0; y(3) = 77.0d0
  call dspmv('L', 3, 1.0d0, ap, x, 1, 0.0d0, y, 1)
  call begin_test('lower_beta_zero')
  call print_array('y', y, 3)
  call end_test()

  ! Test 8: beta=1, uplo='U'
  ap = 0.0d0
  ap(1) = 1.0d0
  ap(2) = 2.0d0; ap(3) = 4.0d0
  ap(4) = 3.0d0; ap(5) = 5.0d0; ap(6) = 6.0d0
  x(1) = 1.0d0; x(2) = 1.0d0; x(3) = 1.0d0
  y(1) = 10.0d0; y(2) = 20.0d0; y(3) = 30.0d0
  call dspmv('U', 3, 1.0d0, ap, x, 1, 1.0d0, y, 1)
  call begin_test('upper_beta_one')
  call print_array('y', y, 3)
  call end_test()

  ! Test 9: stride=2, uplo='U'
  ap = 0.0d0
  ap(1) = 1.0d0
  ap(2) = 2.0d0; ap(3) = 5.0d0
  ap(4) = 3.0d0; ap(5) = 6.0d0; ap(6) = 8.0d0
  ap(7) = 4.0d0; ap(8) = 7.0d0; ap(9) = 9.0d0; ap(10) = 10.0d0
  x = 0.0d0; y = 0.0d0
  x(1) = 1.0d0; x(3) = 2.0d0; x(5) = 3.0d0; x(7) = 4.0d0
  call dspmv('U', 4, 1.0d0, ap, x, 2, 0.0d0, y, 2)
  call begin_test('stride')
  call print_array('y', y, 8)
  call end_test()

  ! Test 10: stride=2, uplo='L', alpha=2, beta=0.5
  ap = 0.0d0
  ap(1) = 1.0d0; ap(2) = 2.0d0; ap(3) = 3.0d0
  ap(4) = 4.0d0; ap(5) = 5.0d0
  ap(6) = 6.0d0
  x = 0.0d0; y = 0.0d0
  x(1) = 1.0d0; x(3) = 2.0d0; x(5) = 3.0d0
  y(1) = 10.0d0; y(3) = 20.0d0; y(5) = 30.0d0
  call dspmv('L', 3, 2.0d0, ap, x, 2, 0.5d0, y, 2)
  call begin_test('lower_stride_alpha_beta')
  call print_array('y', y, 6)
  call end_test()

  ! Test 11: negative stride, uplo='U'
  ! Symmetric: [[1,2,3],[2,4,5],[3,5,6]]
  ap = 0.0d0
  ap(1) = 1.0d0
  ap(2) = 2.0d0; ap(3) = 4.0d0
  ap(4) = 3.0d0; ap(5) = 5.0d0; ap(6) = 6.0d0
  x = 0.0d0; y = 0.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0
  call dspmv('U', 3, 1.0d0, ap, x, -1, 0.0d0, y, -1)
  call begin_test('negative_stride')
  call print_array('y', y, 3)
  call end_test()

  ! Test 12: negative stride=2, uplo='L'
  ap = 0.0d0
  ap(1) = 1.0d0; ap(2) = 2.0d0; ap(3) = 3.0d0
  ap(4) = 4.0d0; ap(5) = 5.0d0
  ap(6) = 6.0d0
  x = 0.0d0; y = 0.0d0
  x(1) = 1.0d0; x(3) = 2.0d0; x(5) = 3.0d0
  y(1) = 10.0d0; y(3) = 20.0d0; y(5) = 30.0d0
  call dspmv('L', 3, 1.0d0, ap, x, -2, 0.5d0, y, -2)
  call begin_test('lower_negative_stride')
  call print_array('y', y, 6)
  call end_test()

end program
