program test_ztpmv
  use test_utils
  implicit none
  complex*16 :: ap(15), x(8)
  double precision :: ap_r(30), x_r(16)
  equivalence (ap, ap_r)
  equivalence (x, x_r)

  ! 4x4 upper triangular matrix in packed form:
  ! Upper packed: [a11, a12, a22, a13, a23, a33, a14, a24, a34, a44]

  ! Test 1: UPLO='U', TRANS='N', DIAG='N'
  ap = (0.0d0, 0.0d0)
  ap(1) = (2.0d0, 1.0d0)    ! a11
  ap(2) = (1.0d0, 0.5d0)    ! a12
  ap(3) = (3.0d0, -1.0d0)   ! a22
  ap(4) = (4.0d0, 2.0d0)    ! a13
  ap(5) = (5.0d0, 0.0d0)    ! a23
  ap(6) = (6.0d0, -0.5d0)   ! a33
  ap(7) = (7.0d0, 1.0d0)    ! a14
  ap(8) = (8.0d0, -2.0d0)   ! a24
  ap(9) = (9.0d0, 0.5d0)    ! a34
  ap(10) = (10.0d0, -1.0d0) ! a44

  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0); x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0); x(4) = (4.0d0, 0.5d0)
  call ztpmv('U', 'N', 'N', 4, ap, x, 1)
  call begin_test('upper_no_trans_nonunit')
  call print_array('x', x_r, 8)
  call end_test()

  ! Test 2: UPLO='U', TRANS='T'
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0); x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0); x(4) = (4.0d0, 0.5d0)
  call ztpmv('U', 'T', 'N', 4, ap, x, 1)
  call begin_test('upper_trans_nonunit')
  call print_array('x', x_r, 8)
  call end_test()

  ! Test 3: UPLO='U', TRANS='C'
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0); x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0); x(4) = (4.0d0, 0.5d0)
  call ztpmv('U', 'C', 'N', 4, ap, x, 1)
  call begin_test('upper_conj_trans_nonunit')
  call print_array('x', x_r, 8)
  call end_test()

  ! Test 4: UPLO='U', DIAG='U' (unit diagonal)
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0); x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0); x(4) = (4.0d0, 0.5d0)
  call ztpmv('U', 'N', 'U', 4, ap, x, 1)
  call begin_test('upper_no_trans_unit')
  call print_array('x', x_r, 8)
  call end_test()

  ! Test 5: UPLO='L', TRANS='N', DIAG='N'
  ! Lower packed: [a11, a21, a31, a41, a22, a32, a42, a33, a43, a44]
  ap = (0.0d0, 0.0d0)
  ap(1) = (2.0d0, 1.0d0)    ! a11
  ap(2) = (1.0d0, 0.5d0)    ! a21
  ap(3) = (4.0d0, 2.0d0)    ! a31
  ap(4) = (7.0d0, 1.0d0)    ! a41
  ap(5) = (3.0d0, -1.0d0)   ! a22
  ap(6) = (5.0d0, 0.0d0)    ! a32
  ap(7) = (8.0d0, -2.0d0)   ! a42
  ap(8) = (6.0d0, -0.5d0)   ! a33
  ap(9) = (9.0d0, 0.5d0)    ! a43
  ap(10) = (10.0d0, -1.0d0) ! a44

  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0); x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0); x(4) = (4.0d0, 0.5d0)
  call ztpmv('L', 'N', 'N', 4, ap, x, 1)
  call begin_test('lower_no_trans_nonunit')
  call print_array('x', x_r, 8)
  call end_test()

  ! Test 6: UPLO='L', TRANS='T'
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0); x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0); x(4) = (4.0d0, 0.5d0)
  call ztpmv('L', 'T', 'N', 4, ap, x, 1)
  call begin_test('lower_trans_nonunit')
  call print_array('x', x_r, 8)
  call end_test()

  ! Test 7: UPLO='L', TRANS='C'
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0); x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0); x(4) = (4.0d0, 0.5d0)
  call ztpmv('L', 'C', 'N', 4, ap, x, 1)
  call begin_test('lower_conj_trans_nonunit')
  call print_array('x', x_r, 8)
  call end_test()

  ! Test 8: UPLO='L', DIAG='U' (unit diagonal)
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0); x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0); x(4) = (4.0d0, 0.5d0)
  call ztpmv('L', 'N', 'U', 4, ap, x, 1)
  call begin_test('lower_no_trans_unit')
  call print_array('x', x_r, 8)
  call end_test()

  ! Test 9: N=0 quick return
  x(1) = (99.0d0, 0.0d0)
  call ztpmv('U', 'N', 'N', 0, ap, x, 1)
  call begin_test('n_zero')
  call print_array('x', x_r, 2)
  call end_test()

  ! Test 10: non-unit stride incx=2
  ap = (0.0d0, 0.0d0)
  ap(1) = (2.0d0, 1.0d0); ap(2) = (1.0d0, 0.5d0); ap(3) = (3.0d0, -1.0d0)
  ap(4) = (4.0d0, 2.0d0); ap(5) = (5.0d0, 0.0d0); ap(6) = (6.0d0, -0.5d0)
  ap(7) = (7.0d0, 1.0d0); ap(8) = (8.0d0, -2.0d0); ap(9) = (9.0d0, 0.5d0)
  ap(10) = (10.0d0, -1.0d0)
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0); x(3) = (2.0d0, 1.0d0)
  x(5) = (3.0d0, -1.0d0); x(7) = (4.0d0, 0.5d0)
  call ztpmv('U', 'N', 'N', 4, ap, x, 2)
  call begin_test('upper_stride_2')
  call print_array('x', x_r, 16)
  call end_test()

  ! Test 11: 1x1 scalar
  ap = (0.0d0, 0.0d0); ap(1) = (5.0d0, 2.0d0)
  x = (0.0d0, 0.0d0); x(1) = (3.0d0, -1.0d0)
  call ztpmv('U', 'N', 'N', 1, ap, x, 1)
  call begin_test('scalar')
  call print_array('x', x_r, 2)
  call end_test()

end program
