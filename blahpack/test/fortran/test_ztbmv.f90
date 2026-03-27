program test_ztbmv
  use test_utils
  implicit none
  complex*16 :: a(12), x(8)
  double precision :: a_r(24), x_r(16)
  equivalence (a, a_r)
  equivalence (x, x_r)

  ! 4x4 upper triangular band matrix with K=2
  ! Upper band storage (LDA=3):
  ! Row 1 (2nd superdiag): *      *      a13    a24
  ! Row 2 (1st superdiag): *      a12    a23    a34
  ! Row 3 (diagonal):      a11    a22    a33    a44

  ! Test 1: UPLO='U', TRANS='N', DIAG='N'
  a = (0.0d0, 0.0d0)
  a(3) = (2.0d0, 1.0d0)     ! a11
  a(5) = (1.0d0, 0.5d0)     ! a12
  a(6) = (3.0d0, -1.0d0)    ! a22
  a(7) = (4.0d0, 2.0d0)     ! a13
  a(8) = (5.0d0, 0.0d0)     ! a23
  a(9) = (6.0d0, -0.5d0)    ! a33
  a(11) = (7.0d0, 1.0d0)    ! a34
  a(12) = (8.0d0, -2.0d0)   ! a44

  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0); x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0); x(4) = (4.0d0, 0.5d0)
  call ztbmv('U', 'N', 'N', 4, 2, a, 3, x, 1)
  call begin_test('upper_no_trans_nonunit')
  call print_array('x', x_r, 8)
  call end_test()

  ! Test 2: UPLO='U', TRANS='T'
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0); x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0); x(4) = (4.0d0, 0.5d0)
  call ztbmv('U', 'T', 'N', 4, 2, a, 3, x, 1)
  call begin_test('upper_trans_nonunit')
  call print_array('x', x_r, 8)
  call end_test()

  ! Test 3: UPLO='U', TRANS='C' (conjugate transpose)
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0); x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0); x(4) = (4.0d0, 0.5d0)
  call ztbmv('U', 'C', 'N', 4, 2, a, 3, x, 1)
  call begin_test('upper_conj_trans_nonunit')
  call print_array('x', x_r, 8)
  call end_test()

  ! Test 4: UPLO='U', DIAG='U' (unit diagonal)
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0); x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0); x(4) = (4.0d0, 0.5d0)
  call ztbmv('U', 'N', 'U', 4, 2, a, 3, x, 1)
  call begin_test('upper_no_trans_unit')
  call print_array('x', x_r, 8)
  call end_test()

  ! Test 5: UPLO='L', TRANS='N', DIAG='N'
  ! Lower band storage (LDA=3, K=2):
  ! Row 1 (diagonal):      a11    a22    a33    a44
  ! Row 2 (1st subdiag):   a21    a32    a43    *
  ! Row 3 (2nd subdiag):   a31    a42    *      *
  a = (0.0d0, 0.0d0)
  a(1) = (2.0d0, 1.0d0)     ! a11
  a(2) = (1.0d0, 0.5d0)     ! a21
  a(3) = (4.0d0, 2.0d0)     ! a31
  a(4) = (3.0d0, -1.0d0)    ! a22
  a(5) = (5.0d0, 0.0d0)     ! a32
  a(6) = (7.0d0, 1.0d0)     ! a42
  a(7) = (6.0d0, -0.5d0)    ! a33
  a(8) = (8.0d0, -2.0d0)    ! a43
  a(10) = (9.0d0, 0.5d0)    ! a44

  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0); x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0); x(4) = (4.0d0, 0.5d0)
  call ztbmv('L', 'N', 'N', 4, 2, a, 3, x, 1)
  call begin_test('lower_no_trans_nonunit')
  call print_array('x', x_r, 8)
  call end_test()

  ! Test 6: UPLO='L', TRANS='T'
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0); x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0); x(4) = (4.0d0, 0.5d0)
  call ztbmv('L', 'T', 'N', 4, 2, a, 3, x, 1)
  call begin_test('lower_trans_nonunit')
  call print_array('x', x_r, 8)
  call end_test()

  ! Test 7: UPLO='L', TRANS='C'
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0); x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0); x(4) = (4.0d0, 0.5d0)
  call ztbmv('L', 'C', 'N', 4, 2, a, 3, x, 1)
  call begin_test('lower_conj_trans_nonunit')
  call print_array('x', x_r, 8)
  call end_test()

  ! Test 8: N=0 quick return
  x(1) = (99.0d0, 0.0d0)
  call ztbmv('U', 'N', 'N', 0, 2, a, 3, x, 1)
  call begin_test('n_zero')
  call print_array('x', x_r, 2)
  call end_test()

  ! Test 9: non-unit stride incx=2
  a = (0.0d0, 0.0d0)
  a(3) = (2.0d0, 1.0d0); a(5) = (1.0d0, 0.5d0); a(6) = (3.0d0, -1.0d0)
  a(7) = (4.0d0, 2.0d0); a(8) = (5.0d0, 0.0d0); a(9) = (6.0d0, -0.5d0)
  a(11) = (7.0d0, 1.0d0); a(12) = (8.0d0, -2.0d0)
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0); x(3) = (2.0d0, 1.0d0)
  x(5) = (3.0d0, -1.0d0); x(7) = (4.0d0, 0.5d0)
  call ztbmv('U', 'N', 'N', 4, 2, a, 3, x, 2)
  call begin_test('upper_stride_2')
  call print_array('x', x_r, 16)
  call end_test()

  ! Test 10: 1x1 scalar case
  a = (0.0d0, 0.0d0); a(1) = (5.0d0, 2.0d0)
  x = (0.0d0, 0.0d0); x(1) = (3.0d0, -1.0d0)
  call ztbmv('U', 'N', 'N', 1, 0, a, 1, x, 1)
  call begin_test('scalar')
  call print_array('x', x_r, 2)
  call end_test()

  ! Test 11: UPLO='L', DIAG='U' (unit diagonal)
  a = (0.0d0, 0.0d0)
  a(1) = (2.0d0, 1.0d0); a(2) = (1.0d0, 0.5d0); a(3) = (4.0d0, 2.0d0)
  a(4) = (3.0d0, -1.0d0); a(5) = (5.0d0, 0.0d0); a(6) = (7.0d0, 1.0d0)
  a(7) = (6.0d0, -0.5d0); a(8) = (8.0d0, -2.0d0)
  a(10) = (9.0d0, 0.5d0)
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0); x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0); x(4) = (4.0d0, 0.5d0)
  call ztbmv('L', 'N', 'U', 4, 2, a, 3, x, 1)
  call begin_test('lower_no_trans_unit')
  call print_array('x', x_r, 8)
  call end_test()

end program
