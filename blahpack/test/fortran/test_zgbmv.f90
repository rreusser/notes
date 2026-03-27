program test_zgbmv
  use test_utils
  implicit none
  ! Band storage for 4x4 matrix with KL=1, KU=2: band has KL+KU+1=4 rows
  complex*16 :: a(20), x(10), y(10)
  double precision :: a_r(40), x_r(20), y_r(20)
  equivalence (a, a_r)
  equivalence (x, x_r)
  equivalence (y, y_r)

  ! 4x4 general matrix with KL=1, KU=2:
  ! Full:  [a11  a12  a13   0 ]
  !        [a21  a22  a23  a24]
  !        [ 0   a32  a33  a34]
  !        [ 0    0   a43  a44]
  !
  ! Band storage (LDA=4, KL+KU+1=4 rows, 4 cols):
  !   A_band(KU+1+i-j, j) = A_full(i,j)  (1-based)
  !   With KU=2:
  !   Row 1 (2nd superdiag): *    *    a13  a24
  !   Row 2 (1st superdiag): *    a12  a23  a34
  !   Row 3 (diagonal):      a11  a22  a33  a44
  !   Row 4 (subdiag):       a21  a32  a43  *
  !
  ! Fortran col-major: A_band(i,j) = a((j-1)*LDA + i), LDA=4

  ! Test 1: TRANS='N', alpha=(1,0), beta=(0,0), basic
  a = (0.0d0, 0.0d0)
  ! Col 1: rows 3-4 valid (diagonal and sub)
  a(3) = (1.0d0, 0.5d0)    ! A(1,1) = a11
  a(4) = (2.0d0, -1.0d0)   ! A(2,1) = a21
  ! Col 2: rows 2-4 valid
  a(6) = (3.0d0, 1.0d0)    ! A(1,2) = a12
  a(7) = (4.0d0, 0.0d0)    ! A(2,2) = a22
  a(8) = (5.0d0, 2.0d0)    ! A(3,2) = a32
  ! Col 3: rows 1-4 valid
  a(9) = (6.0d0, -0.5d0)   ! A(1,3) = a13
  a(10) = (7.0d0, 1.5d0)   ! A(2,3) = a23
  a(11) = (8.0d0, -1.0d0)  ! A(3,3) = a33
  a(12) = (9.0d0, 0.0d0)   ! A(4,3) = a43
  ! Col 4: rows 2-4 valid (a24, a34, a44)
  a(14) = (10.0d0, 1.0d0)  ! A(2,4) = a24
  a(15) = (11.0d0, -2.0d0) ! A(3,4) = a34
  a(16) = (12.0d0, 0.5d0)  ! A(4,4) = a44

  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  x(4) = (4.0d0, 0.5d0)

  y = (0.0d0, 0.0d0)
  call zgbmv('N', 4, 4, 1, 2, (1.0d0,0.0d0), a, 4, x, 1, &
             (0.0d0,0.0d0), y, 1)
  call begin_test('no_trans_basic')
  call print_array('y', y_r, 8)
  call end_test()

  ! Test 2: TRANS='T' (transpose, no conjugate)
  y = (0.0d0, 0.0d0)
  call zgbmv('T', 4, 4, 1, 2, (1.0d0,0.0d0), a, 4, x, 1, &
             (0.0d0,0.0d0), y, 1)
  call begin_test('trans_basic')
  call print_array('y', y_r, 8)
  call end_test()

  ! Test 3: TRANS='C' (conjugate transpose)
  y = (0.0d0, 0.0d0)
  call zgbmv('C', 4, 4, 1, 2, (1.0d0,0.0d0), a, 4, x, 1, &
             (0.0d0,0.0d0), y, 1)
  call begin_test('conj_trans_basic')
  call print_array('y', y_r, 8)
  call end_test()

  ! Test 4: complex alpha=(2,1), beta=(0.5,-0.5)
  y = (0.0d0, 0.0d0)
  y(1) = (1.0d0, 1.0d0)
  y(2) = (2.0d0, -1.0d0)
  y(3) = (0.5d0, 0.5d0)
  y(4) = (3.0d0, 0.0d0)
  call zgbmv('N', 4, 4, 1, 2, (2.0d0,1.0d0), a, 4, x, 1, &
             (0.5d0,-0.5d0), y, 1)
  call begin_test('complex_alpha_beta')
  call print_array('y', y_r, 8)
  call end_test()

  ! Test 5: alpha=(0,0), beta=(2,0) — only scale y
  y = (0.0d0, 0.0d0)
  y(1) = (1.0d0, 2.0d0)
  y(2) = (3.0d0, 4.0d0)
  y(3) = (5.0d0, 6.0d0)
  y(4) = (7.0d0, 8.0d0)
  call zgbmv('N', 4, 4, 1, 2, (0.0d0,0.0d0), a, 4, x, 1, &
             (2.0d0,0.0d0), y, 1)
  call begin_test('alpha_zero')
  call print_array('y', y_r, 8)
  call end_test()

  ! Test 6: M=0 quick return
  y(1) = (99.0d0, 0.0d0)
  call zgbmv('N', 0, 4, 1, 2, (1.0d0,0.0d0), a, 4, x, 1, &
             (0.0d0,0.0d0), y, 1)
  call begin_test('m_zero')
  call print_array('y', y_r, 2)
  call end_test()

  ! Test 7: N=0 quick return
  y(1) = (99.0d0, 0.0d0)
  call zgbmv('N', 4, 0, 1, 2, (1.0d0,0.0d0), a, 4, x, 1, &
             (0.0d0,0.0d0), y, 1)
  call begin_test('n_zero')
  call print_array('y', y_r, 2)
  call end_test()

  ! Test 8: alpha=(0,0), beta=(0,0) — zero out y
  y = (0.0d0, 0.0d0)
  y(1) = (99.0d0, 88.0d0)
  y(2) = (77.0d0, 66.0d0)
  y(3) = (55.0d0, 44.0d0)
  y(4) = (33.0d0, 22.0d0)
  call zgbmv('N', 4, 4, 1, 2, (0.0d0,0.0d0), a, 4, x, 1, &
             (0.0d0,0.0d0), y, 1)
  call begin_test('alpha_zero_beta_zero')
  call print_array('y', y_r, 8)
  call end_test()

  ! Test 9: non-unit incx=2
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(3) = (2.0d0, 1.0d0)
  x(5) = (3.0d0, -1.0d0)
  x(7) = (4.0d0, 0.5d0)
  y = (0.0d0, 0.0d0)
  call zgbmv('N', 4, 4, 1, 2, (1.0d0,0.0d0), a, 4, x, 2, &
             (0.0d0,0.0d0), y, 1)
  call begin_test('incx_2')
  call print_array('y', y_r, 8)
  call end_test()

  ! Test 10: non-unit incy=2
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  x(4) = (4.0d0, 0.5d0)
  y = (0.0d0, 0.0d0)
  call zgbmv('N', 4, 4, 1, 2, (1.0d0,0.0d0), a, 4, x, 1, &
             (0.0d0,0.0d0), y, 2)
  call begin_test('incy_2')
  call print_array('y', y_r, 16)
  call end_test()

  ! Test 11: M != N (3x5 matrix with KL=1, KU=1, LDA=3)
  ! Full matrix:
  !   [ a11  a12   0    0    0  ]
  !   [ a21  a22  a23   0    0  ]
  !   [  0   a32  a33  a34   0  ]
  ! Band storage (LDA=3, 3 rows):
  !   Row 1 (superdiag): *    a12  a23  a34  *
  !   Row 2 (diagonal):  a11  a22  a33  a34  *  -- wait, that's not right for 3x5
  !   Row 3 (subdiag):   a21  a32  a33  *    *
  !
  ! Correct: A_band(KU+1+i-j, j) with KU=1
  !   Row 1 (superdiag): *    a12  a23  a34  a(3,5)=0 (row 4>M=3)
  !   Row 2 (diagonal):  a11  a22  a33  a(4,4)=0  a(5,5)=0 (rows>M)
  !   Row 3 (subdiag):   a21  a32  a(4,3)=0 ... (rows>M)
  ! Wait, actually for M=3, N=5, KL=1, KU=1:
  !   Col j, valid rows i: max(1,j-1) to min(3,j+1)
  !   Col 1: i=1,2   Col 2: i=1,2,3  Col 3: i=2,3  Col 4: i=3  Col 5: none (min(3,6)=3, max(1,4)=4 -> empty)
  ! Actually col 5: max(1,5-1)=4, min(3,5+1)=3 -> empty, no elements
  ! Col 4: max(1,4-1)=3, min(3,4+1)=3 -> i=3 only
  ! So it's a 3x5 matrix but only cols 1-4 have nonzero band entries
  a = (0.0d0, 0.0d0)
  ! LDA=3, Col 1: a((1-1)*3+i)
  ! band row KU+1+i-j = 1+1+i-1 = i+1
  ! i=1: row 2, i=2: row 3
  a(2) = (1.0d0, 1.0d0)    ! A(1,1)
  a(3) = (2.0d0, 0.0d0)    ! A(2,1)
  ! Col 2: a((2-1)*3+i) = a(3+i)
  ! i=1: row 1+1-2=0 -> row 1, i=2: row 2, i=3: row 3
  a(4) = (3.0d0, -1.0d0)   ! A(1,2) at band row 1
  a(5) = (4.0d0, 0.5d0)    ! A(2,2) at band row 2
  a(6) = (5.0d0, 1.0d0)    ! A(3,2) at band row 3
  ! Col 3: a((3-1)*3+i) = a(6+i)
  ! i=2: row 1+1+2-3=1, i=3: row 2
  a(7) = (6.0d0, 0.0d0)    ! A(2,3) at band row 1
  a(8) = (7.0d0, -0.5d0)   ! A(3,3) at band row 2
  ! Col 4: a((4-1)*3+i) = a(9+i)
  ! i=3: row 1+1+3-4=1
  a(10) = (8.0d0, 1.0d0)   ! A(3,4) at band row 1

  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  x(4) = (0.5d0, 0.5d0)
  x(5) = (1.0d0, -0.5d0)

  y = (0.0d0, 0.0d0)
  call zgbmv('N', 3, 5, 1, 1, (1.0d0,0.0d0), a, 3, x, 1, &
             (0.0d0,0.0d0), y, 1)
  call begin_test('rect_m_lt_n')
  call print_array('y', y_r, 6)
  call end_test()

  ! Test 12: beta=(1,0), alpha=(1,0) — add to existing y
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  x(4) = (4.0d0, 0.5d0)
  y = (0.0d0, 0.0d0)
  y(1) = (10.0d0, 5.0d0)
  y(2) = (20.0d0, -10.0d0)
  y(3) = (30.0d0, 15.0d0)
  y(4) = (40.0d0, -20.0d0)
  ! Reuse the 4x4 band matrix from above
  a = (0.0d0, 0.0d0)
  a(3) = (1.0d0, 0.5d0)
  a(4) = (2.0d0, -1.0d0)
  a(6) = (3.0d0, 1.0d0)
  a(7) = (4.0d0, 0.0d0)
  a(8) = (5.0d0, 2.0d0)
  a(9) = (6.0d0, -0.5d0)
  a(10) = (7.0d0, 1.5d0)
  a(11) = (8.0d0, -1.0d0)
  a(12) = (9.0d0, 0.0d0)
  a(14) = (10.0d0, 1.0d0)
  a(15) = (11.0d0, -2.0d0)
  a(16) = (12.0d0, 0.5d0)
  call zgbmv('N', 4, 4, 1, 2, (1.0d0,0.0d0), a, 4, x, 1, &
             (1.0d0,0.0d0), y, 1)
  call begin_test('beta_one')
  call print_array('y', y_r, 8)
  call end_test()

end program
