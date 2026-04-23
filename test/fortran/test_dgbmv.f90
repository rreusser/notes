program test_dgbmv
  use test_utils
  implicit none
  ! 4x5 band matrix with KL=1, KU=2 stored in (KL+KU+1)x5 = 4x5 band format
  double precision :: a(20), x(20), y(20)

  ! Band storage (column-major, 4 rows per column):
  ! Row 0 = 2nd superdiag, Row 1 = 1st superdiag, Row 2 = diagonal, Row 3 = subdiag
  ! Full matrix A (4x5):
  !   [ 1  2  3  0  0 ]
  !   [ 4  5  6  7  0 ]
  !   [ 0  8  9 10 11 ]
  !   [ 0  0 12 13 14 ]
  a = 0.0d0
  ! col 1: diag=1, sub=4
  a(3) = 1.0d0; a(4) = 4.0d0
  ! col 2: super1=2, diag=5, sub=8
  a(6) = 2.0d0; a(7) = 5.0d0; a(8) = 8.0d0
  ! col 3: super2=3, super1=6, diag=9, sub=12
  a(9) = 3.0d0; a(10) = 6.0d0; a(11) = 9.0d0; a(12) = 12.0d0
  ! col 4: super2=7 (nope, not super2 — let me recalculate)
  ! Actually for column j, band row = KU + i - j (0-based)
  ! KU=2, so diagonal at row 2
  ! col 1 (j=1): row 2=A(1,1)=1, row 3=A(2,1)=4
  ! col 2 (j=2): row 1=A(1,2)=2, row 2=A(2,2)=5, row 3=A(3,2)=8
  ! col 3 (j=3): row 0=A(1,3)=3, row 1=A(2,3)=6, row 2=A(3,3)=9, row 3=A(4,3)=12
  ! col 4 (j=4): row 0=A(2,4)=7, row 1=A(3,4)=10, row 2=A(4,4)=13
  ! col 5 (j=5): row 0=A(3,5)=11, row 1=A(4,5)=14
  ! Fortran col-major: A(i, j) = a((j-1)*LDA + i), LDA=4
  a = 0.0d0
  ! col 1
  a(3) = 1.0d0; a(4) = 4.0d0
  ! col 2
  a(6) = 2.0d0; a(7) = 5.0d0; a(8) = 8.0d0
  ! col 3
  a(9) = 3.0d0; a(10) = 6.0d0; a(11) = 9.0d0; a(12) = 12.0d0
  ! col 4
  a(13) = 7.0d0; a(14) = 10.0d0; a(15) = 13.0d0
  ! col 5
  a(17) = 11.0d0; a(18) = 14.0d0

  ! Test 1: no-transpose, alpha=1, beta=0
  ! y = A * x where x = [1,2,3,4,5]
  x(1:5) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0/)
  y = 0.0d0
  call dgbmv('N', 4, 5, 1, 2, 1.0d0, a, 4, x, 1, 0.0d0, y, 1)
  call begin_test('notrans')
  call print_array('y', y, 4)
  call end_test()

  ! Test 2: transpose, alpha=1, beta=0
  ! y = A^T * x where x = [1,2,3,4]
  x(1:4) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0/)
  y = 0.0d0
  call dgbmv('T', 4, 5, 1, 2, 1.0d0, a, 4, x, 1, 0.0d0, y, 1)
  call begin_test('trans')
  call print_array('y', y, 5)
  call end_test()

  ! Test 3: alpha=2, beta=0.5
  x(1:5) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0/)
  y(1:4) = (/10.0d0, 20.0d0, 30.0d0, 40.0d0/)
  call dgbmv('N', 4, 5, 1, 2, 2.0d0, a, 4, x, 1, 0.5d0, y, 1)
  call begin_test('alpha_beta')
  call print_array('y', y, 4)
  call end_test()

  ! Test 4: n=0
  y(1) = 99.0d0
  call dgbmv('N', 0, 0, 0, 0, 1.0d0, a, 1, x, 1, 0.0d0, y, 1)
  call begin_test('n_zero')
  call print_array('y', y, 1)
  call end_test()

  ! Test 5: alpha=0, beta=2
  y(1:4) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0/)
  call dgbmv('N', 4, 5, 1, 2, 0.0d0, a, 4, x, 1, 2.0d0, y, 1)
  call begin_test('alpha_zero')
  call print_array('y', y, 4)
  call end_test()

  ! Test 6: non-unit strides
  x = 0.0d0; y = 0.0d0
  x(1) = 1.0d0; x(3) = 2.0d0; x(5) = 3.0d0; x(7) = 4.0d0; x(9) = 5.0d0
  y(1) = 0.0d0; y(3) = 0.0d0; y(5) = 0.0d0; y(7) = 0.0d0
  call dgbmv('N', 4, 5, 1, 2, 1.0d0, a, 4, x, 2, 0.0d0, y, 2)
  call begin_test('stride')
  call print_array('y', y, 8)
  call end_test()

end program
