program test_dsbmv
  use test_utils
  implicit none
  double precision :: a(20), x(20), y(20)

  ! 4x4 symmetric matrix with K=1 (tridiagonal)
  ! Full matrix:
  !   [ 1  2  0  0 ]
  !   [ 2  3  4  0 ]
  !   [ 0  4  5  6 ]
  !   [ 0  0  6  7 ]

  ! Test 1: upper band storage, alpha=1, beta=0
  ! Upper band: LDA=2, row 0 = superdiag, row 1 = diag
  ! col 1: diag=1
  ! col 2: super=2, diag=3
  ! col 3: super=4, diag=5
  ! col 4: super=6, diag=7
  a = 0.0d0
  a(2) = 1.0d0  ! col1: diag
  a(3) = 2.0d0; a(4) = 3.0d0  ! col2: super, diag
  a(5) = 4.0d0; a(6) = 5.0d0  ! col3: super, diag
  a(7) = 6.0d0; a(8) = 7.0d0  ! col4: super, diag
  x(1:4) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0/)
  y = 0.0d0
  call dsbmv('U', 4, 1, 1.0d0, a, 2, x, 1, 0.0d0, y, 1)
  call begin_test('upper_basic')
  call print_array('y', y, 4)
  call end_test()

  ! Test 2: lower band storage
  ! Lower band: LDA=2, row 0 = diag, row 1 = subdiag
  ! col 1: diag=1, sub=2
  ! col 2: diag=3, sub=4
  ! col 3: diag=5, sub=6
  ! col 4: diag=7
  a = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0  ! col1
  a(3) = 3.0d0; a(4) = 4.0d0  ! col2
  a(5) = 5.0d0; a(6) = 6.0d0  ! col3
  a(7) = 7.0d0                  ! col4
  x(1:4) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0/)
  y = 0.0d0
  call dsbmv('L', 4, 1, 1.0d0, a, 2, x, 1, 0.0d0, y, 1)
  call begin_test('lower_basic')
  call print_array('y', y, 4)
  call end_test()

  ! Test 3: alpha=2, beta=0.5
  a = 0.0d0
  a(2) = 1.0d0
  a(3) = 2.0d0; a(4) = 3.0d0
  a(5) = 4.0d0; a(6) = 5.0d0
  a(7) = 6.0d0; a(8) = 7.0d0
  x(1:4) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0/)
  y(1:4) = (/10.0d0, 20.0d0, 30.0d0, 40.0d0/)
  call dsbmv('U', 4, 1, 2.0d0, a, 2, x, 1, 0.5d0, y, 1)
  call begin_test('alpha_beta')
  call print_array('y', y, 4)
  call end_test()

  ! Test 4: n=0
  y(1) = 99.0d0
  call dsbmv('U', 0, 1, 1.0d0, a, 2, x, 1, 0.0d0, y, 1)
  call begin_test('n_zero')
  call print_array('y', y, 1)
  call end_test()

  ! Test 5: stride=2
  a = 0.0d0
  a(2) = 1.0d0
  a(3) = 2.0d0; a(4) = 3.0d0
  a(5) = 4.0d0; a(6) = 5.0d0
  a(7) = 6.0d0; a(8) = 7.0d0
  x = 0.0d0; y = 0.0d0
  x(1) = 1.0d0; x(3) = 2.0d0; x(5) = 3.0d0; x(7) = 4.0d0
  call dsbmv('U', 4, 1, 1.0d0, a, 2, x, 2, 0.0d0, y, 2)
  call begin_test('stride')
  call print_array('y', y, 8)
  call end_test()

end program
