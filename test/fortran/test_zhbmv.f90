program test_zhbmv
  use test_utils
  implicit none
  ! 4x4 Hermitian matrix with K=2 super-diagonals, LDA=K+1=3
  complex*16 :: a(12), x(8), y(8)
  double precision :: a_r(24), x_r(16), y_r(16)
  equivalence (a, a_r)
  equivalence (x, x_r)
  equivalence (y, y_r)

  ! Hermitian 4x4 with K=2:
  ! Full: [d1     a12    a13    0   ]
  !       [c(a12) d2     a23    a24 ]
  !       [c(a13) c(a23) d3     a34 ]
  !       [0      c(a24) c(a34) d4  ]
  ! (diagonal is real, off-diagonal: A(i,j) = conj(A(j,i)))
  !
  ! Upper band storage (LDA=3, K=2):
  ! Row 1 (2nd superdiag): *      *      a13    a24
  ! Row 2 (1st superdiag): *      a12    a23    a34
  ! Row 3 (diagonal):      d1     d2     d3     d4

  ! Test 1: UPLO='U', basic
  a = (0.0d0, 0.0d0)
  ! Col 1: only diagonal
  a(3) = (2.0d0, 0.0d0)     ! d1
  ! Col 2: 1st superdiag + diag
  a(5) = (1.0d0, 1.0d0)     ! a12
  a(6) = (4.0d0, 0.0d0)     ! d2
  ! Col 3: 2nd superdiag + 1st superdiag + diag
  a(7) = (3.0d0, -2.0d0)    ! a13
  a(8) = (2.0d0, 1.0d0)     ! a23
  a(9) = (5.0d0, 0.0d0)     ! d3
  ! Col 4:
  a(10) = (0.5d0, -1.0d0)   ! a24
  a(11) = (1.5d0, 0.5d0)    ! a34
  a(12) = (3.0d0, 0.0d0)    ! d4

  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.5d0); x(2) = (2.0d0, -1.0d0)
  x(3) = (3.0d0, 1.0d0); x(4) = (4.0d0, 0.0d0)

  y = (0.0d0, 0.0d0)
  call zhbmv('U', 4, 2, (1.0d0,0.0d0), a, 3, x, 1, (0.0d0,0.0d0), y, 1)
  call begin_test('upper_basic')
  call print_array('y', y_r, 8)
  call end_test()

  ! Test 2: UPLO='L' - same Hermitian matrix stored in lower band
  ! Lower band (LDA=3, K=2):
  ! Row 1 (diagonal):      d1     d2     d3     d4
  ! Row 2 (1st subdiag):   c(a12) c(a23) c(a34) *
  ! Row 3 (2nd subdiag):   c(a13) c(a24) *      *
  a = (0.0d0, 0.0d0)
  ! Col 1:
  a(1) = (2.0d0, 0.0d0)     ! d1
  a(2) = (1.0d0, -1.0d0)    ! conj(a12)
  a(3) = (3.0d0, 2.0d0)     ! conj(a13)
  ! Col 2:
  a(4) = (4.0d0, 0.0d0)     ! d2
  a(5) = (2.0d0, -1.0d0)    ! conj(a23)
  a(6) = (0.5d0, 1.0d0)     ! conj(a24)
  ! Col 3:
  a(7) = (5.0d0, 0.0d0)     ! d3
  a(8) = (1.5d0, -0.5d0)    ! conj(a34)
  ! Col 4:
  a(10) = (3.0d0, 0.0d0)    ! d4

  y = (0.0d0, 0.0d0)
  call zhbmv('L', 4, 2, (1.0d0,0.0d0), a, 3, x, 1, (0.0d0,0.0d0), y, 1)
  call begin_test('lower_basic')
  call print_array('y', y_r, 8)
  call end_test()

  ! Test 3: complex alpha=(2,1), beta=(0.5,-0.5)
  ! Reuse upper storage
  a = (0.0d0, 0.0d0)
  a(3) = (2.0d0, 0.0d0); a(5) = (1.0d0, 1.0d0); a(6) = (4.0d0, 0.0d0)
  a(7) = (3.0d0, -2.0d0); a(8) = (2.0d0, 1.0d0); a(9) = (5.0d0, 0.0d0)
  a(10) = (0.5d0, -1.0d0); a(11) = (1.5d0, 0.5d0); a(12) = (3.0d0, 0.0d0)

  y = (0.0d0, 0.0d0)
  y(1) = (1.0d0, 1.0d0); y(2) = (2.0d0, -1.0d0)
  y(3) = (0.5d0, 0.5d0); y(4) = (3.0d0, 0.0d0)
  call zhbmv('U', 4, 2, (2.0d0,1.0d0), a, 3, x, 1, (0.5d0,-0.5d0), y, 1)
  call begin_test('complex_alpha_beta')
  call print_array('y', y_r, 8)
  call end_test()

  ! Test 4: alpha=0, beta=(2,0)
  y = (0.0d0, 0.0d0)
  y(1) = (1.0d0, 2.0d0); y(2) = (3.0d0, 4.0d0)
  y(3) = (5.0d0, 6.0d0); y(4) = (7.0d0, 8.0d0)
  call zhbmv('U', 4, 2, (0.0d0,0.0d0), a, 3, x, 1, (2.0d0,0.0d0), y, 1)
  call begin_test('alpha_zero')
  call print_array('y', y_r, 8)
  call end_test()

  ! Test 5: N=0 quick return
  y(1) = (99.0d0, 0.0d0)
  call zhbmv('U', 0, 2, (1.0d0,0.0d0), a, 3, x, 1, (0.0d0,0.0d0), y, 1)
  call begin_test('n_zero')
  call print_array('y', y_r, 2)
  call end_test()

  ! Test 6: alpha=0, beta=0
  y = (0.0d0, 0.0d0)
  y(1) = (99.0d0, 88.0d0); y(2) = (77.0d0, 66.0d0)
  y(3) = (55.0d0, 44.0d0); y(4) = (33.0d0, 22.0d0)
  call zhbmv('U', 4, 2, (0.0d0,0.0d0), a, 3, x, 1, (0.0d0,0.0d0), y, 1)
  call begin_test('alpha_zero_beta_zero')
  call print_array('y', y_r, 8)
  call end_test()

  ! Test 7: non-unit strides
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.5d0); x(3) = (2.0d0, -1.0d0)
  x(5) = (3.0d0, 1.0d0); x(7) = (4.0d0, 0.0d0)
  y = (0.0d0, 0.0d0)
  ! Reuse upper storage from test 3
  call zhbmv('U', 4, 2, (1.0d0,0.0d0), a, 3, x, 2, (0.0d0,0.0d0), y, 2)
  call begin_test('stride_2')
  call print_array('y', y_r, 16)
  call end_test()

  ! Test 8: 1x1 scalar, K=0
  a = (0.0d0, 0.0d0); a(1) = (5.0d0, 0.0d0)
  x = (0.0d0, 0.0d0); x(1) = (3.0d0, 2.0d0)
  y = (0.0d0, 0.0d0)
  call zhbmv('U', 1, 0, (2.0d0,1.0d0), a, 1, x, 1, (0.0d0,0.0d0), y, 1)
  call begin_test('scalar')
  call print_array('y', y_r, 2)
  call end_test()

end program
