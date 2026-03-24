program test_zlasyf
  use test_utils
  implicit none

  integer, parameter :: MAXN = 10
  double precision :: A_r(2*MAXN*MAXN), W_r(2*MAXN*MAXN)
  complex*16 :: A(MAXN,MAXN), W(MAXN,MAXN)
  equivalence (A, A_r)
  equivalence (W, W_r)
  integer :: ipiv(MAXN), info, kb, n, nb, lda, ldw

  ! ZLASYF: partial factorization of complex symmetric matrix
  ! using Bunch-Kaufman diagonal pivoting

  ! Test 1: 4x4 upper, nb=4
  n = 4
  nb = 4
  lda = MAXN
  ldw = MAXN
  A = (0.0d0, 0.0d0)
  A(1,1) = (4.0d0, 1.0d0)
  A(1,2) = (1.0d0, 2.0d0); A(2,2) = (5.0d0, 0.0d0)
  A(1,3) = (2.0d0, 0.0d0); A(2,3) = (1.0d0, 1.0d0); A(3,3) = (6.0d0, 2.0d0)
  A(1,4) = (0.0d0, 1.0d0); A(2,4) = (0.0d0, 0.0d0); A(3,4) = (1.0d0, -1.0d0); A(4,4) = (3.0d0, 0.0d0)
  W = (0.0d0, 0.0d0)
  ipiv = 0
  call zlasyf('U', n, nb, kb, A, lda, ipiv, W, ldw, info)
  call begin_test('upper_4x4')
  call print_array('A', A_r, 2*lda*n)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call print_int('kb', kb)
  call end_test()

  ! Test 2: 4x4 lower, nb=4
  n = 4
  nb = 4
  lda = MAXN
  ldw = MAXN
  A = (0.0d0, 0.0d0)
  A(1,1) = (4.0d0, 1.0d0)
  A(2,1) = (1.0d0, 2.0d0); A(2,2) = (5.0d0, 0.0d0)
  A(3,1) = (2.0d0, 0.0d0); A(3,2) = (1.0d0, 1.0d0); A(3,3) = (6.0d0, 2.0d0)
  A(4,1) = (0.0d0, 1.0d0); A(4,2) = (0.0d0, 0.0d0); A(4,3) = (1.0d0, -1.0d0); A(4,4) = (3.0d0, 0.0d0)
  W = (0.0d0, 0.0d0)
  ipiv = 0
  call zlasyf('L', n, nb, kb, A, lda, ipiv, W, ldw, info)
  call begin_test('lower_4x4')
  call print_array('A', A_r, 2*lda*n)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call print_int('kb', kb)
  call end_test()

  ! Test 3: 3x3 upper, nb=2 (partial panel)
  n = 3
  nb = 2
  lda = MAXN
  ldw = MAXN
  A = (0.0d0, 0.0d0)
  A(1,1) = (5.0d0, 0.0d0)
  A(1,2) = (2.0d0, 1.0d0); A(2,2) = (4.0d0, 0.0d0)
  A(1,3) = (1.0d0, 0.0d0); A(2,3) = (1.0d0, -1.0d0); A(3,3) = (6.0d0, 1.0d0)
  W = (0.0d0, 0.0d0)
  ipiv = 0
  call zlasyf('U', n, nb, kb, A, lda, ipiv, W, ldw, info)
  call begin_test('upper_3x3_nb2')
  call print_array('A', A_r, 2*lda*n)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call print_int('kb', kb)
  call end_test()

  ! Test 4: 1x1 matrix
  n = 1
  nb = 1
  lda = MAXN
  ldw = MAXN
  A = (0.0d0, 0.0d0)
  A(1,1) = (7.0d0, 3.0d0)
  W = (0.0d0, 0.0d0)
  ipiv = 0
  call zlasyf('U', n, nb, kb, A, lda, ipiv, W, ldw, info)
  call begin_test('upper_1x1')
  call print_array('A', A_r, 2*lda*n)
  call print_int_array('ipiv', ipiv, 1)
  call print_int('info', info)
  call print_int('kb', kb)
  call end_test()

  ! Test 5: 2x2 with pivot swap (upper)
  n = 2
  nb = 2
  lda = MAXN
  ldw = MAXN
  A = (0.0d0, 0.0d0)
  A(1,1) = (0.1d0, 0.0d0)
  A(1,2) = (3.0d0, 4.0d0); A(2,2) = (10.0d0, 0.0d0)
  W = (0.0d0, 0.0d0)
  ipiv = 0
  call zlasyf('U', n, nb, kb, A, lda, ipiv, W, ldw, info)
  call begin_test('upper_2x2_pivot')
  call print_array('A', A_r, 2*lda*n)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call print_int('kb', kb)
  call end_test()

  ! Test 6: 3x3 lower, nb=3
  n = 3
  nb = 3
  lda = MAXN
  ldw = MAXN
  A = (0.0d0, 0.0d0)
  A(1,1) = (5.0d0, 0.0d0)
  A(2,1) = (2.0d0, 1.0d0); A(2,2) = (4.0d0, 0.0d0)
  A(3,1) = (1.0d0, 0.0d0); A(3,2) = (1.0d0, -1.0d0); A(3,3) = (6.0d0, 1.0d0)
  W = (0.0d0, 0.0d0)
  ipiv = 0
  call zlasyf('L', n, nb, kb, A, lda, ipiv, W, ldw, info)
  call begin_test('lower_3x3')
  call print_array('A', A_r, 2*lda*n)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call print_int('kb', kb)
  call end_test()

  ! ==========================================================================
  ! Test 7: 8x8 upper, nb=4 -- exercises blocked loop (k goes below N-nb)
  ! ==========================================================================
  n = 8
  nb = 4
  lda = MAXN
  ldw = MAXN
  A = (0.0d0, 0.0d0)
  ! Diagonally dominant symmetric matrix
  A(1,1) = (10.0d0, 1.0d0)
  A(1,2) = (1.0d0, 0.5d0); A(2,2) = (9.0d0, 0.0d0)
  A(1,3) = (0.5d0, 0.2d0); A(2,3) = (0.8d0, -0.3d0); A(3,3) = (11.0d0, 0.5d0)
  A(1,4) = (0.3d0, 0.1d0); A(2,4) = (0.4d0, 0.2d0); A(3,4) = (0.6d0, -0.1d0); A(4,4) = (8.0d0, 0.0d0)
  A(1,5) = (0.2d0, 0.0d0); A(2,5) = (0.3d0, 0.1d0); A(3,5) = (0.5d0, 0.2d0); A(4,5) = (0.7d0, -0.3d0)
  A(5,5) = (12.0d0, 1.0d0)
  A(1,6) = (0.1d0, 0.1d0); A(2,6) = (0.2d0, 0.0d0); A(3,6) = (0.4d0, 0.1d0); A(4,6) = (0.3d0, 0.2d0)
  A(5,6) = (0.6d0, -0.1d0); A(6,6) = (7.0d0, 0.0d0)
  A(1,7) = (0.15d0, 0.05d0); A(2,7) = (0.25d0, 0.15d0); A(3,7) = (0.35d0, -0.05d0)
  A(4,7) = (0.2d0, 0.1d0); A(5,7) = (0.4d0, 0.2d0); A(6,7) = (0.5d0, -0.15d0)
  A(7,7) = (13.0d0, 0.5d0)
  A(1,8) = (0.1d0, 0.0d0); A(2,8) = (0.15d0, 0.1d0); A(3,8) = (0.2d0, 0.05d0)
  A(4,8) = (0.1d0, 0.0d0); A(5,8) = (0.3d0, 0.1d0); A(6,8) = (0.35d0, -0.2d0)
  A(7,8) = (0.45d0, 0.1d0); A(8,8) = (6.0d0, 0.0d0)

  W = (0.0d0, 0.0d0)
  ipiv = 0
  call zlasyf('U', n, nb, kb, A, lda, ipiv, W, ldw, info)
  call begin_test('upper_8x8_nb4')
  call print_array('A', A_r, 2*lda*n)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call print_int('kb', kb)
  call end_test()

  ! ==========================================================================
  ! Test 8: 8x8 lower, nb=4 -- exercises blocked loop
  ! ==========================================================================
  n = 8
  nb = 4
  lda = MAXN
  ldw = MAXN
  A = (0.0d0, 0.0d0)
  ! Same matrix, stored in lower triangle
  A(1,1) = (10.0d0, 1.0d0)
  A(2,1) = (1.0d0, 0.5d0); A(2,2) = (9.0d0, 0.0d0)
  A(3,1) = (0.5d0, 0.2d0); A(3,2) = (0.8d0, -0.3d0); A(3,3) = (11.0d0, 0.5d0)
  A(4,1) = (0.3d0, 0.1d0); A(4,2) = (0.4d0, 0.2d0); A(4,3) = (0.6d0, -0.1d0); A(4,4) = (8.0d0, 0.0d0)
  A(5,1) = (0.2d0, 0.0d0); A(5,2) = (0.3d0, 0.1d0); A(5,3) = (0.5d0, 0.2d0); A(5,4) = (0.7d0, -0.3d0)
  A(5,5) = (12.0d0, 1.0d0)
  A(6,1) = (0.1d0, 0.1d0); A(6,2) = (0.2d0, 0.0d0); A(6,3) = (0.4d0, 0.1d0); A(6,4) = (0.3d0, 0.2d0)
  A(6,5) = (0.6d0, -0.1d0); A(6,6) = (7.0d0, 0.0d0)
  A(7,1) = (0.15d0, 0.05d0); A(7,2) = (0.25d0, 0.15d0); A(7,3) = (0.35d0, -0.05d0)
  A(7,4) = (0.2d0, 0.1d0); A(7,5) = (0.4d0, 0.2d0); A(7,6) = (0.5d0, -0.15d0)
  A(7,7) = (13.0d0, 0.5d0)
  A(8,1) = (0.1d0, 0.0d0); A(8,2) = (0.15d0, 0.1d0); A(8,3) = (0.2d0, 0.05d0)
  A(8,4) = (0.1d0, 0.0d0); A(8,5) = (0.3d0, 0.1d0); A(8,6) = (0.35d0, -0.2d0)
  A(8,7) = (0.45d0, 0.1d0); A(8,8) = (6.0d0, 0.0d0)

  W = (0.0d0, 0.0d0)
  ipiv = 0
  call zlasyf('L', n, nb, kb, A, lda, ipiv, W, ldw, info)
  call begin_test('lower_8x8_nb4')
  call print_array('A', A_r, 2*lda*n)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call print_int('kb', kb)
  call end_test()

  ! ==========================================================================
  ! Test 9: 6x6 upper, nb=2 -- more partial panels
  ! ==========================================================================
  n = 6
  nb = 2
  lda = MAXN
  ldw = MAXN
  A = (0.0d0, 0.0d0)
  A(1,1) = (8.0d0, 0.0d0)
  A(1,2) = (1.0d0, 1.0d0); A(2,2) = (7.0d0, 0.5d0)
  A(1,3) = (0.5d0, 0.0d0); A(2,3) = (0.8d0, -0.2d0); A(3,3) = (9.0d0, 0.0d0)
  A(1,4) = (0.3d0, 0.1d0); A(2,4) = (0.4d0, 0.3d0); A(3,4) = (0.6d0, -0.1d0); A(4,4) = (10.0d0, 1.0d0)
  A(1,5) = (0.2d0, 0.0d0); A(2,5) = (0.3d0, 0.1d0); A(3,5) = (0.5d0, 0.2d0); A(4,5) = (0.7d0, -0.3d0)
  A(5,5) = (6.0d0, 0.0d0)
  A(1,6) = (0.1d0, 0.05d0); A(2,6) = (0.2d0, -0.1d0); A(3,6) = (0.4d0, 0.1d0)
  A(4,6) = (0.3d0, 0.2d0); A(5,6) = (0.5d0, -0.15d0); A(6,6) = (11.0d0, 0.5d0)

  W = (0.0d0, 0.0d0)
  ipiv = 0
  call zlasyf('U', n, nb, kb, A, lda, ipiv, W, ldw, info)
  call begin_test('upper_6x6_nb2')
  call print_array('A', A_r, 2*lda*n)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call print_int('kb', kb)
  call end_test()

  ! ==========================================================================
  ! Test 10: 6x6 lower, nb=2 -- more partial panels
  ! ==========================================================================
  n = 6
  nb = 2
  lda = MAXN
  ldw = MAXN
  A = (0.0d0, 0.0d0)
  A(1,1) = (8.0d0, 0.0d0)
  A(2,1) = (1.0d0, 1.0d0); A(2,2) = (7.0d0, 0.5d0)
  A(3,1) = (0.5d0, 0.0d0); A(3,2) = (0.8d0, -0.2d0); A(3,3) = (9.0d0, 0.0d0)
  A(4,1) = (0.3d0, 0.1d0); A(4,2) = (0.4d0, 0.3d0); A(4,3) = (0.6d0, -0.1d0); A(4,4) = (10.0d0, 1.0d0)
  A(5,1) = (0.2d0, 0.0d0); A(5,2) = (0.3d0, 0.1d0); A(5,3) = (0.5d0, 0.2d0); A(5,4) = (0.7d0, -0.3d0)
  A(5,5) = (6.0d0, 0.0d0)
  A(6,1) = (0.1d0, 0.05d0); A(6,2) = (0.2d0, -0.1d0); A(6,3) = (0.4d0, 0.1d0)
  A(6,4) = (0.3d0, 0.2d0); A(6,5) = (0.5d0, -0.15d0); A(6,6) = (11.0d0, 0.5d0)

  W = (0.0d0, 0.0d0)
  ipiv = 0
  call zlasyf('L', n, nb, kb, A, lda, ipiv, W, ldw, info)
  call begin_test('lower_6x6_nb2')
  call print_array('A', A_r, 2*lda*n)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call print_int('kb', kb)
  call end_test()

  ! ==========================================================================
  ! Test 11: 5x5 upper with 2x2 pivot (small diagonal forces 2x2 pivot)
  ! ==========================================================================
  n = 5
  nb = 5
  lda = MAXN
  ldw = MAXN
  A = (0.0d0, 0.0d0)
  ! Make diagonal small, off-diagonal large to force 2x2 pivots
  A(1,1) = (0.01d0, 0.0d0)
  A(1,2) = (0.02d0, 0.01d0); A(2,2) = (0.01d0, 0.0d0)
  A(1,3) = (0.5d0, 0.3d0); A(2,3) = (0.4d0, -0.2d0); A(3,3) = (0.01d0, 0.0d0)
  A(1,4) = (0.3d0, 0.1d0); A(2,4) = (0.6d0, 0.2d0); A(3,4) = (0.7d0, -0.1d0); A(4,4) = (5.0d0, 0.0d0)
  A(1,5) = (0.2d0, 0.0d0); A(2,5) = (0.3d0, 0.1d0); A(3,5) = (0.5d0, 0.2d0); A(4,5) = (0.4d0, -0.3d0)
  A(5,5) = (6.0d0, 1.0d0)

  W = (0.0d0, 0.0d0)
  ipiv = 0
  call zlasyf('U', n, nb, kb, A, lda, ipiv, W, ldw, info)
  call begin_test('upper_5x5_2x2pivot')
  call print_array('A', A_r, 2*lda*n)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call print_int('kb', kb)
  call end_test()

  ! ==========================================================================
  ! Test 12: 5x5 lower with 2x2 pivot
  ! ==========================================================================
  n = 5
  nb = 5
  lda = MAXN
  ldw = MAXN
  A = (0.0d0, 0.0d0)
  A(1,1) = (6.0d0, 1.0d0)
  A(2,1) = (0.4d0, -0.3d0); A(2,2) = (5.0d0, 0.0d0)
  A(3,1) = (0.5d0, 0.2d0); A(3,2) = (0.7d0, -0.1d0); A(3,3) = (0.01d0, 0.0d0)
  A(4,1) = (0.3d0, 0.1d0); A(4,2) = (0.6d0, 0.2d0); A(4,3) = (0.4d0, -0.2d0); A(4,4) = (0.01d0, 0.0d0)
  A(5,1) = (0.2d0, 0.0d0); A(5,2) = (0.3d0, 0.1d0); A(5,3) = (0.5d0, 0.3d0); A(5,4) = (0.02d0, 0.01d0)
  A(5,5) = (0.01d0, 0.0d0)

  W = (0.0d0, 0.0d0)
  ipiv = 0
  call zlasyf('L', n, nb, kb, A, lda, ipiv, W, ldw, info)
  call begin_test('lower_5x5_2x2pivot')
  call print_array('A', A_r, 2*lda*n)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call print_int('kb', kb)
  call end_test()

end program
