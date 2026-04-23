program test_zhetri_rook
  use test_utils
  implicit none

  integer, parameter :: NMAX = 10
  complex*16 :: a(NMAX, NMAX), work(NMAX*NMAX)
  double precision :: a_r(2*NMAX*NMAX)
  equivalence (a, a_r)
  integer :: ipiv(NMAX), info, n, lda

  lda = NMAX

  ! Test 1: N=0 quick return
  n = 0
  info = -999
  call zhetri_rook('U', n, a, lda, ipiv, work, info)
  call begin_test('n0')
  call print_int('info', info)
  call end_test()

  ! Test 2: N=1 upper
  n = 1
  a = (0.0d0, 0.0d0)
  a(1,1) = (4.0d0, 0.0d0)
  call zhetrf_rook('U', n, a, lda, ipiv, work, NMAX*NMAX, info)
  call begin_test('n1_upper_factored')
  call print_array('a', a_r, 2*n*n)
  call print_int_array('ipiv', ipiv, n)
  call end_test()
  call zhetri_rook('U', n, a, lda, ipiv, work, info)
  call begin_test('n1_upper')
  call print_array('a', a_r, 2*n*n)
  call print_int('info', info)
  call end_test()

  ! Test 3: N=1 lower
  n = 1
  a = (0.0d0, 0.0d0)
  a(1,1) = (9.0d0, 0.0d0)
  call zhetrf_rook('L', n, a, lda, ipiv, work, NMAX*NMAX, info)
  call begin_test('n1_lower_factored')
  call print_array('a', a_r, 2*n*n)
  call print_int_array('ipiv', ipiv, n)
  call end_test()
  call zhetri_rook('L', n, a, lda, ipiv, work, info)
  call begin_test('n1_lower')
  call print_array('a', a_r, 2*n*n)
  call print_int('info', info)
  call end_test()

  ! Test 4: 3x3 upper Hermitian positive definite
  n = 3
  a = (0.0d0, 0.0d0)
  a(1,1) = (4.0d0, 0.0d0)
  a(1,2) = (1.0d0, 2.0d0)
  a(2,2) = (5.0d0, 0.0d0)
  a(1,3) = (3.0d0, -1.0d0)
  a(2,3) = (2.0d0, 1.0d0)
  a(3,3) = (7.0d0, 0.0d0)
  call zhetrf_rook('U', n, a, lda, ipiv, work, NMAX*NMAX, info)
  call begin_test('3x3_upper_factored')
  call print_matrix('a', a_r, 2*lda, 2*n, n)
  call print_int_array('ipiv', ipiv, n)
  call end_test()
  call zhetri_rook('U', n, a, lda, ipiv, work, info)
  call begin_test('3x3_upper')
  call print_matrix('a', a_r, 2*lda, 2*n, n)
  call print_int('info', info)
  call end_test()

  ! Test 5: 3x3 lower Hermitian positive definite
  n = 3
  a = (0.0d0, 0.0d0)
  a(1,1) = (4.0d0, 0.0d0)
  a(2,1) = (1.0d0, -2.0d0)
  a(3,1) = (3.0d0, 1.0d0)
  a(2,2) = (5.0d0, 0.0d0)
  a(3,2) = (2.0d0, -1.0d0)
  a(3,3) = (7.0d0, 0.0d0)
  call zhetrf_rook('L', n, a, lda, ipiv, work, NMAX*NMAX, info)
  call begin_test('3x3_lower_factored')
  call print_matrix('a', a_r, 2*lda, 2*n, n)
  call print_int_array('ipiv', ipiv, n)
  call end_test()
  call zhetri_rook('L', n, a, lda, ipiv, work, info)
  call begin_test('3x3_lower')
  call print_matrix('a', a_r, 2*lda, 2*n, n)
  call print_int('info', info)
  call end_test()

  ! Test 6: 4x4 upper indefinite (forces 2x2 pivots)
  n = 4
  a = (0.0d0, 0.0d0)
  a(1,1) = (0.0d0, 0.0d0)
  a(1,2) = (1.0d0, 1.0d0)
  a(2,2) = (0.0d0, 0.0d0)
  a(1,3) = (2.0d0, -1.0d0)
  a(2,3) = (4.0d0, 2.0d0)
  a(3,3) = (0.0d0, 0.0d0)
  a(1,4) = (3.0d0, 0.5d0)
  a(2,4) = (5.0d0, -1.0d0)
  a(3,4) = (6.0d0, 1.0d0)
  a(4,4) = (0.0d0, 0.0d0)
  call zhetrf_rook('U', n, a, lda, ipiv, work, NMAX*NMAX, info)
  call begin_test('4x4_upper_indef_factored')
  call print_matrix('a', a_r, 2*lda, 2*n, n)
  call print_int_array('ipiv', ipiv, n)
  call end_test()
  if (info .eq. 0) then
    call zhetri_rook('U', n, a, lda, ipiv, work, info)
  end if
  call begin_test('4x4_upper_indef')
  call print_matrix('a', a_r, 2*lda, 2*n, n)
  call print_int('info', info)
  call end_test()

  ! Test 7: 4x4 lower indefinite (forces 2x2 pivots)
  n = 4
  a = (0.0d0, 0.0d0)
  a(1,1) = (0.0d0, 0.0d0)
  a(2,1) = (1.0d0, -1.0d0)
  a(3,1) = (2.0d0, 1.0d0)
  a(4,1) = (3.0d0, -0.5d0)
  a(2,2) = (0.0d0, 0.0d0)
  a(3,2) = (4.0d0, -2.0d0)
  a(4,2) = (5.0d0, 1.0d0)
  a(3,3) = (0.0d0, 0.0d0)
  a(4,3) = (6.0d0, -1.0d0)
  a(4,4) = (0.0d0, 0.0d0)
  call zhetrf_rook('L', n, a, lda, ipiv, work, NMAX*NMAX, info)
  call begin_test('4x4_lower_indef_factored')
  call print_matrix('a', a_r, 2*lda, 2*n, n)
  call print_int_array('ipiv', ipiv, n)
  call end_test()
  if (info .eq. 0) then
    call zhetri_rook('L', n, a, lda, ipiv, work, info)
  end if
  call begin_test('4x4_lower_indef')
  call print_matrix('a', a_r, 2*lda, 2*n, n)
  call print_int('info', info)
  call end_test()

  ! Test 8: 4x4 upper general (mix of 1x1 and 2x2 pivots with interchange)
  n = 4
  a = (0.0d0, 0.0d0)
  a(1,1) = (0.1d0, 0.0d0)
  a(1,2) = (0.2d0, 0.3d0)
  a(2,2) = (5.0d0, 0.0d0)
  a(1,3) = (10.0d0, 1.0d0)
  a(2,3) = (0.5d0, -0.5d0)
  a(3,3) = (3.0d0, 0.0d0)
  a(1,4) = (0.1d0, 0.2d0)
  a(2,4) = (0.3d0, -0.1d0)
  a(3,4) = (0.4d0, 0.6d0)
  a(4,4) = (4.0d0, 0.0d0)
  call zhetrf_rook('U', n, a, lda, ipiv, work, NMAX*NMAX, info)
  call begin_test('4x4_upper_swap_factored')
  call print_matrix('a', a_r, 2*lda, 2*n, n)
  call print_int_array('ipiv', ipiv, n)
  call end_test()
  if (info .eq. 0) then
    call zhetri_rook('U', n, a, lda, ipiv, work, info)
  end if
  call begin_test('4x4_upper_swap')
  call print_matrix('a', a_r, 2*lda, 2*n, n)
  call print_int('info', info)
  call end_test()

  ! Test 9: 4x4 lower general (mix of 1x1 and 2x2 pivots with interchange)
  n = 4
  a = (0.0d0, 0.0d0)
  a(1,1) = (0.1d0, 0.0d0)
  a(2,1) = (0.2d0, -0.3d0)
  a(3,1) = (10.0d0, -1.0d0)
  a(4,1) = (0.1d0, -0.2d0)
  a(2,2) = (5.0d0, 0.0d0)
  a(3,2) = (0.5d0, 0.5d0)
  a(4,2) = (0.3d0, 0.1d0)
  a(3,3) = (3.0d0, 0.0d0)
  a(4,3) = (0.4d0, -0.6d0)
  a(4,4) = (4.0d0, 0.0d0)
  call zhetrf_rook('L', n, a, lda, ipiv, work, NMAX*NMAX, info)
  call begin_test('4x4_lower_swap_factored')
  call print_matrix('a', a_r, 2*lda, 2*n, n)
  call print_int_array('ipiv', ipiv, n)
  call end_test()
  if (info .eq. 0) then
    call zhetri_rook('L', n, a, lda, ipiv, work, info)
  end if
  call begin_test('4x4_lower_swap')
  call print_matrix('a', a_r, 2*lda, 2*n, n)
  call print_int('info', info)
  call end_test()

end program
