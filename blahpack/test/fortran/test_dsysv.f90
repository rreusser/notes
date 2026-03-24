program test_dsysv
  use test_utils
  implicit none
  double precision :: a(100), b(100), work(200)
  integer :: ipiv(10), info, lwork

  lwork = 200

  ! Test 1: 4x4 upper symmetric, single RHS
  ! A (symmetric, store upper triangle in column-major):
  ! [ 4  1  2  3 ]
  ! [ 1  5  1  2 ]
  ! [ 2  1  6  1 ]
  ! [ 3  2  1  7 ]
  ! x = [1; 2; 3; 4], b = A*x = [24; 22; 26; 38]
  a = 0.0d0; b = 0.0d0
  ! Column-major storage (full matrix, dsysv uses only upper)
  a(1) = 4.0d0; a(2) = 1.0d0; a(3) = 2.0d0; a(4) = 3.0d0
  a(5) = 1.0d0; a(6) = 5.0d0; a(7) = 1.0d0; a(8) = 2.0d0
  a(9) = 2.0d0; a(10) = 1.0d0; a(11) = 6.0d0; a(12) = 1.0d0
  a(13) = 3.0d0; a(14) = 2.0d0; a(15) = 1.0d0; a(16) = 7.0d0
  b(1) = 24.0d0; b(2) = 22.0d0; b(3) = 26.0d0; b(4) = 38.0d0
  call dsysv('U', 4, 1, a, 4, ipiv, b, 4, work, lwork, info)
  call begin_test('upper_4x4')
  call print_array('x', b, 4)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call print_matrix('A', a, 4, 4, 4)
  call end_test()

  ! Test 2: 4x4 lower symmetric, single RHS
  ! Same matrix, use lower triangle
  a = 0.0d0; b = 0.0d0
  a(1) = 4.0d0; a(2) = 1.0d0; a(3) = 2.0d0; a(4) = 3.0d0
  a(5) = 1.0d0; a(6) = 5.0d0; a(7) = 1.0d0; a(8) = 2.0d0
  a(9) = 2.0d0; a(10) = 1.0d0; a(11) = 6.0d0; a(12) = 1.0d0
  a(13) = 3.0d0; a(14) = 2.0d0; a(15) = 1.0d0; a(16) = 7.0d0
  b(1) = 24.0d0; b(2) = 22.0d0; b(3) = 26.0d0; b(4) = 38.0d0
  call dsysv('L', 4, 1, a, 4, ipiv, b, 4, work, lwork, info)
  call begin_test('lower_4x4')
  call print_array('x', b, 4)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call print_matrix('A', a, 4, 4, 4)
  call end_test()

  ! Test 3: Multiple RHS (NRHS=2)
  ! A = [2 -1; -1 3], b = [1 4; 5 7]
  ! A*x = b => x = A^{-1}*b
  a = 0.0d0; b = 0.0d0
  a(1) = 2.0d0; a(2) = -1.0d0
  a(3) = -1.0d0; a(4) = 3.0d0
  b(1) = 1.0d0; b(2) = 5.0d0
  b(3) = 4.0d0; b(4) = 7.0d0
  call dsysv('U', 2, 2, a, 2, ipiv, b, 2, work, lwork, info)
  call begin_test('multi_rhs')
  call print_array('x', b, 4)
  call print_int_array('ipiv', ipiv, 2)
  call print_int('info', info)
  call end_test()

  ! Test 4: Singular matrix (INFO > 0)
  ! A = [1 2; 2 4] is singular (rank 1)
  a = 0.0d0; b = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0
  a(3) = 2.0d0; a(4) = 4.0d0
  b(1) = 1.0d0; b(2) = 2.0d0
  call dsysv('U', 2, 1, a, 2, ipiv, b, 2, work, lwork, info)
  call begin_test('singular')
  call print_int('info', info)
  call end_test()

  ! Test 5: N=1 edge case
  ! A = [3], b = [9] => x = 3
  a = 0.0d0; b = 0.0d0
  a(1) = 3.0d0
  b(1) = 9.0d0
  call dsysv('U', 1, 1, a, 1, ipiv, b, 1, work, lwork, info)
  call begin_test('n1')
  call print_array('x', b, 1)
  call print_int_array('ipiv', ipiv, 1)
  call print_int('info', info)
  call end_test()

  ! Test 6: Matrix triggering 2x2 pivots
  ! A = [0 1 0 0; 1 0 0 0; 0 0 4 1; 0 0 1 4]
  ! This has zero diagonal elements which forces 2x2 pivots
  ! b = [1; 1; 5; 5]
  a = 0.0d0; b = 0.0d0
  a(1) = 0.0d0; a(2) = 1.0d0; a(3) = 0.0d0; a(4) = 0.0d0
  a(5) = 1.0d0; a(6) = 0.0d0; a(7) = 0.0d0; a(8) = 0.0d0
  a(9) = 0.0d0; a(10) = 0.0d0; a(11) = 4.0d0; a(12) = 1.0d0
  a(13) = 0.0d0; a(14) = 0.0d0; a(15) = 1.0d0; a(16) = 4.0d0
  b(1) = 1.0d0; b(2) = 1.0d0; b(3) = 5.0d0; b(4) = 5.0d0
  call dsysv('U', 4, 1, a, 4, ipiv, b, 4, work, lwork, info)
  call begin_test('pivot_2x2_upper')
  call print_array('x', b, 4)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 7: Same 2x2 pivot matrix with lower
  a = 0.0d0; b = 0.0d0
  a(1) = 0.0d0; a(2) = 1.0d0; a(3) = 0.0d0; a(4) = 0.0d0
  a(5) = 1.0d0; a(6) = 0.0d0; a(7) = 0.0d0; a(8) = 0.0d0
  a(9) = 0.0d0; a(10) = 0.0d0; a(11) = 4.0d0; a(12) = 1.0d0
  a(13) = 0.0d0; a(14) = 0.0d0; a(15) = 1.0d0; a(16) = 4.0d0
  b(1) = 1.0d0; b(2) = 1.0d0; b(3) = 5.0d0; b(4) = 5.0d0
  call dsysv('L', 4, 1, a, 4, ipiv, b, 4, work, lwork, info)
  call begin_test('pivot_2x2_lower')
  call print_array('x', b, 4)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 8: N=0 quick return
  call dsysv('U', 0, 1, a, 1, ipiv, b, 1, work, lwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

end program
