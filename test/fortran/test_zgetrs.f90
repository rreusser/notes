program test_zgetrs
  use test_utils
  implicit none
  double precision :: a_r(200), b_r(200), aorig_r(200)
  complex*16 :: a(100), b(100), aorig(100)
  equivalence (a, a_r)
  equivalence (b, b_r)
  equivalence (aorig, aorig_r)
  integer :: ipiv(10), info, i

  ! Test 1: solve_3x3 - basic A*X = B (no-transpose)
  ! A = [(2+1i) (1+0.5i) (1+0.1i); (4+2i) (3+1i) (3+0.5i); (8+3i) (7+2i) (9+1i)]
  a = (0.0d0, 0.0d0)
  a(1) = (2.0d0, 1.0d0); a(2) = (4.0d0, 2.0d0); a(3) = (8.0d0, 3.0d0)
  a(4) = (1.0d0, 0.5d0); a(5) = (3.0d0, 1.0d0); a(6) = (7.0d0, 2.0d0)
  a(7) = (1.0d0, 0.1d0); a(8) = (3.0d0, 0.5d0); a(9) = (9.0d0, 1.0d0)
  aorig = a
  b = (0.0d0, 0.0d0)
  b(1) = (1.0d0, 0.5d0); b(2) = (2.0d0, 1.0d0); b(3) = (3.0d0, 0.0d0)
  ipiv = 0
  call zgetrf(3, 3, a, 3, ipiv, info)
  call zgetrs('N', 3, 1, a, 3, ipiv, b, 3, info)
  call begin_test('solve_3x3')
  call print_array('x', b_r, 6)
  call print_int('info', info)
  call end_test()

  ! Test 2: solve_3x3_trans - A^T * X = B (transpose)
  a = aorig
  b = (0.0d0, 0.0d0)
  b(1) = (1.0d0, 0.5d0); b(2) = (2.0d0, 1.0d0); b(3) = (3.0d0, 0.0d0)
  ipiv = 0
  call zgetrf(3, 3, a, 3, ipiv, info)
  call zgetrs('T', 3, 1, a, 3, ipiv, b, 3, info)
  call begin_test('solve_3x3_trans')
  call print_array('x', b_r, 6)
  call print_int('info', info)
  call end_test()

  ! Test 3: solve_3x3_conj - A^H * X = B (conjugate transpose)
  a = aorig
  b = (0.0d0, 0.0d0)
  b(1) = (1.0d0, 0.5d0); b(2) = (2.0d0, 1.0d0); b(3) = (3.0d0, 0.0d0)
  ipiv = 0
  call zgetrf(3, 3, a, 3, ipiv, info)
  call zgetrs('C', 3, 1, a, 3, ipiv, b, 3, info)
  call begin_test('solve_3x3_conj')
  call print_array('x', b_r, 6)
  call print_int('info', info)
  call end_test()

  ! Test 4: multi_rhs - A*X = B with NRHS=2
  a = aorig
  b = (0.0d0, 0.0d0)
  b(1) = (1.0d0, 0.0d0); b(2) = (0.0d0, 0.0d0); b(3) = (0.0d0, 0.0d0)
  b(4) = (0.0d0, 0.0d0); b(5) = (1.0d0, 0.0d0); b(6) = (0.0d0, 0.0d0)
  ipiv = 0
  call zgetrf(3, 3, a, 3, ipiv, info)
  call zgetrs('N', 3, 2, a, 3, ipiv, b, 3, info)
  call begin_test('multi_rhs')
  call print_array('x', b_r, 12)
  call print_int('info', info)
  call end_test()

  ! Test 5: n_zero - quick return for N=0
  info = -999
  call zgetrs('N', 0, 1, a, 1, ipiv, b, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: nrhs_zero - quick return for NRHS=0
  info = -999
  call zgetrs('N', 3, 0, a, 3, ipiv, b, 3, info)
  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

  ! Test 7: 1x1 - single element solve
  a(1) = (5.0d0, 3.0d0)
  b(1) = (10.0d0, 6.0d0)
  ipiv = 0
  call zgetrf(1, 1, a, 1, ipiv, info)
  call zgetrs('N', 1, 1, a, 1, ipiv, b, 1, info)
  call begin_test('1x1')
  call print_array('x', b_r, 2)
  call print_int('info', info)
  call end_test()

  ! Test 8: identity - 3x3 identity matrix
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0); a(5) = (1.0d0, 0.0d0); a(9) = (1.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  b(1) = (3.0d0, 1.0d0); b(2) = (5.0d0, 2.0d0); b(3) = (7.0d0, 3.0d0)
  ipiv = 0
  call zgetrf(3, 3, a, 3, ipiv, info)
  call zgetrs('N', 3, 1, a, 3, ipiv, b, 3, info)
  call begin_test('identity')
  call print_array('x', b_r, 6)
  call print_int('info', info)
  call end_test()

  ! Test 9: multi_rhs_conj - A^H * X = B with NRHS=2
  a = aorig
  b = (0.0d0, 0.0d0)
  b(1) = (1.0d0, 0.0d0); b(2) = (0.0d0, 0.0d0); b(3) = (0.0d0, 0.0d0)
  b(4) = (0.0d0, 0.0d0); b(5) = (1.0d0, 0.0d0); b(6) = (0.0d0, 0.0d0)
  ipiv = 0
  call zgetrf(3, 3, a, 3, ipiv, info)
  call zgetrs('C', 3, 2, a, 3, ipiv, b, 3, info)
  call begin_test('multi_rhs_conj')
  call print_array('x', b_r, 12)
  call print_int('info', info)
  call end_test()

end program
