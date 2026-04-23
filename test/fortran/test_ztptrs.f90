program test_ztptrs
  use test_utils
  implicit none
  complex*16 :: ap(100), b(100)
  double precision :: ap_r(200), b_r(200)
  equivalence (ap, ap_r)
  equivalence (b, b_r)
  integer :: info, n, nrhs, ldb

  ! Test 1: 3x3 upper triangular packed, no transpose, non-unit diagonal
  ! A = [2+1i  1+2i  3+0i;  0  4+1i  5-1i;  0  0  6+2i]
  ! Upper packed (col-major): [2+1i, 1+2i, 4+1i, 3+0i, 5-1i, 6+2i]
  ! b = [1+1i; 2-1i; 3+0.5i]
  n = 3; nrhs = 1; ldb = 3
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  ap(1) = (2.0d0, 1.0d0)
  ap(2) = (1.0d0, 2.0d0); ap(3) = (4.0d0, 1.0d0)
  ap(4) = (3.0d0, 0.0d0); ap(5) = (5.0d0, -1.0d0); ap(6) = (6.0d0, 2.0d0)
  b(1) = (1.0d0, 1.0d0); b(2) = (2.0d0, -1.0d0); b(3) = (3.0d0, 0.5d0)
  call ztptrs('U', 'N', 'N', n, nrhs, ap, b, ldb, info)
  call begin_test('upper_no_trans')
  call print_array('x', b_r, 2*n)
  call print_int('info', info)
  call end_test()

  ! Test 2: 3x3 lower triangular packed, no transpose, non-unit diagonal
  ! L = [2+1i  0  0;  1+2i  4+1i  0;  3+0i  5-1i  6+2i]
  ! Lower packed (col-major): [2+1i, 1+2i, 3+0i, 4+1i, 5-1i, 6+2i]
  n = 3; nrhs = 1; ldb = 3
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  ap(1) = (2.0d0, 1.0d0); ap(2) = (1.0d0, 2.0d0); ap(3) = (3.0d0, 0.0d0)
  ap(4) = (4.0d0, 1.0d0); ap(5) = (5.0d0, -1.0d0)
  ap(6) = (6.0d0, 2.0d0)
  b(1) = (1.0d0, 1.0d0); b(2) = (2.0d0, -1.0d0); b(3) = (3.0d0, 0.5d0)
  call ztptrs('L', 'N', 'N', n, nrhs, ap, b, ldb, info)
  call begin_test('lower_no_trans')
  call print_array('x', b_r, 2*n)
  call print_int('info', info)
  call end_test()

  ! Test 3: 3x3 upper triangular packed, conjugate-transpose, non-unit
  ! Solve A^H * x = b
  n = 3; nrhs = 1; ldb = 3
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  ap(1) = (2.0d0, 1.0d0)
  ap(2) = (1.0d0, 2.0d0); ap(3) = (4.0d0, 1.0d0)
  ap(4) = (3.0d0, 0.0d0); ap(5) = (5.0d0, -1.0d0); ap(6) = (6.0d0, 2.0d0)
  b(1) = (1.0d0, 1.0d0); b(2) = (2.0d0, -1.0d0); b(3) = (3.0d0, 0.5d0)
  call ztptrs('U', 'C', 'N', n, nrhs, ap, b, ldb, info)
  call begin_test('upper_conj_trans')
  call print_array('x', b_r, 2*n)
  call print_int('info', info)
  call end_test()

  ! Test 4: 3x3 lower triangular packed, conjugate-transpose, non-unit
  ! Solve L^H * x = b
  n = 3; nrhs = 1; ldb = 3
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  ap(1) = (2.0d0, 1.0d0); ap(2) = (1.0d0, 2.0d0); ap(3) = (3.0d0, 0.0d0)
  ap(4) = (4.0d0, 1.0d0); ap(5) = (5.0d0, -1.0d0)
  ap(6) = (6.0d0, 2.0d0)
  b(1) = (1.0d0, 1.0d0); b(2) = (2.0d0, -1.0d0); b(3) = (3.0d0, 0.5d0)
  call ztptrs('L', 'C', 'N', n, nrhs, ap, b, ldb, info)
  call begin_test('lower_conj_trans')
  call print_array('x', b_r, 2*n)
  call print_int('info', info)
  call end_test()

  ! Test 5: 3x3 upper triangular packed, unit diagonal, no transpose
  n = 3; nrhs = 1; ldb = 3
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  ap(1) = (1.0d0, 0.0d0)
  ap(2) = (2.0d0, 1.0d0); ap(3) = (1.0d0, 0.0d0)
  ap(4) = (3.0d0, -1.0d0); ap(5) = (4.0d0, 2.0d0); ap(6) = (1.0d0, 0.0d0)
  b(1) = (10.0d0, 5.0d0); b(2) = (5.0d0, -2.0d0); b(3) = (1.0d0, 1.0d0)
  call ztptrs('U', 'N', 'U', n, nrhs, ap, b, ldb, info)
  call begin_test('upper_unit_diag')
  call print_array('x', b_r, 2*n)
  call print_int('info', info)
  call end_test()

  ! Test 6: 3x3 lower triangular packed, unit diagonal, no transpose
  n = 3; nrhs = 1; ldb = 3
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  ap(1) = (1.0d0, 0.0d0); ap(2) = (2.0d0, 1.0d0); ap(3) = (3.0d0, -1.0d0)
  ap(4) = (1.0d0, 0.0d0); ap(5) = (4.0d0, 2.0d0)
  ap(6) = (1.0d0, 0.0d0)
  b(1) = (10.0d0, 5.0d0); b(2) = (5.0d0, -2.0d0); b(3) = (1.0d0, 1.0d0)
  call ztptrs('L', 'N', 'U', n, nrhs, ap, b, ldb, info)
  call begin_test('lower_unit_diag')
  call print_array('x', b_r, 2*n)
  call print_int('info', info)
  call end_test()

  ! Test 7: N=0 quick return
  info = -99
  call ztptrs('U', 'N', 'N', 0, 1, ap, b, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 8: N=1 edge case
  n = 1; nrhs = 1; ldb = 1
  ap(1) = (5.0d0, 2.0d0)
  b(1) = (15.0d0, 1.0d0)
  call ztptrs('U', 'N', 'N', n, nrhs, ap, b, ldb, info)
  call begin_test('n_one')
  call print_array('x', b_r, 2)
  call print_int('info', info)
  call end_test()

  ! Test 9: Singular matrix (zero on diagonal), upper
  ! Upper packed: [2+1i, 1+2i, 0+0i, 3+0i, 5-1i, 6+2i] -> A(2,2) = 0
  n = 3; nrhs = 1; ldb = 3
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  ap(1) = (2.0d0, 1.0d0)
  ap(2) = (1.0d0, 2.0d0); ap(3) = (0.0d0, 0.0d0)
  ap(4) = (3.0d0, 0.0d0); ap(5) = (5.0d0, -1.0d0); ap(6) = (6.0d0, 2.0d0)
  b(1) = (1.0d0, 0.0d0); b(2) = (1.0d0, 0.0d0); b(3) = (1.0d0, 0.0d0)
  call ztptrs('U', 'N', 'N', n, nrhs, ap, b, ldb, info)
  call begin_test('singular_upper')
  call print_int('info', info)
  call end_test()

  ! Test 10: Singular lower packed: [0+0i, ...] -> L(1,1) = 0
  n = 3; nrhs = 1; ldb = 3
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  ap(1) = (0.0d0, 0.0d0); ap(2) = (1.0d0, 2.0d0); ap(3) = (3.0d0, 0.0d0)
  ap(4) = (4.0d0, 1.0d0); ap(5) = (5.0d0, -1.0d0)
  ap(6) = (6.0d0, 2.0d0)
  b(1) = (1.0d0, 0.0d0); b(2) = (1.0d0, 0.0d0); b(3) = (1.0d0, 0.0d0)
  call ztptrs('L', 'N', 'N', n, nrhs, ap, b, ldb, info)
  call begin_test('singular_lower')
  call print_int('info', info)
  call end_test()

  ! Test 11: Singular lower packed at last diagonal
  n = 3; nrhs = 1; ldb = 3
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  ap(1) = (2.0d0, 1.0d0); ap(2) = (1.0d0, 2.0d0); ap(3) = (3.0d0, 0.0d0)
  ap(4) = (4.0d0, 1.0d0); ap(5) = (5.0d0, -1.0d0)
  ap(6) = (0.0d0, 0.0d0)
  b(1) = (1.0d0, 0.0d0); b(2) = (1.0d0, 0.0d0); b(3) = (1.0d0, 0.0d0)
  call ztptrs('L', 'N', 'N', n, nrhs, ap, b, ldb, info)
  call begin_test('singular_lower_last')
  call print_int('info', info)
  call end_test()

  ! Test 12: Multiple RHS (NRHS=2), upper triangular, no transpose
  n = 3; nrhs = 2; ldb = 3
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  ap(1) = (2.0d0, 1.0d0)
  ap(2) = (1.0d0, 2.0d0); ap(3) = (4.0d0, 1.0d0)
  ap(4) = (3.0d0, 0.0d0); ap(5) = (5.0d0, -1.0d0); ap(6) = (6.0d0, 2.0d0)
  ! B is 3x2, col-major: col1=[1+1i, 2-1i, 3+0.5i], col2=[4+0i, 5-2i, 6+1i]
  b(1) = (1.0d0, 1.0d0); b(2) = (2.0d0, -1.0d0); b(3) = (3.0d0, 0.5d0)
  b(4) = (4.0d0, 0.0d0); b(5) = (5.0d0, -2.0d0); b(6) = (6.0d0, 1.0d0)
  call ztptrs('U', 'N', 'N', n, nrhs, ap, b, ldb, info)
  call begin_test('multi_rhs')
  call print_array('x', b_r, 2*n*nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 13: 4x4 lower, no transpose, non-unit
  n = 4; nrhs = 1; ldb = 4
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  ! L = [3+1i  0  0  0;  1+2i  2+1i  0  0;  4-1i  1+0i  5+2i  0;  2+0i  3-1i  1+1i  4+0i]
  ap(1) = (3.0d0, 1.0d0); ap(2) = (1.0d0, 2.0d0)
  ap(3) = (4.0d0, -1.0d0); ap(4) = (2.0d0, 0.0d0)
  ap(5) = (2.0d0, 1.0d0); ap(6) = (1.0d0, 0.0d0); ap(7) = (3.0d0, -1.0d0)
  ap(8) = (5.0d0, 2.0d0); ap(9) = (1.0d0, 1.0d0)
  ap(10) = (4.0d0, 0.0d0)
  b(1) = (10.0d0, 5.0d0); b(2) = (20.0d0, -3.0d0)
  b(3) = (30.0d0, 1.0d0); b(4) = (40.0d0, -10.0d0)
  call ztptrs('L', 'N', 'N', n, nrhs, ap, b, ldb, info)
  call begin_test('lower_4x4')
  call print_array('x', b_r, 2*n)
  call print_int('info', info)
  call end_test()

  ! Test 14: upper, unit diagonal, conjugate-transpose
  n = 3; nrhs = 1; ldb = 3
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  ap(1) = (1.0d0, 0.0d0)
  ap(2) = (2.0d0, 1.0d0); ap(3) = (1.0d0, 0.0d0)
  ap(4) = (3.0d0, -1.0d0); ap(5) = (4.0d0, 2.0d0); ap(6) = (1.0d0, 0.0d0)
  b(1) = (10.0d0, 5.0d0); b(2) = (5.0d0, -2.0d0); b(3) = (1.0d0, 1.0d0)
  call ztptrs('U', 'C', 'U', n, nrhs, ap, b, ldb, info)
  call begin_test('upper_unit_conj_trans')
  call print_array('x', b_r, 2*n)
  call print_int('info', info)
  call end_test()

end program
