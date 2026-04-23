program test_dtptrs
  use test_utils
  implicit none
  double precision :: ap(100), b(100)
  integer :: info, n, nrhs, ldb

  ! Test 1: 3x3 upper triangular packed, no transpose, non-unit diagonal
  ! A = [2 1 3; 0 4 5; 0 0 6] packed as [2, 1, 4, 3, 5, 6]
  ! b = [1; 2; 3]
  n = 3; nrhs = 1; ldb = 3
  ap = 0.0d0; b = 0.0d0
  ap(1) = 2.0d0; ap(2) = 1.0d0; ap(3) = 4.0d0
  ap(4) = 3.0d0; ap(5) = 5.0d0; ap(6) = 6.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  call dtptrs('U', 'N', 'N', n, nrhs, ap, b, ldb, info)
  call begin_test('upper_no_trans')
  call print_array('x', b, n)
  call print_int('info', info)
  call end_test()

  ! Test 2: 3x3 lower triangular packed, no transpose, non-unit diagonal
  ! L = [2 0 0; 1 4 0; 3 5 6] packed as [2, 1, 3, 4, 5, 6]
  ! b = [1; 2; 3]
  n = 3; nrhs = 1; ldb = 3
  ap = 0.0d0; b = 0.0d0
  ap(1) = 2.0d0; ap(2) = 1.0d0; ap(3) = 3.0d0
  ap(4) = 4.0d0; ap(5) = 5.0d0; ap(6) = 6.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  call dtptrs('L', 'N', 'N', n, nrhs, ap, b, ldb, info)
  call begin_test('lower_no_trans')
  call print_array('x', b, n)
  call print_int('info', info)
  call end_test()

  ! Test 3: 3x3 upper triangular packed, transpose
  ! A = [2 1 3; 0 4 5; 0 0 6] packed as [2, 1, 4, 3, 5, 6]
  ! Solve A^T * x = b
  n = 3; nrhs = 1; ldb = 3
  ap = 0.0d0; b = 0.0d0
  ap(1) = 2.0d0; ap(2) = 1.0d0; ap(3) = 4.0d0
  ap(4) = 3.0d0; ap(5) = 5.0d0; ap(6) = 6.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  call dtptrs('U', 'T', 'N', n, nrhs, ap, b, ldb, info)
  call begin_test('upper_trans')
  call print_array('x', b, n)
  call print_int('info', info)
  call end_test()

  ! Test 4: 3x3 lower triangular packed, transpose
  ! L = [2 0 0; 1 4 0; 3 5 6] packed as [2, 1, 3, 4, 5, 6]
  ! Solve L^T * x = b
  n = 3; nrhs = 1; ldb = 3
  ap = 0.0d0; b = 0.0d0
  ap(1) = 2.0d0; ap(2) = 1.0d0; ap(3) = 3.0d0
  ap(4) = 4.0d0; ap(5) = 5.0d0; ap(6) = 6.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  call dtptrs('L', 'T', 'N', n, nrhs, ap, b, ldb, info)
  call begin_test('lower_trans')
  call print_array('x', b, n)
  call print_int('info', info)
  call end_test()

  ! Test 5: 3x3 upper triangular packed, unit diagonal
  ! A = [* 2 3; 0 * 4; 0 0 *] (unit diag, packed as [*, 2, *, 3, 4, *])
  ! b = [10; 5; 1]
  n = 3; nrhs = 1; ldb = 3
  ap = 0.0d0; b = 0.0d0
  ap(1) = 1.0d0; ap(2) = 2.0d0; ap(3) = 1.0d0
  ap(4) = 3.0d0; ap(5) = 4.0d0; ap(6) = 1.0d0
  b(1) = 10.0d0; b(2) = 5.0d0; b(3) = 1.0d0
  call dtptrs('U', 'N', 'U', n, nrhs, ap, b, ldb, info)
  call begin_test('upper_unit_diag')
  call print_array('x', b, n)
  call print_int('info', info)
  call end_test()

  ! Test 6: 3x3 lower triangular packed, unit diagonal
  ! L = [* 0 0; 2 * 0; 3 4 *] (unit diag, packed as [*, 2, 3, *, 4, *])
  n = 3; nrhs = 1; ldb = 3
  ap = 0.0d0; b = 0.0d0
  ap(1) = 1.0d0; ap(2) = 2.0d0; ap(3) = 3.0d0
  ap(4) = 1.0d0; ap(5) = 4.0d0; ap(6) = 1.0d0
  b(1) = 10.0d0; b(2) = 5.0d0; b(3) = 1.0d0
  call dtptrs('L', 'N', 'U', n, nrhs, ap, b, ldb, info)
  call begin_test('lower_unit_diag')
  call print_array('x', b, n)
  call print_int('info', info)
  call end_test()

  ! Test 7: N=0 quick return
  call dtptrs('U', 'N', 'N', 0, 1, ap, b, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 8: N=1 edge case
  n = 1; nrhs = 1; ldb = 1
  ap(1) = 5.0d0
  b(1) = 15.0d0
  call dtptrs('U', 'N', 'N', n, nrhs, ap, b, ldb, info)
  call begin_test('n_one')
  call print_array('x', b, n)
  call print_int('info', info)
  call end_test()

  ! Test 9: Singular matrix (zero on diagonal) -> info > 0
  ! Upper packed: [2, 1, 0, 3, 5, 6] -> A(2,2) = 0
  n = 3; nrhs = 1; ldb = 3
  ap = 0.0d0; b = 0.0d0
  ap(1) = 2.0d0; ap(2) = 1.0d0; ap(3) = 0.0d0
  ap(4) = 3.0d0; ap(5) = 5.0d0; ap(6) = 6.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  call dtptrs('U', 'N', 'N', n, nrhs, ap, b, ldb, info)
  call begin_test('singular_upper')
  call print_int('info', info)
  call end_test()

  ! Test 10: Singular lower packed: [0, 1, 3, 4, 5, 6] -> L(1,1) = 0
  n = 3; nrhs = 1; ldb = 3
  ap = 0.0d0; b = 0.0d0
  ap(1) = 0.0d0; ap(2) = 1.0d0; ap(3) = 3.0d0
  ap(4) = 4.0d0; ap(5) = 5.0d0; ap(6) = 6.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  call dtptrs('L', 'N', 'N', n, nrhs, ap, b, ldb, info)
  call begin_test('singular_lower')
  call print_int('info', info)
  call end_test()

  ! Test 11: Singular lower packed at last diagonal: [2, 1, 3, 4, 5, 0] -> L(3,3) = 0
  n = 3; nrhs = 1; ldb = 3
  ap = 0.0d0; b = 0.0d0
  ap(1) = 2.0d0; ap(2) = 1.0d0; ap(3) = 3.0d0
  ap(4) = 4.0d0; ap(5) = 5.0d0; ap(6) = 0.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  call dtptrs('L', 'N', 'N', n, nrhs, ap, b, ldb, info)
  call begin_test('singular_lower_last')
  call print_int('info', info)
  call end_test()

  ! Test 12: Multiple RHS (NRHS=2), upper triangular
  n = 3; nrhs = 2; ldb = 3
  ap = 0.0d0; b = 0.0d0
  ap(1) = 2.0d0; ap(2) = 1.0d0; ap(3) = 4.0d0
  ap(4) = 3.0d0; ap(5) = 5.0d0; ap(6) = 6.0d0
  ! B is 3x2, col-major: col1=[1,2,3], col2=[4,5,6]
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  b(4) = 4.0d0; b(5) = 5.0d0; b(6) = 6.0d0
  call dtptrs('U', 'N', 'N', n, nrhs, ap, b, ldb, info)
  call begin_test('multi_rhs')
  call print_array('x', b, n*nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 13: conjugate-transpose (same as transpose for real)
  n = 3; nrhs = 1; ldb = 3
  ap = 0.0d0; b = 0.0d0
  ap(1) = 2.0d0; ap(2) = 1.0d0; ap(3) = 4.0d0
  ap(4) = 3.0d0; ap(5) = 5.0d0; ap(6) = 6.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  call dtptrs('U', 'C', 'N', n, nrhs, ap, b, ldb, info)
  call begin_test('upper_conj_trans')
  call print_array('x', b, n)
  call print_int('info', info)
  call end_test()

  ! Test 14: 4x4 lower, no transpose (slightly larger matrix)
  n = 4; nrhs = 1; ldb = 4
  ap = 0.0d0; b = 0.0d0
  ! L = [3 0 0 0; 1 2 0 0; 4 1 5 0; 2 3 1 4]
  ! Packed: [3, 1, 4, 2, 2, 1, 3, 5, 1, 4]
  ap(1) = 3.0d0; ap(2) = 1.0d0; ap(3) = 4.0d0; ap(4) = 2.0d0
  ap(5) = 2.0d0; ap(6) = 1.0d0; ap(7) = 3.0d0
  ap(8) = 5.0d0; ap(9) = 1.0d0
  ap(10) = 4.0d0
  b(1) = 10.0d0; b(2) = 20.0d0; b(3) = 30.0d0; b(4) = 40.0d0
  call dtptrs('L', 'N', 'N', n, nrhs, ap, b, ldb, info)
  call begin_test('lower_4x4')
  call print_array('x', b, n)
  call print_int('info', info)
  call end_test()

end program
