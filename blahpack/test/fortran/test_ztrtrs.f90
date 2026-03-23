program test_ztrtrs
  use test_utils
  implicit none

  complex*16 :: a(100), b(100)
  double precision :: a_r(200), b_r(200)
  equivalence (a, a_r)
  equivalence (b, b_r)
  integer :: info

  ! Test 1: 3x3 upper triangular, no transpose
  ! A = [2+1i  1+2i  3+0i;  0  4+1i  5+2i;  0  0  6+1i]
  ! b = [1+0i; 2+1i; 3+2i]
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  a(1) = (2.0d0, 1.0d0); a(2) = (0.0d0, 0.0d0); a(3) = (0.0d0, 0.0d0)
  a(4) = (1.0d0, 2.0d0); a(5) = (4.0d0, 1.0d0); a(6) = (0.0d0, 0.0d0)
  a(7) = (3.0d0, 0.0d0); a(8) = (5.0d0, 2.0d0); a(9) = (6.0d0, 1.0d0)
  b(1) = (1.0d0, 0.0d0); b(2) = (2.0d0, 1.0d0); b(3) = (3.0d0, 2.0d0)
  call ztrtrs('U', 'N', 'N', 3, 1, a, 3, b, 3, info)
  call begin_test('upper_no_trans')
  call print_array('x', b_r, 6)
  call print_int('info', info)
  call end_test()

  ! Test 2: 3x3 lower triangular, no transpose
  ! L = [2+1i  0  0;  1+2i  4+1i  0;  3+0i  5+2i  6+1i]
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  a(1) = (2.0d0, 1.0d0); a(2) = (1.0d0, 2.0d0); a(3) = (3.0d0, 0.0d0)
  a(4) = (0.0d0, 0.0d0); a(5) = (4.0d0, 1.0d0); a(6) = (5.0d0, 2.0d0)
  a(7) = (0.0d0, 0.0d0); a(8) = (0.0d0, 0.0d0); a(9) = (6.0d0, 1.0d0)
  b(1) = (1.0d0, 0.0d0); b(2) = (2.0d0, 1.0d0); b(3) = (3.0d0, 2.0d0)
  call ztrtrs('L', 'N', 'N', 3, 1, a, 3, b, 3, info)
  call begin_test('lower_no_trans')
  call print_array('x', b_r, 6)
  call print_int('info', info)
  call end_test()

  ! Test 3: 3x3 upper triangular, transpose (A^T * X = B)
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  a(1) = (2.0d0, 1.0d0); a(2) = (0.0d0, 0.0d0); a(3) = (0.0d0, 0.0d0)
  a(4) = (1.0d0, 2.0d0); a(5) = (4.0d0, 1.0d0); a(6) = (0.0d0, 0.0d0)
  a(7) = (3.0d0, 0.0d0); a(8) = (5.0d0, 2.0d0); a(9) = (6.0d0, 1.0d0)
  b(1) = (1.0d0, 0.0d0); b(2) = (2.0d0, 1.0d0); b(3) = (3.0d0, 2.0d0)
  call ztrtrs('U', 'T', 'N', 3, 1, a, 3, b, 3, info)
  call begin_test('upper_trans')
  call print_array('x', b_r, 6)
  call print_int('info', info)
  call end_test()

  ! Test 4: 3x3 upper triangular, conjugate transpose (A^H * X = B)
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  a(1) = (2.0d0, 1.0d0); a(2) = (0.0d0, 0.0d0); a(3) = (0.0d0, 0.0d0)
  a(4) = (1.0d0, 2.0d0); a(5) = (4.0d0, 1.0d0); a(6) = (0.0d0, 0.0d0)
  a(7) = (3.0d0, 0.0d0); a(8) = (5.0d0, 2.0d0); a(9) = (6.0d0, 1.0d0)
  b(1) = (1.0d0, 0.0d0); b(2) = (2.0d0, 1.0d0); b(3) = (3.0d0, 2.0d0)
  call ztrtrs('U', 'C', 'N', 3, 1, a, 3, b, 3, info)
  call begin_test('upper_conj_trans')
  call print_array('x', b_r, 6)
  call print_int('info', info)
  call end_test()

  ! Test 5: unit diagonal
  ! A = [*  2+1i  3+0i;  0  *  4+2i;  0  0  *] where * = unit diagonal (ignored)
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  a(1) = (99.0d0, 99.0d0); a(2) = (0.0d0, 0.0d0); a(3) = (0.0d0, 0.0d0)
  a(4) = (2.0d0, 1.0d0); a(5) = (99.0d0, 99.0d0); a(6) = (0.0d0, 0.0d0)
  a(7) = (3.0d0, 0.0d0); a(8) = (4.0d0, 2.0d0); a(9) = (99.0d0, 99.0d0)
  b(1) = (10.0d0, 5.0d0); b(2) = (5.0d0, 3.0d0); b(3) = (1.0d0, 1.0d0)
  call ztrtrs('U', 'N', 'U', 3, 1, a, 3, b, 3, info)
  call begin_test('upper_unit_diag')
  call print_array('x', b_r, 6)
  call print_int('info', info)
  call end_test()

  ! Test 6: N=0 quick return
  info = -99
  call ztrtrs('U', 'N', 'N', 0, 1, a, 1, b, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 7: singular (zero diagonal element) -> info > 0
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  a(1) = (2.0d0, 1.0d0); a(2) = (0.0d0, 0.0d0); a(3) = (0.0d0, 0.0d0)
  a(4) = (1.0d0, 2.0d0); a(5) = (0.0d0, 0.0d0); a(6) = (0.0d0, 0.0d0)
  a(7) = (3.0d0, 0.0d0); a(8) = (5.0d0, 2.0d0); a(9) = (6.0d0, 1.0d0)
  b(1) = (1.0d0, 0.0d0); b(2) = (2.0d0, 1.0d0); b(3) = (3.0d0, 2.0d0)
  call ztrtrs('U', 'N', 'N', 3, 1, a, 3, b, 3, info)
  call begin_test('singular')
  call print_int('info', info)
  call end_test()

  ! Test 8: multiple RHS (NRHS=2)
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  a(1) = (2.0d0, 1.0d0); a(2) = (0.0d0, 0.0d0); a(3) = (0.0d0, 0.0d0)
  a(4) = (1.0d0, 2.0d0); a(5) = (4.0d0, 1.0d0); a(6) = (0.0d0, 0.0d0)
  a(7) = (3.0d0, 0.0d0); a(8) = (5.0d0, 2.0d0); a(9) = (6.0d0, 1.0d0)
  b(1) = (1.0d0, 0.0d0); b(2) = (2.0d0, 1.0d0); b(3) = (3.0d0, 2.0d0)
  b(4) = (4.0d0, 1.0d0); b(5) = (5.0d0, 2.0d0); b(6) = (6.0d0, 3.0d0)
  call ztrtrs('U', 'N', 'N', 3, 2, a, 3, b, 3, info)
  call begin_test('multi_rhs')
  call print_array('x', b_r, 12)
  call print_int('info', info)
  call end_test()

  ! Test 9: lower triangular, transpose
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  a(1) = (2.0d0, 1.0d0); a(2) = (1.0d0, 2.0d0); a(3) = (3.0d0, 0.0d0)
  a(4) = (0.0d0, 0.0d0); a(5) = (4.0d0, 1.0d0); a(6) = (5.0d0, 2.0d0)
  a(7) = (0.0d0, 0.0d0); a(8) = (0.0d0, 0.0d0); a(9) = (6.0d0, 1.0d0)
  b(1) = (1.0d0, 0.0d0); b(2) = (2.0d0, 1.0d0); b(3) = (3.0d0, 2.0d0)
  call ztrtrs('L', 'T', 'N', 3, 1, a, 3, b, 3, info)
  call begin_test('lower_trans')
  call print_array('x', b_r, 6)
  call print_int('info', info)
  call end_test()

  ! Test 10: lower triangular, conjugate transpose
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  a(1) = (2.0d0, 1.0d0); a(2) = (1.0d0, 2.0d0); a(3) = (3.0d0, 0.0d0)
  a(4) = (0.0d0, 0.0d0); a(5) = (4.0d0, 1.0d0); a(6) = (5.0d0, 2.0d0)
  a(7) = (0.0d0, 0.0d0); a(8) = (0.0d0, 0.0d0); a(9) = (6.0d0, 1.0d0)
  b(1) = (1.0d0, 0.0d0); b(2) = (2.0d0, 1.0d0); b(3) = (3.0d0, 2.0d0)
  call ztrtrs('L', 'C', 'N', 3, 1, a, 3, b, 3, info)
  call begin_test('lower_conj_trans')
  call print_array('x', b_r, 6)
  call print_int('info', info)
  call end_test()

  ! Test 11: singular at first element
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  a(1) = (0.0d0, 0.0d0)
  a(4) = (1.0d0, 0.0d0); a(5) = (2.0d0, 0.0d0)
  a(7) = (1.0d0, 0.0d0); a(8) = (1.0d0, 0.0d0); a(9) = (3.0d0, 0.0d0)
  b(1) = (1.0d0, 0.0d0); b(2) = (2.0d0, 0.0d0); b(3) = (3.0d0, 0.0d0)
  call ztrtrs('U', 'N', 'N', 3, 1, a, 3, b, 3, info)
  call begin_test('singular_first')
  call print_int('info', info)
  call end_test()

  ! Test 12: singular at last element
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  a(1) = (2.0d0, 0.0d0)
  a(4) = (1.0d0, 0.0d0); a(5) = (3.0d0, 0.0d0)
  a(7) = (1.0d0, 0.0d0); a(8) = (1.0d0, 0.0d0); a(9) = (0.0d0, 0.0d0)
  b(1) = (1.0d0, 0.0d0); b(2) = (2.0d0, 0.0d0); b(3) = (3.0d0, 0.0d0)
  call ztrtrs('U', 'N', 'N', 3, 1, a, 3, b, 3, info)
  call begin_test('singular_last')
  call print_int('info', info)
  call end_test()

  ! Test 13: NRHS=0 (no right-hand sides)
  a = (0.0d0, 0.0d0)
  a(1) = (2.0d0, 1.0d0)
  info = -99
  call ztrtrs('U', 'N', 'N', 1, 0, a, 1, b, 1, info)
  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

end program
