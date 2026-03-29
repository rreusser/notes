program test_zlagtm
  use test_utils
  implicit none
  complex*16 :: dl(10), d(10), du(10), x(10, 4), b(10, 4)
  double precision :: b_r(80), x_r(80)
  equivalence (b, b_r)
  equivalence (x, x_r)

  ! Set up a 4x4 complex tridiagonal matrix:
  ! D  = [2+1i, 4+2i, 5-1i, 6+3i]
  ! DL = [3+1i, 1-2i, 2+0i]
  ! DU = [-1+1i, -2+3i, -3-1i]
  dl(1) = (3.0d0, 1.0d0)
  dl(2) = (1.0d0, -2.0d0)
  dl(3) = (2.0d0, 0.0d0)
  d(1) = (2.0d0, 1.0d0)
  d(2) = (4.0d0, 2.0d0)
  d(3) = (5.0d0, -1.0d0)
  d(4) = (6.0d0, 3.0d0)
  du(1) = (-1.0d0, 1.0d0)
  du(2) = (-2.0d0, 3.0d0)
  du(3) = (-3.0d0, -1.0d0)

  ! Test 1: no-transpose, alpha=1, beta=0, N=4, NRHS=1
  x(1, 1) = (1.0d0, 0.0d0)
  x(2, 1) = (2.0d0, 1.0d0)
  x(3, 1) = (3.0d0, -1.0d0)
  x(4, 1) = (4.0d0, 2.0d0)
  b = (0.0d0, 0.0d0)
  call zlagtm('N', 4, 1, 1.0d0, dl, d, du, x, 10, 0.0d0, b, 10)
  call begin_test('notrans_alpha1_beta0')
  call print_array('b', b_r(1:8), 8)
  call end_test()

  ! Test 2: transpose, alpha=1, beta=0, N=4, NRHS=1
  b = (0.0d0, 0.0d0)
  call zlagtm('T', 4, 1, 1.0d0, dl, d, du, x, 10, 0.0d0, b, 10)
  call begin_test('trans_alpha1_beta0')
  call print_array('b', b_r(1:8), 8)
  call end_test()

  ! Test 3: conjugate-transpose, alpha=1, beta=0, N=4, NRHS=1
  b = (0.0d0, 0.0d0)
  call zlagtm('C', 4, 1, 1.0d0, dl, d, du, x, 10, 0.0d0, b, 10)
  call begin_test('conjtrans_alpha1_beta0')
  call print_array('b', b_r(1:8), 8)
  call end_test()

  ! Test 4: no-transpose, alpha=-1, beta=0, N=4, NRHS=1
  b = (0.0d0, 0.0d0)
  call zlagtm('N', 4, 1, -1.0d0, dl, d, du, x, 10, 0.0d0, b, 10)
  call begin_test('notrans_alpham1_beta0')
  call print_array('b', b_r(1:8), 8)
  call end_test()

  ! Test 5: transpose, alpha=-1, beta=0, N=4, NRHS=1
  b = (0.0d0, 0.0d0)
  call zlagtm('T', 4, 1, -1.0d0, dl, d, du, x, 10, 0.0d0, b, 10)
  call begin_test('trans_alpham1_beta0')
  call print_array('b', b_r(1:8), 8)
  call end_test()

  ! Test 6: conjugate-transpose, alpha=-1, beta=0, N=4, NRHS=1
  b = (0.0d0, 0.0d0)
  call zlagtm('C', 4, 1, -1.0d0, dl, d, du, x, 10, 0.0d0, b, 10)
  call begin_test('conjtrans_alpham1_beta0')
  call print_array('b', b_r(1:8), 8)
  call end_test()

  ! Test 7: no-transpose, alpha=1, beta=1 (accumulate into B)
  b(1, 1) = (10.0d0, 5.0d0)
  b(2, 1) = (20.0d0, -3.0d0)
  b(3, 1) = (30.0d0, 7.0d0)
  b(4, 1) = (40.0d0, -2.0d0)
  call zlagtm('N', 4, 1, 1.0d0, dl, d, du, x, 10, 1.0d0, b, 10)
  call begin_test('notrans_alpha1_beta1')
  call print_array('b', b_r(1:8), 8)
  call end_test()

  ! Test 8: no-transpose, alpha=1, beta=-1 (negate B first)
  b(1, 1) = (10.0d0, 5.0d0)
  b(2, 1) = (20.0d0, -3.0d0)
  b(3, 1) = (30.0d0, 7.0d0)
  b(4, 1) = (40.0d0, -2.0d0)
  call zlagtm('N', 4, 1, 1.0d0, dl, d, du, x, 10, -1.0d0, b, 10)
  call begin_test('notrans_alpha1_betam1')
  call print_array('b', b_r(1:8), 8)
  call end_test()

  ! Test 9: no-transpose, alpha=0, beta=0 (zero everything)
  b(1, 1) = (10.0d0, 5.0d0)
  b(2, 1) = (20.0d0, -3.0d0)
  b(3, 1) = (30.0d0, 7.0d0)
  b(4, 1) = (40.0d0, -2.0d0)
  call zlagtm('N', 4, 1, 0.0d0, dl, d, du, x, 10, 0.0d0, b, 10)
  call begin_test('notrans_alpha0_beta0')
  call print_array('b', b_r(1:8), 8)
  call end_test()

  ! Test 10: no-transpose, alpha=0, beta=1 (no change)
  b(1, 1) = (10.0d0, 5.0d0)
  b(2, 1) = (20.0d0, -3.0d0)
  b(3, 1) = (30.0d0, 7.0d0)
  b(4, 1) = (40.0d0, -2.0d0)
  call zlagtm('N', 4, 1, 0.0d0, dl, d, du, x, 10, 1.0d0, b, 10)
  call begin_test('notrans_alpha0_beta1')
  call print_array('b', b_r(1:8), 8)
  call end_test()

  ! Test 11: N=1, no-transpose
  d(1) = (5.0d0, -2.0d0)
  x(1, 1) = (3.0d0, 1.0d0)
  b = (0.0d0, 0.0d0)
  call zlagtm('N', 1, 1, 1.0d0, dl, d, du, x, 10, 0.0d0, b, 10)
  call begin_test('n1_notrans')
  call print_array('b', b_r(1:2), 2)
  call end_test()

  ! Test 12: N=1, transpose
  b = (0.0d0, 0.0d0)
  call zlagtm('T', 1, 1, 1.0d0, dl, d, du, x, 10, 0.0d0, b, 10)
  call begin_test('n1_trans')
  call print_array('b', b_r(1:2), 2)
  call end_test()

  ! Test 13: N=1, conjugate-transpose
  b = (0.0d0, 0.0d0)
  call zlagtm('C', 1, 1, 1.0d0, dl, d, du, x, 10, 0.0d0, b, 10)
  call begin_test('n1_conjtrans')
  call print_array('b', b_r(1:2), 2)
  call end_test()

  ! Test 14: N=0 (quick return) - B should remain unchanged
  b(1, 1) = (99.0d0, -99.0d0)
  call zlagtm('N', 0, 1, 1.0d0, dl, d, du, x, 10, 0.0d0, b, 10)
  call begin_test('n0_quickreturn')
  call print_array('b', b_r(1:2), 2)
  call end_test()

  ! Test 15: multiple RHS, no-transpose, alpha=1, beta=0
  dl(1) = (3.0d0, 1.0d0)
  dl(2) = (1.0d0, -2.0d0)
  dl(3) = (2.0d0, 0.0d0)
  d(1) = (2.0d0, 1.0d0)
  d(2) = (4.0d0, 2.0d0)
  d(3) = (5.0d0, -1.0d0)
  d(4) = (6.0d0, 3.0d0)
  du(1) = (-1.0d0, 1.0d0)
  du(2) = (-2.0d0, 3.0d0)
  du(3) = (-3.0d0, -1.0d0)
  x(1, 1) = (1.0d0, 0.0d0)
  x(2, 1) = (0.0d0, 1.0d0)
  x(3, 1) = (-1.0d0, 0.0d0)
  x(4, 1) = (0.0d0, -1.0d0)
  x(1, 2) = (2.0d0, 1.0d0)
  x(2, 2) = (1.0d0, -1.0d0)
  x(3, 2) = (3.0d0, 0.0d0)
  x(4, 2) = (-1.0d0, 2.0d0)
  b = (0.0d0, 0.0d0)
  call zlagtm('N', 4, 2, 1.0d0, dl, d, du, x, 10, 0.0d0, b, 10)
  call begin_test('notrans_multi_rhs')
  call print_array('b1', b_r(1:8), 8)
  call print_array('b2', b_r(21:28), 8)
  call end_test()

  ! Test 16: conjugate-transpose, multiple RHS, alpha=-1, beta=-1
  b(1, 1) = (1.0d0, 1.0d0)
  b(2, 1) = (2.0d0, 2.0d0)
  b(3, 1) = (3.0d0, 3.0d0)
  b(4, 1) = (4.0d0, 4.0d0)
  b(1, 2) = (5.0d0, 5.0d0)
  b(2, 2) = (6.0d0, 6.0d0)
  b(3, 2) = (7.0d0, 7.0d0)
  b(4, 2) = (8.0d0, 8.0d0)
  call zlagtm('C', 4, 2, -1.0d0, dl, d, du, x, 10, -1.0d0, b, 10)
  call begin_test('conjtrans_alpham1_betam1_multi_rhs')
  call print_array('b1', b_r(1:8), 8)
  call print_array('b2', b_r(21:28), 8)
  call end_test()

  ! Test 17: N=1, alpha=-1, beta=-1
  d(1) = (5.0d0, -2.0d0)
  x(1, 1) = (3.0d0, 1.0d0)
  b(1, 1) = (7.0d0, 3.0d0)
  call zlagtm('N', 1, 1, -1.0d0, dl, d, du, x, 10, -1.0d0, b, 10)
  call begin_test('n1_alpham1_betam1')
  call print_array('b', b_r(1:2), 2)
  call end_test()

end program
