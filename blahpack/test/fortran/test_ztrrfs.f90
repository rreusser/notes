program test_ztrrfs
  use test_utils
  implicit none
  complex*16 :: a(100), b(100), x(100), work(200)
  double precision :: a_r(200), b_r(200), x_r(200), work_r(400)
  equivalence (a, a_r)
  equivalence (b, b_r)
  equivalence (x, x_r)
  equivalence (work, work_r)
  double precision :: ferr(10), berr(10), rwork(100)
  integer :: info, n, nrhs, lda, ldb, ldx

  ! Test 1: 3x3 upper triangular, no transpose, single RHS
  ! A = [2+1i  1+2i  3+1i;  0  4+1i  5+2i;  0  0  6+1i]
  n = 3; nrhs = 1; lda = 3; ldb = 3; ldx = 3
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  a(1) = (2.0d0, 1.0d0); a(2) = (0.0d0, 0.0d0); a(3) = (0.0d0, 0.0d0)
  a(4) = (1.0d0, 2.0d0); a(5) = (4.0d0, 1.0d0); a(6) = (0.0d0, 0.0d0)
  a(7) = (3.0d0, 1.0d0); a(8) = (5.0d0, 2.0d0); a(9) = (6.0d0, 1.0d0)
  ! x = [1+0i; 1+0i; 1+0i], compute b = A*x
  x(1) = (1.0d0, 0.0d0); x(2) = (1.0d0, 0.0d0); x(3) = (1.0d0, 0.0d0)
  ! b = A*x: row1=(2+1i)+(1+2i)+(3+1i)=(6+4i), row2=(4+1i)+(5+2i)=(9+3i), row3=(6+1i)
  b(1) = (6.0d0, 4.0d0); b(2) = (9.0d0, 3.0d0); b(3) = (6.0d0, 1.0d0)
  ! Solve A*x = b
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call ztrsv('U', 'N', 'N', n, a, lda, x, 1)
  call ztrrfs('U', 'N', 'N', n, nrhs, a, lda, b, ldb, x, ldx, &
              ferr, berr, work, rwork, info)
  call begin_test('upper_no_trans')
  call print_array('x', x_r, 2*n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 2: 3x3 lower triangular, conjugate-transpose, single RHS
  ! L = [2+1i 0 0; 1+2i 4+1i 0; 3+1i 5+2i 6+1i]
  n = 3; nrhs = 1; lda = 3; ldb = 3; ldx = 3
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  a(1) = (2.0d0, 1.0d0); a(2) = (1.0d0, 2.0d0); a(3) = (3.0d0, 1.0d0)
  a(4) = (0.0d0, 0.0d0); a(5) = (4.0d0, 1.0d0); a(6) = (5.0d0, 2.0d0)
  a(7) = (0.0d0, 0.0d0); a(8) = (0.0d0, 0.0d0); a(9) = (6.0d0, 1.0d0)
  ! x = [1+1i; 2+0i; 1-1i], compute b = L^H * x
  ! L^H = [2-1i  1-2i  3-1i; 0 4-1i 5-2i; 0 0 6-1i]
  ! b_1 = (2-i)(1+i) + (1-2i)(2) + (3-i)(1-i) = (3+i)+(2-4i)+(2-4i) = (7-7i)
  ! b_2 = (4-i)(2) + (5-2i)(1-i) = (8-2i) + (3-7i) = (11-9i)
  ! b_3 = (6-i)(1-i) = (5-7i)
  b(1) = (7.0d0, -7.0d0); b(2) = (11.0d0, -9.0d0); b(3) = (5.0d0, -7.0d0)
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call ztrsv('L', 'C', 'N', n, a, lda, x, 1)
  call ztrrfs('L', 'C', 'N', n, nrhs, a, lda, b, ldb, x, ldx, &
              ferr, berr, work, rwork, info)
  call begin_test('lower_conj_trans')
  call print_array('x', x_r, 2*n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 3: 3x3 upper triangular, unit diagonal, no transpose
  ! A = [1 2+1i 3+2i; 0 1 4+1i; 0 0 1]
  n = 3; nrhs = 1; lda = 3; ldb = 3; ldx = 3
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0); a(2) = (0.0d0, 0.0d0); a(3) = (0.0d0, 0.0d0)
  a(4) = (2.0d0, 1.0d0); a(5) = (1.0d0, 0.0d0); a(6) = (0.0d0, 0.0d0)
  a(7) = (3.0d0, 2.0d0); a(8) = (4.0d0, 1.0d0); a(9) = (1.0d0, 0.0d0)
  ! x = [1; 2; 3], b = A*x
  ! b_1 = 1 + 2*(2+i) + 3*(3+2i) = 1 + (4+2i) + (9+6i) = (14+8i)
  ! b_2 = 2 + 3*(4+i) = 2 + (12+3i) = (14+3i)
  ! b_3 = 3
  b(1) = (14.0d0, 8.0d0); b(2) = (14.0d0, 3.0d0); b(3) = (3.0d0, 0.0d0)
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call ztrsv('U', 'N', 'U', n, a, lda, x, 1)
  call ztrrfs('U', 'N', 'U', n, nrhs, a, lda, b, ldb, x, ldx, &
              ferr, berr, work, rwork, info)
  call begin_test('upper_unit_diag')
  call print_array('x', x_r, 2*n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 4: 3x3 lower triangular, no transpose, single RHS
  ! L = [3+1i 0 0; 2+1i 5+2i 0; 1+1i 4+1i 7+1i]
  n = 3; nrhs = 1; lda = 3; ldb = 3; ldx = 3
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  a(1) = (3.0d0, 1.0d0); a(2) = (2.0d0, 1.0d0); a(3) = (1.0d0, 1.0d0)
  a(4) = (0.0d0, 0.0d0); a(5) = (5.0d0, 2.0d0); a(6) = (4.0d0, 1.0d0)
  a(7) = (0.0d0, 0.0d0); a(8) = (0.0d0, 0.0d0); a(9) = (7.0d0, 1.0d0)
  ! x = [1; 1; 1], b = L*x
  ! b_1 = (3+i)
  ! b_2 = (2+i) + (5+2i) = (7+3i)
  ! b_3 = (1+i) + (4+i) + (7+i) = (12+3i)
  b(1) = (3.0d0, 1.0d0); b(2) = (7.0d0, 3.0d0); b(3) = (12.0d0, 3.0d0)
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call ztrsv('L', 'N', 'N', n, a, lda, x, 1)
  call ztrrfs('L', 'N', 'N', n, nrhs, a, lda, b, ldb, x, ldx, &
              ferr, berr, work, rwork, info)
  call begin_test('lower_no_trans')
  call print_array('x', x_r, 2*n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 5: Multiple RHS (NRHS=2), upper, no transpose
  n = 3; nrhs = 2; lda = 3; ldb = 3; ldx = 3
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  a(1) = (2.0d0, 1.0d0); a(2) = (0.0d0, 0.0d0); a(3) = (0.0d0, 0.0d0)
  a(4) = (1.0d0, 2.0d0); a(5) = (4.0d0, 1.0d0); a(6) = (0.0d0, 0.0d0)
  a(7) = (3.0d0, 1.0d0); a(8) = (5.0d0, 2.0d0); a(9) = (6.0d0, 1.0d0)
  ! RHS 1: x=[1;1;1], b=A*x same as test 1
  b(1) = (6.0d0, 4.0d0); b(2) = (9.0d0, 3.0d0); b(3) = (6.0d0, 1.0d0)
  ! RHS 2: x=[1+i; 2; 1-i], b=A*x
  ! b_4 = (2+i)(1+i) + (1+2i)(2) + (3+i)(1-i) = (1+3i) + (2+4i) + (4-2i) = (7+5i)
  ! b_5 = (4+i)(2) + (5+2i)(1-i) = (8+2i) + (7-3i) = (15-i)
  ! b_6 = (6+i)(1-i) = (7-5i)
  b(4) = (7.0d0, 5.0d0); b(5) = (15.0d0, -1.0d0); b(6) = (7.0d0, -5.0d0)
  ! Solve each column
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  x(4) = b(4); x(5) = b(5); x(6) = b(6)
  call ztrsv('U', 'N', 'N', n, a, lda, x(1), 1)
  call ztrsv('U', 'N', 'N', n, a, lda, x(4), 1)
  call ztrrfs('U', 'N', 'N', n, nrhs, a, lda, b, ldb, x, ldx, &
              ferr, berr, work, rwork, info)
  call begin_test('multi_rhs')
  call print_array('x', x_r, 2*n*nrhs)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 6: N=0 quick return
  ferr(1) = 99.0d0; berr(1) = 99.0d0
  call ztrrfs('U', 'N', 'N', 0, 1, a, 1, b, 1, x, 1, &
              ferr, berr, work, rwork, info)
  call begin_test('n_zero')
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_int('info', info)
  call end_test()

  ! Test 7: NRHS=0 quick return
  call ztrrfs('U', 'N', 'N', 3, 0, a, 3, b, 3, x, 3, &
              ferr, berr, work, rwork, info)
  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

  ! Test 8: lower unit diagonal, conjugate-transpose
  n = 3; nrhs = 1; lda = 3; ldb = 3; ldx = 3
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0); a(2) = (2.0d0, 1.0d0); a(3) = (3.0d0, 2.0d0)
  a(4) = (0.0d0, 0.0d0); a(5) = (1.0d0, 0.0d0); a(6) = (5.0d0, 1.0d0)
  a(7) = (0.0d0, 0.0d0); a(8) = (0.0d0, 0.0d0); a(9) = (1.0d0, 0.0d0)
  ! L = [1 0 0; 2+i 1 0; 3+2i 5+i 1]
  ! L^H = [1 2-i 3-2i; 0 1 5-i; 0 0 1]
  ! x=[1;1;1], b = L^H * x
  ! b_1 = 1 + (2-i) + (3-2i) = (6-3i)
  ! b_2 = 1 + (5-i) = (6-i)
  ! b_3 = 1
  b(1) = (6.0d0, -3.0d0); b(2) = (6.0d0, -1.0d0); b(3) = (1.0d0, 0.0d0)
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call ztrsv('L', 'C', 'U', n, a, lda, x, 1)
  call ztrrfs('L', 'C', 'U', n, nrhs, a, lda, b, ldb, x, ldx, &
              ferr, berr, work, rwork, info)
  call begin_test('lower_unit_conj_trans')
  call print_array('x', x_r, 2*n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 9: upper triangular, conjugate-transpose, non-unit
  n = 3; nrhs = 1; lda = 3; ldb = 3; ldx = 3
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  a(1) = (2.0d0, 1.0d0); a(2) = (0.0d0, 0.0d0); a(3) = (0.0d0, 0.0d0)
  a(4) = (1.0d0, 2.0d0); a(5) = (4.0d0, 1.0d0); a(6) = (0.0d0, 0.0d0)
  a(7) = (3.0d0, 1.0d0); a(8) = (5.0d0, 2.0d0); a(9) = (6.0d0, 1.0d0)
  ! A^H = [2-i 0 0; 1-2i 4-i 0; 3-i 5-2i 6-i]
  ! x=[1;1;1], b = A^H * x
  ! b_1 = (2-i) = (2-i)
  ! b_2 = (1-2i) + (4-i) = (5-3i)
  ! b_3 = (3-i) + (5-2i) + (6-i) = (14-4i)
  b(1) = (2.0d0, -1.0d0); b(2) = (5.0d0, -3.0d0); b(3) = (14.0d0, -4.0d0)
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call ztrsv('U', 'C', 'N', n, a, lda, x, 1)
  call ztrrfs('U', 'C', 'N', n, nrhs, a, lda, b, ldb, x, ldx, &
              ferr, berr, work, rwork, info)
  call begin_test('upper_conj_trans')
  call print_array('x', x_r, 2*n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 10: upper triangular, conjugate-transpose, unit diagonal
  n = 3; nrhs = 1; lda = 3; ldb = 3; ldx = 3
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0); a(2) = (0.0d0, 0.0d0); a(3) = (0.0d0, 0.0d0)
  a(4) = (2.0d0, 1.0d0); a(5) = (1.0d0, 0.0d0); a(6) = (0.0d0, 0.0d0)
  a(7) = (3.0d0, 2.0d0); a(8) = (4.0d0, 1.0d0); a(9) = (1.0d0, 0.0d0)
  ! A^H (unit) = [1 0 0; 2-i 1 0; 3-2i 4-i 1]
  ! x=[1;1;1], b = A^H * x
  ! b_1 = 1
  ! b_2 = (2-i) + 1 = (3-i)
  ! b_3 = (3-2i) + (4-i) + 1 = (8-3i)
  b(1) = (1.0d0, 0.0d0); b(2) = (3.0d0, -1.0d0); b(3) = (8.0d0, -3.0d0)
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call ztrsv('U', 'C', 'U', n, a, lda, x, 1)
  call ztrrfs('U', 'C', 'U', n, nrhs, a, lda, b, ldb, x, ldx, &
              ferr, berr, work, rwork, info)
  call begin_test('upper_unit_conj_trans')
  call print_array('x', x_r, 2*n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 11: lower triangular, no transpose, unit diagonal
  n = 3; nrhs = 1; lda = 3; ldb = 3; ldx = 3
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0); a(2) = (2.0d0, 1.0d0); a(3) = (3.0d0, 2.0d0)
  a(4) = (0.0d0, 0.0d0); a(5) = (1.0d0, 0.0d0); a(6) = (5.0d0, 1.0d0)
  a(7) = (0.0d0, 0.0d0); a(8) = (0.0d0, 0.0d0); a(9) = (1.0d0, 0.0d0)
  ! L = [1 0 0; 2+i 1 0; 3+2i 5+i 1]
  ! x=[1;1;1], b = L*x
  ! b_1 = 1
  ! b_2 = (2+i) + 1 = (3+i)
  ! b_3 = (3+2i) + (5+i) + 1 = (9+3i)
  b(1) = (1.0d0, 0.0d0); b(2) = (3.0d0, 1.0d0); b(3) = (9.0d0, 3.0d0)
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call ztrsv('L', 'N', 'U', n, a, lda, x, 1)
  call ztrrfs('L', 'N', 'U', n, nrhs, a, lda, b, ldb, x, ldx, &
              ferr, berr, work, rwork, info)
  call begin_test('lower_unit_no_trans')
  call print_array('x', x_r, 2*n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

end program
