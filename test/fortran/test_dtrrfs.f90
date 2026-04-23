program test_dtrrfs
  use test_utils
  implicit none
  double precision :: a(100), b(100), x(100), ferr(10), berr(10), work(300)
  integer :: iwork(100), info, n, nrhs, lda, ldb, ldx

  ! Test 1: 3x3 upper triangular, no transpose, single RHS
  ! A = [2 1 3; 0 4 5; 0 0 6]
  n = 3; nrhs = 1; lda = 3; ldb = 3; ldx = 3
  a = 0.0d0; b = 0.0d0; x = 0.0d0
  a(1) = 2.0d0; a(2) = 0.0d0; a(3) = 0.0d0
  a(4) = 1.0d0; a(5) = 4.0d0; a(6) = 0.0d0
  a(7) = 3.0d0; a(8) = 5.0d0; a(9) = 6.0d0
  ! b = A * [1; 2; 3] = [2+2+9; 0+8+15; 0+0+18] = [13; 23; 18]
  b(1) = 13.0d0; b(2) = 23.0d0; b(3) = 18.0d0
  ! Solve A*x = b => x = [1; 2; 3]
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call dtrsv('U', 'N', 'N', n, a, lda, x, 1)
  call dtrrfs('U', 'N', 'N', n, nrhs, a, lda, b, ldb, x, ldx, &
              ferr, berr, work, iwork, info)
  call begin_test('upper_no_trans')
  call print_array('x', x, n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 2: 3x3 lower triangular, transpose, single RHS
  ! L = [2 0 0; 1 4 0; 3 5 6], solve L^T * x = b
  n = 3; nrhs = 1; lda = 3; ldb = 3; ldx = 3
  a = 0.0d0; b = 0.0d0; x = 0.0d0
  a(1) = 2.0d0; a(2) = 1.0d0; a(3) = 3.0d0
  a(4) = 0.0d0; a(5) = 4.0d0; a(6) = 5.0d0
  a(7) = 0.0d0; a(8) = 0.0d0; a(9) = 6.0d0
  ! b = L^T * [1; 2; 3] = [2+1+3; 0+8+15; 0+0+18] ... wait, L^T is:
  ! L^T = [2 1 3; 0 4 5; 0 0 6]
  ! L^T * [1;2;3] = [2+2+9; 8+15; 18] = [13; 23; 18]
  b(1) = 13.0d0; b(2) = 23.0d0; b(3) = 18.0d0
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call dtrsv('L', 'T', 'N', n, a, lda, x, 1)
  call dtrrfs('L', 'T', 'N', n, nrhs, a, lda, b, ldb, x, ldx, &
              ferr, berr, work, iwork, info)
  call begin_test('lower_trans')
  call print_array('x', x, n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 3: 3x3 upper triangular, unit diagonal, no transpose
  ! A = [1 2 3; 0 1 4; 0 0 1] (unit diag)
  n = 3; nrhs = 1; lda = 3; ldb = 3; ldx = 3
  a = 0.0d0; b = 0.0d0; x = 0.0d0
  a(1) = 1.0d0; a(2) = 0.0d0; a(3) = 0.0d0
  a(4) = 2.0d0; a(5) = 1.0d0; a(6) = 0.0d0
  a(7) = 3.0d0; a(8) = 4.0d0; a(9) = 1.0d0
  ! b = A * [1; 2; 3] = [1+4+9; 2+12; 3] = [14; 14; 3]
  b(1) = 14.0d0; b(2) = 14.0d0; b(3) = 3.0d0
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call dtrsv('U', 'N', 'U', n, a, lda, x, 1)
  call dtrrfs('U', 'N', 'U', n, nrhs, a, lda, b, ldb, x, ldx, &
              ferr, berr, work, iwork, info)
  call begin_test('upper_unit_diag')
  call print_array('x', x, n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 4: 3x3 lower triangular, no transpose, single RHS
  ! L = [3 0 0; 2 5 0; 1 4 7]
  n = 3; nrhs = 1; lda = 3; ldb = 3; ldx = 3
  a = 0.0d0; b = 0.0d0; x = 0.0d0
  a(1) = 3.0d0; a(2) = 2.0d0; a(3) = 1.0d0
  a(4) = 0.0d0; a(5) = 5.0d0; a(6) = 4.0d0
  a(7) = 0.0d0; a(8) = 0.0d0; a(9) = 7.0d0
  ! b = L * [1; 2; 3] = [3; 12; 30]
  b(1) = 3.0d0; b(2) = 12.0d0; b(3) = 30.0d0
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call dtrsv('L', 'N', 'N', n, a, lda, x, 1)
  call dtrrfs('L', 'N', 'N', n, nrhs, a, lda, b, ldb, x, ldx, &
              ferr, berr, work, iwork, info)
  call begin_test('lower_no_trans')
  call print_array('x', x, n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 5: Multiple RHS (NRHS=2), upper, no transpose
  n = 3; nrhs = 2; lda = 3; ldb = 3; ldx = 3
  a = 0.0d0; b = 0.0d0; x = 0.0d0
  a(1) = 2.0d0; a(2) = 0.0d0; a(3) = 0.0d0
  a(4) = 1.0d0; a(5) = 4.0d0; a(6) = 0.0d0
  a(7) = 3.0d0; a(8) = 5.0d0; a(9) = 6.0d0
  ! RHS 1: b1 = A*[1;2;3] = [13;23;18]
  b(1) = 13.0d0; b(2) = 23.0d0; b(3) = 18.0d0
  ! RHS 2: b2 = A*[4;5;6] = [8+5+18;20+30;36] = [26;50;36]
  b(4) = 26.0d0; b(5) = 50.0d0; b(6) = 36.0d0
  ! Solve each column
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  x(4) = b(4); x(5) = b(5); x(6) = b(6)
  call dtrsv('U', 'N', 'N', n, a, lda, x(1), 1)
  call dtrsv('U', 'N', 'N', n, a, lda, x(4), 1)
  call dtrrfs('U', 'N', 'N', n, nrhs, a, lda, b, ldb, x, ldx, &
              ferr, berr, work, iwork, info)
  call begin_test('multi_rhs')
  call print_array('x', x, n*nrhs)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 6: N=0 quick return
  call dtrrfs('U', 'N', 'N', 0, 1, a, 1, b, 1, x, 1, &
              ferr, berr, work, iwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 7: NRHS=0 quick return
  call dtrrfs('U', 'N', 'N', 3, 0, a, 3, b, 3, x, 3, &
              ferr, berr, work, iwork, info)
  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

  ! Test 8: lower unit diagonal, transpose
  n = 3; nrhs = 1; lda = 3; ldb = 3; ldx = 3
  a = 0.0d0; b = 0.0d0; x = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0
  a(4) = 0.0d0; a(5) = 1.0d0; a(6) = 5.0d0
  a(7) = 0.0d0; a(8) = 0.0d0; a(9) = 1.0d0
  ! L = [1 0 0; 2 1 0; 3 5 1], L^T = [1 2 3; 0 1 5; 0 0 1]
  ! b = L^T * [1;2;3] = [1+4+9; 2+15; 3] = [14; 17; 3]
  b(1) = 14.0d0; b(2) = 17.0d0; b(3) = 3.0d0
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call dtrsv('L', 'T', 'U', n, a, lda, x, 1)
  call dtrrfs('L', 'T', 'U', n, nrhs, a, lda, b, ldb, x, ldx, &
              ferr, berr, work, iwork, info)
  call begin_test('lower_unit_trans')
  call print_array('x', x, n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 9: upper triangular, transpose, non-unit
  n = 3; nrhs = 1; lda = 3; ldb = 3; ldx = 3
  a = 0.0d0; b = 0.0d0; x = 0.0d0
  a(1) = 2.0d0; a(2) = 0.0d0; a(3) = 0.0d0
  a(4) = 1.0d0; a(5) = 4.0d0; a(6) = 0.0d0
  a(7) = 3.0d0; a(8) = 5.0d0; a(9) = 6.0d0
  ! A^T = [2 0 0; 1 4 0; 3 5 6]
  ! b = A^T * [1;2;3] = [2; 1+8; 3+10+18] = [2; 9; 31]
  b(1) = 2.0d0; b(2) = 9.0d0; b(3) = 31.0d0
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call dtrsv('U', 'T', 'N', n, a, lda, x, 1)
  call dtrrfs('U', 'T', 'N', n, nrhs, a, lda, b, ldb, x, ldx, &
              ferr, berr, work, iwork, info)
  call begin_test('upper_trans')
  call print_array('x', x, n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 10: upper triangular, transpose, unit diagonal
  n = 3; nrhs = 1; lda = 3; ldb = 3; ldx = 3
  a = 0.0d0; b = 0.0d0; x = 0.0d0
  a(1) = 1.0d0; a(2) = 0.0d0; a(3) = 0.0d0
  a(4) = 2.0d0; a(5) = 1.0d0; a(6) = 0.0d0
  a(7) = 3.0d0; a(8) = 4.0d0; a(9) = 1.0d0
  ! A = [1 2 3; 0 1 4; 0 0 1], A^T = [1 0 0; 2 1 0; 3 4 1]
  ! b = A^T * [1;2;3] = [1; 2+2; 3+8+3] = [1; 4; 14]
  b(1) = 1.0d0; b(2) = 4.0d0; b(3) = 14.0d0
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call dtrsv('U', 'T', 'U', n, a, lda, x, 1)
  call dtrrfs('U', 'T', 'U', n, nrhs, a, lda, b, ldb, x, ldx, &
              ferr, berr, work, iwork, info)
  call begin_test('upper_unit_trans')
  call print_array('x', x, n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 11: lower triangular, no transpose, unit diagonal
  n = 3; nrhs = 1; lda = 3; ldb = 3; ldx = 3
  a = 0.0d0; b = 0.0d0; x = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0
  a(4) = 0.0d0; a(5) = 1.0d0; a(6) = 5.0d0
  a(7) = 0.0d0; a(8) = 0.0d0; a(9) = 1.0d0
  ! L = [1 0 0; 2 1 0; 3 5 1]
  ! b = L * [1;2;3] = [1; 2+2; 3+10+3] = [1; 4; 16]
  b(1) = 1.0d0; b(2) = 4.0d0; b(3) = 16.0d0
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call dtrsv('L', 'N', 'U', n, a, lda, x, 1)
  call dtrrfs('L', 'N', 'U', n, nrhs, a, lda, b, ldb, x, ldx, &
              ferr, berr, work, iwork, info)
  call begin_test('lower_unit_no_trans')
  call print_array('x', x, n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

end program
