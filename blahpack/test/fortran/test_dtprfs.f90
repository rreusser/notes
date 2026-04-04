program test_dtprfs
  use test_utils
  implicit none
  double precision :: ap(100), b(100), x(100), ferr(10), berr(10), work(300)
  integer :: iwork(100), info, n, nrhs, ldb, ldx

  ! Test 1: 3x3 upper triangular packed, no transpose, non-unit
  ! A = [2 1 3; 0 4 5; 0 0 6] packed (col-major upper) = [2, 1, 4, 3, 5, 6]
  ! b = A * [1; 2; 3] = [2+2+9; 8+15; 18] = [13; 23; 18]
  n = 3; nrhs = 1; ldb = 3; ldx = 3
  ap = 0.0d0; b = 0.0d0; x = 0.0d0
  ap(1) = 2.0d0; ap(2) = 1.0d0; ap(3) = 4.0d0
  ap(4) = 3.0d0; ap(5) = 5.0d0; ap(6) = 6.0d0
  b(1) = 13.0d0; b(2) = 23.0d0; b(3) = 18.0d0
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call dtpsv('U', 'N', 'N', n, ap, x, 1)
  call dtprfs('U', 'N', 'N', n, nrhs, ap, b, ldb, x, ldx, &
              ferr, berr, work, iwork, info)
  call begin_test('upper_no_trans')
  call print_array('x', x, n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 2: 3x3 lower triangular packed, transpose, non-unit
  ! L = [2 0 0; 1 4 0; 3 5 6] packed (col-major lower) = [2, 1, 3, 4, 5, 6]
  ! L^T = [2 1 3; 0 4 5; 0 0 6]
  ! b = L^T * [1;2;3] = [2+2+9; 8+15; 18] = [13; 23; 18]
  n = 3; nrhs = 1; ldb = 3; ldx = 3
  ap = 0.0d0; b = 0.0d0; x = 0.0d0
  ap(1) = 2.0d0; ap(2) = 1.0d0; ap(3) = 3.0d0
  ap(4) = 4.0d0; ap(5) = 5.0d0; ap(6) = 6.0d0
  b(1) = 13.0d0; b(2) = 23.0d0; b(3) = 18.0d0
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call dtpsv('L', 'T', 'N', n, ap, x, 1)
  call dtprfs('L', 'T', 'N', n, nrhs, ap, b, ldb, x, ldx, &
              ferr, berr, work, iwork, info)
  call begin_test('lower_trans')
  call print_array('x', x, n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 3: 3x3 upper triangular packed, unit diagonal, no transpose
  ! A = [1 2 3; 0 1 4; 0 0 1] packed = [1, 2, 1, 3, 4, 1]
  ! b = A * [1; 2; 3] = [1+4+9; 2+12; 3] = [14; 14; 3]
  n = 3; nrhs = 1; ldb = 3; ldx = 3
  ap = 0.0d0; b = 0.0d0; x = 0.0d0
  ap(1) = 1.0d0; ap(2) = 2.0d0; ap(3) = 1.0d0
  ap(4) = 3.0d0; ap(5) = 4.0d0; ap(6) = 1.0d0
  b(1) = 14.0d0; b(2) = 14.0d0; b(3) = 3.0d0
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call dtpsv('U', 'N', 'U', n, ap, x, 1)
  call dtprfs('U', 'N', 'U', n, nrhs, ap, b, ldb, x, ldx, &
              ferr, berr, work, iwork, info)
  call begin_test('upper_unit_no_trans')
  call print_array('x', x, n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 4: 3x3 lower triangular packed, no transpose, non-unit
  ! L = [3 0 0; 2 5 0; 1 4 7] packed = [3, 2, 1, 5, 4, 7]
  ! b = L * [1; 2; 3] = [3; 12; 30]
  n = 3; nrhs = 1; ldb = 3; ldx = 3
  ap = 0.0d0; b = 0.0d0; x = 0.0d0
  ap(1) = 3.0d0; ap(2) = 2.0d0; ap(3) = 1.0d0
  ap(4) = 5.0d0; ap(5) = 4.0d0; ap(6) = 7.0d0
  b(1) = 3.0d0; b(2) = 12.0d0; b(3) = 30.0d0
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call dtpsv('L', 'N', 'N', n, ap, x, 1)
  call dtprfs('L', 'N', 'N', n, nrhs, ap, b, ldb, x, ldx, &
              ferr, berr, work, iwork, info)
  call begin_test('lower_no_trans')
  call print_array('x', x, n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 5: Multiple RHS (NRHS=2), upper, no transpose
  ! A = [2 1 3; 0 4 5; 0 0 6] packed = [2, 1, 4, 3, 5, 6]
  ! RHS 1: b1 = A*[1;2;3] = [13;23;18]
  ! RHS 2: b2 = A*[4;5;6] = [8+5+18;20+30;36] = [31;50;36]
  n = 3; nrhs = 2; ldb = 3; ldx = 3
  ap = 0.0d0; b = 0.0d0; x = 0.0d0
  ap(1) = 2.0d0; ap(2) = 1.0d0; ap(3) = 4.0d0
  ap(4) = 3.0d0; ap(5) = 5.0d0; ap(6) = 6.0d0
  b(1) = 13.0d0; b(2) = 23.0d0; b(3) = 18.0d0
  b(4) = 31.0d0; b(5) = 50.0d0; b(6) = 36.0d0
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  x(4) = b(4); x(5) = b(5); x(6) = b(6)
  call dtpsv('U', 'N', 'N', n, ap, x(1), 1)
  call dtpsv('U', 'N', 'N', n, ap, x(4), 1)
  call dtprfs('U', 'N', 'N', n, nrhs, ap, b, ldb, x, ldx, &
              ferr, berr, work, iwork, info)
  call begin_test('multi_rhs')
  call print_array('x', x, n*nrhs)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 6: N=0 quick return
  call dtprfs('U', 'N', 'N', 0, 1, ap, b, 1, x, 1, &
              ferr, berr, work, iwork, info)
  call begin_test('n_zero')
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_int('info', info)
  call end_test()

  ! Test 7: N=1 edge case
  n = 1; nrhs = 1; ldb = 1; ldx = 1
  ap(1) = 5.0d0
  b(1) = 15.0d0
  x(1) = b(1)
  call dtpsv('U', 'N', 'N', n, ap, x, 1)
  call dtprfs('U', 'N', 'N', n, nrhs, ap, b, ldb, x, ldx, &
              ferr, berr, work, iwork, info)
  call begin_test('n_one')
  call print_array('x', x, n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 8: lower unit diagonal, transpose
  ! L = [1 0 0; 2 1 0; 3 5 1] packed = [1, 2, 3, 1, 5, 1]
  ! L^T = [1 2 3; 0 1 5; 0 0 1]
  ! b = L^T * [1;2;3] = [1+4+9; 2+15; 3] = [14; 17; 3]
  n = 3; nrhs = 1; ldb = 3; ldx = 3
  ap = 0.0d0; b = 0.0d0; x = 0.0d0
  ap(1) = 1.0d0; ap(2) = 2.0d0; ap(3) = 3.0d0
  ap(4) = 1.0d0; ap(5) = 5.0d0; ap(6) = 1.0d0
  b(1) = 14.0d0; b(2) = 17.0d0; b(3) = 3.0d0
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call dtpsv('L', 'T', 'U', n, ap, x, 1)
  call dtprfs('L', 'T', 'U', n, nrhs, ap, b, ldb, x, ldx, &
              ferr, berr, work, iwork, info)
  call begin_test('lower_unit_trans')
  call print_array('x', x, n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 9: upper triangular packed, transpose, non-unit
  ! A = [2 1 3; 0 4 5; 0 0 6] packed = [2, 1, 4, 3, 5, 6]
  ! A^T = [2 0 0; 1 4 0; 3 5 6]
  ! b = A^T * [1;2;3] = [2; 1+8; 3+10+18] = [2; 9; 31]
  n = 3; nrhs = 1; ldb = 3; ldx = 3
  ap = 0.0d0; b = 0.0d0; x = 0.0d0
  ap(1) = 2.0d0; ap(2) = 1.0d0; ap(3) = 4.0d0
  ap(4) = 3.0d0; ap(5) = 5.0d0; ap(6) = 6.0d0
  b(1) = 2.0d0; b(2) = 9.0d0; b(3) = 31.0d0
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call dtpsv('U', 'T', 'N', n, ap, x, 1)
  call dtprfs('U', 'T', 'N', n, nrhs, ap, b, ldb, x, ldx, &
              ferr, berr, work, iwork, info)
  call begin_test('upper_trans')
  call print_array('x', x, n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 10: upper triangular packed, transpose, unit diagonal
  ! A = [1 2 3; 0 1 4; 0 0 1] packed = [1, 2, 1, 3, 4, 1]
  ! A^T = [1 0 0; 2 1 0; 3 4 1]
  ! b = A^T * [1;2;3] = [1; 2+2; 3+8+3] = [1; 4; 14]
  n = 3; nrhs = 1; ldb = 3; ldx = 3
  ap = 0.0d0; b = 0.0d0; x = 0.0d0
  ap(1) = 1.0d0; ap(2) = 2.0d0; ap(3) = 1.0d0
  ap(4) = 3.0d0; ap(5) = 4.0d0; ap(6) = 1.0d0
  b(1) = 1.0d0; b(2) = 4.0d0; b(3) = 14.0d0
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call dtpsv('U', 'T', 'U', n, ap, x, 1)
  call dtprfs('U', 'T', 'U', n, nrhs, ap, b, ldb, x, ldx, &
              ferr, berr, work, iwork, info)
  call begin_test('upper_unit_trans')
  call print_array('x', x, n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 11: lower triangular packed, no transpose, unit diagonal
  ! L = [1 0 0; 2 1 0; 3 5 1] packed = [1, 2, 3, 1, 5, 1]
  ! b = L * [1;2;3] = [1; 2+2; 3+10+3] = [1; 4; 16]
  n = 3; nrhs = 1; ldb = 3; ldx = 3
  ap = 0.0d0; b = 0.0d0; x = 0.0d0
  ap(1) = 1.0d0; ap(2) = 2.0d0; ap(3) = 3.0d0
  ap(4) = 1.0d0; ap(5) = 5.0d0; ap(6) = 1.0d0
  b(1) = 1.0d0; b(2) = 4.0d0; b(3) = 16.0d0
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call dtpsv('L', 'N', 'U', n, ap, x, 1)
  call dtprfs('L', 'N', 'U', n, nrhs, ap, b, ldb, x, ldx, &
              ferr, berr, work, iwork, info)
  call begin_test('lower_unit_no_trans')
  call print_array('x', x, n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

end program
