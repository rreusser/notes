program test_ztprfs
  use test_utils
  implicit none
  complex*16 :: ap(100), b(100), x(100), work(200)
  double precision :: ap_r(200), b_r(200), x_r(200)
  double precision :: ferr(10), berr(10), rwork(100)
  integer :: info, n, nrhs, ldb, ldx
  equivalence (ap, ap_r)
  equivalence (b, b_r)
  equivalence (x, x_r)

  ! Test 1: 3x3 upper triangular packed, no transpose, non-unit
  ! A = [2+i   1+0.5i  3+2i ]
  !     [0     4-i     5    ]
  !     [0     0       6-0.5i]
  ! packed col-major upper = [a11, a12, a22, a13, a23, a33]
  n = 3; nrhs = 1; ldb = 3; ldx = 3
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  ap(1) = (2.0d0, 1.0d0)
  ap(2) = (1.0d0, 0.5d0)
  ap(3) = (4.0d0, -1.0d0)
  ap(4) = (3.0d0, 2.0d0)
  ap(5) = (5.0d0, 0.0d0)
  ap(6) = (6.0d0, -0.5d0)
  ! x_true = [1+i, 2-i, 3+0.5i]
  ! b = A * x_true
  x(1) = (1.0d0, 1.0d0)
  x(2) = (2.0d0, -1.0d0)
  x(3) = (3.0d0, 0.5d0)
  ! Compute b = A*x using ztpmv on a copy
  b(1) = x(1); b(2) = x(2); b(3) = x(3)
  call ztpmv('U', 'N', 'N', n, ap, b, 1)
  ! Now solve to get x back
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call ztpsv('U', 'N', 'N', n, ap, x, 1)
  call ztprfs('U', 'N', 'N', n, nrhs, ap, b, ldb, x, ldx, &
              ferr, berr, work, rwork, info)
  call begin_test('upper_no_trans')
  call print_array('x', x_r, 2*n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 2: 3x3 lower triangular packed, conjugate-transpose, non-unit
  ! L = [2+i   0      0     ]
  !     [1-0.5i 4+i   0     ]
  !     [3+2i   5-i   6+0.5i]
  ! packed col-major lower = [a11, a21, a31, a22, a32, a33]
  n = 3; nrhs = 1; ldb = 3; ldx = 3
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  ap(1) = (2.0d0, 1.0d0)
  ap(2) = (1.0d0, -0.5d0)
  ap(3) = (3.0d0, 2.0d0)
  ap(4) = (4.0d0, 1.0d0)
  ap(5) = (5.0d0, -1.0d0)
  ap(6) = (6.0d0, 0.5d0)
  x(1) = (1.0d0, 0.5d0)
  x(2) = (2.0d0, -1.0d0)
  x(3) = (3.0d0, 1.0d0)
  b(1) = x(1); b(2) = x(2); b(3) = x(3)
  call ztpmv('L', 'C', 'N', n, ap, b, 1)
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call ztpsv('L', 'C', 'N', n, ap, x, 1)
  call ztprfs('L', 'C', 'N', n, nrhs, ap, b, ldb, x, ldx, &
              ferr, berr, work, rwork, info)
  call begin_test('lower_conj_trans')
  call print_array('x', x_r, 2*n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 3: 3x3 upper triangular, unit diagonal, no transpose
  ! A = [1     2+i    3-0.5i]
  !     [0     1      4+i   ]
  !     [0     0      1     ]
  n = 3; nrhs = 1; ldb = 3; ldx = 3
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  ap(1) = (1.0d0, 0.0d0)
  ap(2) = (2.0d0, 1.0d0)
  ap(3) = (1.0d0, 0.0d0)
  ap(4) = (3.0d0, -0.5d0)
  ap(5) = (4.0d0, 1.0d0)
  ap(6) = (1.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  b(1) = x(1); b(2) = x(2); b(3) = x(3)
  call ztpmv('U', 'N', 'U', n, ap, b, 1)
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call ztpsv('U', 'N', 'U', n, ap, x, 1)
  call ztprfs('U', 'N', 'U', n, nrhs, ap, b, ldb, x, ldx, &
              ferr, berr, work, rwork, info)
  call begin_test('upper_unit_no_trans')
  call print_array('x', x_r, 2*n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 4: 3x3 lower triangular, no transpose, non-unit
  n = 3; nrhs = 1; ldb = 3; ldx = 3
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  ap(1) = (3.0d0, 1.0d0)
  ap(2) = (2.0d0, -0.5d0)
  ap(3) = (1.0d0, 2.0d0)
  ap(4) = (5.0d0, 0.0d0)
  ap(5) = (4.0d0, -1.0d0)
  ap(6) = (7.0d0, 0.5d0)
  x(1) = (1.0d0, 1.0d0)
  x(2) = (2.0d0, -1.0d0)
  x(3) = (3.0d0, 0.5d0)
  b(1) = x(1); b(2) = x(2); b(3) = x(3)
  call ztpmv('L', 'N', 'N', n, ap, b, 1)
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call ztpsv('L', 'N', 'N', n, ap, x, 1)
  call ztprfs('L', 'N', 'N', n, nrhs, ap, b, ldb, x, ldx, &
              ferr, berr, work, rwork, info)
  call begin_test('lower_no_trans')
  call print_array('x', x_r, 2*n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 5: Multiple RHS (NRHS=2), upper, no transpose, non-unit
  n = 3; nrhs = 2; ldb = 3; ldx = 3
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  ap(1) = (2.0d0, 1.0d0)
  ap(2) = (1.0d0, 0.5d0)
  ap(3) = (4.0d0, -1.0d0)
  ap(4) = (3.0d0, 2.0d0)
  ap(5) = (5.0d0, 0.0d0)
  ap(6) = (6.0d0, -0.5d0)
  ! RHS 1
  x(1) = (1.0d0, 1.0d0)
  x(2) = (2.0d0, -1.0d0)
  x(3) = (3.0d0, 0.5d0)
  ! RHS 2
  x(4) = (4.0d0, -0.5d0)
  x(5) = (5.0d0, 1.0d0)
  x(6) = (6.0d0, -1.0d0)
  b(1) = x(1); b(2) = x(2); b(3) = x(3)
  b(4) = x(4); b(5) = x(5); b(6) = x(6)
  call ztpmv('U', 'N', 'N', n, ap, b(1), 1)
  call ztpmv('U', 'N', 'N', n, ap, b(4), 1)
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  x(4) = b(4); x(5) = b(5); x(6) = b(6)
  call ztpsv('U', 'N', 'N', n, ap, x(1), 1)
  call ztpsv('U', 'N', 'N', n, ap, x(4), 1)
  call ztprfs('U', 'N', 'N', n, nrhs, ap, b, ldb, x, ldx, &
              ferr, berr, work, rwork, info)
  call begin_test('multi_rhs')
  call print_array('x', x_r, 2*n*nrhs)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 6: N=0 quick return
  ferr(1) = 999.0d0; berr(1) = 999.0d0
  call ztprfs('U', 'N', 'N', 0, 1, ap, b, 1, x, 1, &
              ferr, berr, work, rwork, info)
  call begin_test('n_zero')
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_int('info', info)
  call end_test()

  ! Test 7: N=1 edge case
  n = 1; nrhs = 1; ldb = 1; ldx = 1
  ap(1) = (5.0d0, 2.0d0)
  x(1) = (3.0d0, -1.0d0)
  b(1) = x(1)
  call ztpmv('U', 'N', 'N', n, ap, b, 1)
  x(1) = b(1)
  call ztpsv('U', 'N', 'N', n, ap, x, 1)
  call ztprfs('U', 'N', 'N', n, nrhs, ap, b, ldb, x, ldx, &
              ferr, berr, work, rwork, info)
  call begin_test('n_one')
  call print_array('x', x_r, 2*n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 8: lower unit diagonal, conjugate-transpose
  n = 3; nrhs = 1; ldb = 3; ldx = 3
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  ap(1) = (1.0d0, 0.0d0)
  ap(2) = (2.0d0, 1.0d0)
  ap(3) = (3.0d0, -0.5d0)
  ap(4) = (1.0d0, 0.0d0)
  ap(5) = (5.0d0, 0.5d0)
  ap(6) = (1.0d0, 0.0d0)
  x(1) = (1.0d0, 0.5d0)
  x(2) = (2.0d0, -1.0d0)
  x(3) = (3.0d0, 1.0d0)
  b(1) = x(1); b(2) = x(2); b(3) = x(3)
  call ztpmv('L', 'C', 'U', n, ap, b, 1)
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call ztpsv('L', 'C', 'U', n, ap, x, 1)
  call ztprfs('L', 'C', 'U', n, nrhs, ap, b, ldb, x, ldx, &
              ferr, berr, work, rwork, info)
  call begin_test('lower_unit_conj_trans')
  call print_array('x', x_r, 2*n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 9: upper triangular, conjugate-transpose, non-unit
  n = 3; nrhs = 1; ldb = 3; ldx = 3
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  ap(1) = (2.0d0, 1.0d0)
  ap(2) = (1.0d0, 0.5d0)
  ap(3) = (4.0d0, -1.0d0)
  ap(4) = (3.0d0, 2.0d0)
  ap(5) = (5.0d0, 0.0d0)
  ap(6) = (6.0d0, -0.5d0)
  x(1) = (1.0d0, 1.0d0)
  x(2) = (2.0d0, -1.0d0)
  x(3) = (3.0d0, 0.5d0)
  b(1) = x(1); b(2) = x(2); b(3) = x(3)
  call ztpmv('U', 'C', 'N', n, ap, b, 1)
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call ztpsv('U', 'C', 'N', n, ap, x, 1)
  call ztprfs('U', 'C', 'N', n, nrhs, ap, b, ldb, x, ldx, &
              ferr, berr, work, rwork, info)
  call begin_test('upper_conj_trans')
  call print_array('x', x_r, 2*n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 10: upper triangular, conjugate-transpose, unit diagonal
  n = 3; nrhs = 1; ldb = 3; ldx = 3
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  ap(1) = (1.0d0, 0.0d0)
  ap(2) = (2.0d0, 1.0d0)
  ap(3) = (1.0d0, 0.0d0)
  ap(4) = (3.0d0, -0.5d0)
  ap(5) = (4.0d0, 1.0d0)
  ap(6) = (1.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  b(1) = x(1); b(2) = x(2); b(3) = x(3)
  call ztpmv('U', 'C', 'U', n, ap, b, 1)
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call ztpsv('U', 'C', 'U', n, ap, x, 1)
  call ztprfs('U', 'C', 'U', n, nrhs, ap, b, ldb, x, ldx, &
              ferr, berr, work, rwork, info)
  call begin_test('upper_unit_conj_trans')
  call print_array('x', x_r, 2*n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 11: lower triangular, no transpose, unit diagonal
  n = 3; nrhs = 1; ldb = 3; ldx = 3
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  ap(1) = (1.0d0, 0.0d0)
  ap(2) = (2.0d0, 1.0d0)
  ap(3) = (3.0d0, -0.5d0)
  ap(4) = (1.0d0, 0.0d0)
  ap(5) = (5.0d0, 0.5d0)
  ap(6) = (1.0d0, 0.0d0)
  x(1) = (1.0d0, 0.5d0)
  x(2) = (2.0d0, -1.0d0)
  x(3) = (3.0d0, 1.0d0)
  b(1) = x(1); b(2) = x(2); b(3) = x(3)
  call ztpmv('L', 'N', 'U', n, ap, b, 1)
  x(1) = b(1); x(2) = b(2); x(3) = b(3)
  call ztpsv('L', 'N', 'U', n, ap, x, 1)
  call ztprfs('L', 'N', 'U', n, nrhs, ap, b, ldb, x, ldx, &
              ferr, berr, work, rwork, info)
  call begin_test('lower_unit_no_trans')
  call print_array('x', x_r, 2*n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

end program
