program test_zlatrs
  use test_utils
  implicit none
  integer, parameter :: NMAX = 4
  complex*16 :: A(NMAX, NMAX), x(NMAX)
  double precision :: x_r(2*NMAX), A_r(2*NMAX*NMAX)
  double precision :: scale, cnorm(NMAX)
  integer :: info, n
  equivalence (x, x_r)
  equivalence (A, A_r)

  ! Test 1: Upper triangular, no transpose, non-unit, 3x3
  ! A = [[2+i, 1+i, 0.5], [0, 3+0.5i, 1-i], [0, 0, 4-i]]
  n = 3
  A = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 1.0d0)
  A(1,2) = (1.0d0, 1.0d0)
  A(1,3) = (0.5d0, 0.0d0)
  A(2,2) = (3.0d0, 0.5d0)
  A(2,3) = (1.0d0, -1.0d0)
  A(3,3) = (4.0d0, -1.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatrs('U', 'N', 'N', 'N', n, A, NMAX, x, scale, cnorm, info)
  call begin_test('upper_N_nonunit')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

  ! Test 2: Lower triangular, no transpose, non-unit, 3x3
  A = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 1.0d0)
  A(2,1) = (1.0d0, 1.0d0)
  A(2,2) = (3.0d0, 0.5d0)
  A(3,1) = (0.5d0, 0.0d0)
  A(3,2) = (1.0d0, -1.0d0)
  A(3,3) = (4.0d0, -1.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatrs('L', 'N', 'N', 'N', n, A, NMAX, x, scale, cnorm, info)
  call begin_test('lower_N_nonunit')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

  ! Test 3: Upper triangular, transpose, non-unit, 3x3
  A = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 1.0d0)
  A(1,2) = (1.0d0, 1.0d0)
  A(1,3) = (0.5d0, 0.0d0)
  A(2,2) = (3.0d0, 0.5d0)
  A(2,3) = (1.0d0, -1.0d0)
  A(3,3) = (4.0d0, -1.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatrs('U', 'T', 'N', 'N', n, A, NMAX, x, scale, cnorm, info)
  call begin_test('upper_T_nonunit')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 4: Upper triangular, conjugate transpose, non-unit, 3x3
  A = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 1.0d0)
  A(1,2) = (1.0d0, 1.0d0)
  A(1,3) = (0.5d0, 0.0d0)
  A(2,2) = (3.0d0, 0.5d0)
  A(2,3) = (1.0d0, -1.0d0)
  A(3,3) = (4.0d0, -1.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatrs('U', 'C', 'N', 'N', n, A, NMAX, x, scale, cnorm, info)
  call begin_test('upper_C_nonunit')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 5: Lower triangular, transpose, non-unit, 3x3
  A = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 1.0d0)
  A(2,1) = (1.0d0, 1.0d0)
  A(2,2) = (3.0d0, 0.5d0)
  A(3,1) = (0.5d0, 0.0d0)
  A(3,2) = (1.0d0, -1.0d0)
  A(3,3) = (4.0d0, -1.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatrs('L', 'T', 'N', 'N', n, A, NMAX, x, scale, cnorm, info)
  call begin_test('lower_T_nonunit')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 6: Lower triangular, conjugate transpose, non-unit, 3x3
  A = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 1.0d0)
  A(2,1) = (1.0d0, 1.0d0)
  A(2,2) = (3.0d0, 0.5d0)
  A(3,1) = (0.5d0, 0.0d0)
  A(3,2) = (1.0d0, -1.0d0)
  A(3,3) = (4.0d0, -1.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatrs('L', 'C', 'N', 'N', n, A, NMAX, x, scale, cnorm, info)
  call begin_test('lower_C_nonunit')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 7: Upper triangular, unit diagonal, no transpose
  A = (0.0d0, 0.0d0)
  A(1,1) = (99.0d0, 99.0d0)
  A(1,2) = (1.0d0, 1.0d0)
  A(1,3) = (0.5d0, 0.0d0)
  A(2,2) = (99.0d0, 99.0d0)
  A(2,3) = (1.0d0, -1.0d0)
  A(3,3) = (99.0d0, 99.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatrs('U', 'N', 'U', 'N', n, A, NMAX, x, scale, cnorm, info)
  call begin_test('upper_N_unit')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 8: Lower triangular, unit diagonal, no transpose
  A = (0.0d0, 0.0d0)
  A(1,1) = (99.0d0, 99.0d0)
  A(2,1) = (1.0d0, 1.0d0)
  A(2,2) = (99.0d0, 99.0d0)
  A(3,1) = (0.5d0, 0.0d0)
  A(3,2) = (1.0d0, -1.0d0)
  A(3,3) = (99.0d0, 99.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatrs('L', 'N', 'U', 'N', n, A, NMAX, x, scale, cnorm, info)
  call begin_test('lower_N_unit')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 9: N=0
  n = 0
  scale = 0.0d0
  call zlatrs('U', 'N', 'N', 'N', 0, A, NMAX, x, scale, cnorm, info)
  call begin_test('n_zero')
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 10: N=1
  n = 1
  A(1,1) = (5.0d0, 2.0d0)
  x(1) = (10.0d0, -3.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatrs('U', 'N', 'N', 'N', 1, A, NMAX, x, scale, cnorm, info)
  call begin_test('n_one')
  call print_array('x', x_r, 2)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 11: Upper, normin='Y' (pre-computed column norms)
  n = 3
  A = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 1.0d0)
  A(1,2) = (1.0d0, 1.0d0)
  A(1,3) = (0.5d0, 0.0d0)
  A(2,2) = (3.0d0, 0.5d0)
  A(2,3) = (1.0d0, -1.0d0)
  A(3,3) = (4.0d0, -1.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  ! Pre-compute column norms for upper: col j norm = sum |a(i,j)| for i<j
  cnorm(1) = 0.0d0
  cnorm(2) = abs(real(A(1,2))) + abs(aimag(A(1,2)))
  cnorm(3) = abs(real(A(1,3))) + abs(aimag(A(1,3))) + abs(real(A(2,3))) + abs(aimag(A(2,3)))
  scale = 0.0d0
  call zlatrs('U', 'N', 'N', 'Y', n, A, NMAX, x, scale, cnorm, info)
  call begin_test('upper_N_normin_Y')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 12: Upper, conjugate transpose, unit diagonal
  A = (0.0d0, 0.0d0)
  A(1,1) = (99.0d0, 99.0d0)
  A(1,2) = (1.0d0, 1.0d0)
  A(1,3) = (0.5d0, 0.0d0)
  A(2,2) = (99.0d0, 99.0d0)
  A(2,3) = (1.0d0, -1.0d0)
  A(3,3) = (99.0d0, 99.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatrs('U', 'C', 'U', 'N', n, A, NMAX, x, scale, cnorm, info)
  call begin_test('upper_C_unit')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 13: Lower, conjugate transpose, unit diagonal
  A = (0.0d0, 0.0d0)
  A(1,1) = (99.0d0, 99.0d0)
  A(2,1) = (1.0d0, 1.0d0)
  A(2,2) = (99.0d0, 99.0d0)
  A(3,1) = (0.5d0, 0.0d0)
  A(3,2) = (1.0d0, -1.0d0)
  A(3,3) = (99.0d0, 99.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatrs('L', 'C', 'U', 'N', n, A, NMAX, x, scale, cnorm, info)
  call begin_test('lower_C_unit')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 14: 4x4 upper, no transpose (tests more columns)
  n = 4
  A = (0.0d0, 0.0d0)
  A(1,1) = (3.0d0, 0.0d0)
  A(1,2) = (1.0d0, 0.5d0)
  A(1,3) = (0.0d0, 1.0d0)
  A(1,4) = (0.5d0, 0.0d0)
  A(2,2) = (4.0d0, 1.0d0)
  A(2,3) = (1.0d0, 0.0d0)
  A(2,4) = (0.0d0, 0.5d0)
  A(3,3) = (2.0d0, -1.0d0)
  A(3,4) = (1.0d0, 1.0d0)
  A(4,4) = (5.0d0, 0.0d0)
  x(1) = (1.0d0, 1.0d0)
  x(2) = (2.0d0, 0.0d0)
  x(3) = (0.0d0, 3.0d0)
  x(4) = (1.0d0, -2.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatrs('U', 'N', 'N', 'N', n, A, NMAX, x, scale, cnorm, info)
  call begin_test('upper_N_4x4')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 15: Lower, transpose, unit, normin='Y'
  n = 3
  A = (0.0d0, 0.0d0)
  A(1,1) = (99.0d0, 99.0d0)
  A(2,1) = (1.0d0, 1.0d0)
  A(2,2) = (99.0d0, 99.0d0)
  A(3,1) = (0.5d0, 0.0d0)
  A(3,2) = (1.0d0, -1.0d0)
  A(3,3) = (99.0d0, 99.0d0)
  x(1) = (5.0d0, 1.0d0)
  x(2) = (3.0d0, -2.0d0)
  x(3) = (1.0d0, 0.0d0)
  ! Norms for lower: col j norm = sum |a(i,j)| for i>j
  cnorm(1) = abs(real(A(2,1)))+abs(aimag(A(2,1)))+abs(real(A(3,1)))+abs(aimag(A(3,1)))
  cnorm(2) = abs(real(A(3,2)))+abs(aimag(A(3,2)))
  cnorm(3) = 0.0d0
  scale = 0.0d0
  call zlatrs('L', 'T', 'U', 'Y', n, A, NMAX, x, scale, cnorm, info)
  call begin_test('lower_T_unit_norminY')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 16: Careful solve - upper, no-transpose, non-unit
  ! Near-singular diagonal to force grow < SMLNUM => careful path
  n = 3
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d-300, 0.0d0)
  A(1,2) = (1.0d0, 1.0d0)
  A(1,3) = (0.5d0, 0.0d0)
  A(2,2) = (1.0d-300, 0.0d0)
  A(2,3) = (1.0d0, -1.0d0)
  A(3,3) = (1.0d-300, 0.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatrs('U', 'N', 'N', 'N', n, A, NMAX, x, scale, cnorm, info)
  call begin_test('upper_N_careful')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 17: Careful solve - lower, no-transpose, non-unit
  n = 3
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d-300, 0.0d0)
  A(2,1) = (1.0d0, 1.0d0)
  A(2,2) = (1.0d-300, 0.0d0)
  A(3,1) = (0.5d0, 0.0d0)
  A(3,2) = (1.0d0, -1.0d0)
  A(3,3) = (1.0d-300, 0.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatrs('L', 'N', 'N', 'N', n, A, NMAX, x, scale, cnorm, info)
  call begin_test('lower_N_careful')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 18: Careful solve - upper, transpose, non-unit
  n = 3
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d-300, 0.0d0)
  A(1,2) = (1.0d0, 1.0d0)
  A(1,3) = (0.5d0, 0.0d0)
  A(2,2) = (1.0d-300, 0.0d0)
  A(2,3) = (1.0d0, -1.0d0)
  A(3,3) = (1.0d-300, 0.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatrs('U', 'T', 'N', 'N', n, A, NMAX, x, scale, cnorm, info)
  call begin_test('upper_T_careful')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 19: Careful solve - upper, conjugate-transpose, non-unit
  n = 3
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d-300, 0.0d0)
  A(1,2) = (1.0d0, 1.0d0)
  A(1,3) = (0.5d0, 0.0d0)
  A(2,2) = (1.0d-300, 0.0d0)
  A(2,3) = (1.0d0, -1.0d0)
  A(3,3) = (1.0d-300, 0.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatrs('U', 'C', 'N', 'N', n, A, NMAX, x, scale, cnorm, info)
  call begin_test('upper_C_careful')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 20: Careful solve - upper, no-transpose, unit diagonal (large off-diag)
  n = 3
  A = (0.0d0, 0.0d0)
  A(1,1) = (99.0d0, 99.0d0)
  A(1,2) = (1.0d+150, 1.0d+150)
  A(1,3) = (1.0d+150, 0.0d0)
  A(2,2) = (99.0d0, 99.0d0)
  A(2,3) = (1.0d+150, -1.0d+150)
  A(3,3) = (99.0d0, 99.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatrs('U', 'N', 'U', 'N', n, A, NMAX, x, scale, cnorm, info)
  call begin_test('upper_N_unit_careful')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 21: Careful solve - lower, conjugate-transpose, non-unit
  n = 3
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d-300, 0.0d0)
  A(2,1) = (1.0d0, 1.0d0)
  A(2,2) = (1.0d-300, 0.0d0)
  A(3,1) = (0.5d0, 0.0d0)
  A(3,2) = (1.0d0, -1.0d0)
  A(3,3) = (1.0d-300, 0.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatrs('L', 'C', 'N', 'N', n, A, NMAX, x, scale, cnorm, info)
  call begin_test('lower_C_careful')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

end program
