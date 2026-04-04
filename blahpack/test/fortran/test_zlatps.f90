program test_zlatps
  use test_utils
  implicit none
  integer, parameter :: NMAX = 4
  integer, parameter :: APMAX = NMAX*(NMAX+1)/2
  complex*16 :: AP(APMAX), x(NMAX)
  double precision :: x_r(2*NMAX), AP_r(2*APMAX)
  double precision :: scale, cnorm(NMAX)
  integer :: info, n
  equivalence (x, x_r)
  equivalence (AP, AP_r)

  ! Test 1: Upper triangular, no transpose, non-unit, 3x3
  ! A = [[2+i, 1+i, 0.5], [0, 3+0.5i, 1-i], [0, 0, 4-i]]
  ! Upper packed: col-major => A(1,1), A(1,2), A(2,2), A(1,3), A(2,3), A(3,3)
  n = 3
  AP = (0.0d0, 0.0d0)
  AP(1) = (2.0d0, 1.0d0)
  AP(2) = (1.0d0, 1.0d0)
  AP(3) = (3.0d0, 0.5d0)
  AP(4) = (0.5d0, 0.0d0)
  AP(5) = (1.0d0, -1.0d0)
  AP(6) = (4.0d0, -1.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatps('U', 'N', 'N', 'N', n, AP, x, scale, cnorm, info)
  call begin_test('upper_N_nonunit')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

  ! Test 2: Lower triangular, no transpose, non-unit, 3x3
  ! A = [[2+i, 0, 0], [1+i, 3+0.5i, 0], [0.5, 1-i, 4-i]]
  ! Lower packed: col-major => A(1,1), A(2,1), A(3,1), A(2,2), A(3,2), A(3,3)
  AP = (0.0d0, 0.0d0)
  AP(1) = (2.0d0, 1.0d0)
  AP(2) = (1.0d0, 1.0d0)
  AP(3) = (0.5d0, 0.0d0)
  AP(4) = (3.0d0, 0.5d0)
  AP(5) = (1.0d0, -1.0d0)
  AP(6) = (4.0d0, -1.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatps('L', 'N', 'N', 'N', n, AP, x, scale, cnorm, info)
  call begin_test('lower_N_nonunit')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

  ! Test 3: Upper triangular, conjugate-transpose, non-unit, 3x3
  AP = (0.0d0, 0.0d0)
  AP(1) = (2.0d0, 1.0d0)
  AP(2) = (1.0d0, 1.0d0)
  AP(3) = (3.0d0, 0.5d0)
  AP(4) = (0.5d0, 0.0d0)
  AP(5) = (1.0d0, -1.0d0)
  AP(6) = (4.0d0, -1.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatps('U', 'C', 'N', 'N', n, AP, x, scale, cnorm, info)
  call begin_test('upper_C_nonunit')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 4: Lower triangular, conjugate-transpose, non-unit, 3x3
  AP = (0.0d0, 0.0d0)
  AP(1) = (2.0d0, 1.0d0)
  AP(2) = (1.0d0, 1.0d0)
  AP(3) = (0.5d0, 0.0d0)
  AP(4) = (3.0d0, 0.5d0)
  AP(5) = (1.0d0, -1.0d0)
  AP(6) = (4.0d0, -1.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatps('L', 'C', 'N', 'N', n, AP, x, scale, cnorm, info)
  call begin_test('lower_C_nonunit')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 5: Upper triangular, unit diagonal, no transpose
  AP = (0.0d0, 0.0d0)
  AP(1) = (99.0d0, 99.0d0)
  AP(2) = (1.0d0, 1.0d0)
  AP(3) = (99.0d0, 99.0d0)
  AP(4) = (0.5d0, 0.0d0)
  AP(5) = (1.0d0, -1.0d0)
  AP(6) = (99.0d0, 99.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatps('U', 'N', 'U', 'N', n, AP, x, scale, cnorm, info)
  call begin_test('upper_N_unit')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 6: Lower triangular, unit diagonal, no transpose
  AP = (0.0d0, 0.0d0)
  AP(1) = (99.0d0, 99.0d0)
  AP(2) = (1.0d0, 1.0d0)
  AP(3) = (0.5d0, 0.0d0)
  AP(4) = (99.0d0, 99.0d0)
  AP(5) = (1.0d0, -1.0d0)
  AP(6) = (99.0d0, 99.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatps('L', 'N', 'U', 'N', n, AP, x, scale, cnorm, info)
  call begin_test('lower_N_unit')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 7: N=0
  scale = 0.0d0
  call zlatps('U', 'N', 'N', 'N', 0, AP, x, scale, cnorm, info)
  call begin_test('n_zero')
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 8: N=1
  AP(1) = (5.0d0, 2.0d0)
  x(1) = (10.0d0, -3.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatps('U', 'N', 'N', 'N', 1, AP, x, scale, cnorm, info)
  call begin_test('n_one')
  call print_array('x', x_r, 2)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 9: Upper, normin='Y' (pre-computed column norms)
  n = 3
  AP = (0.0d0, 0.0d0)
  AP(1) = (2.0d0, 1.0d0)
  AP(2) = (1.0d0, 1.0d0)
  AP(3) = (3.0d0, 0.5d0)
  AP(4) = (0.5d0, 0.0d0)
  AP(5) = (1.0d0, -1.0d0)
  AP(6) = (4.0d0, -1.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  cnorm(1) = 0.0d0
  cnorm(2) = abs(1.0d0) + abs(1.0d0)
  cnorm(3) = abs(0.5d0) + abs(0.0d0) + abs(1.0d0) + abs(-1.0d0)
  scale = 0.0d0
  call zlatps('U', 'N', 'N', 'Y', n, AP, x, scale, cnorm, info)
  call begin_test('upper_N_normin_Y')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 10: Upper, conjugate transpose, unit diagonal
  AP = (0.0d0, 0.0d0)
  AP(1) = (99.0d0, 99.0d0)
  AP(2) = (1.0d0, 1.0d0)
  AP(3) = (99.0d0, 99.0d0)
  AP(4) = (0.5d0, 0.0d0)
  AP(5) = (1.0d0, -1.0d0)
  AP(6) = (99.0d0, 99.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatps('U', 'C', 'U', 'N', n, AP, x, scale, cnorm, info)
  call begin_test('upper_C_unit')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 11: Lower, conjugate transpose, unit diagonal
  AP = (0.0d0, 0.0d0)
  AP(1) = (99.0d0, 99.0d0)
  AP(2) = (1.0d0, 1.0d0)
  AP(3) = (0.5d0, 0.0d0)
  AP(4) = (99.0d0, 99.0d0)
  AP(5) = (1.0d0, -1.0d0)
  AP(6) = (99.0d0, 99.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatps('L', 'C', 'U', 'N', n, AP, x, scale, cnorm, info)
  call begin_test('lower_C_unit')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 12: 4x4 upper, no transpose
  n = 4
  AP = (0.0d0, 0.0d0)
  ! Upper packed col-major for 4x4:
  ! col1: A(1,1) => AP(1)
  ! col2: A(1,2), A(2,2) => AP(2), AP(3)
  ! col3: A(1,3), A(2,3), A(3,3) => AP(4), AP(5), AP(6)
  ! col4: A(1,4), A(2,4), A(3,4), A(4,4) => AP(7), AP(8), AP(9), AP(10)
  AP(1) = (3.0d0, 0.0d0)
  AP(2) = (1.0d0, 0.5d0)
  AP(3) = (4.0d0, 1.0d0)
  AP(4) = (0.0d0, 1.0d0)
  AP(5) = (1.0d0, 0.0d0)
  AP(6) = (2.0d0, -1.0d0)
  AP(7) = (0.5d0, 0.0d0)
  AP(8) = (0.0d0, 0.5d0)
  AP(9) = (1.0d0, 1.0d0)
  AP(10) = (5.0d0, 0.0d0)
  x(1) = (1.0d0, 1.0d0)
  x(2) = (2.0d0, 0.0d0)
  x(3) = (0.0d0, 3.0d0)
  x(4) = (1.0d0, -2.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatps('U', 'N', 'N', 'N', n, AP, x, scale, cnorm, info)
  call begin_test('upper_N_4x4')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 13: Lower, transpose, unit, normin='Y'
  n = 3
  AP = (0.0d0, 0.0d0)
  AP(1) = (99.0d0, 99.0d0)
  AP(2) = (1.0d0, 1.0d0)
  AP(3) = (0.5d0, 0.0d0)
  AP(4) = (99.0d0, 99.0d0)
  AP(5) = (1.0d0, -1.0d0)
  AP(6) = (99.0d0, 99.0d0)
  x(1) = (5.0d0, 1.0d0)
  x(2) = (3.0d0, -2.0d0)
  x(3) = (1.0d0, 0.0d0)
  cnorm(1) = abs(1.0d0)+abs(1.0d0)+abs(0.5d0)+abs(0.0d0)
  cnorm(2) = abs(1.0d0)+abs(-1.0d0)
  cnorm(3) = 0.0d0
  scale = 0.0d0
  call zlatps('L', 'T', 'U', 'Y', n, AP, x, scale, cnorm, info)
  call begin_test('lower_T_unit_norminY')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 14: Careful solve - upper, no-transpose, non-unit (tiny diagonal)
  n = 3
  AP = (0.0d0, 0.0d0)
  AP(1) = (1.0d-300, 0.0d0)
  AP(2) = (1.0d0, 1.0d0)
  AP(3) = (1.0d-300, 0.0d0)
  AP(4) = (0.5d0, 0.0d0)
  AP(5) = (1.0d0, -1.0d0)
  AP(6) = (1.0d-300, 0.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatps('U', 'N', 'N', 'N', n, AP, x, scale, cnorm, info)
  call begin_test('upper_N_careful')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 15: Careful solve - lower, no-transpose, non-unit
  n = 3
  AP = (0.0d0, 0.0d0)
  AP(1) = (1.0d-300, 0.0d0)
  AP(2) = (1.0d0, 1.0d0)
  AP(3) = (0.5d0, 0.0d0)
  AP(4) = (1.0d-300, 0.0d0)
  AP(5) = (1.0d0, -1.0d0)
  AP(6) = (1.0d-300, 0.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatps('L', 'N', 'N', 'N', n, AP, x, scale, cnorm, info)
  call begin_test('lower_N_careful')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 16: Careful solve - upper, conjugate-transpose, non-unit
  n = 3
  AP = (0.0d0, 0.0d0)
  AP(1) = (1.0d-300, 0.0d0)
  AP(2) = (1.0d0, 1.0d0)
  AP(3) = (1.0d-300, 0.0d0)
  AP(4) = (0.5d0, 0.0d0)
  AP(5) = (1.0d0, -1.0d0)
  AP(6) = (1.0d-300, 0.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatps('U', 'C', 'N', 'N', n, AP, x, scale, cnorm, info)
  call begin_test('upper_C_careful')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 17: Careful solve - lower, conjugate-transpose, non-unit
  n = 3
  AP = (0.0d0, 0.0d0)
  AP(1) = (1.0d-300, 0.0d0)
  AP(2) = (1.0d0, 1.0d0)
  AP(3) = (0.5d0, 0.0d0)
  AP(4) = (1.0d-300, 0.0d0)
  AP(5) = (1.0d0, -1.0d0)
  AP(6) = (1.0d-300, 0.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatps('L', 'C', 'N', 'N', n, AP, x, scale, cnorm, info)
  call begin_test('lower_C_careful')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 18: 4x4 lower, conjugate-transpose, non-unit
  n = 4
  AP = (0.0d0, 0.0d0)
  ! Lower packed col-major for 4x4:
  ! col1: A(1,1), A(2,1), A(3,1), A(4,1) => AP(1..4)
  ! col2: A(2,2), A(3,2), A(4,2) => AP(5..7)
  ! col3: A(3,3), A(4,3) => AP(8..9)
  ! col4: A(4,4) => AP(10)
  AP(1) = (3.0d0, 0.0d0)
  AP(2) = (1.0d0, 0.5d0)
  AP(3) = (0.0d0, 1.0d0)
  AP(4) = (0.5d0, 0.0d0)
  AP(5) = (4.0d0, 1.0d0)
  AP(6) = (1.0d0, 0.0d0)
  AP(7) = (0.0d0, 0.5d0)
  AP(8) = (2.0d0, -1.0d0)
  AP(9) = (1.0d0, 1.0d0)
  AP(10) = (5.0d0, 0.0d0)
  x(1) = (1.0d0, 1.0d0)
  x(2) = (2.0d0, 0.0d0)
  x(3) = (0.0d0, 3.0d0)
  x(4) = (1.0d0, -2.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatps('L', 'C', 'N', 'N', n, AP, x, scale, cnorm, info)
  call begin_test('lower_C_4x4')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 19: Upper, unit, careful solve (large off-diagonal)
  n = 3
  AP = (0.0d0, 0.0d0)
  AP(1) = (99.0d0, 99.0d0)
  AP(2) = (1.0d+150, 1.0d+150)
  AP(3) = (99.0d0, 99.0d0)
  AP(4) = (1.0d+150, 0.0d0)
  AP(5) = (1.0d+150, -1.0d+150)
  AP(6) = (99.0d0, 99.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 1.0d0)
  x(3) = (3.0d0, -1.0d0)
  scale = 0.0d0
  cnorm = 0.0d0
  call zlatps('U', 'N', 'U', 'N', n, AP, x, scale, cnorm, info)
  call begin_test('upper_N_unit_careful')
  call print_array('x', x_r, 2*n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

end program
