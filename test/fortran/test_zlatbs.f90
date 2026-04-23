program test_zlatbs
  use test_utils
  implicit none

  integer, parameter :: NMAX = 5, KDMAX = 2
  complex*16 :: AB(KDMAX+1, NMAX), X(NMAX)
  double precision :: CNORM(NMAX), SCALE
  double precision :: X_r(2*NMAX)
  equivalence (X, X_r)
  integer :: INFO, n, kd

  ! Test 1: Upper, non-unit, no-transpose, 4x4, kd=2
  n = 4
  kd = 2
  AB = (0.0d0, 0.0d0)
  ! Band storage: AB(kd+1+i-j, j) = A(i,j) for max(1,j-kd)<=i<=j
  ! Diagonal: AB(3, j) = A(j,j)
  AB(3,1) = (2.0d0, 1.0d0)
  AB(3,2) = (3.0d0, -1.0d0)
  AB(3,3) = (4.0d0, 2.0d0)
  AB(3,4) = (5.0d0, 0.0d0)
  ! Superdiag 1: AB(2,j) = A(j-1,j)
  AB(2,2) = (1.0d0, 0.5d0)
  AB(2,3) = (2.0d0, -1.0d0)
  AB(2,4) = (1.5d0, 1.0d0)
  ! Superdiag 2: AB(1,j) = A(j-2,j)
  AB(1,3) = (0.5d0, 0.0d0)
  AB(1,4) = (0.3d0, 0.2d0)

  X(1) = (1.0d0, 0.0d0)
  X(2) = (2.0d0, 1.0d0)
  X(3) = (-1.0d0, 3.0d0)
  X(4) = (0.5d0, -0.5d0)

  CNORM = 0.0d0
  call ZLATBS('U', 'N', 'N', 'N', n, kd, AB, kd+1, X, SCALE, CNORM, INFO)
  call begin_test('upper_notrans_nonunit_4x4')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call print_array('X', X_r, 2*n)
  call print_array('cnorm', CNORM, n)
  call end_test()

  ! Test 2: Lower, non-unit, no-transpose, 4x4, kd=2
  n = 4
  kd = 2
  AB = (0.0d0, 0.0d0)
  ! Band storage lower: AB(1+i-j, j) = A(i,j) for j<=i<=min(n,j+kd)
  ! Diagonal: AB(1,j) = A(j,j)
  AB(1,1) = (3.0d0, 0.0d0)
  AB(1,2) = (4.0d0, 1.0d0)
  AB(1,3) = (2.0d0, -1.0d0)
  AB(1,4) = (5.0d0, 2.0d0)
  ! Subdiag 1: AB(2,j) = A(j+1,j)
  AB(2,1) = (1.0d0, 1.0d0)
  AB(2,2) = (0.5d0, -0.5d0)
  AB(2,3) = (2.0d0, 0.0d0)
  ! Subdiag 2: AB(3,j) = A(j+2,j)
  AB(3,1) = (0.2d0, 0.1d0)
  AB(3,2) = (0.3d0, -0.2d0)

  X(1) = (1.0d0, 0.0d0)
  X(2) = (0.0d0, 1.0d0)
  X(3) = (2.0d0, -1.0d0)
  X(4) = (-1.0d0, 0.5d0)

  CNORM = 0.0d0
  call ZLATBS('L', 'N', 'N', 'N', n, kd, AB, kd+1, X, SCALE, CNORM, INFO)
  call begin_test('lower_notrans_nonunit_4x4')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call print_array('X', X_r, 2*n)
  call print_array('cnorm', CNORM, n)
  call end_test()

  ! Test 3: Upper, conjugate-transpose, non-unit, 4x4, kd=2
  n = 4
  kd = 2
  AB = (0.0d0, 0.0d0)
  AB(3,1) = (2.0d0, 1.0d0)
  AB(3,2) = (3.0d0, -1.0d0)
  AB(3,3) = (4.0d0, 2.0d0)
  AB(3,4) = (5.0d0, 0.0d0)
  AB(2,2) = (1.0d0, 0.5d0)
  AB(2,3) = (2.0d0, -1.0d0)
  AB(2,4) = (1.5d0, 1.0d0)
  AB(1,3) = (0.5d0, 0.0d0)
  AB(1,4) = (0.3d0, 0.2d0)

  X(1) = (1.0d0, 2.0d0)
  X(2) = (-1.0d0, 1.0d0)
  X(3) = (0.5d0, -0.5d0)
  X(4) = (3.0d0, 0.0d0)

  CNORM = 0.0d0
  call ZLATBS('U', 'C', 'N', 'N', n, kd, AB, kd+1, X, SCALE, CNORM, INFO)
  call begin_test('upper_conjtrans_nonunit_4x4')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call print_array('X', X_r, 2*n)
  call print_array('cnorm', CNORM, n)
  call end_test()

  ! Test 4: Upper, transpose, non-unit, 4x4, kd=2
  n = 4
  kd = 2
  AB = (0.0d0, 0.0d0)
  AB(3,1) = (2.0d0, 1.0d0)
  AB(3,2) = (3.0d0, -1.0d0)
  AB(3,3) = (4.0d0, 2.0d0)
  AB(3,4) = (5.0d0, 0.0d0)
  AB(2,2) = (1.0d0, 0.5d0)
  AB(2,3) = (2.0d0, -1.0d0)
  AB(2,4) = (1.5d0, 1.0d0)
  AB(1,3) = (0.5d0, 0.0d0)
  AB(1,4) = (0.3d0, 0.2d0)

  X(1) = (1.0d0, 2.0d0)
  X(2) = (-1.0d0, 1.0d0)
  X(3) = (0.5d0, -0.5d0)
  X(4) = (3.0d0, 0.0d0)

  CNORM = 0.0d0
  call ZLATBS('U', 'T', 'N', 'N', n, kd, AB, kd+1, X, SCALE, CNORM, INFO)
  call begin_test('upper_trans_nonunit_4x4')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call print_array('X', X_r, 2*n)
  call print_array('cnorm', CNORM, n)
  call end_test()

  ! Test 5: Lower, unit diagonal, no-transpose, 4x4, kd=2 (reuse full AB size)
  n = 4
  kd = 2
  AB = (0.0d0, 0.0d0)
  ! Diagonal ignored for unit
  AB(1,1) = (999.0d0, 999.0d0)
  AB(1,2) = (999.0d0, 999.0d0)
  AB(1,3) = (999.0d0, 999.0d0)
  AB(1,4) = (999.0d0, 999.0d0)
  ! Subdiag 1: AB(2,j) = A(j+1,j)
  AB(2,1) = (0.5d0, 0.5d0)
  AB(2,2) = (1.0d0, -1.0d0)
  AB(2,3) = (-0.5d0, 0.25d0)
  ! Subdiag 2: all zero (already init)

  X(1) = (1.0d0, 0.0d0)
  X(2) = (2.0d0, 1.0d0)
  X(3) = (-1.0d0, 2.0d0)
  X(4) = (0.5d0, -0.5d0)

  CNORM = 0.0d0
  call ZLATBS('L', 'N', 'U', 'N', n, kd, AB, kd+1, X, SCALE, CNORM, INFO)
  call begin_test('lower_notrans_unit_4x4')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call print_array('X', X_r, 2*n)
  call print_array('cnorm', CNORM, n)
  call end_test()

  ! Test 6: N=0
  call ZLATBS('U', 'N', 'N', 'N', 0, 0, AB, 1, X, SCALE, CNORM, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call end_test()

  ! Test 7: N=1, upper, non-unit (kd=2 but only 1 element matters)
  n = 1
  kd = 2
  AB = (0.0d0, 0.0d0)
  AB(3,1) = (3.0d0, 4.0d0)
  X(1) = (5.0d0, -2.0d0)
  CNORM = 0.0d0
  call ZLATBS('U', 'N', 'N', 'N', n, kd, AB, kd+1, X, SCALE, CNORM, INFO)
  call begin_test('n1_upper')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call print_array('X', X_r, 2*n)
  call end_test()

  ! Test 8: Lower, conjugate-transpose, non-unit, 4x4, kd=2
  n = 4
  kd = 2
  AB = (0.0d0, 0.0d0)
  AB(1,1) = (3.0d0, 0.0d0)
  AB(1,2) = (4.0d0, 1.0d0)
  AB(1,3) = (2.0d0, -1.0d0)
  AB(1,4) = (5.0d0, 2.0d0)
  AB(2,1) = (1.0d0, 1.0d0)
  AB(2,2) = (0.5d0, -0.5d0)
  AB(2,3) = (2.0d0, 0.0d0)
  AB(3,1) = (0.2d0, 0.1d0)
  AB(3,2) = (0.3d0, -0.2d0)

  X(1) = (1.0d0, -1.0d0)
  X(2) = (2.0d0, 0.0d0)
  X(3) = (0.0d0, 3.0d0)
  X(4) = (-1.0d0, 1.0d0)

  CNORM = 0.0d0
  call ZLATBS('L', 'C', 'N', 'N', n, kd, AB, kd+1, X, SCALE, CNORM, INFO)
  call begin_test('lower_conjtrans_nonunit_4x4')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call print_array('X', X_r, 2*n)
  call print_array('cnorm', CNORM, n)
  call end_test()

  ! Test 9: Singular matrix (zero diagonal), upper, non-unit, kd=2
  n = 3
  kd = 2
  AB = (0.0d0, 0.0d0)
  ! Diagonal: AB(kd+1, j) = AB(3, j)
  AB(3,1) = (2.0d0, 0.0d0)
  AB(3,2) = (0.0d0, 0.0d0)  ! Singular!
  AB(3,3) = (3.0d0, 1.0d0)
  ! Superdiag 1: AB(2, j)
  AB(2,2) = (1.0d0, -1.0d0)
  AB(2,3) = (0.5d0, 0.5d0)
  ! Superdiag 2: all zero

  X(1) = (1.0d0, 0.0d0)
  X(2) = (2.0d0, 1.0d0)
  X(3) = (-1.0d0, 0.0d0)

  CNORM = 0.0d0
  call ZLATBS('U', 'N', 'N', 'N', n, kd, AB, kd+1, X, SCALE, CNORM, INFO)
  call begin_test('singular_upper')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call print_array('X', X_r, 2*n)
  call end_test()

end program
