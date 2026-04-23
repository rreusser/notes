program test_dtbcon
  use test_utils
  implicit none
  double precision :: ab(5, 6)
  double precision :: work(18)
  integer :: iwork(6)
  double precision :: rcond
  integer :: info, n, kd, ldab

  ! ============================================================
  ! Test matrix 1: 4x4 upper triangular, bandwidth kd=2
  !
  ! Full matrix A:
  !   [ 10  -1   2   0 ]
  !   [  0   8  -2   1 ]
  !   [  0   0  12  -3 ]
  !   [  0   0   0   6 ]
  !
  ! Band storage (LDAB=5, KD=2, upper):
  !   row 0 (superdiag-2): AB(1,j) = A(j-2, j)
  !   row 1 (superdiag-1): AB(2,j) = A(j-1, j)
  !   row 2 (diagonal):    AB(3,j) = A(j, j)
  !
  !   AB (3 x 4, 1-indexed, Fortran rows 1..3):
  !   col:   1     2     3     4
  !   [ *     *     2     1 ]   <- superdiag 2
  !   [ *    -1    -2    -3 ]   <- superdiag 1
  !   [ 10    8    12     6 ]   <- diagonal
  ! ============================================================
  n = 4
  kd = 2
  ldab = 5

  ab = 0.0d0
  ! Column 1: diagonal only
  ab(3, 1) = 10.0d0
  ! Column 2: superdiag-1 and diagonal
  ab(2, 2) = -1.0d0
  ab(3, 2) =  8.0d0
  ! Column 3: superdiag-2, superdiag-1, diagonal
  ab(1, 3) =  2.0d0
  ab(2, 3) = -2.0d0
  ab(3, 3) = 12.0d0
  ! Column 4: superdiag-2, superdiag-1, diagonal
  ab(1, 4) =  1.0d0
  ab(2, 4) = -3.0d0
  ab(3, 4) =  6.0d0

  ! Test 1: upper, non-unit, 1-norm
  call dtbcon('1', 'U', 'N', n, kd, ab, ldab, rcond, work, iwork, info)
  call begin_test('upper_nonunit_onenorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 2: upper, non-unit, inf-norm
  call dtbcon('I', 'U', 'N', n, kd, ab, ldab, rcond, work, iwork, info)
  call begin_test('upper_nonunit_infnorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 3: upper, unit diagonal, 1-norm
  call dtbcon('1', 'U', 'U', n, kd, ab, ldab, rcond, work, iwork, info)
  call begin_test('upper_unit_onenorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 4: upper, unit diagonal, inf-norm
  call dtbcon('I', 'U', 'U', n, kd, ab, ldab, rcond, work, iwork, info)
  call begin_test('upper_unit_infnorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test matrix 2: 4x4 lower triangular, bandwidth kd=2
  !
  ! Full matrix A:
  !   [  5   0   0   0 ]
  !   [ -2   7   0   0 ]
  !   [  1  -1   9   0 ]
  !   [  0   2  -3  11 ]
  !
  ! Band storage (LDAB=5, KD=2, lower):
  !   row 0 (diagonal):    AB(1,j) = A(j, j)
  !   row 1 (subdiag-1):   AB(2,j) = A(j+1, j)
  !   row 2 (subdiag-2):   AB(3,j) = A(j+2, j)
  !
  !   AB (3 x 4, 1-indexed, Fortran rows 1..3):
  !   col:   1     2     3     4
  !   [  5    7     9    11 ]   <- diagonal
  !   [ -2   -1    -3     * ]   <- subdiag 1
  !   [  1    2     *     * ]   <- subdiag 2
  ! ============================================================
  ab = 0.0d0
  ! Column 1
  ab(1, 1) =  5.0d0
  ab(2, 1) = -2.0d0
  ab(3, 1) =  1.0d0
  ! Column 2
  ab(1, 2) =  7.0d0
  ab(2, 2) = -1.0d0
  ab(3, 2) =  2.0d0
  ! Column 3
  ab(1, 3) =  9.0d0
  ab(2, 3) = -3.0d0
  ! Column 4
  ab(1, 4) = 11.0d0

  ! Test 5: lower, non-unit, 1-norm
  call dtbcon('1', 'L', 'N', n, kd, ab, ldab, rcond, work, iwork, info)
  call begin_test('lower_nonunit_onenorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 6: lower, non-unit, inf-norm
  call dtbcon('I', 'L', 'N', n, kd, ab, ldab, rcond, work, iwork, info)
  call begin_test('lower_nonunit_infnorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 7: lower, unit diagonal, 1-norm
  call dtbcon('1', 'L', 'U', n, kd, ab, ldab, rcond, work, iwork, info)
  call begin_test('lower_unit_onenorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 8: lower, unit diagonal, inf-norm
  call dtbcon('I', 'L', 'U', n, kd, ab, ldab, rcond, work, iwork, info)
  call begin_test('lower_unit_infnorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Edge cases
  ! ============================================================

  ! Test 9: N=0
  call dtbcon('1', 'U', 'N', 0, 0, ab, ldab, rcond, work, iwork, info)
  call begin_test('edge_n0')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 10: N=1, kd=0
  ab = 0.0d0
  ab(1, 1) = 3.0d0
  call dtbcon('1', 'U', 'N', 1, 0, ab, ldab, rcond, work, iwork, info)
  call begin_test('edge_n1_kd0')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 11: identity band matrix (perfect condition), kd=1
  !
  !   [ 1  0  0 ]
  !   [ 0  1  0 ]
  !   [ 0  0  1 ]
  !
  !   Band storage (LDAB=5, KD=1, upper):
  !   col:  1    2    3
  !   [ *    0    0 ]  <- superdiag
  !   [ 1    1    1 ]  <- diagonal
  ! ============================================================
  n = 3
  kd = 1
  ab = 0.0d0
  ab(2, 1) = 1.0d0
  ab(2, 2) = 1.0d0
  ab(2, 3) = 1.0d0

  call dtbcon('1', 'U', 'N', n, kd, ab, ldab, rcond, work, iwork, info)
  call begin_test('identity_onenorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  call dtbcon('I', 'U', 'N', n, kd, ab, ldab, rcond, work, iwork, info)
  call begin_test('identity_infnorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 13: ill-conditioned band matrix (diagonal only, kd=0)
  !
  !   [ 1e12   0      0    ]
  !   [  0     1      0    ]
  !   [  0     0     1e-12 ]
  ! ============================================================
  n = 3
  kd = 0
  ab = 0.0d0
  ab(1, 1) = 1.0d12
  ab(1, 2) = 1.0d0
  ab(1, 3) = 1.0d-12

  call dtbcon('1', 'U', 'N', n, kd, ab, ldab, rcond, work, iwork, info)
  call begin_test('ill_conditioned_onenorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  call dtbcon('I', 'U', 'N', n, kd, ab, ldab, rcond, work, iwork, info)
  call begin_test('ill_conditioned_infnorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 15: 5x5 upper triangular, kd=3 (wider bandwidth)
  !
  ! Full matrix A:
  !   [ 4  -1   2  -1   0 ]
  !   [ 0   6  -2   1  -1 ]
  !   [ 0   0   8  -3   2 ]
  !   [ 0   0   0   5  -2 ]
  !   [ 0   0   0   0   7 ]
  !
  ! Band storage (KD=3, upper):
  !   row 0 (superdiag-3): AB(1,j) = A(j-3, j)
  !   row 1 (superdiag-2): AB(2,j) = A(j-2, j)
  !   row 2 (superdiag-1): AB(3,j) = A(j-1, j)
  !   row 3 (diagonal):    AB(4,j) = A(j, j)
  ! ============================================================
  n = 5
  kd = 3

  ab = 0.0d0
  ! Col 1
  ab(4, 1) = 4.0d0
  ! Col 2
  ab(3, 2) = -1.0d0
  ab(4, 2) =  6.0d0
  ! Col 3
  ab(2, 3) =  2.0d0
  ab(3, 3) = -2.0d0
  ab(4, 3) =  8.0d0
  ! Col 4
  ab(1, 4) = -1.0d0
  ab(2, 4) =  1.0d0
  ab(3, 4) = -3.0d0
  ab(4, 4) =  5.0d0
  ! Col 5
  ab(2, 5) = -1.0d0
  ab(3, 5) =  2.0d0
  ab(4, 5) = -2.0d0
  ! Note: ab(4,5) should be the diagonal. Let me fix:
  ab(4, 5) =  7.0d0
  ab(3, 5) = -2.0d0
  ab(2, 5) =  2.0d0

  call dtbcon('1', 'U', 'N', n, kd, ab, ldab, rcond, work, iwork, info)
  call begin_test('upper_nonunit_onenorm_kd3')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  call dtbcon('I', 'U', 'N', n, kd, ab, ldab, rcond, work, iwork, info)
  call begin_test('upper_nonunit_infnorm_kd3')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

end program
