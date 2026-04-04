program test_dgbequ
  use test_utils
  implicit none
  integer, parameter :: MAXAB = 200
  double precision :: ab(MAXAB)
  double precision :: r(10), c(10)
  double precision :: rowcnd, colcnd, amax
  integer :: info, m, n, kl, ku, ldab

  ! Test 1: basic 3x3 band matrix with KL=1, KU=1 (tridiagonal)
  ! Dense matrix A:
  !   [ 2  3  0 ]
  !   [ 1  4  2 ]
  !   [ 0  5  6 ]
  ! Band storage (LDAB=3):
  ! Row 0 (superdiag): *    3    2
  ! Row 1 (diagonal):  2    4    6
  ! Row 2 (subdiag):   1    5    *
  m = 3
  n = 3
  kl = 1
  ku = 1
  ldab = kl + ku + 1
  ab = 0.0d0
  ! Column 1
  ab(2) = 2.0d0   ! A(1,1) diag
  ab(3) = 1.0d0   ! A(2,1) subdiag
  ! Column 2
  ab(4) = 3.0d0   ! A(1,2) superdiag
  ab(5) = 4.0d0   ! A(2,2) diag
  ab(6) = 5.0d0   ! A(3,2) subdiag
  ! Column 3
  ab(7) = 2.0d0   ! A(2,3) superdiag
  ab(8) = 6.0d0   ! A(3,3) diag
  call dgbequ(m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd, amax, info)
  call begin_test('basic')
  call print_array('r', r, m)
  call print_array('c', c, n)
  call print_scalar('rowcnd', rowcnd)
  call print_scalar('colcnd', colcnd)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

  ! Test 2: 4x4 matrix with KL=2, KU=1
  ! Dense matrix A:
  !   [ 1  2  0  0 ]
  !   [ 3  4  5  0 ]
  !   [ 7  6  8  9 ]
  !   [ 0  1  3  4 ]
  ! Band storage (LDAB=4):
  ! Row 0 (superdiag):  *    2    5    9
  ! Row 1 (diagonal):   1    4    8    4
  ! Row 2 (subdiag1):   3    6    3    *
  ! Row 3 (subdiag2):   7    1    *    *
  m = 4
  n = 4
  kl = 2
  ku = 1
  ldab = kl + ku + 1
  ab = 0.0d0
  ! Column 1
  ab(2) = 1.0d0   ! A(1,1) diag
  ab(3) = 3.0d0   ! A(2,1) subdiag1
  ab(4) = 7.0d0   ! A(3,1) subdiag2
  ! Column 2
  ab(5) = 2.0d0   ! A(1,2) superdiag
  ab(6) = 4.0d0   ! A(2,2) diag
  ab(7) = 6.0d0   ! A(3,2) subdiag1
  ab(8) = 1.0d0   ! A(4,2) subdiag2
  ! Column 3
  ab(9) = 5.0d0    ! A(2,3) superdiag
  ab(10) = 8.0d0   ! A(3,3) diag
  ab(11) = 3.0d0   ! A(4,3) subdiag1
  ! Column 4
  ab(13) = 9.0d0   ! A(3,4) superdiag
  ab(14) = 4.0d0   ! A(4,4) diag
  call dgbequ(m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd, amax, info)
  call begin_test('larger')
  call print_array('r', r, m)
  call print_array('c', c, n)
  call print_scalar('rowcnd', rowcnd)
  call print_scalar('colcnd', colcnd)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

  ! Test 3: zero row (row 2 is all zero)
  ! Dense matrix A:
  !   [ 1  2 ]
  !   [ 0  0 ]
  !   [ 0  3 ]
  ! KL=2, KU=1
  m = 3
  n = 2
  kl = 2
  ku = 1
  ldab = kl + ku + 1
  ab = 0.0d0
  ! Column 1
  ab(2) = 1.0d0   ! A(1,1)
  ab(3) = 0.0d0   ! A(2,1)
  ab(4) = 0.0d0   ! A(3,1)
  ! Column 2
  ab(5) = 2.0d0   ! A(1,2)
  ab(6) = 0.0d0   ! A(2,2)
  ab(7) = 3.0d0   ! A(3,2)
  call dgbequ(m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd, amax, info)
  call begin_test('zero_row')
  call print_int('info', info)
  call end_test()

  ! Test 4: zero column (after row scaling)
  ! Dense matrix A:
  !   [ 1  0 ]
  !   [ 2  0 ]
  ! KL=1, KU=0
  m = 2
  n = 2
  kl = 1
  ku = 0
  ldab = kl + ku + 1
  ab = 0.0d0
  ! Column 1
  ab(1) = 1.0d0   ! A(1,1)
  ab(2) = 2.0d0   ! A(2,1)
  ! Column 2
  ab(3) = 0.0d0   ! A(1,2) doesn't exist
  ab(4) = 0.0d0   ! A(2,2)
  call dgbequ(m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd, amax, info)
  call begin_test('zero_col')
  call print_array('r', r, m)
  call print_int('info', info)
  call end_test()

  ! Test 5: quick return M=0
  call dgbequ(0, 3, 0, 0, ab, 1, r, c, rowcnd, colcnd, amax, info)
  call begin_test('m_zero')
  call print_scalar('rowcnd', rowcnd)
  call print_scalar('colcnd', colcnd)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

  ! Test 6: quick return N=0
  call dgbequ(3, 0, 0, 0, ab, 1, r, c, rowcnd, colcnd, amax, info)
  call begin_test('n_zero')
  call print_scalar('rowcnd', rowcnd)
  call print_scalar('colcnd', colcnd)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

  ! Test 7: diagonal only (KL=0, KU=0)
  ! Dense matrix A = diag(3, 1, 2)
  m = 3
  n = 3
  kl = 0
  ku = 0
  ldab = 1
  ab(1) = 3.0d0   ! A(1,1)
  ab(2) = 1.0d0   ! A(2,2)
  ab(3) = 2.0d0   ! A(3,3)
  call dgbequ(m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd, amax, info)
  call begin_test('diagonal')
  call print_array('r', r, m)
  call print_array('c', c, n)
  call print_scalar('rowcnd', rowcnd)
  call print_scalar('colcnd', colcnd)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

  ! Test 8: non-square 2x4 band matrix KL=0, KU=1
  ! Dense matrix A:
  !   [ 1  3  0  0 ]
  !   [ 0  4  5  0 ]
  ! Band storage (LDAB=2):
  ! Row 0 (superdiag): *    3    5    *
  ! Row 1 (diagonal):  1    4    *    *
  m = 2
  n = 4
  kl = 0
  ku = 1
  ldab = kl + ku + 1
  ab = 0.0d0
  ! Column 1: diag only
  ab(2) = 1.0d0   ! A(1,1)
  ! Column 2: superdiag + diag
  ab(3) = 3.0d0   ! A(1,2) superdiag
  ab(4) = 4.0d0   ! A(2,2) diag
  ! Column 3: superdiag only
  ab(5) = 5.0d0   ! A(2,3)
  ! Column 4: no valid entries
  call dgbequ(m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd, amax, info)
  call begin_test('nonsquare')
  call print_array('r', r, m)
  call print_array('c', c, n)
  call print_scalar('rowcnd', rowcnd)
  call print_scalar('colcnd', colcnd)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

  ! Test 9: 1x1 matrix
  m = 1
  n = 1
  kl = 0
  ku = 0
  ldab = 1
  ab(1) = 7.0d0
  call dgbequ(m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd, amax, info)
  call begin_test('one_by_one')
  call print_array('r', r, m)
  call print_array('c', c, n)
  call print_scalar('rowcnd', rowcnd)
  call print_scalar('colcnd', colcnd)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

end program
