program test_zgbequ
  use test_utils
  implicit none
  integer, parameter :: MAXAB = 200
  complex*16 :: ab(MAXAB)
  double precision :: ab_r(2*MAXAB)
  equivalence (ab, ab_r)
  double precision :: r(10), c(10)
  double precision :: rowcnd, colcnd, amax
  integer :: info, m, n, kl, ku, ldab

  ! Test 1: basic 3x3 band matrix with KL=1, KU=1 (tridiagonal)
  ! Dense matrix A:
  !   [( 2, 1) ( 1, 2)  0     ]
  !   [( 3, 0) ( 4, 1) ( 1, 1)]
  !   [ 0      ( 2, 3) ( 5, 0)]
  ! Band storage (LDAB=3, KU+1+i-j indexing):
  ! Row 0 (superdiag): *       (1,2)   (1,1)
  ! Row 1 (diagonal):  (2,1)   (4,1)   (5,0)
  ! Row 2 (subdiag):   (3,0)   (2,3)   *
  m = 3
  n = 3
  kl = 1
  ku = 1
  ldab = kl + ku + 1
  ab = (0.0d0, 0.0d0)
  ! Column 1: rows KU+1-0=2 is diag(1,1)=A(1,1), row 3 is subdiag=A(2,1)
  ab(2) = (2.0d0, 1.0d0)   ! A(1,1) at row KU+1=2
  ab(3) = (3.0d0, 0.0d0)   ! A(2,1) at row KU+1+1=3
  ! Column 2: superdiag=A(1,2) at row 1, diag=A(2,2) at row 2, subdiag=A(3,2) at row 3
  ab(4) = (1.0d0, 2.0d0)   ! A(1,2) at row KU+1+1-2=1
  ab(5) = (4.0d0, 1.0d0)   ! A(2,2) at row KU+1=2
  ab(6) = (2.0d0, 3.0d0)   ! A(3,2) at row KU+1+3-2=3
  ! Column 3: superdiag=A(2,3) at row 1, diag=A(3,3) at row 2
  ab(7) = (1.0d0, 1.0d0)   ! A(2,3) at row 1
  ab(8) = (5.0d0, 0.0d0)   ! A(3,3) at row 2
  call zgbequ(m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd, amax, info)
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
  !   [(1,1) (2,0)  0     0    ]
  !   [(3,2) (4,0) (1,3)  0    ]
  !   [(0,5) (6,1) (2,2) (3,0) ]
  !   [ 0    (1,0) (7,0) (4,4) ]
  ! Band storage (LDAB=4):
  ! Row 0 (superdiag):  *      (2,0)   (1,3)   (3,0)
  ! Row 1 (diagonal):   (1,1)  (4,0)   (2,2)   (4,4)
  ! Row 2 (subdiag1):   (3,2)  (6,1)   (7,0)   *
  ! Row 3 (subdiag2):   (0,5)  (1,0)   *       *
  m = 4
  n = 4
  kl = 2
  ku = 1
  ldab = kl + ku + 1
  ab = (0.0d0, 0.0d0)
  ! Column 1
  ab(2) = (1.0d0, 1.0d0)   ! A(1,1) diag
  ab(3) = (3.0d0, 2.0d0)   ! A(2,1) subdiag1
  ab(4) = (0.0d0, 5.0d0)   ! A(3,1) subdiag2
  ! Column 2
  ab(5) = (2.0d0, 0.0d0)   ! A(1,2) superdiag
  ab(6) = (4.0d0, 0.0d0)   ! A(2,2) diag
  ab(7) = (6.0d0, 1.0d0)   ! A(3,2) subdiag1
  ab(8) = (1.0d0, 0.0d0)   ! A(4,2) subdiag2
  ! Column 3
  ab(9) = (1.0d0, 3.0d0)    ! A(2,3) superdiag
  ab(10) = (2.0d0, 2.0d0)   ! A(3,3) diag
  ab(11) = (7.0d0, 0.0d0)   ! A(4,3) subdiag1
  ! Column 4
  ab(13) = (3.0d0, 0.0d0)   ! A(3,4) superdiag
  ab(14) = (4.0d0, 4.0d0)   ! A(4,4) diag
  call zgbequ(m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd, amax, info)
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
  !   [(1,0) (2,0)]
  !   [(0,0) (0,0)]
  !   [(0,0) (3,0)]
  ! KL=2, KU=1
  m = 3
  n = 2
  kl = 2
  ku = 1
  ldab = kl + ku + 1
  ab = (0.0d0, 0.0d0)
  ! Column 1: A(1,1)=diag row KU+1=2
  ab(2) = (1.0d0, 0.0d0)   ! A(1,1)
  ab(3) = (0.0d0, 0.0d0)   ! A(2,1)
  ab(4) = (0.0d0, 0.0d0)   ! A(3,1)
  ! Column 2: A(1,2)=superdiag row 1, A(2,2)=diag row 2, A(3,2)=subdiag row 3
  ab(5) = (2.0d0, 0.0d0)   ! A(1,2)
  ab(6) = (0.0d0, 0.0d0)   ! A(2,2)
  ab(7) = (3.0d0, 0.0d0)   ! A(3,2)
  call zgbequ(m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd, amax, info)
  call begin_test('zero_row')
  call print_int('info', info)
  call end_test()

  ! Test 4: zero column (after row scaling)
  ! Dense matrix A:
  !   [(1,0) (0,0)]
  !   [(2,0) (0,0)]
  ! KL=1, KU=0, so only diagonal and subdiagonals
  m = 2
  n = 2
  kl = 1
  ku = 0
  ldab = kl + ku + 1
  ab = (0.0d0, 0.0d0)
  ! Column 1: A(1,1) diag at row 1, A(2,1) subdiag at row 2
  ab(1) = (1.0d0, 0.0d0)   ! A(1,1)
  ab(2) = (2.0d0, 0.0d0)   ! A(2,1)
  ! Column 2: A(2,2) diag at row 1
  ab(3) = (0.0d0, 0.0d0)   ! A(1,2) — doesn't exist (j-ku=2-0=2 > i=1)
  ab(4) = (0.0d0, 0.0d0)   ! A(2,2)
  call zgbequ(m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd, amax, info)
  call begin_test('zero_col')
  call print_array('r', r, m)
  call print_int('info', info)
  call end_test()

  ! Test 5: quick return M=0
  call zgbequ(0, 3, 0, 0, ab, 1, r, c, rowcnd, colcnd, amax, info)
  call begin_test('m_zero')
  call print_scalar('rowcnd', rowcnd)
  call print_scalar('colcnd', colcnd)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

  ! Test 6: quick return N=0
  call zgbequ(3, 0, 0, 0, ab, 1, r, c, rowcnd, colcnd, amax, info)
  call begin_test('n_zero')
  call print_scalar('rowcnd', rowcnd)
  call print_scalar('colcnd', colcnd)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

  ! Test 7: diagonal only (KL=0, KU=0)
  ! Dense matrix A = diag((3,4), (1,0), (0,2))
  m = 3
  n = 3
  kl = 0
  ku = 0
  ldab = 1
  ab(1) = (3.0d0, 4.0d0)   ! A(1,1)
  ab(2) = (1.0d0, 0.0d0)   ! A(2,2)
  ab(3) = (0.0d0, 2.0d0)   ! A(3,3)
  call zgbequ(m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd, amax, info)
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
  !   [(1,1) (2,3) 0     0    ]
  !   [ 0    (4,0) (5,1) 0    ]
  ! Band storage (LDAB=2):
  ! Row 0 (superdiag): *      (2,3)   (5,1)   *
  ! Row 1 (diagonal):  (1,1)  (4,0)   *       *
  ! But note: only columns j where max(1,j-ku)<=min(m,j+kl) exist
  m = 2
  n = 4
  kl = 0
  ku = 1
  ldab = kl + ku + 1
  ab = (0.0d0, 0.0d0)
  ! Column 1: diag only
  ab(2) = (1.0d0, 1.0d0)   ! A(1,1) at row KU+1=2
  ! Column 2: superdiag + diag
  ab(3) = (2.0d0, 3.0d0)   ! A(1,2) at row 1
  ab(4) = (4.0d0, 0.0d0)   ! A(2,2) at row 2
  ! Column 3: superdiag only (diag would be row 3 which > m=2)
  ab(5) = (5.0d0, 1.0d0)   ! A(2,3) at row 1
  ! Column 4: no valid entries (max(1,4-1)=3 > min(2,4+0)=2)
  call zgbequ(m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd, amax, info)
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
  ab(1) = (7.0d0, 3.0d0)
  call zgbequ(m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd, amax, info)
  call begin_test('one_by_one')
  call print_array('r', r, m)
  call print_array('c', c, n)
  call print_scalar('rowcnd', rowcnd)
  call print_scalar('colcnd', colcnd)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

end program
