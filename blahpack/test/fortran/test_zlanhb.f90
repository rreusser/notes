program test_zlanhb
  use test_utils
  implicit none
  complex*16 :: ab(4, 5)
  double precision :: work(10)
  double precision :: result
  double precision :: zlanhb
  external :: zlanhb
  integer :: n, k, ldab

  ! ============================================================
  ! Test matrix: 5x5 Hermitian band matrix with K=2
  !
  ! Full Hermitian matrix A (upper storage):
  !   [ 1        (-4+2i)  (7-1i)   0        0       ]
  !   [ (-4-2i)   5       (-8+3i)  (6-2i)   0       ]
  !   [ (7+1i)   (-8-3i)   9       (-3+1i)  (2+4i)  ]
  !   [ 0        (6+2i)   (-3-1i)   4       (-1+3i) ]
  !   [ 0         0       (2-4i)   (-1-3i)   3      ]
  !
  ! Diagonal is real (Hermitian property).
  !
  ! Upper band storage (LDAB=K+1=3, K=2):
  !   row 1 (superdiag-2): AB(1,j) = A(j-2, j)
  !   row 2 (superdiag-1): AB(2,j) = A(j-1, j)
  !   row 3 (diagonal):    AB(3,j) = A(j, j)
  !
  !   AB (3 x 5, 1-indexed):
  !   col:   1            2            3            4            5
  !   [ *            *            (7-1i)       (6-2i)       (2+4i) ]  <- superdiag 2
  !   [ *            (-4+2i)      (-8+3i)      (-3+1i)      (-1+3i)]  <- superdiag 1
  !   [ (1,0)        (5,0)        (9,0)        (4,0)        (3,0)  ]  <- diagonal
  ! ============================================================
  n = 5
  k = 2
  ldab = 4  ! >= k+1 = 3

  ab = (0.0d0, 0.0d0)
  ! Column 1: diagonal only
  ab(3, 1) = (1.0d0, 0.0d0)
  ! Column 2: superdiag-1 and diagonal
  ab(2, 2) = (-4.0d0, 2.0d0)
  ab(3, 2) = (5.0d0, 0.0d0)
  ! Column 3: superdiag-2, superdiag-1, diagonal
  ab(1, 3) = (7.0d0, -1.0d0)
  ab(2, 3) = (-8.0d0, 3.0d0)
  ab(3, 3) = (9.0d0, 0.0d0)
  ! Column 4
  ab(1, 4) = (6.0d0, -2.0d0)
  ab(2, 4) = (-3.0d0, 1.0d0)
  ab(3, 4) = (4.0d0, 0.0d0)
  ! Column 5
  ab(1, 5) = (2.0d0, 4.0d0)
  ab(2, 5) = (-1.0d0, 3.0d0)
  ab(3, 5) = (3.0d0, 0.0d0)

  ! Test 1: upper, max norm
  result = zlanhb('M', 'U', n, k, ab, ldab, work)
  call begin_test('upper_max')
  call print_scalar('result', result)
  call end_test()

  ! Test 2: upper, one-norm
  result = zlanhb('1', 'U', n, k, ab, ldab, work)
  call begin_test('upper_one')
  call print_scalar('result', result)
  call end_test()

  ! Test 3: upper, inf-norm
  result = zlanhb('I', 'U', n, k, ab, ldab, work)
  call begin_test('upper_inf')
  call print_scalar('result', result)
  call end_test()

  ! Test 4: upper, frobenius norm
  result = zlanhb('F', 'U', n, k, ab, ldab, work)
  call begin_test('upper_frob')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! Test matrix: 5x5 Hermitian band matrix with K=2 (lower storage)
  !
  ! Full Hermitian matrix A (lower storage):
  !   [ 2        (-3-1i)  (1+2i)   0        0       ]
  !   [ (-3+1i)   6       (-5+3i)  (7-1i)   0       ]
  !   [ (1-2i)   (-5-3i)   8       (-2+4i)  (-4+1i) ]
  !   [ 0        (7+1i)   (-2-4i)   3       (1+2i)  ]
  !   [ 0         0       (-4-1i)  (1-2i)    5      ]
  !
  ! Lower band storage (LDAB=K+1=3, K=2):
  !   row 1 (diagonal):    AB(1,j) = A(j, j)
  !   row 2 (subdiag-1):   AB(2,j) = A(j+1, j)
  !   row 3 (subdiag-2):   AB(3,j) = A(j+2, j)
  !
  !   AB (3 x 5, 1-indexed):
  !   col:   1            2            3            4            5
  !   [ (2,0)        (6,0)        (8,0)        (3,0)        (5,0)  ]  <- diagonal
  !   [ (-3+1i)      (-5-3i)      (-2-4i)      (1-2i)       *     ]  <- subdiag 1
  !   [ (1-2i)       (7+1i)       (-4-1i)      *            *     ]  <- subdiag 2
  ! ============================================================
  ab = (0.0d0, 0.0d0)
  ! Column 1
  ab(1, 1) = (2.0d0, 0.0d0)
  ab(2, 1) = (-3.0d0, 1.0d0)
  ab(3, 1) = (1.0d0, -2.0d0)
  ! Column 2
  ab(1, 2) = (6.0d0, 0.0d0)
  ab(2, 2) = (-5.0d0, -3.0d0)
  ab(3, 2) = (7.0d0, 1.0d0)
  ! Column 3
  ab(1, 3) = (8.0d0, 0.0d0)
  ab(2, 3) = (-2.0d0, -4.0d0)
  ab(3, 3) = (-4.0d0, -1.0d0)
  ! Column 4
  ab(1, 4) = (3.0d0, 0.0d0)
  ab(2, 4) = (1.0d0, -2.0d0)
  ! Column 5
  ab(1, 5) = (5.0d0, 0.0d0)

  ! Test 5: lower, max norm
  result = zlanhb('M', 'L', n, k, ab, ldab, work)
  call begin_test('lower_max')
  call print_scalar('result', result)
  call end_test()

  ! Test 6: lower, one-norm
  result = zlanhb('1', 'L', n, k, ab, ldab, work)
  call begin_test('lower_one')
  call print_scalar('result', result)
  call end_test()

  ! Test 7: lower, inf-norm
  result = zlanhb('I', 'L', n, k, ab, ldab, work)
  call begin_test('lower_inf')
  call print_scalar('result', result)
  call end_test()

  ! Test 8: lower, frobenius norm
  result = zlanhb('F', 'L', n, k, ab, ldab, work)
  call begin_test('lower_frob')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! Edge case: N=0
  ! ============================================================
  result = zlanhb('M', 'U', 0, 2, ab, ldab, work)
  call begin_test('edge_n0')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! Edge case: 1x1 matrix, K=0
  ! Diagonal element (5, 0) -- real diagonal
  ! ============================================================
  ab = (0.0d0, 0.0d0)
  ab(1, 1) = (5.0d0, 0.0d0)

  result = zlanhb('M', 'U', 1, 0, ab, ldab, work)
  call begin_test('edge_1x1_max')
  call print_scalar('result', result)
  call end_test()

  result = zlanhb('1', 'U', 1, 0, ab, ldab, work)
  call begin_test('edge_1x1_one')
  call print_scalar('result', result)
  call end_test()

  result = zlanhb('I', 'U', 1, 0, ab, ldab, work)
  call begin_test('edge_1x1_inf')
  call print_scalar('result', result)
  call end_test()

  result = zlanhb('F', 'U', 1, 0, ab, ldab, work)
  call begin_test('edge_1x1_frob')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! K=0 (diagonal matrix only), N=4
  ! Diagonal entries are real for Hermitian
  ! ============================================================
  ab = (0.0d0, 0.0d0)
  ab(1, 1) = (3.0d0, 0.0d0)
  ab(1, 2) = (-7.0d0, 0.0d0)
  ab(1, 3) = (2.0d0, 0.0d0)
  ab(1, 4) = (-4.0d0, 0.0d0)

  result = zlanhb('M', 'U', 4, 0, ab, ldab, work)
  call begin_test('diag_k0_upper_max')
  call print_scalar('result', result)
  call end_test()

  result = zlanhb('1', 'U', 4, 0, ab, ldab, work)
  call begin_test('diag_k0_upper_one')
  call print_scalar('result', result)
  call end_test()

  result = zlanhb('I', 'U', 4, 0, ab, ldab, work)
  call begin_test('diag_k0_upper_inf')
  call print_scalar('result', result)
  call end_test()

  result = zlanhb('F', 'U', 4, 0, ab, ldab, work)
  call begin_test('diag_k0_upper_frob')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! K=1 bandwidth, N=4, upper Hermitian
  !
  ! Full Hermitian matrix A:
  !   [ 2        (-3+1i)   0        0       ]
  !   [ (-3-1i)   4        (1+2i)   0       ]
  !   [ 0        (1-2i)   -5        (6-3i)  ]
  !   [ 0         0       (6+3i)    7       ]
  !
  ! Upper band storage (LDAB=2, K=1):
  !   row 1 (superdiag): AB(1,j) = A(j-1,j)
  !   row 2 (diagonal):  AB(2,j) = A(j,j)
  !
  !   AB (2 x 4, 1-indexed):
  !   col:  1          2          3          4
  !   [ *          (-3+1i)    (1+2i)     (6-3i) ]   <- superdiag
  !   [ (2,0)      (4,0)      (-5,0)     (7,0)  ]   <- diagonal
  ! ============================================================
  ab = (0.0d0, 0.0d0)
  ab(2, 1) = (2.0d0, 0.0d0)
  ab(1, 2) = (-3.0d0, 1.0d0)
  ab(2, 2) = (4.0d0, 0.0d0)
  ab(1, 3) = (1.0d0, 2.0d0)
  ab(2, 3) = (-5.0d0, 0.0d0)
  ab(1, 4) = (6.0d0, -3.0d0)
  ab(2, 4) = (7.0d0, 0.0d0)

  result = zlanhb('M', 'U', 4, 1, ab, ldab, work)
  call begin_test('upper_k1_max')
  call print_scalar('result', result)
  call end_test()

  result = zlanhb('1', 'U', 4, 1, ab, ldab, work)
  call begin_test('upper_k1_one')
  call print_scalar('result', result)
  call end_test()

  result = zlanhb('I', 'U', 4, 1, ab, ldab, work)
  call begin_test('upper_k1_inf')
  call print_scalar('result', result)
  call end_test()

  result = zlanhb('F', 'U', 4, 1, ab, ldab, work)
  call begin_test('upper_k1_frob')
  call print_scalar('result', result)
  call end_test()

  ! ============================================================
  ! K=1 bandwidth, N=4, lower Hermitian
  !
  ! Same full Hermitian matrix as above but in lower storage:
  !
  ! Lower band storage (LDAB=2, K=1):
  !   row 1 (diagonal):  AB(1,j) = A(j,j)
  !   row 2 (subdiag):   AB(2,j) = A(j+1,j)
  !
  !   AB (2 x 4, 1-indexed):
  !   col:  1          2          3          4
  !   [ (2,0)      (4,0)      (-5,0)     (7,0)  ]   <- diagonal
  !   [ (-3-1i)    (1-2i)     (6+3i)     *      ]   <- subdiag
  ! ============================================================
  ab = (0.0d0, 0.0d0)
  ab(1, 1) = (2.0d0, 0.0d0)
  ab(2, 1) = (-3.0d0, -1.0d0)
  ab(1, 2) = (4.0d0, 0.0d0)
  ab(2, 2) = (1.0d0, -2.0d0)
  ab(1, 3) = (-5.0d0, 0.0d0)
  ab(2, 3) = (6.0d0, 3.0d0)
  ab(1, 4) = (7.0d0, 0.0d0)

  result = zlanhb('M', 'L', 4, 1, ab, ldab, work)
  call begin_test('lower_k1_max')
  call print_scalar('result', result)
  call end_test()

  result = zlanhb('1', 'L', 4, 1, ab, ldab, work)
  call begin_test('lower_k1_one')
  call print_scalar('result', result)
  call end_test()

  result = zlanhb('I', 'L', 4, 1, ab, ldab, work)
  call begin_test('lower_k1_inf')
  call print_scalar('result', result)
  call end_test()

  result = zlanhb('F', 'L', 4, 1, ab, ldab, work)
  call begin_test('lower_k1_frob')
  call print_scalar('result', result)
  call end_test()

end program
