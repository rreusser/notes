program test_dgttrs
  use test_utils
  implicit none
  double precision :: dl(10), d(10), du(10), du2(10), b(10, 4)
  integer :: ipiv(10), info, n, nrhs

  ! ============================================================
  ! Test 1: 5x5 no-transpose, single RHS
  ! A = [2  -1   0   0   0]
  !     [-1   2  -1   0   0]
  !     [ 0  -1   2  -1   0]
  !     [ 0   0  -1   2  -1]
  !     [ 0   0   0  -1   2]
  n = 5
  nrhs = 1
  dl(1) = -1.0d0; dl(2) = -1.0d0; dl(3) = -1.0d0; dl(4) = -1.0d0
  d(1) = 2.0d0; d(2) = 2.0d0; d(3) = 2.0d0; d(4) = 2.0d0; d(5) = 2.0d0
  du(1) = -1.0d0; du(2) = -1.0d0; du(3) = -1.0d0; du(4) = -1.0d0
  ! b = [1, 0, 0, 0, 1]
  b(1,1) = 1.0d0; b(2,1) = 0.0d0; b(3,1) = 0.0d0; b(4,1) = 0.0d0; b(5,1) = 1.0d0

  call DGTTRF(n, dl, d, du, du2, ipiv, info)
  call DGTTRS('N', n, nrhs, dl, d, du, du2, ipiv, b, n, info)
  call begin_test('notrans_single_rhs')
  call print_matrix('B', b, 10, n, nrhs)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 2: 5x5 transpose, single RHS
  dl(1) = -1.0d0; dl(2) = -1.0d0; dl(3) = -1.0d0; dl(4) = -1.0d0
  d(1) = 2.0d0; d(2) = 2.0d0; d(3) = 2.0d0; d(4) = 2.0d0; d(5) = 2.0d0
  du(1) = -1.0d0; du(2) = -1.0d0; du(3) = -1.0d0; du(4) = -1.0d0
  b(1,1) = 1.0d0; b(2,1) = 0.0d0; b(3,1) = 0.0d0; b(4,1) = 0.0d0; b(5,1) = 1.0d0

  call DGTTRF(n, dl, d, du, du2, ipiv, info)
  call DGTTRS('T', n, nrhs, dl, d, du, du2, ipiv, b, n, info)
  call begin_test('trans_single_rhs')
  call print_matrix('B', b, 10, n, nrhs)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 3: 5x5 no-transpose, multiple RHS (3 columns)
  dl(1) = -1.0d0; dl(2) = -1.0d0; dl(3) = -1.0d0; dl(4) = -1.0d0
  d(1) = 2.0d0; d(2) = 2.0d0; d(3) = 2.0d0; d(4) = 2.0d0; d(5) = 2.0d0
  du(1) = -1.0d0; du(2) = -1.0d0; du(3) = -1.0d0; du(4) = -1.0d0
  nrhs = 3
  ! Column 1: [1,0,0,0,1]
  b(1,1) = 1.0d0; b(2,1) = 0.0d0; b(3,1) = 0.0d0; b(4,1) = 0.0d0; b(5,1) = 1.0d0
  ! Column 2: [1,1,1,1,1]
  b(1,2) = 1.0d0; b(2,2) = 1.0d0; b(3,2) = 1.0d0; b(4,2) = 1.0d0; b(5,2) = 1.0d0
  ! Column 3: [0,0,1,0,0]
  b(1,3) = 0.0d0; b(2,3) = 0.0d0; b(3,3) = 1.0d0; b(4,3) = 0.0d0; b(5,3) = 0.0d0

  call DGTTRF(n, dl, d, du, du2, ipiv, info)
  call DGTTRS('N', n, nrhs, dl, d, du, du2, ipiv, b, 10, info)
  call begin_test('notrans_multi_rhs')
  call print_matrix('B', b, 10, n, nrhs)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 4: 5x5 transpose, multiple RHS (3 columns)
  dl(1) = -1.0d0; dl(2) = -1.0d0; dl(3) = -1.0d0; dl(4) = -1.0d0
  d(1) = 2.0d0; d(2) = 2.0d0; d(3) = 2.0d0; d(4) = 2.0d0; d(5) = 2.0d0
  du(1) = -1.0d0; du(2) = -1.0d0; du(3) = -1.0d0; du(4) = -1.0d0
  nrhs = 3
  b(1,1) = 1.0d0; b(2,1) = 0.0d0; b(3,1) = 0.0d0; b(4,1) = 0.0d0; b(5,1) = 1.0d0
  b(1,2) = 1.0d0; b(2,2) = 1.0d0; b(3,2) = 1.0d0; b(4,2) = 1.0d0; b(5,2) = 1.0d0
  b(1,3) = 0.0d0; b(2,3) = 0.0d0; b(3,3) = 1.0d0; b(4,3) = 0.0d0; b(5,3) = 0.0d0

  call DGTTRF(n, dl, d, du, du2, ipiv, info)
  call DGTTRS('T', n, nrhs, dl, d, du, du2, ipiv, b, 10, info)
  call begin_test('trans_multi_rhs')
  call print_matrix('B', b, 10, n, nrhs)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 5: N=1
  n = 1
  nrhs = 1
  d(1) = 5.0d0
  b(1,1) = 10.0d0
  call DGTTRF(n, dl, d, du, du2, ipiv, info)
  call DGTTRS('N', n, nrhs, dl, d, du, du2, ipiv, b, 1, info)
  call begin_test('n_one')
  call print_matrix('B', b, 10, n, nrhs)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 6: N=0 quick return
  n = 0
  nrhs = 1
  call DGTTRS('N', n, nrhs, dl, d, du, du2, ipiv, b, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 7: 5x5 with pivoting forced, no-transpose
  n = 5
  nrhs = 1
  dl(1) = 10.0d0; dl(2) = 10.0d0; dl(3) = 10.0d0; dl(4) = 10.0d0
  d(1) = 1.0d0; d(2) = 1.0d0; d(3) = 1.0d0; d(4) = 1.0d0; d(5) = 1.0d0
  du(1) = 2.0d0; du(2) = 2.0d0; du(3) = 2.0d0; du(4) = 2.0d0
  b(1,1) = 3.0d0; b(2,1) = 13.0d0; b(3,1) = 13.0d0; b(4,1) = 13.0d0; b(5,1) = 11.0d0

  call DGTTRF(n, dl, d, du, du2, ipiv, info)
  call DGTTRS('N', n, nrhs, dl, d, du, du2, ipiv, b, n, info)
  call begin_test('pivot_notrans')
  call print_matrix('B', b, 10, n, nrhs)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 8: 5x5 with pivoting forced, transpose
  dl(1) = 10.0d0; dl(2) = 10.0d0; dl(3) = 10.0d0; dl(4) = 10.0d0
  d(1) = 1.0d0; d(2) = 1.0d0; d(3) = 1.0d0; d(4) = 1.0d0; d(5) = 1.0d0
  du(1) = 2.0d0; du(2) = 2.0d0; du(3) = 2.0d0; du(4) = 2.0d0
  b(1,1) = 3.0d0; b(2,1) = 13.0d0; b(3,1) = 13.0d0; b(4,1) = 13.0d0; b(5,1) = 11.0d0

  call DGTTRF(n, dl, d, du, du2, ipiv, info)
  call DGTTRS('T', n, nrhs, dl, d, du, du2, ipiv, b, n, info)
  call begin_test('pivot_trans')
  call print_matrix('B', b, 10, n, nrhs)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 9: N=2, no-transpose
  n = 2
  nrhs = 1
  dl(1) = 3.0d0
  d(1) = 4.0d0; d(2) = 7.0d0
  du(1) = 1.0d0
  b(1,1) = 5.0d0; b(2,1) = 10.0d0

  call DGTTRF(n, dl, d, du, du2, ipiv, info)
  call DGTTRS('N', n, nrhs, dl, d, du, du2, ipiv, b, n, info)
  call begin_test('n_two_notrans')
  call print_matrix('B', b, 10, n, nrhs)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 10: N=2, transpose
  dl(1) = 3.0d0
  d(1) = 4.0d0; d(2) = 7.0d0
  du(1) = 1.0d0
  b(1,1) = 5.0d0; b(2,1) = 10.0d0

  call DGTTRF(n, dl, d, du, du2, ipiv, info)
  call DGTTRS('T', n, nrhs, dl, d, du, du2, ipiv, b, n, info)
  call begin_test('n_two_trans')
  call print_matrix('B', b, 10, n, nrhs)
  call print_int('info', info)
  call end_test()

end program
