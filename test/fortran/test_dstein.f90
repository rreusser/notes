program test_dstein
  use test_utils
  implicit none

  integer, parameter :: NMAX = 5
  double precision :: d(NMAX), e(NMAX-1), w(NMAX), Z(NMAX, NMAX)
  double precision :: work(5*NMAX)
  integer :: iblock(NMAX), isplit(NMAX), iwork(NMAX), ifail(NMAX), info
  integer :: i, j, m

  ! Test 1: 5x5 tridiagonal, compute all 5 eigenvectors
  ! Tridiagonal: d = [2, 2, 2, 2, 2], e = [1, 1, 1, 1]
  ! Eigenvalues are 2 + 2*cos(k*pi/6) for k=1..5
  ! We use known eigenvalues computed below
  d(1) = 2.0d0
  d(2) = 2.0d0
  d(3) = 2.0d0
  d(4) = 2.0d0
  d(5) = 2.0d0
  e(1) = 1.0d0
  e(2) = 1.0d0
  e(3) = 1.0d0
  e(4) = 1.0d0

  ! Eigenvalues of this tridiagonal: 2 + 2*cos(k*pi/(N+1)) for k=1..N
  ! k=1: 2 + 2*cos(pi/6) = 2 + sqrt(3) ~ 3.7320508
  ! k=2: 2 + 2*cos(2*pi/6) = 2 + 1 = 3.0
  ! k=3: 2 + 2*cos(3*pi/6) = 2 + 0 = 2.0
  ! k=4: 2 + 2*cos(4*pi/6) = 2 - 1 = 1.0
  ! k=5: 2 + 2*cos(5*pi/6) = 2 - sqrt(3) ~ 0.2679492
  ! Sorted ascending:
  w(1) = 2.0d0 - sqrt(3.0d0)
  w(2) = 1.0d0
  w(3) = 2.0d0
  w(4) = 3.0d0
  w(5) = 2.0d0 + sqrt(3.0d0)

  m = 5
  ! Single block
  do i = 1, m
    iblock(i) = 1
  end do
  isplit(1) = 5

  call DSTEIN(5, d, e, m, w, iblock, isplit, Z, 5, work, iwork, ifail, info)

  call begin_test('basic_5x5_all')
  call print_int('N', 5)
  call print_int('M', m)
  call print_int('info', info)
  call print_array('d', d, 5)
  call print_array('e', e, 4)
  call print_array('w', w, 5)
  call print_int_array('iblock', iblock, 5)
  call print_int_array('isplit', isplit, 1)
  ! Print Z column-major as flat array
  call print_matrix('Z', Z, 5, 5, m)
  call print_int_array('ifail', ifail, m)
  call end_test()

  ! Test 2: Compute only 2 eigenvectors (1st and 3rd eigenvalues)
  d(1) = 2.0d0
  d(2) = 2.0d0
  d(3) = 2.0d0
  d(4) = 2.0d0
  d(5) = 2.0d0
  e(1) = 1.0d0
  e(2) = 1.0d0
  e(3) = 1.0d0
  e(4) = 1.0d0

  w(1) = 2.0d0 - sqrt(3.0d0)
  w(2) = 2.0d0
  m = 2
  iblock(1) = 1
  iblock(2) = 1
  isplit(1) = 5

  call DSTEIN(5, d, e, m, w, iblock, isplit, Z, 5, work, iwork, ifail, info)

  call begin_test('partial_2of5')
  call print_int('N', 5)
  call print_int('M', m)
  call print_int('info', info)
  call print_array('w', w, m)
  call print_matrix('Z', Z, 5, 5, m)
  call print_int_array('ifail', ifail, m)
  call end_test()

  ! Test 3: N=1 edge case
  d(1) = 3.0d0
  w(1) = 3.0d0
  m = 1
  iblock(1) = 1
  isplit(1) = 1

  call DSTEIN(1, d, e, m, w, iblock, isplit, Z, 1, work, iwork, ifail, info)

  call begin_test('n_equals_1')
  call print_int('N', 1)
  call print_int('M', 1)
  call print_int('info', info)
  call print_matrix('Z', Z, 1, 1, 1)
  call end_test()

  ! Test 4: N=0 edge case
  call DSTEIN(0, d, e, 0, w, iblock, isplit, Z, 1, work, iwork, ifail, info)

  call begin_test('n_equals_0')
  call print_int('info', info)
  call end_test()

  ! Test 5: Two blocks - matrix splits into [3x3] and [2x2]
  ! Block 1: d=[4,4,4], e=[1,1] -> eigenvalues 4+sqrt(2), 4, 4-sqrt(2)
  ! Block 2: d=[3,3], e=[0.5] -> eigenvalues 3.5, 2.5
  d(1) = 4.0d0
  d(2) = 4.0d0
  d(3) = 4.0d0
  d(4) = 3.0d0
  d(5) = 3.0d0
  e(1) = 1.0d0
  e(2) = 1.0d0
  e(3) = 0.0d0  ! This causes a split
  e(4) = 0.5d0

  ! Eigenvalues sorted within each block
  ! Block 1: 4-sqrt(2), 4, 4+sqrt(2)
  ! Block 2: 2.5, 3.5
  w(1) = 4.0d0 - sqrt(2.0d0)
  w(2) = 4.0d0
  w(3) = 4.0d0 + sqrt(2.0d0)
  w(4) = 2.5d0
  w(5) = 3.5d0

  m = 5
  iblock(1) = 1
  iblock(2) = 1
  iblock(3) = 1
  iblock(4) = 2
  iblock(5) = 2
  isplit(1) = 3
  isplit(2) = 5

  call DSTEIN(5, d, e, m, w, iblock, isplit, Z, 5, work, iwork, ifail, info)

  call begin_test('two_blocks')
  call print_int('N', 5)
  call print_int('M', m)
  call print_int('info', info)
  call print_array('d_orig', (/ 4.0d0, 4.0d0, 4.0d0, 3.0d0, 3.0d0 /), 5)
  call print_array('e_orig', (/ 1.0d0, 1.0d0, 0.0d0, 0.5d0 /), 4)
  call print_array('w', w, m)
  call print_int_array('iblock', iblock, m)
  call print_int_array('isplit', isplit, 2)
  call print_matrix('Z', Z, 5, 5, m)
  call print_int_array('ifail', ifail, m)
  call end_test()

end program
