program test_zstein
  use test_utils
  implicit none

  integer, parameter :: NMAX = 5
  double precision :: d(NMAX), e(NMAX-1), w(NMAX)
  complex*16 :: Z(NMAX, NMAX)
  double precision :: Z_r(2*NMAX*NMAX)
  equivalence (Z, Z_r)
  double precision :: work(5*NMAX)
  integer :: iblock(NMAX), isplit(NMAX), iwork(NMAX), ifail(NMAX), info
  integer :: i, m

  ! Test 1: 5x5 tridiagonal, compute all 5 eigenvectors
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
  w(2) = 1.0d0
  w(3) = 2.0d0
  w(4) = 3.0d0
  w(5) = 2.0d0 + sqrt(3.0d0)

  m = 5
  do i = 1, m
    iblock(i) = 1
  end do
  isplit(1) = 5

  call ZSTEIN(5, d, e, m, w, iblock, isplit, Z, 5, work, iwork, ifail, info)

  call begin_test('basic_5x5_all')
  call print_int('N', 5)
  call print_int('M', m)
  call print_int('info', info)
  call print_array('d', d, 5)
  call print_array('e', e, 4)
  call print_array('w', w, 5)
  call print_int_array('iblock', iblock, 5)
  call print_int_array('isplit', isplit, 1)
  ! Z is complex: 2*5*5 = 50 doubles (interleaved re,im for each element, column-major)
  call print_array('Z', Z_r, 2*5*m)
  call print_int_array('ifail', ifail, m)
  call end_test()

  ! Test 2: Compute only 2 eigenvectors
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

  call ZSTEIN(5, d, e, m, w, iblock, isplit, Z, 5, work, iwork, ifail, info)

  call begin_test('partial_2of5')
  call print_int('N', 5)
  call print_int('M', m)
  call print_int('info', info)
  call print_array('w', w, m)
  call print_array('Z', Z_r, 2*5*m)
  call print_int_array('ifail', ifail, m)
  call end_test()

  ! Test 3: N=1
  d(1) = 3.0d0
  w(1) = 3.0d0
  m = 1
  iblock(1) = 1
  isplit(1) = 1

  call ZSTEIN(1, d, e, m, w, iblock, isplit, Z, 1, work, iwork, ifail, info)

  call begin_test('n_equals_1')
  call print_int('N', 1)
  call print_int('M', 1)
  call print_int('info', info)
  call print_array('Z', Z_r, 2)
  call end_test()

  ! Test 4: N=0
  call ZSTEIN(0, d, e, 0, w, iblock, isplit, Z, 1, work, iwork, ifail, info)

  call begin_test('n_equals_0')
  call print_int('info', info)
  call end_test()

  ! Test 5: Two blocks
  d(1) = 4.0d0
  d(2) = 4.0d0
  d(3) = 4.0d0
  d(4) = 3.0d0
  d(5) = 3.0d0
  e(1) = 1.0d0
  e(2) = 1.0d0
  e(3) = 0.0d0
  e(4) = 0.5d0

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

  call ZSTEIN(5, d, e, m, w, iblock, isplit, Z, 5, work, iwork, ifail, info)

  call begin_test('two_blocks')
  call print_int('N', 5)
  call print_int('M', m)
  call print_int('info', info)
  call print_array('w', w, m)
  call print_int_array('iblock', iblock, m)
  call print_int_array('isplit', isplit, 2)
  call print_array('Z', Z_r, 2*5*m)
  call print_int_array('ifail', ifail, m)
  call end_test()

end program
