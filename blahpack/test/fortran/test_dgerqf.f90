program test_dgerqf
  use test_utils
  implicit none

  double precision :: A(6, 6), TAU(6), WORK(6000)
  integer :: info, i, j

  integer, parameter :: BIG = 40
  double precision :: ABIG(BIG, BIG), TAUBIG(BIG), WBIG(100000)

  ! Test 1: 3x4 (M < N)
  A = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0; A(1,4) = 1.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0; A(2,4) = 3.0d0
  A(3,1) = 3.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0; A(3,4) = 2.0d0
  call dgerqf(3, 4, A, 6, TAU, WORK, 6000, info)
  call begin_test('3x4')
  call print_matrix('A', A, 6, 3, 4)
  call print_array('TAU', TAU, 3)
  call print_int('INFO', info)
  call end_test()

  ! Test 2: 4x3 (M > N)
  A = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0
  A(3,1) = 3.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0
  A(4,1) = 1.0d0; A(4,2) = 3.0d0; A(4,3) = 1.0d0
  call dgerqf(4, 3, A, 6, TAU, WORK, 6000, info)
  call begin_test('4x3')
  call print_matrix('A', A, 6, 4, 3)
  call print_array('TAU', TAU, 3)
  call print_int('INFO', info)
  call end_test()

  ! Test 3: 3x3 (square)
  A = 0.0d0
  A(1,1) = 4.0d0; A(1,2) = 1.0d0; A(1,3) = 2.0d0
  A(2,1) = 1.0d0; A(2,2) = 3.0d0; A(2,3) = 1.0d0
  A(3,1) = 2.0d0; A(3,2) = 1.0d0; A(3,3) = 5.0d0
  call dgerqf(3, 3, A, 6, TAU, WORK, 6000, info)
  call begin_test('3x3')
  call print_matrix('A', A, 6, 3, 3)
  call print_array('TAU', TAU, 3)
  call print_int('INFO', info)
  call end_test()

  ! Test 4: 1x1
  A = 0.0d0
  A(1,1) = 7.0d0
  call dgerqf(1, 1, A, 6, TAU, WORK, 6000, info)
  call begin_test('1x1')
  call print_matrix('A', A, 6, 1, 1)
  call print_array('TAU', TAU, 1)
  call print_int('INFO', info)
  call end_test()

  ! Test 5: M=0
  call dgerqf(0, 3, A, 6, TAU, WORK, 6000, info)
  call begin_test('m_zero')
  call print_int('INFO', info)
  call end_test()

  ! Test 6: N=0
  call dgerqf(3, 0, A, 6, TAU, WORK, 6000, info)
  call begin_test('n_zero')
  call print_int('INFO', info)
  call end_test()

  ! Test 7: 2x5 (wide)
  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 3.0d0; A(1,4) = 4.0d0; A(1,5) = 5.0d0
  A(2,1) = 6.0d0; A(2,2) = 7.0d0; A(2,3) = 8.0d0; A(2,4) = 9.0d0; A(2,5) = 10.0d0
  call dgerqf(2, 5, A, 6, TAU, WORK, 6000, info)
  call begin_test('2x5')
  call print_matrix('A', A, 6, 2, 5)
  call print_array('TAU', TAU, 2)
  call print_int('INFO', info)
  call end_test()

  ! Test 8: Large 40x40 matrix (triggers blocked path)
  do j = 1, BIG
    do i = 1, BIG
      ABIG(i, j) = dble(mod(i*7 + j*13, 97)) / 97.0d0
    end do
  end do
  call dgerqf(BIG, BIG, ABIG, BIG, TAUBIG, WBIG, 100000, info)
  call begin_test('40x40')
  call print_matrix('A', ABIG, BIG, BIG, BIG)
  call print_array('TAU', TAUBIG, BIG)
  call print_int('INFO', info)
  call end_test()

  ! Test 9: 40x35 (M > N, blocked)
  do j = 1, 35
    do i = 1, BIG
      ABIG(i, j) = dble(mod(i*11 + j*7, 101)) / 101.0d0
    end do
  end do
  call dgerqf(BIG, 35, ABIG, BIG, TAUBIG, WBIG, 100000, info)
  call begin_test('40x35')
  call print_matrix('A', ABIG, BIG, BIG, 35)
  call print_array('TAU', TAUBIG, 35)
  call print_int('INFO', info)
  call end_test()

end program
