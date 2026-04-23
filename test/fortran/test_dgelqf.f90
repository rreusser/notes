program test_dgelqf
  use test_utils
  implicit none

  double precision :: A(50, 50), TAU(50), WORK(2000)
  integer :: INFO, LWORK, i, j

  LWORK = 2000

  ! Test 1: 3x5 matrix (M < N)
  A = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0; A(1,4) = 1.0d0; A(1,5) = 4.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0; A(2,4) = 3.0d0; A(2,5) = 1.0d0
  A(3,1) = 3.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0; A(3,4) = 2.0d0; A(3,5) = 3.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGELQF(3, 5, A, 50, TAU, WORK, LWORK, INFO)
  call begin_test('3x5')
  call print_matrix('A', A, 50, 3, 5)
  call print_array('TAU', TAU, 3)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 2: 5x3 matrix (M > N)
  A = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0
  A(3,1) = 3.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0
  A(4,1) = 1.0d0; A(4,2) = 3.0d0; A(4,3) = 1.0d0
  A(5,1) = 2.0d0; A(5,2) = 1.0d0; A(5,3) = 4.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGELQF(5, 3, A, 50, TAU, WORK, LWORK, INFO)
  call begin_test('5x3')
  call print_matrix('A', A, 50, 5, 3)
  call print_array('TAU', TAU, 3)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 3: 4x4 square matrix
  A = 0.0d0
  A(1,1) = 4.0d0; A(1,2) = 1.0d0; A(1,3) = 2.0d0; A(1,4) = 1.0d0
  A(2,1) = 1.0d0; A(2,2) = 3.0d0; A(2,3) = 1.0d0; A(2,4) = 2.0d0
  A(3,1) = 2.0d0; A(3,2) = 1.0d0; A(3,3) = 5.0d0; A(3,4) = 3.0d0
  A(4,1) = 1.0d0; A(4,2) = 2.0d0; A(4,3) = 3.0d0; A(4,4) = 6.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGELQF(4, 4, A, 50, TAU, WORK, LWORK, INFO)
  call begin_test('4x4')
  call print_matrix('A', A, 50, 4, 4)
  call print_array('TAU', TAU, 4)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 4: 1x1 edge case
  A = 0.0d0
  A(1,1) = 7.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGELQF(1, 1, A, 50, TAU, WORK, LWORK, INFO)
  call begin_test('1x1')
  call print_matrix('A', A, 50, 1, 1)
  call print_array('TAU', TAU, 1)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 5: M=0 quick return
  TAU = 0.0d0; WORK = 0.0d0
  call DGELQF(0, 5, A, 50, TAU, WORK, LWORK, INFO)
  call begin_test('m_zero')
  call print_int('INFO', INFO)
  call end_test()

  ! Test 6: N=0 quick return
  TAU = 0.0d0; WORK = 0.0d0
  call DGELQF(3, 0, A, 50, TAU, WORK, LWORK, INFO)
  call begin_test('n_zero')
  call print_int('INFO', INFO)
  call end_test()

  ! Test 7: 35x40 large matrix to exercise blocked path (NB=32)
  ! Use a well-conditioned diagonally dominant matrix
  A = 0.0d0
  do i = 1, 35
    do j = 1, 40
      A(i, j) = 1.0d0 / dble(i + j)
    end do
    A(i, i) = 10.0d0 + dble(i)
  end do
  TAU = 0.0d0; WORK = 0.0d0
  call DGELQF(35, 40, A, 50, TAU, WORK, LWORK, INFO)
  call begin_test('35x40')
  call print_matrix('A', A, 50, 35, 40)
  call print_array('TAU', TAU, 35)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 8: 2x6 wide matrix (M << N)
  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 3.0d0; A(1,4) = 4.0d0; A(1,5) = 5.0d0; A(1,6) = 6.0d0
  A(2,1) = 7.0d0; A(2,2) = 8.0d0; A(2,3) = 9.0d0; A(2,4) = 10.0d0; A(2,5) = 11.0d0; A(2,6) = 12.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGELQF(2, 6, A, 50, TAU, WORK, LWORK, INFO)
  call begin_test('2x6')
  call print_matrix('A', A, 50, 2, 6)
  call print_array('TAU', TAU, 2)
  call print_int('INFO', INFO)
  call end_test()

end program
