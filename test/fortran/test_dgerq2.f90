program test_dgerq2
  use test_utils
  implicit none

  double precision :: A(6, 6), TAU(6), WORK(6)
  integer :: INFO

  ! Test 1: 3x4 matrix (M < N)
  A = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0; A(1,4) = 1.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0; A(2,4) = 3.0d0
  A(3,1) = 3.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0; A(3,4) = 2.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGERQ2(3, 4, A, 6, TAU, WORK, INFO)
  call begin_test('3x4')
  call print_matrix('A', A, 6, 3, 4)
  call print_array('TAU', TAU, 3)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 2: 4x3 matrix (M > N)
  A = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0
  A(3,1) = 3.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0
  A(4,1) = 1.0d0; A(4,2) = 3.0d0; A(4,3) = 1.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGERQ2(4, 3, A, 6, TAU, WORK, INFO)
  call begin_test('4x3')
  call print_matrix('A', A, 6, 4, 3)
  call print_array('TAU', TAU, 3)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 3: 3x3 square matrix
  A = 0.0d0
  A(1,1) = 4.0d0; A(1,2) = 1.0d0; A(1,3) = 2.0d0
  A(2,1) = 1.0d0; A(2,2) = 3.0d0; A(2,3) = 1.0d0
  A(3,1) = 2.0d0; A(3,2) = 1.0d0; A(3,3) = 5.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGERQ2(3, 3, A, 6, TAU, WORK, INFO)
  call begin_test('3x3')
  call print_matrix('A', A, 6, 3, 3)
  call print_array('TAU', TAU, 3)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 4: 1x4 (single row)
  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 3.0d0; A(1,4) = 4.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGERQ2(1, 4, A, 6, TAU, WORK, INFO)
  call begin_test('1x4')
  call print_matrix('A', A, 6, 1, 4)
  call print_array('TAU', TAU, 1)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 5: 3x1 (single column)
  A = 0.0d0
  A(1,1) = 2.0d0
  A(2,1) = 3.0d0
  A(3,1) = 4.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGERQ2(3, 1, A, 6, TAU, WORK, INFO)
  call begin_test('3x1')
  call print_matrix('A', A, 6, 3, 1)
  call print_array('TAU', TAU, 1)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 6: M=0 (quick return)
  TAU = 0.0d0; WORK = 0.0d0
  call DGERQ2(0, 3, A, 6, TAU, WORK, INFO)
  call begin_test('m_zero')
  call print_int('INFO', INFO)
  call end_test()

  ! Test 7: N=0 (quick return)
  TAU = 0.0d0; WORK = 0.0d0
  call DGERQ2(3, 0, A, 6, TAU, WORK, INFO)
  call begin_test('n_zero')
  call print_int('INFO', INFO)
  call end_test()

  ! Test 8: 1x1 matrix
  A = 0.0d0
  A(1,1) = 7.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGERQ2(1, 1, A, 6, TAU, WORK, INFO)
  call begin_test('1x1')
  call print_matrix('A', A, 6, 1, 1)
  call print_array('TAU', TAU, 1)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 9: 2x5 matrix (wide, M << N)
  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 3.0d0; A(1,4) = 4.0d0; A(1,5) = 5.0d0
  A(2,1) = 6.0d0; A(2,2) = 7.0d0; A(2,3) = 8.0d0; A(2,4) = 9.0d0; A(2,5) = 10.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGERQ2(2, 5, A, 6, TAU, WORK, INFO)
  call begin_test('2x5')
  call print_matrix('A', A, 6, 2, 5)
  call print_array('TAU', TAU, 2)
  call print_int('INFO', INFO)
  call end_test()

end program
