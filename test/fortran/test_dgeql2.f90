program test_dgeql2
  use test_utils
  implicit none

  double precision :: A(4, 4), TAU(4), WORK(4)
  integer :: INFO

  ! Test 1: 3x2 matrix
  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0
  A(2,1) = 3.0d0; A(2,2) = 4.0d0
  A(3,1) = 5.0d0; A(3,2) = 6.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGEQL2(3, 2, A, 4, TAU, WORK, INFO)
  call begin_test('3x2')
  call print_matrix('A', A, 4, 3, 2)
  call print_array('TAU', TAU, 2)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 2: 2x2 matrix
  A = 0.0d0
  A(1,1) = 4.0d0; A(1,2) = 1.0d0
  A(2,1) = 3.0d0; A(2,2) = 2.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGEQL2(2, 2, A, 4, TAU, WORK, INFO)
  call begin_test('2x2')
  call print_matrix('A', A, 4, 2, 2)
  call print_array('TAU', TAU, 2)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 3: N=0
  A = 0.0d0
  A(1,1) = 1.0d0; A(2,1) = 2.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGEQL2(2, 0, A, 4, TAU, WORK, INFO)
  call begin_test('n_zero')
  call print_int('INFO', INFO)
  call end_test()

  ! Test 4: M=0
  TAU = 0.0d0; WORK = 0.0d0
  call DGEQL2(0, 2, A, 4, TAU, WORK, INFO)
  call begin_test('m_zero')
  call print_int('INFO', INFO)
  call end_test()

  ! Test 5: 4x3 matrix
  A = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0
  A(3,1) = 3.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0
  A(4,1) = 1.0d0; A(4,2) = 3.0d0; A(4,3) = 1.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGEQL2(4, 3, A, 4, TAU, WORK, INFO)
  call begin_test('4x3')
  call print_matrix('A', A, 4, 4, 3)
  call print_array('TAU', TAU, 3)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 6: 2x3 matrix (M < N, wide)
  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 3.0d0
  A(2,1) = 4.0d0; A(2,2) = 5.0d0; A(2,3) = 6.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGEQL2(2, 3, A, 4, TAU, WORK, INFO)
  call begin_test('2x3')
  call print_matrix('A', A, 4, 2, 3)
  call print_array('TAU', TAU, 2)
  call print_int('INFO', INFO)
  call end_test()

end program
