program test_dorg2l_inputs
  use test_utils
  implicit none

  double precision :: A(6, 4), TAU(4), WORK(4)
  integer :: INFO, M, N

  ! Output the QL factorization state (A and TAU after DGEQL2)
  ! so we know what to feed to DORG2L in JS tests.

  ! Test 1: 4x3
  A = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0
  A(3,1) = 3.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0
  A(4,1) = 1.0d0; A(4,2) = 3.0d0; A(4,3) = 1.0d0
  TAU = 0.0d0; WORK = 0.0d0
  M = 4; N = 3
  call DGEQL2(M, N, A, 6, TAU, WORK, INFO)
  call begin_test('4x3_ql')
  call print_matrix('A', A, 6, M, N)
  call print_array('TAU', TAU, N)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 2: 3x3
  A = 0.0d0
  A(1,1) = 4.0d0; A(1,2) = 1.0d0; A(1,3) = 2.0d0
  A(2,1) = 3.0d0; A(2,2) = 2.0d0; A(2,3) = 1.0d0
  A(3,1) = 1.0d0; A(3,2) = 5.0d0; A(3,3) = 3.0d0
  TAU = 0.0d0; WORK = 0.0d0
  M = 3; N = 3
  call DGEQL2(M, N, A, 6, TAU, WORK, INFO)
  call begin_test('3x3_ql')
  call print_matrix('A', A, 6, M, N)
  call print_array('TAU', TAU, N)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 3: 4x2
  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0
  A(2,1) = 3.0d0; A(2,2) = 4.0d0
  A(3,1) = 5.0d0; A(3,2) = 6.0d0
  A(4,1) = 7.0d0; A(4,2) = 8.0d0
  TAU = 0.0d0; WORK = 0.0d0
  M = 4; N = 2
  call DGEQL2(M, N, A, 6, TAU, WORK, INFO)
  call begin_test('4x2_ql')
  call print_matrix('A', A, 6, M, N)
  call print_array('TAU', TAU, N)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 4: 5x3
  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 1.0d0
  A(2,1) = 4.0d0; A(2,2) = 1.0d0; A(2,3) = 3.0d0
  A(3,1) = 2.0d0; A(3,2) = 3.0d0; A(3,3) = 2.0d0
  A(4,1) = 1.0d0; A(4,2) = 1.0d0; A(4,3) = 4.0d0
  A(5,1) = 3.0d0; A(5,2) = 2.0d0; A(5,3) = 1.0d0
  TAU = 0.0d0; WORK = 0.0d0
  M = 5; N = 3
  call DGEQL2(M, N, A, 6, TAU, WORK, INFO)
  call begin_test('5x3_ql')
  call print_matrix('A', A, 6, M, N)
  call print_array('TAU', TAU, N)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 5: 6x4
  A = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0; A(1,4) = 1.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0; A(2,4) = 3.0d0
  A(3,1) = 3.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0; A(3,4) = 2.0d0
  A(4,1) = 1.0d0; A(4,2) = 3.0d0; A(4,3) = 1.0d0; A(4,4) = 4.0d0
  A(5,1) = 2.0d0; A(5,2) = 1.0d0; A(5,3) = 4.0d0; A(5,4) = 1.0d0
  A(6,1) = 1.0d0; A(6,2) = 2.0d0; A(6,3) = 1.0d0; A(6,4) = 3.0d0
  TAU = 0.0d0; WORK = 0.0d0
  M = 6; N = 4
  call DGEQL2(M, N, A, 6, TAU, WORK, INFO)
  call begin_test('6x4_ql')
  call print_matrix('A', A, 6, M, N)
  call print_array('TAU', TAU, N)
  call print_int('INFO', INFO)
  call end_test()

end program
