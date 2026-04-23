program test_dgebd2
  use test_utils
  implicit none

  double precision :: A(6, 6), D(6), E(6), TAUQ(6), TAUP(6), WORK(6)
  integer :: INFO

  ! Test 1: 4x3 matrix (M > N, upper bidiagonal)
  A = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0
  A(3,1) = 3.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0
  A(4,1) = 1.0d0; A(4,2) = 3.0d0; A(4,3) = 1.0d0
  D = 0.0d0; E = 0.0d0; TAUQ = 0.0d0; TAUP = 0.0d0; WORK = 0.0d0
  call DGEBD2(4, 3, A, 6, D, E, TAUQ, TAUP, WORK, INFO)
  call begin_test('4x3_upper')
  call print_matrix('A', A, 6, 4, 3)
  call print_array('D', D, 3)
  call print_array('E', E, 2)
  call print_array('TAUQ', TAUQ, 3)
  call print_array('TAUP', TAUP, 3)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 2: 3x4 matrix (M < N, lower bidiagonal)
  A = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0; A(1,4) = 1.0d0
  A(2,1) = 4.0d0; A(2,2) = 2.0d0; A(2,3) = 1.0d0; A(2,4) = 3.0d0
  A(3,1) = 1.0d0; A(3,2) = 5.0d0; A(3,3) = 2.0d0; A(3,4) = 4.0d0
  D = 0.0d0; E = 0.0d0; TAUQ = 0.0d0; TAUP = 0.0d0; WORK = 0.0d0
  call DGEBD2(3, 4, A, 6, D, E, TAUQ, TAUP, WORK, INFO)
  call begin_test('3x4_lower')
  call print_matrix('A', A, 6, 3, 4)
  call print_array('D', D, 3)
  call print_array('E', E, 2)
  call print_array('TAUQ', TAUQ, 3)
  call print_array('TAUP', TAUP, 3)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 3: 3x3 square matrix
  A = 0.0d0
  A(1,1) = 5.0d0; A(1,2) = 2.0d0; A(1,3) = 1.0d0
  A(2,1) = 3.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0
  A(3,1) = 1.0d0; A(3,2) = 3.0d0; A(3,3) = 6.0d0
  D = 0.0d0; E = 0.0d0; TAUQ = 0.0d0; TAUP = 0.0d0; WORK = 0.0d0
  call DGEBD2(3, 3, A, 6, D, E, TAUQ, TAUP, WORK, INFO)
  call begin_test('3x3_square')
  call print_matrix('A', A, 6, 3, 3)
  call print_array('D', D, 3)
  call print_array('E', E, 2)
  call print_array('TAUQ', TAUQ, 3)
  call print_array('TAUP', TAUP, 3)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 4: 1x3 matrix (M=1, M < N)
  A = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 3.0d0; A(1,3) = 4.0d0
  D = 0.0d0; E = 0.0d0; TAUQ = 0.0d0; TAUP = 0.0d0; WORK = 0.0d0
  call DGEBD2(1, 3, A, 6, D, E, TAUQ, TAUP, WORK, INFO)
  call begin_test('1x3')
  call print_matrix('A', A, 6, 1, 3)
  call print_array('D', D, 1)
  call print_array('TAUQ', TAUQ, 1)
  call print_array('TAUP', TAUP, 1)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 5: 3x1 matrix (N=1, M > N)
  A = 0.0d0
  A(1,1) = 2.0d0
  A(2,1) = 3.0d0
  A(3,1) = 4.0d0
  D = 0.0d0; E = 0.0d0; TAUQ = 0.0d0; TAUP = 0.0d0; WORK = 0.0d0
  call DGEBD2(3, 1, A, 6, D, E, TAUQ, TAUP, WORK, INFO)
  call begin_test('3x1')
  call print_matrix('A', A, 6, 3, 1)
  call print_array('D', D, 1)
  call print_array('TAUQ', TAUQ, 1)
  call print_array('TAUP', TAUP, 1)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 6: M=0 quick return
  D = 0.0d0; E = 0.0d0; TAUQ = 0.0d0; TAUP = 0.0d0; WORK = 0.0d0
  call DGEBD2(0, 3, A, 6, D, E, TAUQ, TAUP, WORK, INFO)
  call begin_test('m_zero')
  call print_int('INFO', INFO)
  call end_test()

  ! Test 7: N=0 quick return
  D = 0.0d0; E = 0.0d0; TAUQ = 0.0d0; TAUP = 0.0d0; WORK = 0.0d0
  call DGEBD2(3, 0, A, 6, D, E, TAUQ, TAUP, WORK, INFO)
  call begin_test('n_zero')
  call print_int('INFO', INFO)
  call end_test()

  ! Test 8: 1x1 matrix
  A = 0.0d0
  A(1,1) = 7.0d0
  D = 0.0d0; E = 0.0d0; TAUQ = 0.0d0; TAUP = 0.0d0; WORK = 0.0d0
  call DGEBD2(1, 1, A, 6, D, E, TAUQ, TAUP, WORK, INFO)
  call begin_test('1x1')
  call print_matrix('A', A, 6, 1, 1)
  call print_array('D', D, 1)
  call print_array('TAUQ', TAUQ, 1)
  call print_array('TAUP', TAUP, 1)
  call print_int('INFO', INFO)
  call end_test()

end program
