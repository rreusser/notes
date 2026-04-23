program test_dormbr
  use test_utils
  implicit none

  ! Variables for small tests
  double precision :: A(6, 6), Asave(6, 6), C(6, 6)
  double precision :: D(6), E(6), TAUQ(6), TAUP(6), WORK(10000)
  integer :: INFO, LWORK, i, j

  LWORK = 10000

  ! =========================================================================
  ! Compute bidiagonal reduction of a 4x3 (M > N, upper bidiagonal) matrix
  ! =========================================================================
  A = 0.0d0
  A(1,1) = 2.0d0; A(2,1) = 1.0d0; A(3,1) = 3.0d0; A(4,1) = 1.0d0
  A(1,2) = 1.0d0; A(2,2) = 4.0d0; A(3,2) = 2.0d0; A(4,2) = 3.0d0
  A(1,3) = 3.0d0; A(2,3) = 2.0d0; A(3,3) = 5.0d0; A(4,3) = 1.0d0
  D = 0.0d0; E = 0.0d0; TAUQ = 0.0d0; TAUP = 0.0d0
  call DGEBRD(4, 3, A, 6, D, E, TAUQ, TAUP, WORK, LWORK, INFO)
  Asave = A  ! save for reuse

  ! Print bidiagonal factors for JS test reuse
  call begin_test('gebrd_4x3')
  call print_int('INFO', INFO)
  call print_matrix('A', A, 6, 4, 3)
  call print_array('TAUQ', TAUQ, 3)
  call print_array('TAUP', TAUP, 3)
  call end_test()

  ! ----- Test 1: VECT='Q', SIDE='L', TRANS='N' (Q * C) -----
  ! C = I_4, result should be Q (4x4) applied from left to 4x3 identity slice
  C = 0.0d0
  do i = 1, 4
    C(i,i) = 1.0d0
  end do
  A = Asave
  call DORMBR('Q', 'L', 'N', 4, 4, 3, A, 6, TAUQ, C, 6, WORK, LWORK, INFO)
  call begin_test('Q_L_N_upper')
  call print_int('INFO', INFO)
  call print_matrix('C', C, 6, 4, 4)
  call end_test()

  ! ----- Test 2: VECT='Q', SIDE='L', TRANS='T' (Q^T * C) -----
  C = 0.0d0
  do i = 1, 4
    C(i,i) = 1.0d0
  end do
  A = Asave
  call DORMBR('Q', 'L', 'T', 4, 4, 3, A, 6, TAUQ, C, 6, WORK, LWORK, INFO)
  call begin_test('Q_L_T_upper')
  call print_int('INFO', INFO)
  call print_matrix('C', C, 6, 4, 4)
  call end_test()

  ! ----- Test 3: VECT='P', SIDE='R', TRANS='N' (C * P) -----
  ! For M>=N (upper bidiagonal), P is 3x3 (NQ=N=3, K=N=3, NQ>=K)
  C = 0.0d0
  do i = 1, 3
    C(i,i) = 1.0d0
  end do
  A = Asave
  call DORMBR('P', 'R', 'N', 3, 3, 3, A, 6, TAUP, C, 6, WORK, LWORK, INFO)
  call begin_test('P_R_N_upper')
  call print_int('INFO', INFO)
  call print_matrix('C', C, 6, 3, 3)
  call end_test()

  ! ----- Test 4: VECT='P', SIDE='R', TRANS='T' (C * P^T) -----
  C = 0.0d0
  do i = 1, 3
    C(i,i) = 1.0d0
  end do
  A = Asave
  call DORMBR('P', 'R', 'T', 3, 3, 3, A, 6, TAUP, C, 6, WORK, LWORK, INFO)
  call begin_test('P_R_T_upper')
  call print_int('INFO', INFO)
  call print_matrix('C', C, 6, 3, 3)
  call end_test()

  ! ----- Test 5: VECT='Q', SIDE='R' (C * Q, less common) -----
  C = 0.0d0
  do i = 1, 4
    C(i,i) = 1.0d0
  end do
  A = Asave
  call DORMBR('Q', 'R', 'N', 4, 4, 3, A, 6, TAUQ, C, 6, WORK, LWORK, INFO)
  call begin_test('Q_R_N_upper')
  call print_int('INFO', INFO)
  call print_matrix('C', C, 6, 4, 4)
  call end_test()

  ! ----- Test 6: VECT='P', SIDE='L' (P * C) -----
  C = 0.0d0
  do i = 1, 3
    C(i,i) = 1.0d0
  end do
  A = Asave
  call DORMBR('P', 'L', 'N', 3, 3, 3, A, 6, TAUP, C, 6, WORK, LWORK, INFO)
  call begin_test('P_L_N_upper')
  call print_int('INFO', INFO)
  call print_matrix('C', C, 6, 3, 3)
  call end_test()

  ! =========================================================================
  ! Compute bidiagonal reduction of a 3x4 (M < N, lower bidiagonal) matrix
  ! =========================================================================
  A = 0.0d0
  A(1,1) = 2.0d0; A(2,1) = 4.0d0; A(3,1) = 1.0d0
  A(1,2) = 1.0d0; A(2,2) = 2.0d0; A(3,2) = 5.0d0
  A(1,3) = 3.0d0; A(2,3) = 1.0d0; A(3,3) = 2.0d0
  A(1,4) = 1.0d0; A(2,4) = 3.0d0; A(3,4) = 4.0d0
  D = 0.0d0; E = 0.0d0; TAUQ = 0.0d0; TAUP = 0.0d0
  call DGEBRD(3, 4, A, 6, D, E, TAUQ, TAUP, WORK, LWORK, INFO)
  Asave = A

  call begin_test('gebrd_3x4')
  call print_int('INFO', INFO)
  call print_matrix('A', A, 6, 3, 4)
  call print_array('TAUQ', TAUQ, 3)
  call print_array('TAUP', TAUP, 3)
  call end_test()

  ! ----- Test 7: VECT='Q', SIDE='L', TRANS='N' (Q * C) for M < N -----
  ! For M<N, Q is 3x3 (NQ=M=3, K=N=4, NQ < K)
  C = 0.0d0
  do i = 1, 3
    C(i,i) = 1.0d0
  end do
  A = Asave
  call DORMBR('Q', 'L', 'N', 3, 3, 4, A, 6, TAUQ, C, 6, WORK, LWORK, INFO)
  call begin_test('Q_L_N_lower')
  call print_int('INFO', INFO)
  call print_matrix('C', C, 6, 3, 3)
  call end_test()

  ! ----- Test 8: VECT='Q', SIDE='L', TRANS='T' for M < N -----
  C = 0.0d0
  do i = 1, 3
    C(i,i) = 1.0d0
  end do
  A = Asave
  call DORMBR('Q', 'L', 'T', 3, 3, 4, A, 6, TAUQ, C, 6, WORK, LWORK, INFO)
  call begin_test('Q_L_T_lower')
  call print_int('INFO', INFO)
  call print_matrix('C', C, 6, 3, 3)
  call end_test()

  ! ----- Test 9: VECT='P', SIDE='R', TRANS='N' (C * P) for M < N -----
  ! For M<N (lower bidiagonal), P is 4x4 (NQ=N=4, K=M=3, NQ > K)
  C = 0.0d0
  do i = 1, 4
    C(i,i) = 1.0d0
  end do
  A = Asave
  call DORMBR('P', 'R', 'N', 4, 4, 3, A, 6, TAUP, C, 6, WORK, LWORK, INFO)
  call begin_test('P_R_N_lower')
  call print_int('INFO', INFO)
  call print_matrix('C', C, 6, 4, 4)
  call end_test()

  ! ----- Test 10: VECT='P', SIDE='R', TRANS='T' for M < N -----
  C = 0.0d0
  do i = 1, 4
    C(i,i) = 1.0d0
  end do
  A = Asave
  call DORMBR('P', 'R', 'T', 4, 4, 3, A, 6, TAUP, C, 6, WORK, LWORK, INFO)
  call begin_test('P_R_T_lower')
  call print_int('INFO', INFO)
  call print_matrix('C', C, 6, 4, 4)
  call end_test()

  ! ----- Test 11: VECT='P', SIDE='L', TRANS='N' for M < N -----
  C = 0.0d0
  do i = 1, 4
    C(i,i) = 1.0d0
  end do
  A = Asave
  call DORMBR('P', 'L', 'N', 4, 4, 3, A, 6, TAUP, C, 6, WORK, LWORK, INFO)
  call begin_test('P_L_N_lower')
  call print_int('INFO', INFO)
  call print_matrix('C', C, 6, 4, 4)
  call end_test()

  ! ----- Test 12: VECT='P', SIDE='L', TRANS='T' for M < N -----
  C = 0.0d0
  do i = 1, 4
    C(i,i) = 1.0d0
  end do
  A = Asave
  call DORMBR('P', 'L', 'T', 4, 4, 3, A, 6, TAUP, C, 6, WORK, LWORK, INFO)
  call begin_test('P_L_T_lower')
  call print_int('INFO', INFO)
  call print_matrix('C', C, 6, 4, 4)
  call end_test()

  ! ----- Test 13: VECT='Q', SIDE='R', TRANS='N' for M < N -----
  C = 0.0d0
  do i = 1, 3
    C(i,i) = 1.0d0
  end do
  A = Asave
  call DORMBR('Q', 'R', 'N', 3, 3, 4, A, 6, TAUQ, C, 6, WORK, LWORK, INFO)
  call begin_test('Q_R_N_lower')
  call print_int('INFO', INFO)
  call print_matrix('C', C, 6, 3, 3)
  call end_test()

  ! ----- Test 14: Non-identity C (VECT='Q', SIDE='L', TRANS='N', 4x3 upper) -----
  C = 0.0d0
  C(1,1) = 1.0d0; C(2,1) = 3.0d0; C(3,1) = -1.0d0; C(4,1) = 2.0d0
  C(1,2) = 2.0d0; C(2,2) = 0.0d0; C(3,2) = 4.0d0; C(4,2) = -1.0d0
  ! Reset A to upper bidiagonal factors
  A = 0.0d0
  A(1,1) = 2.0d0; A(2,1) = 1.0d0; A(3,1) = 3.0d0; A(4,1) = 1.0d0
  A(1,2) = 1.0d0; A(2,2) = 4.0d0; A(3,2) = 2.0d0; A(4,2) = 3.0d0
  A(1,3) = 3.0d0; A(2,3) = 2.0d0; A(3,3) = 5.0d0; A(4,3) = 1.0d0
  D = 0.0d0; E = 0.0d0; TAUQ = 0.0d0; TAUP = 0.0d0
  call DGEBRD(4, 3, A, 6, D, E, TAUQ, TAUP, WORK, LWORK, INFO)
  call DORMBR('Q', 'L', 'N', 4, 2, 3, A, 6, TAUQ, C, 6, WORK, LWORK, INFO)
  call begin_test('Q_L_N_nonident')
  call print_int('INFO', INFO)
  call print_matrix('C', C, 6, 4, 2)
  call end_test()

  ! ----- Test 15: M=0 quick return -----
  call DORMBR('Q', 'L', 'N', 0, 3, 3, A, 6, TAUQ, C, 6, WORK, LWORK, INFO)
  call begin_test('m_zero')
  call print_int('INFO', INFO)
  call end_test()

  ! ----- Test 16: N=0 quick return -----
  call DORMBR('Q', 'L', 'N', 3, 0, 3, A, 6, TAUQ, C, 6, WORK, LWORK, INFO)
  call begin_test('n_zero')
  call print_int('INFO', INFO)
  call end_test()

  ! ----- Test 17: K=0 quick return -----
  C = 0.0d0
  do i = 1, 4
    C(i,i) = 1.0d0
  end do
  call DORMBR('Q', 'L', 'N', 4, 4, 0, A, 6, TAUQ, C, 6, WORK, LWORK, INFO)
  call begin_test('k_zero')
  call print_int('INFO', INFO)
  call print_matrix('C', C, 6, 4, 4)
  call end_test()

  ! ----- Test 18: VECT='Q', SIDE='R', TRANS='T' for upper bidiagonal -----
  A = 0.0d0
  A(1,1) = 2.0d0; A(2,1) = 1.0d0; A(3,1) = 3.0d0; A(4,1) = 1.0d0
  A(1,2) = 1.0d0; A(2,2) = 4.0d0; A(3,2) = 2.0d0; A(4,2) = 3.0d0
  A(1,3) = 3.0d0; A(2,3) = 2.0d0; A(3,3) = 5.0d0; A(4,3) = 1.0d0
  D = 0.0d0; E = 0.0d0; TAUQ = 0.0d0; TAUP = 0.0d0
  call DGEBRD(4, 3, A, 6, D, E, TAUQ, TAUP, WORK, LWORK, INFO)
  C = 0.0d0
  do i = 1, 4
    C(i,i) = 1.0d0
  end do
  call DORMBR('Q', 'R', 'T', 4, 4, 3, A, 6, TAUQ, C, 6, WORK, LWORK, INFO)
  call begin_test('Q_R_T_upper')
  call print_int('INFO', INFO)
  call print_matrix('C', C, 6, 4, 4)
  call end_test()

  ! ----- Test 19: VECT='P', SIDE='L', TRANS='T' for upper bidiagonal -----
  A = 0.0d0
  A(1,1) = 2.0d0; A(2,1) = 1.0d0; A(3,1) = 3.0d0; A(4,1) = 1.0d0
  A(1,2) = 1.0d0; A(2,2) = 4.0d0; A(3,2) = 2.0d0; A(4,2) = 3.0d0
  A(1,3) = 3.0d0; A(2,3) = 2.0d0; A(3,3) = 5.0d0; A(4,3) = 1.0d0
  D = 0.0d0; E = 0.0d0; TAUQ = 0.0d0; TAUP = 0.0d0
  call DGEBRD(4, 3, A, 6, D, E, TAUQ, TAUP, WORK, LWORK, INFO)
  C = 0.0d0
  do i = 1, 3
    C(i,i) = 1.0d0
  end do
  call DORMBR('P', 'L', 'T', 3, 3, 3, A, 6, TAUP, C, 6, WORK, LWORK, INFO)
  call begin_test('P_L_T_upper')
  call print_int('INFO', INFO)
  call print_matrix('C', C, 6, 3, 3)
  call end_test()

  ! ----- Test 20: VECT='Q', SIDE='R', TRANS='T' for lower bidiagonal -----
  A = 0.0d0
  A(1,1) = 2.0d0; A(2,1) = 4.0d0; A(3,1) = 1.0d0
  A(1,2) = 1.0d0; A(2,2) = 2.0d0; A(3,2) = 5.0d0
  A(1,3) = 3.0d0; A(2,3) = 1.0d0; A(3,3) = 2.0d0
  A(1,4) = 1.0d0; A(2,4) = 3.0d0; A(3,4) = 4.0d0
  D = 0.0d0; E = 0.0d0; TAUQ = 0.0d0; TAUP = 0.0d0
  call DGEBRD(3, 4, A, 6, D, E, TAUQ, TAUP, WORK, LWORK, INFO)
  C = 0.0d0
  do i = 1, 3
    C(i,i) = 1.0d0
  end do
  call DORMBR('Q', 'R', 'T', 3, 3, 4, A, 6, TAUQ, C, 6, WORK, LWORK, INFO)
  call begin_test('Q_R_T_lower')
  call print_int('INFO', INFO)
  call print_matrix('C', C, 6, 3, 3)
  call end_test()

  ! ----- Test 21: VECT='P', SIDE='R', TRANS='N' non-identity C -----
  A = 0.0d0
  A(1,1) = 2.0d0; A(2,1) = 4.0d0; A(3,1) = 1.0d0
  A(1,2) = 1.0d0; A(2,2) = 2.0d0; A(3,2) = 5.0d0
  A(1,3) = 3.0d0; A(2,3) = 1.0d0; A(3,3) = 2.0d0
  A(1,4) = 1.0d0; A(2,4) = 3.0d0; A(3,4) = 4.0d0
  D = 0.0d0; E = 0.0d0; TAUQ = 0.0d0; TAUP = 0.0d0
  call DGEBRD(3, 4, A, 6, D, E, TAUQ, TAUP, WORK, LWORK, INFO)
  C = 0.0d0
  C(1,1) = 1.0d0; C(2,1) = 3.0d0; C(3,1) = -1.0d0
  C(1,2) = 2.0d0; C(2,2) = 0.0d0; C(3,2) = 4.0d0
  C(1,3) = -1.0d0; C(2,3) = 2.0d0; C(3,3) = 1.0d0
  C(1,4) = 0.0d0; C(2,4) = 1.0d0; C(3,4) = -2.0d0
  call DORMBR('P', 'R', 'N', 3, 4, 3, A, 6, TAUP, C, 6, WORK, LWORK, INFO)
  call begin_test('P_R_N_lower_nonident')
  call print_int('INFO', INFO)
  call print_matrix('C', C, 6, 3, 4)
  call end_test()

end program
