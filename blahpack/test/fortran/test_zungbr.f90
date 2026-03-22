program test_zungbr
  use test_utils
  implicit none

  complex*16 :: A(6, 6), TAUQ(6), TAUP(6), D(6), E(6), WORK(200)
  double precision :: A_r(72), TAUQ_r(12), TAUP_r(12)
  equivalence (A, A_r)
  equivalence (TAUQ, TAUQ_r)
  equivalence (TAUP, TAUP_r)
  integer :: info, i, j, LWORK

  LWORK = 200

  ! Test 1: VECT='Q', M >= K (4x3 bidiagonal reduction)
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(2,1) = (2.0d0, 1.0d0); A(3,1) = (0.0d0, -1.0d0); A(4,1) = (1.0d0, 1.0d0)
  A(1,2) = (0.0d0, 2.0d0); A(2,2) = (1.0d0, 0.0d0); A(3,2) = (3.0d0, 1.0d0); A(4,2) = (2.0d0, 0.0d0)
  A(1,3) = (3.0d0, 1.0d0); A(2,3) = (0.0d0, 0.0d0); A(3,3) = (1.0d0, 0.0d0); A(4,3) = (2.0d0, 1.0d0)
  TAUQ = (0.0d0, 0.0d0)
  TAUP = (0.0d0, 0.0d0)
  call zgebrd(4, 3, A, 6, D, E, TAUQ, TAUP, WORK, LWORK, info)

  ! Generate Q (4x3)
  call zungbr('Q', 4, 3, 3, A, 6, TAUQ, WORK, LWORK, info)
  call begin_test('vect_q_m_ge_k')
  call print_int('info', info)
  call print_array('a', A_r, 48)
  call end_test()

  ! Test 2: VECT='P', K < N (3x5 bidiagonal reduction)
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(1,2) = (2.0d0, 1.0d0); A(1,3) = (0.0d0, 0.0d0); A(1,4) = (1.0d0, 1.0d0); A(1,5) = (3.0d0, 0.0d0)
  A(2,1) = (0.0d0, 2.0d0); A(2,2) = (1.0d0, 0.0d0); A(2,3) = (3.0d0, 1.0d0); A(2,4) = (2.0d0, 0.0d0); A(2,5) = (1.0d0, 1.0d0)
  A(3,1) = (3.0d0, 1.0d0); A(3,2) = (0.0d0, 0.0d0); A(3,3) = (1.0d0, 0.0d0); A(3,4) = (2.0d0, 1.0d0); A(3,5) = (0.0d0, 2.0d0)
  TAUQ = (0.0d0, 0.0d0)
  TAUP = (0.0d0, 0.0d0)
  call zgebrd(3, 5, A, 6, D, E, TAUQ, TAUP, WORK, LWORK, info)

  ! Generate P^H (3x5)
  call zungbr('P', 3, 5, 3, A, 6, TAUP, WORK, LWORK, info)
  call begin_test('vect_p_k_lt_n')
  call print_int('info', info)
  call print_array('a', A_r, 60)
  call end_test()

  ! Test 3: VECT='Q', M < K (3x4 -> Q is 3x3, K=4)
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(2,1) = (2.0d0, 1.0d0); A(3,1) = (0.0d0, -1.0d0)
  A(1,2) = (0.0d0, 2.0d0); A(2,2) = (1.0d0, 0.0d0); A(3,2) = (3.0d0, 1.0d0)
  A(1,3) = (3.0d0, 1.0d0); A(2,3) = (0.0d0, 0.0d0); A(3,3) = (1.0d0, 0.0d0)
  A(1,4) = (1.0d0, 1.0d0); A(2,4) = (2.0d0, 0.0d0); A(3,4) = (2.0d0, 1.0d0)
  TAUQ = (0.0d0, 0.0d0)
  TAUP = (0.0d0, 0.0d0)
  call zgebrd(3, 4, A, 6, D, E, TAUQ, TAUP, WORK, LWORK, info)

  ! Generate Q (3x3) from 3x4 reduction (M < K case)
  call zungbr('Q', 3, 3, 4, A, 6, TAUQ, WORK, LWORK, info)
  call begin_test('vect_q_m_lt_k')
  call print_int('info', info)
  call print_array('a', A_r, 36)
  call end_test()

  ! Test 4: VECT='P', K >= N (4x3 -> P^H is 3x3)
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(2,1) = (2.0d0, 1.0d0); A(3,1) = (0.0d0, -1.0d0); A(4,1) = (1.0d0, 1.0d0)
  A(1,2) = (0.0d0, 2.0d0); A(2,2) = (1.0d0, 0.0d0); A(3,2) = (3.0d0, 1.0d0); A(4,2) = (2.0d0, 0.0d0)
  A(1,3) = (3.0d0, 1.0d0); A(2,3) = (0.0d0, 0.0d0); A(3,3) = (1.0d0, 0.0d0); A(4,3) = (2.0d0, 1.0d0)
  TAUQ = (0.0d0, 0.0d0)
  TAUP = (0.0d0, 0.0d0)
  call zgebrd(4, 3, A, 6, D, E, TAUQ, TAUP, WORK, LWORK, info)

  ! Generate P^H (3x3, K >= N case)
  call zungbr('P', 3, 3, 4, A, 6, TAUP, WORK, LWORK, info)
  call begin_test('vect_p_k_ge_n')
  call print_int('info', info)
  call print_array('a', A_r, 36)
  call end_test()

  ! Test 5: M=0, N=0 quick return
  call zungbr('Q', 0, 0, 0, A, 6, TAUQ, WORK, LWORK, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

end program
