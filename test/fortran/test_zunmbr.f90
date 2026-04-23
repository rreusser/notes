program test_zunmbr
  use test_utils
  implicit none

  complex*16 :: A(6, 6), C(6, 6), TAUQ(6), TAUP(6), D(6), E(6), WORK(200)
  double precision :: A_r(72), C_r(72)
  equivalence (A, A_r)
  equivalence (C, C_r)
  integer :: info, i, LWORK

  LWORK = 200

  ! Test 1: VECT='Q', SIDE='L', TRANS='N', NQ >= K
  ! 4x3 matrix -> bidiagonal -> Q is 4x4, apply to 4x4 identity
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(2,1) = (2.0d0, 1.0d0); A(3,1) = (0.0d0, -1.0d0); A(4,1) = (1.0d0, 1.0d0)
  A(1,2) = (0.0d0, 2.0d0); A(2,2) = (1.0d0, 0.0d0); A(3,2) = (3.0d0, 1.0d0); A(4,2) = (2.0d0, 0.0d0)
  A(1,3) = (3.0d0, 1.0d0); A(2,3) = (0.0d0, 0.0d0); A(3,3) = (1.0d0, 0.0d0); A(4,3) = (2.0d0, 1.0d0)
  TAUQ = (0.0d0, 0.0d0)
  TAUP = (0.0d0, 0.0d0)
  call zgebrd(4, 3, A, 6, D, E, TAUQ, TAUP, WORK, LWORK, info)

  C = (0.0d0, 0.0d0)
  do i = 1, 4
    C(i,i) = (1.0d0, 0.0d0)
  end do
  call zunmbr('Q', 'L', 'N', 4, 4, 3, A, 6, TAUQ, C, 6, WORK, LWORK, info)
  call begin_test('q_left_notrans')
  call print_int('info', info)
  call print_array('c', C_r, 32)
  call end_test()

  ! Test 2: VECT='Q', SIDE='L', TRANS='C', NQ >= K
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(2,1) = (2.0d0, 1.0d0); A(3,1) = (0.0d0, -1.0d0); A(4,1) = (1.0d0, 1.0d0)
  A(1,2) = (0.0d0, 2.0d0); A(2,2) = (1.0d0, 0.0d0); A(3,2) = (3.0d0, 1.0d0); A(4,2) = (2.0d0, 0.0d0)
  A(1,3) = (3.0d0, 1.0d0); A(2,3) = (0.0d0, 0.0d0); A(3,3) = (1.0d0, 0.0d0); A(4,3) = (2.0d0, 1.0d0)
  TAUQ = (0.0d0, 0.0d0)
  TAUP = (0.0d0, 0.0d0)
  call zgebrd(4, 3, A, 6, D, E, TAUQ, TAUP, WORK, LWORK, info)

  C = (0.0d0, 0.0d0)
  do i = 1, 4
    C(i,i) = (1.0d0, 0.0d0)
  end do
  call zunmbr('Q', 'L', 'C', 4, 4, 3, A, 6, TAUQ, C, 6, WORK, LWORK, info)
  call begin_test('q_left_conjtrans')
  call print_int('info', info)
  call print_array('c', C_r, 32)
  call end_test()

  ! Test 3: VECT='P', SIDE='R', TRANS='N'
  ! 3x5 matrix -> bidiagonal -> P^H applies from right
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(1,2) = (2.0d0, 1.0d0); A(1,3) = (0.0d0, 0.0d0); A(1,4) = (1.0d0, 1.0d0); A(1,5) = (3.0d0, 0.0d0)
  A(2,1) = (0.0d0, 2.0d0); A(2,2) = (1.0d0, 0.0d0); A(2,3) = (3.0d0, 1.0d0); A(2,4) = (2.0d0, 0.0d0); A(2,5) = (1.0d0, 1.0d0)
  A(3,1) = (3.0d0, 1.0d0); A(3,2) = (0.0d0, 0.0d0); A(3,3) = (1.0d0, 0.0d0); A(3,4) = (2.0d0, 1.0d0); A(3,5) = (0.0d0, 2.0d0)
  TAUQ = (0.0d0, 0.0d0)
  TAUP = (0.0d0, 0.0d0)
  call zgebrd(3, 5, A, 6, D, E, TAUQ, TAUP, WORK, LWORK, info)

  C = (0.0d0, 0.0d0)
  do i = 1, 5
    C(i,i) = (1.0d0, 0.0d0)
  end do
  call zunmbr('P', 'R', 'N', 5, 5, 3, A, 6, TAUP, C, 6, WORK, LWORK, info)
  call begin_test('p_right_notrans')
  call print_int('info', info)
  call print_array('c', C_r, 50)
  call end_test()

  ! Test 4: VECT='P', SIDE='R', TRANS='C'
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(1,2) = (2.0d0, 1.0d0); A(1,3) = (0.0d0, 0.0d0); A(1,4) = (1.0d0, 1.0d0); A(1,5) = (3.0d0, 0.0d0)
  A(2,1) = (0.0d0, 2.0d0); A(2,2) = (1.0d0, 0.0d0); A(2,3) = (3.0d0, 1.0d0); A(2,4) = (2.0d0, 0.0d0); A(2,5) = (1.0d0, 1.0d0)
  A(3,1) = (3.0d0, 1.0d0); A(3,2) = (0.0d0, 0.0d0); A(3,3) = (1.0d0, 0.0d0); A(3,4) = (2.0d0, 1.0d0); A(3,5) = (0.0d0, 2.0d0)
  TAUQ = (0.0d0, 0.0d0)
  TAUP = (0.0d0, 0.0d0)
  call zgebrd(3, 5, A, 6, D, E, TAUQ, TAUP, WORK, LWORK, info)

  C = (0.0d0, 0.0d0)
  do i = 1, 5
    C(i,i) = (1.0d0, 0.0d0)
  end do
  call zunmbr('P', 'R', 'C', 5, 5, 3, A, 6, TAUP, C, 6, WORK, LWORK, info)
  call begin_test('p_right_conjtrans')
  call print_int('info', info)
  call print_array('c', C_r, 50)
  call end_test()

  ! Test 5: M=0 quick return
  call zunmbr('Q', 'L', 'N', 0, 0, 0, A, 6, TAUQ, C, 6, WORK, LWORK, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: VECT='Q', SIDE='R', TRANS='N' (right multiply by Q)
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(2,1) = (2.0d0, 1.0d0); A(3,1) = (0.0d0, -1.0d0); A(4,1) = (1.0d0, 1.0d0)
  A(1,2) = (0.0d0, 2.0d0); A(2,2) = (1.0d0, 0.0d0); A(3,2) = (3.0d0, 1.0d0); A(4,2) = (2.0d0, 0.0d0)
  A(1,3) = (3.0d0, 1.0d0); A(2,3) = (0.0d0, 0.0d0); A(3,3) = (1.0d0, 0.0d0); A(4,3) = (2.0d0, 1.0d0)
  TAUQ = (0.0d0, 0.0d0)
  TAUP = (0.0d0, 0.0d0)
  call zgebrd(4, 3, A, 6, D, E, TAUQ, TAUP, WORK, LWORK, info)

  C = (0.0d0, 0.0d0)
  do i = 1, 4
    C(i,i) = (1.0d0, 0.0d0)
  end do
  call zunmbr('Q', 'R', 'N', 4, 4, 3, A, 6, TAUQ, C, 6, WORK, LWORK, info)
  call begin_test('q_right_notrans')
  call print_int('info', info)
  call print_array('c', C_r, 32)
  call end_test()

  ! Test 7: VECT='P', SIDE='L', TRANS='C'
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(1,2) = (2.0d0, 1.0d0); A(1,3) = (0.0d0, 0.0d0); A(1,4) = (1.0d0, 1.0d0); A(1,5) = (3.0d0, 0.0d0)
  A(2,1) = (0.0d0, 2.0d0); A(2,2) = (1.0d0, 0.0d0); A(2,3) = (3.0d0, 1.0d0); A(2,4) = (2.0d0, 0.0d0); A(2,5) = (1.0d0, 1.0d0)
  A(3,1) = (3.0d0, 1.0d0); A(3,2) = (0.0d0, 0.0d0); A(3,3) = (1.0d0, 0.0d0); A(3,4) = (2.0d0, 1.0d0); A(3,5) = (0.0d0, 2.0d0)
  TAUQ = (0.0d0, 0.0d0)
  TAUP = (0.0d0, 0.0d0)
  call zgebrd(3, 5, A, 6, D, E, TAUQ, TAUP, WORK, LWORK, info)

  C = (0.0d0, 0.0d0)
  do i = 1, 5
    C(i,i) = (1.0d0, 0.0d0)
  end do
  call zunmbr('P', 'L', 'C', 5, 5, 3, A, 6, TAUP, C, 6, WORK, LWORK, info)
  call begin_test('p_left_conjtrans')
  call print_int('info', info)
  call print_array('c', C_r, 50)
  call end_test()

end program
