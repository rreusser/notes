program test_dgehd2
  use test_utils
  implicit none

  integer :: INFO, i, j, N, LDA
  double precision :: A(40*40), TAU(40), WORK(40)

  ! Test 1: 4x4 full range (ILO=1, IHI=4)
  N = 4
  LDA = 4
  A(1) = 1.0d0; A(5) = 2.0d0; A(9)  = 3.0d0; A(13) = 4.0d0
  A(2) = 5.0d0; A(6) = 6.0d0; A(10) = 7.0d0; A(14) = 8.0d0
  A(3) = 9.0d0; A(7) = 10.0d0; A(11) = 11.0d0; A(15) = 12.0d0
  A(4) = 13.0d0; A(8) = 14.0d0; A(12) = 15.0d0; A(16) = 16.0d0
  TAU = 0.0d0
  WORK = 0.0d0
  call DGEHD2(N, 1, N, A, LDA, TAU, WORK, INFO)
  call begin_test('4x4_full')
  call print_matrix('A', A, LDA, N, N)
  call print_array('TAU', TAU, N-1)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 2: 5x5 full range
  N = 5
  LDA = 5
  A(1) = 2.0d0; A(6) = 1.0d0; A(11) = 3.0d0; A(16) = 1.0d0; A(21) = 4.0d0
  A(2) = 1.0d0; A(7) = 4.0d0; A(12) = 1.0d0; A(17) = 2.0d0; A(22) = 1.0d0
  A(3) = 3.0d0; A(8) = 1.0d0; A(13) = 5.0d0; A(18) = 1.0d0; A(23) = 2.0d0
  A(4) = 1.0d0; A(9) = 2.0d0; A(14) = 1.0d0; A(19) = 6.0d0; A(24) = 1.0d0
  A(5) = 4.0d0; A(10) = 1.0d0; A(15) = 2.0d0; A(20) = 1.0d0; A(25) = 7.0d0
  TAU = 0.0d0
  WORK = 0.0d0
  call DGEHD2(N, 1, N, A, LDA, TAU, WORK, INFO)
  call begin_test('5x5_full')
  call print_matrix('A', A, LDA, N, N)
  call print_array('TAU', TAU, N-1)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 3: 4x4 partial (ILO=2, IHI=3)
  N = 4
  LDA = 4
  A(1) = 1.0d0; A(5) = 2.0d0; A(9)  = 3.0d0; A(13) = 4.0d0
  A(2) = 0.0d0; A(6) = 5.0d0; A(10) = 6.0d0; A(14) = 7.0d0
  A(3) = 0.0d0; A(7) = 8.0d0; A(11) = 9.0d0; A(15) = 10.0d0
  A(4) = 0.0d0; A(8) = 0.0d0; A(12) = 0.0d0; A(16) = 11.0d0
  TAU = 0.0d0
  WORK = 0.0d0
  call DGEHD2(N, 2, 3, A, LDA, TAU, WORK, INFO)
  call begin_test('4x4_partial_ilo2_ihi3')
  call print_matrix('A', A, LDA, N, N)
  call print_array('TAU', TAU, N-1)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 4: N=1 (quick return)
  N = 1
  LDA = 1
  A(1) = 42.0d0
  TAU = 0.0d0
  WORK = 0.0d0
  call DGEHD2(N, 1, 1, A, LDA, TAU, WORK, INFO)
  call begin_test('n_one')
  call print_matrix('A', A, LDA, N, N)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 5: N=2
  N = 2
  LDA = 2
  A(1) = 3.0d0; A(3) = 1.0d0
  A(2) = 4.0d0; A(4) = 2.0d0
  TAU = 0.0d0
  WORK = 0.0d0
  call DGEHD2(N, 1, N, A, LDA, TAU, WORK, INFO)
  call begin_test('n_two')
  call print_matrix('A', A, LDA, N, N)
  call print_array('TAU', TAU, N-1)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 6: ILO=IHI (nothing to reduce)
  N = 4
  LDA = 4
  A(1) = 1.0d0; A(5) = 2.0d0; A(9)  = 3.0d0; A(13) = 4.0d0
  A(2) = 0.0d0; A(6) = 5.0d0; A(10) = 6.0d0; A(14) = 7.0d0
  A(3) = 0.0d0; A(7) = 0.0d0; A(11) = 9.0d0; A(15) = 10.0d0
  A(4) = 0.0d0; A(8) = 0.0d0; A(12) = 0.0d0; A(16) = 11.0d0
  TAU = 0.0d0
  WORK = 0.0d0
  call DGEHD2(N, 2, 2, A, LDA, TAU, WORK, INFO)
  call begin_test('ilo_eq_ihi')
  call print_matrix('A', A, LDA, N, N)
  call print_int('INFO', INFO)
  call end_test()

end program
