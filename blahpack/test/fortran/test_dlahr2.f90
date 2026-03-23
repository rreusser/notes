program test_dlahr2
  use test_utils
  implicit none

  integer :: N, K, NB, LDA, LDT, LDY, i
  double precision :: A(40*40), TAU(40), T(40*40), Y(40*40)

  ! Test 1: Basic 6x6 matrix, K=1, NB=2
  ! This is the typical calling pattern from dgehrd:
  !   N=IHI=6, K=ILO=1, NB=2
  N = 6
  K = 1
  NB = 2
  LDA = 6
  LDT = 2
  LDY = 6

  ! Fill A (column-major, 6x6)
  A(1)  = 2.0d0;  A(7)  = 1.0d0;  A(13) = 3.0d0;  A(19) = 1.0d0;  A(25) = 4.0d0;  A(31) = 2.0d0
  A(2)  = 1.0d0;  A(8)  = 4.0d0;  A(14) = 1.0d0;  A(20) = 2.0d0;  A(26) = 1.0d0;  A(32) = 3.0d0
  A(3)  = 3.0d0;  A(9)  = 1.0d0;  A(15) = 5.0d0;  A(21) = 1.0d0;  A(27) = 2.0d0;  A(33) = 1.0d0
  A(4)  = 1.0d0;  A(10) = 2.0d0;  A(16) = 1.0d0;  A(22) = 6.0d0;  A(28) = 1.0d0;  A(34) = 2.0d0
  A(5)  = 4.0d0;  A(11) = 1.0d0;  A(17) = 2.0d0;  A(23) = 1.0d0;  A(29) = 7.0d0;  A(35) = 1.0d0
  A(6)  = 2.0d0;  A(12) = 3.0d0;  A(18) = 1.0d0;  A(24) = 2.0d0;  A(30) = 1.0d0;  A(36) = 8.0d0

  TAU = 0.0d0
  T = 0.0d0
  Y = 0.0d0

  call DLAHR2(N, K, NB, A, LDA, TAU, T, LDT, Y, LDY)
  call begin_test('basic_6x6_k1_nb2')
  call print_matrix('A', A, LDA, N, N)
  call print_array('TAU', TAU, NB)
  call print_matrix('T', T, LDT, NB, NB)
  call print_matrix('Y', Y, LDY, N, NB)
  call end_test()

  ! Test 2: 8x8 matrix, K=1, NB=3
  N = 8
  K = 1
  NB = 3
  LDA = 8
  LDT = 3
  LDY = 8

  A = 0.0d0
  do i = 1, N
    A((i-1)*N + i) = dble(i + 1)
  end do
  ! Off-diagonal entries
  A(2)  = 1.0d0; A(3)  = 2.0d0; A(4)  = 0.5d0
  A(9)  = 1.5d0; A(10) = 1.0d0; A(11) = 0.5d0
  A(17) = 2.0d0; A(18) = 0.5d0; A(19) = 1.0d0
  A(25) = 1.0d0; A(26) = 0.5d0; A(27) = 2.0d0
  A(33) = 0.5d0; A(34) = 1.0d0; A(35) = 0.5d0
  A(41) = 2.0d0; A(42) = 0.5d0; A(43) = 1.0d0
  A(49) = 1.0d0; A(50) = 2.0d0; A(51) = 0.5d0
  A(5)  = 1.0d0; A(6)  = 0.5d0; A(7)  = 2.0d0; A(8)  = 1.0d0

  TAU = 0.0d0
  T = 0.0d0
  Y = 0.0d0

  call DLAHR2(N, K, NB, A, LDA, TAU, T, LDT, Y, LDY)
  call begin_test('8x8_k1_nb3')
  call print_matrix('A', A, LDA, N, N)
  call print_array('TAU', TAU, NB)
  call print_matrix('T', T, LDT, NB, NB)
  call print_matrix('Y', Y, LDY, N, NB)
  call end_test()

  ! Test 3: NB=1 (single column reduction)
  N = 5
  K = 1
  NB = 1
  LDA = 5
  LDT = 1
  LDY = 5

  A(1)  = 1.0d0;  A(6)  = 2.0d0;  A(11) = 3.0d0;  A(16) = 4.0d0;  A(21) = 5.0d0
  A(2)  = 6.0d0;  A(7)  = 7.0d0;  A(12) = 8.0d0;  A(17) = 9.0d0;  A(22) = 10.0d0
  A(3)  = 11.0d0; A(8)  = 12.0d0; A(13) = 13.0d0; A(18) = 14.0d0; A(23) = 15.0d0
  A(4)  = 16.0d0; A(9)  = 17.0d0; A(14) = 18.0d0; A(19) = 19.0d0; A(24) = 20.0d0
  A(5)  = 21.0d0; A(10) = 22.0d0; A(15) = 23.0d0; A(20) = 24.0d0; A(25) = 25.0d0

  TAU = 0.0d0
  T = 0.0d0
  Y = 0.0d0

  call DLAHR2(N, K, NB, A, LDA, TAU, T, LDT, Y, LDY)
  call begin_test('5x5_k1_nb1')
  call print_matrix('A', A, LDA, N, N)
  call print_array('TAU', TAU, NB)
  call print_matrix('T', T, LDT, NB, NB)
  call print_matrix('Y', Y, LDY, N, NB)
  call end_test()

  ! Test 4: N=1 (quick return)
  N = 1
  K = 1
  NB = 1
  LDA = 1
  LDT = 1
  LDY = 1
  A(1) = 5.0d0
  TAU(1) = 99.0d0
  T(1) = 99.0d0
  Y(1) = 99.0d0
  call DLAHR2(N, K, NB, A, LDA, TAU, T, LDT, Y, LDY)
  call begin_test('n_one')
  call print_matrix('A', A, LDA, 1, 1)
  call print_scalar('TAU1', TAU(1))
  call end_test()

  ! Test 5: K=2 (non-unit K offset)
  N = 7
  K = 2
  NB = 2
  LDA = 7
  LDT = 2
  LDY = 7

  A = 0.0d0
  do i = 1, N
    A((i-1)*N + i) = dble(i * 2)
  end do
  A(2)  = 1.0d0; A(3)  = 2.0d0; A(4)  = 0.5d0; A(5)  = 1.0d0
  A(9)  = 1.5d0; A(10) = 1.0d0; A(11) = 0.5d0; A(12) = 2.0d0
  A(16) = 2.0d0; A(17) = 0.5d0; A(18) = 1.0d0; A(19) = 0.5d0
  A(23) = 1.0d0; A(24) = 0.5d0; A(25) = 2.0d0; A(26) = 1.0d0
  A(30) = 0.5d0; A(31) = 1.0d0; A(32) = 0.5d0; A(33) = 2.0d0
  A(37) = 2.0d0; A(38) = 0.5d0; A(39) = 1.0d0; A(40) = 0.5d0
  A(44) = 1.0d0; A(45) = 2.0d0; A(46) = 0.5d0; A(47) = 1.0d0
  A(6)  = 0.5d0; A(7)  = 1.0d0
  A(13) = 2.0d0; A(14) = 0.5d0
  A(20) = 1.0d0; A(21) = 2.0d0
  A(27) = 0.5d0; A(28) = 1.0d0
  A(34) = 2.0d0; A(35) = 0.5d0
  A(41) = 1.0d0; A(42) = 2.0d0
  A(48) = 0.5d0; A(49) = 1.0d0

  TAU = 0.0d0
  T = 0.0d0
  Y = 0.0d0

  call DLAHR2(N, K, NB, A, LDA, TAU, T, LDT, Y, LDY)
  call begin_test('7x7_k2_nb2')
  call print_matrix('A', A, LDA, N, N)
  call print_array('TAU', TAU, NB)
  call print_matrix('T', T, LDT, NB, NB)
  call print_matrix('Y', Y, LDY, N, NB)
  call end_test()

end program
