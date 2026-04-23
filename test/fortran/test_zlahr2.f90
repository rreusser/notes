program test_zlahr2
  use test_utils
  implicit none

  integer :: N, K, NB, LDA, LDT, LDY, i
  double precision :: a_r(400), tau_r(20), t_r(200), y_r(400)
  complex*16 :: A(200), TAU(10), T(100), Y(200)
  equivalence (A, a_r)
  equivalence (TAU, tau_r)
  equivalence (T, t_r)
  equivalence (Y, y_r)

  ! Test 1: N=6, K=1, NB=2
  ! This reduces columns 1..2 of the Hessenberg reduction
  N = 6
  K = 1
  NB = 2
  LDA = 6
  LDT = 2
  LDY = 6
  A = (0.0d0, 0.0d0)
  ! Fill a 6x3 submatrix (NB+1 columns needed: columns 1..NB+1)
  ! Column 1
  A(1) = (1.0d0, 0.0d0);  A(2) = (5.0d0, -0.5d0); A(3) = (9.0d0, 1.0d0)
  A(4) = (13.0d0, -1.0d0);A(5) = (17.0d0, 0.5d0); A(6) = (21.0d0, 0.0d0)
  ! Column 2
  A(7)  = (2.0d0, 1.0d0);  A(8)  = (6.0d0, 0.0d0);  A(9)  = (10.0d0, -1.0d0)
  A(10) = (14.0d0, 0.5d0); A(11) = (18.0d0, 0.0d0); A(12) = (22.0d0, 1.0d0)
  ! Column 3
  A(13) = (3.0d0, -0.5d0); A(14) = (7.0d0, 1.0d0);  A(15) = (11.0d0, 0.0d0)
  A(16) = (15.0d0, 0.0d0); A(17) = (19.0d0, -1.0d0); A(18) = (23.0d0, 0.5d0)
  TAU = (0.0d0, 0.0d0)
  T = (0.0d0, 0.0d0)
  Y = (0.0d0, 0.0d0)
  call ZLAHR2(N, K, NB, A, LDA, TAU, T, LDT, Y, LDY)
  call begin_test('basic_n6_k1_nb2')
  call print_array('A', a_r, 2*LDA*3)
  call print_array('TAU', tau_r, 2*NB)
  call print_array('T', t_r, 2*LDT*NB)
  call print_array('Y', y_r, 2*LDY*NB)
  call end_test()

  ! Test 2: N=5, K=2, NB=2
  ! Reduces columns from offset K=2
  N = 5
  K = 2
  NB = 2
  LDA = 5
  LDT = 2
  LDY = 5
  A = (0.0d0, 0.0d0)
  ! Column 1
  A(1) = (1.0d0, 0.5d0); A(2) = (2.0d0, -1.0d0); A(3) = (3.0d0, 0.0d0)
  A(4) = (4.0d0, 1.0d0); A(5) = (5.0d0, -0.5d0)
  ! Column 2
  A(6)  = (6.0d0, 0.0d0);  A(7)  = (7.0d0, 1.0d0); A(8)  = (8.0d0, -1.0d0)
  A(9)  = (9.0d0, 0.5d0); A(10) = (10.0d0, 0.0d0)
  ! Column 3
  A(11) = (11.0d0, -0.5d0); A(12) = (12.0d0, 0.0d0); A(13) = (13.0d0, 1.0d0)
  A(14) = (14.0d0, -1.0d0); A(15) = (15.0d0, 0.5d0)
  TAU = (0.0d0, 0.0d0)
  T = (0.0d0, 0.0d0)
  Y = (0.0d0, 0.0d0)
  call ZLAHR2(N, K, NB, A, LDA, TAU, T, LDT, Y, LDY)
  call begin_test('n5_k2_nb2')
  call print_array('A', a_r, 2*LDA*3)
  call print_array('TAU', tau_r, 2*NB)
  call print_array('T', t_r, 2*LDT*NB)
  call print_array('Y', y_r, 2*LDY*NB)
  call end_test()

  ! Test 3: N=8, K=1, NB=3
  N = 8
  K = 1
  NB = 3
  LDA = 8
  LDT = 3
  LDY = 8
  A = (0.0d0, 0.0d0)
  ! Column 1
  A(1) = (1.0d0, 0.0d0); A(2) = (2.0d0, 1.0d0); A(3) = (3.0d0, -1.0d0); A(4) = (4.0d0, 0.5d0)
  A(5) = (5.0d0, 0.0d0); A(6) = (6.0d0, -0.5d0);A(7) = (7.0d0, 1.0d0); A(8) = (8.0d0, 0.0d0)
  ! Column 2
  A(9)  = (9.0d0, 0.5d0); A(10) = (10.0d0, 0.0d0); A(11) = (11.0d0, 1.0d0); A(12) = (12.0d0, -1.0d0)
  A(13) = (13.0d0, 0.0d0);A(14) = (14.0d0, 0.5d0); A(15) = (15.0d0, 0.0d0);A(16) = (16.0d0, -0.5d0)
  ! Column 3
  A(17) = (17.0d0, -1.0d0); A(18) = (18.0d0, 0.0d0); A(19) = (19.0d0, 0.5d0); A(20) = (20.0d0, 0.0d0)
  A(21) = (21.0d0, -0.5d0);A(22) = (22.0d0, 1.0d0); A(23) = (23.0d0, 0.0d0);A(24) = (24.0d0, 0.5d0)
  ! Column 4
  A(25) = (25.0d0, 0.0d0); A(26) = (26.0d0, -1.0d0); A(27) = (27.0d0, 0.5d0); A(28) = (28.0d0, 0.0d0)
  A(29) = (29.0d0, 1.0d0); A(30) = (30.0d0, 0.0d0); A(31) = (31.0d0, -0.5d0);A(32) = (32.0d0, 1.0d0)
  TAU = (0.0d0, 0.0d0)
  T = (0.0d0, 0.0d0)
  Y = (0.0d0, 0.0d0)
  call ZLAHR2(N, K, NB, A, LDA, TAU, T, LDT, Y, LDY)
  call begin_test('n8_k1_nb3')
  call print_array('A', a_r, 2*LDA*4)
  call print_array('TAU', tau_r, 2*NB)
  call print_array('T', t_r, 2*LDT*NB)
  call print_array('Y', y_r, 2*LDY*NB)
  call end_test()

  ! Test 4: N=1 (quick return)
  N = 1
  K = 1
  NB = 1
  LDA = 1
  LDT = 1
  LDY = 1
  A(1) = (42.0d0, 3.0d0)
  TAU = (0.0d0, 0.0d0)
  T = (0.0d0, 0.0d0)
  Y = (0.0d0, 0.0d0)
  call ZLAHR2(N, K, NB, A, LDA, TAU, T, LDT, Y, LDY)
  call begin_test('n_one')
  call print_array('A', a_r, 2)
  call end_test()

end program
