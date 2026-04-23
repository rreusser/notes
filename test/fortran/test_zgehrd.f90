program test_zgehrd
  use test_utils
  implicit none

  integer :: INFO, N, LDA, LWORK, i
  double precision :: a_r(3200), tau_r(80), work_r(80000)
  complex*16 :: A(1600), TAU(40), WORK(40000)
  equivalence (A, a_r)
  equivalence (TAU, tau_r)
  equivalence (WORK, work_r)

  ! Test 1: 4x4 full range (ILO=1, IHI=4)
  N = 4
  LDA = 4
  LWORK = 4000
  A = (0.0d0, 0.0d0)
  A(1)  = (1.0d0, 0.5d0);  A(5)  = (2.0d0, -1.0d0); A(9)  = (3.0d0, 0.0d0);  A(13) = (4.0d0, 1.0d0)
  A(2)  = (5.0d0, -0.5d0); A(6)  = (6.0d0, 1.0d0);  A(10) = (7.0d0, -2.0d0); A(14) = (8.0d0, 0.0d0)
  A(3)  = (9.0d0, 1.0d0);  A(7)  = (10.0d0, 0.0d0); A(11) = (11.0d0, 1.0d0); A(15) = (12.0d0, -1.0d0)
  A(4)  = (13.0d0, -1.0d0);A(8)  = (14.0d0, 2.0d0); A(12) = (15.0d0, 0.5d0); A(16) = (16.0d0, 0.0d0)
  TAU = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0)
  call ZGEHRD(N, 1, N, A, LDA, TAU, WORK, LWORK, INFO)
  call begin_test('4x4_full')
  call print_array('A', a_r, 2*N*N)
  call print_array('TAU', tau_r, 2*(N-1))
  call print_int('INFO', INFO)
  call end_test()

  ! Test 2: 6x6 full range
  N = 6
  LDA = 6
  A = (0.0d0, 0.0d0)
  A(1) = (1.0d0, 0.0d0);  A(7)  = (2.0d0, 1.0d0);  A(13) = (3.0d0, -1.0d0);A(19) = (4.0d0, 0.5d0); A(25) = (5.0d0, 0.0d0); A(31) = (6.0d0, -0.5d0)
  A(2) = (7.0d0, 1.0d0);  A(8)  = (8.0d0, 0.0d0);  A(14) = (9.0d0, 0.0d0); A(20) = (10.0d0, -1.0d0);A(26) = (11.0d0, 1.0d0);A(32) = (12.0d0, 0.0d0)
  A(3) = (13.0d0, -1.0d0);A(9)  = (14.0d0, 0.5d0); A(15) = (15.0d0, 0.0d0);A(21) = (16.0d0, 0.0d0);A(27) = (17.0d0, -1.0d0);A(33) = (18.0d0, 0.5d0)
  A(4) = (19.0d0, 0.0d0); A(10) = (20.0d0, -1.0d0);A(16) = (21.0d0, 1.0d0);A(22) = (22.0d0, 0.0d0);A(28) = (23.0d0, 0.0d0);A(34) = (24.0d0, -1.0d0)
  A(5) = (25.0d0, 0.5d0); A(11) = (26.0d0, 0.0d0); A(17) = (27.0d0, -0.5d0);A(23) = (28.0d0, 1.0d0);A(29) = (29.0d0, 0.0d0);A(35) = (30.0d0, 0.0d0)
  A(6) = (31.0d0, -1.0d0);A(12) = (32.0d0, 1.0d0); A(18) = (33.0d0, 0.0d0);A(24) = (34.0d0, -0.5d0);A(30) = (35.0d0, 0.5d0);A(36) = (36.0d0, 0.0d0)
  TAU = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0)
  call ZGEHRD(N, 1, N, A, LDA, TAU, WORK, LWORK, INFO)
  call begin_test('6x6_full')
  call print_array('A', a_r, 2*N*N)
  call print_array('TAU', tau_r, 2*(N-1))
  call print_int('INFO', INFO)
  call end_test()

  ! Test 3: 4x4 partial (ILO=2, IHI=3)
  N = 4
  LDA = 4
  A = (0.0d0, 0.0d0)
  A(1) = (1.0d0, 0.0d0); A(5) = (2.0d0, 0.0d0); A(9)  = (3.0d0, 0.0d0); A(13) = (4.0d0, 0.0d0)
  A(2) = (0.0d0, 0.0d0); A(6) = (5.0d0, 1.0d0); A(10) = (6.0d0, -1.0d0);A(14) = (7.0d0, 0.0d0)
  A(3) = (0.0d0, 0.0d0); A(7) = (8.0d0, -0.5d0);A(11) = (9.0d0, 0.0d0); A(15) = (10.0d0, 1.0d0)
  A(4) = (0.0d0, 0.0d0); A(8) = (0.0d0, 0.0d0); A(12) = (0.0d0, 0.0d0); A(16) = (11.0d0, 0.0d0)
  TAU = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0)
  call ZGEHRD(N, 2, 3, A, LDA, TAU, WORK, LWORK, INFO)
  call begin_test('4x4_partial_ilo2_ihi3')
  call print_array('A', a_r, 2*N*N)
  call print_array('TAU', tau_r, 2*(N-1))
  call print_int('INFO', INFO)
  call end_test()

  ! Test 4: N=1 (quick return)
  N = 1
  LDA = 1
  A(1) = (42.0d0, 3.0d0)
  TAU = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0)
  call ZGEHRD(N, 1, 1, A, LDA, TAU, WORK, LWORK, INFO)
  call begin_test('n_one')
  call print_array('A', a_r, 2)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 5: N=2
  N = 2
  LDA = 2
  A(1) = (3.0d0, 1.0d0); A(3) = (1.0d0, -1.0d0)
  A(2) = (4.0d0, -2.0d0);A(4) = (2.0d0, 0.5d0)
  TAU = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0)
  call ZGEHRD(N, 1, N, A, LDA, TAU, WORK, LWORK, INFO)
  call begin_test('n_two')
  call print_array('A', a_r, 2*N*N)
  call print_array('TAU', tau_r, 2)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 6: 8x8 full range (exercises blocked path if NB is small enough)
  N = 8
  LDA = 8
  A = (0.0d0, 0.0d0)
  ! Fill an 8x8 matrix with interesting complex values
  do i = 1, N*N
    A(i) = dcmplx(dble(i), dble(mod(i*7+3, 11) - 5) * 0.5d0)
  end do
  TAU = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0)
  call ZGEHRD(N, 1, N, A, LDA, TAU, WORK, LWORK, INFO)
  call begin_test('8x8_full')
  call print_array('A', a_r, 2*N*N)
  call print_array('TAU', tau_r, 2*(N-1))
  call print_int('INFO', INFO)
  call end_test()

  ! Test 7: 40x40 full range (exercises blocked path with NB=32)
  N = 40
  LDA = 40
  A = (0.0d0, 0.0d0)
  do i = 1, N*N
    A(i) = dcmplx(dble(mod(i*13+7, 97)) / 10.0d0, dble(mod(i*7+3, 53) - 26) / 20.0d0)
  end do
  TAU = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0)
  call ZGEHRD(N, 1, N, A, LDA, TAU, WORK, LWORK, INFO)
  call begin_test('40x40_full')
  call print_array('A', a_r, 2*N*N)
  call print_array('TAU', tau_r, 2*(N-1))
  call print_int('INFO', INFO)
  call end_test()

  ! Test 8: 5x5 with ILO=1, IHI=5
  N = 5
  LDA = 5
  A = (0.0d0, 0.0d0)
  A(1) = (2.0d0, 1.0d0); A(6)  = (1.0d0, -0.5d0); A(11) = (3.0d0, 0.0d0);  A(16) = (1.0d0, 1.0d0);  A(21) = (4.0d0, -1.0d0)
  A(2) = (1.0d0, 0.5d0); A(7)  = (4.0d0, 0.0d0);  A(12) = (1.0d0, -1.0d0); A(17) = (2.0d0, 0.0d0);  A(22) = (1.0d0, 0.5d0)
  A(3) = (3.0d0, -1.0d0);A(8)  = (1.0d0, 1.0d0);  A(13) = (5.0d0, 0.5d0);  A(18) = (1.0d0, -0.5d0); A(23) = (2.0d0, 0.0d0)
  A(4) = (1.0d0, 0.0d0); A(9)  = (2.0d0, -0.5d0); A(14) = (1.0d0, 0.0d0);  A(19) = (6.0d0, 1.0d0);  A(24) = (1.0d0, -1.0d0)
  A(5) = (4.0d0, 1.0d0); A(10) = (1.0d0, 0.0d0);  A(15) = (2.0d0, 1.0d0);  A(20) = (1.0d0, 0.5d0);  A(25) = (7.0d0, 0.0d0)
  TAU = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0)
  call ZGEHRD(N, 1, N, A, LDA, TAU, WORK, LWORK, INFO)
  call begin_test('5x5_full')
  call print_array('A', a_r, 2*N*N)
  call print_array('TAU', tau_r, 2*(N-1))
  call print_int('INFO', INFO)
  call end_test()

end program
