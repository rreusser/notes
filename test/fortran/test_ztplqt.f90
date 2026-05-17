program test_ztplqt
  use test_utils
  implicit none

  ! Use exact-size arrays per test (LDA = M, LDB = M, LDT = MB) so the printed
  ! buffer maps directly onto a packed Complex128Array used in JS tests.

  integer :: INFO

  ! Test 1: M=4, N=5, L=0, MB=2 (B fully rectangular, two blocks), complex
  call run_4_5_0_2_complex()

  ! Test 2: M=4, N=5, L=2, MB=2 (B has trapezoidal block, two blocks), complex
  call run_4_5_2_2_complex()

  ! Test 3: M=5, N=4, L=0, MB=2 (tall, three blocks 2,2,1), complex
  call run_5_4_0_2_complex()

  ! Test 4: M=5, N=5, L=5, MB=2 (B fully lower triangular), complex
  call run_5_5_5_2_complex()

  ! Test 5: MB=1 (every block is unblocked; exercises pure unblocked-driver path)
  call run_3_4_3_1_complex()

  ! Test 6: MB=M (single block, no blocked update branch)
  call run_3_3_0_3_complex()

  ! Test 7: M=0 (quick return)
  call run_m_zero()

  ! Test 8: N=0 (quick return)
  call run_n_zero()

  ! Test 9: M=6, N=4, L=0, MB=3 (two equal-sized blocks of size 3), real-valued (im=0)
  call run_6_4_0_3_real()

contains

  subroutine run_4_5_0_2_complex()
    complex*16 :: A(4, 4), B(4, 5), T(2, 4)
    double precision :: A_r(2, 4, 4), B_r(2, 4, 5), T_r(2, 2, 4)
    complex*16 :: WORK(2*4)
    equivalence (A, A_r); equivalence (B, B_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    A(1,1) = (2.0d0, 0.1d0)
    A(2,1) = (0.5d0, -0.2d0); A(2,2) = (3.0d0, 0.3d0)
    A(3,1) = (0.25d0, 0.1d0); A(3,2) = (0.75d0, -0.1d0); A(3,3) = (4.0d0, 0.2d0)
    A(4,1) = (0.1d0, 0.2d0); A(4,2) = (0.2d0, 0.1d0); A(4,3) = (0.3d0, -0.3d0); A(4,4) = (5.0d0, 0.4d0)
    B(1,1) = (1.0d0, 0.3d0); B(1,2) = (0.5d0, 0.2d0); B(1,3) = (0.25d0, -0.1d0); B(1,4) = (0.1d0, 0.05d0); B(1,5) = (0.05d0, 0.0d0)
    B(2,1) = (0.3d0, -0.1d0); B(2,2) = (1.1d0, 0.4d0); B(2,3) = (0.6d0, 0.1d0); B(2,4) = (0.2d0, -0.2d0); B(2,5) = (0.4d0, 0.3d0)
    B(3,1) = (0.7d0, 0.2d0); B(3,2) = (0.4d0, -0.2d0); B(3,3) = (1.2d0, 0.3d0); B(3,4) = (0.9d0, 0.1d0); B(3,5) = (0.15d0, -0.05d0)
    B(4,1) = (0.2d0, 0.1d0); B(4,2) = (0.3d0, 0.0d0); B(4,3) = (0.4d0, 0.2d0); B(4,4) = (1.3d0, -0.4d0); B(4,5) = (0.6d0, 0.2d0)
    call ZTPLQT(4, 5, 0, 2, A, 4, B, 4, T, 2, WORK, INFO)
    call begin_test('m4_n5_l0_mb2_complex')
    call print_array('A', A_r, 2*4*4)
    call print_array('B', B_r, 2*4*5)
    call print_array('T', T_r, 2*2*4)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_4_5_2_2_complex()
    complex*16 :: A(4, 4), B(4, 5), T(2, 4)
    double precision :: A_r(2, 4, 4), B_r(2, 4, 5), T_r(2, 2, 4)
    complex*16 :: WORK(2*4)
    equivalence (A, A_r); equivalence (B, B_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    A(1,1) = (2.0d0, 0.1d0)
    A(2,1) = (0.5d0, -0.2d0); A(2,2) = (3.0d0, 0.3d0)
    A(3,1) = (0.25d0, 0.1d0); A(3,2) = (0.75d0, -0.1d0); A(3,3) = (4.0d0, 0.2d0)
    A(4,1) = (0.1d0, 0.2d0); A(4,2) = (0.2d0, 0.1d0); A(4,3) = (0.3d0, -0.3d0); A(4,4) = (5.0d0, 0.4d0)
    ! B is M-by-N pentagonal with last L=2 columns lower-trapezoidal:
    !   Column N-L+i .. N for row i has nonzero. The first N-L columns are full.
    B(1,1) = (1.0d0, 0.3d0); B(1,2) = (0.5d0, 0.2d0); B(1,3) = (0.25d0, -0.1d0); B(1,4) = (0.1d0, 0.05d0)
    B(2,1) = (0.3d0, -0.1d0); B(2,2) = (1.1d0, 0.4d0); B(2,3) = (0.6d0, 0.1d0); B(2,4) = (0.2d0, -0.2d0); B(2,5) = (0.4d0, 0.3d0)
    B(3,1) = (0.7d0, 0.2d0); B(3,2) = (0.4d0, -0.2d0); B(3,3) = (1.2d0, 0.3d0); B(3,4) = (0.9d0, 0.1d0); B(3,5) = (0.15d0, -0.05d0)
    B(4,1) = (0.2d0, 0.1d0); B(4,2) = (0.3d0, 0.0d0); B(4,3) = (0.4d0, 0.2d0); B(4,4) = (1.3d0, -0.4d0); B(4,5) = (0.6d0, 0.2d0)
    call ZTPLQT(4, 5, 2, 2, A, 4, B, 4, T, 2, WORK, INFO)
    call begin_test('m4_n5_l2_mb2_complex')
    call print_array('A', A_r, 2*4*4)
    call print_array('B', B_r, 2*4*5)
    call print_array('T', T_r, 2*2*4)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_5_4_0_2_complex()
    complex*16 :: A(5, 5), B(5, 4), T(2, 5)
    double precision :: A_r(2, 5, 5), B_r(2, 5, 4), T_r(2, 2, 5)
    complex*16 :: WORK(2*5)
    equivalence (A, A_r); equivalence (B, B_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    A(1,1) = (2.0d0, 0.1d0)
    A(2,1) = (0.5d0, -0.2d0); A(2,2) = (3.0d0, 0.3d0)
    A(3,1) = (0.25d0, 0.1d0); A(3,2) = (0.75d0, -0.1d0); A(3,3) = (4.0d0, 0.2d0)
    A(4,1) = (0.1d0, 0.2d0); A(4,2) = (0.2d0, 0.1d0); A(4,3) = (0.3d0, -0.3d0); A(4,4) = (5.0d0, 0.4d0)
    A(5,1) = (0.05d0, 0.1d0); A(5,2) = (0.15d0, -0.1d0); A(5,3) = (0.25d0, 0.0d0); A(5,4) = (0.35d0, 0.2d0); A(5,5) = (6.0d0, -0.3d0)
    B(1,1) = (1.0d0, 0.3d0); B(1,2) = (0.5d0, 0.2d0); B(1,3) = (0.25d0, -0.1d0); B(1,4) = (0.1d0, 0.05d0)
    B(2,1) = (0.3d0, -0.1d0); B(2,2) = (1.1d0, 0.4d0); B(2,3) = (0.6d0, 0.1d0); B(2,4) = (0.2d0, -0.2d0)
    B(3,1) = (0.7d0, 0.2d0); B(3,2) = (0.4d0, -0.2d0); B(3,3) = (1.2d0, 0.3d0); B(3,4) = (0.9d0, 0.1d0)
    B(4,1) = (0.2d0, 0.1d0); B(4,2) = (0.3d0, 0.0d0); B(4,3) = (0.4d0, 0.2d0); B(4,4) = (1.3d0, -0.4d0)
    B(5,1) = (0.15d0, 0.05d0); B(5,2) = (0.25d0, 0.1d0); B(5,3) = (0.35d0, -0.1d0); B(5,4) = (0.45d0, 0.2d0)
    call ZTPLQT(5, 4, 0, 2, A, 5, B, 5, T, 2, WORK, INFO)
    call begin_test('m5_n4_l0_mb2_complex')
    call print_array('A', A_r, 2*5*5)
    call print_array('B', B_r, 2*5*4)
    call print_array('T', T_r, 2*2*5)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_5_5_5_2_complex()
    complex*16 :: A(5, 5), B(5, 5), T(2, 5)
    double precision :: A_r(2, 5, 5), B_r(2, 5, 5), T_r(2, 2, 5)
    complex*16 :: WORK(2*5)
    equivalence (A, A_r); equivalence (B, B_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    A(1,1) = (2.0d0, 0.1d0)
    A(2,1) = (0.5d0, -0.2d0); A(2,2) = (3.0d0, 0.3d0)
    A(3,1) = (0.25d0, 0.1d0); A(3,2) = (0.75d0, -0.1d0); A(3,3) = (4.0d0, 0.2d0)
    A(4,1) = (0.1d0, 0.2d0); A(4,2) = (0.2d0, 0.1d0); A(4,3) = (0.3d0, -0.3d0); A(4,4) = (5.0d0, 0.4d0)
    A(5,1) = (0.05d0, 0.1d0); A(5,2) = (0.15d0, -0.1d0); A(5,3) = (0.25d0, 0.0d0); A(5,4) = (0.35d0, 0.2d0); A(5,5) = (6.0d0, -0.3d0)
    B(1,1) = (1.0d0, 0.3d0)
    B(2,1) = (0.3d0, -0.1d0); B(2,2) = (1.1d0, 0.4d0)
    B(3,1) = (0.7d0, 0.2d0); B(3,2) = (0.4d0, -0.2d0); B(3,3) = (1.2d0, 0.3d0)
    B(4,1) = (0.2d0, 0.1d0); B(4,2) = (0.3d0, 0.0d0); B(4,3) = (0.4d0, 0.2d0); B(4,4) = (1.3d0, -0.4d0)
    B(5,1) = (0.15d0, 0.05d0); B(5,2) = (0.25d0, 0.1d0); B(5,3) = (0.35d0, -0.1d0); B(5,4) = (0.45d0, 0.2d0); B(5,5) = (1.4d0, 0.3d0)
    call ZTPLQT(5, 5, 5, 2, A, 5, B, 5, T, 2, WORK, INFO)
    call begin_test('m5_n5_l5_mb2_complex')
    call print_array('A', A_r, 2*5*5)
    call print_array('B', B_r, 2*5*5)
    call print_array('T', T_r, 2*2*5)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_3_4_3_1_complex()
    complex*16 :: A(3, 3), B(3, 4), T(1, 3)
    double precision :: A_r(2, 3, 3), B_r(2, 3, 4), T_r(2, 1, 3)
    complex*16 :: WORK(1*3)
    equivalence (A, A_r); equivalence (B, B_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    A(1,1) = (2.0d0, 0.1d0)
    A(2,1) = (0.5d0, -0.2d0); A(2,2) = (3.0d0, 0.3d0)
    A(3,1) = (0.25d0, 0.1d0); A(3,2) = (0.75d0, -0.1d0); A(3,3) = (4.0d0, 0.2d0)
    B(1,1) = (1.0d0, 0.3d0); B(1,2) = (0.5d0, 0.2d0)
    B(2,1) = (0.3d0, -0.1d0); B(2,2) = (1.1d0, 0.4d0); B(2,3) = (0.6d0, 0.1d0)
    B(3,1) = (0.7d0, 0.2d0); B(3,2) = (0.4d0, -0.2d0); B(3,3) = (1.2d0, 0.3d0); B(3,4) = (0.9d0, 0.1d0)
    call ZTPLQT(3, 4, 3, 1, A, 3, B, 3, T, 1, WORK, INFO)
    call begin_test('m3_n4_l3_mb1_complex')
    call print_array('A', A_r, 2*3*3)
    call print_array('B', B_r, 2*3*4)
    call print_array('T', T_r, 2*1*3)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_3_3_0_3_complex()
    complex*16 :: A(3, 3), B(3, 3), T(3, 3)
    double precision :: A_r(2, 3, 3), B_r(2, 3, 3), T_r(2, 3, 3)
    complex*16 :: WORK(3*3)
    equivalence (A, A_r); equivalence (B, B_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    A(1,1) = (2.0d0, 0.1d0)
    A(2,1) = (0.5d0, -0.2d0); A(2,2) = (3.0d0, 0.3d0)
    A(3,1) = (0.25d0, 0.1d0); A(3,2) = (0.75d0, -0.1d0); A(3,3) = (4.0d0, 0.2d0)
    B(1,1) = (1.0d0, 0.3d0); B(1,2) = (0.5d0, 0.2d0); B(1,3) = (0.25d0, -0.1d0)
    B(2,1) = (0.3d0, -0.1d0); B(2,2) = (1.1d0, 0.4d0); B(2,3) = (0.6d0, 0.1d0)
    B(3,1) = (0.7d0, 0.2d0); B(3,2) = (0.4d0, -0.2d0); B(3,3) = (1.2d0, 0.3d0)
    call ZTPLQT(3, 3, 0, 3, A, 3, B, 3, T, 3, WORK, INFO)
    call begin_test('m3_n3_l0_mb3_complex')
    call print_array('A', A_r, 2*3*3)
    call print_array('B', B_r, 2*3*3)
    call print_array('T', T_r, 2*3*3)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_m_zero()
    complex*16 :: A(1, 1), B(1, 3), T(1, 1)
    complex*16 :: WORK(1)
    A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    call ZTPLQT(0, 3, 0, 1, A, 1, B, 1, T, 1, WORK, INFO)
    call begin_test('m_zero')
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_n_zero()
    complex*16 :: A(3, 3), B(3, 1), T(2, 3)
    complex*16 :: WORK(2*3)
    A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    call ZTPLQT(3, 0, 0, 2, A, 3, B, 3, T, 2, WORK, INFO)
    call begin_test('n_zero')
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_6_4_0_3_real()
    complex*16 :: A(6, 6), B(6, 4), T(3, 6)
    double precision :: A_r(2, 6, 6), B_r(2, 6, 4), T_r(2, 3, 6)
    complex*16 :: WORK(3*6)
    equivalence (A, A_r); equivalence (B, B_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    A(1,1) = (2.0d0, 0.0d0)
    A(2,1) = (0.5d0, 0.0d0);  A(2,2) = (3.0d0, 0.0d0)
    A(3,1) = (0.25d0, 0.0d0); A(3,2) = (0.75d0, 0.0d0); A(3,3) = (4.0d0, 0.0d0)
    A(4,1) = (0.1d0, 0.0d0);  A(4,2) = (0.2d0, 0.0d0);  A(4,3) = (0.3d0, 0.0d0);  A(4,4) = (5.0d0, 0.0d0)
    A(5,1) = (0.05d0, 0.0d0); A(5,2) = (0.15d0, 0.0d0); A(5,3) = (0.25d0, 0.0d0); A(5,4) = (0.35d0, 0.0d0); A(5,5) = (6.0d0, 0.0d0)
    A(6,1) = (0.04d0, 0.0d0); A(6,2) = (0.14d0, 0.0d0); A(6,3) = (0.24d0, 0.0d0); A(6,4) = (0.34d0, 0.0d0); A(6,5) = (0.44d0, 0.0d0); A(6,6) = (7.0d0, 0.0d0)
    B(1,1) = (1.0d0, 0.0d0);  B(1,2) = (0.5d0, 0.0d0);  B(1,3) = (0.25d0, 0.0d0); B(1,4) = (0.1d0, 0.0d0)
    B(2,1) = (0.3d0, 0.0d0);  B(2,2) = (1.1d0, 0.0d0);  B(2,3) = (0.6d0, 0.0d0);  B(2,4) = (0.2d0, 0.0d0)
    B(3,1) = (0.7d0, 0.0d0);  B(3,2) = (0.4d0, 0.0d0);  B(3,3) = (1.2d0, 0.0d0);  B(3,4) = (0.9d0, 0.0d0)
    B(4,1) = (0.2d0, 0.0d0);  B(4,2) = (0.3d0, 0.0d0);  B(4,3) = (0.4d0, 0.0d0);  B(4,4) = (1.3d0, 0.0d0)
    B(5,1) = (0.15d0, 0.0d0); B(5,2) = (0.25d0, 0.0d0); B(5,3) = (0.35d0, 0.0d0); B(5,4) = (0.45d0, 0.0d0)
    B(6,1) = (0.13d0, 0.0d0); B(6,2) = (0.23d0, 0.0d0); B(6,3) = (0.33d0, 0.0d0); B(6,4) = (0.43d0, 0.0d0)
    call ZTPLQT(6, 4, 0, 3, A, 6, B, 6, T, 3, WORK, INFO)
    call begin_test('m6_n4_l0_mb3_real')
    call print_array('A', A_r, 2*6*6)
    call print_array('B', B_r, 2*6*4)
    call print_array('T', T_r, 2*3*6)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

end program
