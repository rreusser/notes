program test_zgeqrt2
  use test_utils
  implicit none

  ! Use exact-size arrays per test (LDA = M, LDT = N) so the printed buffer maps
  ! directly onto a packed Complex128Array used in JS tests.

  integer :: INFO

  ! Test 1: M=4, N=3 (well-conditioned tall) — complex
  call run_4_3_complex()

  ! Test 2: M=N=3 (square) — complex
  call run_3_3_complex()

  ! Test 3: M=5, N=2 (very tall) — complex
  call run_5_2_complex()

  ! Test 4: M=N=1 (degenerate scalar) — complex
  call run_1_1_complex()

  ! Test 5: M=3, N=1 (single column tall) — complex
  call run_3_1_complex()

  ! Test 6: M=N=4 (square, larger) — complex
  call run_4_4_complex()

  ! Test 7: M=N=2 — purely real (all imaginary parts zero) to exercise the
  ! "alphi == 0" branch of zlarfg via this routine.
  call run_2_2_real()

contains

  subroutine run_4_3_complex()
    complex*16 :: A(4, 3), T(3, 3)
    double precision :: A_r(2, 4, 3), T_r(2, 3, 3)
    equivalence (A, A_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0)
    A(1,1) = (2.0d0, 0.5d0); A(1,2) = (1.0d0, -0.2d0); A(1,3) = (3.0d0, 0.1d0)
    A(2,1) = (1.0d0, -0.3d0); A(2,2) = (4.0d0, 0.4d0); A(2,3) = (2.0d0, -0.1d0)
    A(3,1) = (3.0d0, 0.2d0); A(3,2) = (2.0d0, 0.1d0); A(3,3) = (5.0d0, 0.3d0)
    A(4,1) = (1.0d0, -0.4d0); A(4,2) = (3.0d0, -0.2d0); A(4,3) = (1.0d0, 0.5d0)
    call ZGEQRT2(4, 3, A, 4, T, 3, INFO)
    call begin_test('m4_n3_complex')
    call print_array('A', A_r, 2*4*3)
    call print_array('T', T_r, 2*3*3)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_3_3_complex()
    complex*16 :: A(3, 3), T(3, 3)
    double precision :: A_r(2, 3, 3), T_r(2, 3, 3)
    equivalence (A, A_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0)
    A(1,1) = (4.0d0, 0.1d0); A(1,2) = (1.0d0, -0.2d0); A(1,3) = (2.0d0, 0.3d0)
    A(2,1) = (1.0d0, 0.2d0); A(2,2) = (5.0d0, -0.1d0); A(2,3) = (3.0d0, 0.4d0)
    A(3,1) = (2.0d0, -0.3d0); A(3,2) = (3.0d0, 0.5d0); A(3,3) = (6.0d0, 0.2d0)
    call ZGEQRT2(3, 3, A, 3, T, 3, INFO)
    call begin_test('m3_n3_complex')
    call print_array('A', A_r, 2*3*3)
    call print_array('T', T_r, 2*3*3)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_5_2_complex()
    complex*16 :: A(5, 2), T(2, 2)
    double precision :: A_r(2, 5, 2), T_r(2, 2, 2)
    equivalence (A, A_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0)
    A(1,1) = (1.0d0, 0.2d0); A(1,2) = (2.0d0, -0.1d0)
    A(2,1) = (3.0d0, -0.4d0); A(2,2) = (4.0d0, 0.3d0)
    A(3,1) = (5.0d0, 0.1d0); A(3,2) = (6.0d0, -0.2d0)
    A(4,1) = (2.0d0, 0.5d0); A(4,2) = (1.0d0, -0.3d0)
    A(5,1) = (4.0d0, -0.1d0); A(5,2) = (3.0d0, 0.4d0)
    call ZGEQRT2(5, 2, A, 5, T, 2, INFO)
    call begin_test('m5_n2_complex')
    call print_array('A', A_r, 2*5*2)
    call print_array('T', T_r, 2*2*2)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_1_1_complex()
    complex*16 :: A(1, 1), T(1, 1)
    double precision :: A_r(2, 1, 1), T_r(2, 1, 1)
    equivalence (A, A_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0)
    A(1,1) = (7.5d0, -1.5d0)
    call ZGEQRT2(1, 1, A, 1, T, 1, INFO)
    call begin_test('m1_n1_complex')
    call print_array('A', A_r, 2*1*1)
    call print_array('T', T_r, 2*1*1)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_3_1_complex()
    complex*16 :: A(3, 1), T(1, 1)
    double precision :: A_r(2, 3, 1), T_r(2, 1, 1)
    equivalence (A, A_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0)
    A(1,1) = (1.0d0, 0.3d0)
    A(2,1) = (2.0d0, -0.4d0)
    A(3,1) = (2.0d0, 0.5d0)
    call ZGEQRT2(3, 1, A, 3, T, 1, INFO)
    call begin_test('m3_n1_complex')
    call print_array('A', A_r, 2*3*1)
    call print_array('T', T_r, 2*1*1)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_4_4_complex()
    complex*16 :: A(4, 4), T(4, 4)
    double precision :: A_r(2, 4, 4), T_r(2, 4, 4)
    equivalence (A, A_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0)
    A(1,1) = (2.0d0, 0.1d0); A(1,2) = (0.5d0, -0.2d0); A(1,3) = (1.0d0, 0.1d0); A(1,4) = (0.3d0, -0.05d0)
    A(2,1) = (1.0d0, -0.1d0); A(2,2) = (3.0d0, 0.2d0); A(2,3) = (0.7d0, -0.3d0); A(2,4) = (0.4d0, 0.1d0)
    A(3,1) = (0.5d0, 0.2d0); A(3,2) = (0.8d0, 0.1d0); A(3,3) = (4.0d0, -0.2d0); A(3,4) = (0.6d0, 0.3d0)
    A(4,1) = (0.2d0, -0.3d0); A(4,2) = (0.9d0, 0.4d0); A(4,3) = (1.2d0, 0.1d0); A(4,4) = (5.0d0, -0.1d0)
    call ZGEQRT2(4, 4, A, 4, T, 4, INFO)
    call begin_test('m4_n4_complex')
    call print_array('A', A_r, 2*4*4)
    call print_array('T', T_r, 2*4*4)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_2_2_real()
    complex*16 :: A(2, 2), T(2, 2)
    double precision :: A_r(2, 2, 2), T_r(2, 2, 2)
    equivalence (A, A_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0)
    A(1,1) = (3.0d0, 0.0d0); A(1,2) = (1.0d0, 0.0d0)
    A(2,1) = (4.0d0, 0.0d0); A(2,2) = (2.0d0, 0.0d0)
    call ZGEQRT2(2, 2, A, 2, T, 2, INFO)
    call begin_test('m2_n2_real')
    call print_array('A', A_r, 2*2*2)
    call print_array('T', T_r, 2*2*2)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

end program
