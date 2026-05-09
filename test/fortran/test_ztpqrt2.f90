program test_ztpqrt2
  use test_utils
  implicit none

  ! Use exact-size arrays per test (LDA = N, LDB = M, LDT = N) so the printed
  ! buffer maps directly onto a packed Complex128Array used in JS tests.

  integer :: INFO

  ! Test 1: M=4, N=3, L=0 (B fully rectangular), real-valued (im = 0)
  call run_4_3_0_real()

  ! Test 2: M=4, N=3, L=3 (B has 1 rectangular row + 3 trapezoidal), complex
  call run_4_3_3_complex()

  ! Test 3: M=N=L=3 (B fully upper triangular), complex
  call run_3_3_3_complex()

  ! Test 4: M=3, N=1, L=1, complex
  call run_3_1_1_complex()

  ! Test 5: M=3, N=2, L=2 small complex
  call run_3_2_2_complex()

  ! Test 6: M=2, N=4, L=2 (wide) complex
  call run_2_4_2_complex()

contains

  subroutine run_4_3_0_real()
    complex*16 :: A(3, 3), B(4, 3), T(3, 3)
    double precision :: A_r(2, 3, 3), B_r(2, 4, 3), T_r(2, 3, 3)
    equivalence (A, A_r); equivalence (B, B_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0)
    A(1,1) = (2.0d0, 0.0d0); A(1,2) = (0.5d0, 0.0d0);  A(1,3) = (0.25d0, 0.0d0)
    A(2,2) = (3.0d0, 0.0d0); A(2,3) = (0.75d0, 0.0d0)
    A(3,3) = (4.0d0, 0.0d0)
    B(1,1) = (1.0d0, 0.0d0); B(1,2) = (0.5d0, 0.0d0);  B(1,3) = (0.25d0, 0.0d0)
    B(2,1) = (0.3d0, 0.0d0); B(2,2) = (1.1d0, 0.0d0);  B(2,3) = (0.6d0, 0.0d0)
    B(3,1) = (0.7d0, 0.0d0); B(3,2) = (0.4d0, 0.0d0);  B(3,3) = (1.2d0, 0.0d0)
    B(4,1) = (0.2d0, 0.0d0); B(4,2) = (0.9d0, 0.0d0);  B(4,3) = (0.8d0, 0.0d0)
    call ZTPQRT2(4, 3, 0, A, 3, B, 4, T, 3, INFO)
    call begin_test('m4_n3_l0_real')
    call print_array('A', A_r, 2*3*3)
    call print_array('B', B_r, 2*4*3)
    call print_array('T', T_r, 2*3*3)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_4_3_3_complex()
    complex*16 :: A(3, 3), B(4, 3), T(3, 3)
    double precision :: A_r(2, 3, 3), B_r(2, 4, 3), T_r(2, 3, 3)
    equivalence (A, A_r); equivalence (B, B_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0)
    A(1,1) = (2.0d0, 0.5d0); A(1,2) = (0.5d0, 0.1d0); A(1,3) = (0.25d0, 0.2d0)
    A(2,2) = (3.0d0, -0.3d0); A(2,3) = (0.75d0, -0.1d0)
    A(3,3) = (4.0d0, 0.4d0)
    ! B is 4x3: 1 rectangular row (M-L=1) + 3 trapezoidal rows
    B(1,1) = (1.0d0, 0.3d0); B(1,2) = (0.5d0, 0.2d0); B(1,3) = (0.25d0, 0.1d0)
    B(2,1) = (1.1d0, -0.1d0); B(2,2) = (0.6d0, 0.4d0); B(2,3) = (0.2d0, -0.2d0)
    B(3,2) = (1.2d0, 0.3d0); B(3,3) = (0.9d0, 0.1d0)
    B(4,3) = (1.5d0, -0.2d0)
    call ZTPQRT2(4, 3, 3, A, 3, B, 4, T, 3, INFO)
    call begin_test('m4_n3_l3_complex')
    call print_array('A', A_r, 2*3*3)
    call print_array('B', B_r, 2*4*3)
    call print_array('T', T_r, 2*3*3)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_3_3_3_complex()
    complex*16 :: A(3, 3), B(3, 3), T(3, 3)
    double precision :: A_r(2, 3, 3), B_r(2, 3, 3), T_r(2, 3, 3)
    equivalence (A, A_r); equivalence (B, B_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0)
    A(1,1) = (2.0d0, 0.1d0); A(1,2) = (0.3d0, -0.2d0); A(1,3) = (0.1d0, 0.3d0)
    A(2,2) = (3.0d0, 0.4d0); A(2,3) = (0.2d0, -0.1d0)
    A(3,3) = (4.0d0, 0.2d0)
    ! B 3x3 fully upper triangular
    B(1,1) = (1.1d0, 0.5d0); B(1,2) = (0.4d0, -0.3d0); B(1,3) = (0.6d0, 0.1d0)
    B(2,2) = (1.5d0, 0.2d0); B(2,3) = (0.3d0, -0.4d0)
    B(3,3) = (1.7d0, 0.3d0)
    call ZTPQRT2(3, 3, 3, A, 3, B, 3, T, 3, INFO)
    call begin_test('m3_n3_l3_complex')
    call print_array('A', A_r, 2*3*3)
    call print_array('B', B_r, 2*3*3)
    call print_array('T', T_r, 2*3*3)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_3_1_1_complex()
    complex*16 :: A(1, 1), B(3, 1), T(1, 1)
    double precision :: A_r(2, 1, 1), B_r(2, 3, 1), T_r(2, 1, 1)
    equivalence (A, A_r); equivalence (B, B_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0)
    A(1,1) = (5.0d0, 1.0d0)
    B(1,1) = (1.0d0, 0.5d0)
    B(2,1) = (2.0d0, -0.3d0)
    B(3,1) = (3.0d0, 0.4d0)
    call ZTPQRT2(3, 1, 1, A, 1, B, 3, T, 1, INFO)
    call begin_test('m3_n1_l1_complex')
    call print_array('A', A_r, 2*1*1)
    call print_array('B', B_r, 2*3*1)
    call print_array('T', T_r, 2*1*1)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_3_2_2_complex()
    complex*16 :: A(2, 2), B(3, 2), T(2, 2)
    double precision :: A_r(2, 2, 2), B_r(2, 3, 2), T_r(2, 2, 2)
    equivalence (A, A_r); equivalence (B, B_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0)
    A(1,1) = (2.0d0, 0.3d0); A(1,2) = (0.5d0, -0.2d0)
    A(2,2) = (3.0d0, 0.4d0)
    ! B 3x2 (M=3, N=2, L=2): 1 rectangular row + 2 trapezoidal rows
    B(1,1) = (1.0d0, 0.2d0); B(1,2) = (0.5d0, -0.1d0)
    B(2,1) = (1.1d0, -0.3d0); B(2,2) = (0.6d0, 0.5d0)
    B(3,2) = (0.4d0, 0.3d0)
    call ZTPQRT2(3, 2, 2, A, 2, B, 3, T, 2, INFO)
    call begin_test('m3_n2_l2_complex')
    call print_array('A', A_r, 2*2*2)
    call print_array('B', B_r, 2*3*2)
    call print_array('T', T_r, 2*2*2)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_2_4_2_complex()
    complex*16 :: A(4, 4), B(2, 4), T(4, 4)
    double precision :: A_r(2, 4, 4), B_r(2, 2, 4), T_r(2, 4, 4)
    equivalence (A, A_r); equivalence (B, B_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0)
    A(1,1) = (2.0d0, 0.1d0); A(1,2) = (0.5d0, -0.2d0); A(1,3) = (0.2d0, 0.1d0); A(1,4) = (0.1d0, 0.2d0)
    A(2,2) = (3.0d0, 0.3d0); A(2,3) = (0.4d0, -0.1d0); A(2,4) = (0.3d0, 0.1d0)
    A(3,3) = (2.5d0, 0.2d0); A(3,4) = (0.6d0, -0.3d0)
    A(4,4) = (3.5d0, 0.4d0)
    ! B is 2x4, M=L=2 so both rows trapezoidal
    B(1,1) = (1.0d0, 0.4d0); B(1,2) = (0.5d0, -0.2d0); B(1,3) = (0.7d0, 0.1d0); B(1,4) = (0.3d0, 0.2d0)
    B(2,2) = (1.2d0, 0.3d0); B(2,3) = (0.8d0, -0.4d0); B(2,4) = (0.6d0, 0.1d0)
    call ZTPQRT2(2, 4, 2, A, 4, B, 2, T, 4, INFO)
    call begin_test('m2_n4_l2_complex')
    call print_array('A', A_r, 2*4*4)
    call print_array('B', B_r, 2*2*4)
    call print_array('T', T_r, 2*4*4)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

end program
