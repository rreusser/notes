program test_zgeqrt3
  use test_utils
  implicit none

  ! Use exact-size arrays per test (LDA = M, LDT = N) so the printed buffer maps
  ! directly onto a packed Complex128Array used in JS tests.

  integer :: INFO

  ! Test 1: M=4, N=1 (base case: N=1)
  call run_m4_n1()

  ! Test 2: M=1, N=1 (degenerate base case)
  call run_m1_n1()

  ! Test 3: M=4, N=2 (single recursive split)
  call run_m4_n2()

  ! Test 4: M=5, N=3 (N1=1, N2=2)
  call run_m5_n3()

  ! Test 5: M=6, N=4 (N1=2, N2=2 — even split with deeper recursion)
  call run_m6_n4()

  ! Test 6: M=7, N=5 (N1=2, N2=3 — uneven split)
  call run_m7_n5()

  ! Test 7: M=N=4 (square)
  call run_m4_n4()

  ! Test 8: M=4, N=1 with real-only entries (sanity check that conjugation logic agrees with real algebra)
  call run_m4_n1_real()

contains

  subroutine run_m4_n1()
    complex*16 :: A(4, 1), T(1, 1)
    double precision :: A_r(2, 4, 1), T_r(2, 1, 1)
    equivalence (A, A_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0)
    A(1,1) = (2.0d0, 0.3d0)
    A(2,1) = (1.5d0, -0.4d0)
    A(3,1) = (0.5d0, 0.2d0)
    A(4,1) = (-1.25d0, 0.6d0)
    call ZGEQRT3(4, 1, A, 4, T, 1, INFO)
    call begin_test('m4_n1')
    call print_array('A', A_r, 2*4*1)
    call print_array('T', T_r, 2*1*1)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_m1_n1()
    complex*16 :: A(1, 1), T(1, 1)
    double precision :: A_r(2, 1, 1), T_r(2, 1, 1)
    equivalence (A, A_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0)
    A(1,1) = (3.5d0, -1.2d0)
    call ZGEQRT3(1, 1, A, 1, T, 1, INFO)
    call begin_test('m1_n1')
    call print_array('A', A_r, 2*1*1)
    call print_array('T', T_r, 2*1*1)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_m4_n2()
    complex*16 :: A(4, 2), T(2, 2)
    double precision :: A_r(2, 4, 2), T_r(2, 2, 2)
    equivalence (A, A_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0)
    A(1,1) = (2.0d0, 0.1d0); A(1,2) = (1.5d0, -0.2d0)
    A(2,1) = (0.7d0, -0.3d0); A(2,2) = (3.0d0, 0.5d0)
    A(3,1) = (0.5d0, 0.3d0); A(3,2) = (1.1d0, -0.4d0)
    A(4,1) = (-1.0d0, 0.4d0); A(4,2) = (0.4d0, 0.2d0)
    call ZGEQRT3(4, 2, A, 4, T, 2, INFO)
    call begin_test('m4_n2')
    call print_array('A', A_r, 2*4*2)
    call print_array('T', T_r, 2*2*2)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_m5_n3()
    complex*16 :: A(5, 3), T(3, 3)
    double precision :: A_r(2, 5, 3), T_r(2, 3, 3)
    equivalence (A, A_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0)
    A(1,1) = (4.0d0, 0.2d0); A(1,2) = (1.0d0, -0.3d0); A(1,3) = (0.5d0, 0.4d0)
    A(2,1) = (0.5d0, 0.4d0); A(2,2) = (3.5d0, -0.2d0); A(2,3) = (1.2d0, 0.6d0)
    A(3,1) = (0.3d0, -0.5d0); A(3,2) = (0.8d0, 0.1d0); A(3,3) = (4.5d0, -0.3d0)
    A(4,1) = (-0.25d0, 0.1d0); A(4,2) = (0.6d0, -0.3d0); A(4,3) = (1.1d0, 0.5d0)
    A(5,1) = (0.75d0, -0.5d0); A(5,2) = (-0.4d0, 0.2d0); A(5,3) = (0.9d0, -0.2d0)
    call ZGEQRT3(5, 3, A, 5, T, 3, INFO)
    call begin_test('m5_n3')
    call print_array('A', A_r, 2*5*3)
    call print_array('T', T_r, 2*3*3)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_m6_n4()
    complex*16 :: A(6, 4), T(4, 4)
    double precision :: A_r(2, 6, 4), T_r(2, 4, 4)
    equivalence (A, A_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0)
    A(1,1) = (3.0d0, 0.1d0); A(1,2) = (0.6d0, -0.2d0); A(1,3) = (0.4d0, 0.3d0); A(1,4) = (0.2d0, -0.1d0)
    A(2,1) = (0.5d0, -0.3d0); A(2,2) = (4.0d0, 0.4d0); A(2,3) = (0.7d0, -0.2d0); A(2,4) = (0.3d0, 0.5d0)
    A(3,1) = (0.2d0, 0.5d0); A(3,2) = (0.5d0, -0.3d0); A(3,3) = (3.5d0, 0.2d0); A(3,4) = (0.8d0, -0.4d0)
    A(4,1) = (0.4d0, -0.1d0); A(4,2) = (0.3d0, 0.4d0); A(4,3) = (0.5d0, -0.5d0); A(4,4) = (4.5d0, 0.3d0)
    A(5,1) = (0.1d0, 0.4d0); A(5,2) = (-0.2d0, -0.4d0); A(5,3) = (0.6d0, 0.1d0); A(5,4) = (1.1d0, -0.2d0)
    A(6,1) = (-0.3d0, 0.2d0); A(6,2) = (0.5d0, 0.1d0); A(6,3) = (0.1d0, -0.5d0); A(6,4) = (-0.5d0, 0.4d0)
    call ZGEQRT3(6, 4, A, 6, T, 4, INFO)
    call begin_test('m6_n4')
    call print_array('A', A_r, 2*6*4)
    call print_array('T', T_r, 2*4*4)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_m7_n5()
    complex*16 :: A(7, 5), T(5, 5)
    double precision :: A_r(2, 7, 5), T_r(2, 5, 5)
    equivalence (A, A_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0)
    A(1,1) = (5.0d0, 0.2d0); A(1,2) = (0.7d0, -0.4d0); A(1,3) = (0.3d0, 0.1d0); A(1,4) = (-0.1d0, 0.3d0); A(1,5) = (0.4d0, -0.5d0)
    A(2,1) = (0.6d0, -0.2d0); A(2,2) = (4.5d0, 0.3d0); A(2,3) = (0.8d0, -0.4d0); A(2,4) = (0.5d0, 0.1d0); A(2,5) = (-0.3d0, 0.5d0)
    A(3,1) = (0.3d0, 0.4d0); A(3,2) = (0.4d0, -0.2d0); A(3,3) = (5.5d0, 0.5d0); A(3,4) = (0.9d0, -0.3d0); A(3,5) = (0.7d0, 0.4d0)
    A(4,1) = (0.2d0, -0.5d0); A(4,2) = (0.5d0, 0.1d0); A(4,3) = (0.6d0, -0.4d0); A(4,4) = (4.8d0, 0.2d0); A(4,5) = (1.0d0, -0.5d0)
    A(5,1) = (0.1d0, 0.3d0); A(5,2) = (0.3d0, -0.4d0); A(5,3) = (0.5d0, 0.2d0); A(5,4) = (0.7d0, -0.1d0); A(5,5) = (5.2d0, 0.5d0)
    A(6,1) = (0.2d0, 0.4d0); A(6,2) = (-0.4d0, 0.3d0); A(6,3) = (-0.2d0, -0.1d0); A(6,4) = (0.3d0, 0.4d0); A(6,5) = (0.9d0, -0.3d0)
    A(7,1) = (0.5d0, -0.1d0); A(7,2) = (0.1d0, 0.2d0); A(7,3) = (0.4d0, 0.5d0); A(7,4) = (-0.4d0, 0.1d0); A(7,5) = (0.6d0, 0.4d0)
    call ZGEQRT3(7, 5, A, 7, T, 5, INFO)
    call begin_test('m7_n5')
    call print_array('A', A_r, 2*7*5)
    call print_array('T', T_r, 2*5*5)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_m4_n4()
    complex*16 :: A(4, 4), T(4, 4)
    double precision :: A_r(2, 4, 4), T_r(2, 4, 4)
    equivalence (A, A_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0)
    A(1,1) = (4.0d0, 0.1d0); A(1,2) = (0.5d0, -0.2d0); A(1,3) = (0.3d0, 0.4d0); A(1,4) = (0.2d0, -0.3d0)
    A(2,1) = (0.6d0, 0.5d0); A(2,2) = (3.5d0, -0.4d0); A(2,3) = (0.4d0, 0.2d0); A(2,4) = (0.1d0, -0.1d0)
    A(3,1) = (0.2d0, -0.4d0); A(3,2) = (0.4d0, 0.3d0); A(3,3) = (4.5d0, -0.5d0); A(3,4) = (0.7d0, 0.2d0)
    A(4,1) = (0.3d0, 0.4d0); A(4,2) = (0.2d0, -0.3d0); A(4,3) = (0.5d0, 0.1d0); A(4,4) = (5.0d0, -0.2d0)
    call ZGEQRT3(4, 4, A, 4, T, 4, INFO)
    call begin_test('m4_n4')
    call print_array('A', A_r, 2*4*4)
    call print_array('T', T_r, 2*4*4)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_m4_n1_real()
    complex*16 :: A(4, 1), T(1, 1)
    double precision :: A_r(2, 4, 1), T_r(2, 1, 1)
    equivalence (A, A_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0)
    A(1,1) = (2.0d0, 0.0d0)
    A(2,1) = (1.5d0, 0.0d0)
    A(3,1) = (0.5d0, 0.0d0)
    A(4,1) = (-1.25d0, 0.0d0)
    call ZGEQRT3(4, 1, A, 4, T, 1, INFO)
    call begin_test('m4_n1_real')
    call print_array('A', A_r, 2*4*1)
    call print_array('T', T_r, 2*1*1)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

end program
