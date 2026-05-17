program test_zgelqt
  use test_utils
  implicit none

  ! Use exact-size arrays per test (LDA = M, LDT = MB) so the printed buffer
  ! maps directly onto a packed Complex128Array used in JS tests.

  integer :: INFO

  ! Test 1: M=4, N=6, MB=2 (M < N, multiple panels with trailing update)
  call run_m4_n6_mb2()

  ! Test 2: M=5, N=7, MB=3 (uneven block at end: K=5, IB=3 then IB=2)
  call run_m5_n7_mb3()

  ! Test 3: M=6, N=4, MB=2 (M > N, K=4)
  call run_m6_n4_mb2()

  ! Test 4: M=N=5, MB=2 (square)
  call run_m5_n5_mb2()

  ! Test 5: M=3, N=5, MB=3 (single block — no trailing update)
  call run_m3_n5_mb3()

  ! Test 6: M=4, N=6, MB=1 (degenerate: each panel is 1 row)
  call run_m4_n6_mb1()

  ! Test 7: M=1, N=4, MB=1 (single-row matrix — K=1, single block)
  call run_m1_n4_mb1()

  ! Test 8: M=0, N=4, MB=1 (quick return on K=0)
  call run_m0_n4_mb1()

contains

  subroutine run_m4_n6_mb2()
    complex*16 :: A(4, 6), T(2, 4), WORK(2*6)
    double precision :: A_r(2, 4, 6), T_r(2, 2, 4)
    equivalence (A, A_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    A(1,1) = (3.0d0, 0.1d0); A(1,2) = (0.6d0, -0.2d0); A(1,3) = (0.4d0, 0.3d0); A(1,4) = (0.2d0, -0.1d0); A(1,5) = (0.1d0, 0.4d0); A(1,6) = (-0.3d0, 0.2d0)
    A(2,1) = (0.5d0, -0.3d0); A(2,2) = (4.0d0, 0.4d0); A(2,3) = (0.7d0, -0.2d0); A(2,4) = (0.3d0, 0.5d0); A(2,5) = (-0.2d0, -0.4d0); A(2,6) = (0.5d0, 0.1d0)
    A(3,1) = (0.2d0, 0.5d0); A(3,2) = (0.5d0, -0.3d0); A(3,3) = (3.5d0, 0.2d0); A(3,4) = (0.8d0, -0.4d0); A(3,5) = (0.6d0, 0.1d0); A(3,6) = (0.1d0, -0.5d0)
    A(4,1) = (0.4d0, -0.1d0); A(4,2) = (0.3d0, 0.4d0); A(4,3) = (0.5d0, -0.5d0); A(4,4) = (4.5d0, 0.3d0); A(4,5) = (1.1d0, -0.2d0); A(4,6) = (-0.5d0, 0.4d0)
    call ZGELQT(4, 6, 2, A, 4, T, 2, WORK, INFO)
    call begin_test('m4_n6_mb2')
    call print_array('A', A_r, 2*4*6)
    call print_array('T', T_r, 2*2*4)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_m5_n7_mb3()
    complex*16 :: A(5, 7), T(3, 5), WORK(3*7)
    double precision :: A_r(2, 5, 7), T_r(2, 3, 5)
    equivalence (A, A_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    A(1,1) = (5.0d0, 0.2d0); A(1,2) = (0.7d0, -0.4d0); A(1,3) = (0.3d0, 0.1d0); A(1,4) = (-0.1d0, 0.3d0); A(1,5) = (0.4d0, -0.5d0); A(1,6) = (0.2d0, 0.4d0); A(1,7) = (0.5d0, -0.1d0)
    A(2,1) = (0.6d0, -0.2d0); A(2,2) = (4.5d0, 0.3d0); A(2,3) = (0.8d0, -0.4d0); A(2,4) = (0.5d0, 0.1d0); A(2,5) = (-0.3d0, 0.5d0); A(2,6) = (0.7d0, -0.3d0); A(2,7) = (0.1d0, 0.2d0)
    A(3,1) = (0.3d0, 0.4d0); A(3,2) = (0.4d0, -0.2d0); A(3,3) = (5.5d0, 0.5d0); A(3,4) = (0.9d0, -0.3d0); A(3,5) = (0.7d0, 0.4d0); A(3,6) = (-0.2d0, -0.1d0); A(3,7) = (0.4d0, 0.5d0)
    A(4,1) = (0.2d0, -0.5d0); A(4,2) = (0.5d0, 0.1d0); A(4,3) = (0.6d0, -0.4d0); A(4,4) = (4.8d0, 0.2d0); A(4,5) = (1.0d0, -0.5d0); A(4,6) = (0.3d0, 0.4d0); A(4,7) = (-0.4d0, 0.1d0)
    A(5,1) = (0.1d0, 0.3d0); A(5,2) = (0.3d0, -0.4d0); A(5,3) = (0.5d0, 0.2d0); A(5,4) = (0.7d0, -0.1d0); A(5,5) = (5.2d0, 0.5d0); A(5,6) = (0.9d0, -0.3d0); A(5,7) = (0.6d0, 0.4d0)
    call ZGELQT(5, 7, 3, A, 5, T, 3, WORK, INFO)
    call begin_test('m5_n7_mb3')
    call print_array('A', A_r, 2*5*7)
    call print_array('T', T_r, 2*3*5)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_m6_n4_mb2()
    complex*16 :: A(6, 4), T(2, 4), WORK(2*4)
    double precision :: A_r(2, 6, 4), T_r(2, 2, 4)
    equivalence (A, A_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    A(1,1) = (4.0d0, 0.1d0); A(1,2) = (0.5d0, -0.2d0); A(1,3) = (0.3d0, 0.4d0); A(1,4) = (0.1d0, -0.3d0)
    A(2,1) = (0.5d0, 0.5d0); A(2,2) = (3.5d0, -0.4d0); A(2,3) = (0.4d0, 0.2d0); A(2,4) = (0.2d0, -0.1d0)
    A(3,1) = (0.2d0, -0.4d0); A(3,2) = (0.4d0, 0.3d0); A(3,3) = (5.0d0, -0.5d0); A(3,4) = (0.6d0, 0.2d0)
    A(4,1) = (0.6d0, 0.4d0); A(4,2) = (0.3d0, -0.3d0); A(4,3) = (0.5d0, 0.1d0); A(4,4) = (4.5d0, -0.2d0)
    A(5,1) = (0.1d0, 0.2d0); A(5,2) = (0.7d0, -0.5d0); A(5,3) = (0.2d0, 0.3d0); A(5,4) = (0.5d0, -0.4d0)
    A(6,1) = (0.3d0, -0.3d0); A(6,2) = (0.2d0, 0.4d0); A(6,3) = (0.6d0, -0.2d0); A(6,4) = (0.4d0, 0.1d0)
    call ZGELQT(6, 4, 2, A, 6, T, 2, WORK, INFO)
    call begin_test('m6_n4_mb2')
    call print_array('A', A_r, 2*6*4)
    call print_array('T', T_r, 2*2*4)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_m5_n5_mb2()
    complex*16 :: A(5, 5), T(2, 5), WORK(2*5)
    double precision :: A_r(2, 5, 5), T_r(2, 2, 5)
    equivalence (A, A_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    A(1,1) = (5.0d0, 0.1d0); A(1,2) = (0.6d0, -0.2d0); A(1,3) = (0.3d0, 0.3d0); A(1,4) = (0.4d0, -0.1d0); A(1,5) = (0.2d0, 0.4d0)
    A(2,1) = (0.5d0, -0.3d0); A(2,2) = (4.5d0, 0.4d0); A(2,3) = (0.7d0, -0.2d0); A(2,4) = (0.2d0, 0.5d0); A(2,5) = (-0.1d0, -0.4d0)
    A(3,1) = (0.2d0, 0.5d0); A(3,2) = (0.4d0, -0.3d0); A(3,3) = (5.5d0, 0.2d0); A(3,4) = (0.8d0, -0.4d0); A(3,5) = (0.6d0, 0.1d0)
    A(4,1) = (0.3d0, -0.1d0); A(4,2) = (0.5d0, 0.4d0); A(4,3) = (0.4d0, -0.5d0); A(4,4) = (4.8d0, 0.3d0); A(4,5) = (1.0d0, -0.2d0)
    A(5,1) = (0.1d0, 0.3d0); A(5,2) = (0.3d0, -0.4d0); A(5,3) = (0.5d0, 0.2d0); A(5,4) = (0.7d0, -0.1d0); A(5,5) = (5.2d0, 0.5d0)
    call ZGELQT(5, 5, 2, A, 5, T, 2, WORK, INFO)
    call begin_test('m5_n5_mb2')
    call print_array('A', A_r, 2*5*5)
    call print_array('T', T_r, 2*2*5)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_m3_n5_mb3()
    complex*16 :: A(3, 5), T(3, 3), WORK(3*5)
    double precision :: A_r(2, 3, 5), T_r(2, 3, 3)
    equivalence (A, A_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    A(1,1) = (4.0d0, 0.2d0); A(1,2) = (1.0d0, -0.3d0); A(1,3) = (0.5d0, 0.4d0); A(1,4) = (-0.25d0, 0.1d0); A(1,5) = (0.75d0, -0.5d0)
    A(2,1) = (0.5d0, 0.4d0); A(2,2) = (3.5d0, -0.2d0); A(2,3) = (1.2d0, 0.6d0); A(2,4) = (0.6d0, -0.3d0); A(2,5) = (-0.4d0, 0.2d0)
    A(3,1) = (0.3d0, -0.5d0); A(3,2) = (0.8d0, 0.1d0); A(3,3) = (4.5d0, -0.3d0); A(3,4) = (1.1d0, 0.5d0); A(3,5) = (0.9d0, -0.2d0)
    call ZGELQT(3, 5, 3, A, 3, T, 3, WORK, INFO)
    call begin_test('m3_n5_mb3')
    call print_array('A', A_r, 2*3*5)
    call print_array('T', T_r, 2*3*3)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_m4_n6_mb1()
    complex*16 :: A(4, 6), T(1, 4), WORK(1*6)
    double precision :: A_r(2, 4, 6), T_r(2, 1, 4)
    equivalence (A, A_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    A(1,1) = (3.0d0, 0.1d0); A(1,2) = (0.6d0, -0.2d0); A(1,3) = (0.4d0, 0.3d0); A(1,4) = (0.2d0, -0.1d0); A(1,5) = (0.1d0, 0.4d0); A(1,6) = (-0.3d0, 0.2d0)
    A(2,1) = (0.5d0, -0.3d0); A(2,2) = (4.0d0, 0.4d0); A(2,3) = (0.7d0, -0.2d0); A(2,4) = (0.3d0, 0.5d0); A(2,5) = (-0.2d0, -0.4d0); A(2,6) = (0.5d0, 0.1d0)
    A(3,1) = (0.2d0, 0.5d0); A(3,2) = (0.5d0, -0.3d0); A(3,3) = (3.5d0, 0.2d0); A(3,4) = (0.8d0, -0.4d0); A(3,5) = (0.6d0, 0.1d0); A(3,6) = (0.1d0, -0.5d0)
    A(4,1) = (0.4d0, -0.1d0); A(4,2) = (0.3d0, 0.4d0); A(4,3) = (0.5d0, -0.5d0); A(4,4) = (4.5d0, 0.3d0); A(4,5) = (1.1d0, -0.2d0); A(4,6) = (-0.5d0, 0.4d0)
    call ZGELQT(4, 6, 1, A, 4, T, 1, WORK, INFO)
    call begin_test('m4_n6_mb1')
    call print_array('A', A_r, 2*4*6)
    call print_array('T', T_r, 2*1*4)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_m1_n4_mb1()
    complex*16 :: A(1, 4), T(1, 1), WORK(1*4)
    double precision :: A_r(2, 1, 4), T_r(2, 1, 1)
    equivalence (A, A_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    A(1,1) = (2.0d0, 0.3d0); A(1,2) = (1.5d0, -0.4d0); A(1,3) = (0.5d0, 0.2d0); A(1,4) = (-1.25d0, 0.6d0)
    call ZGELQT(1, 4, 1, A, 1, T, 1, WORK, INFO)
    call begin_test('m1_n4_mb1')
    call print_array('A', A_r, 2*1*4)
    call print_array('T', T_r, 2*1*1)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_m0_n4_mb1()
    complex*16 :: A(1, 4), T(1, 1), WORK(1*4)
    double precision :: A_r(2, 1, 4), T_r(2, 1, 1)
    equivalence (A, A_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    call ZGELQT(0, 4, 1, A, 1, T, 1, WORK, INFO)
    call begin_test('m0_n4_mb1')
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

end program
