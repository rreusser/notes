program test_zgeqrt
  use test_utils
  implicit none

  ! Use exact-size arrays per test (LDA = M, LDT = NB) so the printed buffer
  ! maps directly onto a packed Complex128Array used in JS tests.

  integer :: INFO

  ! Test 1: M=4, N=3, NB=2 (forces blocked path)
  call run_4_3_nb2()

  ! Test 2: M=N=3, NB=3 (single block, square)
  call run_3_3_nb3()

  ! Test 3: M=5, N=2, NB=1 (single-reflector blocks)
  call run_5_2_nb1()

  ! Test 4: M=N=1, NB=1 (degenerate scalar)
  call run_1_1_nb1()

  ! Test 5: M=3, N=1, NB=1 (single column)
  call run_3_1_nb1()

  ! Test 6: M=N=4, NB=2 (multiple equal blocks)
  call run_4_4_nb2()

  ! Test 7: M=6, N=4, NB=3 (last block smaller than NB)
  call run_6_4_nb3()

  ! Test 8: M=3, N=5, NB=2 (wide matrix M < N)
  call run_3_5_nb2()

  ! Test 9: M=2, N=2, NB=1 — purely real input (exercises alphi==0 path of zlarfg)
  call run_2_2_nb1_real()

  ! Test 10: M=0 quick return
  call run_m0_quick()

  ! Test 11: N=0 quick return
  call run_n0_quick()

contains

  subroutine run_4_3_nb2()
    complex*16 :: A(4, 3), T(2, 3), WORK(2*3)
    double precision :: A_r(2, 4, 3), T_r(2, 2, 3)
    equivalence (A, A_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    A(1,1) = (2.0d0, 0.5d0); A(1,2) = (1.0d0, -0.2d0); A(1,3) = (3.0d0, 0.1d0)
    A(2,1) = (1.0d0, -0.3d0); A(2,2) = (4.0d0, 0.4d0); A(2,3) = (2.0d0, -0.1d0)
    A(3,1) = (3.0d0, 0.2d0); A(3,2) = (2.0d0, 0.1d0); A(3,3) = (5.0d0, 0.3d0)
    A(4,1) = (1.0d0, -0.4d0); A(4,2) = (3.0d0, -0.2d0); A(4,3) = (1.0d0, 0.5d0)
    call ZGEQRT(4, 3, 2, A, 4, T, 2, WORK, INFO)
    call begin_test('m4_n3_nb2')
    call print_array('A', A_r, 2*4*3)
    call print_array('T', T_r, 2*2*3)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_3_3_nb3()
    complex*16 :: A(3, 3), T(3, 3), WORK(3*3)
    double precision :: A_r(2, 3, 3), T_r(2, 3, 3)
    equivalence (A, A_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    A(1,1) = (4.0d0, 0.1d0); A(1,2) = (1.0d0, -0.2d0); A(1,3) = (2.0d0, 0.3d0)
    A(2,1) = (1.0d0, 0.2d0); A(2,2) = (5.0d0, -0.1d0); A(2,3) = (3.0d0, 0.4d0)
    A(3,1) = (2.0d0, -0.3d0); A(3,2) = (3.0d0, 0.5d0); A(3,3) = (6.0d0, 0.2d0)
    call ZGEQRT(3, 3, 3, A, 3, T, 3, WORK, INFO)
    call begin_test('m3_n3_nb3')
    call print_array('A', A_r, 2*3*3)
    call print_array('T', T_r, 2*3*3)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_5_2_nb1()
    complex*16 :: A(5, 2), T(1, 2), WORK(1*2)
    double precision :: A_r(2, 5, 2), T_r(2, 1, 2)
    equivalence (A, A_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    A(1,1) = (1.0d0, 0.2d0); A(1,2) = (2.0d0, -0.1d0)
    A(2,1) = (3.0d0, -0.4d0); A(2,2) = (4.0d0, 0.3d0)
    A(3,1) = (5.0d0, 0.1d0); A(3,2) = (6.0d0, -0.2d0)
    A(4,1) = (2.0d0, 0.5d0); A(4,2) = (1.0d0, -0.3d0)
    A(5,1) = (4.0d0, -0.1d0); A(5,2) = (3.0d0, 0.4d0)
    call ZGEQRT(5, 2, 1, A, 5, T, 1, WORK, INFO)
    call begin_test('m5_n2_nb1')
    call print_array('A', A_r, 2*5*2)
    call print_array('T', T_r, 2*1*2)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_1_1_nb1()
    complex*16 :: A(1, 1), T(1, 1), WORK(1)
    double precision :: A_r(2, 1, 1), T_r(2, 1, 1)
    equivalence (A, A_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    A(1,1) = (7.5d0, -1.5d0)
    call ZGEQRT(1, 1, 1, A, 1, T, 1, WORK, INFO)
    call begin_test('m1_n1_nb1')
    call print_array('A', A_r, 2*1*1)
    call print_array('T', T_r, 2*1*1)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_3_1_nb1()
    complex*16 :: A(3, 1), T(1, 1), WORK(1)
    double precision :: A_r(2, 3, 1), T_r(2, 1, 1)
    equivalence (A, A_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    A(1,1) = (1.0d0, 0.3d0)
    A(2,1) = (2.0d0, -0.4d0)
    A(3,1) = (2.0d0, 0.5d0)
    call ZGEQRT(3, 1, 1, A, 3, T, 1, WORK, INFO)
    call begin_test('m3_n1_nb1')
    call print_array('A', A_r, 2*3*1)
    call print_array('T', T_r, 2*1*1)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_4_4_nb2()
    complex*16 :: A(4, 4), T(2, 4), WORK(2*4)
    double precision :: A_r(2, 4, 4), T_r(2, 2, 4)
    equivalence (A, A_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    A(1,1) = (2.0d0, 0.1d0); A(1,2) = (0.5d0, -0.2d0); A(1,3) = (1.0d0, 0.1d0); A(1,4) = (0.3d0, -0.05d0)
    A(2,1) = (1.0d0, -0.1d0); A(2,2) = (3.0d0, 0.2d0); A(2,3) = (0.7d0, -0.3d0); A(2,4) = (0.4d0, 0.1d0)
    A(3,1) = (0.5d0, 0.2d0); A(3,2) = (0.8d0, 0.1d0); A(3,3) = (4.0d0, -0.2d0); A(3,4) = (0.6d0, 0.3d0)
    A(4,1) = (0.2d0, -0.3d0); A(4,2) = (0.9d0, 0.4d0); A(4,3) = (1.2d0, 0.1d0); A(4,4) = (5.0d0, -0.1d0)
    call ZGEQRT(4, 4, 2, A, 4, T, 2, WORK, INFO)
    call begin_test('m4_n4_nb2')
    call print_array('A', A_r, 2*4*4)
    call print_array('T', T_r, 2*2*4)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_6_4_nb3()
    complex*16 :: A(6, 4), T(3, 4), WORK(3*4)
    double precision :: A_r(2, 6, 4), T_r(2, 3, 4)
    equivalence (A, A_r); equivalence (T, T_r)
    integer :: ii, jj
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    do jj = 1, 4
      do ii = 1, 6
        if (ii .eq. jj) then
          A(ii, jj) = cmplx(5.0d0 + dble(jj), 0.1d0 * dble(jj), kind=8)
        else
          A(ii, jj) = cmplx(1.0d0 / dble(abs(ii - jj) + 1), &
                            0.05d0 * dble(ii - jj), kind=8)
        end if
      end do
    end do
    call ZGEQRT(6, 4, 3, A, 6, T, 3, WORK, INFO)
    call begin_test('m6_n4_nb3')
    call print_array('A', A_r, 2*6*4)
    call print_array('T', T_r, 2*3*4)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_3_5_nb2()
    complex*16 :: A(3, 5), T(2, 3), WORK(2*5)
    double precision :: A_r(2, 3, 5), T_r(2, 2, 3)
    equivalence (A, A_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    A(1,1) = (3.0d0, 0.1d0); A(1,2) = (1.0d0, -0.2d0); A(1,3) = (2.0d0, 0.3d0); A(1,4) = (0.5d0, -0.05d0); A(1,5) = (0.3d0, 0.15d0)
    A(2,1) = (1.0d0, 0.2d0); A(2,2) = (4.0d0, -0.1d0); A(2,3) = (0.7d0, 0.4d0); A(2,4) = (1.5d0, -0.3d0); A(2,5) = (0.4d0, 0.25d0)
    A(3,1) = (0.5d0, -0.3d0); A(3,2) = (0.8d0, 0.5d0); A(3,3) = (5.0d0, 0.2d0); A(3,4) = (0.6d0, 0.1d0); A(3,5) = (1.1d0, -0.2d0)
    call ZGEQRT(3, 5, 2, A, 3, T, 2, WORK, INFO)
    call begin_test('m3_n5_nb2')
    call print_array('A', A_r, 2*3*5)
    call print_array('T', T_r, 2*2*3)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_2_2_nb1_real()
    complex*16 :: A(2, 2), T(1, 2), WORK(1*2)
    double precision :: A_r(2, 2, 2), T_r(2, 1, 2)
    equivalence (A, A_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    A(1,1) = (3.0d0, 0.0d0); A(1,2) = (1.0d0, 0.0d0)
    A(2,1) = (4.0d0, 0.0d0); A(2,2) = (2.0d0, 0.0d0)
    call ZGEQRT(2, 2, 1, A, 2, T, 1, WORK, INFO)
    call begin_test('m2_n2_nb1_real')
    call print_array('A', A_r, 2*2*2)
    call print_array('T', T_r, 2*1*2)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_m0_quick()
    complex*16 :: A(1, 3), T(1, 3), WORK(3)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    call ZGEQRT(0, 3, 1, A, 1, T, 1, WORK, INFO)
    call begin_test('m0_n3')
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_n0_quick()
    complex*16 :: A(3, 1), T(1, 1), WORK(1)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    call ZGEQRT(3, 0, 1, A, 3, T, 1, WORK, INFO)
    call begin_test('m3_n0')
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

end program
