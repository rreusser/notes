program test_zungtsqr_row
  use test_utils
  implicit none

  integer :: INFO

  ! Test 1: bottom-up sweep — M=8, N=3, MB=4, NB=2.
  ! NUMBLK = ceil((M-N)/(MB-N)) = ceil(5/1) = 5
  call run_8_3_4_2()

  ! Test 2: M-N divides MB-N evenly — M=10, N=2, MB=4, NB=2.
  call run_10_2_4_2()

  ! Test 3: MB > M — single top-row-block path.
  call run_4_3_8_2()

  ! Test 4: MB == N (Fortran requires MB > N, so use MB = N+1) — exercise smallest blocked case.
  call run_6_3_4_2()

  ! Test 5: Larger blocked case — M=12, N=3, MB=5, NB=3.
  call run_12_3_5_3()

  ! Test 6: M=N square — falls through to top-row-block only.
  call run_4_4_6_2()

  ! Test 7: NB=1 — single Householder per column block.
  call run_9_2_4_1()

  ! Test 8: M=0 quick return.
  call run_m0()

  ! Test 9: N=0 quick return.
  call run_n0()

  ! Test 10: tall blocked — M=20, N=4, MB=8, NB=4.
  call run_20_4_8_4()

  ! Test 11: dummy-B branch (MB1-KB-KNB+1 == 0). Needs MB=M, kb_last + knb == M.
  ! M=4, N=2, NB=2, NBLOCAL=2, KB_LAST=1, KNB=2 => MB1-KB-KNB+1 = 4-1-2+1 = 2. Not zero.
  ! Use M=3, N=2, MB=4 (MB>N=2, MB>M=3 so MB1=3), NB=2 => KB_LAST=1, KNB=2 => 3-1-2+1=1.
  ! Use M=2, N=2, MB=3, NB=2 => MB1=2, KB_LAST=1, KNB=2 => 2-1-2+1 = 0. YES.
  call run_2_2_3_2()

contains

  subroutine run_8_3_4_2()
    integer, parameter :: M = 8, N = 3, MB = 4, NB = 2
    integer, parameter :: NUMBLK = 5
    integer, parameter :: TC = NUMBLK * N
    integer, parameter :: NBLOCAL = 2
    integer, parameter :: LWORK = NBLOCAL * MAX(NBLOCAL, N - NBLOCAL)
    complex*16 :: A(M, N), T(NB, TC), WORK(NB*N)
    double precision :: A_r(2, M, N), T_r(2, NB, TC)
    equivalence (A, A_r); equivalence (T, T_r)
    integer :: ii, jj
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    do jj = 1, N
      do ii = 1, M
        if (ii .eq. jj) then
          A(ii, jj) = cmplx(4.0d0 + dble(jj), 0.1d0 * dble(jj), kind=8)
        else
          A(ii, jj) = cmplx(1.0d0 / dble(abs(ii - jj) + 1), &
                            0.05d0 * dble(ii - jj), kind=8)
        end if
      end do
    end do
    call ZLATSQR(M, N, MB, NB, A, M, T, NB, WORK, NB*N, INFO)
    call ZUNGTSQR_ROW(M, N, MB, NB, A, M, T, NB, WORK, MAX(1,LWORK), INFO)
    call begin_test('m8_n3_mb4_nb2')
    call print_array('A', A_r, 2*M*N)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_10_2_4_2()
    integer, parameter :: M = 10, N = 2, MB = 4, NB = 2
    integer, parameter :: NUMBLK = 4
    integer, parameter :: TC = NUMBLK * N
    integer, parameter :: NBLOCAL = 2
    integer, parameter :: LWORK = NBLOCAL * MAX(NBLOCAL, N - NBLOCAL)
    complex*16 :: A(M, N), T(NB, TC), WORK(NB*N)
    double precision :: A_r(2, M, N), T_r(2, NB, TC)
    equivalence (A, A_r); equivalence (T, T_r)
    integer :: ii, jj
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    do jj = 1, N
      do ii = 1, M
        if (ii .eq. jj) then
          A(ii, jj) = cmplx(6.0d0 + dble(jj), -0.1d0 * dble(jj), kind=8)
        else
          A(ii, jj) = cmplx(1.0d0 / dble(abs(ii - jj) + 1), &
                            0.04d0 * dble(ii - jj), kind=8)
        end if
      end do
    end do
    call ZLATSQR(M, N, MB, NB, A, M, T, NB, WORK, NB*N, INFO)
    call ZUNGTSQR_ROW(M, N, MB, NB, A, M, T, NB, WORK, MAX(1,LWORK), INFO)
    call begin_test('m10_n2_mb4_nb2_evendiv')
    call print_array('A', A_r, 2*M*N)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_4_3_8_2()
    integer, parameter :: M = 4, N = 3, MB = 8, NB = 2
    integer, parameter :: NBLOCAL = 2
    integer, parameter :: LWORK = NBLOCAL * MAX(NBLOCAL, N - NBLOCAL)
    complex*16 :: A(M, N), T(NB, N), WORK(NB*N)
    double precision :: A_r(2, M, N), T_r(2, NB, N)
    equivalence (A, A_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    A(1,1) = (2.0d0, 0.1d0); A(1,2) = (1.0d0, -0.2d0); A(1,3) = (3.0d0, 0.3d0)
    A(2,1) = (1.0d0, -0.1d0); A(2,2) = (4.0d0, 0.2d0); A(2,3) = (2.0d0, -0.3d0)
    A(3,1) = (3.0d0, 0.2d0); A(3,2) = (2.0d0, 0.1d0); A(3,3) = (5.0d0, 0.4d0)
    A(4,1) = (1.0d0, -0.4d0); A(4,2) = (3.0d0, -0.2d0); A(4,3) = (1.0d0, 0.5d0)
    call ZLATSQR(M, N, MB, NB, A, M, T, NB, WORK, NB*N, INFO)
    call ZUNGTSQR_ROW(M, N, MB, NB, A, M, T, NB, WORK, MAX(1,LWORK), INFO)
    call begin_test('m4_n3_mb8_nb2_topblockonly')
    call print_array('A', A_r, 2*M*N)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_6_3_4_2()
    integer, parameter :: M = 6, N = 3, MB = 4, NB = 2
    integer, parameter :: NUMBLK = 3  ! ceil((6-3)/(4-3)) = 3
    integer, parameter :: TC = NUMBLK * N
    integer, parameter :: NBLOCAL = 2
    integer, parameter :: LWORK = NBLOCAL * MAX(NBLOCAL, N - NBLOCAL)
    complex*16 :: A(M, N), T(NB, TC), WORK(NB*N)
    double precision :: A_r(2, M, N), T_r(2, NB, TC)
    equivalence (A, A_r); equivalence (T, T_r)
    integer :: ii, jj
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    do jj = 1, N
      do ii = 1, M
        if (ii .eq. jj) then
          A(ii, jj) = cmplx(4.0d0 + dble(jj), 0.05d0 * dble(jj), kind=8)
        else
          A(ii, jj) = cmplx(1.0d0 / dble(abs(ii - jj) + 2), &
                            0.03d0 * dble(ii - jj), kind=8)
        end if
      end do
    end do
    call ZLATSQR(M, N, MB, NB, A, M, T, NB, WORK, NB*N, INFO)
    call ZUNGTSQR_ROW(M, N, MB, NB, A, M, T, NB, WORK, MAX(1,LWORK), INFO)
    call begin_test('m6_n3_mb4_nb2')
    call print_array('A', A_r, 2*M*N)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_12_3_5_3()
    integer, parameter :: M = 12, N = 3, MB = 5, NB = 3
    integer, parameter :: NUMBLK = 5
    integer, parameter :: TC = NUMBLK * N
    integer, parameter :: NBLOCAL = 3
    integer, parameter :: LWORK = NBLOCAL * MAX(NBLOCAL, N - NBLOCAL)
    complex*16 :: A(M, N), T(NB, TC), WORK(NB*N)
    double precision :: A_r(2, M, N), T_r(2, NB, TC)
    equivalence (A, A_r); equivalence (T, T_r)
    integer :: ii, jj
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    do jj = 1, N
      do ii = 1, M
        if (ii .eq. jj) then
          A(ii, jj) = cmplx(5.0d0 + dble(jj) * 0.5d0, 0.1d0, kind=8)
        else
          A(ii, jj) = cmplx(dble(mod(ii*7 + jj*3, 11)) / 5.0d0 + 0.1d0, &
                            dble(mod(ii*5 + jj*2, 7)) / 8.0d0 - 0.3d0, kind=8)
        end if
      end do
    end do
    call ZLATSQR(M, N, MB, NB, A, M, T, NB, WORK, NB*N, INFO)
    call ZUNGTSQR_ROW(M, N, MB, NB, A, M, T, NB, WORK, MAX(1,LWORK), INFO)
    call begin_test('m12_n3_mb5_nb3')
    call print_array('A', A_r, 2*M*N)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_4_4_6_2()
    integer, parameter :: M = 4, N = 4, MB = 6, NB = 2
    integer, parameter :: NBLOCAL = 2
    integer, parameter :: LWORK = NBLOCAL * MAX(NBLOCAL, N - NBLOCAL)
    complex*16 :: A(M, N), T(NB, N), WORK(NB*N)
    double precision :: A_r(2, M, N), T_r(2, NB, N)
    equivalence (A, A_r); equivalence (T, T_r)
    integer :: ii, jj
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    do jj = 1, N
      do ii = 1, M
        if (ii .eq. jj) then
          A(ii, jj) = cmplx(5.0d0 + dble(jj), 0.2d0 * dble(jj), kind=8)
        else
          A(ii, jj) = cmplx(0.5d0 / dble(abs(ii - jj) + 1), &
                            0.05d0 * dble(ii + jj), kind=8)
        end if
      end do
    end do
    call ZLATSQR(M, N, MB, NB, A, M, T, NB, WORK, NB*N, INFO)
    call ZUNGTSQR_ROW(M, N, MB, NB, A, M, T, NB, WORK, MAX(1,LWORK), INFO)
    call begin_test('m4_n4_mb6_nb2_square')
    call print_array('A', A_r, 2*M*N)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_9_2_4_1()
    integer, parameter :: M = 9, N = 2, MB = 4, NB = 1
    integer, parameter :: NUMBLK = 4
    integer, parameter :: TC = NUMBLK * N
    integer, parameter :: NBLOCAL = 1
    integer, parameter :: LWORK = NBLOCAL * MAX(NBLOCAL, N - NBLOCAL)
    complex*16 :: A(M, N), T(NB, TC), WORK(NB*N)
    double precision :: A_r(2, M, N), T_r(2, NB, TC)
    equivalence (A, A_r); equivalence (T, T_r)
    integer :: ii, jj
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    do jj = 1, N
      do ii = 1, M
        A(ii, jj) = cmplx(dble(mod(ii*5 + jj*7, 13)) / 4.0d0 + 0.5d0, &
                          dble(mod(ii*3 + jj*2, 5)) / 6.0d0 - 0.1d0, kind=8)
        if (ii .eq. jj) then
          A(ii, jj) = A(ii, jj) + (4.0d0, 0.0d0)
        end if
      end do
    end do
    call ZLATSQR(M, N, MB, NB, A, M, T, NB, WORK, NB*N, INFO)
    call ZUNGTSQR_ROW(M, N, MB, NB, A, M, T, NB, WORK, MAX(1,LWORK), INFO)
    call begin_test('m9_n2_mb4_nb1')
    call print_array('A', A_r, 2*M*N)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_m0()
    complex*16 :: A(1, 1), T(1, 1), WORK(1)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    ! M=0, N=0 quick return; MB must satisfy MB > N (so MB >= 1).
    call ZUNGTSQR_ROW(0, 0, 1, 1, A, 1, T, 1, WORK, 1, INFO)
    call begin_test('m0_n0_quickreturn')
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_n0()
    complex*16 :: A(3, 1), T(1, 1), WORK(1)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    call ZUNGTSQR_ROW(3, 0, 1, 1, A, 3, T, 1, WORK, 1, INFO)
    call begin_test('m3_n0_quickreturn')
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_20_4_8_4()
    integer, parameter :: M = 20, N = 4, MB = 8, NB = 4
    integer, parameter :: NUMBLK = 4
    integer, parameter :: TC = NUMBLK * N
    integer, parameter :: NBLOCAL = 4
    integer, parameter :: LWORK = NBLOCAL * MAX(NBLOCAL, N - NBLOCAL)
    complex*16 :: A(M, N), T(NB, TC), WORK(NB*N)
    double precision :: A_r(2, M, N), T_r(2, NB, TC)
    equivalence (A, A_r); equivalence (T, T_r)
    integer :: ii, jj
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    do jj = 1, N
      do ii = 1, M
        A(ii, jj) = cmplx(dble(mod(ii*11 + jj*5, 17)) / 6.0d0 + 0.2d0, &
                          dble(mod(ii*4 + jj*9, 13)) / 7.0d0 - 0.2d0, kind=8)
        if (ii .eq. jj) then
          A(ii, jj) = A(ii, jj) + (6.0d0, 0.0d0)
        end if
      end do
    end do
    call ZLATSQR(M, N, MB, NB, A, M, T, NB, WORK, NB*N, INFO)
    call ZUNGTSQR_ROW(M, N, MB, NB, A, M, T, NB, WORK, MAX(1,LWORK), INFO)
    call begin_test('m20_n4_mb8_nb4')
    call print_array('A', A_r, 2*M*N)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_2_2_3_2()
    ! M=2, N=2, MB=3, NB=2: top row block fall-through; KB_LAST=1, KNB=2,
    ! MB1-KB-KNB+1 = 2-1-2+1 = 0 — exercises the dummy-B branch.
    integer, parameter :: M = 2, N = 2, MB = 3, NB = 2
    integer, parameter :: NBLOCAL = 2
    integer, parameter :: LWORK = NBLOCAL * MAX(NBLOCAL, N - NBLOCAL)
    complex*16 :: A(M, N), T(NB, N), WORK(LWORK)
    double precision :: A_r(2, M, N), T_r(2, NB, N)
    equivalence (A, A_r); equivalence (T, T_r)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    A(1,1) = (3.0d0, 0.2d0); A(1,2) = (1.0d0, -0.1d0)
    A(2,1) = (1.0d0, 0.3d0); A(2,2) = (4.0d0, 0.4d0)
    call ZLATSQR(M, N, MB, NB, A, M, T, NB, WORK, NB*N, INFO)
    call ZUNGTSQR_ROW(M, N, MB, NB, A, M, T, NB, WORK, MAX(1,LWORK), INFO)
    call begin_test('m2_n2_mb3_nb2_dummyB')
    call print_array('A', A_r, 2*M*N)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

end program
