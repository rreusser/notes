program test_zgetsqrhrt
  use test_utils
  implicit none

  integer :: INFO

  ! Test 1: blocked case — M=8, N=3, MB1=4, NB1=2, NB2=2.
  call run_8_3_4_2_2()

  ! Test 2: NB2 < N — different output blocking from internal TSQR.
  call run_10_4_5_3_2()

  ! Test 3: MB1 > M — single TSQR row block; small full case.
  call run_5_3_8_2_3()

  ! Test 4: NB2 > N — clamp NB2LOCAL = N.
  call run_6_3_4_2_8()

  ! Test 5: NB1 == N — single TSQR column block.
  call run_9_3_5_3_2()

  ! Test 6: NB1 == 1 — one Householder per TSQR step.
  call run_8_2_4_1_2()

  ! Test 7: square M==N — degenerate TSQR (single block).
  call run_4_4_8_2_2()

  ! Test 8: M=N=0 quick return.
  call run_m0()

  ! Test 9: N=0 with M>0 quick return.
  call run_n0()

  ! Test 10: tall blocked larger — M=12, N=3, MB1=5, NB1=3, NB2=3.
  call run_12_3_5_3_3()

contains

  subroutine run_one(M, N, MB1, NB1, NB2, A, label)
    integer, intent(in) :: M, N, MB1, NB1, NB2
    complex*16, intent(inout) :: A(M, N)
    character(len=*), intent(in) :: label
    integer :: NB2LOCAL, NB1LOCAL, NUM_ALL_ROW_BLOCKS, LWT, LW1, LW2, LWORKOPT
    integer :: LDT
    complex*16, allocatable :: T(:,:), WORK(:)
    double precision, allocatable :: A_r(:), T_r(:)
    integer :: i, j

    NB1LOCAL = MIN(NB1, N)
    NB2LOCAL = MIN(NB2, N)
    LDT = MAX(1, NB2LOCAL)
    if (M .gt. N) then
      NUM_ALL_ROW_BLOCKS = MAX(1, ceiling( dble(M - N) / dble(MB1 - N) ))
    else
      NUM_ALL_ROW_BLOCKS = 1
    end if
    LWT = NUM_ALL_ROW_BLOCKS * N * MAX(1, NB1LOCAL)
    LW1 = MAX(1, NB1LOCAL) * MAX(1, N)
    LW2 = MAX(1, NB1LOCAL) * MAX(MAX(1,NB1LOCAL), MAX(0, N - NB1LOCAL))
    LWORKOPT = MAX(LWT + LW1, MAX(LWT + N*N + LW2, LWT + N*N + N))
    LWORKOPT = MAX(LWORKOPT, N*N + 1)

    allocate(T(LDT, MAX(1, N)))
    allocate(WORK(MAX(1, LWORKOPT)))
    allocate(A_r(2*M*N))
    allocate(T_r(2*LDT*MAX(1, N)))
    T = (0.0d0, 0.0d0)
    WORK = (0.0d0, 0.0d0)

    call ZGETSQRHRT(M, N, MB1, NB1, NB2, A, M, T, LDT, WORK, LWORKOPT, INFO)

    call begin_test(label)
    ! Pack A into contiguous A_r (M-by-N)
    do j = 1, N
      do i = 1, M
        A_r(2*((j-1)*M + (i-1)) + 1) = dble(A(i, j))
        A_r(2*((j-1)*M + (i-1)) + 2) = aimag(A(i, j))
      end do
    end do
    call print_array('A', A_r, 2*M*N)
    ! Pack T into contiguous T_r (LDT-by-N) — only first NB2LOCAL rows are meaningful
    do j = 1, N
      do i = 1, LDT
        T_r(2*((j-1)*LDT + (i-1)) + 1) = dble(T(i, j))
        T_r(2*((j-1)*LDT + (i-1)) + 2) = aimag(T(i, j))
      end do
    end do
    call print_array('T', T_r, 2*LDT*N)
    call print_int('INFO', INFO)
    call end_test()

    deallocate(T, WORK, A_r, T_r)
  end subroutine

  subroutine fill_diag_dom(M, N, A, scale, im_scale)
    integer, intent(in) :: M, N
    complex*16, intent(inout) :: A(M, N)
    double precision, intent(in) :: scale, im_scale
    integer :: i, j
    do j = 1, N
      do i = 1, M
        if (i .eq. j) then
          A(i, j) = cmplx(scale + dble(j), im_scale * dble(j), kind=8)
        else
          A(i, j) = cmplx(1.0d0 / dble(abs(i - j) + 1), &
                          0.05d0 * dble(i - j), kind=8)
        end if
      end do
    end do
  end subroutine

  subroutine run_8_3_4_2_2()
    integer, parameter :: M = 8, N = 3, MB1 = 4, NB1 = 2, NB2 = 2
    complex*16 :: A(M, N)
    A = (0.0d0, 0.0d0)
    call fill_diag_dom(M, N, A, 4.0d0, 0.1d0)
    call run_one(M, N, MB1, NB1, NB2, A, 'm8_n3_mb14_nb12_nb22')
  end subroutine

  subroutine run_10_4_5_3_2()
    integer, parameter :: M = 10, N = 4, MB1 = 5, NB1 = 3, NB2 = 2
    complex*16 :: A(M, N)
    A = (0.0d0, 0.0d0)
    call fill_diag_dom(M, N, A, 5.0d0, -0.1d0)
    call run_one(M, N, MB1, NB1, NB2, A, 'm10_n4_mb15_nb13_nb22')
  end subroutine

  subroutine run_5_3_8_2_3()
    integer, parameter :: M = 5, N = 3, MB1 = 8, NB1 = 2, NB2 = 3
    complex*16 :: A(M, N)
    A = (0.0d0, 0.0d0)
    call fill_diag_dom(M, N, A, 4.5d0, 0.2d0)
    call run_one(M, N, MB1, NB1, NB2, A, 'm5_n3_mb18_nb12_nb23')
  end subroutine

  subroutine run_6_3_4_2_8()
    integer, parameter :: M = 6, N = 3, MB1 = 4, NB1 = 2, NB2 = 8
    complex*16 :: A(M, N)
    A = (0.0d0, 0.0d0)
    call fill_diag_dom(M, N, A, 5.5d0, 0.05d0)
    call run_one(M, N, MB1, NB1, NB2, A, 'm6_n3_mb14_nb12_nb28_clamp')
  end subroutine

  subroutine run_9_3_5_3_2()
    integer, parameter :: M = 9, N = 3, MB1 = 5, NB1 = 3, NB2 = 2
    complex*16 :: A(M, N)
    A = (0.0d0, 0.0d0)
    call fill_diag_dom(M, N, A, 6.0d0, 0.07d0)
    call run_one(M, N, MB1, NB1, NB2, A, 'm9_n3_mb15_nb13_nb22')
  end subroutine

  subroutine run_8_2_4_1_2()
    integer, parameter :: M = 8, N = 2, MB1 = 4, NB1 = 1, NB2 = 2
    complex*16 :: A(M, N)
    A = (0.0d0, 0.0d0)
    call fill_diag_dom(M, N, A, 4.0d0, -0.05d0)
    call run_one(M, N, MB1, NB1, NB2, A, 'm8_n2_mb14_nb11_nb22')
  end subroutine

  subroutine run_4_4_8_2_2()
    integer, parameter :: M = 4, N = 4, MB1 = 8, NB1 = 2, NB2 = 2
    complex*16 :: A(M, N)
    A = (0.0d0, 0.0d0)
    call fill_diag_dom(M, N, A, 6.0d0, 0.1d0)
    call run_one(M, N, MB1, NB1, NB2, A, 'm4_n4_mb18_nb12_nb22_square')
  end subroutine

  subroutine run_m0()
    complex*16 :: A(1, 1), T(1, 1), WORK(1)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    ! M=0, N=0 quick return; MB1 > N requires MB1 >= 1.
    call ZGETSQRHRT(0, 0, 1, 1, 1, A, 1, T, 1, WORK, 1, INFO)
    call begin_test('m0_n0_quickreturn')
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_n0()
    complex*16 :: A(3, 1), T(1, 1), WORK(1)
    A = (0.0d0, 0.0d0); T = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
    ! N=0 quick return; MB1 > 0 OK.
    call ZGETSQRHRT(3, 0, 1, 1, 1, A, 3, T, 1, WORK, 1, INFO)
    call begin_test('m3_n0_quickreturn')
    call print_int('INFO', INFO)
    call end_test()
  end subroutine

  subroutine run_12_3_5_3_3()
    integer, parameter :: M = 12, N = 3, MB1 = 5, NB1 = 3, NB2 = 3
    complex*16 :: A(M, N)
    integer :: i, j
    A = (0.0d0, 0.0d0)
    do j = 1, N
      do i = 1, M
        if (i .eq. j) then
          A(i, j) = cmplx(5.0d0 + dble(j) * 0.5d0, 0.1d0, kind=8)
        else
          A(i, j) = cmplx(dble(mod(i*7 + j*3, 11)) / 5.0d0 + 0.1d0, &
                          dble(mod(i*5 + j*2, 7)) / 8.0d0 - 0.3d0, kind=8)
        end if
      end do
    end do
    call run_one(M, N, MB1, NB1, NB2, A, 'm12_n3_mb15_nb13_nb23')
  end subroutine

end program
