program test_dorgtsqr_row
  use test_utils
  implicit none

  ! Single large allocation; pack matrices with exact dims before printing
  ! to avoid leading-dimension/EQUIVALENCE mismatches.
  integer, parameter :: NMAX = 80
  integer, parameter :: NBMAX = 16
  integer, parameter :: TCOLS = 256
  integer, parameter :: WLEN = 4096
  double precision :: A(NMAX, NMAX), T(NBMAX, TCOLS), WORK(WLEN)
  double precision :: Apk(NMAX*NMAX), Tpk(NBMAX*TCOLS)
  integer :: M, N, MB, NB

  ! Test 1: typical blocked TSQR with row-block sweep — M=8, N=3, MB=4, NB=2.
  ! MB > N (required), MB < M, so the bottom-up sweep loop is exercised.
  call run_case('m8_n3_mb4_nb2', 8, 3, 4, 2)

  ! Test 2: M divides evenly into MB-N steps — M=10, N=2, MB=4, NB=2.
  ! KK = mod(8, 2) = 0, so trailing block is degenerate; numblk = 4.
  call run_case('m10_n2_mb4_nb2_evendiv', 10, 2, 4, 2)

  ! Test 3: MB > M — single row block, only the "top row block" path runs.
  ! M=4, N=3, MB=8, NB=2. Bottom-up loop is NOT executed.
  call run_case('m4_n3_mb8_nb2_singleblock', 4, 3, 8, 2)

  ! Test 4: KK > 0 — trailing partial block exercises the dummy(1,1)/B-empty
  ! branch in the top loop (last KB column block exactly fills the top row
  ! block, so MB1-KB-KNB+1 == 0).
  ! M=12, N=3, MB=5, NB=3.
  call run_case('m12_n3_mb5_nb3_lastblock', 12, 3, 5, 3)

  ! Test 5: square M = N, MB > M, NB < N. Single row block; identity Q.
  call run_case('m4_n4_mb6_nb2_square', 4, 4, 6, 2)

  ! Test 6: NB=1 (each panel a single reflector) with row blocks. KK > 0.
  call run_case('m9_n2_mb4_nb1', 9, 2, 4, 1)

  ! Test 7: tall and skinny — M=20, N=4, MB=8, NB=4. KK=0; numblk=4.
  call run_case('m20_n4_mb8_nb4', 20, 4, 8, 4)

  ! Test 8: NB == N (single column-block sweep, KB_LAST=1). M=15, N=3, MB=6.
  call run_case('m15_n3_mb6_nb3_singleblock', 15, 3, 6, 3)

  ! Test 9: M=0 quick return.
  call run_quickreturn('m0_n0_quickreturn', 0, 0, 4, 2)

  ! Test 10: N=0 quick return (M>0).
  call run_quickreturn('m5_n0_quickreturn', 5, 0, 4, 1)

contains

  subroutine setup_matrix(mr, nc)
    integer, intent(in) :: mr, nc
    integer :: ii, jj
    A = 0.0d0; T = 0.0d0; WORK = 0.0d0
    do jj = 1, nc
      do ii = 1, mr
        if (ii .eq. jj) then
          A(ii, jj) = 4.0d0 + dble(jj)
        else
          A(ii, jj) = 1.0d0 / dble(abs(ii - jj) + 1)
        end if
      end do
    end do
  end subroutine setup_matrix

  subroutine pack_matrix(src, lds, mr, nc, dst)
    integer, intent(in) :: lds, mr, nc
    double precision, intent(in)  :: src(lds, *)
    double precision, intent(out) :: dst(*)
    integer :: ii, jj
    do jj = 1, nc
      do ii = 1, mr
        dst(ii + (jj-1)*mr) = src(ii, jj)
      end do
    end do
  end subroutine pack_matrix

  subroutine run_case(name, mr, nc, mbr, nbr)
    character(len=*), intent(in) :: name
    integer, intent(in) :: mr, nc, mbr, nbr
    integer :: NBLOCAL, LWORKOPT, INFO, NUMBLK, TC

    ! Determine T's column count for this configuration:
    !   numblk = max(1, ceil((M-N)/(MB-N)))   when MB > N (always for us)
    !   if MB >= M: dlatsqr falls through to dgeqrt and uses N columns of T.
    if (mbr >= mr) then
      NUMBLK = 1
      TC = nc
    else
      NUMBLK = ceiling( dble(mr - nc) / dble(mbr - nc) )
      TC = NUMBLK * nc
    end if

    M = mr; N = nc; MB = mbr; NB = nbr
    call setup_matrix(M, N)
    ! Step 1: factor with DLATSQR.
    call DLATSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, NB*N, INFO)
    if (INFO .ne. 0) stop 'DLATSQR failed'

    ! Snapshot the dlatsqr output so the JS test can replay it as input.
    call begin_test(name)
    call pack_matrix(A, NMAX, M, N, Apk)
    call print_array('Ain', Apk, M*N)
    call pack_matrix(T, NBMAX, NB, TC, Tpk)
    call print_array('Tin', Tpk, NB*TC)
    call print_int('NUMBLK', NUMBLK)

    ! Step 2: reconstruct Q with DORGTSQR_ROW.
    NBLOCAL = MIN(NB, N)
    LWORKOPT = NBLOCAL * MAX(NBLOCAL, N - NBLOCAL)
    call DORGTSQR_ROW(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, MAX(1,LWORKOPT), INFO)

    call pack_matrix(A, NMAX, M, N, Apk)
    call print_array('Q', Apk, M*N)
    call print_int('INFO', INFO)
    call end_test()
  end subroutine run_case

  subroutine run_quickreturn(name, mr, nc, mbr, nbr)
    character(len=*), intent(in) :: name
    integer, intent(in) :: mr, nc, mbr, nbr
    integer :: INFO

    M = mr; N = nc; MB = mbr; NB = nbr
    A = 0.0d0; T = 0.0d0; WORK = 0.0d0
    call DORGTSQR_ROW(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, 1, INFO)
    call begin_test(name)
    call print_int('INFO', INFO)
    call print_int('M', M)
    call print_int('N', N)
    call end_test()
  end subroutine run_quickreturn

end program
