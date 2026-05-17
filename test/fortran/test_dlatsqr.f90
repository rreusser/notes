program test_dlatsqr
  use test_utils
  implicit none

  ! Use a single large allocation; pack matrices with exact dims before printing
  ! to avoid leading-dimension/EQUIVALENCE mismatches.
  integer, parameter :: NMAX = 80
  integer, parameter :: NBMAX = 16
  integer, parameter :: TCOLS = 256
  double precision :: A(NMAX, NMAX), T(NBMAX, TCOLS), WORK(NMAX*NMAX)
  double precision :: Apk(NMAX*NMAX), Tpk(NBMAX*TCOLS)
  integer :: INFO, i, j, M, N, MB, NB, NUMBLK, TC

  ! Test 1: basic blocked TSQR — M=8, N=3, MB=4, NB=2.
  ! Number_of_row_blocks = ceil((M-N)/(MB-N)) = ceil(5/1) = 5
  ! Actually: K = mod(M-N, MB-N) = mod(5,1) = 0; so blocks: first MB rows, then (M-MB)/(MB-N) extra.
  ! With M=8 N=3 MB=4: NUMBLK = ceil((8-3)/(4-3)) = 5; T cols = 5*N = 15.
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  do j = 1, 3
    do i = 1, 8
      if (i .eq. j) then
        A(i,j) = 4.0d0 + dble(j)
      else
        A(i,j) = 1.0d0 / dble(abs(i - j) + 1)
      end if
    end do
  end do
  M = 8; N = 3; MB = 4; NB = 2
  NUMBLK = ceiling( dble(M - N) / dble(MB - N) )
  TC = NUMBLK * N
  call DLATSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, NB*N, INFO)
  call begin_test('m8_n3_mb4_nb2')
  call pack_matrix(A, NMAX, M, N, Apk)
  call print_array('A', Apk, M*N)
  call pack_matrix(T, NBMAX, NB, TC, Tpk)
  call print_array('T', Tpk, NB*TC)
  call print_int('INFO', INFO)
  call print_int('NUMBLK', NUMBLK)
  call end_test()

  ! Test 2: M divides evenly — M=10, N=2, MB=4, NB=2.
  ! MB-N=2; (M-N)=8; mod(8,2)=0 -> KK=0, II=M-0+1=M+1, so last block not used.
  ! NUMBLK = (M-N)/(MB-N) = 4 (when KK=0)
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  do j = 1, 2
    do i = 1, 10
      if (i .eq. j) then
        A(i,j) = 6.0d0 + dble(j)
      else
        A(i,j) = 1.0d0 / dble(abs(i - j) + 1)
      end if
    end do
  end do
  M = 10; N = 2; MB = 4; NB = 2
  NUMBLK = ceiling( dble(M - N) / dble(MB - N) )
  TC = NUMBLK * N
  call DLATSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, NB*N, INFO)
  call begin_test('m10_n2_mb4_nb2_evendiv')
  call pack_matrix(A, NMAX, M, N, Apk)
  call print_array('A', Apk, M*N)
  call pack_matrix(T, NBMAX, NB, TC, Tpk)
  call print_array('T', Tpk, NB*TC)
  call print_int('INFO', INFO)
  call print_int('NUMBLK', NUMBLK)
  call end_test()

  ! Test 3: short fall-through to DGEQRT — MB >= M, so single dgeqrt.
  ! M=4, N=3, MB=8 (MB>M); routine just calls DGEQRT(M,N,NB,...).
  ! T then has N columns (per DGEQRT layout).
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0
  A(3,1) = 3.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0
  A(4,1) = 1.0d0; A(4,2) = 3.0d0; A(4,3) = 1.0d0
  M = 4; N = 3; MB = 8; NB = 2
  call DLATSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, NB*N, INFO)
  call begin_test('m4_n3_mb8_nb2_fallthrough')
  call pack_matrix(A, NMAX, M, N, Apk)
  call print_array('A', Apk, M*N)
  call pack_matrix(T, NBMAX, NB, N, Tpk)
  call print_array('T', Tpk, NB*N)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 4: MB == N also routes to DGEQRT.
  ! M=6, N=3, MB=3, NB=2
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  do j = 1, 3
    do i = 1, 6
      if (i .eq. j) then
        A(i,j) = 4.0d0 + dble(j)
      else
        A(i,j) = 1.0d0 / dble(abs(i - j) + 2)
      end if
    end do
  end do
  M = 6; N = 3; MB = 3; NB = 2
  call DLATSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, NB*N, INFO)
  call begin_test('m6_n3_mb3_nb2_mbeqn')
  call pack_matrix(A, NMAX, M, N, Apk)
  call print_array('A', Apk, M*N)
  call pack_matrix(T, NBMAX, NB, N, Tpk)
  call print_array('T', Tpk, NB*N)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 5: KK > 0 (last block triggers extra DTPQRT).
  ! M=11, N=3, MB=5; (M-N)=8, MB-N=2, mod=0... try MB=4: MB-N=1, mod=8/1=0.
  ! Use M=11, N=3, MB=5: (M-N)=8, MB-N=2, mod=0. Hmm.
  ! Use M=12, N=3, MB=5: (M-N)=9, MB-N=2, mod=1, KK=1, II=12 -> trailing block.
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  do j = 1, 3
    do i = 1, 12
      if (i .eq. j) then
        A(i,j) = 5.0d0 + dble(j) * 0.5d0
      else
        A(i,j) = dble(mod(i*7 + j*3, 11)) / 5.0d0 + 0.1d0
      end if
    end do
  end do
  M = 12; N = 3; MB = 5; NB = 3
  NUMBLK = ceiling( dble(M - N) / dble(MB - N) )
  TC = NUMBLK * N
  call DLATSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, NB*N, INFO)
  call begin_test('m12_n3_mb5_nb3_lastblock')
  call pack_matrix(A, NMAX, M, N, Apk)
  call print_array('A', Apk, M*N)
  call pack_matrix(T, NBMAX, NB, TC, Tpk)
  call print_array('T', Tpk, NB*TC)
  call print_int('INFO', INFO)
  call print_int('NUMBLK', NUMBLK)
  call end_test()

  ! Test 6: M=N edge case (square matrix). Should fall through to DGEQRT.
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  do j = 1, 4
    do i = 1, 4
      if (i .eq. j) then
        A(i,j) = 5.0d0 + dble(j)
      else
        A(i,j) = 0.5d0 / dble(abs(i - j) + 1)
      end if
    end do
  end do
  M = 4; N = 4; MB = 6; NB = 2
  call DLATSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, NB*N, INFO)
  call begin_test('m4_n4_mb6_nb2_square')
  call pack_matrix(A, NMAX, M, N, Apk)
  call print_array('A', Apk, M*N)
  call pack_matrix(T, NBMAX, NB, N, Tpk)
  call print_array('T', Tpk, NB*N)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 7: NB=1 (each reflector its own panel) with blocked TSQR.
  ! M=9, N=2, MB=4, NB=1. (M-N)=7, MB-N=2, mod=1, KK=1, last block triggered.
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  do j = 1, 2
    do i = 1, 9
      A(i,j) = dble(mod(i*5 + j*7, 13)) / 4.0d0 + 0.5d0
      if (i .eq. j) A(i,j) = A(i,j) + 4.0d0
    end do
  end do
  M = 9; N = 2; MB = 4; NB = 1
  NUMBLK = ceiling( dble(M - N) / dble(MB - N) )
  TC = NUMBLK * N
  call DLATSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, NB*N, INFO)
  call begin_test('m9_n2_mb4_nb1')
  call pack_matrix(A, NMAX, M, N, Apk)
  call print_array('A', Apk, M*N)
  call pack_matrix(T, NBMAX, NB, TC, Tpk)
  call print_array('T', Tpk, NB*TC)
  call print_int('INFO', INFO)
  call print_int('NUMBLK', NUMBLK)
  call end_test()

  ! Test 8: M=0 quick return.
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  M = 0; N = 0; MB = 4; NB = 2
  call DLATSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, NB*max(N,1), INFO)
  call begin_test('m0_n0_quickreturn')
  call print_int('INFO', INFO)
  call end_test()

  ! Test 9: N=0 quick return (M>0).
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  M = 3; N = 0; MB = 4; NB = 1
  call DLATSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, max(1,1), INFO)
  call begin_test('m3_n0_quickreturn')
  call print_int('INFO', INFO)
  call end_test()

  ! Test 10: tall and skinny — M=20, N=4, MB=8, NB=4.
  ! (M-N)=16, MB-N=4, mod=0; numblk=4.
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  do j = 1, 4
    do i = 1, 20
      A(i,j) = dble(mod(i*11 + j*5, 17)) / 6.0d0 + 0.2d0
      if (i .eq. j) A(i,j) = A(i,j) + 6.0d0
    end do
  end do
  M = 20; N = 4; MB = 8; NB = 4
  NUMBLK = ceiling( dble(M - N) / dble(MB - N) )
  TC = NUMBLK * N
  call DLATSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, NB*N, INFO)
  call begin_test('m20_n4_mb8_nb4')
  call pack_matrix(A, NMAX, M, N, Apk)
  call print_array('A', Apk, M*N)
  call pack_matrix(T, NBMAX, NB, TC, Tpk)
  call print_array('T', Tpk, NB*TC)
  call print_int('INFO', INFO)
  call print_int('NUMBLK', NUMBLK)
  call end_test()

contains

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

end program
