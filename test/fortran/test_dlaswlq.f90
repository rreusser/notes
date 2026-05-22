program test_dlaswlq
  use test_utils
  implicit none

  ! Use a single large allocation; pack matrices with exact dims before printing
  ! to avoid leading-dimension/EQUIVALENCE mismatches.
  integer, parameter :: NMAX = 80
  integer, parameter :: MBMAX = 16
  integer, parameter :: TCOLS = 256
  double precision :: A(NMAX, NMAX), T(MBMAX, TCOLS), WORK(NMAX*NMAX)
  double precision :: Apk(NMAX*NMAX), Tpk(MBMAX*TCOLS)
  integer :: INFO, i, j, M, N, MB, NB, NUMBLK, TC

  ! Test 1: basic blocked TSLQ -- M=3, N=8, MB=2, NB=4.
  ! NUMBLK = ceil((N-M)/(NB-M)) = ceil(5/1) = 5; T cols = NUMBLK * M = 15.
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  do j = 1, 8
    do i = 1, 3
      if (i .eq. j) then
        A(i,j) = 4.0d0 + dble(j)
      else
        A(i,j) = 1.0d0 / dble(abs(i - j) + 1)
      end if
    end do
  end do
  M = 3; N = 8; MB = 2; NB = 4
  NUMBLK = ceiling( dble(N - M) / dble(NB - M) )
  TC = NUMBLK * M
  call DLASWLQ(M, N, MB, NB, A, NMAX, T, MBMAX, WORK, MB*M, INFO)
  call begin_test('m3_n8_mb2_nb4')
  call pack_matrix(A, NMAX, M, N, Apk)
  call print_array('A', Apk, M*N)
  call pack_matrix(T, MBMAX, MB, TC, Tpk)
  call print_array('T', Tpk, MB*TC)
  call print_int('INFO', INFO)
  call print_int('NUMBLK', NUMBLK)
  call end_test()

  ! Test 2: N divides evenly -- M=2, N=10, MB=2, NB=4.
  ! NB-M=2; (N-M)=8; mod(8,2)=0 -> KK=0; NUMBLK = (N-M)/(NB-M) = 4
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  do j = 1, 10
    do i = 1, 2
      if (i .eq. j) then
        A(i,j) = 6.0d0 + dble(j)
      else
        A(i,j) = 1.0d0 / dble(abs(i - j) + 1)
      end if
    end do
  end do
  M = 2; N = 10; MB = 2; NB = 4
  NUMBLK = ceiling( dble(N - M) / dble(NB - M) )
  TC = NUMBLK * M
  call DLASWLQ(M, N, MB, NB, A, NMAX, T, MBMAX, WORK, MB*M, INFO)
  call begin_test('m2_n10_mb2_nb4_evendiv')
  call pack_matrix(A, NMAX, M, N, Apk)
  call print_array('A', Apk, M*N)
  call pack_matrix(T, MBMAX, MB, TC, Tpk)
  call print_array('T', Tpk, MB*TC)
  call print_int('INFO', INFO)
  call print_int('NUMBLK', NUMBLK)
  call end_test()

  ! Test 3: short fall-through to DGELQT -- NB >= N, so single DGELQT.
  ! M=3, N=4, MB=2, NB=8. NB > N => routes to DGELQT(M, N, MB, ...).
  ! Then T has M columns (per DGELQT layout, since min(M,N)=M).
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0; A(1,4) = 1.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0; A(2,4) = 3.0d0
  A(3,1) = 3.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0; A(3,4) = 1.0d0
  M = 3; N = 4; MB = 2; NB = 8
  call DLASWLQ(M, N, MB, NB, A, NMAX, T, MBMAX, WORK, MB*M, INFO)
  call begin_test('m3_n4_mb2_nb8_fallthrough')
  call pack_matrix(A, NMAX, M, N, Apk)
  call print_array('A', Apk, M*N)
  call pack_matrix(T, MBMAX, MB, M, Tpk)
  call print_array('T', Tpk, MB*M)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 4: NB == M also routes to DGELQT.
  ! M=3, N=6, MB=2, NB=3 (NB == M).
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  do j = 1, 6
    do i = 1, 3
      if (i .eq. j) then
        A(i,j) = 4.0d0 + dble(j)
      else
        A(i,j) = 1.0d0 / dble(abs(i - j) + 2)
      end if
    end do
  end do
  M = 3; N = 6; MB = 2; NB = 3
  call DLASWLQ(M, N, MB, NB, A, NMAX, T, MBMAX, WORK, MB*M, INFO)
  call begin_test('m3_n6_mb2_nb3_nbeqm')
  call pack_matrix(A, NMAX, M, N, Apk)
  call print_array('A', Apk, M*N)
  call pack_matrix(T, MBMAX, MB, M, Tpk)
  call print_array('T', Tpk, MB*M)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 5: KK > 0 (last block triggers extra DTPLQT).
  ! M=3, N=12, MB=3, NB=5: (N-M)=9, NB-M=2, mod=1, KK=1, II=12 -> trailing block.
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  do j = 1, 12
    do i = 1, 3
      if (i .eq. j) then
        A(i,j) = 5.0d0 + dble(j) * 0.5d0
      else
        A(i,j) = dble(mod(i*7 + j*3, 11)) / 5.0d0 + 0.1d0
      end if
    end do
  end do
  M = 3; N = 12; MB = 3; NB = 5
  NUMBLK = ceiling( dble(N - M) / dble(NB - M) )
  TC = NUMBLK * M
  call DLASWLQ(M, N, MB, NB, A, NMAX, T, MBMAX, WORK, MB*M, INFO)
  call begin_test('m3_n12_mb3_nb5_lastblock')
  call pack_matrix(A, NMAX, M, N, Apk)
  call print_array('A', Apk, M*N)
  call pack_matrix(T, MBMAX, MB, TC, Tpk)
  call print_array('T', Tpk, MB*TC)
  call print_int('INFO', INFO)
  call print_int('NUMBLK', NUMBLK)
  call end_test()

  ! Test 6: M==N edge case (square matrix). M>=N triggers single DGELQT.
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
  M = 4; N = 4; MB = 2; NB = 6
  call DLASWLQ(M, N, MB, NB, A, NMAX, T, MBMAX, WORK, MB*M, INFO)
  call begin_test('m4_n4_mb2_nb6_square')
  call pack_matrix(A, NMAX, M, N, Apk)
  call print_array('A', Apk, M*N)
  call pack_matrix(T, MBMAX, MB, M, Tpk)
  call print_array('T', Tpk, MB*M)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 7: MB=1 (single-reflector inner block size) with blocked TSLQ.
  ! M=2, N=9, MB=1, NB=4. (N-M)=7, NB-M=2, mod=1, KK=1, last block triggered.
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  do j = 1, 9
    do i = 1, 2
      A(i,j) = dble(mod(i*5 + j*7, 13)) / 4.0d0 + 0.5d0
      if (i .eq. j) A(i,j) = A(i,j) + 4.0d0
    end do
  end do
  M = 2; N = 9; MB = 1; NB = 4
  NUMBLK = ceiling( dble(N - M) / dble(NB - M) )
  TC = NUMBLK * M
  call DLASWLQ(M, N, MB, NB, A, NMAX, T, MBMAX, WORK, MB*M, INFO)
  call begin_test('m2_n9_mb1_nb4')
  call pack_matrix(A, NMAX, M, N, Apk)
  call print_array('A', Apk, M*N)
  call pack_matrix(T, MBMAX, MB, TC, Tpk)
  call print_array('T', Tpk, MB*TC)
  call print_int('INFO', INFO)
  call print_int('NUMBLK', NUMBLK)
  call end_test()

  ! Test 8: M=0 quick return.
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  M = 0; N = 0; MB = 2; NB = 4
  call DLASWLQ(M, N, MB, NB, A, NMAX, T, MBMAX, WORK, max(MB*max(M,1),1), INFO)
  call begin_test('m0_n0_quickreturn')
  call print_int('INFO', INFO)
  call end_test()

  ! Test 9: N=0 quick return (M=0 required since M<=N, but test M=0,N=0 already covered;
  ! here M>0, N=0 violates M<=N -- so we test M=0, N>0 instead).
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  M = 0; N = 3; MB = 1; NB = 4
  call DLASWLQ(M, N, MB, NB, A, NMAX, T, MBMAX, WORK, max(1,1), INFO)
  call begin_test('m0_n3_quickreturn')
  call print_int('INFO', INFO)
  call end_test()

  ! Test 10: short and wide -- M=4, N=20, MB=4, NB=8.
  ! (N-M)=16, NB-M=4, mod=0; numblk=4.
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  do j = 1, 20
    do i = 1, 4
      A(i,j) = dble(mod(i*11 + j*5, 17)) / 6.0d0 + 0.2d0
      if (i .eq. j) A(i,j) = A(i,j) + 6.0d0
    end do
  end do
  M = 4; N = 20; MB = 4; NB = 8
  NUMBLK = ceiling( dble(N - M) / dble(NB - M) )
  TC = NUMBLK * M
  call DLASWLQ(M, N, MB, NB, A, NMAX, T, MBMAX, WORK, MB*M, INFO)
  call begin_test('m4_n20_mb4_nb8')
  call pack_matrix(A, NMAX, M, N, Apk)
  call print_array('A', Apk, M*N)
  call pack_matrix(T, MBMAX, MB, TC, Tpk)
  call print_array('T', Tpk, MB*TC)
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
