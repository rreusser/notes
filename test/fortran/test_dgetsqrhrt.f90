program test_dgetsqrhrt
  use test_utils
  implicit none

  ! Use a single large allocation; pack matrices with exact dims before
  ! printing to avoid leading-dimension/EQUIVALENCE mismatches.
  integer, parameter :: NMAX = 80
  integer, parameter :: NBMAX = 16
  integer, parameter :: TCMAX = 80
  integer, parameter :: LWMAX = 4096
  double precision :: A(NMAX, NMAX), T(NBMAX, TCMAX), WORK(LWMAX)
  double precision :: Apk(NMAX*NMAX), Tpk(NBMAX*TCMAX)
  integer :: INFO, i, j, M, N, MB1, NB1, NB2, NB1L, NB2L, NUMBLK, LWT, LW1, LW2, LWORK
  integer :: TCOLS

  ! ---------- Test 1: M=8, N=3, MB1=4, NB1=2, NB2=2 (basic blocked TSQR + HRT) ----------
  ! NB1L = min(NB1,N) = 2; NUMBLK = ceil((8-3)/(4-3)) = 5
  ! LWT = 5*3*2 = 30; LW1 = 2*3 = 6; LW2 = 2*max(2, 3-2) = 4
  ! LWORK >= max(LWT+LW1, LWT+N*N+LW2, LWT+N*N+N) = max(36, 39+4, 39+3) = 43
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  do j = 1, 3
    do i = 1, 8
      A(i, j) = sin(real(i + 3*j, 8)) + 0.5d0 * cos(real(2*i - j, 8))
      if (i .eq. j) A(i, j) = A(i, j) + 4.0d0
    end do
  end do
  M = 8; N = 3; MB1 = 4; NB1 = 2; NB2 = 2
  NB1L = min(NB1, N); NB2L = min(NB2, N)
  NUMBLK = max(1, ceiling(dble(M - N) / dble(MB1 - N)))
  LWT = NUMBLK * N * NB1L
  LW1 = NB1L * N
  LW2 = NB1L * max(NB1L, N - NB1L)
  LWORK = max(LWT + LW1, max(LWT + N*N + LW2, LWT + N*N + N))
  TCOLS = (((N - 1) / NB2L) + 1) * NB2L
  call DGETSQRHRT(M, N, MB1, NB1, NB2, A, NMAX, T, NBMAX, WORK, LWORK, INFO)
  call begin_test('m8_n3_mb4_nb1_2_nb2_2')
  call pack_matrix(A, NMAX, M, N, Apk)
  call print_array('A', Apk, M*N)
  call pack_matrix(T, NBMAX, NB2L, TCOLS, Tpk)
  call print_array('T', Tpk, NB2L*TCOLS)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------- Test 2: M=10, N=2, MB1=4, NB1=2, NB2=1 (NB2 != NB1) ----------
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  do j = 1, 2
    do i = 1, 10
      A(i, j) = sin(real(2*i + j, 8)) + 0.3d0 * cos(real(i - 2*j, 8))
      if (i .eq. j) A(i, j) = A(i, j) + 6.0d0
    end do
  end do
  M = 10; N = 2; MB1 = 4; NB1 = 2; NB2 = 1
  NB1L = min(NB1, N); NB2L = min(NB2, N)
  NUMBLK = max(1, ceiling(dble(M - N) / dble(MB1 - N)))
  LWT = NUMBLK * N * NB1L
  LW1 = NB1L * N
  LW2 = NB1L * max(NB1L, N - NB1L)
  LWORK = max(LWT + LW1, max(LWT + N*N + LW2, LWT + N*N + N))
  TCOLS = (((N - 1) / NB2L) + 1) * NB2L
  call DGETSQRHRT(M, N, MB1, NB1, NB2, A, NMAX, T, NBMAX, WORK, LWORK, INFO)
  call begin_test('m10_n2_mb4_nb1_2_nb2_1')
  call pack_matrix(A, NMAX, M, N, Apk)
  call print_array('A', Apk, M*N)
  call pack_matrix(T, NBMAX, NB2L, TCOLS, Tpk)
  call print_array('T', Tpk, NB2L*TCOLS)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------- Test 3: M=N=4, MB1=6 (square; MB1 > M -> single dgeqrt path) ----------
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  do j = 1, 4
    do i = 1, 4
      A(i, j) = sin(real(i*3 + j*5, 8)) + 0.2d0
      if (i .eq. j) A(i, j) = A(i, j) + 5.0d0
    end do
  end do
  M = 4; N = 4; MB1 = 6; NB1 = 2; NB2 = 2
  NB1L = min(NB1, N); NB2L = min(NB2, N)
  NUMBLK = max(1, ceiling(dble(M - N) / dble(MB1 - N)))
  LWT = NUMBLK * N * NB1L
  LW1 = NB1L * N
  LW2 = NB1L * max(NB1L, N - NB1L)
  LWORK = max(LWT + LW1, max(LWT + N*N + LW2, LWT + N*N + N))
  TCOLS = (((N - 1) / NB2L) + 1) * NB2L
  call DGETSQRHRT(M, N, MB1, NB1, NB2, A, NMAX, T, NBMAX, WORK, LWORK, INFO)
  call begin_test('m4_n4_mb6_square')
  call pack_matrix(A, NMAX, M, N, Apk)
  call print_array('A', Apk, M*N)
  call pack_matrix(T, NBMAX, NB2L, TCOLS, Tpk)
  call print_array('T', Tpk, NB2L*TCOLS)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------- Test 4: M=12, N=3, MB1=5, NB1=3, NB2=3 (KK > 0, last partial block) ----------
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  do j = 1, 3
    do i = 1, 12
      A(i, j) = sin(real(i + 4*j, 8)) + 0.5d0 * cos(real(3*i - j, 8))
      if (i .eq. j) A(i, j) = A(i, j) + 5.0d0
    end do
  end do
  M = 12; N = 3; MB1 = 5; NB1 = 3; NB2 = 3
  NB1L = min(NB1, N); NB2L = min(NB2, N)
  NUMBLK = max(1, ceiling(dble(M - N) / dble(MB1 - N)))
  LWT = NUMBLK * N * NB1L
  LW1 = NB1L * N
  LW2 = NB1L * max(NB1L, N - NB1L)
  LWORK = max(LWT + LW1, max(LWT + N*N + LW2, LWT + N*N + N))
  TCOLS = (((N - 1) / NB2L) + 1) * NB2L
  call DGETSQRHRT(M, N, MB1, NB1, NB2, A, NMAX, T, NBMAX, WORK, LWORK, INFO)
  call begin_test('m12_n3_mb5_nb1_3_nb2_3')
  call pack_matrix(A, NMAX, M, N, Apk)
  call print_array('A', Apk, M*N)
  call pack_matrix(T, NBMAX, NB2L, TCOLS, Tpk)
  call print_array('T', Tpk, NB2L*TCOLS)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------- Test 5: M=20, N=5, MB1=8, NB1=3, NB2=2 (tall and skinny, multiple HRT blocks) ----------
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  do j = 1, 5
    do i = 1, 20
      A(i, j) = sin(real(i + 7*j, 8)) + 0.4d0 * cos(real(5*i - 2*j, 8))
      if (i .eq. j) A(i, j) = A(i, j) + 7.0d0
    end do
  end do
  M = 20; N = 5; MB1 = 8; NB1 = 3; NB2 = 2
  NB1L = min(NB1, N); NB2L = min(NB2, N)
  NUMBLK = max(1, ceiling(dble(M - N) / dble(MB1 - N)))
  LWT = NUMBLK * N * NB1L
  LW1 = NB1L * N
  LW2 = NB1L * max(NB1L, N - NB1L)
  LWORK = max(LWT + LW1, max(LWT + N*N + LW2, LWT + N*N + N))
  TCOLS = (((N - 1) / NB2L) + 1) * NB2L
  call DGETSQRHRT(M, N, MB1, NB1, NB2, A, NMAX, T, NBMAX, WORK, LWORK, INFO)
  call begin_test('m20_n5_mb8_nb1_3_nb2_2')
  call pack_matrix(A, NMAX, M, N, Apk)
  call print_array('A', Apk, M*N)
  call pack_matrix(T, NBMAX, NB2L, TCOLS, Tpk)
  call print_array('T', Tpk, NB2L*TCOLS)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------- Test 6: M=N=1 (smallest non-trivial; MB1 must be > N so MB1=2) ----------
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  A(1, 1) = 3.5d0
  M = 1; N = 1; MB1 = 2; NB1 = 1; NB2 = 1
  NB1L = min(NB1, N); NB2L = min(NB2, N)
  NUMBLK = max(1, ceiling(dble(M - N) / dble(MB1 - N)))
  LWT = NUMBLK * N * NB1L
  LW1 = NB1L * N
  LW2 = NB1L * max(NB1L, N - NB1L)
  LWORK = max(LWT + LW1, max(LWT + N*N + LW2, LWT + N*N + N))
  TCOLS = (((N - 1) / NB2L) + 1) * NB2L
  call DGETSQRHRT(M, N, MB1, NB1, NB2, A, NMAX, T, NBMAX, WORK, LWORK, INFO)
  call begin_test('m1_n1')
  call pack_matrix(A, NMAX, M, N, Apk)
  call print_array('A', Apk, M*N)
  call pack_matrix(T, NBMAX, NB2L, TCOLS, Tpk)
  call print_array('T', Tpk, NB2L*TCOLS)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------- Test 7: M=N=0 quick return ----------
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  M = 0; N = 0; MB1 = 1; NB1 = 1; NB2 = 1
  ! With N=0 the LWORK constraint reduces; we just need LWORK >= max(1, N*N+1) = 1.
  call DGETSQRHRT(M, N, 1, NB1, NB2, A, max(1, M), T, max(1, min(NB2, N)), WORK, max(1, N*N + 1), INFO)
  call begin_test('m0_n0_quickreturn')
  call print_int('INFO', INFO)
  call end_test()

  ! ---------- Test 8: M=6, N=6 square with MB1 just barely > N ----------
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  do j = 1, 6
    do i = 1, 6
      A(i, j) = sin(real(i*2 + j*3, 8)) + 0.3d0
      if (i .eq. j) A(i, j) = A(i, j) + 5.0d0
    end do
  end do
  M = 6; N = 6; MB1 = 7; NB1 = 3; NB2 = 4
  NB1L = min(NB1, N); NB2L = min(NB2, N)
  NUMBLK = max(1, ceiling(dble(M - N) / dble(MB1 - N)))
  LWT = NUMBLK * N * NB1L
  LW1 = NB1L * N
  LW2 = NB1L * max(NB1L, N - NB1L)
  LWORK = max(LWT + LW1, max(LWT + N*N + LW2, LWT + N*N + N))
  TCOLS = (((N - 1) / NB2L) + 1) * NB2L
  call DGETSQRHRT(M, N, MB1, NB1, NB2, A, NMAX, T, NBMAX, WORK, LWORK, INFO)
  call begin_test('m6_n6_mb7_nb1_3_nb2_4')
  call pack_matrix(A, NMAX, M, N, Apk)
  call print_array('A', Apk, M*N)
  call pack_matrix(T, NBMAX, NB2L, TCOLS, Tpk)
  call print_array('T', Tpk, NB2L*TCOLS)
  call print_int('INFO', INFO)
  call end_test()

contains

  subroutine pack_matrix(src, lds, mr, nc, dst)
    integer, intent(in) :: lds, mr, nc
    double precision, intent(in)  :: src(lds, *)
    double precision, intent(out) :: dst(*)
    integer :: ii, jj
    do jj = 1, nc
      do ii = 1, mr
        dst(ii + (jj - 1) * mr) = src(ii, jj)
      end do
    end do
  end subroutine pack_matrix

end program
