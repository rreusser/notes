program test_dlamswlq
  use test_utils
  implicit none

  ! Test dlamswlq: apply Q (or Q^T) from a DLASWLQ-produced factorization
  ! to a matrix C. Pattern: factor A with DLASWLQ (K-by-X wide matrix),
  ! then call DLAMSWLQ for all four (SIDE, TRANS) combinations.
  ! For SIDE='L', Q is M-by-M and A was K-by-M (K <= M).
  ! For SIDE='R', Q is N-by-N and A was K-by-N (K <= N).
  ! K = number of reflectors; MB inner block; NB column block (NB > K).

  integer, parameter :: NMAX = 40
  integer, parameter :: NBMAX = 16
  integer, parameter :: TCOLS = 256
  integer, parameter :: WMAX = NMAX*NMAX*4

  double precision :: A(NMAX, NMAX), T(NBMAX, TCOLS)
  double precision :: C(NMAX, NMAX), WORK(WMAX)
  double precision :: Apk(NMAX*NMAX), Tpk(NBMAX*TCOLS), Cpk(NMAX*NMAX)
  integer :: INFO, i, j, M, N, K, MB, NB, NUMBLK, TC, LW, X

  ! ====================================================================
  ! Case A: SIDE='L' cases. Factor A as K-by-M wide matrix.
  ! Use M=8, K=3, MB=2, NB=4 (genuine SWLQ partition: NB-K=1, M-K=5).
  ! ====================================================================
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  K = 3; X = 8; MB = 2; NB = 4
  ! Build a 3-by-8 wide A
  do j = 1, X
    do i = 1, K
      if (i .eq. j) then
        A(i,j) = 4.0d0 + dble(i)
      else
        A(i,j) = 1.0d0 / dble(abs(i - j) + 1)
      end if
    end do
  end do
  NUMBLK = ceiling( dble(X - K) / dble(NB - K) )
  TC = NUMBLK * K
  ! DLASWLQ(M, N, MB, NB, A, LDA, T, LDT, WORK, LWORK, INFO)
  call DLASWLQ(K, X, MB, NB, A, NMAX, T, NBMAX, WORK, MB*K, INFO)

  call begin_test('factor_k3_m8_mb2_nb4')
  call pack_matrix(A, NMAX, K, X, Apk)
  call print_array('A', Apk, K*X)
  call pack_matrix(T, NBMAX, MB, TC, Tpk)
  call print_array('T', Tpk, MB*TC)
  call print_int('INFO', INFO)
  call end_test()

  ! Test A.LN: SIDE='L', TRANS='N' on 8x4 dense C
  M = X
  N = 4
  do j = 1, N
    do i = 1, M
      C(i,j) = sin(dble(i + 2*j)) + 0.3d0
    end do
  end do
  LW = N * MB
  call DLAMSWLQ('L','N', M, N, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, LW, INFO)
  call begin_test('A_left_notrans')
  call pack_matrix(C, NMAX, M, N, Cpk)
  call print_array('C', Cpk, M*N)
  call print_int('INFO', INFO)
  call end_test()

  ! Test A.LT: SIDE='L', TRANS='T' on the same 8x4 dense C
  do j = 1, N
    do i = 1, M
      C(i,j) = sin(dble(i + 2*j)) + 0.3d0
    end do
  end do
  call DLAMSWLQ('L','T', M, N, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, LW, INFO)
  call begin_test('A_left_trans')
  call pack_matrix(C, NMAX, M, N, Cpk)
  call print_array('C', Cpk, M*N)
  call print_int('INFO', INFO)
  call end_test()

  ! Test A.RN: SIDE='R', TRANS='N'. Q acts on the right; cols of C equal X (Q is X-by-X).
  ! Use a 5x8 C: 5 rows, 8 cols.
  do j = 1, X
    do i = 1, 5
      C(i,j) = cos(dble(i*3 + j)) + 0.1d0 * dble(i)
    end do
  end do
  LW = 5 * MB
  call DLAMSWLQ('R','N', 5, X, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, LW, INFO)
  call begin_test('A_right_notrans')
  call pack_matrix(C, NMAX, 5, X, Cpk)
  call print_array('C', Cpk, 5*X)
  call print_int('INFO', INFO)
  call end_test()

  ! Test A.RT: SIDE='R', TRANS='T' on the same 5x8 dense C
  do j = 1, X
    do i = 1, 5
      C(i,j) = cos(dble(i*3 + j)) + 0.1d0 * dble(i)
    end do
  end do
  call DLAMSWLQ('R','T', 5, X, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, LW, INFO)
  call begin_test('A_right_trans')
  call pack_matrix(C, NMAX, 5, X, Cpk)
  call print_array('C', Cpk, 5*X)
  call print_int('INFO', INFO)
  call end_test()

  ! ====================================================================
  ! Case B: K=3, X=12, MB=3, NB=5 (KK > 0 -- trailing partial block).
  ! Factor a 3-by-12 wide matrix.
  ! ====================================================================
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  K = 3; X = 12; MB = 3; NB = 5
  do j = 1, X
    do i = 1, K
      if (i .eq. j) then
        A(i,j) = 5.0d0 + dble(i) * 0.5d0
      else
        A(i,j) = dble(mod(i*7 + j*3, 11)) / 5.0d0 + 0.1d0
      end if
    end do
  end do
  NUMBLK = ceiling( dble(X - K) / dble(NB - K) )
  TC = NUMBLK * K
  call DLASWLQ(K, X, MB, NB, A, NMAX, T, NBMAX, WORK, MB*K, INFO)

  call begin_test('factor_k3_m12_mb3_nb5')
  call pack_matrix(A, NMAX, K, X, Apk)
  call print_array('A', Apk, K*X)
  call pack_matrix(T, NBMAX, MB, TC, Tpk)
  call print_array('T', Tpk, MB*TC)
  call print_int('INFO', INFO)
  call end_test()

  M = X
  N = 2
  do j = 1, N
    do i = 1, M
      C(i,j) = sin(dble(2*i + j)) + 0.5d0
    end do
  end do
  LW = N * MB
  call DLAMSWLQ('L','N', M, N, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, LW, INFO)
  call begin_test('B_left_notrans')
  call pack_matrix(C, NMAX, M, N, Cpk)
  call print_array('C', Cpk, M*N)
  call print_int('INFO', INFO)
  call end_test()

  do j = 1, N
    do i = 1, M
      C(i,j) = sin(dble(2*i + j)) + 0.5d0
    end do
  end do
  call DLAMSWLQ('L','T', M, N, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, LW, INFO)
  call begin_test('B_left_trans')
  call pack_matrix(C, NMAX, M, N, Cpk)
  call print_array('C', Cpk, M*N)
  call print_int('INFO', INFO)
  call end_test()

  ! Right side: C is 4x12, Q is 12x12
  do j = 1, X
    do i = 1, 4
      C(i,j) = cos(dble(i + 2*j)) + 0.2d0
    end do
  end do
  LW = 4 * MB
  call DLAMSWLQ('R','N', 4, X, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, LW, INFO)
  call begin_test('B_right_notrans')
  call pack_matrix(C, NMAX, 4, X, Cpk)
  call print_array('C', Cpk, 4*X)
  call print_int('INFO', INFO)
  call end_test()

  do j = 1, X
    do i = 1, 4
      C(i,j) = cos(dble(i + 2*j)) + 0.2d0
    end do
  end do
  call DLAMSWLQ('R','T', 4, X, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, LW, INFO)
  call begin_test('B_right_trans')
  call pack_matrix(C, NMAX, 4, X, Cpk)
  call print_array('C', Cpk, 4*X)
  call print_int('INFO', INFO)
  call end_test()

  ! ====================================================================
  ! Case C: K=2, X=10, MB=2, NB=4 (KK == 0, even-divide, no trailing).
  ! (X-K)/(NB-K) = 8/2 = 4 with remainder 0.
  ! ====================================================================
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  K = 2; X = 10; MB = 2; NB = 4
  do j = 1, X
    do i = 1, K
      if (i .eq. j) then
        A(i,j) = 6.0d0 + dble(i)
      else
        A(i,j) = 1.0d0 / dble(abs(i - j) + 1)
      end if
    end do
  end do
  NUMBLK = ceiling( dble(X - K) / dble(NB - K) )
  TC = NUMBLK * K
  call DLASWLQ(K, X, MB, NB, A, NMAX, T, NBMAX, WORK, MB*K, INFO)

  call begin_test('factor_k2_m10_mb2_nb4_evendiv')
  call pack_matrix(A, NMAX, K, X, Apk)
  call print_array('A', Apk, K*X)
  call pack_matrix(T, NBMAX, MB, TC, Tpk)
  call print_array('T', Tpk, MB*TC)
  call print_int('INFO', INFO)
  call end_test()

  M = X
  N = 3
  do j = 1, N
    do i = 1, M
      C(i,j) = sin(dble(i*5 + j*3)) + 0.4d0
    end do
  end do
  LW = N * MB
  call DLAMSWLQ('L','N', M, N, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, LW, INFO)
  call begin_test('C_left_notrans')
  call pack_matrix(C, NMAX, M, N, Cpk)
  call print_array('C', Cpk, M*N)
  call print_int('INFO', INFO)
  call end_test()

  do j = 1, N
    do i = 1, M
      C(i,j) = sin(dble(i*5 + j*3)) + 0.4d0
    end do
  end do
  call DLAMSWLQ('L','T', M, N, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, LW, INFO)
  call begin_test('C_left_trans')
  call pack_matrix(C, NMAX, M, N, Cpk)
  call print_array('C', Cpk, M*N)
  call print_int('INFO', INFO)
  call end_test()

  do j = 1, X
    do i = 1, 3
      C(i,j) = cos(dble(i*2 + j*3)) + 0.7d0
    end do
  end do
  LW = 3 * MB
  call DLAMSWLQ('R','N', 3, X, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, LW, INFO)
  call begin_test('C_right_notrans')
  call pack_matrix(C, NMAX, 3, X, Cpk)
  call print_array('C', Cpk, 3*X)
  call print_int('INFO', INFO)
  call end_test()

  do j = 1, X
    do i = 1, 3
      C(i,j) = cos(dble(i*2 + j*3)) + 0.7d0
    end do
  end do
  call DLAMSWLQ('R','T', 3, X, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, LW, INFO)
  call begin_test('C_right_trans')
  call pack_matrix(C, NMAX, 3, X, Cpk)
  call print_array('C', Cpk, 3*X)
  call print_int('INFO', INFO)
  call end_test()

  ! ====================================================================
  ! Case D: short-and-wide fall-through to DGEMLQT (NB >= max(M,N,K)).
  ! ====================================================================
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  K = 3; X = 5; MB = 2; NB = 8
  do j = 1, X
    do i = 1, K
      if (i .eq. j) then
        A(i,j) = 4.0d0 + dble(i)
      else
        A(i,j) = 0.5d0 / dble(abs(i - j) + 1)
      end if
    end do
  end do
  ! DLASWLQ with NB >= X (X=5, NB=8) routes to a single dgelqt; T is MB-by-K
  call DLASWLQ(K, X, MB, NB, A, NMAX, T, NBMAX, WORK, MB*K, INFO)

  call begin_test('factor_k3_m5_mb2_nb8_fall')
  call pack_matrix(A, NMAX, K, X, Apk)
  call print_array('A', Apk, K*X)
  call pack_matrix(T, NBMAX, MB, K, Tpk)
  call print_array('T', Tpk, MB*K)
  call print_int('INFO', INFO)
  call end_test()

  M = X
  N = 4
  do j = 1, N
    do i = 1, M
      C(i,j) = sin(dble(i + 3*j)) + 0.2d0
    end do
  end do
  LW = N * MB
  call DLAMSWLQ('L','N', M, N, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, LW, INFO)
  call begin_test('D_left_notrans_fall')
  call pack_matrix(C, NMAX, M, N, Cpk)
  call print_array('C', Cpk, M*N)
  call print_int('INFO', INFO)
  call end_test()

  do j = 1, N
    do i = 1, M
      C(i,j) = sin(dble(i + 3*j)) + 0.2d0
    end do
  end do
  call DLAMSWLQ('L','T', M, N, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, LW, INFO)
  call begin_test('D_left_trans_fall')
  call pack_matrix(C, NMAX, M, N, Cpk)
  call print_array('C', Cpk, M*N)
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
        dst(ii + (jj-1)*mr) = src(ii, jj)
      end do
    end do
  end subroutine pack_matrix

end program
