program test_dlamtsqr
  use test_utils
  implicit none

  ! Test dlamtsqr: apply Q (or Q^T) from a DLATSQR-produced factorization
  ! to a matrix C. Pattern: factor A with DLATSQR, then call DLAMTSQR for
  ! all four (SIDE, TRANS) combinations.

  integer, parameter :: NMAX = 40
  integer, parameter :: NBMAX = 16
  integer, parameter :: TCOLS = 256
  integer, parameter :: WMAX = NMAX*NMAX*4

  double precision :: A(NMAX, NMAX), T(NBMAX, TCOLS)
  double precision :: C(NMAX, NMAX), WORK(WMAX)
  double precision :: Apk(NMAX*NMAX), Tpk(NBMAX*TCOLS), Cpk(NMAX*NMAX)
  integer :: INFO, i, j, M, N, K, MB, NB, NUMBLK, TC, LW

  ! ====================================================================
  ! Case A: M=8, K=3, MB=4, NB=2 (genuine TSQR partition).
  ! Factor a tall 8x3 matrix; then apply Q to a 8x4 matrix C.
  ! ====================================================================
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
  M = 8; K = 3; MB = 4; NB = 2
  NUMBLK = ceiling( dble(M - K) / dble(MB - K) )
  TC = NUMBLK * K
  call DLATSQR(M, K, MB, NB, A, NMAX, T, NBMAX, WORK, NB*K, INFO)

  call begin_test('factor_m8_k3_mb4_nb2')
  call pack_matrix(A, NMAX, M, K, Apk)
  call print_array('A', Apk, M*K)
  call pack_matrix(T, NBMAX, NB, TC, Tpk)
  call print_array('T', Tpk, NB*TC)
  call print_int('INFO', INFO)
  call end_test()

  ! Test A.LN: SIDE='L', TRANS='N' on 8x4 dense C
  N = 4
  do j = 1, N
    do i = 1, M
      C(i,j) = sin(dble(i + 2*j)) + 0.3d0
    end do
  end do
  LW = N * NB
  call DLAMTSQR('L','N', M, N, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, LW, INFO)
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
  call DLAMTSQR('L','T', M, N, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, LW, INFO)
  call begin_test('A_left_trans')
  call pack_matrix(C, NMAX, M, N, Cpk)
  call print_array('C', Cpk, M*N)
  call print_int('INFO', INFO)
  call end_test()

  ! Test A.RN: SIDE='R', TRANS='N'. Q acts on the right; rows of C are
  ! whatever, columns equal to M (Q is M-by-M).
  ! Use a 5x8 C: 5 rows, 8 cols.
  do j = 1, M
    do i = 1, 5
      C(i,j) = cos(dble(i*3 + j)) + 0.1d0 * dble(i)
    end do
  end do
  LW = MB * NB
  call DLAMTSQR('R','N', 5, M, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, LW, INFO)
  call begin_test('A_right_notrans')
  call pack_matrix(C, NMAX, 5, M, Cpk)
  call print_array('C', Cpk, 5*M)
  call print_int('INFO', INFO)
  call end_test()

  ! Test A.RT: SIDE='R', TRANS='T' on the same 5x8 dense C
  do j = 1, M
    do i = 1, 5
      C(i,j) = cos(dble(i*3 + j)) + 0.1d0 * dble(i)
    end do
  end do
  call DLAMTSQR('R','T', 5, M, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, LW, INFO)
  call begin_test('A_right_trans')
  call pack_matrix(C, NMAX, 5, M, Cpk)
  call print_array('C', Cpk, 5*M)
  call print_int('INFO', INFO)
  call end_test()

  ! ====================================================================
  ! Case B: M=12, K=3, MB=5, NB=3 (KK > 0 -- trailing partial block).
  ! ====================================================================
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
  M = 12; K = 3; MB = 5; NB = 3
  NUMBLK = ceiling( dble(M - K) / dble(MB - K) )
  TC = NUMBLK * K
  call DLATSQR(M, K, MB, NB, A, NMAX, T, NBMAX, WORK, NB*K, INFO)

  call begin_test('factor_m12_k3_mb5_nb3')
  call pack_matrix(A, NMAX, M, K, Apk)
  call print_array('A', Apk, M*K)
  call pack_matrix(T, NBMAX, NB, TC, Tpk)
  call print_array('T', Tpk, NB*TC)
  call print_int('INFO', INFO)
  call end_test()

  ! All four combos with N=2
  N = 2
  do j = 1, N
    do i = 1, M
      C(i,j) = sin(dble(2*i + j)) + 0.5d0
    end do
  end do
  LW = N * NB
  call DLAMTSQR('L','N', M, N, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, LW, INFO)
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
  call DLAMTSQR('L','T', M, N, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, LW, INFO)
  call begin_test('B_left_trans')
  call pack_matrix(C, NMAX, M, N, Cpk)
  call print_array('C', Cpk, M*N)
  call print_int('INFO', INFO)
  call end_test()

  ! Right side: C is 4x12, Q is 12x12
  do j = 1, M
    do i = 1, 4
      C(i,j) = cos(dble(i + 2*j)) + 0.2d0
    end do
  end do
  LW = MB * NB
  call DLAMTSQR('R','N', 4, M, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, LW, INFO)
  call begin_test('B_right_notrans')
  call pack_matrix(C, NMAX, 4, M, Cpk)
  call print_array('C', Cpk, 4*M)
  call print_int('INFO', INFO)
  call end_test()

  do j = 1, M
    do i = 1, 4
      C(i,j) = cos(dble(i + 2*j)) + 0.2d0
    end do
  end do
  call DLAMTSQR('R','T', 4, M, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, LW, INFO)
  call begin_test('B_right_trans')
  call pack_matrix(C, NMAX, 4, M, Cpk)
  call print_array('C', Cpk, 4*M)
  call print_int('INFO', INFO)
  call end_test()

  ! ====================================================================
  ! Case C: M=10, K=2, MB=4, NB=2 (KK == 0, even-divide, no trailing).
  ! ====================================================================
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
  M = 10; K = 2; MB = 4; NB = 2
  NUMBLK = ceiling( dble(M - K) / dble(MB - K) )
  TC = NUMBLK * K
  call DLATSQR(M, K, MB, NB, A, NMAX, T, NBMAX, WORK, NB*K, INFO)

  call begin_test('factor_m10_k2_mb4_nb2_evendiv')
  call pack_matrix(A, NMAX, M, K, Apk)
  call print_array('A', Apk, M*K)
  call pack_matrix(T, NBMAX, NB, TC, Tpk)
  call print_array('T', Tpk, NB*TC)
  call print_int('INFO', INFO)
  call end_test()

  N = 3
  do j = 1, N
    do i = 1, M
      C(i,j) = sin(dble(i*5 + j*3)) + 0.4d0
    end do
  end do
  LW = N * NB
  call DLAMTSQR('L','N', M, N, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, LW, INFO)
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
  call DLAMTSQR('L','T', M, N, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, LW, INFO)
  call begin_test('C_left_trans')
  call pack_matrix(C, NMAX, M, N, Cpk)
  call print_array('C', Cpk, M*N)
  call print_int('INFO', INFO)
  call end_test()

  do j = 1, M
    do i = 1, 3
      C(i,j) = cos(dble(i*2 + j*3)) + 0.7d0
    end do
  end do
  LW = MB * NB
  call DLAMTSQR('R','N', 3, M, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, LW, INFO)
  call begin_test('C_right_notrans')
  call pack_matrix(C, NMAX, 3, M, Cpk)
  call print_array('C', Cpk, 3*M)
  call print_int('INFO', INFO)
  call end_test()

  do j = 1, M
    do i = 1, 3
      C(i,j) = cos(dble(i*2 + j*3)) + 0.7d0
    end do
  end do
  call DLAMTSQR('R','T', 3, M, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, LW, INFO)
  call begin_test('C_right_trans')
  call pack_matrix(C, NMAX, 3, M, Cpk)
  call print_array('C', Cpk, 3*M)
  call print_int('INFO', INFO)
  call end_test()

  ! ====================================================================
  ! Case D: short-and-wide fall-through to DGEMQRT (MB >= max(M,N,K)).
  ! ====================================================================
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  do j = 1, 3
    do i = 1, 5
      if (i .eq. j) then
        A(i,j) = 4.0d0 + dble(j)
      else
        A(i,j) = 0.5d0 / dble(abs(i - j) + 1)
      end if
    end do
  end do
  M = 5; K = 3; MB = 8; NB = 2
  ! DLATSQR with MB >= M routes to a single dgeqrt; T is NB-by-K
  call DLATSQR(M, K, MB, NB, A, NMAX, T, NBMAX, WORK, NB*K, INFO)

  call begin_test('factor_m5_k3_mb8_nb2_fall')
  call pack_matrix(A, NMAX, M, K, Apk)
  call print_array('A', Apk, M*K)
  call pack_matrix(T, NBMAX, NB, K, Tpk)
  call print_array('T', Tpk, NB*K)
  call print_int('INFO', INFO)
  call end_test()

  N = 4
  do j = 1, N
    do i = 1, M
      C(i,j) = sin(dble(i + 3*j)) + 0.2d0
    end do
  end do
  LW = N * NB
  call DLAMTSQR('L','N', M, N, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, LW, INFO)
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
  call DLAMTSQR('L','T', M, N, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, LW, INFO)
  call begin_test('D_left_trans_fall')
  call pack_matrix(C, NMAX, M, N, Cpk)
  call print_array('C', Cpk, M*N)
  call print_int('INFO', INFO)
  call end_test()

  ! ====================================================================
  ! Quick-return cases.
  ! ====================================================================
  ! M=0 (and M >= K so K must be 0). Validation requires K >= NB; the
  ! Fortran validator checks param 7 (NB) and rejects (K=0,NB>=1). To get
  ! a true quick-return, give M=K=0 with NB satisfying K >= NB only when
  ! both are zero. Fortran allows NB=1 with K=0 only via the early M=0 branch.
  ! To produce a clean fixture line we exercise the JS-side quick-return
  ! manually in the JS tests; skip in the Fortran fixture.

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
