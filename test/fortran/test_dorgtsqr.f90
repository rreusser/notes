program test_dorgtsqr
  use test_utils
  implicit none

  ! Test DORGTSQR: generate the M-by-N orthonormal Q produced by DLATSQR.
  ! Pattern: factor a tall A with DLATSQR, then call DORGTSQR to form Q
  ! explicitly. Verify by printing Q and Q^T*Q (should be identity for the
  ! first N columns).

  integer, parameter :: NMAX = 80
  integer, parameter :: NBMAX = 16
  integer, parameter :: TCOLS = 256
  integer, parameter :: WMAX = NMAX*NMAX*4

  double precision :: A(NMAX, NMAX), T(NBMAX, TCOLS), WORK(WMAX)
  double precision :: QtQ(NMAX, NMAX)
  double precision :: Apk(NMAX*NMAX), Qpk(NMAX*NMAX)
  integer :: INFO, i, j, k, M, N, MB, NB, NUMBLK, LW, LWORK, LWQR

  ! ====================================================================
  ! Test 1: M=8, N=3, MB=4, NB=2 — non-trivial TSQR partition.
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
  M = 8; N = 3; MB = 4; NB = 2
  NUMBLK = ceiling( dble(M - N) / dble(MB - N) )
  LWQR = NB * N
  call DLATSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, LWQR, INFO)
  LWORK = (M + NB) * N
  call DORGTSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, LWORK, INFO)
  ! Compute Q^T * Q
  QtQ = 0.0d0
  do i = 1, N
    do j = 1, N
      do k = 1, M
        QtQ(i, j) = QtQ(i, j) + A(k, i) * A(k, j)
      end do
    end do
  end do
  call begin_test('m8_n3_mb4_nb2')
  call pack_matrix(A, NMAX, M, N, Qpk)
  call print_array('Q', Qpk, M*N)
  call pack_matrix(QtQ, NMAX, N, N, Apk)
  call print_array('QtQ', Apk, N*N)
  call print_int('INFO', INFO)
  call end_test()

  ! ====================================================================
  ! Test 2: M=10, N=2, MB=4, NB=2 (M-N divides evenly).
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
  M = 10; N = 2; MB = 4; NB = 2
  LWQR = NB * N
  call DLATSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, LWQR, INFO)
  LWORK = (M + NB) * N
  call DORGTSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, LWORK, INFO)
  QtQ = 0.0d0
  do i = 1, N
    do j = 1, N
      do k = 1, M
        QtQ(i, j) = QtQ(i, j) + A(k, i) * A(k, j)
      end do
    end do
  end do
  call begin_test('m10_n2_mb4_nb2_evendiv')
  call pack_matrix(A, NMAX, M, N, Qpk)
  call print_array('Q', Qpk, M*N)
  call pack_matrix(QtQ, NMAX, N, N, Apk)
  call print_array('QtQ', Apk, N*N)
  call print_int('INFO', INFO)
  call end_test()

  ! ====================================================================
  ! Test 3: MB > M — DLATSQR/DLAMTSQR collapse to single dgeqrt/dgemqrt.
  ! M=4, N=3, MB=8, NB=2.
  ! ====================================================================
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0
  A(3,1) = 3.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0
  A(4,1) = 1.0d0; A(4,2) = 3.0d0; A(4,3) = 1.0d0
  M = 4; N = 3; MB = 8; NB = 2
  LWQR = NB * N
  call DLATSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, LWQR, INFO)
  LWORK = (M + NB) * N
  call DORGTSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, LWORK, INFO)
  QtQ = 0.0d0
  do i = 1, N
    do j = 1, N
      do k = 1, M
        QtQ(i, j) = QtQ(i, j) + A(k, i) * A(k, j)
      end do
    end do
  end do
  call begin_test('m4_n3_mb8_nb2_fallthrough')
  call pack_matrix(A, NMAX, M, N, Qpk)
  call print_array('Q', Qpk, M*N)
  call pack_matrix(QtQ, NMAX, N, N, Apk)
  call print_array('QtQ', Apk, N*N)
  call print_int('INFO', INFO)
  call end_test()

  ! ====================================================================
  ! Test 4: Square M=N case. M=4, N=4, MB=8, NB=2.
  ! Note: MB must be > N, so MB=5+.
  ! ====================================================================
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
  M = 4; N = 4; MB = 8; NB = 2
  LWQR = NB * N
  call DLATSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, LWQR, INFO)
  LWORK = (M + NB) * N
  call DORGTSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, LWORK, INFO)
  QtQ = 0.0d0
  do i = 1, N
    do j = 1, N
      do k = 1, M
        QtQ(i, j) = QtQ(i, j) + A(k, i) * A(k, j)
      end do
    end do
  end do
  call begin_test('m4_n4_mb8_nb2_square')
  call pack_matrix(A, NMAX, M, N, Qpk)
  call print_array('Q', Qpk, M*N)
  call pack_matrix(QtQ, NMAX, N, N, Apk)
  call print_array('QtQ', Apk, N*N)
  call print_int('INFO', INFO)
  call end_test()

  ! ====================================================================
  ! Test 5: KK > 0 — trailing partial block. M=12, N=3, MB=5, NB=3.
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
  M = 12; N = 3; MB = 5; NB = 3
  LWQR = NB * N
  call DLATSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, LWQR, INFO)
  LWORK = (M + NB) * N
  call DORGTSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, LWORK, INFO)
  QtQ = 0.0d0
  do i = 1, N
    do j = 1, N
      do k = 1, M
        QtQ(i, j) = QtQ(i, j) + A(k, i) * A(k, j)
      end do
    end do
  end do
  call begin_test('m12_n3_mb5_nb3_lastblock')
  call pack_matrix(A, NMAX, M, N, Qpk)
  call print_array('Q', Qpk, M*N)
  call pack_matrix(QtQ, NMAX, N, N, Apk)
  call print_array('QtQ', Apk, N*N)
  call print_int('INFO', INFO)
  call end_test()

  ! ====================================================================
  ! Test 6: NB=1 (each reflector is its own column block). M=9, N=2, MB=4.
  ! ====================================================================
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  do j = 1, 2
    do i = 1, 9
      A(i,j) = dble(mod(i*5 + j*7, 13)) / 4.0d0 + 0.5d0
      if (i .eq. j) A(i,j) = A(i,j) + 4.0d0
    end do
  end do
  M = 9; N = 2; MB = 4; NB = 1
  LWQR = NB * N
  call DLATSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, LWQR, INFO)
  LWORK = (M + NB) * N
  call DORGTSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, LWORK, INFO)
  QtQ = 0.0d0
  do i = 1, N
    do j = 1, N
      do k = 1, M
        QtQ(i, j) = QtQ(i, j) + A(k, i) * A(k, j)
      end do
    end do
  end do
  call begin_test('m9_n2_mb4_nb1')
  call pack_matrix(A, NMAX, M, N, Qpk)
  call print_array('Q', Qpk, M*N)
  call pack_matrix(QtQ, NMAX, N, N, Apk)
  call print_array('QtQ', Apk, N*N)
  call print_int('INFO', INFO)
  call end_test()

  ! ====================================================================
  ! Test 7: Tall and skinny — M=20, N=4, MB=8, NB=4.
  ! ====================================================================
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  do j = 1, 4
    do i = 1, 20
      A(i,j) = dble(mod(i*11 + j*5, 17)) / 6.0d0 + 0.2d0
      if (i .eq. j) A(i,j) = A(i,j) + 6.0d0
    end do
  end do
  M = 20; N = 4; MB = 8; NB = 4
  LWQR = NB * N
  call DLATSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, LWQR, INFO)
  LWORK = (M + NB) * N
  call DORGTSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, LWORK, INFO)
  QtQ = 0.0d0
  do i = 1, N
    do j = 1, N
      do k = 1, M
        QtQ(i, j) = QtQ(i, j) + A(k, i) * A(k, j)
      end do
    end do
  end do
  call begin_test('m20_n4_mb8_nb4')
  call pack_matrix(A, NMAX, M, N, Qpk)
  call print_array('Q', Qpk, M*N)
  call pack_matrix(QtQ, NMAX, N, N, Apk)
  call print_array('QtQ', Apk, N*N)
  call print_int('INFO', INFO)
  call end_test()

  ! ====================================================================
  ! Test 8: M=0, N=0 quick return.
  ! ====================================================================
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  M = 0; N = 0; MB = 4; NB = 2
  LWORK = max(2, (M + NB) * max(N, 1))
  call DORGTSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, LWORK, INFO)
  call begin_test('m0_n0_quickreturn')
  call print_int('INFO', INFO)
  call end_test()

  ! ====================================================================
  ! Test 9: N=0, M>0 quick return.
  ! ====================================================================
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  M = 5; N = 0; MB = 4; NB = 1
  LWORK = max(2, (M + NB) * max(N, 1))
  call DORGTSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, LWORK, INFO)
  call begin_test('m5_n0_quickreturn')
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
