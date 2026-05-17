program test_zlamtsqr
  use test_utils
  implicit none

  ! Test ZLAMTSQR: apply Q from a TSQR factorization (built by ZLATSQR) to C.
  ! For each scenario we emit the factorization (A, T) AND the input C0 and the
  ! resulting C, so the JS test can call zlamtsqr directly without recomputing
  ! the factorization.

  integer, parameter :: NMAX = 32
  integer, parameter :: NBMAX = 8
  integer, parameter :: TCOLS = 64
  integer, parameter :: WMAX = 4096

  ! Scratch arrays declared at the program scope.
  complex*16 :: A(NMAX, NMAX), T(NBMAX, TCOLS)
  complex*16 :: C(NMAX, NMAX), C0(NMAX, NMAX)
  complex*16 :: WORK(WMAX)

  ! Packed buffers for printing (matches actual M*N or NB*TC dims).
  double precision :: Apk_r(2*NMAX*NMAX), Tpk_r(2*NBMAX*TCOLS)
  double precision :: Cpk_r(2*NMAX*NMAX), C0pk_r(2*NMAX*NMAX)
  complex*16       :: Apk(NMAX*NMAX), Tpk(NBMAX*TCOLS)
  complex*16       :: Cpk(NMAX*NMAX), C0pk(NMAX*NMAX)
  equivalence (Apk, Apk_r)
  equivalence (Tpk, Tpk_r)
  equivalence (Cpk, Cpk_r)
  equivalence (C0pk, C0pk_r)

  integer :: INFO, M, N, K, MB, NB, NUMBLK, TC, MR

  ! ============================================================
  ! Test 1: SIDE='L', TRANS='N': basic blocked TSQR apply with KK > 0.
  ! M=12, K=3, MB=5, NB=2: M-K=9, MB-K=2, mod=1 (KK=1, trailing block).
  ! ============================================================
  M = 12; K = 3; MB = 5; NB = 2
  call build_panel(M, K, A)
  call ZLATSQR(M, K, MB, NB, A, NMAX, T, NBMAX, WORK, NB*K, INFO)
  NUMBLK = ceiling( dble(M - K) / dble(MB - K) )
  TC = NUMBLK * K
  call build_C(M, 4, C0)
  C = C0
  call ZLAMTSQR('L', 'N', M, 4, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, 4*NB, INFO)
  call emit('left_notrans_blocked', M, K, MB, NB, 4, A, T, TC, C0, C, INFO, .true.)

  ! ============================================================
  ! Test 2: SIDE='L', TRANS='C': Q^H * C using same factorization.
  ! ============================================================
  C = C0
  call ZLAMTSQR('L', 'C', M, 4, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, 4*NB, INFO)
  call emit('left_ctrans_blocked', M, K, MB, NB, 4, A, T, TC, C0, C, INFO, .true.)

  ! ============================================================
  ! Test 3: SIDE='R', TRANS='N': C * Q with KK > 0.
  ! N=12, K=3, MB=5, NB=2: N-K=9, MB-K=2, mod=1 (KK=1, trailing block).
  ! ============================================================
  N = 12; K = 3; MB = 5; NB = 2
  call build_panel(N, K, A)
  call ZLATSQR(N, K, MB, NB, A, NMAX, T, NBMAX, WORK, NB*K, INFO)
  NUMBLK = ceiling( dble(N - K) / dble(MB - K) )
  TC = NUMBLK * K
  MR = 5
  call build_C(MR, N, C0)
  C = C0
  call ZLAMTSQR('R', 'N', MR, N, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, MR*NB, INFO)
  call emit_right('right_notrans_blocked', N, K, MB, NB, MR, A, T, TC, C0, C, INFO)

  ! ============================================================
  ! Test 4: SIDE='R', TRANS='C': C * Q^H using same factorization.
  ! ============================================================
  C = C0
  call ZLAMTSQR('R', 'C', MR, N, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, MR*NB, INFO)
  call emit_right('right_ctrans_blocked', N, K, MB, NB, MR, A, T, TC, C0, C, INFO)

  ! ============================================================
  ! Test 5/6: SIDE='L', TRANS='N'/'C': fall-through to ZGEMQRT (MB > M).
  ! M=4, K=3, MB=8 (MB > MAX(M,N,K)). Use ZGEQRT directly to build A,T.
  ! ============================================================
  M = 4; K = 3; MB = 8; NB = 2
  call build_panel(M, K, A)
  call ZGEQRT(M, K, NB, A, NMAX, T, NBMAX, WORK, INFO)
  call build_C(M, 3, C0)
  C = C0
  call ZLAMTSQR('L', 'N', M, 3, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, 3*NB, INFO)
  call emit('left_notrans_fallthrough', M, K, MB, NB, 3, A, T, K, C0, C, INFO, .false.)

  C = C0
  call ZLAMTSQR('L', 'C', M, 3, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, 3*NB, INFO)
  call emit('left_ctrans_fallthrough', M, K, MB, NB, 3, A, T, K, C0, C, INFO, .false.)

  ! ============================================================
  ! Test 7/8: SIDE='L', TRANS='N'/'C': KK=0 case.
  ! M=10, K=2, MB=4 (MB-K=2, M-K=8, mod=0).
  ! ============================================================
  M = 10; K = 2; MB = 4; NB = 2
  call build_panel(M, K, A)
  call ZLATSQR(M, K, MB, NB, A, NMAX, T, NBMAX, WORK, NB*K, INFO)
  NUMBLK = ceiling( dble(M - K) / dble(MB - K) )
  TC = NUMBLK * K
  call build_C(M, 3, C0)
  C = C0
  call ZLAMTSQR('L', 'N', M, 3, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, 3*NB, INFO)
  call emit('left_notrans_kk0', M, K, MB, NB, 3, A, T, TC, C0, C, INFO, .true.)

  C = C0
  call ZLAMTSQR('L', 'C', M, 3, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, 3*NB, INFO)
  call emit('left_ctrans_kk0', M, K, MB, NB, 3, A, T, TC, C0, C, INFO, .true.)

  ! ============================================================
  ! Test 9/10: SIDE='R', KK=0 case.
  ! N=10, K=2, MB=4, MR=4.
  ! ============================================================
  N = 10; K = 2; MB = 4; NB = 2
  call build_panel(N, K, A)
  call ZLATSQR(N, K, MB, NB, A, NMAX, T, NBMAX, WORK, NB*K, INFO)
  NUMBLK = ceiling( dble(N - K) / dble(MB - K) )
  TC = NUMBLK * K
  MR = 4
  call build_C(MR, N, C0)
  C = C0
  call ZLAMTSQR('R', 'N', MR, N, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, MR*NB, INFO)
  call emit_right('right_notrans_kk0', N, K, MB, NB, MR, A, T, TC, C0, C, INFO)

  C = C0
  call ZLAMTSQR('R', 'C', MR, N, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, MR*NB, INFO)
  call emit_right('right_ctrans_kk0', N, K, MB, NB, MR, A, T, TC, C0, C, INFO)

  ! ============================================================
  ! Test 11/12: SIDE='R', TRANS='N'/'C': fall-through (NB == K, MB > N).
  ! N=4, K=3, MB=8, MR=5.
  ! ============================================================
  N = 4; K = 3; MB = 8; NB = 2
  call build_panel(N, K, A)
  call ZGEQRT(N, K, NB, A, NMAX, T, NBMAX, WORK, INFO)
  MR = 5
  call build_C(MR, N, C0)
  C = C0
  call ZLAMTSQR('R', 'N', MR, N, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, MR*NB, INFO)
  call emit_right('right_notrans_fallthrough', N, K, MB, NB, MR, A, T, K, C0, C, INFO)

  C = C0
  call ZLAMTSQR('R', 'C', MR, N, K, MB, NB, A, NMAX, T, NBMAX, C, NMAX, WORK, MR*NB, INFO)
  call emit_right('right_ctrans_fallthrough', N, K, MB, NB, MR, A, T, K, C0, C, INFO)

contains

  subroutine build_panel(mm, kk, dst)
    integer, intent(in) :: mm, kk
    complex*16, intent(out) :: dst(NMAX, NMAX)
    integer :: ii, jj
    dst = (0.0d0, 0.0d0)
    do jj = 1, kk
      do ii = 1, mm
        if (ii .eq. jj) then
          dst(ii,jj) = dcmplx(4.0d0 + dble(jj), 0.2d0)
        else
          dst(ii,jj) = dcmplx(1.0d0 / dble(abs(ii - jj) + 1), -0.1d0 * dble(ii - jj))
        end if
      end do
    end do
  end subroutine build_panel

  subroutine build_C(mr, nc, dst)
    integer, intent(in) :: mr, nc
    complex*16, intent(out) :: dst(NMAX, NMAX)
    integer :: ii, jj
    dst = (0.0d0, 0.0d0)
    do jj = 1, nc
      do ii = 1, mr
        dst(ii,jj) = dcmplx(dble(ii + jj), -0.5d0 * dble(ii - jj))
      end do
    end do
  end subroutine build_C

  subroutine pack_zmatrix(src, lds, mr, nc, dst)
    integer, intent(in) :: lds, mr, nc
    complex*16, intent(in)  :: src(lds, *)
    complex*16, intent(out) :: dst(*)
    integer :: ii, jj
    do jj = 1, nc
      do ii = 1, mr
        dst(ii + (jj-1)*mr) = src(ii, jj)
      end do
    end do
  end subroutine pack_zmatrix

  ! Emit a SIDE='L' test case. mm = rows of A and C, kk = K, mbb/nbb = MB/NB
  ! (informational), nc = cols of C, tc = number of T columns. last
  ! `is_blocked` controls whether A/T is M-by-K / NB-by-TC (blocked) or just
  ! M-by-K / NB-by-K (fall-through).
  subroutine emit(name, mm, kk, mbb, nbb, nc, Ain, Tin, tc, C0in, Cin, info, is_blocked)
    character(len=*), intent(in) :: name
    integer, intent(in) :: mm, kk, mbb, nbb, nc, tc, info
    complex*16, intent(in) :: Ain(NMAX,*), Tin(NBMAX,*)
    complex*16, intent(in) :: C0in(NMAX,*), Cin(NMAX,*)
    logical, intent(in) :: is_blocked
    if (is_blocked) continue
    call begin_test(name)
    call print_int('M', mm)
    call print_int('N', nc)
    call print_int('K', kk)
    call print_int('MB', mbb)
    call print_int('NB', nbb)
    call pack_zmatrix(Ain, NMAX, mm, kk, Apk)
    call print_array('A', Apk_r, 2*mm*kk)
    call pack_zmatrix(Tin, NBMAX, nbb, tc, Tpk)
    call print_array('T', Tpk_r, 2*nbb*tc)
    call print_int('LDT', nbb)
    call pack_zmatrix(C0in, NMAX, mm, nc, C0pk)
    call print_array('C0', C0pk_r, 2*mm*nc)
    call pack_zmatrix(Cin, NMAX, mm, nc, Cpk)
    call print_array('C', Cpk_r, 2*mm*nc)
    call print_int('INFO', info)
    call end_test()
  end subroutine emit

  ! Emit a SIDE='R' test case. nn = rows of A (which is rows-of-Q), kk = K,
  ! mr = rows of C, nn = cols of C.
  subroutine emit_right(name, nn, kk, mbb, nbb, mr, Ain, Tin, tc, C0in, Cin, info)
    character(len=*), intent(in) :: name
    integer, intent(in) :: nn, kk, mbb, nbb, mr, tc, info
    complex*16, intent(in) :: Ain(NMAX,*), Tin(NBMAX,*)
    complex*16, intent(in) :: C0in(NMAX,*), Cin(NMAX,*)
    call begin_test(name)
    call print_int('M', mr)
    call print_int('N', nn)
    call print_int('K', kk)
    call print_int('MB', mbb)
    call print_int('NB', nbb)
    call pack_zmatrix(Ain, NMAX, nn, kk, Apk)
    call print_array('A', Apk_r, 2*nn*kk)
    call pack_zmatrix(Tin, NBMAX, nbb, tc, Tpk)
    call print_array('T', Tpk_r, 2*nbb*tc)
    call print_int('LDT', nbb)
    call pack_zmatrix(C0in, NMAX, mr, nn, C0pk)
    call print_array('C0', C0pk_r, 2*mr*nn)
    call pack_zmatrix(Cin, NMAX, mr, nn, Cpk)
    call print_array('C', Cpk_r, 2*mr*nn)
    call print_int('INFO', info)
    call end_test()
  end subroutine emit_right

end program
