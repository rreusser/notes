program test_zungtsqr
  use test_utils
  implicit none

  ! Test ZUNGTSQR: form an M-by-N complex matrix Q with orthonormal columns
  ! from a Tall-Skinny QR factorization (the V/T output of ZLATSQR).

  integer, parameter :: NMAX = 32
  integer, parameter :: NBMAX = 8
  integer, parameter :: TCOLS = 64
  integer, parameter :: WMAX = 8192

  ! Scratch arrays declared at the program scope.
  complex*16 :: A(NMAX, NMAX), T(NBMAX, TCOLS)
  complex*16 :: V(NMAX, NMAX)
  complex*16 :: WORK(WMAX)

  ! Packed buffers for printing (matches actual M*N or NB*TC dims).
  double precision :: Vpk_r(2*NMAX*NMAX), Tpk_r(2*NBMAX*TCOLS)
  double precision :: Qpk_r(2*NMAX*NMAX)
  complex*16       :: Vpk(NMAX*NMAX), Tpk(NBMAX*TCOLS)
  complex*16       :: Qpk(NMAX*NMAX)
  equivalence (Vpk, Vpk_r)
  equivalence (Tpk, Tpk_r)
  equivalence (Qpk, Qpk_r)

  integer :: INFO, M, N, MB, NB, NUMBLK, TC, LWRK

  ! ============================================================
  ! Test 1: Basic blocked TSQR with KK > 0 trailing block.
  ! M=12, N=3, MB=5, NB=2: M-N=9, MB-N=2 → NUMBLK=5.
  ! ============================================================
  M = 12; N = 3; MB = 5; NB = 2
  call build_panel(M, N, A)
  call ZLATSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, NB*N, INFO)
  V = A
  NUMBLK = ceiling( dble(M - N) / dble(MB - N) )
  TC = NUMBLK * N
  LWRK = (M + NB)*N
  call ZUNGTSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, LWRK, INFO)
  call emit('basic_blocked', M, N, MB, NB, V, A, T, TC, INFO)

  ! ============================================================
  ! Test 2: Square M=N case (just the first MB rows are factorized).
  ! M=8, N=4, MB=8, NB=2 (fall-through: only one input block).
  ! When M <= MB, ZLATSQR uses ZGEQRT directly on the panel.
  ! ============================================================
  M = 8; N = 4; MB = 8; NB = 2
  call build_panel(M, N, A)
  call ZLATSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, NB*N, INFO)
  V = A
  TC = N
  LWRK = (M + NB)*N
  call ZUNGTSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, LWRK, INFO)
  call emit('square_singleblock', M, N, MB, NB, V, A, T, TC, INFO)

  ! ============================================================
  ! Test 3: KK=0 case (M-N divisible by MB-N).
  ! M=10, N=2, MB=4: M-N=8, MB-N=2, mod=0.
  ! ============================================================
  M = 10; N = 2; MB = 4; NB = 2
  call build_panel(M, N, A)
  call ZLATSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, NB*N, INFO)
  V = A
  NUMBLK = ceiling( dble(M - N) / dble(MB - N) )
  TC = NUMBLK * N
  LWRK = (M + NB)*N
  call ZUNGTSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, LWRK, INFO)
  call emit('kk0_case', M, N, MB, NB, V, A, T, TC, INFO)

  ! ============================================================
  ! Test 4: Tall narrow M=20, N=2, MB=6, NB=1.
  ! ============================================================
  M = 20; N = 2; MB = 6; NB = 1
  call build_panel(M, N, A)
  call ZLATSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, NB*N, INFO)
  V = A
  NUMBLK = ceiling( dble(M - N) / dble(MB - N) )
  TC = NUMBLK * N
  LWRK = (M + NB)*N
  call ZUNGTSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, LWRK, INFO)
  call emit('tall_narrow_nb1', M, N, MB, NB, V, A, T, TC, INFO)

  ! ============================================================
  ! Test 5: N=1 column (degenerate).
  ! M=15, N=1, MB=5, NB=1.
  ! ============================================================
  M = 15; N = 1; MB = 5; NB = 1
  call build_panel(M, N, A)
  call ZLATSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, NB*N, INFO)
  V = A
  NUMBLK = ceiling( dble(M - N) / dble(MB - N) )
  TC = NUMBLK * N
  LWRK = (M + NB)*N
  call ZUNGTSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, LWRK, INFO)
  call emit('n_eq_1', M, N, MB, NB, V, A, T, TC, INFO)

  ! ============================================================
  ! Test 6: M=N+1, smallest non-trivial tall case with multiple input blocks.
  ! M=6, N=2, MB=3, NB=2: M-N=4, MB-N=1, NUMBLK=4.
  ! ============================================================
  M = 6; N = 2; MB = 3; NB = 2
  call build_panel(M, N, A)
  call ZLATSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, NB*N, INFO)
  V = A
  NUMBLK = ceiling( dble(M - N) / dble(MB - N) )
  TC = NUMBLK * N
  LWRK = (M + NB)*N
  call ZUNGTSQR(M, N, MB, NB, A, NMAX, T, NBMAX, WORK, LWRK, INFO)
  call emit('many_small_blocks', M, N, MB, NB, V, A, T, TC, INFO)

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

  ! Emit a test case: print M, N, MB, NB, the V factor (M-by-N), the T
  ! buffer (NB-by-TC), and the resulting Q (M-by-N) plus INFO.
  subroutine emit(name, mm, nn, mbb, nbb, Vin, Qin, Tin, tc, info)
    character(len=*), intent(in) :: name
    integer, intent(in) :: mm, nn, mbb, nbb, tc, info
    complex*16, intent(in) :: Vin(NMAX,*), Qin(NMAX,*), Tin(NBMAX,*)
    integer :: nblocal
    nblocal = min(nbb, nn)
    call begin_test(name)
    call print_int('M', mm)
    call print_int('N', nn)
    call print_int('MB', mbb)
    call print_int('NB', nbb)
    call pack_zmatrix(Vin, NMAX, mm, nn, Vpk)
    call print_array('V', Vpk_r, 2*mm*nn)
    call pack_zmatrix(Tin, NBMAX, nblocal, tc, Tpk)
    call print_array('T', Tpk_r, 2*nblocal*tc)
    call print_int('LDT', nblocal)
    call print_int('TCOLS', tc)
    call pack_zmatrix(Qin, NMAX, mm, nn, Qpk)
    call print_array('Q', Qpk_r, 2*mm*nn)
    call print_int('INFO', info)
    call end_test()
  end subroutine emit

end program
