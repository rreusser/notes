program test_zlar1v
  use test_utils
  implicit none

  integer, parameter :: NMAX = 10
  integer :: N, B1, BN, R, NEGCNT, ISUPPZ(2), I
  double precision :: LAMBDA, PIVMIN, GAPTOL, ZTZ, MINGMA, NRMINV, RESID, RQCORR
  double precision :: D(NMAX), L(NMAX), LD(NMAX), LLD(NMAX)
  complex*16 :: Z(NMAX)
  double precision :: Z_r(2*NMAX)
  equivalence (Z, Z_r)
  double precision :: WORK(4*NMAX)
  double precision :: TD(NMAX), TE(NMAX)
  logical :: WANTNC
  double precision :: DLAMCH
  external :: ZLAR1V, DLAMCH

  ! ============================================================
  ! Test 1: 5x5 tridiag, T = tridiag(1, 4, 1), lambda near eigenvalue
  ! ============================================================
  N = 5
  TD(1) = 4.0d0
  TD(2) = 4.0d0
  TD(3) = 4.0d0
  TD(4) = 4.0d0
  TD(5) = 4.0d0
  TE(1) = 1.0d0
  TE(2) = 1.0d0
  TE(3) = 1.0d0
  TE(4) = 1.0d0
  call build_ldl(N, TD, TE, D, L, LD, LLD)

  B1 = 1
  BN = 5
  LAMBDA = 2.2679491924311229d0
  PIVMIN = DLAMCH('S')
  GAPTOL = 0.0d0
  WANTNC = .true.
  R = 0
  do I = 1, N
    Z(I) = (0.0d0, 0.0d0)
  end do
  do I = 1, 4*N
    WORK(I) = 0.0d0
  end do

  call ZLAR1V(N, B1, BN, LAMBDA, D, L, LD, LLD, PIVMIN, GAPTOL, &
              Z, WANTNC, NEGCNT, ZTZ, MINGMA, R, ISUPPZ, NRMINV, &
              RESID, RQCORR, WORK)

  call begin_test('tridiag5_smallest')
  call print_array('z', Z_r, 2*N)
  call print_scalar('ztz', ZTZ)
  call print_scalar('mingma', MINGMA)
  call print_int('r', R)
  call print_int('negcnt', NEGCNT)
  call print_int('isuppz1', ISUPPZ(1))
  call print_int('isuppz2', ISUPPZ(2))
  call end_test()

  ! ============================================================
  ! Test 2: tridiag3, T diag=[2,3,2] off=[1,1], eigenvalues 1,2,4
  ! ============================================================
  N = 3
  TD(1) = 2.0d0
  TD(2) = 3.0d0
  TD(3) = 2.0d0
  TE(1) = 1.0d0
  TE(2) = 1.0d0
  call build_ldl(N, TD, TE, D, L, LD, LLD)

  B1 = 1
  BN = 3
  LAMBDA = 4.0d0
  PIVMIN = DLAMCH('S')
  GAPTOL = 0.0d0
  WANTNC = .true.
  R = 0
  do I = 1, N
    Z(I) = (0.0d0, 0.0d0)
  end do

  call ZLAR1V(N, B1, BN, LAMBDA, D, L, LD, LLD, PIVMIN, GAPTOL, &
              Z, WANTNC, NEGCNT, ZTZ, MINGMA, R, ISUPPZ, NRMINV, &
              RESID, RQCORR, WORK)

  call begin_test('tridiag3_largest')
  call print_array('z', Z_r, 2*N)
  call print_scalar('ztz', ZTZ)
  call print_int('r', R)
  call print_int('negcnt', NEGCNT)
  call print_int('isuppz1', ISUPPZ(1))
  call print_int('isuppz2', ISUPPZ(2))
  call end_test()

  ! ============================================================
  ! Test 3: preset twist r=3 with middle lambda
  ! ============================================================
  N = 5
  TD(1) = 4.0d0
  TD(2) = 4.0d0
  TD(3) = 4.0d0
  TD(4) = 4.0d0
  TD(5) = 4.0d0
  TE(1) = 1.0d0
  TE(2) = 1.0d0
  TE(3) = 1.0d0
  TE(4) = 1.0d0
  call build_ldl(N, TD, TE, D, L, LD, LLD)
  B1 = 1
  BN = 5
  LAMBDA = 4.0d0
  PIVMIN = DLAMCH('S')
  GAPTOL = 0.0d0
  WANTNC = .true.
  R = 3
  do I = 1, N
    Z(I) = (0.0d0, 0.0d0)
  end do

  call ZLAR1V(N, B1, BN, LAMBDA, D, L, LD, LLD, PIVMIN, GAPTOL, &
              Z, WANTNC, NEGCNT, ZTZ, MINGMA, R, ISUPPZ, NRMINV, &
              RESID, RQCORR, WORK)

  call begin_test('tridiag5_twist3')
  call print_array('z', Z_r, 2*N)
  call print_scalar('ztz', ZTZ)
  call print_int('r', R)
  call end_test()

contains

  subroutine build_ldl(n, td, te, d, l, ld, lld)
    integer, intent(in) :: n
    double precision, intent(in) :: td(*), te(*)
    double precision, intent(out) :: d(*), l(*), ld(*), lld(*)
    integer :: k
    d(1) = td(1)
    do k = 1, n - 1
      l(k) = te(k) / d(k)
      d(k+1) = td(k+1) - l(k) * te(k)
      ld(k) = l(k) * d(k)
      lld(k) = l(k) * l(k) * d(k)
    end do
    l(n) = 0.0d0
    ld(n) = 0.0d0
    lld(n) = 0.0d0
  end subroutine build_ldl

end program
