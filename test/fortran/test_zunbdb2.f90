program test_zunbdb2
  use test_utils
  implicit none

  integer, parameter :: NMAX = 16
  complex*16 :: X11(NMAX, NMAX), X21(NMAX, NMAX)
  complex*16 :: X11in(NMAX, NMAX), X21in(NMAX, NMAX)
  double precision :: THETA(NMAX), PHI(NMAX)
  complex*16 :: TAUP1(NMAX), TAUP2(NMAX), TAUQ1(NMAX)
  complex*16 :: WORK(NMAX*NMAX)
  ! Packed buffers for printing the contents of the X11 and X21 blocks at
  ! their actual P-by-Q (resp. (M-P)-by-Q) extents — avoids the NMAX-stride
  ! padding that EQUIVALENCE-style printing would pick up.
  complex*16 :: Apk(NMAX*NMAX)
  double precision :: Apk_r(2*NMAX*NMAX)
  equivalence (Apk, Apk_r)
  double precision :: TAUP1_r(2*NMAX), TAUP2_r(2*NMAX), TAUQ1_r(2*NMAX)
  equivalence (TAUP1, TAUP1_r)
  equivalence (TAUP2, TAUP2_r)
  equivalence (TAUQ1, TAUQ1_r)
  integer :: m, p, q, lwork, info, i
  complex*16, parameter :: CZERO = (0.0d0, 0.0d0)

  ! ---------------------------------------------------------------------
  ! Test 1: M=10, P=3, Q=5 (so M-P=7, M-Q=5). Constraints satisfied:
  !   0 <= P <= min(M-P, Q, M-Q) = min(7, 5, 5) = 5.
  !   Exercises both the inner i<P branch (i=0,1) and the second loop
  !   (I = P+1, Q  ->  i=3,4).
  ! ---------------------------------------------------------------------
  m = 10
  p = 3
  q = 5
  lwork = NMAX * NMAX

  call zero_outputs(THETA, PHI, TAUP1, TAUP2, TAUQ1, NMAX)
  call init_orthonormal_columns(X11, X21, p, m - p, q, NMAX, NMAX, 1)
  call save_inputs(X11, X21, X11in, X21in, NMAX)

  call ZUNBDB2(m, p, q, X11, NMAX, X21, NMAX, THETA, PHI, &
               TAUP1, TAUP2, TAUQ1, WORK, lwork, info)

  call begin_test('m10_p3_q5')
  call print_int('info', info)
  call print_array('THETA', THETA, q)
  if ( q > 1 ) call print_array('PHI', PHI, q - 1)
  if ( p > 1 ) call print_array('TAUP1', TAUP1_r, 2*(p - 1))
  call print_array('TAUP2', TAUP2_r, 2*(m - p))
  call print_array('TAUQ1', TAUQ1_r, 2*q)
  call pack_matrix(X11in, NMAX, p, q, Apk)
  call print_array('X11in', Apk_r, 2*p*q)
  call pack_matrix(X21in, NMAX, m - p, q, Apk)
  call print_array('X21in', Apk_r, 2*(m - p)*q)
  call pack_matrix(X11, NMAX, p, q, Apk)
  call print_array('X11', Apk_r, 2*p*q)
  call pack_matrix(X21, NMAX, m - p, q, Apk)
  call print_array('X21', Apk_r, 2*(m - p)*q)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 2: M=12, P=4, Q=6 — larger case for more loop coverage.
  ! ---------------------------------------------------------------------
  m = 12
  p = 4
  q = 6
  lwork = NMAX * NMAX

  call zero_outputs(THETA, PHI, TAUP1, TAUP2, TAUQ1, NMAX)
  call init_orthonormal_columns(X11, X21, p, m - p, q, NMAX, NMAX, 2)
  call save_inputs(X11, X21, X11in, X21in, NMAX)

  call ZUNBDB2(m, p, q, X11, NMAX, X21, NMAX, THETA, PHI, &
               TAUP1, TAUP2, TAUQ1, WORK, lwork, info)

  call begin_test('m12_p4_q6')
  call print_int('info', info)
  call print_array('THETA', THETA, q)
  call print_array('PHI', PHI, q - 1)
  call print_array('TAUP1', TAUP1_r, 2*(p - 1))
  call print_array('TAUP2', TAUP2_r, 2*(m - p))
  call print_array('TAUQ1', TAUQ1_r, 2*q)
  call pack_matrix(X11in, NMAX, p, q, Apk)
  call print_array('X11in', Apk_r, 2*p*q)
  call pack_matrix(X21in, NMAX, m - p, q, Apk)
  call print_array('X21in', Apk_r, 2*(m - p)*q)
  call pack_matrix(X11, NMAX, p, q, Apk)
  call print_array('X11', Apk_r, 2*p*q)
  call pack_matrix(X21, NMAX, m - p, q, Apk)
  call print_array('X21', Apk_r, 2*(m - p)*q)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 3: P=1 case — the inner I .LT. P branch is never taken (only one
  ! iteration of the first loop, and P-1 = 0).
  ! M=6, P=1, Q=2 (M-P=5, M-Q=4, all >= P=1).
  ! ---------------------------------------------------------------------
  m = 6
  p = 1
  q = 2
  lwork = NMAX * NMAX

  call zero_outputs(THETA, PHI, TAUP1, TAUP2, TAUQ1, NMAX)
  call init_orthonormal_columns(X11, X21, p, m - p, q, NMAX, NMAX, 3)
  call save_inputs(X11, X21, X11in, X21in, NMAX)

  call ZUNBDB2(m, p, q, X11, NMAX, X21, NMAX, THETA, PHI, &
               TAUP1, TAUP2, TAUQ1, WORK, lwork, info)

  call begin_test('p1_no_taup1')
  call print_int('info', info)
  call print_array('THETA', THETA, q)
  if ( q > 1 ) call print_array('PHI', PHI, q - 1)
  call print_array('TAUP2', TAUP2_r, 2*(m - p))
  call print_array('TAUQ1', TAUQ1_r, 2*q)
  call pack_matrix(X11in, NMAX, p, q, Apk)
  call print_array('X11in', Apk_r, 2*p*q)
  call pack_matrix(X21in, NMAX, m - p, q, Apk)
  call print_array('X21in', Apk_r, 2*(m - p)*q)
  call pack_matrix(X11, NMAX, p, q, Apk)
  call print_array('X11', Apk_r, 2*p*q)
  call pack_matrix(X21, NMAX, m - p, q, Apk)
  call print_array('X21', Apk_r, 2*(m - p)*q)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 4: P=0 — first loop never runs; only the second loop runs (over
  ! columns 1..Q of X21). M=6, P=0, Q=3 (M-P=6, M-Q=3, P<=min: 0<=3).
  ! ---------------------------------------------------------------------
  m = 6
  p = 0
  q = 3
  lwork = NMAX * NMAX

  call zero_outputs(THETA, PHI, TAUP1, TAUP2, TAUQ1, NMAX)
  call init_orthonormal_columns(X11, X21, max(1, p), m - p, q, NMAX, NMAX, 4)
  call save_inputs(X11, X21, X11in, X21in, NMAX)

  call ZUNBDB2(m, p, q, X11, NMAX, X21, NMAX, THETA, PHI, &
               TAUP1, TAUP2, TAUQ1, WORK, lwork, info)

  call begin_test('p0_only_second_loop')
  call print_int('info', info)
  call print_array('TAUP2', TAUP2_r, 2*q)
  call pack_matrix(X21in, NMAX, m - p, q, Apk)
  call print_array('X21in', Apk_r, 2*(m - p)*q)
  call pack_matrix(X21, NMAX, m - p, q, Apk)
  call print_array('X21', Apk_r, 2*(m - p)*q)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 5: P=Q (so the second loop runs zero times). Constraints:
  !   M=8, P=2, Q=2 (M-P=6, M-Q=6, P<=min(6,2,6)=2).
  ! ---------------------------------------------------------------------
  m = 8
  p = 2
  q = 2
  lwork = NMAX * NMAX

  call zero_outputs(THETA, PHI, TAUP1, TAUP2, TAUQ1, NMAX)
  call init_orthonormal_columns(X11, X21, p, m - p, q, NMAX, NMAX, 5)
  call save_inputs(X11, X21, X11in, X21in, NMAX)

  call ZUNBDB2(m, p, q, X11, NMAX, X21, NMAX, THETA, PHI, &
               TAUP1, TAUP2, TAUQ1, WORK, lwork, info)

  call begin_test('p_eq_q')
  call print_int('info', info)
  call print_array('THETA', THETA, q)
  if ( q > 1 ) call print_array('PHI', PHI, q - 1)
  if ( p > 1 ) call print_array('TAUP1', TAUP1_r, 2*(p - 1))
  call print_array('TAUP2', TAUP2_r, 2*(m - p))
  call print_array('TAUQ1', TAUQ1_r, 2*q)
  call pack_matrix(X11in, NMAX, p, q, Apk)
  call print_array('X11in', Apk_r, 2*p*q)
  call pack_matrix(X21in, NMAX, m - p, q, Apk)
  call print_array('X21in', Apk_r, 2*(m - p)*q)
  call pack_matrix(X11, NMAX, p, q, Apk)
  call print_array('X11', Apk_r, 2*p*q)
  call pack_matrix(X21, NMAX, m - p, q, Apk)
  call print_array('X21', Apk_r, 2*(m - p)*q)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 6: Q=0 — outer loops never execute. Quick-return.
  ! ---------------------------------------------------------------------
  m = 4
  p = 0
  q = 0
  lwork = NMAX * NMAX

  do i = 1, NMAX
     THETA(i) = -777.0d0
     PHI(i)   = -777.0d0
     TAUP1(i) = CZERO
     TAUP2(i) = CZERO
     TAUQ1(i) = CZERO
  end do

  call init_orthonormal_columns(X11, X21, max(1, p), m - p, max(1, q), NMAX, NMAX, 6)

  call ZUNBDB2(m, p, q, X11, NMAX, X21, NMAX, THETA, PHI, &
               TAUP1, TAUP2, TAUQ1, WORK, lwork, info)

  call begin_test('q0_quick_return')
  call print_int('info', info)
  call end_test()

contains

  ! Zero the THETA, PHI, TAUP1, TAUP2, TAUQ1 buffers between test cases.
  subroutine zero_outputs(THETA_, PHI_, TAUP1_, TAUP2_, TAUQ1_, sz)
    integer, intent(in) :: sz
    double precision, intent(out) :: THETA_(sz), PHI_(sz)
    complex*16, intent(out) :: TAUP1_(sz), TAUP2_(sz), TAUQ1_(sz)
    complex*16, parameter :: CZ = (0.0d0, 0.0d0)
    integer :: ii
    do ii = 1, sz
       THETA_(ii) = 0.0d0
       PHI_(ii)   = 0.0d0
       TAUP1_(ii) = CZ
       TAUP2_(ii) = CZ
       TAUQ1_(ii) = CZ
    end do
  end subroutine

  ! Save copies of the input matrices so we can print both inputs and outputs.
  subroutine save_inputs(X11_, X21_, X11in_, X21in_, sz)
    integer, intent(in) :: sz
    complex*16, intent(in) :: X11_(sz, sz), X21_(sz, sz)
    complex*16, intent(out) :: X11in_(sz, sz), X21in_(sz, sz)
    integer :: ii, jj
    do jj = 1, sz
       do ii = 1, sz
          X11in_(ii, jj) = X11_(ii, jj)
          X21in_(ii, jj) = X21_(ii, jj)
       end do
    end do
  end subroutine

  ! Build the input matrices [X11; X21] with orthonormal columns (complex).
  ! Generates a complex matrix from sin/cos of (i*17 + j*31 + seed*7), then
  ! Gram-Schmidt-twice to get orthonormal columns.
  subroutine init_orthonormal_columns(X11_, X21_, p_, mp_, q_, ld11, ld21, seed)
    integer, intent(in) :: p_, mp_, q_, ld11, ld21, seed
    complex*16, intent(out) :: X11_(ld11, *), X21_(ld21, *)
    complex*16 :: A(ld11 + ld21, q_)
    complex*16 :: dot
    double precision :: nrm, arg
    integer :: ii, jj, kk, ll, mm, pass

    mm = p_ + mp_

    do jj = 1, q_
       do ii = 1, mm
          arg = real(ii * 17 + jj * 31 + seed * 7, kind=8)
          A(ii, jj) = cmplx(sin(arg), cos(arg * 1.31d0), kind=8)
       end do
    end do

    do pass = 1, 2
       do jj = 1, q_
          do ii = 1, jj - 1
             dot = (0.0d0, 0.0d0)
             do kk = 1, mm
                dot = dot + conjg(A(kk, ii)) * A(kk, jj)
             end do
             do ll = 1, mm
                A(ll, jj) = A(ll, jj) - dot * A(ll, ii)
             end do
          end do
          nrm = 0.0d0
          do kk = 1, mm
             nrm = nrm + real(A(kk, jj))**2 + aimag(A(kk, jj))**2
          end do
          nrm = sqrt(nrm)
          do ll = 1, mm
             A(ll, jj) = A(ll, jj) / nrm
          end do
       end do
    end do

    do jj = 1, q_
       do ii = 1, p_
          X11_(ii, jj) = A(ii, jj)
       end do
       do ii = 1, mp_
          X21_(ii, jj) = A(p_ + ii, jj)
       end do
    end do
  end subroutine

  ! Pack the leading p-by-q submatrix of A (with leading dim ld) into
  ! a contiguous column-major buffer for safe printing.
  subroutine pack_matrix(A, ld, prows, qcols, B)
    integer, intent(in) :: ld, prows, qcols
    complex*16, intent(in) :: A(ld, *)
    complex*16, intent(out) :: B(prows * qcols)
    integer :: ii, jj, idx

    idx = 0
    do jj = 1, qcols
       do ii = 1, prows
          idx = idx + 1
          B(idx) = A(ii, jj)
       end do
    end do
  end subroutine

end program
