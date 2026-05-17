program test_dorbdb2
  use test_utils
  implicit none

  integer, parameter :: NMAX = 16
  double precision :: X11(NMAX, NMAX), X21(NMAX, NMAX)
  double precision :: X11in(NMAX, NMAX), X21in(NMAX, NMAX)
  double precision :: THETA(NMAX), PHI(NMAX)
  double precision :: TAUP1(NMAX), TAUP2(NMAX), TAUQ1(NMAX)
  double precision :: WORK(NMAX*NMAX)
  ! Packed buffers for printing the contents of the X11 and X21 blocks at
  ! their actual P-by-Q (resp. (M-P)-by-Q) extents — avoids the NMAX-stride
  ! padding that EQUIVALENCE-style printing would pick up.
  double precision :: Apk(NMAX*NMAX)
  integer :: m, p, q, lwork, info, i

  ! ---------------------------------------------------------------------
  ! Test 1: M=10, P=3, Q=5 (so M-P=7, M-Q=5). Constraints satisfied:
  !   0 <= P <= min(M-P, Q, M-Q) = min(7, 5, 5) = 5.
  !   This exercises both the inner i<P branch (i=0,1) and the second
  !   loop (I = P+1, Q -> i=3,4).
  ! ---------------------------------------------------------------------
  m = 10
  p = 3
  q = 5
  lwork = NMAX * NMAX

  call zero_outputs(THETA, PHI, TAUP1, TAUP2, TAUQ1, NMAX)
  call init_orthonormal_columns(X11, X21, p, m - p, q, NMAX, NMAX, 1)
  call save_inputs(X11, X21, X11in, X21in, NMAX)

  call DORBDB2(m, p, q, X11, NMAX, X21, NMAX, THETA, PHI, &
               TAUP1, TAUP2, TAUQ1, WORK, lwork, info)

  call begin_test('m10_p3_q5')
  call print_int('info', info)
  call print_array('THETA', THETA, q)
  if ( q > 1 ) call print_array('PHI', PHI, q - 1)
  if ( p > 1 ) call print_array('TAUP1', TAUP1, p - 1)
  call print_array('TAUP2', TAUP2, m - p)
  call print_array('TAUQ1', TAUQ1, q)
  call pack_matrix(X11in, NMAX, p, q, Apk)
  call print_array('X11in', Apk, p * q)
  call pack_matrix(X21in, NMAX, m - p, q, Apk)
  call print_array('X21in', Apk, (m - p) * q)
  call pack_matrix(X11, NMAX, p, q, Apk)
  call print_array('X11', Apk, p * q)
  call pack_matrix(X21, NMAX, m - p, q, Apk)
  call print_array('X21', Apk, (m - p) * q)
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

  call DORBDB2(m, p, q, X11, NMAX, X21, NMAX, THETA, PHI, &
               TAUP1, TAUP2, TAUQ1, WORK, lwork, info)

  call begin_test('m12_p4_q6')
  call print_int('info', info)
  call print_array('THETA', THETA, q)
  call print_array('PHI', PHI, q - 1)
  call print_array('TAUP1', TAUP1, p - 1)
  call print_array('TAUP2', TAUP2, m - p)
  call print_array('TAUQ1', TAUQ1, q)
  call pack_matrix(X11in, NMAX, p, q, Apk)
  call print_array('X11in', Apk, p * q)
  call pack_matrix(X21in, NMAX, m - p, q, Apk)
  call print_array('X21in', Apk, (m - p) * q)
  call pack_matrix(X11, NMAX, p, q, Apk)
  call print_array('X11', Apk, p * q)
  call pack_matrix(X21, NMAX, m - p, q, Apk)
  call print_array('X21', Apk, (m - p) * q)
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

  call DORBDB2(m, p, q, X11, NMAX, X21, NMAX, THETA, PHI, &
               TAUP1, TAUP2, TAUQ1, WORK, lwork, info)

  call begin_test('p1_no_taup1')
  call print_int('info', info)
  call print_array('THETA', THETA, q)
  if ( q > 1 ) call print_array('PHI', PHI, q - 1)
  call print_array('TAUP2', TAUP2, m - p)
  call print_array('TAUQ1', TAUQ1, q)
  call pack_matrix(X11in, NMAX, p, q, Apk)
  call print_array('X11in', Apk, p * q)
  call pack_matrix(X21in, NMAX, m - p, q, Apk)
  call print_array('X21in', Apk, (m - p) * q)
  call pack_matrix(X11, NMAX, p, q, Apk)
  call print_array('X11', Apk, p * q)
  call pack_matrix(X21, NMAX, m - p, q, Apk)
  call print_array('X21', Apk, (m - p) * q)
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

  call DORBDB2(m, p, q, X11, NMAX, X21, NMAX, THETA, PHI, &
               TAUP1, TAUP2, TAUQ1, WORK, lwork, info)

  call begin_test('p0_only_second_loop')
  call print_int('info', info)
  call print_array('TAUP2', TAUP2, q)
  call pack_matrix(X21in, NMAX, m - p, q, Apk)
  call print_array('X21in', Apk, (m - p) * q)
  call pack_matrix(X21, NMAX, m - p, q, Apk)
  call print_array('X21', Apk, (m - p) * q)
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

  call DORBDB2(m, p, q, X11, NMAX, X21, NMAX, THETA, PHI, &
               TAUP1, TAUP2, TAUQ1, WORK, lwork, info)

  call begin_test('p_eq_q')
  call print_int('info', info)
  call print_array('THETA', THETA, q)
  if ( q > 1 ) call print_array('PHI', PHI, q - 1)
  if ( p > 1 ) call print_array('TAUP1', TAUP1, p - 1)
  call print_array('TAUP2', TAUP2, m - p)
  call print_array('TAUQ1', TAUQ1, q)
  call pack_matrix(X11in, NMAX, p, q, Apk)
  call print_array('X11in', Apk, p * q)
  call pack_matrix(X21in, NMAX, m - p, q, Apk)
  call print_array('X21in', Apk, (m - p) * q)
  call pack_matrix(X11, NMAX, p, q, Apk)
  call print_array('X11', Apk, p * q)
  call pack_matrix(X21, NMAX, m - p, q, Apk)
  call print_array('X21', Apk, (m - p) * q)
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
     TAUP1(i) = -777.0d0
     TAUP2(i) = -777.0d0
     TAUQ1(i) = -777.0d0
  end do

  call init_orthonormal_columns(X11, X21, max(1, p), m - p, max(1, q), NMAX, NMAX, 6)

  call DORBDB2(m, p, q, X11, NMAX, X21, NMAX, THETA, PHI, &
               TAUP1, TAUP2, TAUQ1, WORK, lwork, info)

  call begin_test('q0_quick_return')
  call print_int('info', info)
  call end_test()

contains

  ! Zero the THETA, PHI, TAUP1, TAUP2, TAUQ1 buffers between test cases.
  subroutine zero_outputs(THETA_, PHI_, TAUP1_, TAUP2_, TAUQ1_, sz)
    integer, intent(in) :: sz
    double precision, intent(out) :: THETA_(sz), PHI_(sz), TAUP1_(sz), TAUP2_(sz), TAUQ1_(sz)
    integer :: ii
    do ii = 1, sz
       THETA_(ii) = 0.0d0
       PHI_(ii)   = 0.0d0
       TAUP1_(ii) = 0.0d0
       TAUP2_(ii) = 0.0d0
       TAUQ1_(ii) = 0.0d0
    end do
  end subroutine

  ! Save copies of the input matrices so we can print both inputs and outputs.
  subroutine save_inputs(X11_, X21_, X11in_, X21in_, sz)
    integer, intent(in) :: sz
    double precision, intent(in) :: X11_(sz, sz), X21_(sz, sz)
    double precision, intent(out) :: X11in_(sz, sz), X21in_(sz, sz)
    integer :: ii, jj
    do jj = 1, sz
       do ii = 1, sz
          X11in_(ii, jj) = X11_(ii, jj)
          X21in_(ii, jj) = X21_(ii, jj)
       end do
    end do
  end subroutine

  ! Build the input matrices [X11; X21] with orthonormal columns.
  subroutine init_orthonormal_columns(X11_, X21_, p_, mp_, q_, ld11, ld21, seed)
    integer, intent(in) :: p_, mp_, q_, ld11, ld21, seed
    double precision, intent(out) :: X11_(ld11, *), X21_(ld21, *)
    double precision :: A(ld11 + ld21, q_)
    double precision :: dot
    integer :: ii, jj, kk, ll, mm, pass

    mm = p_ + mp_

    do jj = 1, q_
       do ii = 1, mm
          A(ii, jj) = sin( real(ii * 17 + jj * 31 + seed * 7, kind=8) )
       end do
    end do

    do pass = 1, 2
       do jj = 1, q_
          do ii = 1, jj - 1
             dot = 0.0d0
             do kk = 1, mm
                dot = dot + A(kk, ii) * A(kk, jj)
             end do
             do ll = 1, mm
                A(ll, jj) = A(ll, jj) - dot * A(ll, ii)
             end do
          end do
          dot = 0.0d0
          do kk = 1, mm
             dot = dot + A(kk, jj) * A(kk, jj)
          end do
          dot = sqrt(dot)
          do ll = 1, mm
             A(ll, jj) = A(ll, jj) / dot
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
    double precision, intent(in) :: A(ld, *)
    double precision, intent(out) :: B(prows * qcols)
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
