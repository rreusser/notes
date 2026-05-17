program test_dorbdb3
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
  ! Test 1: M=8, P=5, Q=4. M-P=3 (so 1st loop runs 3 times, 2nd loop
  ! runs once for I=4=Q). Constraints: M-P=3 <= min(P=5,Q=4,M-Q=4) ✓.
  ! Exercises: i>1 DROT branch, i<M-P PHI/TAUP2 branch, both loops.
  ! ---------------------------------------------------------------------
  m = 8
  p = 5
  q = 4
  lwork = NMAX*NMAX

  call zero_outputs(THETA, PHI, TAUP1, TAUP2, TAUQ1, NMAX)
  call init_orthonormal_columns(X11, X21, p, m - p, q, NMAX, NMAX, 1)
  call save_inputs(X11, X21, X11in, X21in, NMAX)

  call DORBDB3(m, p, q, X11, NMAX, X21, NMAX, THETA, PHI, &
               TAUP1, TAUP2, TAUQ1, WORK, lwork, info)

  call begin_test('basic_8x5x4')
  call print_int('info', info)
  call print_array('THETA', THETA, q)
  call print_array('PHI', PHI, m - p - 1)
  call print_array('TAUP1', TAUP1, p)
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
  ! Test 2: M=10, P=6, Q=5. M-P=4. Two iterations of second loop.
  ! ---------------------------------------------------------------------
  m = 10
  p = 6
  q = 5
  lwork = NMAX*NMAX

  call zero_outputs(THETA, PHI, TAUP1, TAUP2, TAUQ1, NMAX)
  call init_orthonormal_columns(X11, X21, p, m - p, q, NMAX, NMAX, 2)
  call save_inputs(X11, X21, X11in, X21in, NMAX)

  call DORBDB3(m, p, q, X11, NMAX, X21, NMAX, THETA, PHI, &
               TAUP1, TAUP2, TAUQ1, WORK, lwork, info)

  call begin_test('m10_p6_q5')
  call print_int('info', info)
  call print_array('THETA', THETA, q)
  call print_array('PHI', PHI, m - p - 1)
  call print_array('TAUP1', TAUP1, p)
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
  ! Test 3: Q=M-P (second loop empty). M=8, P=5, Q=3. M-P=3.
  ! Constraints: 3 <= min(5,3,5) ✓. Only first loop runs.
  ! ---------------------------------------------------------------------
  m = 8
  p = 5
  q = 3
  lwork = NMAX*NMAX

  call zero_outputs(THETA, PHI, TAUP1, TAUP2, TAUQ1, NMAX)
  call init_orthonormal_columns(X11, X21, p, m - p, q, NMAX, NMAX, 3)
  call save_inputs(X11, X21, X11in, X21in, NMAX)

  call DORBDB3(m, p, q, X11, NMAX, X21, NMAX, THETA, PHI, &
               TAUP1, TAUP2, TAUQ1, WORK, lwork, info)

  call begin_test('q_eq_mmp')
  call print_int('info', info)
  call print_array('THETA', THETA, q)
  call print_array('PHI', PHI, m - p - 1)
  call print_array('TAUP1', TAUP1, p)
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
  ! Test 4: M-P=1 (single row of X21 — i<M-P branch never taken; PHI/TAUP2
  ! never get a non-trivial entry). M=4, P=3, Q=2. Constraints: 1 <= min(3,2,2) ✓.
  ! ---------------------------------------------------------------------
  m = 4
  p = 3
  q = 2
  lwork = NMAX*NMAX

  call zero_outputs(THETA, PHI, TAUP1, TAUP2, TAUQ1, NMAX)
  call init_orthonormal_columns(X11, X21, p, m - p, q, NMAX, NMAX, 4)
  call save_inputs(X11, X21, X11in, X21in, NMAX)

  call DORBDB3(m, p, q, X11, NMAX, X21, NMAX, THETA, PHI, &
               TAUP1, TAUP2, TAUQ1, WORK, lwork, info)

  call begin_test('mp_eq_1')
  call print_int('info', info)
  call print_array('THETA', THETA, q)
  call print_array('TAUP1', TAUP1, p)
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
  ! Test 5: Q=0 quick return. Loop body never runs.
  ! Constraints require Q >= M-P, so use M=P=2, Q=0 (M-P=0 too).
  ! ---------------------------------------------------------------------
  m = 2
  p = 2
  q = 0
  lwork = NMAX*NMAX

  do i = 1, NMAX
     THETA(i) = -777.0d0
     PHI(i)   = -777.0d0
     TAUP1(i) = -777.0d0
     TAUP2(i) = -777.0d0
     TAUQ1(i) = -777.0d0
  end do

  call init_orthonormal_columns(X11, X21, p, m - p, max(1, q), NMAX, NMAX, 5)

  call DORBDB3(m, p, q, X11, NMAX, X21, NMAX, THETA, PHI, &
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
