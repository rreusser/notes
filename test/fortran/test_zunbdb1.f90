program test_zunbdb1
  use test_utils
  implicit none

  integer, parameter :: NMAX = 16
  complex*16 :: X11(NMAX, NMAX), X21(NMAX, NMAX)
  complex*16 :: X11in(NMAX, NMAX), X21in(NMAX, NMAX)
  double precision :: THETA(NMAX), PHI(NMAX)
  complex*16 :: TAUP1(NMAX), TAUP2(NMAX), TAUQ1(NMAX)
  complex*16 :: WORK(NMAX*NMAX)
  ! Packed buffers (complex). Real view via EQUIVALENCE.
  complex*16 :: Apk(NMAX*NMAX)
  double precision :: Apk_r(2*NMAX*NMAX)
  double precision :: TAUP1_r(2*NMAX), TAUP2_r(2*NMAX), TAUQ1_r(2*NMAX)
  equivalence (Apk, Apk_r)
  equivalence (TAUP1, TAUP1_r)
  equivalence (TAUP2, TAUP2_r)
  equivalence (TAUQ1, TAUQ1_r)
  integer :: m, p, q, lwork, info, i

  ! ---------------------------------------------------------------------
  ! Test 1: M=8, P=4, Q=2 (so M-P=4). All dimension constraints satisfied:
  !   Q <= min(P, M-P, M-Q) = min(4, 4, 6) = 4. Real-valued input.
  ! ---------------------------------------------------------------------
  m = 8
  p = 4
  q = 2
  lwork = m - q

  call zero_outputs(THETA, PHI, TAUP1, TAUP2, TAUQ1, NMAX)
  call init_orthonormal_columns(X11, X21, p, m - p, q, NMAX, NMAX, 1)
  call save_inputs(X11, X21, X11in, X21in, NMAX)

  call ZUNBDB1(m, p, q, X11, NMAX, X21, NMAX, THETA, PHI, &
               TAUP1, TAUP2, TAUQ1, WORK, lwork, info)

  call begin_test('basic_8x4x2')
  call print_int('info', info)
  call print_array('THETA', THETA, q)
  if ( q > 1 ) call print_array('PHI', PHI, q - 1)
  call pack_z(TAUP1, NMAX, p, 1, Apk)
  call print_array('TAUP1', Apk_r, 2 * p)
  call pack_z(TAUP2, NMAX, m - p, 1, Apk)
  call print_array('TAUP2', Apk_r, 2 * (m - p))
  call pack_z(TAUQ1, NMAX, q, 1, Apk)
  call print_array('TAUQ1', Apk_r, 2 * q)
  call pack_matrix_z(X11in, NMAX, p, q, Apk)
  call print_array('X11in', Apk_r, 2 * p * q)
  call pack_matrix_z(X21in, NMAX, m - p, q, Apk)
  call print_array('X21in', Apk_r, 2 * (m - p) * q)
  call pack_matrix_z(X11, NMAX, p, q, Apk)
  call print_array('X11', Apk_r, 2 * p * q)
  call pack_matrix_z(X21, NMAX, m - p, q, Apk)
  call print_array('X21', Apk_r, 2 * (m - p) * q)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 2: M=10, P=5, Q=3 with complex input.
  ! ---------------------------------------------------------------------
  m = 10
  p = 5
  q = 3
  lwork = m - q

  call zero_outputs(THETA, PHI, TAUP1, TAUP2, TAUQ1, NMAX)
  call init_complex_orthonormal_columns(X11, X21, p, m - p, q, NMAX, NMAX, 2)
  call save_inputs(X11, X21, X11in, X21in, NMAX)

  call ZUNBDB1(m, p, q, X11, NMAX, X21, NMAX, THETA, PHI, &
               TAUP1, TAUP2, TAUQ1, WORK, lwork, info)

  call begin_test('m10_p5_q3')
  call print_int('info', info)
  call print_array('THETA', THETA, q)
  call print_array('PHI', PHI, q - 1)
  call pack_z(TAUP1, NMAX, p, 1, Apk)
  call print_array('TAUP1', Apk_r, 2 * p)
  call pack_z(TAUP2, NMAX, m - p, 1, Apk)
  call print_array('TAUP2', Apk_r, 2 * (m - p))
  call pack_z(TAUQ1, NMAX, q, 1, Apk)
  call print_array('TAUQ1', Apk_r, 2 * q)
  call pack_matrix_z(X11in, NMAX, p, q, Apk)
  call print_array('X11in', Apk_r, 2 * p * q)
  call pack_matrix_z(X21in, NMAX, m - p, q, Apk)
  call print_array('X21in', Apk_r, 2 * (m - p) * q)
  call pack_matrix_z(X11, NMAX, p, q, Apk)
  call print_array('X11', Apk_r, 2 * p * q)
  call pack_matrix_z(X21, NMAX, m - p, q, Apk)
  call print_array('X21', Apk_r, 2 * (m - p) * q)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 3: Q=1 (single-column case — the i < q-1 branch is never taken).
  ! ---------------------------------------------------------------------
  m = 6
  p = 3
  q = 1
  lwork = m - q

  call zero_outputs(THETA, PHI, TAUP1, TAUP2, TAUQ1, NMAX)
  call init_complex_orthonormal_columns(X11, X21, p, m - p, q, NMAX, NMAX, 3)
  call save_inputs(X11, X21, X11in, X21in, NMAX)

  call ZUNBDB1(m, p, q, X11, NMAX, X21, NMAX, THETA, PHI, &
               TAUP1, TAUP2, TAUQ1, WORK, lwork, info)

  call begin_test('q1_no_phi')
  call print_int('info', info)
  call print_array('THETA', THETA, q)
  call pack_z(TAUP1, NMAX, p, 1, Apk)
  call print_array('TAUP1', Apk_r, 2 * p)
  call pack_z(TAUP2, NMAX, m - p, 1, Apk)
  call print_array('TAUP2', Apk_r, 2 * (m - p))
  call pack_z(TAUQ1, NMAX, q, 1, Apk)
  call print_array('TAUQ1', Apk_r, 2 * q)
  call pack_matrix_z(X11in, NMAX, p, q, Apk)
  call print_array('X11in', Apk_r, 2 * p * q)
  call pack_matrix_z(X21in, NMAX, m - p, q, Apk)
  call print_array('X21in', Apk_r, 2 * (m - p) * q)
  call pack_matrix_z(X11, NMAX, p, q, Apk)
  call print_array('X11', Apk_r, 2 * p * q)
  call pack_matrix_z(X21, NMAX, m - p, q, Apk)
  call print_array('X21', Apk_r, 2 * (m - p) * q)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 4: Q=0 — quick return (loop body never runs).
  ! ---------------------------------------------------------------------
  m = 4
  p = 2
  q = 0
  lwork = max(1, m - q)

  do i = 1, NMAX
     THETA(i) = -777.0d0
     PHI(i)   = -777.0d0
     TAUP1(i) = (-777.0d0, 0.0d0)
     TAUP2(i) = (-777.0d0, 0.0d0)
     TAUQ1(i) = (-777.0d0, 0.0d0)
  end do

  call init_complex_orthonormal_columns(X11, X21, p, m - p, max(1, q), NMAX, NMAX, 4)

  call ZUNBDB1(m, p, q, X11, NMAX, X21, NMAX, THETA, PHI, &
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
    integer :: ii
    do ii = 1, sz
       THETA_(ii) = 0.0d0
       PHI_(ii)   = 0.0d0
       TAUP1_(ii) = (0.0d0, 0.0d0)
       TAUP2_(ii) = (0.0d0, 0.0d0)
       TAUQ1_(ii) = (0.0d0, 0.0d0)
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

  ! Build the input matrices [X11; X21] with real orthonormal columns
  ! (stored as complex with imaginary part = 0).
  subroutine init_orthonormal_columns(X11_, X21_, p_, mp_, q_, ld11, ld21, seed)
    integer, intent(in) :: p_, mp_, q_, ld11, ld21, seed
    complex*16, intent(out) :: X11_(ld11, *), X21_(ld21, *)
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
          X11_(ii, jj) = cmplx( A(ii, jj), 0.0d0, kind=8 )
       end do
       do ii = 1, mp_
          X21_(ii, jj) = cmplx( A(p_ + ii, jj), 0.0d0, kind=8 )
       end do
    end do
  end subroutine

  ! Build the input matrices [X11; X21] with complex orthonormal columns.
  ! Generate (re, im) deterministically from sin, then Gram-Schmidt
  ! (with conjugate inner product) to enforce orthonormality.
  subroutine init_complex_orthonormal_columns(X11_, X21_, p_, mp_, q_, ld11, ld21, seed)
    integer, intent(in) :: p_, mp_, q_, ld11, ld21, seed
    complex*16, intent(out) :: X11_(ld11, *), X21_(ld21, *)
    complex*16 :: A(ld11 + ld21, q_)
    complex*16 :: dot
    double precision :: nrm
    integer :: ii, jj, kk, ll, mm, pass

    mm = p_ + mp_

    do jj = 1, q_
       do ii = 1, mm
          A(ii, jj) = cmplx( &
              sin( real(ii * 17 + jj * 31 + seed * 7, kind=8) ), &
              sin( real(ii * 13 + jj * 23 + seed * 11 + 5, kind=8) ), &
              kind=8 )
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
             nrm = nrm + dble( conjg(A(kk, jj)) * A(kk, jj) )
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
  ! a contiguous column-major complex buffer for safe printing via
  ! the Apk_r EQUIVALENCE.
  subroutine pack_matrix_z(A, ld, prows, qcols, B)
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

  ! Pack a strided 1D complex array (stride in elements, length n)
  ! into a contiguous buffer for printing.
  subroutine pack_z(A, ld, n, stride, B)
    integer, intent(in) :: ld, n, stride
    complex*16, intent(in) :: A(*)
    complex*16, intent(out) :: B(n)
    integer :: ii
    do ii = 1, n
       B(ii) = A(1 + (ii - 1) * stride)
    end do
    if ( .false. ) print *, ld  ! silence unused warning
  end subroutine

end program
