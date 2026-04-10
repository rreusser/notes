program test_ztprfb
  use test_utils
  implicit none

  ! Tests ztprfb across all 8 combinations of (side, direct, storev) with trans='N' or 'C'.
  ! We keep dimensions small; the test values only need to be reproducible, not meaningful.

  complex*16 :: V(64), T(16), A(16), B(64), work(64)
  double precision :: V_real(128), T_real(32), A_real(32), B_real(128), work_real(128)
  equivalence (V, V_real)
  equivalence (T, T_real)
  equivalence (A, A_real)
  equivalence (B, B_real)
  equivalence (work, work_real)
  integer :: i

  ! Fill V, T, A, B with deterministic values.
  call fill_all()

  ! Dimensions:
  !   M = 5, N = 4, K = 3, L = 2
  !   SIDE='L': A is K-by-N = 3x4, B is M-by-N = 5x4
  !   SIDE='R': A is M-by-K = 5x3, B is M-by-N = 5x4
  !
  ! For STOREV='C' (columnwise) and SIDE='L':   V is M-by-K = 5x3, LDV=5
  ! For STOREV='C' (columnwise) and SIDE='R':   V is N-by-K = 4x3, LDV=4
  ! For STOREV='R' (rowwise)    and SIDE='L':   V is K-by-M = 3x5, LDV=3
  ! For STOREV='R' (rowwise)    and SIDE='R':   V is K-by-N = 3x4, LDV=3

  ! --- Case 1: COLUMN FORWARD LEFT, trans='N' ---
  call fill_all()
  call ztprfb('L', 'N', 'F', 'C', 5, 4, 3, 2, V, 5, T, 3, A, 3, B, 5, work, 3)
  call begin_test('ztprfb_col_fwd_left_N')
  call print_array('A', A_real, 24)
  call print_array('B', B_real, 40)
  call end_test()

  ! --- Case 2: COLUMN FORWARD LEFT, trans='C' ---
  call fill_all()
  call ztprfb('L', 'C', 'F', 'C', 5, 4, 3, 2, V, 5, T, 3, A, 3, B, 5, work, 3)
  call begin_test('ztprfb_col_fwd_left_C')
  call print_array('A', A_real, 24)
  call print_array('B', B_real, 40)
  call end_test()

  ! --- Case 3: COLUMN FORWARD RIGHT, trans='N' ---
  ! M=5,N=4,K=3,L=2 ; A is 5x3, B is 5x4 ; V is 4x3 (LDV=4)
  call fill_all()
  call ztprfb('R', 'N', 'F', 'C', 5, 4, 3, 2, V, 4, T, 3, A, 5, B, 5, work, 5)
  call begin_test('ztprfb_col_fwd_right_N')
  call print_array('A', A_real, 30)
  call print_array('B', B_real, 40)
  call end_test()

  ! --- Case 4: COLUMN BACKWARD LEFT, trans='C' ---
  call fill_all()
  call ztprfb('L', 'C', 'B', 'C', 5, 4, 3, 2, V, 5, T, 3, A, 3, B, 5, work, 3)
  call begin_test('ztprfb_col_bwd_left_C')
  call print_array('A', A_real, 24)
  call print_array('B', B_real, 40)
  call end_test()

  ! --- Case 5: COLUMN BACKWARD RIGHT, trans='N' ---
  call fill_all()
  call ztprfb('R', 'N', 'B', 'C', 5, 4, 3, 2, V, 4, T, 3, A, 5, B, 5, work, 5)
  call begin_test('ztprfb_col_bwd_right_N')
  call print_array('A', A_real, 30)
  call print_array('B', B_real, 40)
  call end_test()

  ! --- Case 6: ROW FORWARD LEFT, trans='N' ---
  ! V is 3x5 (LDV=3)
  ! Note: reference ZTPRFB has a quirk where the first ZTRMM in this branch
  ! uses LDB as the leading dimension of WORK. We match by using LDWORK=LDB=5.
  call fill_all()
  call ztprfb('L', 'N', 'F', 'R', 5, 4, 3, 2, V, 3, T, 3, A, 3, B, 5, work, 5)
  call begin_test('ztprfb_row_fwd_left_N')
  call print_array('A', A_real, 24)
  call print_array('B', B_real, 40)
  call end_test()

  ! --- Case 7: ROW FORWARD RIGHT, trans='C' ---
  ! V is 3x4 (LDV=3)
  call fill_all()
  call ztprfb('R', 'C', 'F', 'R', 5, 4, 3, 2, V, 3, T, 3, A, 5, B, 5, work, 5)
  call begin_test('ztprfb_row_fwd_right_C')
  call print_array('A', A_real, 30)
  call print_array('B', B_real, 40)
  call end_test()

  ! --- Case 8: ROW BACKWARD LEFT, trans='N' ---
  call fill_all()
  call ztprfb('L', 'N', 'B', 'R', 5, 4, 3, 2, V, 3, T, 3, A, 3, B, 5, work, 3)
  call begin_test('ztprfb_row_bwd_left_N')
  call print_array('A', A_real, 24)
  call print_array('B', B_real, 40)
  call end_test()

  ! --- Case 9: ROW BACKWARD RIGHT, trans='C' ---
  call fill_all()
  call ztprfb('R', 'C', 'B', 'R', 5, 4, 3, 2, V, 3, T, 3, A, 5, B, 5, work, 5)
  call begin_test('ztprfb_row_bwd_right_C')
  call print_array('A', A_real, 30)
  call print_array('B', B_real, 40)
  call end_test()

  ! --- Case 10: quick return M=0 ---
  call fill_all()
  call ztprfb('L', 'N', 'F', 'C', 0, 4, 3, 2, V, 5, T, 3, A, 3, B, 5, work, 3)
  call begin_test('ztprfb_quick_return')
  call print_array('A', A_real, 24)
  call end_test()

contains

  subroutine fill_all()
    integer :: k
    do k = 1, 64
      V(k) = cmplx(0.1d0*k, -0.05d0*k, kind=8)
    end do
    do k = 1, 16
      T(k) = cmplx(0.0d0, 0.0d0, kind=8)
    end do
    ! Triangular T (3x3, LDT=3). For DIRECT='F' it's upper; for 'B' it's lower.
    ! We fill both upper and lower with small values; ztprfb only touches its own.
    T(1) = cmplx(1.1d0, 0.1d0, kind=8)
    T(4) = cmplx(0.3d0, -0.2d0, kind=8)
    T(5) = cmplx(1.2d0, -0.1d0, kind=8)
    T(7) = cmplx(0.2d0, 0.4d0, kind=8)
    T(8) = cmplx(-0.1d0, 0.3d0, kind=8)
    T(9) = cmplx(1.3d0, 0.05d0, kind=8)
    ! Lower triangle of T (for backward direct).
    T(2) = cmplx(0.25d0, -0.15d0, kind=8)
    T(3) = cmplx(-0.35d0, 0.1d0, kind=8)
    T(6) = cmplx(0.45d0, 0.2d0, kind=8)
    do k = 1, 16
      A(k) = cmplx(0.5d0 + 0.1d0*k, 0.2d0 - 0.03d0*k, kind=8)
    end do
    do k = 1, 64
      B(k) = cmplx(-0.3d0 + 0.07d0*k, 0.15d0 + 0.02d0*k, kind=8)
    end do
    do k = 1, 64
      work(k) = cmplx(0.0d0, 0.0d0, kind=8)
    end do
  end subroutine fill_all

end program test_ztprfb
