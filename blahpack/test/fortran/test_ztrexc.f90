program test_ztrexc
  use test_utils
  implicit none

  integer, parameter :: MAXN = 4
  complex*16 :: T(MAXN, MAXN), Q(MAXN, MAXN)
  double precision :: T_r(2*MAXN*MAXN), Q_r(2*MAXN*MAXN)
  equivalence (T, T_r)
  equivalence (Q, Q_r)
  integer :: INFO, N, I

  ! ============================================================
  ! Test 1: Move eigenvalue from position 3 to position 1 in 4x4, COMPQ='V'
  ! T = upper triangular with distinct eigenvalues on diagonal
  ! ============================================================
  N = 4
  T = (0.0d0, 0.0d0); Q = (0.0d0, 0.0d0)
  T(1,1) = (1.0d0, 0.5d0); T(1,2) = (0.3d0, 0.1d0); T(1,3) = (0.2d0, -0.1d0); T(1,4) = (0.1d0, 0.05d0)
  T(2,2) = (2.0d0, -0.3d0); T(2,3) = (0.4d0, 0.2d0); T(2,4) = (0.15d0, -0.1d0)
  T(3,3) = (3.0d0, 1.0d0); T(3,4) = (0.5d0, 0.3d0)
  T(4,4) = (4.0d0, -0.5d0)
  ! Q = I
  do I = 1, N
    Q(I, I) = (1.0d0, 0.0d0)
  end do
  call ZTREXC('V', N, T, MAXN, Q, MAXN, 3, 1, INFO)
  call begin_test('move 3 to 1 compq=V')
  call print_array('T', T_r, 2*N*N)
  call print_array('Q', Q_r, 2*N*N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 2: Move eigenvalue from position 1 to position 4 in 4x4, COMPQ='V'
  ! ============================================================
  N = 4
  T = (0.0d0, 0.0d0); Q = (0.0d0, 0.0d0)
  T(1,1) = (1.0d0, 0.5d0); T(1,2) = (0.3d0, 0.1d0); T(1,3) = (0.2d0, -0.1d0); T(1,4) = (0.1d0, 0.05d0)
  T(2,2) = (2.0d0, -0.3d0); T(2,3) = (0.4d0, 0.2d0); T(2,4) = (0.15d0, -0.1d0)
  T(3,3) = (3.0d0, 1.0d0); T(3,4) = (0.5d0, 0.3d0)
  T(4,4) = (4.0d0, -0.5d0)
  do I = 1, N
    Q(I, I) = (1.0d0, 0.0d0)
  end do
  call ZTREXC('V', N, T, MAXN, Q, MAXN, 1, 4, INFO)
  call begin_test('move 1 to 4 compq=V')
  call print_array('T', T_r, 2*N*N)
  call print_array('Q', Q_r, 2*N*N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 3: COMPQ='N' (don't update Q), move position 2 to 4
  ! ============================================================
  N = 4
  T = (0.0d0, 0.0d0); Q = (0.0d0, 0.0d0)
  T(1,1) = (1.0d0, 0.5d0); T(1,2) = (0.3d0, 0.1d0); T(1,3) = (0.2d0, -0.1d0); T(1,4) = (0.1d0, 0.05d0)
  T(2,2) = (2.0d0, -0.3d0); T(2,3) = (0.4d0, 0.2d0); T(2,4) = (0.15d0, -0.1d0)
  T(3,3) = (3.0d0, 1.0d0); T(3,4) = (0.5d0, 0.3d0)
  T(4,4) = (4.0d0, -0.5d0)
  call ZTREXC('N', N, T, MAXN, Q, MAXN, 2, 4, INFO)
  call begin_test('move 2 to 4 compq=N')
  call print_array('T', T_r, 2*N*N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 4: IFST=ILST (no-op)
  ! ============================================================
  N = 4
  T = (0.0d0, 0.0d0); Q = (0.0d0, 0.0d0)
  T(1,1) = (1.0d0, 0.0d0); T(1,2) = (0.5d0, 0.0d0)
  T(2,2) = (2.0d0, 0.0d0)
  T(3,3) = (3.0d0, 0.0d0)
  T(4,4) = (4.0d0, 0.0d0)
  do I = 1, N
    Q(I, I) = (1.0d0, 0.0d0)
  end do
  call ZTREXC('V', N, T, MAXN, Q, MAXN, 2, 2, INFO)
  call begin_test('ifst=ilst no-op')
  call print_array('T', T_r, 2*N*N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 5: N=1 quick return
  ! ============================================================
  N = 1
  T = (0.0d0, 0.0d0); Q = (0.0d0, 0.0d0)
  T(1,1) = (5.0d0, 1.0d0)
  Q(1,1) = (1.0d0, 0.0d0)
  call ZTREXC('V', N, T, MAXN, Q, MAXN, 1, 1, INFO)
  call begin_test('N=1')
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 6: Move eigenvalue from position 4 to position 2 in 4x4
  ! ============================================================
  N = 4
  T = (0.0d0, 0.0d0); Q = (0.0d0, 0.0d0)
  T(1,1) = (5.0d0, 2.0d0); T(1,2) = (1.0d0, 0.3d0); T(1,3) = (0.5d0, -0.2d0); T(1,4) = (0.2d0, 0.1d0)
  T(2,2) = (3.0d0, -1.0d0); T(2,3) = (0.8d0, 0.4d0); T(2,4) = (0.3d0, -0.15d0)
  T(3,3) = (1.0d0, 0.5d0); T(3,4) = (0.6d0, 0.2d0)
  T(4,4) = (-1.0d0, 0.0d0)
  do I = 1, N
    Q(I, I) = (1.0d0, 0.0d0)
  end do
  call ZTREXC('V', N, T, MAXN, Q, MAXN, 4, 2, INFO)
  call begin_test('move 4 to 2 compq=V')
  call print_array('T', T_r, 2*N*N)
  call print_array('Q', Q_r, 2*N*N)
  call print_int('info', INFO)
  call end_test()

end program
