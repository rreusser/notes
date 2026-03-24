program test_ztrsen
  use test_utils
  implicit none

  integer, parameter :: MAXN = 4
  complex*16 :: T(MAXN, MAXN), Q(MAXN, MAXN), W(MAXN), WORK(MAXN*MAXN)
  double precision :: T_r(2*MAXN*MAXN), Q_r(2*MAXN*MAXN), W_r(2*MAXN)
  equivalence (T, T_r)
  equivalence (Q, Q_r)
  equivalence (W, W_r)
  logical :: SELEC(MAXN)
  double precision :: S_val, SEP_val
  integer :: INFO, M, N, I, LWORK

  LWORK = MAXN * MAXN

  ! ============================================================
  ! Test 1: JOB='N', COMPQ='V', select eigenvalues 1 and 3
  ! ============================================================
  N = 4
  T = (0.0d0, 0.0d0); Q = (0.0d0, 0.0d0); W = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0); S_val = 0.0d0; SEP_val = 0.0d0
  T(1,1) = (1.0d0, 0.5d0); T(1,2) = (0.3d0, 0.1d0); T(1,3) = (0.2d0, -0.1d0); T(1,4) = (0.1d0, 0.05d0)
  T(2,2) = (2.0d0, -0.3d0); T(2,3) = (0.4d0, 0.2d0); T(2,4) = (0.15d0, -0.1d0)
  T(3,3) = (3.0d0, 1.0d0); T(3,4) = (0.5d0, 0.3d0)
  T(4,4) = (4.0d0, -0.5d0)
  do I = 1, N
    Q(I, I) = (1.0d0, 0.0d0)
  end do
  SELEC(1) = .TRUE.
  SELEC(2) = .FALSE.
  SELEC(3) = .TRUE.
  SELEC(4) = .FALSE.
  call ZTRSEN('N', 'V', SELEC, N, T, MAXN, Q, MAXN, W, M, S_val, SEP_val, WORK, LWORK, INFO)
  call begin_test('job=N compq=V sel 1,3')
  call print_array('T', T_r, 2*N*N)
  call print_array('Q', Q_r, 2*N*N)
  call print_array('W', W_r, 2*N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 2: JOB='E', COMPQ='V', select eigenvalues 1 and 3
  ! Compute condition number for eigenvalues
  ! ============================================================
  N = 4
  T = (0.0d0, 0.0d0); Q = (0.0d0, 0.0d0); W = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0); S_val = 0.0d0; SEP_val = 0.0d0
  T(1,1) = (1.0d0, 0.5d0); T(1,2) = (0.3d0, 0.1d0); T(1,3) = (0.2d0, -0.1d0); T(1,4) = (0.1d0, 0.05d0)
  T(2,2) = (2.0d0, -0.3d0); T(2,3) = (0.4d0, 0.2d0); T(2,4) = (0.15d0, -0.1d0)
  T(3,3) = (3.0d0, 1.0d0); T(3,4) = (0.5d0, 0.3d0)
  T(4,4) = (4.0d0, -0.5d0)
  do I = 1, N
    Q(I, I) = (1.0d0, 0.0d0)
  end do
  SELEC(1) = .TRUE.
  SELEC(2) = .FALSE.
  SELEC(3) = .TRUE.
  SELEC(4) = .FALSE.
  call ZTRSEN('E', 'V', SELEC, N, T, MAXN, Q, MAXN, W, M, S_val, SEP_val, WORK, LWORK, INFO)
  call begin_test('job=E compq=V sel 1,3')
  call print_array('T', T_r, 2*N*N)
  call print_array('Q', Q_r, 2*N*N)
  call print_array('W', W_r, 2*N)
  call print_int('M', M)
  call print_scalar('S', S_val)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 3: JOB='V', COMPQ='V', select eigenvalues 1 and 3
  ! Compute condition number for invariant subspace
  ! ============================================================
  N = 4
  T = (0.0d0, 0.0d0); Q = (0.0d0, 0.0d0); W = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0); S_val = 0.0d0; SEP_val = 0.0d0
  T(1,1) = (1.0d0, 0.5d0); T(1,2) = (0.3d0, 0.1d0); T(1,3) = (0.2d0, -0.1d0); T(1,4) = (0.1d0, 0.05d0)
  T(2,2) = (2.0d0, -0.3d0); T(2,3) = (0.4d0, 0.2d0); T(2,4) = (0.15d0, -0.1d0)
  T(3,3) = (3.0d0, 1.0d0); T(3,4) = (0.5d0, 0.3d0)
  T(4,4) = (4.0d0, -0.5d0)
  do I = 1, N
    Q(I, I) = (1.0d0, 0.0d0)
  end do
  SELEC(1) = .TRUE.
  SELEC(2) = .FALSE.
  SELEC(3) = .TRUE.
  SELEC(4) = .FALSE.
  call ZTRSEN('V', 'V', SELEC, N, T, MAXN, Q, MAXN, W, M, S_val, SEP_val, WORK, LWORK, INFO)
  call begin_test('job=V compq=V sel 1,3')
  call print_array('T', T_r, 2*N*N)
  call print_array('Q', Q_r, 2*N*N)
  call print_array('W', W_r, 2*N)
  call print_int('M', M)
  call print_scalar('SEP', SEP_val)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 4: JOB='B', COMPQ='V', select eigenvalues 1 and 3
  ! Compute both condition numbers
  ! ============================================================
  N = 4
  T = (0.0d0, 0.0d0); Q = (0.0d0, 0.0d0); W = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0); S_val = 0.0d0; SEP_val = 0.0d0
  T(1,1) = (1.0d0, 0.5d0); T(1,2) = (0.3d0, 0.1d0); T(1,3) = (0.2d0, -0.1d0); T(1,4) = (0.1d0, 0.05d0)
  T(2,2) = (2.0d0, -0.3d0); T(2,3) = (0.4d0, 0.2d0); T(2,4) = (0.15d0, -0.1d0)
  T(3,3) = (3.0d0, 1.0d0); T(3,4) = (0.5d0, 0.3d0)
  T(4,4) = (4.0d0, -0.5d0)
  do I = 1, N
    Q(I, I) = (1.0d0, 0.0d0)
  end do
  SELEC(1) = .TRUE.
  SELEC(2) = .FALSE.
  SELEC(3) = .TRUE.
  SELEC(4) = .FALSE.
  call ZTRSEN('B', 'V', SELEC, N, T, MAXN, Q, MAXN, W, M, S_val, SEP_val, WORK, LWORK, INFO)
  call begin_test('job=B compq=V sel 1,3')
  call print_array('T', T_r, 2*N*N)
  call print_array('Q', Q_r, 2*N*N)
  call print_array('W', W_r, 2*N)
  call print_int('M', M)
  call print_scalar('S', S_val)
  call print_scalar('SEP', SEP_val)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 5: JOB='N', COMPQ='N', select eigenvalue 2
  ! Don't update Q
  ! ============================================================
  N = 4
  T = (0.0d0, 0.0d0); Q = (0.0d0, 0.0d0); W = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0); S_val = 0.0d0; SEP_val = 0.0d0
  T(1,1) = (1.0d0, 0.5d0); T(1,2) = (0.3d0, 0.1d0); T(1,3) = (0.2d0, -0.1d0); T(1,4) = (0.1d0, 0.05d0)
  T(2,2) = (2.0d0, -0.3d0); T(2,3) = (0.4d0, 0.2d0); T(2,4) = (0.15d0, -0.1d0)
  T(3,3) = (3.0d0, 1.0d0); T(3,4) = (0.5d0, 0.3d0)
  T(4,4) = (4.0d0, -0.5d0)
  SELEC(1) = .FALSE.
  SELEC(2) = .TRUE.
  SELEC(3) = .FALSE.
  SELEC(4) = .FALSE.
  call ZTRSEN('N', 'N', SELEC, N, T, MAXN, Q, MAXN, W, M, S_val, SEP_val, WORK, LWORK, INFO)
  call begin_test('job=N compq=N sel 2')
  call print_array('T', T_r, 2*N*N)
  call print_array('W', W_r, 2*N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 6: N=1 quick return
  ! ============================================================
  N = 1
  T = (0.0d0, 0.0d0); Q = (0.0d0, 0.0d0); W = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0); S_val = 0.0d0; SEP_val = 0.0d0; M = 0
  T(1,1) = (5.0d0, 1.0d0)
  Q(1,1) = (1.0d0, 0.0d0)
  SELEC(1) = .TRUE.
  call ZTRSEN('B', 'V', SELEC, N, T, MAXN, Q, MAXN, W, M, S_val, SEP_val, WORK, LWORK, INFO)
  call begin_test('N=1')
  call print_array('W', W_r, 2)
  call print_int('M', M)
  call print_scalar('S', S_val)
  call print_scalar('SEP', SEP_val)
  call print_int('info', INFO)
  call end_test()

end program
