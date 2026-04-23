program test_ztgsen
  use test_utils
  implicit none

  integer, parameter :: MAXN = 4
  complex*16 :: A(MAXN, MAXN), B(MAXN, MAXN), Q(MAXN, MAXN), Z(MAXN, MAXN)
  complex*16 :: ALPHA(MAXN), BETA(MAXN)
  complex*16 :: WORK(4*MAXN*MAXN)
  integer :: IWORK(4*MAXN*MAXN)
  logical :: SEL(MAXN)
  double precision :: DIF(2), PL, PR
  integer :: INFO, M, N, I, LWORK, LIWORK

  complex*16 :: PACK(MAXN*MAXN)
  double precision :: PACK_r(2*MAXN*MAXN)
  equivalence (PACK, PACK_r)

  double precision :: ALPHA_r(2*MAXN), BETA_r(2*MAXN)
  equivalence (ALPHA, ALPHA_r)
  equivalence (BETA, BETA_r)

  ! ============================================================
  ! Test 1: IJOB=0, select eigenvalues 1 and 3, wantq=T, wantz=T, N=4
  ! ============================================================
  N = 4
  LWORK = 4*MAXN*MAXN
  LIWORK = 4*MAXN*MAXN
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.5d0);  A(1,2) = (0.3d0, 0.1d0);  A(1,3) = (0.1d0, -0.1d0); A(1,4) = (0.05d0, 0.02d0)
  A(2,2) = (2.0d0, -0.3d0); A(2,3) = (0.4d0, 0.3d0);  A(2,4) = (0.2d0, -0.1d0)
  A(3,3) = (3.0d0, 1.0d0);  A(3,4) = (0.6d0, 0.1d0)
  A(4,4) = (4.0d0, 0.8d0)
  B(1,1) = (1.0d0, 0.2d0);  B(1,2) = (0.1d0, 0.1d0);  B(1,3) = (0.05d0, 0.0d0);  B(1,4) = (0.02d0, -0.01d0)
  B(2,2) = (2.0d0, -0.1d0); B(2,3) = (0.15d0, -0.05d0); B(2,4) = (0.08d0, 0.03d0)
  B(3,3) = (1.5d0, 0.3d0);  B(3,4) = (0.12d0, 0.04d0)
  B(4,4) = (3.0d0, 0.0d0)
  do I = 1, N
    Q(I,I) = (1.0d0, 0.0d0)
    Z(I,I) = (1.0d0, 0.0d0)
  end do
  SEL(1) = .TRUE.;  SEL(2) = .FALSE.
  SEL(3) = .TRUE.;  SEL(4) = .FALSE.

  call ZTGSEN(0, .TRUE., .TRUE., SEL, N, A, MAXN, B, MAXN, &
              ALPHA, BETA, Q, MAXN, Z, MAXN, M, PL, PR, DIF, &
              WORK, LWORK, IWORK, LIWORK, INFO)

  call begin_test('ijob=0 select=[T,F,T,F] wantq=T wantz=T N=4')
  call pack_and_print('A', A, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('B', B, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('Q', Q, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('Z', Z, MAXN, N, N, PACK, PACK_r)
  call print_array('ALPHA', ALPHA_r, 2*N)
  call print_array('BETA', BETA_r, 2*N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 2: IJOB=0, select all, N=3
  ! ============================================================
  N = 3
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 1.0d0);  A(1,2) = (0.5d0, -0.2d0); A(1,3) = (0.3d0, 0.1d0)
  A(2,2) = (4.0d0, 0.0d0);  A(2,3) = (0.7d0, -0.3d0)
  A(3,3) = (6.0d0, -1.0d0)
  B(1,1) = (1.0d0, 0.0d0);  B(1,2) = (0.1d0, 0.05d0); B(1,3) = (0.0d0, 0.0d0)
  B(2,2) = (1.0d0, 0.0d0);  B(2,3) = (0.2d0, -0.1d0)
  B(3,3) = (1.0d0, 0.0d0)
  do I = 1, N
    Q(I,I) = (1.0d0, 0.0d0)
    Z(I,I) = (1.0d0, 0.0d0)
  end do
  SEL(1) = .TRUE.; SEL(2) = .TRUE.; SEL(3) = .TRUE.

  call ZTGSEN(0, .TRUE., .TRUE., SEL, N, A, MAXN, B, MAXN, &
              ALPHA, BETA, Q, MAXN, Z, MAXN, M, PL, PR, DIF, &
              WORK, LWORK, IWORK, LIWORK, INFO)

  call begin_test('ijob=0 select=[T,T,T] wantq=T wantz=T N=3')
  call pack_and_print('A', A, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('B', B, MAXN, N, N, PACK, PACK_r)
  call print_array('ALPHA', ALPHA_r, 2*N)
  call print_array('BETA', BETA_r, 2*N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 3: IJOB=0, select none, N=3
  ! ============================================================
  N = 3
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 1.0d0);  A(1,2) = (0.5d0, -0.2d0); A(1,3) = (0.3d0, 0.1d0)
  A(2,2) = (4.0d0, 0.0d0);  A(2,3) = (0.7d0, -0.3d0)
  A(3,3) = (6.0d0, -1.0d0)
  B(1,1) = (1.0d0, 0.0d0);  B(1,2) = (0.1d0, 0.05d0); B(1,3) = (0.0d0, 0.0d0)
  B(2,2) = (1.0d0, 0.0d0);  B(2,3) = (0.2d0, -0.1d0)
  B(3,3) = (1.0d0, 0.0d0)
  do I = 1, N
    Q(I,I) = (1.0d0, 0.0d0)
    Z(I,I) = (1.0d0, 0.0d0)
  end do
  SEL(1) = .FALSE.; SEL(2) = .FALSE.; SEL(3) = .FALSE.

  call ZTGSEN(0, .TRUE., .TRUE., SEL, N, A, MAXN, B, MAXN, &
              ALPHA, BETA, Q, MAXN, Z, MAXN, M, PL, PR, DIF, &
              WORK, LWORK, IWORK, LIWORK, INFO)

  call begin_test('ijob=0 select=[F,F,F] wantq=T wantz=T N=3')
  call print_array('ALPHA', ALPHA_r, 2*N)
  call print_array('BETA', BETA_r, 2*N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 4: IJOB=0, wantq=F, wantz=F, N=4
  ! ============================================================
  N = 4
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.5d0);  A(1,2) = (0.3d0, 0.1d0);  A(1,3) = (0.1d0, -0.1d0); A(1,4) = (0.05d0, 0.02d0)
  A(2,2) = (2.0d0, -0.3d0); A(2,3) = (0.4d0, 0.3d0);  A(2,4) = (0.2d0, -0.1d0)
  A(3,3) = (3.0d0, 1.0d0);  A(3,4) = (0.6d0, 0.1d0)
  A(4,4) = (4.0d0, 0.8d0)
  B(1,1) = (1.0d0, 0.2d0);  B(1,2) = (0.1d0, 0.1d0);  B(1,3) = (0.05d0, 0.0d0);  B(1,4) = (0.02d0, -0.01d0)
  B(2,2) = (2.0d0, -0.1d0); B(2,3) = (0.15d0, -0.05d0); B(2,4) = (0.08d0, 0.03d0)
  B(3,3) = (1.5d0, 0.3d0);  B(3,4) = (0.12d0, 0.04d0)
  B(4,4) = (3.0d0, 0.0d0)
  SEL(1) = .FALSE.; SEL(2) = .TRUE.
  SEL(3) = .FALSE.; SEL(4) = .TRUE.

  call ZTGSEN(0, .FALSE., .FALSE., SEL, N, A, MAXN, B, MAXN, &
              ALPHA, BETA, Q, MAXN, Z, MAXN, M, PL, PR, DIF, &
              WORK, LWORK, IWORK, LIWORK, INFO)

  call begin_test('ijob=0 select=[F,T,F,T] wantq=F wantz=F N=4')
  call pack_and_print('A', A, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('B', B, MAXN, N, N, PACK, PACK_r)
  call print_array('ALPHA', ALPHA_r, 2*N)
  call print_array('BETA', BETA_r, 2*N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 5: IJOB=1 (condition numbers for eigenvalue cluster), N=4
  ! ============================================================
  N = 4
  LWORK = 4*MAXN*MAXN
  LIWORK = 4*MAXN*MAXN
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.5d0);  A(1,2) = (0.3d0, 0.1d0);  A(1,3) = (0.1d0, -0.1d0); A(1,4) = (0.05d0, 0.02d0)
  A(2,2) = (2.0d0, -0.3d0); A(2,3) = (0.4d0, 0.3d0);  A(2,4) = (0.2d0, -0.1d0)
  A(3,3) = (3.0d0, 1.0d0);  A(3,4) = (0.6d0, 0.1d0)
  A(4,4) = (4.0d0, 0.8d0)
  B(1,1) = (1.0d0, 0.2d0);  B(1,2) = (0.1d0, 0.1d0);  B(1,3) = (0.05d0, 0.0d0);  B(1,4) = (0.02d0, -0.01d0)
  B(2,2) = (2.0d0, -0.1d0); B(2,3) = (0.15d0, -0.05d0); B(2,4) = (0.08d0, 0.03d0)
  B(3,3) = (1.5d0, 0.3d0);  B(3,4) = (0.12d0, 0.04d0)
  B(4,4) = (3.0d0, 0.0d0)
  do I = 1, N
    Q(I,I) = (1.0d0, 0.0d0)
    Z(I,I) = (1.0d0, 0.0d0)
  end do
  SEL(1) = .TRUE.;  SEL(2) = .FALSE.
  SEL(3) = .TRUE.;  SEL(4) = .FALSE.

  call ZTGSEN(1, .TRUE., .TRUE., SEL, N, A, MAXN, B, MAXN, &
              ALPHA, BETA, Q, MAXN, Z, MAXN, M, PL, PR, DIF, &
              WORK, LWORK, IWORK, LIWORK, INFO)

  call begin_test('ijob=1 select=[T,F,T,F] wantq=T wantz=T N=4')
  call pack_and_print('A', A, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('B', B, MAXN, N, N, PACK, PACK_r)
  call print_array('ALPHA', ALPHA_r, 2*N)
  call print_array('BETA', BETA_r, 2*N)
  call print_scalar('PL', PL)
  call print_scalar('PR', PR)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 6: N=1 (trivial case)
  ! ============================================================
  N = 1
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  A(1,1) = (5.0d0, 3.0d0)
  B(1,1) = (2.0d0, 1.0d0)
  Q(1,1) = (1.0d0, 0.0d0)
  Z(1,1) = (1.0d0, 0.0d0)
  SEL(1) = .TRUE.

  call ZTGSEN(0, .TRUE., .TRUE., SEL, N, A, MAXN, B, MAXN, &
              ALPHA, BETA, Q, MAXN, Z, MAXN, M, PL, PR, DIF, &
              WORK, LWORK, IWORK, LIWORK, INFO)

  call begin_test('ijob=0 N=1 select=[T]')
  call print_array('ALPHA', ALPHA_r, 2*N)
  call print_array('BETA', BETA_r, 2*N)
  call pack_and_print('A', A, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('B', B, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('Q', Q, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('Z', Z, MAXN, N, N, PACK, PACK_r)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 7: IJOB=2 (DIF computation via direct solve), N=4
  ! ============================================================
  N = 4
  LWORK = 4*MAXN*MAXN
  LIWORK = 4*MAXN*MAXN
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.5d0);  A(1,2) = (0.3d0, 0.1d0);  A(1,3) = (0.1d0, -0.1d0); A(1,4) = (0.05d0, 0.02d0)
  A(2,2) = (2.0d0, -0.3d0); A(2,3) = (0.4d0, 0.3d0);  A(2,4) = (0.2d0, -0.1d0)
  A(3,3) = (3.0d0, 1.0d0);  A(3,4) = (0.6d0, 0.1d0)
  A(4,4) = (4.0d0, 0.8d0)
  B(1,1) = (1.0d0, 0.2d0);  B(1,2) = (0.1d0, 0.1d0);  B(1,3) = (0.05d0, 0.0d0);  B(1,4) = (0.02d0, -0.01d0)
  B(2,2) = (2.0d0, -0.1d0); B(2,3) = (0.15d0, -0.05d0); B(2,4) = (0.08d0, 0.03d0)
  B(3,3) = (1.5d0, 0.3d0);  B(3,4) = (0.12d0, 0.04d0)
  B(4,4) = (3.0d0, 0.0d0)
  do I = 1, N
    Q(I,I) = (1.0d0, 0.0d0)
    Z(I,I) = (1.0d0, 0.0d0)
  end do
  SEL(1) = .TRUE.;  SEL(2) = .FALSE.
  SEL(3) = .TRUE.;  SEL(4) = .FALSE.

  call ZTGSEN(2, .TRUE., .TRUE., SEL, N, A, MAXN, B, MAXN, &
              ALPHA, BETA, Q, MAXN, Z, MAXN, M, PL, PR, DIF, &
              WORK, LWORK, IWORK, LIWORK, INFO)

  call begin_test('ijob=2 select=[T,F,T,F] wantq=T wantz=T N=4')
  call print_array('DIF', DIF, 2)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 8: IJOB=3 (DIF via condition estimation), N=4
  ! ============================================================
  N = 4
  LWORK = 4*MAXN*MAXN
  LIWORK = 4*MAXN*MAXN
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.5d0);  A(1,2) = (0.3d0, 0.1d0);  A(1,3) = (0.1d0, -0.1d0); A(1,4) = (0.05d0, 0.02d0)
  A(2,2) = (2.0d0, -0.3d0); A(2,3) = (0.4d0, 0.3d0);  A(2,4) = (0.2d0, -0.1d0)
  A(3,3) = (3.0d0, 1.0d0);  A(3,4) = (0.6d0, 0.1d0)
  A(4,4) = (4.0d0, 0.8d0)
  B(1,1) = (1.0d0, 0.2d0);  B(1,2) = (0.1d0, 0.1d0);  B(1,3) = (0.05d0, 0.0d0);  B(1,4) = (0.02d0, -0.01d0)
  B(2,2) = (2.0d0, -0.1d0); B(2,3) = (0.15d0, -0.05d0); B(2,4) = (0.08d0, 0.03d0)
  B(3,3) = (1.5d0, 0.3d0);  B(3,4) = (0.12d0, 0.04d0)
  B(4,4) = (3.0d0, 0.0d0)
  do I = 1, N
    Q(I,I) = (1.0d0, 0.0d0)
    Z(I,I) = (1.0d0, 0.0d0)
  end do
  SEL(1) = .TRUE.;  SEL(2) = .FALSE.
  SEL(3) = .TRUE.;  SEL(4) = .FALSE.

  call ZTGSEN(3, .TRUE., .TRUE., SEL, N, A, MAXN, B, MAXN, &
              ALPHA, BETA, Q, MAXN, Z, MAXN, M, PL, PR, DIF, &
              WORK, LWORK, IWORK, LIWORK, INFO)

  call begin_test('ijob=3 select=[T,F,T,F] wantq=T wantz=T N=4')
  call print_array('DIF', DIF, 2)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 9: IJOB=4 (PL,PR + DIF direct), N=4
  ! ============================================================
  N = 4
  LWORK = 4*MAXN*MAXN
  LIWORK = 4*MAXN*MAXN
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.5d0);  A(1,2) = (0.3d0, 0.1d0);  A(1,3) = (0.1d0, -0.1d0); A(1,4) = (0.05d0, 0.02d0)
  A(2,2) = (2.0d0, -0.3d0); A(2,3) = (0.4d0, 0.3d0);  A(2,4) = (0.2d0, -0.1d0)
  A(3,3) = (3.0d0, 1.0d0);  A(3,4) = (0.6d0, 0.1d0)
  A(4,4) = (4.0d0, 0.8d0)
  B(1,1) = (1.0d0, 0.2d0);  B(1,2) = (0.1d0, 0.1d0);  B(1,3) = (0.05d0, 0.0d0);  B(1,4) = (0.02d0, -0.01d0)
  B(2,2) = (2.0d0, -0.1d0); B(2,3) = (0.15d0, -0.05d0); B(2,4) = (0.08d0, 0.03d0)
  B(3,3) = (1.5d0, 0.3d0);  B(3,4) = (0.12d0, 0.04d0)
  B(4,4) = (3.0d0, 0.0d0)
  do I = 1, N
    Q(I,I) = (1.0d0, 0.0d0)
    Z(I,I) = (1.0d0, 0.0d0)
  end do
  SEL(1) = .TRUE.;  SEL(2) = .FALSE.
  SEL(3) = .TRUE.;  SEL(4) = .FALSE.

  call ZTGSEN(4, .TRUE., .TRUE., SEL, N, A, MAXN, B, MAXN, &
              ALPHA, BETA, Q, MAXN, Z, MAXN, M, PL, PR, DIF, &
              WORK, LWORK, IWORK, LIWORK, INFO)

  call begin_test('ijob=4 select=[T,F,T,F] wantq=T wantz=T N=4')
  call print_scalar('PL', PL)
  call print_scalar('PR', PR)
  call print_array('DIF', DIF, 2)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 10: IJOB=5 (PL,PR + DIF estimated), N=4
  ! ============================================================
  N = 4
  LWORK = 4*MAXN*MAXN
  LIWORK = 4*MAXN*MAXN
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.5d0);  A(1,2) = (0.3d0, 0.1d0);  A(1,3) = (0.1d0, -0.1d0); A(1,4) = (0.05d0, 0.02d0)
  A(2,2) = (2.0d0, -0.3d0); A(2,3) = (0.4d0, 0.3d0);  A(2,4) = (0.2d0, -0.1d0)
  A(3,3) = (3.0d0, 1.0d0);  A(3,4) = (0.6d0, 0.1d0)
  A(4,4) = (4.0d0, 0.8d0)
  B(1,1) = (1.0d0, 0.2d0);  B(1,2) = (0.1d0, 0.1d0);  B(1,3) = (0.05d0, 0.0d0);  B(1,4) = (0.02d0, -0.01d0)
  B(2,2) = (2.0d0, -0.1d0); B(2,3) = (0.15d0, -0.05d0); B(2,4) = (0.08d0, 0.03d0)
  B(3,3) = (1.5d0, 0.3d0);  B(3,4) = (0.12d0, 0.04d0)
  B(4,4) = (3.0d0, 0.0d0)
  do I = 1, N
    Q(I,I) = (1.0d0, 0.0d0)
    Z(I,I) = (1.0d0, 0.0d0)
  end do
  SEL(1) = .TRUE.;  SEL(2) = .FALSE.
  SEL(3) = .TRUE.;  SEL(4) = .FALSE.

  call ZTGSEN(5, .TRUE., .TRUE., SEL, N, A, MAXN, B, MAXN, &
              ALPHA, BETA, Q, MAXN, Z, MAXN, M, PL, PR, DIF, &
              WORK, LWORK, IWORK, LIWORK, INFO)

  call begin_test('ijob=5 select=[T,F,T,F] wantq=T wantz=T N=4')
  call print_scalar('PL', PL)
  call print_scalar('PR', PR)
  call print_array('DIF', DIF, 2)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 11: IJOB=2, all selected (M=N early return path), N=3
  ! ============================================================
  N = 3
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 1.0d0);  A(1,2) = (0.5d0, -0.2d0); A(1,3) = (0.3d0, 0.1d0)
  A(2,2) = (4.0d0, 0.0d0);  A(2,3) = (0.7d0, -0.3d0)
  A(3,3) = (6.0d0, -1.0d0)
  B(1,1) = (1.0d0, 0.0d0);  B(1,2) = (0.1d0, 0.05d0); B(1,3) = (0.0d0, 0.0d0)
  B(2,2) = (1.0d0, 0.0d0);  B(2,3) = (0.2d0, -0.1d0)
  B(3,3) = (1.0d0, 0.0d0)
  do I = 1, N
    Q(I,I) = (1.0d0, 0.0d0)
    Z(I,I) = (1.0d0, 0.0d0)
  end do
  SEL(1) = .TRUE.; SEL(2) = .TRUE.; SEL(3) = .TRUE.

  call ZTGSEN(2, .TRUE., .TRUE., SEL, N, A, MAXN, B, MAXN, &
              ALPHA, BETA, Q, MAXN, Z, MAXN, M, PL, PR, DIF, &
              WORK, LWORK, IWORK, LIWORK, INFO)

  call begin_test('ijob=2 select=[T,T,T] wantq=T wantz=T N=3 allsel')
  call print_array('DIF', DIF, 2)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 12: IJOB=1, none selected (M=0 early return path), N=3
  ! ============================================================
  N = 3
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 1.0d0);  A(1,2) = (0.5d0, -0.2d0); A(1,3) = (0.3d0, 0.1d0)
  A(2,2) = (4.0d0, 0.0d0);  A(2,3) = (0.7d0, -0.3d0)
  A(3,3) = (6.0d0, -1.0d0)
  B(1,1) = (1.0d0, 0.0d0);  B(1,2) = (0.1d0, 0.05d0); B(1,3) = (0.0d0, 0.0d0)
  B(2,2) = (1.0d0, 0.0d0);  B(2,3) = (0.2d0, -0.1d0)
  B(3,3) = (1.0d0, 0.0d0)
  do I = 1, N
    Q(I,I) = (1.0d0, 0.0d0)
    Z(I,I) = (1.0d0, 0.0d0)
  end do
  SEL(1) = .FALSE.; SEL(2) = .FALSE.; SEL(3) = .FALSE.

  call ZTGSEN(1, .TRUE., .TRUE., SEL, N, A, MAXN, B, MAXN, &
              ALPHA, BETA, Q, MAXN, Z, MAXN, M, PL, PR, DIF, &
              WORK, LWORK, IWORK, LIWORK, INFO)

  call begin_test('ijob=1 select=[F,F,F] wantq=T wantz=T N=3 nonesel')
  call print_scalar('PL', PL)
  call print_scalar('PR', PR)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

contains

  subroutine pack_and_print(label, MAT, LDM, NROWS, NCOLS, PK, PK_r)
    character(*), intent(in) :: label
    integer, intent(in) :: LDM, NROWS, NCOLS
    complex*16, intent(in) :: MAT(LDM, *)
    complex*16, intent(inout) :: PK(*)
    double precision, intent(inout) :: PK_r(*)
    integer :: II, JJ, KK
    KK = 1
    do JJ = 1, NCOLS
      do II = 1, NROWS
        PK(KK) = MAT(II, JJ)
        KK = KK + 1
      end do
    end do
    call print_array(label, PK_r, 2*NROWS*NCOLS)
  end subroutine

end program test_ztgsen
