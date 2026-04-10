program test_ztgsna
  use test_utils
  implicit none

  integer, parameter :: MAXN = 4
  complex*16 :: A(MAXN, MAXN), B(MAXN, MAXN), VL(MAXN, MAXN), VR(MAXN, MAXN)
  complex*16 :: WORK(2*MAXN*MAXN)
  integer :: IWORK(MAXN+2)
  logical :: SEL(MAXN)
  double precision :: S(MAXN), DIFF(MAXN)
  integer :: INFO, M, N, I, LWORK

  ! ============================================================
  ! Test 1: JOB='B' HOWMNY='A' N=4 (both, all eigenpairs)
  ! Upper triangular A,B as generalized Schur form; VL=VR=I
  ! (so eigenvector directions coincide with canonical basis)
  ! ============================================================
  N = 4
  LWORK = 2*MAXN*MAXN
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  VL = (0.0d0, 0.0d0); VR = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 0.5d0);  A(1,2) = (0.3d0, 0.1d0);  A(1,3) = (0.1d0, -0.1d0); A(1,4) = (0.05d0, 0.02d0)
  A(2,2) = (3.0d0, -0.3d0); A(2,3) = (0.4d0, 0.3d0);  A(2,4) = (0.2d0, -0.1d0)
  A(3,3) = (4.0d0, 1.0d0);  A(3,4) = (0.6d0, 0.1d0)
  A(4,4) = (5.0d0, 0.8d0)
  B(1,1) = (1.0d0, 0.2d0);  B(1,2) = (0.1d0, 0.1d0);  B(1,3) = (0.05d0, 0.0d0); B(1,4) = (0.02d0, -0.01d0)
  B(2,2) = (2.0d0, -0.1d0); B(2,3) = (0.15d0, -0.05d0); B(2,4) = (0.08d0, 0.03d0)
  B(3,3) = (1.5d0, 0.3d0);  B(3,4) = (0.12d0, 0.04d0)
  B(4,4) = (3.0d0, 0.0d0)
  do I = 1, N
    VL(I,I) = (1.0d0, 0.0d0)
    VR(I,I) = (1.0d0, 0.0d0)
  end do
  S = 0.0d0; DIFF = 0.0d0

  call ZTGSNA('B', 'A', SEL, N, A, MAXN, B, MAXN, VL, MAXN, VR, MAXN, &
              S, DIFF, MAXN, M, WORK, LWORK, IWORK, INFO)

  call begin_test('job=B howmny=A N=4')
  call print_array('S', S, N)
  call print_array('DIF', DIFF, N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 2: JOB='E' HOWMNY='A' N=3 (eigenvalues only)
  ! ============================================================
  N = 3
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  VL = (0.0d0, 0.0d0); VR = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 1.0d0);  A(1,2) = (0.5d0, -0.2d0); A(1,3) = (0.3d0, 0.1d0)
  A(2,2) = (4.0d0, 0.0d0);  A(2,3) = (0.7d0, -0.3d0)
  A(3,3) = (6.0d0, -1.0d0)
  B(1,1) = (1.0d0, 0.0d0);  B(1,2) = (0.1d0, 0.05d0); B(1,3) = (0.0d0, 0.0d0)
  B(2,2) = (1.0d0, 0.0d0);  B(2,3) = (0.2d0, -0.1d0)
  B(3,3) = (1.0d0, 0.0d0)
  do I = 1, N
    VL(I,I) = (1.0d0, 0.0d0)
    VR(I,I) = (1.0d0, 0.0d0)
  end do
  S = 0.0d0; DIFF = 0.0d0

  call ZTGSNA('E', 'A', SEL, N, A, MAXN, B, MAXN, VL, MAXN, VR, MAXN, &
              S, DIFF, MAXN, M, WORK, LWORK, IWORK, INFO)

  call begin_test('job=E howmny=A N=3')
  call print_array('S', S, N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 3: JOB='V' HOWMNY='A' N=3 (eigenvectors only)
  ! ============================================================
  N = 3
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  VL = (0.0d0, 0.0d0); VR = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 1.0d0);  A(1,2) = (0.5d0, -0.2d0); A(1,3) = (0.3d0, 0.1d0)
  A(2,2) = (4.0d0, 0.0d0);  A(2,3) = (0.7d0, -0.3d0)
  A(3,3) = (6.0d0, -1.0d0)
  B(1,1) = (1.0d0, 0.0d0);  B(1,2) = (0.1d0, 0.05d0); B(1,3) = (0.0d0, 0.0d0)
  B(2,2) = (1.0d0, 0.0d0);  B(2,3) = (0.2d0, -0.1d0)
  B(3,3) = (1.0d0, 0.0d0)
  do I = 1, N
    VL(I,I) = (1.0d0, 0.0d0)
    VR(I,I) = (1.0d0, 0.0d0)
  end do
  S = 0.0d0; DIFF = 0.0d0

  call ZTGSNA('V', 'A', SEL, N, A, MAXN, B, MAXN, VL, MAXN, VR, MAXN, &
              S, DIFF, MAXN, M, WORK, LWORK, IWORK, INFO)

  call begin_test('job=V howmny=A N=3')
  call print_array('DIF', DIFF, N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 4: JOB='B' HOWMNY='S' N=4 (selected eigenpairs)
  ! Select eigenvalues 1 and 3
  ! ============================================================
  N = 4
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  VL = (0.0d0, 0.0d0); VR = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 0.5d0);  A(1,2) = (0.3d0, 0.1d0);  A(1,3) = (0.1d0, -0.1d0); A(1,4) = (0.05d0, 0.02d0)
  A(2,2) = (3.0d0, -0.3d0); A(2,3) = (0.4d0, 0.3d0);  A(2,4) = (0.2d0, -0.1d0)
  A(3,3) = (4.0d0, 1.0d0);  A(3,4) = (0.6d0, 0.1d0)
  A(4,4) = (5.0d0, 0.8d0)
  B(1,1) = (1.0d0, 0.2d0);  B(1,2) = (0.1d0, 0.1d0);  B(1,3) = (0.05d0, 0.0d0); B(1,4) = (0.02d0, -0.01d0)
  B(2,2) = (2.0d0, -0.1d0); B(2,3) = (0.15d0, -0.05d0); B(2,4) = (0.08d0, 0.03d0)
  B(3,3) = (1.5d0, 0.3d0);  B(3,4) = (0.12d0, 0.04d0)
  B(4,4) = (3.0d0, 0.0d0)
  do I = 1, N
    VL(I,I) = (1.0d0, 0.0d0)
    VR(I,I) = (1.0d0, 0.0d0)
  end do
  SEL(1) = .TRUE.; SEL(2) = .FALSE.
  SEL(3) = .TRUE.; SEL(4) = .FALSE.
  S = 0.0d0; DIFF = 0.0d0

  call ZTGSNA('B', 'S', SEL, N, A, MAXN, B, MAXN, VL, MAXN, VR, MAXN, &
              S, DIFF, 2, M, WORK, LWORK, IWORK, INFO)

  call begin_test('job=B howmny=S select=[T,F,T,F] N=4')
  call print_array('S', S, 2)
  call print_array('DIF', DIFF, 2)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 5: N=1 trivial case
  ! ============================================================
  N = 1
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  VL = (0.0d0, 0.0d0); VR = (0.0d0, 0.0d0)
  A(1,1) = (3.0d0, 2.0d0)
  B(1,1) = (1.0d0, 0.5d0)
  VL(1,1) = (1.0d0, 0.0d0)
  VR(1,1) = (1.0d0, 0.0d0)
  S = 0.0d0; DIFF = 0.0d0

  call ZTGSNA('B', 'A', SEL, N, A, MAXN, B, MAXN, VL, MAXN, VR, MAXN, &
              S, DIFF, MAXN, M, WORK, LWORK, IWORK, INFO)

  call begin_test('job=B howmny=A N=1')
  call print_array('S', S, N)
  call print_array('DIF', DIFF, N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

end program test_ztgsna
