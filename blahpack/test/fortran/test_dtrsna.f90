program test_dtrsna
  use test_utils
  implicit none
  double precision :: T(5,5), VL(5,5), VR(5,5), S(5), SEP(5)
  double precision :: WORK(5,11)
  integer :: IWORK(10), INFO, N, M, MM, LDWORK
  logical :: SEL(5)
  character :: JOB, HOWMNY
  integer :: i, j

  LDWORK = 5

  ! Test 1: N=1 diagonal
  N = 1; JOB = 'B'; HOWMNY = 'A'; MM = 5
  T = 0.0d0; T(1,1) = 5.0d0
  VL = 0.0d0; VL(1,1) = 1.0d0
  VR = 0.0d0; VR(1,1) = 1.0d0
  SEL = .TRUE.
  S = 0.0d0; SEP = 0.0d0
  call DTRSNA(JOB, HOWMNY, SEL, N, T, 5, VL, 5, VR, 5, S, SEP, MM, M, WORK, LDWORK, IWORK, INFO)
  call begin_test('n1_both')
  call print_scalar('S1', S(1))
  call print_scalar('SEP1', SEP(1))
  call print_int('M', M)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 2: N=3 diagonal, job=E (eigenvalues only), all
  N = 3; JOB = 'E'; HOWMNY = 'A'; MM = 5
  T = 0.0d0; T(1,1) = 1.0d0; T(2,2) = 2.0d0; T(3,3) = 4.0d0
  VL = 0.0d0; VL(1,1) = 1.0d0; VL(2,2) = 1.0d0; VL(3,3) = 1.0d0
  VR = 0.0d0; VR(1,1) = 1.0d0; VR(2,2) = 1.0d0; VR(3,3) = 1.0d0
  SEL = .TRUE.
  S = 0.0d0; SEP = 0.0d0
  call DTRSNA(JOB, HOWMNY, SEL, N, T, 5, VL, 5, VR, 5, S, SEP, MM, M, WORK, LDWORK, IWORK, INFO)
  call begin_test('n3_diag_job_E_all')
  call print_scalar('S1', S(1))
  call print_scalar('S2', S(2))
  call print_scalar('S3', S(3))
  call print_int('M', M)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 3: N=3 diagonal, job=V (eigenvectors only)
  N = 3; JOB = 'V'; HOWMNY = 'A'; MM = 5
  T = 0.0d0; T(1,1) = 1.0d0; T(2,2) = 2.0d0; T(3,3) = 4.0d0
  VL = 0.0d0; VL(1,1) = 1.0d0; VL(2,2) = 1.0d0; VL(3,3) = 1.0d0
  VR = 0.0d0; VR(1,1) = 1.0d0; VR(2,2) = 1.0d0; VR(3,3) = 1.0d0
  SEL = .TRUE.
  S = 0.0d0; SEP = 0.0d0
  call DTRSNA(JOB, HOWMNY, SEL, N, T, 5, VL, 5, VR, 5, S, SEP, MM, M, WORK, LDWORK, IWORK, INFO)
  call begin_test('n3_diag_job_V_all')
  call print_scalar('SEP1', SEP(1))
  call print_scalar('SEP2', SEP(2))
  call print_scalar('SEP3', SEP(3))
  call print_int('M', M)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 4: N=3 diagonal, job=B (both)
  N = 3; JOB = 'B'; HOWMNY = 'A'; MM = 5
  T = 0.0d0; T(1,1) = 1.0d0; T(2,2) = 2.0d0; T(3,3) = 4.0d0
  VL = 0.0d0; VL(1,1) = 1.0d0; VL(2,2) = 1.0d0; VL(3,3) = 1.0d0
  VR = 0.0d0; VR(1,1) = 1.0d0; VR(2,2) = 1.0d0; VR(3,3) = 1.0d0
  SEL = .TRUE.
  S = 0.0d0; SEP = 0.0d0
  call DTRSNA(JOB, HOWMNY, SEL, N, T, 5, VL, 5, VR, 5, S, SEP, MM, M, WORK, LDWORK, IWORK, INFO)
  call begin_test('n3_diag_job_B_all')
  call print_scalar('S1', S(1))
  call print_scalar('S2', S(2))
  call print_scalar('S3', S(3))
  call print_scalar('SEP1', SEP(1))
  call print_scalar('SEP2', SEP(2))
  call print_scalar('SEP3', SEP(3))
  call print_int('M', M)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 5: N=3 upper triangular (non-diagonal), job=B
  ! Eigenvalues 1, 2, 4 on diagonal.
  ! Right eigenvectors (upper triangular T means right evecs are upper).
  ! For T = [1 a b; 0 2 c; 0 0 4]: lambda=1 -> v=(1,0,0);
  !   lambda=2 -> v=(a,1,0) (columns); lambda=4 -> v=(bb,cc,1)
  ! Left eigenvectors are rows of T^(-1) * ... actually use identity approx.
  N = 3; JOB = 'B'; HOWMNY = 'A'; MM = 5
  T = 0.0d0
  T(1,1) = 1.0d0; T(1,2) = 0.5d0; T(1,3) = 0.25d0
  T(2,2) = 2.0d0; T(2,3) = 0.5d0
  T(3,3) = 4.0d0
  ! Right eigenvectors (normalized columns):
  VR = 0.0d0
  VR(1,1) = 1.0d0
  VR(1,2) = 0.4472135954999579d0   ! 0.5/sqrt(1+0.25) = 0.5/1.118 scaled so col norm =1; actually (0.5,1,0)/sqrt(1.25)
  VR(2,2) = 0.8944271909999159d0
  ! lambda=4: solve (T-4I)v=0 -> v1=(0.5*v2+0.25*v3)/3, v2=0.5*v3/2, v3=1
  ! v2 = 0.25, v1 = (0.125+0.25)/3 = 0.125; vec=(0.125,0.25,1), norm=sqrt(0.015625+0.0625+1)=1.0382
  VR(1,3) = 0.12038585308576454d0
  VR(2,3) = 0.24077170617152908d0
  VR(3,3) = 0.9630868246861164d0
  ! Left eigenvectors: rows of inv(VR) normalized, or computed from T^T.
  ! Solve T^T y = lambda y. For lambda=1: y = (1,0,0); lambda=2: (T^T-2I)y=0 ->
  !   -y1+0.5... actually for simplicity just use T inverse.
  VL = 0.0d0
  VL(1,1) = 1.0d0
  ! lambda=2: solve (T^T-2I)y=0: -y1=0? Row1: (-1)y1=0->y1=0; Row2: 0.5y1+0y2=0 ok; Row3: 0.25y1+0.5y2+2y3=0 -> y3=-y2/4
  ! set y2=1 -> y3=-0.25, norm=sqrt(1+0.0625)=1.03078
  VL(2,2) = 0.9701425001453319d0
  VL(3,2) = -0.24253562503633297d0
  ! lambda=4: (T^T-4I)y=0: -3y1=0->y1=0; 0.5y1-2y2=0->y2=0; 0.25y1+0.5y2+0y3=0 ok, y3 free
  VL(3,3) = 1.0d0
  SEL = .TRUE.
  S = 0.0d0; SEP = 0.0d0
  call DTRSNA(JOB, HOWMNY, SEL, N, T, 5, VL, 5, VR, 5, S, SEP, MM, M, WORK, LDWORK, IWORK, INFO)
  call begin_test('n3_triangular_job_B')
  call print_scalar('S1', S(1))
  call print_scalar('S2', S(2))
  call print_scalar('S3', S(3))
  call print_scalar('SEP1', SEP(1))
  call print_scalar('SEP2', SEP(2))
  call print_scalar('SEP3', SEP(3))
  call print_int('M', M)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 6: N=3 diagonal, HOWMNY='S' selected (only 2nd eigenvalue)
  N = 3; JOB = 'B'; HOWMNY = 'S'; MM = 5
  T = 0.0d0; T(1,1) = 1.0d0; T(2,2) = 2.0d0; T(3,3) = 4.0d0
  VL = 0.0d0; VL(1,1) = 1.0d0; VL(2,2) = 1.0d0; VL(3,3) = 1.0d0
  VR = 0.0d0
  VR(1,1) = 1.0d0  ! first column unused in selection
  VR(2,1) = 1.0d0  ! for selected k=2: vector goes in column (ks=1)
  VR(3,1) = 1.0d0  ! k=3 not selected
  ! NOTE: When HOWMNY='S', the columns of VL/VR are packed: the ks-th
  ! selected eigenvalue occupies the ks-th column. Here only k=2 is
  ! selected so VR(:,1) = (0,1,0), VL(:,1) = (0,1,0).
  VR = 0.0d0; VR(2,1) = 1.0d0
  VL = 0.0d0; VL(2,1) = 1.0d0
  SEL = .FALSE.; SEL(2) = .TRUE.
  S = 0.0d0; SEP = 0.0d0
  call DTRSNA(JOB, HOWMNY, SEL, N, T, 5, VL, 5, VR, 5, S, SEP, MM, M, WORK, LDWORK, IWORK, INFO)
  call begin_test('n3_diag_selected_k2')
  call print_scalar('S1', S(1))
  call print_scalar('SEP1', SEP(1))
  call print_int('M', M)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 7: N=2 with complex conjugate pair (2x2 block)
  ! T = [[0, -1], [1, 0]]: eigenvalues +/- i
  ! Right eigenvectors (real rep): col1 = (1,0), col2 = (0,1)
  ! Left eigenvectors (real rep): row1 = (1,0), row2 = (0,1)
  N = 2; JOB = 'B'; HOWMNY = 'A'; MM = 5
  T = 0.0d0
  T(1,1) = 0.0d0; T(1,2) = -1.0d0
  T(2,1) = 1.0d0; T(2,2) = 0.0d0
  VR = 0.0d0; VR(1,1) = 1.0d0; VR(2,2) = 1.0d0
  VL = 0.0d0; VL(1,1) = 1.0d0; VL(2,2) = 1.0d0
  SEL = .TRUE.
  S = 0.0d0; SEP = 0.0d0
  call DTRSNA(JOB, HOWMNY, SEL, N, T, 5, VL, 5, VR, 5, S, SEP, MM, M, WORK, LDWORK, IWORK, INFO)
  call begin_test('n2_complex_pair')
  call print_scalar('S1', S(1))
  call print_scalar('S2', S(2))
  call print_scalar('SEP1', SEP(1))
  call print_scalar('SEP2', SEP(2))
  call print_int('M', M)
  call print_int('INFO', INFO)
  call end_test()

  ! Silence unused-var warnings on i, j
  i = 0; j = 0
  if ( i + j .lt. 0 ) print *, i, j

end program
