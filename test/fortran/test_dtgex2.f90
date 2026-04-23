program test_dtgex2
  use test_utils
  implicit none

  integer, parameter :: MAXN = 6
  double precision :: A(MAXN, MAXN), B(MAXN, MAXN)
  double precision :: Q(MAXN, MAXN), Z(MAXN, MAXN)
  double precision :: WORK(200)
  integer :: INFO, LWORK, N, J1, N1, N2
  logical :: WANTQ, WANTZ

  LWORK = 200

  ! ============================================
  ! Test 1: 1x1 swap, no Q/Z accumulation
  ! ============================================
  N = 3; J1 = 1; N1 = 1; N2 = 1
  WANTQ = .FALSE.; WANTZ = .FALSE.

  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0

  ! Upper triangular A
  A(1,1) = 1.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0
  A(2,2) = 2.0d0; A(2,3) = 0.4d0
  A(3,3) = 3.0d0

  ! Upper triangular B
  B(1,1) = 1.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0
  B(2,2) = 1.5d0; B(2,3) = 0.3d0
  B(3,3) = 2.0d0

  call DTGEX2(.FALSE., .FALSE., N, A, MAXN, B, MAXN, &
              Q, MAXN, Z, MAXN, J1, N1, N2, WORK, LWORK, INFO)
  call begin_test('swap_1x1_no_qz')
  call print_int('info', INFO)
  call print_matrix('A', A, MAXN, N, N)
  call print_matrix('B', B, MAXN, N, N)
  call end_test()

  ! ============================================
  ! Test 2: 1x1 swap with Q and Z
  ! ============================================
  N = 3; J1 = 1; N1 = 1; N2 = 1
  WANTQ = .TRUE.; WANTZ = .TRUE.

  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0

  A(1,1) = 1.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0
  A(2,2) = 2.0d0; A(2,3) = 0.4d0
  A(3,3) = 3.0d0

  B(1,1) = 1.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0
  B(2,2) = 1.5d0; B(2,3) = 0.3d0
  B(3,3) = 2.0d0

  ! Initialize Q and Z to identity
  Q(1,1) = 1.0d0; Q(2,2) = 1.0d0; Q(3,3) = 1.0d0
  Z(1,1) = 1.0d0; Z(2,2) = 1.0d0; Z(3,3) = 1.0d0

  call DTGEX2(.TRUE., .TRUE., N, A, MAXN, B, MAXN, &
              Q, MAXN, Z, MAXN, J1, N1, N2, WORK, LWORK, INFO)
  call begin_test('swap_1x1_with_qz')
  call print_int('info', INFO)
  call print_matrix('A', A, MAXN, N, N)
  call print_matrix('B', B, MAXN, N, N)
  call print_matrix('Q', Q, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call end_test()

  ! ============================================
  ! Test 3: 1x1 swap at J1=2 (last two diag entries)
  ! ============================================
  N = 3; J1 = 2; N1 = 1; N2 = 1
  WANTQ = .TRUE.; WANTZ = .TRUE.

  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0

  A(1,1) = 4.0d0; A(1,2) = 0.3d0; A(1,3) = 0.2d0
  A(2,2) = 1.0d0; A(2,3) = 0.6d0
  A(3,3) = 5.0d0

  B(1,1) = 2.0d0; B(1,2) = 0.1d0; B(1,3) = 0.3d0
  B(2,2) = 3.0d0; B(2,3) = 0.5d0
  B(3,3) = 1.0d0

  Q(1,1) = 1.0d0; Q(2,2) = 1.0d0; Q(3,3) = 1.0d0
  Z(1,1) = 1.0d0; Z(2,2) = 1.0d0; Z(3,3) = 1.0d0

  call DTGEX2(.TRUE., .TRUE., N, A, MAXN, B, MAXN, &
              Q, MAXN, Z, MAXN, J1, N1, N2, WORK, LWORK, INFO)
  call begin_test('swap_1x1_j1_2')
  call print_int('info', INFO)
  call print_matrix('A', A, MAXN, N, N)
  call print_matrix('B', B, MAXN, N, N)
  call print_matrix('Q', Q, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call end_test()

  ! ============================================
  ! Test 4: 2x2 with 1x1 swap (N1=2, N2=1)
  ! ============================================
  N = 4; J1 = 1; N1 = 2; N2 = 1
  WANTQ = .TRUE.; WANTZ = .TRUE.

  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0

  ! A: quasi-upper-triangular with 2x2 block at (1:2,1:2)
  A(1,1) = 1.0d0;  A(1,2) = 0.5d0;  A(1,3) = 0.3d0; A(1,4) = 0.1d0
  A(2,1) = -0.5d0; A(2,2) = 1.0d0;  A(2,3) = 0.2d0; A(2,4) = 0.15d0
  A(3,3) = 3.0d0;  A(3,4) = 0.4d0
  A(4,4) = 4.0d0

  B(1,1) = 2.0d0; B(1,2) = 0.3d0; B(1,3) = 0.1d0; B(1,4) = 0.05d0
  B(2,2) = 2.5d0; B(2,3) = 0.2d0; B(2,4) = 0.1d0
  B(3,3) = 3.0d0; B(3,4) = 0.3d0
  B(4,4) = 1.5d0

  ! Init Q,Z to identity (4x4)
  Q(1,1) = 1.0d0; Q(2,2) = 1.0d0; Q(3,3) = 1.0d0; Q(4,4) = 1.0d0
  Z(1,1) = 1.0d0; Z(2,2) = 1.0d0; Z(3,3) = 1.0d0; Z(4,4) = 1.0d0

  call DTGEX2(.TRUE., .TRUE., N, A, MAXN, B, MAXN, &
              Q, MAXN, Z, MAXN, J1, N1, N2, WORK, LWORK, INFO)
  call begin_test('swap_2x2_1x1')
  call print_int('info', INFO)
  call print_matrix('A', A, MAXN, N, N)
  call print_matrix('B', B, MAXN, N, N)
  call print_matrix('Q', Q, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call end_test()

  ! ============================================
  ! Test 5: 1x1 with 2x2 swap (N1=1, N2=2)
  ! ============================================
  N = 4; J1 = 1; N1 = 1; N2 = 2
  WANTQ = .TRUE.; WANTZ = .TRUE.

  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0

  ! 1x1 block followed by 2x2 block
  A(1,1) = 5.0d0;  A(1,2) = 0.3d0;  A(1,3) = 0.2d0; A(1,4) = 0.1d0
  A(2,2) = 1.0d0;  A(2,3) = 0.5d0;  A(2,4) = 0.15d0
  A(3,2) = -0.5d0; A(3,3) = 1.0d0;  A(3,4) = 0.2d0
  A(4,4) = 4.0d0

  B(1,1) = 2.0d0; B(1,2) = 0.1d0; B(1,3) = 0.05d0; B(1,4) = 0.02d0
  B(2,2) = 1.5d0; B(2,3) = 0.3d0;  B(2,4) = 0.1d0
  B(3,3) = 2.5d0; B(3,4) = 0.2d0
  B(4,4) = 3.0d0

  Q(1,1) = 1.0d0; Q(2,2) = 1.0d0; Q(3,3) = 1.0d0; Q(4,4) = 1.0d0
  Z(1,1) = 1.0d0; Z(2,2) = 1.0d0; Z(3,3) = 1.0d0; Z(4,4) = 1.0d0

  call DTGEX2(.TRUE., .TRUE., N, A, MAXN, B, MAXN, &
              Q, MAXN, Z, MAXN, J1, N1, N2, WORK, LWORK, INFO)
  call begin_test('swap_1x1_2x2')
  call print_int('info', INFO)
  call print_matrix('A', A, MAXN, N, N)
  call print_matrix('B', B, MAXN, N, N)
  call print_matrix('Q', Q, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call end_test()

  ! ============================================
  ! Test 6: 2x2 with 2x2 swap (N1=2, N2=2)
  ! ============================================
  N = 5; J1 = 1; N1 = 2; N2 = 2
  WANTQ = .TRUE.; WANTZ = .TRUE.

  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0

  ! First 2x2 block
  A(1,1) = 2.0d0;  A(1,2) = 1.0d0;  A(1,3) = 0.3d0; A(1,4) = 0.2d0; A(1,5) = 0.1d0
  A(2,1) = -1.0d0; A(2,2) = 2.0d0;  A(2,3) = 0.4d0; A(2,4) = 0.1d0; A(2,5) = 0.05d0
  ! Second 2x2 block
  A(3,3) = 5.0d0;  A(3,4) = 0.8d0;  A(3,5) = 0.3d0
  A(4,3) = -0.8d0; A(4,4) = 5.0d0;  A(4,5) = 0.2d0
  A(5,5) = 8.0d0

  B(1,1) = 1.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0; B(1,4) = 0.05d0; B(1,5) = 0.02d0
  B(2,2) = 1.5d0; B(2,3) = 0.3d0; B(2,4) = 0.1d0;  B(2,5) = 0.05d0
  B(3,3) = 2.0d0; B(3,4) = 0.2d0; B(3,5) = 0.1d0
  B(4,4) = 2.5d0; B(4,5) = 0.15d0
  B(5,5) = 3.0d0

  Q(1,1) = 1.0d0; Q(2,2) = 1.0d0; Q(3,3) = 1.0d0; Q(4,4) = 1.0d0; Q(5,5) = 1.0d0
  Z(1,1) = 1.0d0; Z(2,2) = 1.0d0; Z(3,3) = 1.0d0; Z(4,4) = 1.0d0; Z(5,5) = 1.0d0

  call DTGEX2(.TRUE., .TRUE., N, A, MAXN, B, MAXN, &
              Q, MAXN, Z, MAXN, J1, N1, N2, WORK, LWORK, INFO)
  call begin_test('swap_2x2_2x2')
  call print_int('info', INFO)
  call print_matrix('A', A, MAXN, N, N)
  call print_matrix('B', B, MAXN, N, N)
  call print_matrix('Q', Q, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call end_test()

  ! ============================================
  ! Test 7: 1x1 swap, larger matrix N=5, J1=2
  ! ============================================
  N = 5; J1 = 2; N1 = 1; N2 = 1
  WANTQ = .TRUE.; WANTZ = .TRUE.

  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0

  A(1,1) = 1.0d0; A(1,2) = 0.3d0; A(1,3) = 0.2d0; A(1,4) = 0.1d0; A(1,5) = 0.05d0
  A(2,2) = 2.0d0; A(2,3) = 0.5d0; A(2,4) = 0.3d0; A(2,5) = 0.1d0
  A(3,3) = 4.0d0; A(3,4) = 0.6d0; A(3,5) = 0.2d0
  A(4,4) = 5.0d0; A(4,5) = 0.4d0
  A(5,5) = 6.0d0

  B(1,1) = 1.0d0; B(1,2) = 0.1d0; B(1,3) = 0.05d0; B(1,4) = 0.02d0; B(1,5) = 0.01d0
  B(2,2) = 1.5d0; B(2,3) = 0.2d0;  B(2,4) = 0.1d0;  B(2,5) = 0.05d0
  B(3,3) = 2.0d0; B(3,4) = 0.3d0;  B(3,5) = 0.1d0
  B(4,4) = 2.5d0; B(4,5) = 0.2d0
  B(5,5) = 3.0d0

  Q(1,1) = 1.0d0; Q(2,2) = 1.0d0; Q(3,3) = 1.0d0; Q(4,4) = 1.0d0; Q(5,5) = 1.0d0
  Z(1,1) = 1.0d0; Z(2,2) = 1.0d0; Z(3,3) = 1.0d0; Z(4,4) = 1.0d0; Z(5,5) = 1.0d0

  call DTGEX2(.TRUE., .TRUE., N, A, MAXN, B, MAXN, &
              Q, MAXN, Z, MAXN, J1, N1, N2, WORK, LWORK, INFO)
  call begin_test('swap_1x1_n5_j2')
  call print_int('info', INFO)
  call print_matrix('A', A, MAXN, N, N)
  call print_matrix('B', B, MAXN, N, N)
  call print_matrix('Q', Q, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call end_test()

  ! ============================================
  ! Test 8: 1x1 swap where sb > sa (exercise sb >= sa branch)
  ! Set |A(1,1)|*|B(2,2)| > |A(2,2)|*|B(1,1)|
  ! ============================================
  N = 3; J1 = 1; N1 = 1; N2 = 1
  WANTQ = .TRUE.; WANTZ = .TRUE.

  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0

  A(1,1) = 5.0d0; A(1,2) = 0.3d0; A(1,3) = 0.2d0
  A(2,2) = 0.1d0; A(2,3) = 0.4d0
  A(3,3) = 3.0d0

  B(1,1) = 0.1d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0
  B(2,2) = 5.0d0; B(2,3) = 0.3d0
  B(3,3) = 2.0d0

  Q(1,1) = 1.0d0; Q(2,2) = 1.0d0; Q(3,3) = 1.0d0
  Z(1,1) = 1.0d0; Z(2,2) = 1.0d0; Z(3,3) = 1.0d0

  call DTGEX2(.TRUE., .TRUE., N, A, MAXN, B, MAXN, &
              Q, MAXN, Z, MAXN, J1, N1, N2, WORK, LWORK, INFO)
  call begin_test('swap_1x1_sb_gt_sa')
  call print_int('info', INFO)
  call print_matrix('A', A, MAXN, N, N)
  call print_matrix('B', B, MAXN, N, N)
  call print_matrix('Q', Q, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call end_test()

  ! ============================================
  ! Test 9: 2x2-with-1x1 swap at j1=2 (non-zero leading part)
  ! Use well-separated eigenvalues so swap succeeds
  ! ============================================
  N = 5; J1 = 2; N1 = 1; N2 = 2
  WANTQ = .TRUE.; WANTZ = .TRUE.

  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0

  ! 1x1 at pos 1, then 1x1 at pos 2, then 2x2 at (3:4,3:4), then 1x1 at pos 5
  A(1,1) = 10.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0; A(1,4) = 0.2d0; A(1,5) = 0.1d0
  A(2,2) = 5.0d0;  A(2,3) = 0.3d0; A(2,4) = 0.2d0; A(2,5) = 0.15d0
  A(3,3) = 1.0d0;  A(3,4) = 0.5d0; A(3,5) = 0.2d0
  A(4,3) = -0.5d0; A(4,4) = 1.0d0; A(4,5) = 0.1d0
  A(5,5) = 8.0d0

  B(1,1) = 1.0d0; B(1,2) = 0.1d0; B(1,3) = 0.05d0; B(1,4) = 0.02d0; B(1,5) = 0.01d0
  B(2,2) = 1.5d0; B(2,3) = 0.2d0;  B(2,4) = 0.1d0;  B(2,5) = 0.05d0
  B(3,3) = 2.0d0; B(3,4) = 0.3d0;  B(3,5) = 0.1d0
  B(4,4) = 2.5d0; B(4,5) = 0.15d0
  B(5,5) = 3.0d0

  Q(1,1) = 1.0d0; Q(2,2) = 1.0d0; Q(3,3) = 1.0d0; Q(4,4) = 1.0d0; Q(5,5) = 1.0d0
  Z(1,1) = 1.0d0; Z(2,2) = 1.0d0; Z(3,3) = 1.0d0; Z(4,4) = 1.0d0; Z(5,5) = 1.0d0

  call DTGEX2(.TRUE., .TRUE., N, A, MAXN, B, MAXN, &
              Q, MAXN, Z, MAXN, J1, N1, N2, WORK, LWORK, INFO)
  call begin_test('swap_1x1_2x2_j2')
  call print_int('info', INFO)
  call print_matrix('A', A, MAXN, N, N)
  call print_matrix('B', B, MAXN, N, N)
  call print_matrix('Q', Q, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call end_test()

end program
