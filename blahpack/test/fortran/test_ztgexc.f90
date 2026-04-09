program test_ztgexc
  use test_utils
  implicit none

  integer, parameter :: MAXN = 4
  complex*16 :: A(MAXN, MAXN), B(MAXN, MAXN), Q(MAXN, MAXN), Z(MAXN, MAXN)
  complex*16 :: PACK(MAXN*MAXN)
  double precision :: PACK_r(2*MAXN*MAXN)
  equivalence (PACK, PACK_r)
  integer :: INFO, N, I, IFST, ILST

  ! ============================================================
  ! Test 1: Move forward (ifst=1 -> ilst=3) with Q and Z, N=4
  ! ============================================================
  N = 4
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.5d0);  A(1,2) = (0.3d0, 0.1d0);  A(1,3) = (0.1d0, -0.1d0); A(1,4) = (0.05d0, 0.02d0)
  A(2,2) = (2.0d0, -0.3d0); A(2,3) = (0.4d0, 0.3d0);  A(2,4) = (0.2d0, -0.1d0)
  A(3,3) = (3.0d0, 1.0d0);  A(3,4) = (0.6d0, 0.1d0)
  A(4,4) = (4.0d0, 0.8d0)
  B(1,1) = (1.0d0, 0.0d0);  B(1,2) = (0.1d0, 0.1d0);  B(1,3) = (0.05d0, 0.0d0);  B(1,4) = (0.02d0, -0.01d0)
  B(2,2) = (1.0d0, 0.0d0);  B(2,3) = (0.15d0, -0.05d0); B(2,4) = (0.08d0, 0.03d0)
  B(3,3) = (1.0d0, 0.0d0);  B(3,4) = (0.12d0, 0.04d0)
  B(4,4) = (1.0d0, 0.0d0)
  do I = 1, N
    Q(I,I) = (1.0d0, 0.0d0)
    Z(I,I) = (1.0d0, 0.0d0)
  end do
  IFST = 1; ILST = 3
  call ZTGEXC(.TRUE., .TRUE., N, A, MAXN, B, MAXN, Q, MAXN, Z, MAXN, IFST, ILST, INFO)
  call begin_test('move forward ifst=1 ilst=3 wantq=T wantz=T')
  call pack_and_print('A', A, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('B', B, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('Q', Q, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('Z', Z, MAXN, N, N, PACK, PACK_r)
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call end_test()

  ! ============================================================
  ! Test 2: Move backward (ifst=3 -> ilst=1) with Q and Z, N=4
  ! ============================================================
  N = 4
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.5d0);  A(1,2) = (0.3d0, 0.1d0);  A(1,3) = (0.1d0, -0.1d0); A(1,4) = (0.05d0, 0.02d0)
  A(2,2) = (2.0d0, -0.3d0); A(2,3) = (0.4d0, 0.3d0);  A(2,4) = (0.2d0, -0.1d0)
  A(3,3) = (3.0d0, 1.0d0);  A(3,4) = (0.6d0, 0.1d0)
  A(4,4) = (4.0d0, 0.8d0)
  B(1,1) = (1.0d0, 0.0d0);  B(1,2) = (0.1d0, 0.1d0);  B(1,3) = (0.05d0, 0.0d0);  B(1,4) = (0.02d0, -0.01d0)
  B(2,2) = (1.0d0, 0.0d0);  B(2,3) = (0.15d0, -0.05d0); B(2,4) = (0.08d0, 0.03d0)
  B(3,3) = (1.0d0, 0.0d0);  B(3,4) = (0.12d0, 0.04d0)
  B(4,4) = (1.0d0, 0.0d0)
  do I = 1, N
    Q(I,I) = (1.0d0, 0.0d0)
    Z(I,I) = (1.0d0, 0.0d0)
  end do
  IFST = 3; ILST = 1
  call ZTGEXC(.TRUE., .TRUE., N, A, MAXN, B, MAXN, Q, MAXN, Z, MAXN, IFST, ILST, INFO)
  call begin_test('move backward ifst=3 ilst=1 wantq=T wantz=T')
  call pack_and_print('A', A, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('B', B, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('Q', Q, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('Z', Z, MAXN, N, N, PACK, PACK_r)
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call end_test()

  ! ============================================================
  ! Test 3: Move forward without Q/Z (wantq=F, wantz=F)
  ! ============================================================
  N = 3
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 1.0d0);  A(1,2) = (0.5d0, -0.2d0); A(1,3) = (0.3d0, 0.1d0)
  A(2,2) = (4.0d0, 0.0d0);  A(2,3) = (0.7d0, -0.3d0)
  A(3,3) = (6.0d0, -1.0d0)
  B(1,1) = (1.0d0, 0.0d0);  B(1,2) = (0.2d0, 0.1d0);  B(1,3) = (0.1d0, -0.05d0)
  B(2,2) = (1.0d0, 0.0d0);  B(2,3) = (0.3d0, 0.2d0)
  B(3,3) = (1.0d0, 0.0d0)
  IFST = 1; ILST = 3
  call ZTGEXC(.FALSE., .FALSE., N, A, MAXN, B, MAXN, Q, MAXN, Z, MAXN, IFST, ILST, INFO)
  call begin_test('move forward ifst=1 ilst=3 wantq=F wantz=F')
  call pack_and_print('A', A, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('B', B, MAXN, N, N, PACK, PACK_r)
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call end_test()

  ! ============================================================
  ! Test 4: ifst == ilst (no-op), N=3
  ! ============================================================
  N = 3
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 1.0d0);  A(1,2) = (0.5d0, -0.2d0); A(1,3) = (0.3d0, 0.1d0)
  A(2,2) = (4.0d0, 0.0d0);  A(2,3) = (0.7d0, -0.3d0)
  A(3,3) = (6.0d0, -1.0d0)
  B(1,1) = (1.0d0, 0.0d0);  B(1,2) = (0.2d0, 0.1d0);  B(1,3) = (0.1d0, -0.05d0)
  B(2,2) = (1.0d0, 0.0d0);  B(2,3) = (0.3d0, 0.2d0)
  B(3,3) = (1.0d0, 0.0d0)
  IFST = 2; ILST = 2
  call ZTGEXC(.TRUE., .TRUE., N, A, MAXN, B, MAXN, Q, MAXN, Z, MAXN, IFST, ILST, INFO)
  call begin_test('no-op ifst=ilst=2')
  call pack_and_print('A', A, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('B', B, MAXN, N, N, PACK, PACK_r)
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call end_test()

  ! ============================================================
  ! Test 5: N=1 (trivial)
  ! ============================================================
  N = 1
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  A(1,1) = (5.0d0, 2.0d0)
  B(1,1) = (1.0d0, 0.0d0)
  Q(1,1) = (1.0d0, 0.0d0)
  Z(1,1) = (1.0d0, 0.0d0)
  IFST = 1; ILST = 1
  call ZTGEXC(.TRUE., .TRUE., N, A, MAXN, B, MAXN, Q, MAXN, Z, MAXN, IFST, ILST, INFO)
  call begin_test('N=1 trivial')
  call pack_and_print('A', A, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('B', B, MAXN, N, N, PACK, PACK_r)
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call end_test()

  ! ============================================================
  ! Test 6: Move last to first (ifst=4 -> ilst=1), N=4
  ! ============================================================
  N = 4
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.5d0);  A(1,2) = (0.3d0, 0.1d0);  A(1,3) = (0.1d0, -0.1d0); A(1,4) = (0.05d0, 0.02d0)
  A(2,2) = (2.0d0, -0.3d0); A(2,3) = (0.4d0, 0.3d0);  A(2,4) = (0.2d0, -0.1d0)
  A(3,3) = (3.0d0, 1.0d0);  A(3,4) = (0.6d0, 0.1d0)
  A(4,4) = (4.0d0, 0.8d0)
  B(1,1) = (1.0d0, 0.0d0);  B(1,2) = (0.1d0, 0.1d0);  B(1,3) = (0.05d0, 0.0d0);  B(1,4) = (0.02d0, -0.01d0)
  B(2,2) = (1.0d0, 0.0d0);  B(2,3) = (0.15d0, -0.05d0); B(2,4) = (0.08d0, 0.03d0)
  B(3,3) = (1.0d0, 0.0d0);  B(3,4) = (0.12d0, 0.04d0)
  B(4,4) = (1.0d0, 0.0d0)
  do I = 1, N
    Q(I,I) = (1.0d0, 0.0d0)
    Z(I,I) = (1.0d0, 0.0d0)
  end do
  IFST = 4; ILST = 1
  call ZTGEXC(.TRUE., .TRUE., N, A, MAXN, B, MAXN, Q, MAXN, Z, MAXN, IFST, ILST, INFO)
  call begin_test('move last to first ifst=4 ilst=1 wantq=T wantz=T')
  call pack_and_print('A', A, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('B', B, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('Q', Q, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('Z', Z, MAXN, N, N, PACK, PACK_r)
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call end_test()

  ! ============================================================
  ! Test 7: Move first to last (ifst=1 -> ilst=4), N=4
  ! ============================================================
  N = 4
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.5d0);  A(1,2) = (0.3d0, 0.1d0);  A(1,3) = (0.1d0, -0.1d0); A(1,4) = (0.05d0, 0.02d0)
  A(2,2) = (2.0d0, -0.3d0); A(2,3) = (0.4d0, 0.3d0);  A(2,4) = (0.2d0, -0.1d0)
  A(3,3) = (3.0d0, 1.0d0);  A(3,4) = (0.6d0, 0.1d0)
  A(4,4) = (4.0d0, 0.8d0)
  B(1,1) = (1.0d0, 0.0d0);  B(1,2) = (0.1d0, 0.1d0);  B(1,3) = (0.05d0, 0.0d0);  B(1,4) = (0.02d0, -0.01d0)
  B(2,2) = (1.0d0, 0.0d0);  B(2,3) = (0.15d0, -0.05d0); B(2,4) = (0.08d0, 0.03d0)
  B(3,3) = (1.0d0, 0.0d0);  B(3,4) = (0.12d0, 0.04d0)
  B(4,4) = (1.0d0, 0.0d0)
  do I = 1, N
    Q(I,I) = (1.0d0, 0.0d0)
    Z(I,I) = (1.0d0, 0.0d0)
  end do
  IFST = 1; ILST = 4
  call ZTGEXC(.TRUE., .TRUE., N, A, MAXN, B, MAXN, Q, MAXN, Z, MAXN, IFST, ILST, INFO)
  call begin_test('move first to last ifst=1 ilst=4 wantq=T wantz=T')
  call pack_and_print('A', A, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('B', B, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('Q', Q, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('Z', Z, MAXN, N, N, PACK, PACK_r)
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call end_test()

  ! ============================================================
  ! Test 8: N=2 move backward (ifst=2 -> ilst=1)
  ! ============================================================
  N = 2
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.5d0);  A(1,2) = (0.3d0, 0.1d0)
  A(2,2) = (2.0d0, -0.3d0)
  B(1,1) = (1.0d0, 0.0d0);  B(1,2) = (0.2d0, -0.1d0)
  B(2,2) = (0.5d0, 0.2d0)
  do I = 1, N
    Q(I,I) = (1.0d0, 0.0d0)
    Z(I,I) = (1.0d0, 0.0d0)
  end do
  IFST = 2; ILST = 1
  call ZTGEXC(.TRUE., .TRUE., N, A, MAXN, B, MAXN, Q, MAXN, Z, MAXN, IFST, ILST, INFO)
  call begin_test('N=2 move backward ifst=2 ilst=1')
  call pack_and_print('A', A, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('B', B, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('Q', Q, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('Z', Z, MAXN, N, N, PACK, PACK_r)
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
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

end program
