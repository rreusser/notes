program test_ztgex2
  use test_utils
  implicit none

  integer, parameter :: MAXN = 4
  complex*16 :: A(MAXN, MAXN), B(MAXN, MAXN), Q(MAXN, MAXN), Z(MAXN, MAXN)
  complex*16 :: PACK(MAXN*MAXN)
  double precision :: PACK_r(2*MAXN*MAXN)
  equivalence (PACK, PACK_r)
  integer :: INFO, N, I, J, K

  ! ============================================================
  ! Test 1: Basic 2x2 swap with WANTQ=T, WANTZ=T
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
  call ZTGEX2(.TRUE., .TRUE., N, A, MAXN, B, MAXN, Q, MAXN, Z, MAXN, 1, INFO)
  call begin_test('basic 2x2 swap wantq=T wantz=T')
  call pack_and_print('A', A, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('B', B, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('Q', Q, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('Z', Z, MAXN, N, N, PACK, PACK_r)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 2: 4x4 swap at position 2 with WANTQ=T, WANTZ=T
  ! ============================================================
  N = 4
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  A(1,1) = (3.0d0, 1.0d0);  A(1,2) = (0.5d0, 0.2d0);  A(1,3) = (0.1d0, -0.1d0); A(1,4) = (0.05d0, 0.02d0)
  A(2,2) = (1.0d0, -0.5d0); A(2,3) = (0.4d0, 0.3d0);  A(2,4) = (0.2d0, -0.1d0)
  A(3,3) = (5.0d0, 0.0d0);  A(3,4) = (0.6d0, 0.1d0)
  A(4,4) = (2.0d0, 0.8d0)
  B(1,1) = (1.0d0, 0.0d0);  B(1,2) = (0.1d0, 0.1d0);  B(1,3) = (0.05d0, 0.0d0);  B(1,4) = (0.02d0, -0.01d0)
  B(2,2) = (1.0d0, 0.0d0);  B(2,3) = (0.15d0, -0.05d0); B(2,4) = (0.08d0, 0.03d0)
  B(3,3) = (1.0d0, 0.0d0);  B(3,4) = (0.12d0, 0.04d0)
  B(4,4) = (1.0d0, 0.0d0)
  do I = 1, N
    Q(I,I) = (1.0d0, 0.0d0)
    Z(I,I) = (1.0d0, 0.0d0)
  end do
  call ZTGEX2(.TRUE., .TRUE., N, A, MAXN, B, MAXN, Q, MAXN, Z, MAXN, 2, INFO)
  call begin_test('4x4 swap at j1=2 wantq=T wantz=T')
  call pack_and_print('A', A, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('B', B, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('Q', Q, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('Z', Z, MAXN, N, N, PACK, PACK_r)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 3: 4x4 swap at position 3 with WANTQ=T, WANTZ=T
  ! ============================================================
  N = 4
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  A(1,1) = (3.0d0, 1.0d0);  A(1,2) = (0.5d0, 0.2d0);  A(1,3) = (0.1d0, -0.1d0); A(1,4) = (0.05d0, 0.02d0)
  A(2,2) = (1.0d0, -0.5d0); A(2,3) = (0.4d0, 0.3d0);  A(2,4) = (0.2d0, -0.1d0)
  A(3,3) = (5.0d0, 0.0d0);  A(3,4) = (0.6d0, 0.1d0)
  A(4,4) = (2.0d0, 0.8d0)
  B(1,1) = (1.0d0, 0.0d0);  B(1,2) = (0.1d0, 0.1d0);  B(1,3) = (0.05d0, 0.0d0);  B(1,4) = (0.02d0, -0.01d0)
  B(2,2) = (1.0d0, 0.0d0);  B(2,3) = (0.15d0, -0.05d0); B(2,4) = (0.08d0, 0.03d0)
  B(3,3) = (1.0d0, 0.0d0);  B(3,4) = (0.12d0, 0.04d0)
  B(4,4) = (1.0d0, 0.0d0)
  do I = 1, N
    Q(I,I) = (1.0d0, 0.0d0)
    Z(I,I) = (1.0d0, 0.0d0)
  end do
  call ZTGEX2(.TRUE., .TRUE., N, A, MAXN, B, MAXN, Q, MAXN, Z, MAXN, 3, INFO)
  call begin_test('4x4 swap at j1=3 wantq=T wantz=T')
  call pack_and_print('A', A, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('B', B, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('Q', Q, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('Z', Z, MAXN, N, N, PACK, PACK_r)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 4: WANTQ=F, WANTZ=F
  ! ============================================================
  N = 3
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 1.0d0);  A(1,2) = (0.5d0, -0.2d0); A(1,3) = (0.1d0, 0.1d0)
  A(2,2) = (4.0d0, -1.0d0); A(2,3) = (0.3d0, 0.4d0)
  A(3,3) = (1.0d0, 0.0d0)
  B(1,1) = (1.0d0, 0.0d0);  B(1,2) = (0.2d0, 0.1d0);  B(1,3) = (0.05d0, -0.02d0)
  B(2,2) = (1.0d0, 0.0d0);  B(2,3) = (0.1d0, 0.05d0)
  B(3,3) = (1.0d0, 0.0d0)
  call ZTGEX2(.FALSE., .FALSE., N, A, MAXN, B, MAXN, Q, MAXN, Z, MAXN, 1, INFO)
  call begin_test('3x3 swap at j1=1 wantq=F wantz=F')
  call pack_and_print('A', A, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('B', B, MAXN, N, N, PACK, PACK_r)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 5: N=1 quick return
  ! ============================================================
  N = 1
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  A(1,1) = (5.0d0, 1.0d0)
  B(1,1) = (1.0d0, 0.0d0)
  call ZTGEX2(.TRUE., .TRUE., N, A, MAXN, B, MAXN, Q, MAXN, Z, MAXN, 1, INFO)
  call begin_test('N=1 quick return')
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 6: 4x4 swap at position 1 (first two eigenvalues)
  ! ============================================================
  N = 4
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 2.0d0);  A(1,2) = (0.7d0, -0.3d0); A(1,3) = (0.2d0, 0.1d0);  A(1,4) = (0.15d0, -0.05d0)
  A(2,2) = (3.0d0, -1.0d0); A(2,3) = (0.5d0, 0.4d0);  A(2,4) = (0.25d0, 0.1d0)
  A(3,3) = (5.0d0, 0.5d0);  A(3,4) = (0.8d0, -0.2d0)
  A(4,4) = (7.0d0, 0.0d0)
  B(1,1) = (2.0d0, 0.5d0);  B(1,2) = (0.3d0, 0.2d0);  B(1,3) = (0.1d0, -0.05d0); B(1,4) = (0.04d0, 0.01d0)
  B(2,2) = (1.5d0, -0.3d0); B(2,3) = (0.2d0, 0.1d0);  B(2,4) = (0.06d0, -0.02d0)
  B(3,3) = (3.0d0, 0.0d0);  B(3,4) = (0.15d0, 0.08d0)
  B(4,4) = (0.5d0, 0.1d0)
  do I = 1, N
    Q(I,I) = (1.0d0, 0.0d0)
    Z(I,I) = (1.0d0, 0.0d0)
  end do
  call ZTGEX2(.TRUE., .TRUE., N, A, MAXN, B, MAXN, Q, MAXN, Z, MAXN, 1, INFO)
  call begin_test('4x4 swap at j1=1 wantq=T wantz=T')
  call pack_and_print('A', A, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('B', B, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('Q', Q, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('Z', Z, MAXN, N, N, PACK, PACK_r)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 7: WANTQ=T, WANTZ=F
  ! ============================================================
  N = 3
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 1.0d0);  A(1,2) = (0.5d0, -0.2d0); A(1,3) = (0.1d0, 0.1d0)
  A(2,2) = (4.0d0, -1.0d0); A(2,3) = (0.3d0, 0.4d0)
  A(3,3) = (1.0d0, 0.0d0)
  B(1,1) = (1.0d0, 0.0d0);  B(1,2) = (0.2d0, 0.1d0);  B(1,3) = (0.05d0, -0.02d0)
  B(2,2) = (1.0d0, 0.0d0);  B(2,3) = (0.1d0, 0.05d0)
  B(3,3) = (1.0d0, 0.0d0)
  do I = 1, N
    Q(I,I) = (1.0d0, 0.0d0)
    Z(I,I) = (1.0d0, 0.0d0)
  end do
  call ZTGEX2(.TRUE., .FALSE., N, A, MAXN, B, MAXN, Q, MAXN, Z, MAXN, 2, INFO)
  call begin_test('3x3 swap at j1=2 wantq=T wantz=F')
  call pack_and_print('A', A, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('B', B, MAXN, N, N, PACK, PACK_r)
  call pack_and_print('Q', Q, MAXN, N, N, PACK, PACK_r)
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

end program
