program test_zlatdf
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4
  complex*16 :: Z(NMAX,NMAX), RHS(NMAX)
  double precision :: RDSUM, RDSCAL
  integer :: IPIV(NMAX), JPIV(NMAX), INFO, N

  ! NOTE: The reference ZLATDF has MAXDIM=2, which causes buffer overflows
  ! for N>2 (undersized RWORK for ZGECON). We call ZLATDF only for N<=2,
  ! and use a manually-inlined implementation with correct workspace for N>2.

  ! Large workspace for manual IJOB=2 implementation
  complex*16 :: WORK(4*NMAX), XM(NMAX), XP(NMAX)
  double precision :: RWORK(2*NMAX), RTEMP, SCALE_OUT
  complex*16 :: CONE, TEMP, ZDOTC
  double precision :: DZASUM

  ! Temp arrays for packing and printing via EQUIVALENCE
  complex*16 :: Rpk(NMAX)
  double precision :: Rpk_r(2*NMAX)
  equivalence (Rpk, Rpk_r)

  integer :: i

  CONE = (1.0d0, 0.0d0)

  ! ========================================
  ! Test 1: IJOB=2, 2x2 system (safe to call ZLATDF)
  ! ========================================
  N = 2
  Z(1,1) = (4.0d0, 1.0d0);  Z(1,2) = (3.0d0, -1.0d0)
  Z(2,1) = (2.0d0, 0.5d0);  Z(2,2) = (1.0d0, 2.0d0)
  call ZGETC2(N, Z, NMAX, IPIV, JPIV, INFO)
  RHS(1) = (1.0d0, 0.5d0);  RHS(2) = (-1.0d0, 1.0d0)
  RDSUM = 0.0d0; RDSCAL = 1.0d0
  call ZLATDF(2, N, Z, NMAX, RHS, RDSUM, RDSCAL, IPIV, JPIV)

  do i = 1, N
    Rpk(i) = RHS(i)
  end do
  call begin_test('ijob2_2x2')
  call print_array('rhs', Rpk_r, 2*N)
  call print_scalar('rdsum', RDSUM)
  call print_scalar('rdscal', RDSCAL)
  call end_test()

  ! ========================================
  ! Test 2: IJOB=5, 2x2 system (safe to call ZLATDF)
  ! ========================================
  N = 2
  Z(1,1) = (4.0d0, 1.0d0);  Z(1,2) = (3.0d0, -1.0d0)
  Z(2,1) = (2.0d0, 0.5d0);  Z(2,2) = (1.0d0, 2.0d0)
  call ZGETC2(N, Z, NMAX, IPIV, JPIV, INFO)
  RHS(1) = (1.0d0, 0.5d0);  RHS(2) = (-1.0d0, 1.0d0)
  RDSUM = 0.0d0; RDSCAL = 1.0d0
  call ZLATDF(5, N, Z, NMAX, RHS, RDSUM, RDSCAL, IPIV, JPIV)

  do i = 1, N
    Rpk(i) = RHS(i)
  end do
  call begin_test('ijob5_2x2')
  call print_array('rhs', Rpk_r, 2*N)
  call print_scalar('rdsum', RDSUM)
  call print_scalar('rdscal', RDSCAL)
  call end_test()

  ! ========================================
  ! Test 3: IJOB=2, 3x3 system (manual - correct workspace)
  ! ========================================
  N = 3
  Z(1,1) = (5.0d0, 1.0d0);  Z(1,2) = (7.0d0, -2.0d0);  Z(1,3) = (6.0d0, 0.5d0)
  Z(2,1) = (7.0d0, 0.0d0);  Z(2,2) = (10.0d0, 1.0d0);   Z(2,3) = (8.0d0, -1.0d0)
  Z(3,1) = (6.0d0, -1.0d0); Z(3,2) = (8.0d0, 0.5d0);    Z(3,3) = (10.0d0, 2.0d0)
  call ZGETC2(N, Z, NMAX, IPIV, JPIV, INFO)
  RHS(1) = (1.0d0, -0.5d0); RHS(2) = (-1.0d0, 1.0d0); RHS(3) = (0.5d0, 0.0d0)
  RDSUM = 1.0d0; RDSCAL = 1.0d0

  ! Manual IJOB=2 implementation with correct workspace
  call ZGECON('I', N, Z, NMAX, 1.0d0, RTEMP, WORK, RWORK, INFO)
  call ZCOPY(N, WORK(N+1), 1, XM, 1)
  call ZLASWP(1, XM, NMAX, 1, N-1, IPIV, -1)
  TEMP = CONE / SQRT(ZDOTC(N, XM, 1, XM, 1))
  call ZSCAL(N, TEMP, XM, 1)
  call ZCOPY(N, XM, 1, XP, 1)
  call ZAXPY(N, CONE, RHS, 1, XP, 1)
  call ZAXPY(N, -CONE, XM, 1, RHS, 1)
  call ZGESC2(N, Z, NMAX, RHS, IPIV, JPIV, SCALE_OUT)
  call ZGESC2(N, Z, NMAX, XP, IPIV, JPIV, SCALE_OUT)
  if (DZASUM(N, XP, 1) .GT. DZASUM(N, RHS, 1)) then
    call ZCOPY(N, XP, 1, RHS, 1)
  end if
  call ZLASSQ(N, RHS, 1, RDSCAL, RDSUM)

  do i = 1, N
    Rpk(i) = RHS(i)
  end do
  call begin_test('ijob2_3x3')
  call print_array('rhs', Rpk_r, 2*N)
  call print_scalar('rdsum', RDSUM)
  call print_scalar('rdscal', RDSCAL)
  call end_test()

  ! ========================================
  ! Test 4: IJOB=5, 3x3 system (safe - look-ahead path doesn't use RWORK)
  ! ========================================
  N = 3
  Z(1,1) = (5.0d0, 1.0d0);  Z(1,2) = (7.0d0, -2.0d0);  Z(1,3) = (6.0d0, 0.5d0)
  Z(2,1) = (7.0d0, 0.0d0);  Z(2,2) = (10.0d0, 1.0d0);   Z(2,3) = (8.0d0, -1.0d0)
  Z(3,1) = (6.0d0, -1.0d0); Z(3,2) = (8.0d0, 0.5d0);    Z(3,3) = (10.0d0, 2.0d0)
  call ZGETC2(N, Z, NMAX, IPIV, JPIV, INFO)
  RHS(1) = (1.0d0, -0.5d0); RHS(2) = (-1.0d0, 1.0d0); RHS(3) = (0.5d0, 0.0d0)
  RDSUM = 1.0d0; RDSCAL = 1.0d0
  call ZLATDF(5, N, Z, NMAX, RHS, RDSUM, RDSCAL, IPIV, JPIV)

  do i = 1, N
    Rpk(i) = RHS(i)
  end do
  call begin_test('ijob5_3x3')
  call print_array('rhs', Rpk_r, 2*N)
  call print_scalar('rdsum', RDSUM)
  call print_scalar('rdscal', RDSCAL)
  call end_test()

  ! ========================================
  ! Test 5: IJOB=2, 4x4 system (manual - correct workspace)
  ! ========================================
  N = 4
  Z(1,1) = (5.0d0, 1.0d0);  Z(1,2) = (7.0d0, -2.0d0)
  Z(1,3) = (6.0d0, 0.5d0);  Z(1,4) = (5.0d0, -1.0d0)
  Z(2,1) = (7.0d0, 0.0d0);  Z(2,2) = (10.0d0, 1.0d0)
  Z(2,3) = (8.0d0, -1.0d0); Z(2,4) = (7.0d0, 0.5d0)
  Z(3,1) = (6.0d0, -1.0d0); Z(3,2) = (8.0d0, 0.5d0)
  Z(3,3) = (10.0d0, 2.0d0); Z(3,4) = (9.0d0, -0.5d0)
  Z(4,1) = (5.0d0, 0.5d0);  Z(4,2) = (7.0d0, -1.0d0)
  Z(4,3) = (9.0d0, 1.0d0);  Z(4,4) = (10.0d0, 3.0d0)
  call ZGETC2(N, Z, NMAX, IPIV, JPIV, INFO)
  RHS(1) = (1.0d0, -0.5d0); RHS(2) = (-1.0d0, 1.0d0)
  RHS(3) = (2.0d0, 0.0d0);  RHS(4) = (-0.5d0, 0.5d0)
  RDSUM = 0.0d0; RDSCAL = 1.0d0

  ! Manual IJOB=2 implementation with correct workspace
  call ZGECON('I', N, Z, NMAX, 1.0d0, RTEMP, WORK, RWORK, INFO)
  call ZCOPY(N, WORK(N+1), 1, XM, 1)
  call ZLASWP(1, XM, NMAX, 1, N-1, IPIV, -1)
  TEMP = CONE / SQRT(ZDOTC(N, XM, 1, XM, 1))
  call ZSCAL(N, TEMP, XM, 1)
  call ZCOPY(N, XM, 1, XP, 1)
  call ZAXPY(N, CONE, RHS, 1, XP, 1)
  call ZAXPY(N, -CONE, XM, 1, RHS, 1)
  call ZGESC2(N, Z, NMAX, RHS, IPIV, JPIV, SCALE_OUT)
  call ZGESC2(N, Z, NMAX, XP, IPIV, JPIV, SCALE_OUT)
  if (DZASUM(N, XP, 1) .GT. DZASUM(N, RHS, 1)) then
    call ZCOPY(N, XP, 1, RHS, 1)
  end if
  call ZLASSQ(N, RHS, 1, RDSCAL, RDSUM)

  do i = 1, N
    Rpk(i) = RHS(i)
  end do
  call begin_test('ijob2_4x4')
  call print_array('rhs', Rpk_r, 2*N)
  call print_scalar('rdsum', RDSUM)
  call print_scalar('rdscal', RDSCAL)
  call end_test()

  ! ========================================
  ! Test 6: IJOB=5, 4x4 system (safe - look-ahead path doesn't use RWORK)
  ! ========================================
  N = 4
  Z(1,1) = (5.0d0, 1.0d0);  Z(1,2) = (7.0d0, -2.0d0)
  Z(1,3) = (6.0d0, 0.5d0);  Z(1,4) = (5.0d0, -1.0d0)
  Z(2,1) = (7.0d0, 0.0d0);  Z(2,2) = (10.0d0, 1.0d0)
  Z(2,3) = (8.0d0, -1.0d0); Z(2,4) = (7.0d0, 0.5d0)
  Z(3,1) = (6.0d0, -1.0d0); Z(3,2) = (8.0d0, 0.5d0)
  Z(3,3) = (10.0d0, 2.0d0); Z(3,4) = (9.0d0, -0.5d0)
  Z(4,1) = (5.0d0, 0.5d0);  Z(4,2) = (7.0d0, -1.0d0)
  Z(4,3) = (9.0d0, 1.0d0);  Z(4,4) = (10.0d0, 3.0d0)
  call ZGETC2(N, Z, NMAX, IPIV, JPIV, INFO)
  RHS(1) = (1.0d0, -0.5d0); RHS(2) = (-1.0d0, 1.0d0)
  RHS(3) = (2.0d0, 0.0d0);  RHS(4) = (-0.5d0, 0.5d0)
  RDSUM = 0.0d0; RDSCAL = 1.0d0
  call ZLATDF(5, N, Z, NMAX, RHS, RDSUM, RDSCAL, IPIV, JPIV)

  do i = 1, N
    Rpk(i) = RHS(i)
  end do
  call begin_test('ijob5_4x4')
  call print_array('rhs', Rpk_r, 2*N)
  call print_scalar('rdsum', RDSUM)
  call print_scalar('rdscal', RDSCAL)
  call end_test()

end program
