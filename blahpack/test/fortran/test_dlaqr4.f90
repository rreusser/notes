program test_dlaqr4
  use test_utils
  implicit none

  ! Max sizes
  integer, parameter :: MAXN = 15
  double precision :: H(MAXN, MAXN), Z(MAXN, MAXN)
  double precision :: WR(MAXN), WI(MAXN)
  double precision :: WORK(10000)
  integer :: INFO, N, I, J, LWORK

  LWORK = 10000

  ! ============================================================
  ! Test 1: 6x6 Hessenberg (small path via dlahqr), WANTT=T, WANTZ=T
  ! ============================================================
  N = 6
  H = 0.0d0; Z = 0.0d0; WR = 0.0d0; WI = 0.0d0
  ! Upper Hessenberg with subdiagonal
  H(1,1) = 4.0d0; H(1,2) = 1.0d0; H(1,3) = 0.5d0; H(1,4) = 0.2d0; H(1,5) = 0.1d0; H(1,6) = 0.05d0
  H(2,1) = 1.0d0; H(2,2) = 3.0d0; H(2,3) = 1.0d0; H(2,4) = 0.3d0; H(2,5) = 0.2d0; H(2,6) = 0.1d0
  H(3,3) = 2.0d0; H(3,4) = 1.0d0; H(3,5) = 0.4d0; H(3,6) = 0.2d0
  H(3,2) = 0.8d0
  H(4,4) = 1.0d0; H(4,5) = 0.5d0; H(4,6) = 0.3d0
  H(4,3) = 0.6d0
  H(5,5) = -1.0d0; H(5,6) = 1.0d0
  H(5,4) = 0.4d0
  H(6,6) = -2.0d0
  H(6,5) = 0.3d0
  ! Initialize Z = I
  do I = 1, N
    Z(I, I) = 1.0d0
  end do
  call DLAQR4(.TRUE., .TRUE., N, 1, N, H, MAXN, WR, WI, 1, N, Z, MAXN, WORK, LWORK, INFO)
  call begin_test('6x6 wantt wantz')
  call print_matrix('H', H, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call print_array('WR', WR, N)
  call print_array('WI', WI, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 2: 6x6 WANTT=T, WANTZ=F (eigenvalues only, no Schur vectors)
  ! ============================================================
  N = 6
  H = 0.0d0; Z = 0.0d0; WR = 0.0d0; WI = 0.0d0
  H(1,1) = 4.0d0; H(1,2) = 1.0d0; H(1,3) = 0.5d0; H(1,4) = 0.2d0; H(1,5) = 0.1d0; H(1,6) = 0.05d0
  H(2,1) = 1.0d0; H(2,2) = 3.0d0; H(2,3) = 1.0d0; H(2,4) = 0.3d0; H(2,5) = 0.2d0; H(2,6) = 0.1d0
  H(3,3) = 2.0d0; H(3,4) = 1.0d0; H(3,5) = 0.4d0; H(3,6) = 0.2d0
  H(3,2) = 0.8d0
  H(4,4) = 1.0d0; H(4,5) = 0.5d0; H(4,6) = 0.3d0
  H(4,3) = 0.6d0
  H(5,5) = -1.0d0; H(5,6) = 1.0d0
  H(5,4) = 0.4d0
  H(6,6) = -2.0d0
  H(6,5) = 0.3d0
  call DLAQR4(.TRUE., .FALSE., N, 1, N, H, MAXN, WR, WI, 1, N, Z, MAXN, WORK, LWORK, INFO)
  call begin_test('6x6 wantt no wantz')
  call print_matrix('H', H, MAXN, N, N)
  call print_array('WR', WR, N)
  call print_array('WI', WI, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 3: 6x6 WANTT=F, WANTZ=F (eigenvalues only)
  ! ============================================================
  N = 6
  H = 0.0d0; Z = 0.0d0; WR = 0.0d0; WI = 0.0d0
  H(1,1) = 4.0d0; H(1,2) = 1.0d0; H(1,3) = 0.5d0; H(1,4) = 0.2d0; H(1,5) = 0.1d0; H(1,6) = 0.05d0
  H(2,1) = 1.0d0; H(2,2) = 3.0d0; H(2,3) = 1.0d0; H(2,4) = 0.3d0; H(2,5) = 0.2d0; H(2,6) = 0.1d0
  H(3,3) = 2.0d0; H(3,4) = 1.0d0; H(3,5) = 0.4d0; H(3,6) = 0.2d0
  H(3,2) = 0.8d0
  H(4,4) = 1.0d0; H(4,5) = 0.5d0; H(4,6) = 0.3d0
  H(4,3) = 0.6d0
  H(5,5) = -1.0d0; H(5,6) = 1.0d0
  H(5,4) = 0.4d0
  H(6,6) = -2.0d0
  H(6,5) = 0.3d0
  call DLAQR4(.FALSE., .FALSE., N, 1, N, H, MAXN, WR, WI, 1, N, Z, MAXN, WORK, LWORK, INFO)
  call begin_test('6x6 no wantt no wantz')
  call print_array('WR', WR, N)
  call print_array('WI', WI, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 4: 15x15 Hessenberg (multishift path), WANTT=T, WANTZ=T
  ! ============================================================
  N = 15
  H = 0.0d0; Z = 0.0d0; WR = 0.0d0; WI = 0.0d0
  ! Build a diagonally dominant Hessenberg matrix with known structure
  do I = 1, N
    H(I, I) = dble(N + 1 - I) * 1.0d0
    if (I < N) then
      H(I+1, I) = 1.0d0  ! subdiagonal
    end if
    do J = I+1, N
      H(I, J) = 0.5d0 / dble(J - I)
    end do
  end do
  ! Initialize Z = I
  do I = 1, N
    Z(I, I) = 1.0d0
  end do
  call DLAQR4(.TRUE., .TRUE., N, 1, N, H, MAXN, WR, WI, 1, N, Z, MAXN, WORK, LWORK, INFO)
  call begin_test('15x15 wantt wantz')
  call print_matrix('H', H, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call print_array('WR', WR, N)
  call print_array('WI', WI, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 5: N=0 quick return
  ! ============================================================
  call DLAQR4(.TRUE., .TRUE., 0, 1, 0, H, MAXN, WR, WI, 1, 0, Z, MAXN, WORK, LWORK, INFO)
  call begin_test('N=0')
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 6: N=1 trivial
  ! ============================================================
  N = 1
  H = 0.0d0; Z = 0.0d0; WR = 0.0d0; WI = 0.0d0
  H(1,1) = 7.0d0
  Z(1,1) = 1.0d0
  call DLAQR4(.TRUE., .TRUE., N, 1, N, H, MAXN, WR, WI, 1, N, Z, MAXN, WORK, LWORK, INFO)
  call begin_test('N=1')
  call print_scalar('WR1', WR(1))
  call print_scalar('WI1', WI(1))
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 7: Partial range ILO=2, IHI=5 on 6x6
  ! ============================================================
  N = 6
  H = 0.0d0; Z = 0.0d0; WR = 0.0d0; WI = 0.0d0
  ! Build a Hessenberg matrix that is already reduced outside [2,5]
  H(1,1) = 10.0d0
  H(1,2) = 0.5d0; H(1,3) = 0.3d0; H(1,4) = 0.2d0; H(1,5) = 0.1d0; H(1,6) = 0.05d0
  ! H(2,1) = 0 (already reduced)
  H(2,2) = 4.0d0; H(2,3) = 1.0d0; H(2,4) = 0.3d0; H(2,5) = 0.2d0; H(2,6) = 0.1d0
  H(3,2) = 0.8d0; H(3,3) = 3.0d0; H(3,4) = 0.5d0; H(3,5) = 0.3d0; H(3,6) = 0.15d0
  H(4,3) = 0.6d0; H(4,4) = 2.0d0; H(4,5) = 0.4d0; H(4,6) = 0.2d0
  H(5,4) = 0.4d0; H(5,5) = 1.0d0; H(5,6) = 0.3d0
  ! H(6,5) = 0 (already reduced)
  H(6,6) = -5.0d0
  do I = 1, N
    Z(I, I) = 1.0d0
  end do
  call DLAQR4(.TRUE., .TRUE., N, 2, 5, H, MAXN, WR, WI, 1, N, Z, MAXN, WORK, LWORK, INFO)
  call begin_test('6x6 partial ilo=2 ihi=5')
  call print_matrix('H', H, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call print_array('WR', WR, N)
  call print_array('WI', WI, N)
  call print_int('info', INFO)
  call end_test()

end program
