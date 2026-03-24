program test_dlaqr0
  use test_utils
  implicit none

  integer, parameter :: MAXN = 20
  double precision :: H(MAXN, MAXN), Z(MAXN, MAXN)
  double precision :: WR(MAXN), WI(MAXN), WORK(10*MAXN)
  integer :: N, ILO, IHI, INFO, LDH, LDZ, LWORK, i, j
  logical :: WANTT, WANTZ

  LDH = MAXN
  LDZ = MAXN
  LWORK = 10*MAXN

  ! ==== Test 1: N=0, trivial case ====
  call begin_test('n_eq_0')
  N = 0
  ILO = 1
  IHI = 0
  call DLAQR0(.TRUE., .TRUE., N, ILO, IHI, H, LDH, WR, WI, &
              1, 0, Z, LDZ, WORK, LWORK, INFO)
  call print_int('info', INFO)
  call end_test()

  ! ==== Test 2: N=1, single element ====
  call begin_test('n_eq_1')
  N = 1
  ILO = 1
  IHI = 1
  H = 0.0d0
  Z = 0.0d0
  H(1,1) = 3.5d0
  Z(1,1) = 1.0d0
  call DLAQR0(.TRUE., .TRUE., N, ILO, IHI, H, LDH, WR, WI, &
              1, 1, Z, LDZ, WORK, LWORK, INFO)
  call print_int('info', INFO)
  call print_scalar('wr1', WR(1))
  call print_scalar('wi1', WI(1))
  call print_scalar('h11', H(1,1))
  call end_test()

  ! ==== Test 3: N=2, 2x2 Hessenberg ====
  call begin_test('n_eq_2')
  N = 2
  ILO = 1
  IHI = 2
  H = 0.0d0
  Z = 0.0d0
  H(1,1) = 1.0d0
  H(1,2) = 2.0d0
  H(2,1) = 3.0d0
  H(2,2) = 4.0d0
  Z(1,1) = 1.0d0
  Z(2,2) = 1.0d0
  call DLAQR0(.TRUE., .TRUE., N, ILO, IHI, H, LDH, WR, WI, &
              1, 2, Z, LDZ, WORK, LWORK, INFO)
  call print_int('info', INFO)
  call print_array('wr', WR, 2)
  call print_array('wi', WI, 2)
  call print_array('H', H, 2*2)
  call print_array('Z', Z, 2*2)
  call end_test()

  ! ==== Test 4: 6x6 upper Hessenberg (small, uses DLAHQR) ====
  call begin_test('hess_6x6')
  N = 6
  ILO = 1
  IHI = 6
  H = 0.0d0
  Z = 0.0d0
  ! Build a well-conditioned upper Hessenberg matrix
  do i = 1, N
    do j = 1, N
      if (j >= i-1) then
        H(i,j) = 1.0d0 / dble(i + j)
      end if
    end do
    Z(i,i) = 1.0d0
  end do
  ! Make subdiagonal entries more significant
  do i = 2, N
    H(i,i-1) = 0.5d0
  end do
  call DLAQR0(.TRUE., .TRUE., N, ILO, IHI, H, LDH, WR, WI, &
              1, 6, Z, LDZ, WORK, LWORK, INFO)
  call print_int('info', INFO)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  ! Print upper triangle of Schur form
  call print_array('H', H, N*N)
  call print_array('Z', Z, N*N)
  call end_test()

  ! ==== Test 5: 6x6 eigenvalues only (WANTT=false, WANTZ=false) ====
  call begin_test('hess_6x6_eigonly')
  N = 6
  ILO = 1
  IHI = 6
  H = 0.0d0
  Z = 0.0d0
  do i = 1, N
    do j = 1, N
      if (j >= i-1) then
        H(i,j) = 1.0d0 / dble(i + j)
      end if
    end do
  end do
  do i = 2, N
    H(i,i-1) = 0.5d0
  end do
  call DLAQR0(.FALSE., .FALSE., N, ILO, IHI, H, LDH, WR, WI, &
              1, 1, Z, LDZ, WORK, LWORK, INFO)
  call print_int('info', INFO)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call end_test()

  ! ==== Test 6: ILO=IHI (single active element) ====
  call begin_test('ilo_eq_ihi')
  N = 4
  ILO = 2
  IHI = 2
  H = 0.0d0
  Z = 0.0d0
  ! Set up a matrix that is already triangular except at (2,2)
  H(1,1) = 1.0d0
  H(2,2) = 2.0d0
  H(3,3) = 3.0d0
  H(4,4) = 4.0d0
  H(1,2) = 0.5d0
  H(2,3) = 0.5d0
  H(3,4) = 0.5d0
  do i = 1, N
    Z(i,i) = 1.0d0
  end do
  call DLAQR0(.TRUE., .TRUE., N, ILO, IHI, H, LDH, WR, WI, &
              1, 4, Z, LDZ, WORK, LWORK, INFO)
  call print_int('info', INFO)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call end_test()

  ! ==== Test 7: 15x15 matrix (boundary: uses DLAHQR path) ====
  call begin_test('hess_15x15')
  N = 15
  ILO = 1
  IHI = 15
  H = 0.0d0
  Z = 0.0d0
  ! Build a diagonally dominant upper Hessenberg matrix
  do i = 1, N
    H(i,i) = dble(i) * 2.0d0
    if (i < N) then
      H(i,i+1) = 1.0d0
    end if
    if (i > 1) then
      H(i,i-1) = 0.3d0
    end if
    do j = i+2, N
      H(i,j) = 0.1d0 / dble(j - i)
    end do
    Z(i,i) = 1.0d0
  end do
  call DLAQR0(.TRUE., .TRUE., N, ILO, IHI, H, LDH, WR, WI, &
              1, 15, Z, LDZ, WORK, LWORK, INFO)
  call print_int('info', INFO)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call end_test()

  ! ==== Test 8: 16x16 matrix (above NTINY, exercises multishift path) ====
  call begin_test('hess_16x16')
  N = 16
  ILO = 1
  IHI = 16
  H = 0.0d0
  Z = 0.0d0
  ! Build a well-conditioned upper Hessenberg with known structure
  do i = 1, N
    H(i,i) = dble(i) * 3.0d0
    if (i < N) then
      H(i,i+1) = 2.0d0
    end if
    if (i > 1) then
      H(i,i-1) = 1.0d0
    end if
    do j = i+2, N
      H(i,j) = 0.05d0 / dble(j - i)
    end do
    Z(i,i) = 1.0d0
  end do
  call DLAQR0(.TRUE., .TRUE., N, ILO, IHI, H, LDH, WR, WI, &
              1, 16, Z, LDZ, WORK, LWORK, INFO)
  call print_int('info', INFO)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call end_test()

  ! ==== Test 9: 20x20 matrix (larger multishift test) ====
  call begin_test('hess_20x20')
  N = 20
  ILO = 1
  IHI = 20
  H = 0.0d0
  Z = 0.0d0
  do i = 1, N
    H(i,i) = dble(i) * 2.5d0 + 0.1d0 * dble(mod(i*7, 11))
    if (i < N) then
      H(i,i+1) = 1.5d0
    end if
    if (i > 1) then
      H(i,i-1) = 0.8d0
    end if
    do j = i+2, N
      H(i,j) = 0.02d0 / dble(j - i)
    end do
    Z(i,i) = 1.0d0
  end do
  call DLAQR0(.TRUE., .TRUE., N, ILO, IHI, H, LDH, WR, WI, &
              1, 20, Z, LDZ, WORK, LWORK, INFO)
  call print_int('info', INFO)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call end_test()

  ! ==== Test 10: Partial active block (ILO=3, IHI=8, N=10) ====
  call begin_test('partial_block')
  N = 10
  ILO = 3
  IHI = 8
  H = 0.0d0
  Z = 0.0d0
  ! Already-converged rows 1-2 and 9-10
  H(1,1) = 10.0d0
  H(2,2) = 20.0d0
  H(9,9) = 90.0d0
  H(10,10) = 100.0d0
  H(1,2) = 1.0d0
  H(9,10) = 1.0d0
  ! Active block rows 3-8: upper Hessenberg
  do i = 3, 8
    H(i,i) = dble(i) * 5.0d0
    if (i < 8) then
      H(i,i+1) = 2.0d0
    end if
    if (i > 3) then
      H(i,i-1) = 1.0d0
    end if
  end do
  ! Fill some upper triangle in active block
  H(3,5) = 0.3d0
  H(4,6) = 0.2d0
  H(5,7) = 0.1d0
  ! Connect active block to already-converged
  H(2,3) = 0.5d0
  H(8,9) = 0.5d0
  do i = 1, N
    Z(i,i) = 1.0d0
  end do
  call DLAQR0(.TRUE., .TRUE., N, ILO, IHI, H, LDH, WR, WI, &
              1, 10, Z, LDZ, WORK, LWORK, INFO)
  call print_int('info', INFO)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call end_test()

end program
