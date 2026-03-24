program test_zlaqr5
  use test_utils
  implicit none

  integer, parameter :: NMAX = 10
  integer, parameter :: NSMAX = 6
  complex*16 :: H(NMAX, NMAX), Z(NMAX, NMAX), S(NSMAX)
  complex*16 :: V(3, NSMAX/2), U(2*NSMAX+1, 2*NSMAX+1)
  complex*16 :: WV(NMAX, 2*NSMAX+1), WH(2*NSMAX+1, NMAX)
  double precision :: S_r(2*NSMAX)
  equivalence (S, S_r)
  ! Packed output arrays (n*n) for printing without LDH padding
  complex*16 :: Hpk(NMAX*NMAX), Zpk(NMAX*NMAX)
  double precision :: Hpk_r(2*NMAX*NMAX), Zpk_r(2*NMAX*NMAX)
  equivalence (Hpk, Hpk_r)
  equivalence (Zpk, Zpk_r)
  integer :: n, ktop, kbot, nshfts, kacc22
  integer :: iloz, ihiz, ldh, ldz, ldv, ldu, ldwv, ldwh, nh, nv
  integer :: i, j

  ldh = NMAX
  ldz = NMAX
  ldv = 3
  ldu = 2*NSMAX+1
  ldwv = NMAX
  ldwh = 2*NSMAX+1

  ! ==================================================================
  ! Test 1: 6x6 with 2 shifts, WANTT=true, WANTZ=true
  ! ==================================================================
  n = 6
  ktop = 1
  kbot = 6
  nshfts = 2
  kacc22 = 0
  iloz = 1
  ihiz = 6
  nh = n
  nv = n

  H = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)
  S = (0.0d0, 0.0d0)
  V = (0.0d0, 0.0d0)
  U = (0.0d0, 0.0d0)
  WV = (0.0d0, 0.0d0)
  WH = (0.0d0, 0.0d0)

  ! Upper Hessenberg matrix
  H(1,1) = (5.0d0, 1.0d0)
  H(1,2) = (2.0d0, -0.5d0)
  H(1,3) = (1.0d0, 0.0d0)
  H(1,4) = (0.5d0, 0.2d0)
  H(1,5) = (0.25d0, 0.0d0)
  H(1,6) = (0.1d0, -0.1d0)
  H(2,1) = (1.0d0, 0.0d0)
  H(2,2) = (4.0d0, -1.0d0)
  H(2,3) = (1.5d0, 0.5d0)
  H(2,4) = (0.5d0, 0.0d0)
  H(2,5) = (0.3d0, 0.1d0)
  H(2,6) = (0.15d0, 0.0d0)
  H(3,2) = (0.8d0, 0.2d0)
  H(3,3) = (3.0d0, 0.5d0)
  H(3,4) = (1.0d0, -0.5d0)
  H(3,5) = (0.5d0, 0.0d0)
  H(3,6) = (0.25d0, 0.1d0)
  H(4,3) = (0.6d0, -0.1d0)
  H(4,4) = (2.5d0, -0.5d0)
  H(4,5) = (1.0d0, 0.5d0)
  H(4,6) = (0.5d0, 0.0d0)
  H(5,4) = (0.4d0, 0.15d0)
  H(5,5) = (2.0d0, 0.0d0)
  H(5,6) = (1.0d0, -0.5d0)
  H(6,5) = (0.3d0, -0.1d0)
  H(6,6) = (1.5d0, 1.0d0)

  ! Initialize Z to identity
  do i = 1, n
    Z(i,i) = (1.0d0, 0.0d0)
  end do

  ! Shifts
  S(1) = (3.0d0, 1.0d0)
  S(2) = (2.0d0, -0.5d0)

  call ZLAQR5(.TRUE., .TRUE., kacc22, n, ktop, kbot, nshfts, S, &
               H, ldh, iloz, ihiz, Z, ldz, V, ldv, U, ldu, nv, &
               WV, ldwv, nh, WH, ldwh)

  ! Pack n*n result from LDH-strided storage into contiguous array
  do j = 1, n
    do i = 1, n
      Hpk((j-1)*n + i) = H(i,j)
      Zpk((j-1)*n + i) = Z(i,j)
    end do
  end do
  call begin_test('6x6_2shifts')
  call print_int('n', n)
  call print_array('H', Hpk_r, 2*n*n)
  call print_array('Z', Zpk_r, 2*n*n)
  call print_array('S', S_r, 2*nshfts)
  call end_test()

  ! ==================================================================
  ! Test 2: 8x8 with 4 shifts
  ! ==================================================================
  n = 8
  ktop = 1
  kbot = 8
  nshfts = 4
  kacc22 = 0
  iloz = 1
  ihiz = 8
  nh = n
  nv = n

  H = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)
  S = (0.0d0, 0.0d0)
  V = (0.0d0, 0.0d0)
  U = (0.0d0, 0.0d0)
  WV = (0.0d0, 0.0d0)
  WH = (0.0d0, 0.0d0)

  ! Diagonally dominant upper Hessenberg 8x8
  H(1,1) = (8.0d0, 0.5d0)
  H(1,2) = (1.0d0, -0.5d0)
  H(1,3) = (0.5d0, 0.0d0)
  H(1,4) = (0.25d0, 0.1d0)
  H(2,1) = (1.0d0, 0.0d0)
  H(2,2) = (7.0d0, -0.5d0)
  H(2,3) = (1.0d0, 0.5d0)
  H(2,4) = (0.5d0, 0.0d0)
  H(2,5) = (0.25d0, 0.0d0)
  H(3,2) = (0.8d0, 0.1d0)
  H(3,3) = (6.0d0, 1.0d0)
  H(3,4) = (1.0d0, -0.3d0)
  H(3,5) = (0.5d0, 0.0d0)
  H(3,6) = (0.2d0, 0.0d0)
  H(4,3) = (0.7d0, -0.2d0)
  H(4,4) = (5.0d0, -1.0d0)
  H(4,5) = (1.0d0, 0.5d0)
  H(4,6) = (0.4d0, 0.0d0)
  H(4,7) = (0.1d0, 0.0d0)
  H(5,4) = (0.6d0, 0.1d0)
  H(5,5) = (4.0d0, 0.0d0)
  H(5,6) = (1.0d0, -0.5d0)
  H(5,7) = (0.3d0, 0.0d0)
  H(5,8) = (0.1d0, 0.0d0)
  H(6,5) = (0.5d0, -0.1d0)
  H(6,6) = (3.0d0, 0.5d0)
  H(6,7) = (1.0d0, 0.5d0)
  H(6,8) = (0.2d0, 0.0d0)
  H(7,6) = (0.4d0, 0.05d0)
  H(7,7) = (2.0d0, -0.5d0)
  H(7,8) = (1.0d0, -0.5d0)
  H(8,7) = (0.3d0, -0.1d0)
  H(8,8) = (1.0d0, 1.0d0)

  do i = 1, n
    Z(i,i) = (1.0d0, 0.0d0)
  end do

  ! 4 shifts
  S(1) = (4.5d0, 0.5d0)
  S(2) = (3.5d0, -0.5d0)
  S(3) = (2.5d0, 1.0d0)
  S(4) = (1.5d0, -1.0d0)

  call ZLAQR5(.TRUE., .TRUE., kacc22, n, ktop, kbot, nshfts, S, &
               H, ldh, iloz, ihiz, Z, ldz, V, ldv, U, ldu, nv, &
               WV, ldwv, nh, WH, ldwh)

  do j = 1, n
    do i = 1, n
      Hpk((j-1)*n + i) = H(i,j)
      Zpk((j-1)*n + i) = Z(i,j)
    end do
  end do
  call begin_test('8x8_4shifts')
  call print_int('n', n)
  call print_array('H', Hpk_r, 2*n*n)
  call print_array('Z', Zpk_r, 2*n*n)
  call end_test()

  ! ==================================================================
  ! Test 3: 6x6, partial sweep (ktop=2, kbot=5)
  ! ==================================================================
  n = 6
  ktop = 2
  kbot = 5
  nshfts = 2
  kacc22 = 0
  iloz = 1
  ihiz = 6
  nh = n
  nv = n

  H = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)
  S = (0.0d0, 0.0d0)
  V = (0.0d0, 0.0d0)
  U = (0.0d0, 0.0d0)
  WV = (0.0d0, 0.0d0)
  WH = (0.0d0, 0.0d0)

  ! Already-converged eigenvalue at (1,1) and (6,6)
  H(1,1) = (10.0d0, 0.0d0)
  H(1,2) = (1.0d0, 0.5d0)
  H(1,3) = (0.5d0, 0.0d0)
  ! Active Hessenberg block rows 2-5
  H(2,2) = (5.0d0, 1.0d0)
  H(2,3) = (2.0d0, -0.5d0)
  H(2,4) = (1.0d0, 0.0d0)
  H(2,5) = (0.5d0, 0.2d0)
  H(3,2) = (1.0d0, 0.0d0)
  H(3,3) = (4.0d0, -1.0d0)
  H(3,4) = (1.5d0, 0.5d0)
  H(3,5) = (0.5d0, 0.0d0)
  H(4,3) = (0.8d0, 0.2d0)
  H(4,4) = (3.0d0, 0.5d0)
  H(4,5) = (1.0d0, -0.5d0)
  H(5,4) = (0.6d0, -0.1d0)
  H(5,5) = (2.0d0, 0.0d0)
  H(5,6) = (0.5d0, 0.5d0)
  H(6,6) = (-1.0d0, 2.0d0)

  do i = 1, n
    Z(i,i) = (1.0d0, 0.0d0)
  end do

  S(1) = (3.0d0, 0.5d0)
  S(2) = (2.5d0, -0.5d0)

  call ZLAQR5(.TRUE., .TRUE., kacc22, n, ktop, kbot, nshfts, S, &
               H, ldh, iloz, ihiz, Z, ldz, V, ldv, U, ldu, nv, &
               WV, ldwv, nh, WH, ldwh)

  do j = 1, n
    do i = 1, n
      Hpk((j-1)*n + i) = H(i,j)
      Zpk((j-1)*n + i) = Z(i,j)
    end do
  end do
  call begin_test('6x6_partial_sweep')
  call print_int('n', n)
  call print_array('H', Hpk_r, 2*n*n)
  call print_array('Z', Zpk_r, 2*n*n)
  call end_test()

end program
