program test_zlaqr3
  use test_utils
  implicit none

  integer, parameter :: NMAX = 10
  integer, parameter :: NWMAX = 5
  complex*16 :: H(NMAX, NMAX), Z(NMAX, NMAX)
  complex*16 :: SH(NMAX), V(NWMAX, NWMAX), T(NWMAX, NWMAX)
  complex*16 :: WV(NMAX, NWMAX), WORK(NMAX*NMAX)
  double precision :: H_r(2*NMAX*NMAX), Z_r(2*NMAX*NMAX)
  double precision :: SH_r(2*NMAX), V_r(2*NWMAX*NWMAX), T_r(2*NWMAX*NWMAX)
  equivalence (H, H_r)
  equivalence (Z, Z_r)
  equivalence (SH, SH_r)
  equivalence (V, V_r)
  equivalence (T, T_r)
  integer :: n, ktop, kbot, nw, ns, nd
  integer :: iloz, ihiz, ldh, ldz, ldv, ldt, ldwv, nh, nv, lwork
  integer :: i

  ldh = NMAX
  ldz = NMAX
  ldv = NWMAX
  ldt = NWMAX
  ldwv = NMAX

  ! ==================================================================
  ! Test 1: 8x8 with NW=3 deflation window, WANTT=true, WANTZ=true
  ! (Same matrix as zlaqr2 test for comparison)
  ! ==================================================================
  n = 8
  ktop = 1
  kbot = 8
  nw = 3
  iloz = 1
  ihiz = 8
  nh = nw
  nv = n
  lwork = n * n

  H = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)
  SH = (0.0d0, 0.0d0)
  V = (0.0d0, 0.0d0)
  T = (0.0d0, 0.0d0)
  WV = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0)

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

  call ZLAQR3(.TRUE., .TRUE., n, ktop, kbot, nw, H, ldh, iloz, ihiz, &
               Z, ldz, ns, nd, SH, V, ldv, nh, T, ldt, nv, WV, ldwv, &
               WORK, lwork)

  call begin_test('8x8_nw3')
  call print_int('n', n)
  call print_int('ns', ns)
  call print_int('nd', nd)
  call print_array('H', H_r, 2*n*n)
  call print_array('Z', Z_r, 2*n*n)
  call print_array('SH', SH_r, 2*n)
  call end_test()

  ! ==================================================================
  ! Test 2: 8x8 with NW=4, WANTT=false, WANTZ=false
  ! ==================================================================
  n = 8
  ktop = 1
  kbot = 8
  nw = 4
  iloz = 1
  ihiz = 8
  nh = nw
  nv = n
  lwork = n * n

  H = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)
  SH = (0.0d0, 0.0d0)
  V = (0.0d0, 0.0d0)
  T = (0.0d0, 0.0d0)
  WV = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0)

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

  call ZLAQR3(.FALSE., .FALSE., n, ktop, kbot, nw, H, ldh, iloz, ihiz, &
               Z, ldz, ns, nd, SH, V, ldv, nh, T, ldt, nv, WV, ldwv, &
               WORK, lwork)

  call begin_test('8x8_nw4_no_schur')
  call print_int('n', n)
  call print_int('ns', ns)
  call print_int('nd', nd)
  call print_array('SH', SH_r, 2*n)
  call end_test()

end program
