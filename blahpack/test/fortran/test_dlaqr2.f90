program test_dlaqr2
  use test_utils
  implicit none

  integer, parameter :: MAXN = 10
  double precision :: H(MAXN, MAXN), Z(MAXN, MAXN), SR(MAXN), SI(MAXN)
  double precision :: V(MAXN, MAXN), T(MAXN, MAXN), WV(MAXN, MAXN)
  double precision :: WORK(200)
  integer :: N, KTOP, KBOT, NW, NS, ND, ILOZ, IHIZ, NH, NV, LWORK
  logical :: WANTT, WANTZ
  integer :: i, j

  ! ---- Test 1: 6x6 upper Hessenberg, NW=3, expect deflation ----
  N = 6
  KTOP = 1
  KBOT = 6
  NW = 3
  WANTT = .true.
  WANTZ = .true.
  ILOZ = 1
  IHIZ = 6
  NH = 6
  NV = 6
  LWORK = 200

  ! Build an upper Hessenberg matrix with known eigenvalues
  H = 0.0d0
  H(1,1) = 4.0d0; H(1,2) = 1.0d0; H(1,3) = 0.5d0; H(1,4) = 0.1d0; H(1,5) = 0.2d0; H(1,6) = 0.3d0
  H(2,1) = 1.0d0; H(2,2) = 3.0d0; H(2,3) = 0.8d0; H(2,4) = 0.2d0; H(2,5) = 0.1d0; H(2,6) = 0.4d0
  H(3,2) = 0.5d0; H(3,3) = 2.0d0; H(3,4) = 0.7d0; H(3,5) = 0.3d0; H(3,6) = 0.2d0
  H(4,3) = 0.3d0; H(4,4) = 1.5d0; H(4,5) = 0.9d0; H(4,6) = 0.1d0
  H(5,4) = 0.2d0; H(5,5) = 1.0d0; H(5,6) = 0.6d0
  H(6,5) = 0.1d0; H(6,6) = 0.5d0

  ! Initialize Z = I
  Z = 0.0d0
  do i = 1, N
    Z(i,i) = 1.0d0
  end do

  call DLAQR2(WANTT, WANTZ, N, KTOP, KBOT, NW, H, MAXN, ILOZ, IHIZ, &
              Z, MAXN, NS, ND, SR, SI, V, MAXN, NH, T, MAXN, NV, WV, MAXN, WORK, LWORK)

  call begin_test('6x6 hessenberg NW=3')
  call print_int('ns', NS)
  call print_int('nd', ND)
  call print_array('H', H, MAXN*N)
  call print_array('Z', Z, MAXN*N)
  call print_array('SR', SR, N)
  call print_array('SI', SI, N)
  call end_test()

  ! ---- Test 2: 4x4 upper Hessenberg, NW=2 ----
  SR = 0.0d0; SI = 0.0d0
  N = 4
  KTOP = 1
  KBOT = 4
  NW = 2
  WANTT = .true.
  WANTZ = .true.
  ILOZ = 1
  IHIZ = 4
  NH = 4
  NV = 4
  LWORK = 200

  H = 0.0d0
  H(1,1) = 5.0d0; H(1,2) = 2.0d0; H(1,3) = 0.3d0; H(1,4) = 0.1d0
  H(2,1) = 1.0d0; H(2,2) = 4.0d0; H(2,3) = 1.5d0; H(2,4) = 0.2d0
  H(3,2) = 0.8d0; H(3,3) = 3.0d0; H(3,4) = 1.0d0
  H(4,3) = 0.5d0; H(4,4) = 2.0d0

  Z = 0.0d0
  do i = 1, N
    Z(i,i) = 1.0d0
  end do

  call DLAQR2(WANTT, WANTZ, N, KTOP, KBOT, NW, H, MAXN, ILOZ, IHIZ, &
              Z, MAXN, NS, ND, SR, SI, V, MAXN, NH, T, MAXN, NV, WV, MAXN, WORK, LWORK)

  call begin_test('4x4 hessenberg NW=2')
  call print_int('ns', NS)
  call print_int('nd', ND)
  call print_array('H', H, MAXN*N)
  call print_array('Z', Z, MAXN*N)
  call print_array('SR', SR, N)
  call print_array('SI', SI, N)
  call end_test()

  ! ---- Test 3: NW=1 (1-by-1 deflation window) ----
  SR = 0.0d0; SI = 0.0d0
  N = 4
  KTOP = 1
  KBOT = 4
  NW = 1
  WANTT = .true.
  WANTZ = .false.
  ILOZ = 1
  IHIZ = 4
  NH = 4
  NV = 4
  LWORK = 200

  H = 0.0d0
  H(1,1) = 5.0d0; H(1,2) = 2.0d0; H(1,3) = 0.3d0; H(1,4) = 0.1d0
  H(2,1) = 1.0d0; H(2,2) = 4.0d0; H(2,3) = 1.5d0; H(2,4) = 0.2d0
  H(3,2) = 0.8d0; H(3,3) = 3.0d0; H(3,4) = 1.0d0
  H(4,3) = 0.5d0; H(4,4) = 2.0d0

  call DLAQR2(WANTT, WANTZ, N, KTOP, KBOT, NW, H, MAXN, ILOZ, IHIZ, &
              Z, MAXN, NS, ND, SR, SI, V, MAXN, NH, T, MAXN, NV, WV, MAXN, WORK, LWORK)

  call begin_test('4x4 NW=1 no wantz')
  call print_int('ns', NS)
  call print_int('nd', ND)
  call print_array('H', H, MAXN*N)
  call print_array('SR', SR, N)
  call print_array('SI', SI, N)
  call end_test()

  ! ---- Test 4: Edge case ktop > kbot ----
  N = 4
  KTOP = 3
  KBOT = 2
  NW = 2

  call DLAQR2(.true., .false., N, KTOP, KBOT, NW, H, MAXN, 1, N, &
              Z, MAXN, NS, ND, SR, SI, V, MAXN, NH, T, MAXN, NV, WV, MAXN, WORK, LWORK)

  call begin_test('edge case ktop > kbot')
  call print_int('ns', NS)
  call print_int('nd', ND)
  call end_test()

  ! ---- Test 5: 8x8 Hessenberg NW=4, partial window ----
  SR = 0.0d0; SI = 0.0d0
  N = 8
  KTOP = 2
  KBOT = 7
  NW = 4
  WANTT = .true.
  WANTZ = .true.
  ILOZ = 1
  IHIZ = 8
  NH = 8
  NV = 8
  LWORK = 200

  H = 0.0d0
  ! Build a diagonally dominant Hessenberg
  do i = 1, N
    H(i,i) = dble(N + 1 - i) * 1.5d0
    if (i < N) H(i, i+1) = 0.5d0
    if (i < N) H(i+1, i) = 0.3d0
    if (i + 2 <= N) H(i, i+2) = 0.1d0
  end do

  Z = 0.0d0
  do i = 1, N
    Z(i,i) = 1.0d0
  end do

  call DLAQR2(WANTT, WANTZ, N, KTOP, KBOT, NW, H, MAXN, ILOZ, IHIZ, &
              Z, MAXN, NS, ND, SR, SI, V, MAXN, NH, T, MAXN, NV, WV, MAXN, WORK, LWORK)

  call begin_test('8x8 hessenberg NW=4 partial')
  call print_int('ns', NS)
  call print_int('nd', ND)
  call print_array('H', H, MAXN*N)
  call print_array('Z', Z, MAXN*N)
  call print_array('SR', SR, N)
  call print_array('SI', SI, N)
  call end_test()

  ! ---- Test 6: 6x6 with deflation (tiny subdiag) ----
  SR = 0.0d0; SI = 0.0d0
  N = 6
  KTOP = 1
  KBOT = 6
  NW = 3
  WANTT = .true.
  WANTZ = .true.
  ILOZ = 1
  IHIZ = 6
  NH = 6
  NV = 6
  LWORK = 200

  ! Build Hessenberg with a tiny subdiagonal entry near bottom
  H = 0.0d0
  H(1,1) = 10.0d0; H(1,2) = 1.0d0; H(1,3) = 0.5d0; H(1,4) = 0.1d0; H(1,5) = 0.2d0; H(1,6) = 0.3d0
  H(2,1) = 2.0d0;  H(2,2) = 8.0d0;  H(2,3) = 0.8d0; H(2,4) = 0.2d0; H(2,5) = 0.1d0; H(2,6) = 0.4d0
  H(3,2) = 1.5d0;  H(3,3) = 6.0d0;  H(3,4) = 0.7d0; H(3,5) = 0.3d0; H(3,6) = 0.2d0
  H(4,3) = 1.0d0;  H(4,4) = 4.0d0;  H(4,5) = 0.9d0; H(4,6) = 0.1d0
  H(5,4) = 1.0d-14; H(5,5) = 3.0d0; H(5,6) = 0.6d0
  H(6,5) = 1.0d-15; H(6,6) = 1.0d0

  Z = 0.0d0
  do i = 1, N
    Z(i,i) = 1.0d0
  end do

  call DLAQR2(WANTT, WANTZ, N, KTOP, KBOT, NW, H, MAXN, ILOZ, IHIZ, &
              Z, MAXN, NS, ND, SR, SI, V, MAXN, NH, T, MAXN, NV, WV, MAXN, WORK, LWORK)

  call begin_test('6x6 with deflation')
  call print_int('ns', NS)
  call print_int('nd', ND)
  call print_array('H', H, MAXN*N)
  call print_array('Z', Z, MAXN*N)
  call print_array('SR', SR, N)
  call print_array('SI', SI, N)
  call end_test()

  ! ---- Test 7: 6x6 with NW=6 (full window) ----
  SR = 0.0d0; SI = 0.0d0
  N = 6
  KTOP = 1
  KBOT = 6
  NW = 6
  WANTT = .true.
  WANTZ = .true.
  ILOZ = 1
  IHIZ = 6
  NH = 6
  NV = 6
  LWORK = 200

  H = 0.0d0
  H(1,1) = 10.0d0; H(1,2) = 1.0d0; H(1,3) = 0.5d0; H(1,4) = 0.1d0; H(1,5) = 0.2d0; H(1,6) = 0.3d0
  H(2,1) = 2.0d0;  H(2,2) = 8.0d0;  H(2,3) = 0.8d0; H(2,4) = 0.2d0; H(2,5) = 0.1d0; H(2,6) = 0.4d0
  H(3,2) = 1.5d0;  H(3,3) = 6.0d0;  H(3,4) = 0.7d0; H(3,5) = 0.3d0; H(3,6) = 0.2d0
  H(4,3) = 1.0d0;  H(4,4) = 4.0d0;  H(4,5) = 0.9d0; H(4,6) = 0.1d0
  H(5,4) = 1.0d-14; H(5,5) = 3.0d0; H(5,6) = 0.6d0
  H(6,5) = 1.0d-15; H(6,6) = 1.0d0

  Z = 0.0d0
  do i = 1, N
    Z(i,i) = 1.0d0
  end do

  call DLAQR2(WANTT, WANTZ, N, KTOP, KBOT, NW, H, MAXN, ILOZ, IHIZ, &
              Z, MAXN, NS, ND, SR, SI, V, MAXN, NH, T, MAXN, NV, WV, MAXN, WORK, LWORK)

  call begin_test('6x6 full window')
  call print_int('ns', NS)
  call print_int('nd', ND)
  call print_array('H', H, MAXN*N)
  call print_array('Z', Z, MAXN*N)
  call print_array('SR', SR, N)
  call print_array('SI', SI, N)
  call end_test()

end program
