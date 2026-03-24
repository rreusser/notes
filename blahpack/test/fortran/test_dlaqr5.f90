program test_dlaqr5
  use test_utils
  implicit none

  integer, parameter :: MAXN = 10
  double precision :: H(MAXN,MAXN), Z(MAXN,MAXN)
  double precision :: SR(MAXN), SI(MAXN)
  double precision :: V(3,MAXN), U(MAXN,MAXN)
  double precision :: WV(MAXN,MAXN), WH(MAXN,MAXN)
  integer :: N, KTOP, KBOT, NSHFTS, NH, NV
  integer :: i, j

  ! ============================================================
  ! Test 1: 6x6 Hessenberg, 2 shifts (NS=2), WANTT=true, WANTZ=true, KACC22=0
  ! ============================================================
  N = 6
  KTOP = 1
  KBOT = 6
  NSHFTS = 2

  ! Initialize H as upper Hessenberg
  do j = 1, N
    do i = 1, N
      H(i,j) = 0.0d0
    end do
  end do
  H(1,1) = 4.0d0;  H(1,2) = 3.0d0;  H(1,3) = 2.0d0;  H(1,4) = 1.0d0;  H(1,5) = 0.5d0;  H(1,6) = 0.25d0
  H(2,1) = 1.0d0;  H(2,2) = 3.0d0;  H(2,3) = 2.5d0;  H(2,4) = 1.5d0;  H(2,5) = 0.8d0;  H(2,6) = 0.4d0
                    H(3,2) = 1.5d0;  H(3,3) = 2.0d0;  H(3,4) = 1.0d0;  H(3,5) = 0.7d0;  H(3,6) = 0.3d0
                                     H(4,3) = 0.8d0;  H(4,4) = 1.5d0;  H(4,5) = 0.6d0;  H(4,6) = 0.2d0
                                                       H(5,4) = 0.5d0;  H(5,5) = 1.0d0;  H(5,6) = 0.5d0
                                                                         H(6,5) = 0.3d0;  H(6,6) = 0.5d0

  SR(1) = 2.0d0;  SI(1) = 0.5d0
  SR(2) = 2.0d0;  SI(2) = -0.5d0

  ! Initialize Z as identity
  do j = 1, N
    do i = 1, N
      Z(i,j) = 0.0d0
    end do
    Z(j,j) = 1.0d0
  end do

  NH = N
  NV = N

  call DLAQR5(.TRUE., .TRUE., 0, N, KTOP, KBOT, NSHFTS, &
              SR, SI, H, MAXN, 1, N, Z, MAXN, V, 3, U, MAXN, &
              NV, WV, MAXN, NH, WH, MAXN)

  call begin_test('6x6_2shifts_wantt_wantz')
  call print_matrix('H', H, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call print_array('SR', SR, NSHFTS)
  call print_array('SI', SI, NSHFTS)
  call end_test()

  ! ============================================================
  ! Test 2: 8x8 Hessenberg, 4 shifts (NS=4), WANTT=true, WANTZ=true, KACC22=1
  ! ============================================================
  N = 8
  KTOP = 1
  KBOT = 8
  NSHFTS = 4

  do j = 1, N
    do i = 1, N
      H(i,j) = 0.0d0
    end do
  end do
  H(1,1) = 10.0d0; H(1,2) = 2.0d0;  H(1,3) = 1.0d0;  H(1,4) = 0.5d0
  H(1,5) = 0.3d0;  H(1,6) = 0.2d0;  H(1,7) = 0.1d0;  H(1,8) = 0.05d0
  H(2,1) = 3.0d0;  H(2,2) = 9.0d0;  H(2,3) = 2.0d0;  H(2,4) = 1.0d0
  H(2,5) = 0.4d0;  H(2,6) = 0.3d0;  H(2,7) = 0.2d0;  H(2,8) = 0.1d0
                    H(3,2) = 2.5d0;  H(3,3) = 8.0d0;  H(3,4) = 1.5d0
  H(3,5) = 0.5d0;  H(3,6) = 0.4d0;  H(3,7) = 0.3d0;  H(3,8) = 0.15d0
                                      H(4,3) = 2.0d0;  H(4,4) = 7.0d0
  H(4,5) = 1.0d0;  H(4,6) = 0.5d0;  H(4,7) = 0.4d0;  H(4,8) = 0.2d0
                                                         H(5,4) = 1.5d0
  H(5,5) = 6.0d0;  H(5,6) = 1.0d0;  H(5,7) = 0.5d0;  H(5,8) = 0.3d0
                                                         H(6,5) = 1.0d0
  H(6,6) = 5.0d0;  H(6,7) = 0.8d0;  H(6,8) = 0.4d0
                                                         H(7,6) = 0.8d0
  H(7,7) = 4.0d0;  H(7,8) = 0.6d0
                                                         H(8,7) = 0.5d0
  H(8,8) = 3.0d0

  SR(1) = 5.0d0;  SI(1) = 1.0d0
  SR(2) = 5.0d0;  SI(2) = -1.0d0
  SR(3) = 3.0d0;  SI(3) = 0.5d0
  SR(4) = 3.0d0;  SI(4) = -0.5d0

  do j = 1, N
    do i = 1, N
      Z(i,j) = 0.0d0
    end do
    Z(j,j) = 1.0d0
  end do

  NH = N
  NV = N

  call DLAQR5(.TRUE., .TRUE., 1, N, KTOP, KBOT, NSHFTS, &
              SR, SI, H, MAXN, 1, N, Z, MAXN, V, 3, U, MAXN, &
              NV, WV, MAXN, NH, WH, MAXN)

  call begin_test('8x8_4shifts_kacc22_1')
  call print_matrix('H', H, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call print_array('SR', SR, NSHFTS)
  call print_array('SI', SI, NSHFTS)
  call end_test()

  ! ============================================================
  ! Test 3: WANTT=false, WANTZ=false (minimal computation)
  ! ============================================================
  N = 6
  KTOP = 1
  KBOT = 6
  NSHFTS = 2

  do j = 1, N
    do i = 1, N
      H(i,j) = 0.0d0
    end do
  end do
  H(1,1) = 4.0d0;  H(1,2) = 3.0d0;  H(1,3) = 2.0d0;  H(1,4) = 1.0d0;  H(1,5) = 0.5d0;  H(1,6) = 0.25d0
  H(2,1) = 1.0d0;  H(2,2) = 3.0d0;  H(2,3) = 2.5d0;  H(2,4) = 1.5d0;  H(2,5) = 0.8d0;  H(2,6) = 0.4d0
                    H(3,2) = 1.5d0;  H(3,3) = 2.0d0;  H(3,4) = 1.0d0;  H(3,5) = 0.7d0;  H(3,6) = 0.3d0
                                     H(4,3) = 0.8d0;  H(4,4) = 1.5d0;  H(4,5) = 0.6d0;  H(4,6) = 0.2d0
                                                       H(5,4) = 0.5d0;  H(5,5) = 1.0d0;  H(5,6) = 0.5d0
                                                                         H(6,5) = 0.3d0;  H(6,6) = 0.5d0

  SR(1) = 2.0d0;  SI(1) = 0.5d0
  SR(2) = 2.0d0;  SI(2) = -0.5d0

  NH = N
  NV = N

  call DLAQR5(.FALSE., .FALSE., 0, N, KTOP, KBOT, NSHFTS, &
              SR, SI, H, MAXN, 1, N, Z, MAXN, V, 3, U, MAXN, &
              NV, WV, MAXN, NH, WH, MAXN)

  call begin_test('6x6_no_wantt_no_wantz')
  call print_matrix('H', H, MAXN, N, N)
  call end_test()

  ! ============================================================
  ! Test 4: Edge case NSHFTS=0 (should be a no-op)
  ! ============================================================
  N = 4
  do j = 1, N
    do i = 1, N
      H(i,j) = 0.0d0
    end do
  end do
  H(1,1) = 4.0d0; H(1,2) = 1.0d0; H(1,3) = 0.5d0; H(1,4) = 0.25d0
  H(2,1) = 2.0d0; H(2,2) = 3.0d0; H(2,3) = 1.0d0; H(2,4) = 0.5d0
                   H(3,2) = 1.5d0; H(3,3) = 2.0d0; H(3,4) = 0.8d0
                                    H(4,3) = 1.0d0; H(4,4) = 1.0d0

  NSHFTS = 0
  call DLAQR5(.TRUE., .TRUE., 0, N, 1, N, NSHFTS, &
              SR, SI, H, MAXN, 1, N, Z, MAXN, V, 3, U, MAXN, &
              NV, WV, MAXN, NH, WH, MAXN)

  call begin_test('nshfts_0_noop')
  call print_matrix('H', H, MAXN, N, N)
  call end_test()

  ! ============================================================
  ! Test 5: Edge case KTOP >= KBOT (should be a no-op)
  ! ============================================================
  N = 4
  KTOP = 3
  KBOT = 3
  NSHFTS = 2

  do j = 1, N
    do i = 1, N
      H(i,j) = 0.0d0
    end do
  end do
  H(1,1) = 4.0d0; H(1,2) = 1.0d0; H(1,3) = 0.5d0; H(1,4) = 0.25d0
  H(2,1) = 2.0d0; H(2,2) = 3.0d0; H(2,3) = 1.0d0; H(2,4) = 0.5d0
                   H(3,2) = 1.5d0; H(3,3) = 2.0d0; H(3,4) = 0.8d0
                                    H(4,3) = 1.0d0; H(4,4) = 1.0d0

  SR(1) = 2.0d0;  SI(1) = 0.0d0
  SR(2) = 1.0d0;  SI(2) = 0.0d0

  call DLAQR5(.TRUE., .TRUE., 0, N, KTOP, KBOT, NSHFTS, &
              SR, SI, H, MAXN, 1, N, Z, MAXN, V, 3, U, MAXN, &
              NV, WV, MAXN, NH, WH, MAXN)

  call begin_test('ktop_eq_kbot_noop')
  call print_matrix('H', H, MAXN, N, N)
  call end_test()

  ! ============================================================
  ! Test 6: 6x6, 4 shifts, KACC22=2, WANTT=true, WANTZ=true
  ! ============================================================
  N = 6
  KTOP = 1
  KBOT = 6
  NSHFTS = 4

  do j = 1, N
    do i = 1, N
      H(i,j) = 0.0d0
    end do
  end do
  H(1,1) = 6.0d0;  H(1,2) = 2.0d0;  H(1,3) = 1.5d0;  H(1,4) = 1.0d0;  H(1,5) = 0.5d0;  H(1,6) = 0.3d0
  H(2,1) = 2.0d0;  H(2,2) = 5.0d0;  H(2,3) = 2.0d0;  H(2,4) = 1.5d0;  H(2,5) = 0.8d0;  H(2,6) = 0.4d0
                    H(3,2) = 1.8d0;  H(3,3) = 4.0d0;  H(3,4) = 1.0d0;  H(3,5) = 0.6d0;  H(3,6) = 0.35d0
                                     H(4,3) = 1.2d0;  H(4,4) = 3.0d0;  H(4,5) = 0.9d0;  H(4,6) = 0.5d0
                                                       H(5,4) = 0.7d0;  H(5,5) = 2.0d0;  H(5,6) = 0.6d0
                                                                         H(6,5) = 0.4d0;  H(6,6) = 1.0d0

  SR(1) = 4.0d0;  SI(1) = 1.0d0
  SR(2) = 4.0d0;  SI(2) = -1.0d0
  SR(3) = 2.0d0;  SI(3) = 0.0d0
  SR(4) = 1.0d0;  SI(4) = 0.0d0

  do j = 1, N
    do i = 1, N
      Z(i,j) = 0.0d0
    end do
    Z(j,j) = 1.0d0
  end do

  NH = N
  NV = N

  call DLAQR5(.TRUE., .TRUE., 2, N, KTOP, KBOT, NSHFTS, &
              SR, SI, H, MAXN, 1, N, Z, MAXN, V, 3, U, MAXN, &
              NV, WV, MAXN, NH, WH, MAXN)

  call begin_test('6x6_4shifts_kacc22_2')
  call print_matrix('H', H, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call print_array('SR', SR, NSHFTS)
  call print_array('SI', SI, NSHFTS)
  call end_test()

  ! ============================================================
  ! Test 7: Partial sweep (KTOP=2, KBOT=5 on 6x6)
  ! ============================================================
  N = 6
  KTOP = 2
  KBOT = 5
  NSHFTS = 2

  do j = 1, N
    do i = 1, N
      H(i,j) = 0.0d0
    end do
  end do
  H(1,1) = 5.0d0;  H(1,2) = 1.0d0;  H(1,3) = 0.5d0;  H(1,4) = 0.3d0;  H(1,5) = 0.2d0;  H(1,6) = 0.1d0
  H(2,1) = 0.0d0;  H(2,2) = 4.0d0;  H(2,3) = 2.0d0;  H(2,4) = 1.0d0;  H(2,5) = 0.5d0;  H(2,6) = 0.3d0
                    H(3,2) = 1.5d0;  H(3,3) = 3.0d0;  H(3,4) = 1.5d0;  H(3,5) = 0.7d0;  H(3,6) = 0.4d0
                                     H(4,3) = 1.0d0;  H(4,4) = 2.0d0;  H(4,5) = 1.0d0;  H(4,6) = 0.5d0
                                                       H(5,4) = 0.8d0;  H(5,5) = 1.5d0;  H(5,6) = 0.6d0
                                                                         H(6,5) = 0.0d0;  H(6,6) = 1.0d0

  SR(1) = 3.0d0;  SI(1) = 0.0d0
  SR(2) = 2.0d0;  SI(2) = 0.0d0

  do j = 1, N
    do i = 1, N
      Z(i,j) = 0.0d0
    end do
    Z(j,j) = 1.0d0
  end do

  NH = N
  NV = N

  call DLAQR5(.TRUE., .TRUE., 0, N, KTOP, KBOT, NSHFTS, &
              SR, SI, H, MAXN, 1, N, Z, MAXN, V, 3, U, MAXN, &
              NV, WV, MAXN, NH, WH, MAXN)

  call begin_test('6x6_partial_sweep')
  call print_matrix('H', H, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call end_test()

  ! ============================================================
  ! Test 8: 8x8, 4 shifts, KACC22=0, WANTT=true, WANTZ=true
  ! ============================================================
  N = 8
  KTOP = 1
  KBOT = 8
  NSHFTS = 4

  do j = 1, N
    do i = 1, N
      H(i,j) = 0.0d0
    end do
  end do
  H(1,1) = 10.0d0; H(1,2) = 2.0d0;  H(1,3) = 1.0d0;  H(1,4) = 0.5d0
  H(1,5) = 0.3d0;  H(1,6) = 0.2d0;  H(1,7) = 0.1d0;  H(1,8) = 0.05d0
  H(2,1) = 3.0d0;  H(2,2) = 9.0d0;  H(2,3) = 2.0d0;  H(2,4) = 1.0d0
  H(2,5) = 0.4d0;  H(2,6) = 0.3d0;  H(2,7) = 0.2d0;  H(2,8) = 0.1d0
                    H(3,2) = 2.5d0;  H(3,3) = 8.0d0;  H(3,4) = 1.5d0
  H(3,5) = 0.5d0;  H(3,6) = 0.4d0;  H(3,7) = 0.3d0;  H(3,8) = 0.15d0
                                      H(4,3) = 2.0d0;  H(4,4) = 7.0d0
  H(4,5) = 1.0d0;  H(4,6) = 0.5d0;  H(4,7) = 0.4d0;  H(4,8) = 0.2d0
                                                         H(5,4) = 1.5d0
  H(5,5) = 6.0d0;  H(5,6) = 1.0d0;  H(5,7) = 0.5d0;  H(5,8) = 0.3d0
                                                         H(6,5) = 1.0d0
  H(6,6) = 5.0d0;  H(6,7) = 0.8d0;  H(6,8) = 0.4d0
                                                         H(7,6) = 0.8d0
  H(7,7) = 4.0d0;  H(7,8) = 0.6d0
                                                         H(8,7) = 0.5d0
  H(8,8) = 3.0d0

  SR(1) = 5.0d0;  SI(1) = 1.0d0
  SR(2) = 5.0d0;  SI(2) = -1.0d0
  SR(3) = 3.0d0;  SI(3) = 0.5d0
  SR(4) = 3.0d0;  SI(4) = -0.5d0

  do j = 1, N
    do i = 1, N
      Z(i,j) = 0.0d0
    end do
    Z(j,j) = 1.0d0
  end do

  NH = N
  NV = N

  call DLAQR5(.TRUE., .TRUE., 0, N, KTOP, KBOT, NSHFTS, &
              SR, SI, H, MAXN, 1, N, Z, MAXN, V, 3, U, MAXN, &
              NV, WV, MAXN, NH, WH, MAXN)

  call begin_test('8x8_4shifts_kacc22_0')
  call print_matrix('H', H, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call end_test()

  ! ============================================================
  ! Test 9: 4x4, 2 shifts, KACC22=1 (minimal accum test)
  ! ============================================================
  N = 4
  KTOP = 1
  KBOT = 4
  NSHFTS = 2

  do j = 1, N
    do i = 1, N
      H(i,j) = 0.0d0
    end do
  end do
  H(1,1) = 4.0d0; H(1,2) = 1.0d0; H(1,3) = 0.5d0; H(1,4) = 0.25d0
  H(2,1) = 2.0d0; H(2,2) = 3.0d0; H(2,3) = 1.0d0; H(2,4) = 0.5d0
                   H(3,2) = 1.5d0; H(3,3) = 2.0d0; H(3,4) = 0.8d0
                                    H(4,3) = 1.0d0; H(4,4) = 1.0d0

  SR(1) = 2.5d0;  SI(1) = 0.5d0
  SR(2) = 2.5d0;  SI(2) = -0.5d0

  do j = 1, N
    do i = 1, N
      Z(i,j) = 0.0d0
    end do
    Z(j,j) = 1.0d0
  end do

  NH = N
  NV = N

  call DLAQR5(.TRUE., .TRUE., 1, N, KTOP, KBOT, NSHFTS, &
              SR, SI, H, MAXN, 1, N, Z, MAXN, V, 3, U, MAXN, &
              NV, WV, MAXN, NH, WH, MAXN)

  call begin_test('4x4_2shifts_kacc22_1')
  call print_matrix('H', H, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call end_test()

end program
