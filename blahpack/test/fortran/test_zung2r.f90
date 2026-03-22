program test_zung2r
  use test_utils
  implicit none

  integer :: info
  ! Use allocatable to avoid LDA padding issues; or just size arrays tightly
  ! Max size needed: 4x4 = 16 complex elements
  complex*16 :: A(16), TAU(4), WORK(4)
  double precision :: A_r(32), TAU_r(8)
  equivalence (A, A_r)
  equivalence (TAU, TAU_r)

  ! Test 1: 3x3 identity (K=0, so Q = I)
  ! LDA=3, M=3, N=3, K=0
  A(1:9) = (0.0d0, 0.0d0)
  TAU = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0)
  call zung2r(3, 3, 0, A, 3, TAU, WORK, info)
  call begin_test('zung2r_identity_k0')
  call print_int('info', info)
  call print_array('A', A_r, 2*3*3)
  call end_test()

  ! Test 2: 3x3, K=1, single reflector
  ! LDA=3
  A(1:9) = (0.0d0, 0.0d0)
  A(1) = (1.0d0, 0.0d0)   ! A(1,1) - reflector diagonal
  A(2) = (0.5d0, 0.25d0)  ! A(2,1) - reflector body
  A(3) = (0.3d0, -0.1d0)  ! A(3,1) - reflector body
  TAU(1) = (1.2d0, 0.3d0)
  WORK = (0.0d0, 0.0d0)
  call zung2r(3, 3, 1, A, 3, TAU, WORK, info)
  call begin_test('zung2r_3x3_k1')
  call print_int('info', info)
  call print_array('A', A_r, 2*3*3)
  call end_test()

  ! Test 3: 3x3, K=2, two reflectors
  ! LDA=3
  A(1:9) = (0.0d0, 0.0d0)
  A(1) = (1.0d0, 0.0d0)    ! A(1,1)
  A(2) = (0.4d0, 0.2d0)    ! A(2,1)
  A(3) = (0.1d0, -0.3d0)   ! A(3,1)
  A(5) = (1.0d0, 0.0d0)    ! A(2,2) (column 2 starts at index 4 for LDA=3)
  A(6) = (0.6d0, 0.5d0)    ! A(3,2)
  TAU(1) = (1.1d0, 0.2d0)
  TAU(2) = (0.9d0, -0.1d0)
  WORK = (0.0d0, 0.0d0)
  call zung2r(3, 3, 2, A, 3, TAU, WORK, info)
  call begin_test('zung2r_3x3_k2')
  call print_int('info', info)
  call print_array('A', A_r, 2*3*3)
  call end_test()

  ! Test 4: 4x3, K=3, rectangular matrix
  ! LDA=4
  A(1:12) = (0.0d0, 0.0d0)
  A(1) = (1.0d0, 0.0d0)     ! A(1,1)
  A(2) = (0.3d0, 0.1d0)     ! A(2,1)
  A(3) = (0.2d0, -0.2d0)    ! A(3,1)
  A(4) = (0.1d0, 0.05d0)    ! A(4,1)
  ! Column 2 starts at index 5 (LDA=4)
  A(6) = (1.0d0, 0.0d0)     ! A(2,2)
  A(7) = (0.4d0, 0.3d0)     ! A(3,2)
  A(8) = (-0.1d0, 0.2d0)    ! A(4,2)
  ! Column 3 starts at index 9
  A(11) = (1.0d0, 0.0d0)    ! A(3,3)
  A(12) = (0.5d0, -0.1d0)   ! A(4,3)
  TAU(1) = (1.05d0, 0.1d0)
  TAU(2) = (1.15d0, -0.2d0)
  TAU(3) = (0.8d0, 0.15d0)
  WORK = (0.0d0, 0.0d0)
  call zung2r(4, 3, 3, A, 4, TAU, WORK, info)
  call begin_test('zung2r_4x3_k3')
  call print_int('info', info)
  call print_array('A', A_r, 2*4*3)
  call end_test()

  ! Test 5: N=0 quick return
  info = -99
  call zung2r(3, 0, 0, A, 3, TAU, WORK, info)
  call begin_test('zung2r_n0')
  call print_int('info', info)
  call end_test()

  ! Test 6: M=1, N=1, K=1
  ! LDA=1
  A(1) = (1.0d0, 0.0d0)
  TAU(1) = (0.5d0, 0.5d0)
  WORK = (0.0d0, 0.0d0)
  call zung2r(1, 1, 1, A, 1, TAU, WORK, info)
  call begin_test('zung2r_1x1_k1')
  call print_int('info', info)
  call print_array('A', A_r, 2)
  call end_test()

  ! Test 7: 3x3, K=3, full reflectors
  ! LDA=3
  A(1:9) = (0.0d0, 0.0d0)
  A(1) = (1.0d0, 0.0d0)    ! A(1,1)
  A(2) = (0.2d0, 0.3d0)    ! A(2,1)
  A(3) = (-0.1d0, 0.4d0)   ! A(3,1)
  A(5) = (1.0d0, 0.0d0)    ! A(2,2)
  A(6) = (0.5d0, -0.2d0)   ! A(3,2)
  A(9) = (1.0d0, 0.0d0)    ! A(3,3)
  TAU(1) = (1.3d0, -0.1d0)
  TAU(2) = (0.7d0, 0.4d0)
  TAU(3) = (1.1d0, 0.0d0)
  WORK = (0.0d0, 0.0d0)
  call zung2r(3, 3, 3, A, 3, TAU, WORK, info)
  call begin_test('zung2r_3x3_k3')
  call print_int('info', info)
  call print_array('A', A_r, 2*3*3)
  call end_test()

  ! Test 8: Use actual QR factorization output
  ! Factor a 4x4 matrix first, then regenerate Q
  ! LDA=4
  A(1:16) = (0.0d0, 0.0d0)
  A(1)  = (1.0d0, 2.0d0)
  A(2)  = (3.0d0, 4.0d0)
  A(3)  = (5.0d0, 6.0d0)
  A(4)  = (7.0d0, 8.0d0)
  A(5)  = (2.0d0, -1.0d0)
  A(6)  = (1.0d0, 3.0d0)
  A(7)  = (4.0d0, -2.0d0)
  A(8)  = (0.0d0, 1.0d0)
  A(9)  = (1.0d0, 1.0d0)
  A(10) = (-1.0d0, 2.0d0)
  A(11) = (3.0d0, 0.0d0)
  A(12) = (2.0d0, -3.0d0)
  A(13) = (0.0d0, 3.0d0)
  A(14) = (2.0d0, -1.0d0)
  A(15) = (-1.0d0, 4.0d0)
  A(16) = (1.0d0, 1.0d0)
  call zgeqrf(4, 4, A, 4, TAU, WORK, 4, info)
  call zung2r(4, 4, 4, A, 4, TAU, WORK, info)
  call begin_test('zung2r_from_qr')
  call print_int('info', info)
  call print_array('A', A_r, 2*4*4)
  call end_test()

end program
