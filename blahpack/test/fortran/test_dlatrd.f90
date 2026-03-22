program test_dlatrd
  use test_utils
  implicit none

  ! Test dlatrd: reduces NB rows/columns of a symmetric matrix
  ! to symmetric tridiagonal form
  !
  ! Test 1: UPLO='U', N=6, NB=3
  ! Test 2: UPLO='L', N=6, NB=3
  ! Test 3: NB=0 quick return
  ! Test 4: UPLO='U', N=4, NB=2
  ! Test 5: UPLO='L', N=4, NB=2

  integer, parameter :: MAXN = 6, MAXNB = 3
  double precision :: A(MAXN * MAXN), W(MAXN * MAXNB)
  double precision :: E(MAXN), TAU(MAXN)

  ! -------------------------------------------------------
  ! Test 1: UPLO='U', N=6, NB=3
  ! -------------------------------------------------------
  ! Symmetric 6x6 matrix, upper triangle stored, column-major LDA=6
  A = 0.0d0
  ! Column 1
  A(1) =  2.0d0   ! A(1,1)
  A(2) =  0.5d0   ! A(2,1) (upper: A(1,2))
  A(3) = -0.3d0   ! A(3,1) (upper: A(1,3))
  A(4) =  0.7d0   ! A(4,1) (upper: A(1,4))
  A(5) =  1.0d0   ! A(5,1) (upper: A(1,5))
  A(6) = -0.2d0   ! A(6,1) (upper: A(1,6))
  ! Column 2
  A(7)  =  0.5d0  ! A(1,2)
  A(8)  = -1.0d0  ! A(2,2)
  A(9)  =  0.6d0  ! A(3,2) (upper: A(2,3))
  A(10) =  0.4d0  ! A(4,2) (upper: A(2,4))
  A(11) = -0.8d0  ! A(5,2) (upper: A(2,5))
  A(12) =  0.3d0  ! A(6,2) (upper: A(2,6))
  ! Column 3
  A(13) = -0.3d0  ! A(1,3)
  A(14) =  0.6d0  ! A(2,3)
  A(15) =  1.5d0  ! A(3,3)
  A(16) = -0.5d0  ! A(4,3) (upper: A(3,4))
  A(17) =  0.2d0  ! A(5,3) (upper: A(3,5))
  A(18) =  0.9d0  ! A(6,3) (upper: A(3,6))
  ! Column 4
  A(19) =  0.7d0  ! A(1,4)
  A(20) =  0.4d0  ! A(2,4)
  A(21) = -0.5d0  ! A(3,4)
  A(22) =  0.8d0  ! A(4,4)
  A(23) =  1.2d0  ! A(5,4) (upper: A(4,5))
  A(24) = -0.6d0  ! A(6,4) (upper: A(4,6))
  ! Column 5
  A(25) =  1.0d0  ! A(1,5)
  A(26) = -0.8d0  ! A(2,5)
  A(27) =  0.2d0  ! A(3,5)
  A(28) =  1.2d0  ! A(4,5)
  A(29) = -0.4d0  ! A(5,5)
  A(30) =  0.7d0  ! A(6,5) (upper: A(5,6))
  ! Column 6
  A(31) = -0.2d0  ! A(1,6)
  A(32) =  0.3d0  ! A(2,6)
  A(33) =  0.9d0  ! A(3,6)
  A(34) = -0.6d0  ! A(4,6)
  A(35) =  0.7d0  ! A(5,6)
  A(36) =  1.0d0  ! A(6,6)

  W = 0.0d0
  E = 0.0d0
  TAU = 0.0d0

  call dlatrd('U', 6, 3, A, 6, E, TAU, W, 6)

  call begin_test('uplo_u_6x6_nb3')
  call print_array('A', A, 6*6)
  call print_array('E', E, 6)
  call print_array('TAU', TAU, 6)
  call print_array('W', W, 6*3)
  call end_test()

  ! -------------------------------------------------------
  ! Test 2: UPLO='L', N=6, NB=3
  ! -------------------------------------------------------
  ! Same symmetric matrix, lower triangle stored
  A = 0.0d0
  ! Column 1
  A(1) =  2.0d0   ! A(1,1)
  A(2) =  0.5d0   ! A(2,1)
  A(3) = -0.3d0   ! A(3,1)
  A(4) =  0.7d0   ! A(4,1)
  A(5) =  1.0d0   ! A(5,1)
  A(6) = -0.2d0   ! A(6,1)
  ! Column 2
  A(7)  =  0.5d0  ! A(1,2) (lower: stored but not accessed by lower)
  A(8)  = -1.0d0  ! A(2,2)
  A(9)  =  0.6d0  ! A(3,2)
  A(10) =  0.4d0  ! A(4,2)
  A(11) = -0.8d0  ! A(5,2)
  A(12) =  0.3d0  ! A(6,2)
  ! Column 3
  A(13) = -0.3d0  ! A(1,3)
  A(14) =  0.6d0  ! A(2,3)
  A(15) =  1.5d0  ! A(3,3)
  A(16) = -0.5d0  ! A(4,3)
  A(17) =  0.2d0  ! A(5,3)
  A(18) =  0.9d0  ! A(6,3)
  ! Column 4
  A(19) =  0.7d0  ! A(1,4)
  A(20) =  0.4d0  ! A(2,4)
  A(21) = -0.5d0  ! A(3,4)
  A(22) =  0.8d0  ! A(4,4)
  A(23) =  1.2d0  ! A(5,4)
  A(24) = -0.6d0  ! A(6,4)
  ! Column 5
  A(25) =  1.0d0  ! A(1,5)
  A(26) = -0.8d0  ! A(2,5)
  A(27) =  0.2d0  ! A(3,5)
  A(28) =  1.2d0  ! A(4,5)
  A(29) = -0.4d0  ! A(5,5)
  A(30) =  0.7d0  ! A(6,5)
  ! Column 6
  A(31) = -0.2d0  ! A(1,6)
  A(32) =  0.3d0  ! A(2,6)
  A(33) =  0.9d0  ! A(3,6)
  A(34) = -0.6d0  ! A(4,6)
  A(35) =  0.7d0  ! A(5,6)
  A(36) =  1.0d0  ! A(6,6)

  W = 0.0d0
  E = 0.0d0
  TAU = 0.0d0

  call dlatrd('L', 6, 3, A, 6, E, TAU, W, 6)

  call begin_test('uplo_l_6x6_nb3')
  call print_array('A', A, 6*6)
  call print_array('E', E, 6)
  call print_array('TAU', TAU, 6)
  call print_array('W', W, 6*3)
  call end_test()

  ! -------------------------------------------------------
  ! Test 3: NB=0 quick return
  ! -------------------------------------------------------
  A = 0.0d0
  A(1)  =  2.0d0
  A(2)  =  0.5d0
  A(3)  =  0.5d0
  A(4)  = -1.0d0

  call dlatrd('U', 2, 0, A, 2, E, TAU, W, 2)

  call begin_test('nb0_quick_return')
  call print_array('A', A, 4)
  call end_test()

  ! -------------------------------------------------------
  ! Test 4: UPLO='U', N=4, NB=2
  ! -------------------------------------------------------
  A = 0.0d0
  ! Symmetric 4x4, upper triangle stored
  A(1)  =  3.0d0   ! A(1,1)
  A(2)  =  1.0d0   ! A(2,1)
  A(3)  = -0.5d0   ! A(3,1)
  A(4)  =  0.8d0   ! A(4,1)
  A(5)  =  1.0d0   ! A(1,2)
  A(6)  =  2.0d0   ! A(2,2)
  A(7)  =  0.7d0   ! A(3,2) upper: A(2,3)
  A(8)  = -0.4d0   ! A(4,2) upper: A(2,4)
  A(9)  = -0.5d0   ! A(1,3)
  A(10) =  0.7d0   ! A(2,3)
  A(11) =  1.5d0   ! A(3,3)
  A(12) =  0.6d0   ! A(4,3) upper: A(3,4)
  A(13) =  0.8d0   ! A(1,4)
  A(14) = -0.4d0   ! A(2,4)
  A(15) =  0.6d0   ! A(3,4)
  A(16) = -1.0d0   ! A(4,4)

  W(1:4*2) = 0.0d0
  E(1:4) = 0.0d0
  TAU(1:4) = 0.0d0

  call dlatrd('U', 4, 2, A, 4, E, TAU, W, 4)

  call begin_test('uplo_u_4x4_nb2')
  call print_array('A', A, 4*4)
  call print_array('E', E, 4)
  call print_array('TAU', TAU, 4)
  call print_array('W', W, 4*2)
  call end_test()

  ! -------------------------------------------------------
  ! Test 5: UPLO='L', N=4, NB=2
  ! -------------------------------------------------------
  A = 0.0d0
  ! Symmetric 4x4, lower triangle stored
  A(1)  =  3.0d0   ! A(1,1)
  A(2)  =  1.0d0   ! A(2,1)
  A(3)  = -0.5d0   ! A(3,1)
  A(4)  =  0.8d0   ! A(4,1)
  A(5)  =  1.0d0   ! A(1,2)
  A(6)  =  2.0d0   ! A(2,2)
  A(7)  =  0.7d0   ! A(3,2)
  A(8)  = -0.4d0   ! A(4,2)
  A(9)  = -0.5d0   ! A(1,3)
  A(10) =  0.7d0   ! A(2,3)
  A(11) =  1.5d0   ! A(3,3)
  A(12) =  0.6d0   ! A(4,3)
  A(13) =  0.8d0   ! A(1,4)
  A(14) = -0.4d0   ! A(2,4)
  A(15) =  0.6d0   ! A(3,4)
  A(16) = -1.0d0   ! A(4,4)

  W(1:4*2) = 0.0d0
  E(1:4) = 0.0d0
  TAU(1:4) = 0.0d0

  call dlatrd('L', 4, 2, A, 4, E, TAU, W, 4)

  call begin_test('uplo_l_4x4_nb2')
  call print_array('A', A, 4*4)
  call print_array('E', E, 4)
  call print_array('TAU', TAU, 4)
  call print_array('W', W, 4*2)
  call end_test()

end program
