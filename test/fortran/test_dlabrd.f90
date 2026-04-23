program test_dlabrd
  use test_utils
  implicit none

  ! Test dlabrd: reduces first NB rows/columns to bidiagonal form
  !
  ! Test 1: M >= N case (6x5, NB=3) -> upper bidiagonal
  ! Test 2: M < N case (5x6, NB=3) -> lower bidiagonal
  ! Test 3: Square (4x4, NB=2) -> upper bidiagonal
  ! Test 4: NB=0 quick return
  ! Test 5: NB=1 single step (M >= N, 3x3)
  ! Test 6: NB=1 single step (M < N, 2x3)

  integer, parameter :: MAXM = 6, MAXN = 6, MAXNB = 3
  double precision :: A(MAXM * MAXN), X(MAXM * MAXNB), Y(MAXN * MAXNB)
  double precision :: TAUQ(MAXNB), TAUP(MAXNB)
  double precision :: D(MAXNB), E(MAXNB)

  ! -------------------------------------------------------
  ! Test 1: M=6, N=5, NB=3 (M >= N => upper bidiagonal)
  ! -------------------------------------------------------
  ! A is 6x5 column-major, LDA=6
  A = 0.0d0
  A(1)  =  1.0d0    ! A(1,1)
  A(2)  =  2.0d0    ! A(2,1)
  A(3)  = -0.5d0    ! A(3,1)
  A(4)  =  0.7d0    ! A(4,1)
  A(5)  =  1.5d0    ! A(5,1)
  A(6)  = -0.3d0    ! A(6,1)
  A(7)  =  0.3d0    ! A(1,2)
  A(8)  = -1.0d0    ! A(2,2)
  A(9)  =  0.6d0    ! A(3,2)
  A(10) =  1.2d0    ! A(4,2)
  A(11) = -0.3d0    ! A(5,2)
  A(12) =  0.4d0    ! A(6,2)
  A(13) =  0.5d0    ! A(1,3)
  A(14) =  0.8d0    ! A(2,3)
  A(15) = -0.4d0    ! A(3,3)
  A(16) =  0.2d0    ! A(4,3)
  A(17) =  1.1d0    ! A(5,3)
  A(18) = -0.6d0    ! A(6,3)
  A(19) = -0.2d0    ! A(1,4)
  A(20) =  0.4d0    ! A(2,4)
  A(21) =  0.9d0    ! A(3,4)
  A(22) = -0.6d0    ! A(4,4)
  A(23) =  0.3d0    ! A(5,4)
  A(24) =  0.7d0    ! A(6,4)
  A(25) =  0.8d0    ! A(1,5)
  A(26) = -0.1d0    ! A(2,5)
  A(27) =  0.2d0    ! A(3,5)
  A(28) =  1.3d0    ! A(4,5)
  A(29) = -0.5d0    ! A(5,5)
  A(30) =  0.9d0    ! A(6,5)

  X = 0.0d0
  Y = 0.0d0
  TAUQ = 0.0d0
  TAUP = 0.0d0
  D = 0.0d0
  E = 0.0d0

  call dlabrd(6, 5, 3, A, 6, D, E, TAUQ, TAUP, X, 6, Y, 5)

  call begin_test('m_ge_n_6x5_nb3')
  call print_array('A', A, 6*5)
  call print_array('D', D, 3)
  call print_array('E', E, 3)
  call print_array('TAUQ', TAUQ, 3)
  call print_array('TAUP', TAUP, 3)
  call print_array('X', X, 6*3)
  call print_array('Y', Y, 5*3)
  call end_test()

  ! -------------------------------------------------------
  ! Test 2: M=5, N=6, NB=3 (M < N => lower bidiagonal)
  ! -------------------------------------------------------
  ! A is 5x6 column-major, LDA=5
  A = 0.0d0
  A(1)  =  1.0d0    ! A(1,1)
  A(2)  =  2.0d0    ! A(2,1)
  A(3)  = -0.5d0    ! A(3,1)
  A(4)  =  0.7d0    ! A(4,1)
  A(5)  =  1.5d0    ! A(5,1)
  A(6)  =  0.3d0    ! A(1,2)
  A(7)  = -1.0d0    ! A(2,2)
  A(8)  =  0.6d0    ! A(3,2)
  A(9)  =  1.2d0    ! A(4,2)
  A(10) = -0.3d0    ! A(5,2)
  A(11) =  0.5d0    ! A(1,3)
  A(12) =  0.8d0    ! A(2,3)
  A(13) = -0.4d0    ! A(3,3)
  A(14) =  0.2d0    ! A(4,3)
  A(15) =  1.1d0    ! A(5,3)
  A(16) = -0.2d0    ! A(1,4)
  A(17) =  0.4d0    ! A(2,4)
  A(18) =  0.9d0    ! A(3,4)
  A(19) = -0.6d0    ! A(4,4)
  A(20) =  0.3d0    ! A(5,4)
  A(21) =  0.8d0    ! A(1,5)
  A(22) = -0.1d0    ! A(2,5)
  A(23) =  0.2d0    ! A(3,5)
  A(24) =  1.3d0    ! A(4,5)
  A(25) = -0.5d0    ! A(5,5)
  A(26) = -0.3d0    ! A(1,6)
  A(27) =  0.4d0    ! A(2,6)
  A(28) =  0.7d0    ! A(3,6)
  A(29) =  0.9d0    ! A(4,6)
  A(30) = -0.6d0    ! A(5,6)

  X = 0.0d0
  Y = 0.0d0
  TAUQ = 0.0d0
  TAUP = 0.0d0
  D = 0.0d0
  E = 0.0d0

  call dlabrd(5, 6, 3, A, 5, D, E, TAUQ, TAUP, X, 5, Y, 6)

  call begin_test('m_lt_n_5x6_nb3')
  call print_array('A', A, 5*6)
  call print_array('D', D, 3)
  call print_array('E', E, 3)
  call print_array('TAUQ', TAUQ, 3)
  call print_array('TAUP', TAUP, 3)
  call print_array('X', X, 5*3)
  call print_array('Y', Y, 6*3)
  call end_test()

  ! -------------------------------------------------------
  ! Test 3: Square M=4, N=4, NB=2 (M >= N => upper bidiagonal)
  ! -------------------------------------------------------
  ! A is 4x4 column-major, LDA=4
  A = 0.0d0
  A(1)  =  2.0d0    ! A(1,1)
  A(2)  = -1.0d0    ! A(2,1)
  A(3)  =  0.3d0    ! A(3,1)
  A(4)  =  0.5d0    ! A(4,1)
  A(5)  =  0.5d0    ! A(1,2)
  A(6)  =  1.0d0    ! A(2,2)
  A(7)  = -0.7d0    ! A(3,2)
  A(8)  =  0.4d0    ! A(4,2)
  A(9)  =  0.8d0    ! A(1,3)
  A(10) = -0.3d0    ! A(2,3)
  A(11) =  1.5d0    ! A(3,3)
  A(12) = -0.2d0    ! A(4,3)
  A(13) = -0.4d0    ! A(1,4)
  A(14) =  0.6d0    ! A(2,4)
  A(15) =  0.1d0    ! A(3,4)
  A(16) =  0.9d0    ! A(4,4)

  X(1:4*2) = 0.0d0
  Y(1:4*2) = 0.0d0
  TAUQ(1:2) = 0.0d0
  TAUP(1:2) = 0.0d0
  D(1:2) = 0.0d0
  E(1:2) = 0.0d0

  call dlabrd(4, 4, 2, A, 4, D, E, TAUQ, TAUP, X, 4, Y, 4)

  call begin_test('square_4x4_nb2')
  call print_array('A', A, 4*4)
  call print_array('D', D, 2)
  call print_array('E', E, 2)
  call print_array('TAUQ', TAUQ, 2)
  call print_array('TAUP', TAUP, 2)
  call print_array('X', X, 4*2)
  call print_array('Y', Y, 4*2)
  call end_test()

  ! -------------------------------------------------------
  ! Test 4: NB=0 quick return
  ! -------------------------------------------------------
  A = 0.0d0
  A(1) = 1.0d0
  A(2) = 2.0d0
  A(3) = 3.0d0
  A(4) = 4.0d0

  call dlabrd(2, 2, 0, A, 2, D, E, TAUQ, TAUP, X, 2, Y, 2)

  call begin_test('nb0_quick_return')
  ! A should be unchanged
  call print_array('A', A, 4)
  call end_test()

  ! -------------------------------------------------------
  ! Test 5: NB=1, M >= N (3x3)
  ! -------------------------------------------------------
  A = 0.0d0
  A(1)  =  2.0d0    ! A(1,1)
  A(2)  = -1.0d0    ! A(2,1)
  A(3)  =  0.3d0    ! A(3,1)
  A(4)  =  0.5d0    ! A(1,2)
  A(5)  =  1.0d0    ! A(2,2)
  A(6)  = -0.7d0    ! A(3,2)
  A(7)  =  0.8d0    ! A(1,3)
  A(8)  = -0.3d0    ! A(2,3)
  A(9)  =  1.5d0    ! A(3,3)

  X(1:3) = 0.0d0
  Y(1:3) = 0.0d0
  TAUQ(1) = 0.0d0
  TAUP(1) = 0.0d0
  D(1) = 0.0d0
  E(1) = 0.0d0

  call dlabrd(3, 3, 1, A, 3, D, E, TAUQ, TAUP, X, 3, Y, 3)

  call begin_test('nb1_3x3')
  call print_array('A', A, 3*3)
  call print_array('D', D, 1)
  call print_array('E', E, 1)
  call print_array('TAUQ', TAUQ, 1)
  call print_array('TAUP', TAUP, 1)
  call print_array('X', X, 3*1)
  call print_array('Y', Y, 3*1)
  call end_test()

  ! -------------------------------------------------------
  ! Test 6: NB=1, M < N (2x3)
  ! -------------------------------------------------------
  A = 0.0d0
  A(1)  =  1.5d0    ! A(1,1)
  A(2)  = -0.8d0    ! A(2,1)
  A(3)  =  0.6d0    ! A(1,2)
  A(4)  =  1.0d0    ! A(2,2)
  A(5)  = -0.4d0    ! A(1,3)
  A(6)  =  0.2d0    ! A(2,3)

  X(1:2) = 0.0d0
  Y(1:3) = 0.0d0
  TAUQ(1) = 0.0d0
  TAUP(1) = 0.0d0
  D(1) = 0.0d0
  E(1) = 0.0d0

  call dlabrd(2, 3, 1, A, 2, D, E, TAUQ, TAUP, X, 2, Y, 3)

  call begin_test('nb1_m_lt_n_2x3')
  call print_array('A', A, 2*3)
  call print_array('D', D, 1)
  call print_array('E', E, 1)
  call print_array('TAUQ', TAUQ, 1)
  call print_array('TAUP', TAUP, 1)
  call print_array('X', X, 2*1)
  call print_array('Y', Y, 3*1)
  call end_test()

end program
