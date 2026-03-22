program test_zlabrd
  use test_utils
  implicit none

  ! Test zlabrd: reduces first NB rows/columns to bidiagonal form
  !
  ! Test 1: M >= N case (5x4, NB=2) -> upper bidiagonal
  ! Test 2: M < N case (4x5, NB=2) -> lower bidiagonal
  ! Test 3: Edge case M=0 or N=0 (quick return)
  ! Test 4: NB=1 (single step)

  integer, parameter :: MAXM = 5, MAXN = 5, MAXNB = 2
  complex*16 :: A(MAXM * MAXN), X(MAXM * MAXNB), Y(MAXN * MAXNB)
  complex*16 :: TAUQ(MAXNB), TAUP(MAXNB)
  double precision :: D(MAXNB), E(MAXNB)

  double precision :: A_r(2 * MAXM * MAXN)
  double precision :: X_r(2 * MAXM * MAXNB)
  double precision :: Y_r(2 * MAXN * MAXNB)
  double precision :: TAUQ_r(2 * MAXNB), TAUP_r(2 * MAXNB)
  equivalence (A, A_r)
  equivalence (X, X_r)
  equivalence (Y, Y_r)
  equivalence (TAUQ, TAUQ_r)
  equivalence (TAUP, TAUP_r)

  ! -------------------------------------------------------
  ! Test 1: M=5, N=4, NB=2 (M >= N => upper bidiagonal)
  ! -------------------------------------------------------
  ! A is 5x4 column-major, LDA=5
  A = (0.0d0, 0.0d0)
  A(1)  = ( 1.0d0,  0.5d0)   ! A(1,1)
  A(2)  = ( 2.0d0, -1.0d0)   ! A(2,1)
  A(3)  = (-0.5d0,  0.3d0)   ! A(3,1)
  A(4)  = ( 0.7d0, -0.2d0)   ! A(4,1)
  A(5)  = ( 1.5d0,  0.8d0)   ! A(5,1)
  A(6)  = ( 0.3d0,  0.4d0)   ! A(1,2)
  A(7)  = (-1.0d0,  0.5d0)   ! A(2,2)
  A(8)  = ( 0.6d0, -0.7d0)   ! A(3,2)
  A(9)  = ( 1.2d0,  0.1d0)   ! A(4,2)
  A(10) = (-0.3d0,  0.9d0)   ! A(5,2)
  A(11) = ( 0.5d0, -0.1d0)   ! A(1,3)
  A(12) = ( 0.8d0,  0.2d0)   ! A(2,3)
  A(13) = (-0.4d0,  1.0d0)   ! A(3,3)
  A(14) = ( 0.2d0, -0.5d0)   ! A(4,3)
  A(15) = ( 1.1d0,  0.3d0)   ! A(5,3)
  A(16) = (-0.2d0,  0.6d0)   ! A(1,4)
  A(17) = ( 0.4d0, -0.3d0)   ! A(2,4)
  A(18) = ( 0.9d0,  0.1d0)   ! A(3,4)
  A(19) = (-0.6d0,  0.8d0)   ! A(4,4)
  A(20) = ( 0.3d0, -0.4d0)   ! A(5,4)

  X = (0.0d0, 0.0d0)
  Y = (0.0d0, 0.0d0)
  TAUQ = (0.0d0, 0.0d0)
  TAUP = (0.0d0, 0.0d0)
  D = 0.0d0
  E = 0.0d0

  call zlabrd(5, 4, 2, A, 5, D, E, TAUQ, TAUP, X, 5, Y, 4)

  call begin_test('m_ge_n_5x4_nb2')
  call print_array('A', A_r, 2*5*4)
  call print_array('D', D, 2)
  call print_array('E', E, 2)
  call print_array('TAUQ', TAUQ_r, 2*2)
  call print_array('TAUP', TAUP_r, 2*2)
  call print_array('X', X_r, 2*5*2)
  call print_array('Y', Y_r, 2*4*2)
  call end_test()

  ! -------------------------------------------------------
  ! Test 2: M=4, N=5, NB=2 (M < N => lower bidiagonal)
  ! -------------------------------------------------------
  ! A is 4x5 column-major, LDA=4
  A = (0.0d0, 0.0d0)
  A(1)  = ( 1.0d0,  0.5d0)   ! A(1,1)
  A(2)  = ( 2.0d0, -1.0d0)   ! A(2,1)
  A(3)  = (-0.5d0,  0.3d0)   ! A(3,1)
  A(4)  = ( 0.7d0, -0.2d0)   ! A(4,1)
  A(5)  = ( 0.3d0,  0.4d0)   ! A(1,2)
  A(6)  = (-1.0d0,  0.5d0)   ! A(2,2)
  A(7)  = ( 0.6d0, -0.7d0)   ! A(3,2)
  A(8)  = ( 1.2d0,  0.1d0)   ! A(4,2)
  A(9)  = ( 0.5d0, -0.1d0)   ! A(1,3)
  A(10) = ( 0.8d0,  0.2d0)   ! A(2,3)
  A(11) = (-0.4d0,  1.0d0)   ! A(3,3)
  A(12) = ( 0.2d0, -0.5d0)   ! A(4,3)
  A(13) = (-0.2d0,  0.6d0)   ! A(1,4)
  A(14) = ( 0.4d0, -0.3d0)   ! A(2,4)
  A(15) = ( 0.9d0,  0.1d0)   ! A(3,4)
  A(16) = (-0.6d0,  0.8d0)   ! A(4,4)
  A(17) = ( 1.5d0,  0.8d0)   ! A(1,5)
  A(18) = (-0.3d0,  0.9d0)   ! A(2,5)
  A(19) = ( 1.1d0,  0.3d0)   ! A(3,5)
  A(20) = ( 0.3d0, -0.4d0)   ! A(4,5)

  X = (0.0d0, 0.0d0)
  Y = (0.0d0, 0.0d0)
  TAUQ = (0.0d0, 0.0d0)
  TAUP = (0.0d0, 0.0d0)
  D = 0.0d0
  E = 0.0d0

  call zlabrd(4, 5, 2, A, 4, D, E, TAUQ, TAUP, X, 4, Y, 5)

  call begin_test('m_lt_n_4x5_nb2')
  call print_array('A', A_r, 2*4*5)
  call print_array('D', D, 2)
  call print_array('E', E, 2)
  call print_array('TAUQ', TAUQ_r, 2*2)
  call print_array('TAUP', TAUP_r, 2*2)
  call print_array('X', X_r, 2*4*2)
  call print_array('Y', Y_r, 2*5*2)
  call end_test()

  ! -------------------------------------------------------
  ! Test 3: Quick return (M=0)
  ! -------------------------------------------------------
  A = (0.0d0, 0.0d0)
  X = (0.0d0, 0.0d0)
  Y = (0.0d0, 0.0d0)
  TAUQ = (0.0d0, 0.0d0)
  TAUP = (0.0d0, 0.0d0)
  D = 0.0d0
  E = 0.0d0

  call zlabrd(0, 4, 2, A, 1, D, E, TAUQ, TAUP, X, 1, Y, 4)

  call begin_test('quick_return_m0')
  call print_array('D', D, 2)
  call print_array('E', E, 2)
  call end_test()

  ! -------------------------------------------------------
  ! Test 4: NB=1, M >= N (3x3)
  ! -------------------------------------------------------
  A = (0.0d0, 0.0d0)
  A(1) = ( 2.0d0,  1.0d0)   ! A(1,1)
  A(2) = (-1.0d0,  0.5d0)   ! A(2,1)
  A(3) = ( 0.3d0, -0.2d0)   ! A(3,1)
  A(4) = ( 0.5d0, -0.4d0)   ! A(1,2)
  A(5) = ( 1.0d0,  0.3d0)   ! A(2,2)
  A(6) = (-0.7d0,  0.6d0)   ! A(3,2)
  A(7) = ( 0.8d0,  0.2d0)   ! A(1,3)
  A(8) = (-0.3d0, -0.1d0)   ! A(2,3)
  A(9) = ( 1.5d0, -0.5d0)   ! A(3,3)

  X(1:3) = (0.0d0, 0.0d0)
  Y(1:3) = (0.0d0, 0.0d0)
  TAUQ(1) = (0.0d0, 0.0d0)
  TAUP(1) = (0.0d0, 0.0d0)
  D(1) = 0.0d0
  E(1) = 0.0d0

  call zlabrd(3, 3, 1, A, 3, D, E, TAUQ, TAUP, X, 3, Y, 3)

  call begin_test('nb1_3x3')
  call print_array('A', A_r, 2*3*3)
  call print_array('D', D, 1)
  call print_array('E', E, 1)
  call print_array('TAUQ', TAUQ_r, 2*1)
  call print_array('TAUP', TAUP_r, 2*1)
  call print_array('X', X_r, 2*3*1)
  call print_array('Y', Y_r, 2*3*1)
  call end_test()

  ! -------------------------------------------------------
  ! Test 5: NB=1, M < N (2x3)
  ! -------------------------------------------------------
  A = (0.0d0, 0.0d0)
  A(1) = ( 1.5d0,  0.5d0)   ! A(1,1)
  A(2) = (-0.8d0,  0.3d0)   ! A(2,1)
  A(3) = ( 0.6d0, -0.2d0)   ! A(1,2)
  A(4) = ( 1.0d0,  0.7d0)   ! A(2,2)
  A(5) = (-0.4d0,  0.9d0)   ! A(1,3)
  A(6) = ( 0.2d0, -0.6d0)   ! A(2,3)

  X(1:2) = (0.0d0, 0.0d0)
  Y(1:3) = (0.0d0, 0.0d0)
  TAUQ(1) = (0.0d0, 0.0d0)
  TAUP(1) = (0.0d0, 0.0d0)
  D(1) = 0.0d0
  E(1) = 0.0d0

  call zlabrd(2, 3, 1, A, 2, D, E, TAUQ, TAUP, X, 2, Y, 3)

  call begin_test('nb1_m_lt_n_2x3')
  call print_array('A', A_r, 2*2*3)
  call print_array('D', D, 1)
  call print_array('E', E, 1)
  call print_array('TAUQ', TAUQ_r, 2*1)
  call print_array('TAUP', TAUP_r, 2*1)
  call print_array('X', X_r, 2*2*1)
  call print_array('Y', Y_r, 2*3*1)
  call end_test()

end program
