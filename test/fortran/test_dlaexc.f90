program test_dlaexc
  use test_utils
  implicit none

  integer, parameter :: MAXN = 8
  double precision :: T(MAXN,MAXN), Q(MAXN,MAXN), WORK(MAXN)
  double precision :: T_packed(MAXN*MAXN), Q_packed(MAXN*MAXN)
  integer :: INFO, N, i, j

  ! ==========================================================================
  ! Test 1: N1=1, N2=1, WANTQ=true, N=4, j1=2
  ! Swap two 1x1 blocks at positions 2 and 3 on diagonal
  ! ==========================================================================
  N = 4
  T = 0.0d0
  T(1,1) = 4.0d0; T(1,2) = 1.0d0; T(1,3) = 0.5d0; T(1,4) = 0.2d0
  T(2,2) = 3.0d0; T(2,3) = 0.8d0; T(2,4) = 0.3d0
  T(3,3) = 2.0d0; T(3,4) = 0.6d0
  T(4,4) = 1.0d0

  Q = 0.0d0
  do i = 1, N
    Q(i,i) = 1.0d0
  end do

  call DLAEXC(.TRUE., N, T, MAXN, Q, MAXN, 2, 1, 1, WORK, INFO)

  ! Pack NxN submatrix
  do j = 1, N
    do i = 1, N
      T_packed((j-1)*N + i) = T(i, j)
      Q_packed((j-1)*N + i) = Q(i, j)
    end do
  end do

  call begin_test('n1_1_n2_1_wantq_true')
  call print_int('info', INFO)
  call print_array('T', T_packed, N*N)
  call print_array('Q', Q_packed, N*N)
  call end_test()

  ! ==========================================================================
  ! Test 2: N1=1, N2=1, WANTQ=false
  ! ==========================================================================
  N = 4
  T = 0.0d0
  T(1,1) = 4.0d0; T(1,2) = 1.0d0; T(1,3) = 0.5d0; T(1,4) = 0.2d0
  T(2,2) = 3.0d0; T(2,3) = 0.8d0; T(2,4) = 0.3d0
  T(3,3) = 2.0d0; T(3,4) = 0.6d0
  T(4,4) = 1.0d0

  Q = 0.0d0
  do i = 1, N
    Q(i,i) = 1.0d0
  end do

  call DLAEXC(.FALSE., N, T, MAXN, Q, MAXN, 2, 1, 1, WORK, INFO)

  do j = 1, N
    do i = 1, N
      T_packed((j-1)*N + i) = T(i, j)
      Q_packed((j-1)*N + i) = Q(i, j)
    end do
  end do

  call begin_test('n1_1_n2_1_wantq_false')
  call print_int('info', INFO)
  call print_array('T', T_packed, N*N)
  call print_array('Q', Q_packed, N*N)
  call end_test()

  ! ==========================================================================
  ! Test 3: N1=1, N2=2, WANTQ=true
  ! T has a 2x2 block at rows 2-3 (complex eigenvalue pair)
  ! Swap 1x1 block at j1=1 past 2x2 block
  ! ==========================================================================
  N = 5
  T = 0.0d0
  T(1,1) = 5.0d0
  T(1,2) = 1.0d0; T(1,3) = 0.3d0; T(1,4) = 0.2d0; T(1,5) = 0.1d0
  ! 2x2 block: eigenvalues 3 +/- 2i
  T(2,2) = 3.0d0; T(2,3) = 2.0d0; T(2,4) = 0.5d0; T(2,5) = 0.4d0
  T(3,2) = -2.0d0; T(3,3) = 3.0d0; T(3,4) = 0.7d0; T(3,5) = 0.6d0
  T(4,4) = 1.0d0; T(4,5) = 0.9d0
  T(5,5) = 0.5d0

  Q = 0.0d0
  do i = 1, N
    Q(i,i) = 1.0d0
  end do

  call DLAEXC(.TRUE., N, T, MAXN, Q, MAXN, 1, 1, 2, WORK, INFO)

  do j = 1, N
    do i = 1, N
      T_packed((j-1)*N + i) = T(i, j)
      Q_packed((j-1)*N + i) = Q(i, j)
    end do
  end do

  call begin_test('n1_1_n2_2_wantq_true')
  call print_int('info', INFO)
  call print_array('T', T_packed, N*N)
  call print_array('Q', Q_packed, N*N)
  call end_test()

  ! ==========================================================================
  ! Test 4: N1=1, N2=2, WANTQ=false
  ! ==========================================================================
  N = 5
  T = 0.0d0
  T(1,1) = 5.0d0
  T(1,2) = 1.0d0; T(1,3) = 0.3d0; T(1,4) = 0.2d0; T(1,5) = 0.1d0
  T(2,2) = 3.0d0; T(2,3) = 2.0d0; T(2,4) = 0.5d0; T(2,5) = 0.4d0
  T(3,2) = -2.0d0; T(3,3) = 3.0d0; T(3,4) = 0.7d0; T(3,5) = 0.6d0
  T(4,4) = 1.0d0; T(4,5) = 0.9d0
  T(5,5) = 0.5d0

  Q = 0.0d0
  do i = 1, N
    Q(i,i) = 1.0d0
  end do

  call DLAEXC(.FALSE., N, T, MAXN, Q, MAXN, 1, 1, 2, WORK, INFO)

  do j = 1, N
    do i = 1, N
      T_packed((j-1)*N + i) = T(i, j)
    end do
  end do

  call begin_test('n1_1_n2_2_wantq_false')
  call print_int('info', INFO)
  call print_array('T', T_packed, N*N)
  call end_test()

  ! ==========================================================================
  ! Test 5: N1=2, N2=1, WANTQ=true
  ! 2x2 block at j1=1, swap with 1x1 at j1+2=3
  ! ==========================================================================
  N = 5
  T = 0.0d0
  ! 2x2 block: eigenvalues 3 +/- 2i
  T(1,1) = 3.0d0; T(1,2) = 2.0d0; T(1,3) = 0.5d0; T(1,4) = 0.2d0; T(1,5) = 0.1d0
  T(2,1) = -2.0d0; T(2,2) = 3.0d0; T(2,3) = 0.8d0; T(2,4) = 0.3d0; T(2,5) = 0.15d0
  T(3,3) = 5.0d0; T(3,4) = 0.6d0; T(3,5) = 0.4d0
  T(4,4) = 1.0d0; T(4,5) = 0.9d0
  T(5,5) = 0.5d0

  Q = 0.0d0
  do i = 1, N
    Q(i,i) = 1.0d0
  end do

  call DLAEXC(.TRUE., N, T, MAXN, Q, MAXN, 1, 2, 1, WORK, INFO)

  do j = 1, N
    do i = 1, N
      T_packed((j-1)*N + i) = T(i, j)
      Q_packed((j-1)*N + i) = Q(i, j)
    end do
  end do

  call begin_test('n1_2_n2_1_wantq_true')
  call print_int('info', INFO)
  call print_array('T', T_packed, N*N)
  call print_array('Q', Q_packed, N*N)
  call end_test()

  ! ==========================================================================
  ! Test 6: N1=2, N2=1, WANTQ=false
  ! ==========================================================================
  N = 5
  T = 0.0d0
  T(1,1) = 3.0d0; T(1,2) = 2.0d0; T(1,3) = 0.5d0; T(1,4) = 0.2d0; T(1,5) = 0.1d0
  T(2,1) = -2.0d0; T(2,2) = 3.0d0; T(2,3) = 0.8d0; T(2,4) = 0.3d0; T(2,5) = 0.15d0
  T(3,3) = 5.0d0; T(3,4) = 0.6d0; T(3,5) = 0.4d0
  T(4,4) = 1.0d0; T(4,5) = 0.9d0
  T(5,5) = 0.5d0

  Q = 0.0d0
  do i = 1, N
    Q(i,i) = 1.0d0
  end do

  call DLAEXC(.FALSE., N, T, MAXN, Q, MAXN, 1, 2, 1, WORK, INFO)

  do j = 1, N
    do i = 1, N
      T_packed((j-1)*N + i) = T(i, j)
    end do
  end do

  call begin_test('n1_2_n2_1_wantq_false')
  call print_int('info', INFO)
  call print_array('T', T_packed, N*N)
  call end_test()

  ! ==========================================================================
  ! Test 7: N1=2, N2=2, WANTQ=true
  ! Two adjacent 2x2 blocks
  ! ==========================================================================
  N = 6
  T = 0.0d0
  ! 2x2 block: eigenvalues 4 +/- 1i
  T(1,1) = 4.0d0; T(1,2) = 1.0d0; T(1,3) = 0.5d0; T(1,4) = 0.3d0; T(1,5) = 0.2d0; T(1,6) = 0.1d0
  T(2,1) = -1.0d0; T(2,2) = 4.0d0; T(2,3) = 0.8d0; T(2,4) = 0.4d0; T(2,5) = 0.25d0; T(2,6) = 0.15d0
  ! 2x2 block: eigenvalues 2 +/- 3i
  T(3,3) = 2.0d0; T(3,4) = 3.0d0; T(3,5) = 0.6d0; T(3,6) = 0.35d0
  T(4,3) = -3.0d0; T(4,4) = 2.0d0; T(4,5) = 0.7d0; T(4,6) = 0.45d0
  T(5,5) = 1.0d0; T(5,6) = 0.9d0
  T(6,6) = 0.5d0

  Q = 0.0d0
  do i = 1, N
    Q(i,i) = 1.0d0
  end do

  call DLAEXC(.TRUE., N, T, MAXN, Q, MAXN, 1, 2, 2, WORK, INFO)

  do j = 1, N
    do i = 1, N
      T_packed((j-1)*N + i) = T(i, j)
      Q_packed((j-1)*N + i) = Q(i, j)
    end do
  end do

  call begin_test('n1_2_n2_2_wantq_true')
  call print_int('info', INFO)
  call print_array('T', T_packed, N*N)
  call print_array('Q', Q_packed, N*N)
  call end_test()

  ! ==========================================================================
  ! Test 8: N1=2, N2=2, WANTQ=false
  ! ==========================================================================
  N = 6
  T = 0.0d0
  T(1,1) = 4.0d0; T(1,2) = 1.0d0; T(1,3) = 0.5d0; T(1,4) = 0.3d0; T(1,5) = 0.2d0; T(1,6) = 0.1d0
  T(2,1) = -1.0d0; T(2,2) = 4.0d0; T(2,3) = 0.8d0; T(2,4) = 0.4d0; T(2,5) = 0.25d0; T(2,6) = 0.15d0
  T(3,3) = 2.0d0; T(3,4) = 3.0d0; T(3,5) = 0.6d0; T(3,6) = 0.35d0
  T(4,3) = -3.0d0; T(4,4) = 2.0d0; T(4,5) = 0.7d0; T(4,6) = 0.45d0
  T(5,5) = 1.0d0; T(5,6) = 0.9d0
  T(6,6) = 0.5d0

  Q = 0.0d0
  do i = 1, N
    Q(i,i) = 1.0d0
  end do

  call DLAEXC(.FALSE., N, T, MAXN, Q, MAXN, 1, 2, 2, WORK, INFO)

  do j = 1, N
    do i = 1, N
      T_packed((j-1)*N + i) = T(i, j)
    end do
  end do

  call begin_test('n1_2_n2_2_wantq_false')
  call print_int('info', INFO)
  call print_array('T', T_packed, N*N)
  call end_test()

  ! ==========================================================================
  ! Test 9: Quick return N=0
  ! ==========================================================================
  call DLAEXC(.TRUE., 0, T, MAXN, Q, MAXN, 1, 1, 1, WORK, INFO)
  call begin_test('quick_return_n0')
  call print_int('info', INFO)
  call end_test()

  ! ==========================================================================
  ! Test 10: Quick return n1=0
  ! ==========================================================================
  call DLAEXC(.TRUE., 4, T, MAXN, Q, MAXN, 1, 0, 1, WORK, INFO)
  call begin_test('quick_return_n1_0')
  call print_int('info', INFO)
  call end_test()

  ! ==========================================================================
  ! Test 11: N1=1, N2=1 at the end of the matrix (j1=N-1, 1-based)
  ! Tests boundary condition where j3 > N
  ! ==========================================================================
  N = 3
  T = 0.0d0
  T(1,1) = 5.0d0; T(1,2) = 1.0d0; T(1,3) = 0.5d0
  T(2,2) = 3.0d0; T(2,3) = 0.8d0
  T(3,3) = 1.0d0

  Q = 0.0d0
  do i = 1, N
    Q(i,i) = 1.0d0
  end do

  call DLAEXC(.TRUE., N, T, MAXN, Q, MAXN, 2, 1, 1, WORK, INFO)

  do j = 1, N
    do i = 1, N
      T_packed((j-1)*N + i) = T(i, j)
      Q_packed((j-1)*N + i) = Q(i, j)
    end do
  end do

  call begin_test('n1_1_n2_1_boundary')
  call print_int('info', INFO)
  call print_array('T', T_packed, N*N)
  call print_array('Q', Q_packed, N*N)
  call end_test()

  ! ==========================================================================
  ! Test 12: N1=1, N2=1 at very start (j1=1)
  ! Tests j1-1=0 boundary in left rotation
  ! ==========================================================================
  N = 3
  T = 0.0d0
  T(1,1) = 5.0d0; T(1,2) = 1.0d0; T(1,3) = 0.5d0
  T(2,2) = 3.0d0; T(2,3) = 0.8d0
  T(3,3) = 1.0d0

  Q = 0.0d0
  do i = 1, N
    Q(i,i) = 1.0d0
  end do

  call DLAEXC(.TRUE., N, T, MAXN, Q, MAXN, 1, 1, 1, WORK, INFO)

  do j = 1, N
    do i = 1, N
      T_packed((j-1)*N + i) = T(i, j)
      Q_packed((j-1)*N + i) = Q(i, j)
    end do
  end do

  call begin_test('n1_1_n2_1_start')
  call print_int('info', INFO)
  call print_array('T', T_packed, N*N)
  call print_array('Q', Q_packed, N*N)
  call end_test()

end program
