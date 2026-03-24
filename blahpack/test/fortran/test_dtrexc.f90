program test_dtrexc
  use test_utils
  implicit none

  integer, parameter :: MAXN = 8
  double precision :: T(MAXN,MAXN), Q(MAXN,MAXN), WORK(MAXN)
  double precision :: T_packed(MAXN*MAXN), Q_packed(MAXN*MAXN)
  integer :: IFST, ILST, INFO, N, i, j

  ! ==========================================================================
  ! Test 1: swap 1x1 forward (existing test), COMPQ='V'
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

  IFST = 1
  ILST = 4
  call DTREXC('V', N, T, MAXN, Q, MAXN, IFST, ILST, WORK, INFO)

  do j = 1, N
    do i = 1, N
      T_packed((j-1)*N + i) = T(i, j)
      Q_packed((j-1)*N + i) = Q(i, j)
    end do
  end do

  call begin_test('swap 1x1 forward')
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call print_array('T', T_packed, N*N)
  call print_array('Q', Q_packed, N*N)
  call end_test()

  ! ==========================================================================
  ! Test 2: N=1 quick return
  ! ==========================================================================
  T(1,1) = 5.0d0
  Q(1,1) = 1.0d0
  call DTREXC('V', 1, T, MAXN, Q, MAXN, 1, 1, WORK, INFO)
  call begin_test('N=1 quick return')
  call print_int('info', INFO)
  call end_test()

  ! ==========================================================================
  ! Test 3: swap 1x1 backward, COMPQ='V'
  ! Move eigenvalue at position 4 backward to position 1
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

  IFST = 4
  ILST = 1
  call DTREXC('V', N, T, MAXN, Q, MAXN, IFST, ILST, WORK, INFO)

  do j = 1, N
    do i = 1, N
      T_packed((j-1)*N + i) = T(i, j)
      Q_packed((j-1)*N + i) = Q(i, j)
    end do
  end do

  call begin_test('swap 1x1 backward')
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call print_array('T', T_packed, N*N)
  call print_array('Q', Q_packed, N*N)
  call end_test()

  ! ==========================================================================
  ! Test 4: swap 1x1 forward, COMPQ='N' (no Q update)
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

  IFST = 1
  ILST = 4
  call DTREXC('N', N, T, MAXN, Q, MAXN, IFST, ILST, WORK, INFO)

  do j = 1, N
    do i = 1, N
      T_packed((j-1)*N + i) = T(i, j)
      Q_packed((j-1)*N + i) = Q(i, j)
    end do
  end do

  call begin_test('swap 1x1 forward compq_N')
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call print_array('T', T_packed, N*N)
  call print_array('Q', Q_packed, N*N)
  call end_test()

  ! ==========================================================================
  ! Test 5: forward move of 2x2 block across 1x1 blocks
  ! 2x2 block at rows 1-2, move to position 4
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

  IFST = 1
  ILST = 4
  call DTREXC('V', N, T, MAXN, Q, MAXN, IFST, ILST, WORK, INFO)

  do j = 1, N
    do i = 1, N
      T_packed((j-1)*N + i) = T(i, j)
      Q_packed((j-1)*N + i) = Q(i, j)
    end do
  end do

  call begin_test('forward 2x2 block')
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call print_array('T', T_packed, N*N)
  call print_array('Q', Q_packed, N*N)
  call end_test()

  ! ==========================================================================
  ! Test 6: backward move of 2x2 block
  ! 2x2 block at rows 4-5, move backward to position 1
  ! ==========================================================================
  N = 5
  T = 0.0d0
  T(1,1) = 5.0d0; T(1,2) = 0.5d0; T(1,3) = 0.2d0; T(1,4) = 0.1d0; T(1,5) = 0.3d0
  T(2,2) = 1.0d0; T(2,3) = 0.6d0; T(2,4) = 0.4d0; T(2,5) = 0.15d0
  T(3,3) = 0.5d0; T(3,4) = 0.9d0; T(3,5) = 0.2d0
  ! 2x2 block: eigenvalues 3 +/- 2i
  T(4,4) = 3.0d0; T(4,5) = 2.0d0
  T(5,4) = -2.0d0; T(5,5) = 3.0d0

  Q = 0.0d0
  do i = 1, N
    Q(i,i) = 1.0d0
  end do

  IFST = 4
  ILST = 1
  call DTREXC('V', N, T, MAXN, Q, MAXN, IFST, ILST, WORK, INFO)

  do j = 1, N
    do i = 1, N
      T_packed((j-1)*N + i) = T(i, j)
      Q_packed((j-1)*N + i) = Q(i, j)
    end do
  end do

  call begin_test('backward 2x2 block')
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call print_array('T', T_packed, N*N)
  call print_array('Q', Q_packed, N*N)
  call end_test()

  ! ==========================================================================
  ! Test 7: forward move across a 2x2 block
  ! Move 1x1 at position 1 to position 4, passing a 2x2 block at rows 2-3
  ! ==========================================================================
  N = 5
  T = 0.0d0
  T(1,1) = 5.0d0
  T(1,2) = 0.5d0; T(1,3) = 0.2d0; T(1,4) = 0.1d0; T(1,5) = 0.3d0
  ! 2x2 block at rows 2-3
  T(2,2) = 3.0d0; T(2,3) = 2.0d0; T(2,4) = 0.6d0; T(2,5) = 0.4d0
  T(3,2) = -2.0d0; T(3,3) = 3.0d0; T(3,4) = 0.8d0; T(3,5) = 0.15d0
  T(4,4) = 1.0d0; T(4,5) = 0.9d0
  T(5,5) = 0.5d0

  Q = 0.0d0
  do i = 1, N
    Q(i,i) = 1.0d0
  end do

  IFST = 1
  ILST = 5
  call DTREXC('V', N, T, MAXN, Q, MAXN, IFST, ILST, WORK, INFO)

  do j = 1, N
    do i = 1, N
      T_packed((j-1)*N + i) = T(i, j)
      Q_packed((j-1)*N + i) = Q(i, j)
    end do
  end do

  call begin_test('forward 1x1 across 2x2')
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call print_array('T', T_packed, N*N)
  call print_array('Q', Q_packed, N*N)
  call end_test()

  ! ==========================================================================
  ! Test 8: backward move across a 2x2 block
  ! Move 1x1 at position 5 backward to position 1, passing 2x2 at rows 2-3
  ! ==========================================================================
  N = 5
  T = 0.0d0
  T(1,1) = 1.0d0; T(1,2) = 0.5d0; T(1,3) = 0.2d0; T(1,4) = 0.1d0; T(1,5) = 0.3d0
  ! 2x2 block at rows 2-3
  T(2,2) = 3.0d0; T(2,3) = 2.0d0; T(2,4) = 0.6d0; T(2,5) = 0.4d0
  T(3,2) = -2.0d0; T(3,3) = 3.0d0; T(3,4) = 0.8d0; T(3,5) = 0.15d0
  T(4,4) = 5.0d0; T(4,5) = 0.9d0
  T(5,5) = 0.5d0

  Q = 0.0d0
  do i = 1, N
    Q(i,i) = 1.0d0
  end do

  IFST = 5
  ILST = 1
  call DTREXC('V', N, T, MAXN, Q, MAXN, IFST, ILST, WORK, INFO)

  do j = 1, N
    do i = 1, N
      T_packed((j-1)*N + i) = T(i, j)
      Q_packed((j-1)*N + i) = Q(i, j)
    end do
  end do

  call begin_test('backward 1x1 across 2x2')
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call print_array('T', T_packed, N*N)
  call print_array('Q', Q_packed, N*N)
  call end_test()

  ! ==========================================================================
  ! Test 9: IFST == ILST (no-op)
  ! ==========================================================================
  N = 3
  T = 0.0d0
  T(1,1) = 4.0d0; T(1,2) = 1.0d0; T(1,3) = 0.5d0
  T(2,2) = 3.0d0; T(2,3) = 0.8d0
  T(3,3) = 2.0d0

  Q = 0.0d0
  do i = 1, N
    Q(i,i) = 1.0d0
  end do

  IFST = 2
  ILST = 2
  call DTREXC('V', N, T, MAXN, Q, MAXN, IFST, ILST, WORK, INFO)

  call begin_test('ifst_eq_ilst')
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call end_test()

  ! ==========================================================================
  ! Test 10: IFST points to second row of 2x2 block (adjusted)
  ! ==========================================================================
  N = 4
  T = 0.0d0
  T(1,1) = 3.0d0; T(1,2) = 2.0d0; T(1,3) = 0.5d0; T(1,4) = 0.2d0
  T(2,1) = -2.0d0; T(2,2) = 3.0d0; T(2,3) = 0.8d0; T(2,4) = 0.3d0
  T(3,3) = 5.0d0; T(3,4) = 0.6d0
  T(4,4) = 1.0d0

  Q = 0.0d0
  do i = 1, N
    Q(i,i) = 1.0d0
  end do

  IFST = 2   ! points to second row of 2x2 block -> adjusted to 1
  ILST = 4
  call DTREXC('V', N, T, MAXN, Q, MAXN, IFST, ILST, WORK, INFO)

  do j = 1, N
    do i = 1, N
      T_packed((j-1)*N + i) = T(i, j)
      Q_packed((j-1)*N + i) = Q(i, j)
    end do
  end do

  call begin_test('ifst_adjusted_2x2')
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call print_array('T', T_packed, N*N)
  call print_array('Q', Q_packed, N*N)
  call end_test()

  ! ==========================================================================
  ! Test 11: backward move, COMPQ='N'
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

  IFST = 3
  ILST = 1
  call DTREXC('N', N, T, MAXN, Q, MAXN, IFST, ILST, WORK, INFO)

  do j = 1, N
    do i = 1, N
      T_packed((j-1)*N + i) = T(i, j)
    end do
  end do

  call begin_test('backward 1x1 compq_N')
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call print_array('T', T_packed, N*N)
  call end_test()

  ! ==========================================================================
  ! Test 12: forward move of 2x2 across 2x2
  ! Two 2x2 blocks: swap them
  ! ==========================================================================
  N = 6
  T = 0.0d0
  ! 2x2 block at rows 1-2: eigenvalues 4 +/- 1i
  T(1,1) = 4.0d0; T(1,2) = 1.0d0
  T(2,1) = -1.0d0; T(2,2) = 4.0d0
  T(1,3) = 0.5d0; T(1,4) = 0.3d0; T(1,5) = 0.2d0; T(1,6) = 0.1d0
  T(2,3) = 0.8d0; T(2,4) = 0.4d0; T(2,5) = 0.25d0; T(2,6) = 0.15d0
  ! 2x2 block at rows 3-4: eigenvalues 2 +/- 3i
  T(3,3) = 2.0d0; T(3,4) = 3.0d0
  T(4,3) = -3.0d0; T(4,4) = 2.0d0
  T(3,5) = 0.6d0; T(3,6) = 0.35d0
  T(4,5) = 0.7d0; T(4,6) = 0.45d0
  T(5,5) = 1.0d0; T(5,6) = 0.9d0
  T(6,6) = 0.5d0

  Q = 0.0d0
  do i = 1, N
    Q(i,i) = 1.0d0
  end do

  IFST = 1
  ILST = 4
  call DTREXC('V', N, T, MAXN, Q, MAXN, IFST, ILST, WORK, INFO)

  do j = 1, N
    do i = 1, N
      T_packed((j-1)*N + i) = T(i, j)
      Q_packed((j-1)*N + i) = Q(i, j)
    end do
  end do

  call begin_test('forward 2x2 across 2x2')
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call print_array('T', T_packed, N*N)
  call print_array('Q', Q_packed, N*N)
  call end_test()

end program
