program test_dtgexc
  use test_utils
  implicit none

  integer, parameter :: MAXN = 8
  double precision :: A(MAXN,MAXN), B(MAXN,MAXN)
  double precision :: Q(MAXN,MAXN), Z(MAXN,MAXN)
  double precision :: WORK(200)
  integer :: IFST, ILST, INFO, N, LWORK, i, j

  LWORK = 200

  ! ==========================================================================
  ! Test 1: Move 1x1 block forward, with Q and Z
  ! ==========================================================================
  N = 4
  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0

  ! Upper triangular A (Schur form, all 1x1 blocks)
  A(1,1) = 1.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0; A(1,4) = 0.2d0
  A(2,2) = 2.0d0; A(2,3) = 0.4d0; A(2,4) = 0.1d0
  A(3,3) = 3.0d0; A(3,4) = 0.6d0
  A(4,4) = 4.0d0

  ! Upper triangular B
  B(1,1) = 1.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0; B(1,4) = 0.05d0
  B(2,2) = 1.5d0; B(2,3) = 0.3d0; B(2,4) = 0.15d0
  B(3,3) = 2.0d0; B(3,4) = 0.4d0
  B(4,4) = 2.5d0

  ! Identity Q and Z
  do i = 1, N
    Q(i,i) = 1.0d0
    Z(i,i) = 1.0d0
  end do

  IFST = 1
  ILST = 4
  call DTGEXC(.TRUE., .TRUE., N, A, MAXN, B, MAXN, Q, MAXN, Z, MAXN, &
              IFST, ILST, WORK, LWORK, INFO)

  call begin_test('move_1x1_forward_qz')
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call print_matrix('A', A, MAXN, N, N)
  call print_matrix('B', B, MAXN, N, N)
  call print_matrix('Q', Q, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call end_test()

  ! ==========================================================================
  ! Test 2: Move 1x1 block backward, with Q and Z
  ! ==========================================================================
  N = 4
  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0

  A(1,1) = 1.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0; A(1,4) = 0.2d0
  A(2,2) = 2.0d0; A(2,3) = 0.4d0; A(2,4) = 0.1d0
  A(3,3) = 3.0d0; A(3,4) = 0.6d0
  A(4,4) = 4.0d0

  B(1,1) = 1.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0; B(1,4) = 0.05d0
  B(2,2) = 1.5d0; B(2,3) = 0.3d0; B(2,4) = 0.15d0
  B(3,3) = 2.0d0; B(3,4) = 0.4d0
  B(4,4) = 2.5d0

  do i = 1, N
    Q(i,i) = 1.0d0
    Z(i,i) = 1.0d0
  end do

  IFST = 4
  ILST = 1
  call DTGEXC(.TRUE., .TRUE., N, A, MAXN, B, MAXN, Q, MAXN, Z, MAXN, &
              IFST, ILST, WORK, LWORK, INFO)

  call begin_test('move_1x1_backward_qz')
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call print_matrix('A', A, MAXN, N, N)
  call print_matrix('B', B, MAXN, N, N)
  call print_matrix('Q', Q, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call end_test()

  ! ==========================================================================
  ! Test 3: Move 2x2 block forward, with Q and Z
  ! A has a 2x2 block at rows 1-2 (complex eigenvalue pair)
  ! ==========================================================================
  N = 4
  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0

  ! 2x2 block at (1:2,1:2) with complex eigenvalues
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 0.3d0; A(1,4) = 0.2d0
  A(2,1) = -0.5d0; A(2,2) = 1.0d0; A(2,3) = 0.4d0; A(2,4) = 0.1d0
  A(3,3) = 3.0d0; A(3,4) = 0.6d0
  A(4,4) = 4.0d0

  B(1,1) = 1.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0; B(1,4) = 0.05d0
  B(2,2) = 1.5d0; B(2,3) = 0.3d0; B(2,4) = 0.15d0
  B(3,3) = 2.0d0; B(3,4) = 0.4d0
  B(4,4) = 2.5d0

  do i = 1, N
    Q(i,i) = 1.0d0
    Z(i,i) = 1.0d0
  end do

  IFST = 1
  ILST = 4
  call DTGEXC(.TRUE., .TRUE., N, A, MAXN, B, MAXN, Q, MAXN, Z, MAXN, &
              IFST, ILST, WORK, LWORK, INFO)

  call begin_test('move_2x2_forward_qz')
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call print_matrix('A', A, MAXN, N, N)
  call print_matrix('B', B, MAXN, N, N)
  call print_matrix('Q', Q, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call end_test()

  ! ==========================================================================
  ! Test 4: Move 2x2 block backward
  ! A has a 2x2 block at rows 3-4 (complex eigenvalue pair)
  ! ==========================================================================
  N = 4
  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0

  A(1,1) = 1.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0; A(1,4) = 0.2d0
  A(2,2) = 2.0d0; A(2,3) = 0.4d0; A(2,4) = 0.1d0
  ! 2x2 block at (3:4,3:4)
  A(3,3) = 3.0d0; A(3,4) = 2.0d0
  A(4,3) = -0.5d0; A(4,4) = 3.0d0

  B(1,1) = 1.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0; B(1,4) = 0.05d0
  B(2,2) = 1.5d0; B(2,3) = 0.3d0; B(2,4) = 0.15d0
  B(3,3) = 2.0d0; B(3,4) = 0.4d0
  B(4,4) = 2.5d0

  do i = 1, N
    Q(i,i) = 1.0d0
    Z(i,i) = 1.0d0
  end do

  IFST = 3
  ILST = 1
  call DTGEXC(.TRUE., .TRUE., N, A, MAXN, B, MAXN, Q, MAXN, Z, MAXN, &
              IFST, ILST, WORK, LWORK, INFO)

  call begin_test('move_2x2_backward_qz')
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call print_matrix('A', A, MAXN, N, N)
  call print_matrix('B', B, MAXN, N, N)
  call print_matrix('Q', Q, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call end_test()

  ! ==========================================================================
  ! Test 5: No Q/Z accumulation
  ! ==========================================================================
  N = 3
  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0

  A(1,1) = 1.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0
  A(2,2) = 2.0d0; A(2,3) = 0.4d0
  A(3,3) = 3.0d0

  B(1,1) = 1.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0
  B(2,2) = 1.5d0; B(2,3) = 0.3d0
  B(3,3) = 2.0d0

  IFST = 1
  ILST = 3
  call DTGEXC(.FALSE., .FALSE., N, A, MAXN, B, MAXN, Q, MAXN, Z, MAXN, &
              IFST, ILST, WORK, LWORK, INFO)

  call begin_test('move_1x1_no_qz')
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call print_matrix('A', A, MAXN, N, N)
  call print_matrix('B', B, MAXN, N, N)
  call end_test()

  ! ==========================================================================
  ! Test 6: IFST == ILST (no-op)
  ! ==========================================================================
  N = 3
  A = 0.0d0; B = 0.0d0

  A(1,1) = 1.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0
  A(2,2) = 2.0d0; A(2,3) = 0.4d0
  A(3,3) = 3.0d0

  B(1,1) = 1.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0
  B(2,2) = 1.5d0; B(2,3) = 0.3d0
  B(3,3) = 2.0d0

  IFST = 2
  ILST = 2
  call DTGEXC(.FALSE., .FALSE., N, A, MAXN, B, MAXN, Q, MAXN, Z, MAXN, &
              IFST, ILST, WORK, LWORK, INFO)

  call begin_test('noop_ifst_eq_ilst')
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call print_matrix('A', A, MAXN, N, N)
  call print_matrix('B', B, MAXN, N, N)
  call end_test()

  ! ==========================================================================
  ! Test 7: N=1 quick return
  ! ==========================================================================
  N = 1
  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0
  A(1,1) = 5.0d0
  B(1,1) = 3.0d0
  Q(1,1) = 1.0d0
  Z(1,1) = 1.0d0

  IFST = 1
  ILST = 1
  call DTGEXC(.TRUE., .TRUE., N, A, MAXN, B, MAXN, Q, MAXN, Z, MAXN, &
              IFST, ILST, WORK, LWORK, INFO)

  call begin_test('n1_quick_return')
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call end_test()

  ! ==========================================================================
  ! Test 8: Larger matrix, move block from middle
  ! ==========================================================================
  N = 6
  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0

  A(1,1) = 1.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0; A(1,4) = 0.2d0; A(1,5) = 0.1d0; A(1,6) = 0.05d0
  A(2,2) = 2.0d0; A(2,3) = 0.4d0; A(2,4) = 0.1d0; A(2,5) = 0.2d0; A(2,6) = 0.1d0
  A(3,3) = 3.0d0; A(3,4) = 0.6d0; A(3,5) = 0.3d0; A(3,6) = 0.15d0
  A(4,4) = 4.0d0; A(4,5) = 0.5d0; A(4,6) = 0.2d0
  A(5,5) = 5.0d0; A(5,6) = 0.4d0
  A(6,6) = 6.0d0

  B(1,1) = 1.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0; B(1,4) = 0.05d0; B(1,5) = 0.03d0; B(1,6) = 0.01d0
  B(2,2) = 1.5d0; B(2,3) = 0.3d0; B(2,4) = 0.15d0; B(2,5) = 0.1d0; B(2,6) = 0.05d0
  B(3,3) = 2.0d0; B(3,4) = 0.4d0; B(3,5) = 0.2d0; B(3,6) = 0.1d0
  B(4,4) = 2.5d0; B(4,5) = 0.3d0; B(4,6) = 0.15d0
  B(5,5) = 3.0d0; B(5,6) = 0.4d0
  B(6,6) = 3.5d0

  do i = 1, N
    Q(i,i) = 1.0d0
    Z(i,i) = 1.0d0
  end do

  IFST = 2
  ILST = 5
  call DTGEXC(.TRUE., .TRUE., N, A, MAXN, B, MAXN, Q, MAXN, Z, MAXN, &
              IFST, ILST, WORK, LWORK, INFO)

  call begin_test('move_1x1_forward_6x6')
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call print_matrix('A', A, MAXN, N, N)
  call print_matrix('B', B, MAXN, N, N)
  call print_matrix('Q', Q, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call end_test()

  ! ==========================================================================
  ! Test 9: IFST points to second row of 2x2 block (should adjust)
  ! ==========================================================================
  N = 4
  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0

  ! 2x2 block at (1:2,1:2)
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 0.3d0; A(1,4) = 0.2d0
  A(2,1) = -0.5d0; A(2,2) = 1.0d0; A(2,3) = 0.4d0; A(2,4) = 0.1d0
  A(3,3) = 3.0d0; A(3,4) = 0.6d0
  A(4,4) = 4.0d0

  B(1,1) = 1.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0; B(1,4) = 0.05d0
  B(2,2) = 1.5d0; B(2,3) = 0.3d0; B(2,4) = 0.15d0
  B(3,3) = 2.0d0; B(3,4) = 0.4d0
  B(4,4) = 2.5d0

  do i = 1, N
    Q(i,i) = 1.0d0
    Z(i,i) = 1.0d0
  end do

  ! Point IFST to second row of the 2x2 block
  IFST = 2
  ILST = 4
  call DTGEXC(.TRUE., .TRUE., N, A, MAXN, B, MAXN, Q, MAXN, Z, MAXN, &
              IFST, ILST, WORK, LWORK, INFO)

  call begin_test('ifst_second_row_2x2')
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call print_matrix('A', A, MAXN, N, N)
  call print_matrix('B', B, MAXN, N, N)
  call print_matrix('Q', Q, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call end_test()

  ! ==========================================================================
  ! Test 10: Move 1x1 forward across 2x2 block (nbnext=2 forward path)
  ! A has 1x1 at row 1, 2x2 at rows 2-3, 1x1 at row 4
  ! ==========================================================================
  N = 4
  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0

  A(1,1) = 1.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0; A(1,4) = 0.2d0
  ! 2x2 block at (2:3,2:3)
  A(2,2) = 2.0d0; A(2,3) = 1.5d0; A(2,4) = 0.1d0
  A(3,2) = -0.8d0; A(3,3) = 2.0d0; A(3,4) = 0.6d0
  A(4,4) = 4.0d0

  B(1,1) = 1.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0; B(1,4) = 0.05d0
  B(2,2) = 1.5d0; B(2,3) = 0.3d0; B(2,4) = 0.15d0
  B(3,3) = 2.0d0; B(3,4) = 0.4d0
  B(4,4) = 2.5d0

  do i = 1, N
    Q(i,i) = 1.0d0
    Z(i,i) = 1.0d0
  end do

  IFST = 1
  ILST = 4
  call DTGEXC(.TRUE., .TRUE., N, A, MAXN, B, MAXN, Q, MAXN, Z, MAXN, &
              IFST, ILST, WORK, LWORK, INFO)

  call begin_test('move_1x1_fwd_across_2x2')
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call print_matrix('A', A, MAXN, N, N)
  call print_matrix('B', B, MAXN, N, N)
  call print_matrix('Q', Q, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call end_test()

  ! ==========================================================================
  ! Test 11: Move 1x1 backward across 2x2 block (nbnext=2 backward path)
  ! ==========================================================================
  N = 4
  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0

  ! 2x2 block at (1:2,1:2)
  A(1,1) = 2.0d0; A(1,2) = 1.5d0; A(1,3) = 0.3d0; A(1,4) = 0.2d0
  A(2,1) = -0.8d0; A(2,2) = 2.0d0; A(2,3) = 0.4d0; A(2,4) = 0.1d0
  A(3,3) = 3.0d0; A(3,4) = 0.6d0
  A(4,4) = 4.0d0

  B(1,1) = 1.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0; B(1,4) = 0.05d0
  B(2,2) = 1.5d0; B(2,3) = 0.3d0; B(2,4) = 0.15d0
  B(3,3) = 2.0d0; B(3,4) = 0.4d0
  B(4,4) = 2.5d0

  do i = 1, N
    Q(i,i) = 1.0d0
    Z(i,i) = 1.0d0
  end do

  IFST = 4
  ILST = 1
  call DTGEXC(.TRUE., .TRUE., N, A, MAXN, B, MAXN, Q, MAXN, Z, MAXN, &
              IFST, ILST, WORK, LWORK, INFO)

  call begin_test('move_1x1_bwd_across_2x2')
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call print_matrix('A', A, MAXN, N, N)
  call print_matrix('B', B, MAXN, N, N)
  call print_matrix('Q', Q, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call end_test()

  ! ==========================================================================
  ! Test 12: Move 1x1 forward, ILST targets 2x2 block (nbl=2 adjustment)
  ! A has 1x1 blocks at rows 1,2 and 2x2 block at rows 3-4
  ! ==========================================================================
  N = 4
  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0

  A(1,1) = 1.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0; A(1,4) = 0.2d0
  A(2,2) = 2.0d0; A(2,3) = 0.4d0; A(2,4) = 0.1d0
  ! 2x2 block at (3:4,3:4)
  A(3,3) = 3.0d0; A(3,4) = 1.5d0
  A(4,3) = -0.8d0; A(4,4) = 3.0d0

  B(1,1) = 1.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0; B(1,4) = 0.05d0
  B(2,2) = 1.5d0; B(2,3) = 0.3d0; B(2,4) = 0.15d0
  B(3,3) = 2.0d0; B(3,4) = 0.4d0
  B(4,4) = 2.5d0

  do i = 1, N
    Q(i,i) = 1.0d0
    Z(i,i) = 1.0d0
  end do

  ! Move 1x1 at row 1 forward to 2x2 target at row 3
  ! nbf=1, nbl=2 => ilst adjusted from 3 to 4+1=5... wait, that's out of bounds
  ! Actually: ilst points to first row of 2x2 target, nbf=1, nbl=2 => ilst += 1
  IFST = 1
  ILST = 3
  call DTGEXC(.TRUE., .TRUE., N, A, MAXN, B, MAXN, Q, MAXN, Z, MAXN, &
              IFST, ILST, WORK, LWORK, INFO)

  call begin_test('move_1x1_fwd_nbl2_adjust')
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call print_matrix('A', A, MAXN, N, N)
  call print_matrix('B', B, MAXN, N, N)
  call print_matrix('Q', Q, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call end_test()

  ! ==========================================================================
  ! Test 13: ILST points to second row of 2x2 block (ilst adjustment)
  ! ==========================================================================
  N = 4
  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0

  A(1,1) = 1.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0; A(1,4) = 0.2d0
  A(2,2) = 2.0d0; A(2,3) = 0.4d0; A(2,4) = 0.1d0
  ! 2x2 block at (3:4,3:4)
  A(3,3) = 3.0d0; A(3,4) = 1.5d0
  A(4,3) = -0.8d0; A(4,4) = 3.0d0

  B(1,1) = 1.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0; B(1,4) = 0.05d0
  B(2,2) = 1.5d0; B(2,3) = 0.3d0; B(2,4) = 0.15d0
  B(3,3) = 2.0d0; B(3,4) = 0.4d0
  B(4,4) = 2.5d0

  do i = 1, N
    Q(i,i) = 1.0d0
    Z(i,i) = 1.0d0
  end do

  ! Point ILST to second row of the 2x2 block
  IFST = 1
  ILST = 4
  call DTGEXC(.TRUE., .TRUE., N, A, MAXN, B, MAXN, Q, MAXN, Z, MAXN, &
              IFST, ILST, WORK, LWORK, INFO)

  call begin_test('ilst_second_row_2x2')
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call print_matrix('A', A, MAXN, N, N)
  call print_matrix('B', B, MAXN, N, N)
  call print_matrix('Q', Q, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call end_test()

  ! ==========================================================================
  ! Test 14: 6x6 with 2x2 blocks - exercise forward split (nbf=3) path
  ! 2x2 block at (1:2), 2x2 at (3:4), 1x1 at 5, 1x1 at 6
  ! Move 2x2 from rows 1-2 forward to row 5
  ! ==========================================================================
  N = 6
  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0

  ! 2x2 block at rows 1-2
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 0.3d0; A(1,4) = 0.2d0; A(1,5) = 0.1d0; A(1,6) = 0.05d0
  A(2,1) = -0.5d0; A(2,2) = 1.0d0; A(2,3) = 0.4d0; A(2,4) = 0.1d0; A(2,5) = 0.2d0; A(2,6) = 0.1d0
  ! 2x2 block at rows 3-4
  A(3,3) = 3.0d0; A(3,4) = 1.5d0; A(3,5) = 0.3d0; A(3,6) = 0.15d0
  A(4,3) = -0.8d0; A(4,4) = 3.0d0; A(4,5) = 0.2d0; A(4,6) = 0.1d0
  A(5,5) = 5.0d0; A(5,6) = 0.4d0
  A(6,6) = 6.0d0

  B(1,1) = 1.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0; B(1,4) = 0.05d0; B(1,5) = 0.03d0; B(1,6) = 0.01d0
  B(2,2) = 1.5d0; B(2,3) = 0.3d0; B(2,4) = 0.15d0; B(2,5) = 0.1d0; B(2,6) = 0.05d0
  B(3,3) = 2.0d0; B(3,4) = 0.4d0; B(3,5) = 0.2d0; B(3,6) = 0.1d0
  B(4,4) = 2.5d0; B(4,5) = 0.3d0; B(4,6) = 0.15d0
  B(5,5) = 3.0d0; B(5,6) = 0.4d0
  B(6,6) = 3.5d0

  do i = 1, N
    Q(i,i) = 1.0d0
    Z(i,i) = 1.0d0
  end do

  IFST = 1
  ILST = 5
  call DTGEXC(.TRUE., .TRUE., N, A, MAXN, B, MAXN, Q, MAXN, Z, MAXN, &
              IFST, ILST, WORK, LWORK, INFO)

  call begin_test('move_2x2_fwd_across_2x2_6x6')
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call print_matrix('A', A, MAXN, N, N)
  call print_matrix('B', B, MAXN, N, N)
  call print_matrix('Q', Q, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call end_test()

  ! ==========================================================================
  ! Test 15: 6x6, move 2x2 backward across 2x2
  ! 1x1 at 1, 1x1 at 2, 2x2 at 3-4, 2x2 at 5-6
  ! Move 2x2 from rows 5-6 backward to row 1
  ! ==========================================================================
  N = 6
  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0

  A(1,1) = 1.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0; A(1,4) = 0.2d0; A(1,5) = 0.1d0; A(1,6) = 0.05d0
  A(2,2) = 2.0d0; A(2,3) = 0.4d0; A(2,4) = 0.1d0; A(2,5) = 0.2d0; A(2,6) = 0.1d0
  ! 2x2 block at rows 3-4
  A(3,3) = 3.0d0; A(3,4) = 1.5d0; A(3,5) = 0.3d0; A(3,6) = 0.15d0
  A(4,3) = -0.8d0; A(4,4) = 3.0d0; A(4,5) = 0.2d0; A(4,6) = 0.1d0
  ! 2x2 block at rows 5-6
  A(5,5) = 5.0d0; A(5,6) = 2.0d0
  A(6,5) = -0.3d0; A(6,6) = 5.0d0

  B(1,1) = 1.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0; B(1,4) = 0.05d0; B(1,5) = 0.03d0; B(1,6) = 0.01d0
  B(2,2) = 1.5d0; B(2,3) = 0.3d0; B(2,4) = 0.15d0; B(2,5) = 0.1d0; B(2,6) = 0.05d0
  B(3,3) = 2.0d0; B(3,4) = 0.4d0; B(3,5) = 0.2d0; B(3,6) = 0.1d0
  B(4,4) = 2.5d0; B(4,5) = 0.3d0; B(4,6) = 0.15d0
  B(5,5) = 3.0d0; B(5,6) = 0.4d0
  B(6,6) = 3.5d0

  do i = 1, N
    Q(i,i) = 1.0d0
    Z(i,i) = 1.0d0
  end do

  IFST = 5
  ILST = 1
  call DTGEXC(.TRUE., .TRUE., N, A, MAXN, B, MAXN, Q, MAXN, Z, MAXN, &
              IFST, ILST, WORK, LWORK, INFO)

  call begin_test('move_2x2_bwd_across_2x2_6x6')
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call print_matrix('A', A, MAXN, N, N)
  call print_matrix('B', B, MAXN, N, N)
  call print_matrix('Q', Q, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call end_test()

end program test_dtgexc
