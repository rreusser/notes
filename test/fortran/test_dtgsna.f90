program test_dtgsna
  use test_utils
  implicit none

  integer, parameter :: MAXN = 6
  double precision :: A(MAXN,MAXN), B(MAXN,MAXN)
  double precision :: VL(MAXN,MAXN), VR(MAXN,MAXN)
  double precision :: S(MAXN), DIF(MAXN)
  double precision :: WORK(500)
  integer :: IWORK(500), M, INFO, N, i
  logical :: SELCT(MAXN)

  ! ==========================================================================
  ! Test 1: N=1 trivial, job='B', howmny='A'
  ! ==========================================================================
  N = 1
  A = 0.0d0; B = 0.0d0; VL = 0.0d0; VR = 0.0d0
  A(1,1) = 3.0d0
  B(1,1) = 2.0d0
  VL(1,1) = 1.0d0
  VR(1,1) = 1.0d0

  S = 0.0d0; DIF = 0.0d0
  call DTGSNA('B', 'A', SELCT, N, A, MAXN, B, MAXN, VL, MAXN, VR, MAXN, &
              S, DIF, MAXN, M, WORK, 500, IWORK, INFO)

  call begin_test('n1_both_all')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('S', S, N)
  call print_array('DIF', DIF, N)
  call end_test()

  ! ==========================================================================
  ! Test 2: 3x3 upper triangular (all real eigenvalues), job='E', howmny='A'
  ! ==========================================================================
  N = 3
  A = 0.0d0; B = 0.0d0; VL = 0.0d0; VR = 0.0d0

  A(1,1) = 1.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0
  A(2,2) = 2.0d0; A(2,3) = 0.4d0
  A(3,3) = 3.0d0

  B(1,1) = 1.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0
  B(2,2) = 1.5d0; B(2,3) = 0.3d0
  B(3,3) = 2.0d0

  ! Right eigenvectors (for upper triangular pair, standard basis e_i works
  ! only with back-substitution. Use identity as simple vectors.)
  do i = 1, N
    VL(i,i) = 1.0d0
    VR(i,i) = 1.0d0
  end do

  S = 0.0d0; DIF = 0.0d0
  call DTGSNA('E', 'A', SELCT, N, A, MAXN, B, MAXN, VL, MAXN, VR, MAXN, &
              S, DIF, MAXN, M, WORK, 500, IWORK, INFO)

  call begin_test('n3_eig_all')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('S', S, N)
  call end_test()

  ! ==========================================================================
  ! Test 3: 3x3 upper triangular, job='V' (eigenvectors), howmny='A'
  ! ==========================================================================
  N = 3
  A = 0.0d0; B = 0.0d0; VL = 0.0d0; VR = 0.0d0

  A(1,1) = 1.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0
  A(2,2) = 2.0d0; A(2,3) = 0.4d0
  A(3,3) = 3.0d0

  B(1,1) = 1.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0
  B(2,2) = 1.5d0; B(2,3) = 0.3d0
  B(3,3) = 2.0d0

  do i = 1, N
    VL(i,i) = 1.0d0
    VR(i,i) = 1.0d0
  end do

  S = 0.0d0; DIF = 0.0d0
  call DTGSNA('V', 'A', SELCT, N, A, MAXN, B, MAXN, VL, MAXN, VR, MAXN, &
              S, DIF, MAXN, M, WORK, 500, IWORK, INFO)

  call begin_test('n3_vec_all')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('DIF', DIF, N)
  call end_test()

  ! ==========================================================================
  ! Test 4: 3x3 upper triangular, job='B' (both)
  ! ==========================================================================
  N = 3
  A = 0.0d0; B = 0.0d0; VL = 0.0d0; VR = 0.0d0

  A(1,1) = 1.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0
  A(2,2) = 2.0d0; A(2,3) = 0.4d0
  A(3,3) = 3.0d0

  B(1,1) = 1.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0
  B(2,2) = 1.5d0; B(2,3) = 0.3d0
  B(3,3) = 2.0d0

  do i = 1, N
    VL(i,i) = 1.0d0
    VR(i,i) = 1.0d0
  end do

  S = 0.0d0; DIF = 0.0d0
  call DTGSNA('B', 'A', SELCT, N, A, MAXN, B, MAXN, VL, MAXN, VR, MAXN, &
              S, DIF, MAXN, M, WORK, 500, IWORK, INFO)

  call begin_test('n3_both_all')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('S', S, N)
  call print_array('DIF', DIF, N)
  call end_test()

  ! ==========================================================================
  ! Test 5: 3x3 select only eigenvalues 1 and 3, job='B'
  ! ==========================================================================
  N = 3
  A = 0.0d0; B = 0.0d0; VL = 0.0d0; VR = 0.0d0

  A(1,1) = 1.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0
  A(2,2) = 2.0d0; A(2,3) = 0.4d0
  A(3,3) = 3.0d0

  B(1,1) = 1.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0
  B(2,2) = 1.5d0; B(2,3) = 0.3d0
  B(3,3) = 2.0d0

  do i = 1, N
    VL(i,i) = 1.0d0
    VR(i,i) = 1.0d0
  end do

  SELCT = .false.
  SELCT(1) = .true.
  SELCT(3) = .true.

  S = 0.0d0; DIF = 0.0d0
  call DTGSNA('B', 'S', SELCT, N, A, MAXN, B, MAXN, VL, MAXN, VR, MAXN, &
              S, DIF, MAXN, M, WORK, 500, IWORK, INFO)

  call begin_test('n3_both_selected')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('S', S, M)
  call print_array('DIF', DIF, M)
  call end_test()

  ! ==========================================================================
  ! Test 6: N=0 edge case
  ! ==========================================================================
  N = 0
  M = 99
  call DTGSNA('B', 'A', SELCT, N, A, MAXN, B, MAXN, VL, MAXN, VR, MAXN, &
              S, DIF, MAXN, M, WORK, 500, IWORK, INFO)

  call begin_test('n0')
  call print_int('info', INFO)
  call print_int('M', M)
  call end_test()

  ! ==========================================================================
  ! Test 7: 2x2 diagonal, job='B'
  ! ==========================================================================
  N = 2
  A = 0.0d0; B = 0.0d0; VL = 0.0d0; VR = 0.0d0
  A(1,1) = 2.0d0
  A(2,2) = 5.0d0
  B(1,1) = 1.0d0
  B(2,2) = 2.0d0
  VL(1,1) = 1.0d0
  VL(2,2) = 1.0d0
  VR(1,1) = 1.0d0
  VR(2,2) = 1.0d0

  S = 0.0d0; DIF = 0.0d0
  call DTGSNA('B', 'A', SELCT, N, A, MAXN, B, MAXN, VL, MAXN, VR, MAXN, &
              S, DIF, MAXN, M, WORK, 500, IWORK, INFO)

  call begin_test('n2_diag_both')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('S', S, N)
  call print_array('DIF', DIF, N)
  call end_test()

end program
