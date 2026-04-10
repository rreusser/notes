program test_dlaqtr
  use test_utils
  implicit none
  double precision :: T(4,4), B(4), X(8), WORK(4)
  double precision :: SCALE, W
  integer :: INFO, N
  logical :: LTRAN, LREAL

  ! Test 1: N=0 quick return
  N = 0
  LTRAN = .FALSE.; LREAL = .TRUE.
  W = 0.0d0; SCALE = -1.0d0; INFO = -1
  T = 0.0d0; B = 0.0d0; X = 0.0d0; WORK = 0.0d0
  call DLAQTR(LTRAN, LREAL, N, T, 4, B, W, SCALE, X, WORK, INFO)
  call begin_test('n0_quick')
  call print_scalar('SCALE', SCALE)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 2: real, no-transpose, N=1
  N = 1
  LTRAN = .FALSE.; LREAL = .TRUE.
  T = 0.0d0; T(1,1) = 2.0d0
  B = 0.0d0; W = 0.0d0
  X = 0.0d0; X(1) = 6.0d0
  WORK = 0.0d0
  SCALE = 0.0d0; INFO = 0
  call DLAQTR(LTRAN, LREAL, N, T, 4, B, W, SCALE, X, WORK, INFO)
  call begin_test('real_notran_n1')
  call print_scalar('X1', X(1))
  call print_scalar('SCALE', SCALE)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 3: real, no-transpose, N=3 upper triangular (no 2x2 blocks)
  N = 3
  LTRAN = .FALSE.; LREAL = .TRUE.
  T = 0.0d0
  T(1,1) = 2.0d0; T(1,2) = 1.0d0; T(1,3) = 3.0d0
  T(2,2) = 3.0d0; T(2,3) = -1.0d0
  T(3,3) = 4.0d0
  B = 0.0d0; W = 0.0d0
  X = 0.0d0
  X(1) = 10.0d0; X(2) = 5.0d0; X(3) = 8.0d0
  WORK = 0.0d0
  SCALE = 0.0d0; INFO = 0
  call DLAQTR(LTRAN, LREAL, N, T, 4, B, W, SCALE, X, WORK, INFO)
  call begin_test('real_notran_n3_triangular')
  call print_scalar('X1', X(1))
  call print_scalar('X2', X(2))
  call print_scalar('X3', X(3))
  call print_scalar('SCALE', SCALE)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 4: real, transposed, same system
  N = 3
  LTRAN = .TRUE.; LREAL = .TRUE.
  T = 0.0d0
  T(1,1) = 2.0d0; T(1,2) = 1.0d0; T(1,3) = 3.0d0
  T(2,2) = 3.0d0; T(2,3) = -1.0d0
  T(3,3) = 4.0d0
  B = 0.0d0; W = 0.0d0
  X = 0.0d0
  X(1) = 10.0d0; X(2) = 5.0d0; X(3) = 8.0d0
  WORK = 0.0d0
  SCALE = 0.0d0; INFO = 0
  call DLAQTR(LTRAN, LREAL, N, T, 4, B, W, SCALE, X, WORK, INFO)
  call begin_test('real_trans_n3_triangular')
  call print_scalar('X1', X(1))
  call print_scalar('X2', X(2))
  call print_scalar('X3', X(3))
  call print_scalar('SCALE', SCALE)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 5: real, no-transpose, N=3 with 2x2 block in bottom-right (Schur form)
  N = 3
  LTRAN = .FALSE.; LREAL = .TRUE.
  T = 0.0d0
  T(1,1) = 5.0d0; T(1,2) = 2.0d0; T(1,3) = 1.0d0
  T(2,2) = 1.0d0; T(2,3) = 3.0d0
  T(3,2) = -4.0d0; T(3,3) = 1.0d0
  B = 0.0d0; W = 0.0d0
  X = 0.0d0
  X(1) = 1.0d0; X(2) = 2.0d0; X(3) = 3.0d0
  WORK = 0.0d0
  SCALE = 0.0d0; INFO = 0
  call DLAQTR(LTRAN, LREAL, N, T, 4, B, W, SCALE, X, WORK, INFO)
  call begin_test('real_notran_n3_2x2block')
  call print_scalar('X1', X(1))
  call print_scalar('X2', X(2))
  call print_scalar('X3', X(3))
  call print_scalar('SCALE', SCALE)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 6: real, transposed, with 2x2 block
  N = 3
  LTRAN = .TRUE.; LREAL = .TRUE.
  T = 0.0d0
  T(1,1) = 5.0d0; T(1,2) = 2.0d0; T(1,3) = 1.0d0
  T(2,2) = 1.0d0; T(2,3) = 3.0d0
  T(3,2) = -4.0d0; T(3,3) = 1.0d0
  B = 0.0d0; W = 0.0d0
  X = 0.0d0
  X(1) = 1.0d0; X(2) = 2.0d0; X(3) = 3.0d0
  WORK = 0.0d0
  SCALE = 0.0d0; INFO = 0
  call DLAQTR(LTRAN, LREAL, N, T, 4, B, W, SCALE, X, WORK, INFO)
  call begin_test('real_trans_n3_2x2block')
  call print_scalar('X1', X(1))
  call print_scalar('X2', X(2))
  call print_scalar('X3', X(3))
  call print_scalar('SCALE', SCALE)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 7: complex, no-transpose, N=2 (strictly upper triangular)
  ! First block must be 1x1
  N = 2
  LTRAN = .FALSE.; LREAL = .FALSE.
  T = 0.0d0
  T(1,1) = 3.0d0; T(1,2) = 1.0d0
  T(2,2) = 2.0d0
  B = 0.0d0; B(1) = 0.5d0; B(2) = 0.25d0
  W = 1.5d0
  X = 0.0d0
  X(1) = 4.0d0; X(2) = 3.0d0   ! real parts
  X(3) = 1.0d0; X(4) = 2.0d0   ! imaginary parts
  WORK = 0.0d0
  SCALE = 0.0d0; INFO = 0
  call DLAQTR(LTRAN, LREAL, N, T, 4, B, W, SCALE, X, WORK, INFO)
  call begin_test('complex_notran_n2')
  call print_scalar('X1', X(1))
  call print_scalar('X2', X(2))
  call print_scalar('X3', X(3))
  call print_scalar('X4', X(4))
  call print_scalar('SCALE', SCALE)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 8: complex, transposed, N=2
  N = 2
  LTRAN = .TRUE.; LREAL = .FALSE.
  T = 0.0d0
  T(1,1) = 3.0d0; T(1,2) = 1.0d0
  T(2,2) = 2.0d0
  B = 0.0d0; B(1) = 0.5d0; B(2) = 0.25d0
  W = 1.5d0
  X = 0.0d0
  X(1) = 4.0d0; X(2) = 3.0d0
  X(3) = 1.0d0; X(4) = 2.0d0
  WORK = 0.0d0
  SCALE = 0.0d0; INFO = 0
  call DLAQTR(LTRAN, LREAL, N, T, 4, B, W, SCALE, X, WORK, INFO)
  call begin_test('complex_trans_n2')
  call print_scalar('X1', X(1))
  call print_scalar('X2', X(2))
  call print_scalar('X3', X(3))
  call print_scalar('X4', X(4))
  call print_scalar('SCALE', SCALE)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 9: complex, no-transpose, N=3 with 2x2 block (first must be 1x1)
  N = 3
  LTRAN = .FALSE.; LREAL = .FALSE.
  T = 0.0d0
  T(1,1) = 4.0d0; T(1,2) = 1.0d0; T(1,3) = 0.5d0
  T(2,2) = 1.0d0; T(2,3) = 2.0d0
  T(3,2) = -3.0d0; T(3,3) = 1.0d0
  B = 0.0d0; B(1) = 0.3d0; B(2) = 0.2d0; B(3) = 0.1d0
  W = 0.8d0
  X = 0.0d0
  X(1) = 1.0d0; X(2) = 2.0d0; X(3) = 3.0d0
  X(4) = 0.5d0; X(5) = 0.25d0; X(6) = 0.125d0
  WORK = 0.0d0
  SCALE = 0.0d0; INFO = 0
  call DLAQTR(LTRAN, LREAL, N, T, 4, B, W, SCALE, X, WORK, INFO)
  call begin_test('complex_notran_n3_2x2')
  call print_scalar('X1', X(1))
  call print_scalar('X2', X(2))
  call print_scalar('X3', X(3))
  call print_scalar('X4', X(4))
  call print_scalar('X5', X(5))
  call print_scalar('X6', X(6))
  call print_scalar('SCALE', SCALE)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 10: complex, transposed, N=3 with 2x2 block
  N = 3
  LTRAN = .TRUE.; LREAL = .FALSE.
  T = 0.0d0
  T(1,1) = 4.0d0; T(1,2) = 1.0d0; T(1,3) = 0.5d0
  T(2,2) = 1.0d0; T(2,3) = 2.0d0
  T(3,2) = -3.0d0; T(3,3) = 1.0d0
  B = 0.0d0; B(1) = 0.3d0; B(2) = 0.2d0; B(3) = 0.1d0
  W = 0.8d0
  X = 0.0d0
  X(1) = 1.0d0; X(2) = 2.0d0; X(3) = 3.0d0
  X(4) = 0.5d0; X(5) = 0.25d0; X(6) = 0.125d0
  WORK = 0.0d0
  SCALE = 0.0d0; INFO = 0
  call DLAQTR(LTRAN, LREAL, N, T, 4, B, W, SCALE, X, WORK, INFO)
  call begin_test('complex_trans_n3_2x2')
  call print_scalar('X1', X(1))
  call print_scalar('X2', X(2))
  call print_scalar('X3', X(3))
  call print_scalar('X4', X(4))
  call print_scalar('X5', X(5))
  call print_scalar('X6', X(6))
  call print_scalar('SCALE', SCALE)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 11: real, no-transpose, N=4 with 2x2 blocks and zero RHS
  N = 4
  LTRAN = .FALSE.; LREAL = .TRUE.
  T = 0.0d0
  T(1,1) = 2.0d0; T(1,2) = 1.0d0; T(1,3) = 0.5d0; T(1,4) = 0.25d0
  T(2,2) = 3.0d0; T(2,3) = 1.0d0; T(2,4) = 0.5d0
  T(3,3) = 1.0d0; T(3,4) = 2.0d0
  T(4,3) = -3.0d0; T(4,4) = 1.0d0
  B = 0.0d0; W = 0.0d0
  X = 0.0d0
  X(1) = 0.0d0; X(2) = 0.0d0; X(3) = 1.0d0; X(4) = 0.0d0
  WORK = 0.0d0
  SCALE = 0.0d0; INFO = 0
  call DLAQTR(LTRAN, LREAL, N, T, 4, B, W, SCALE, X, WORK, INFO)
  call begin_test('real_notran_n4_zero_rhs')
  call print_scalar('X1', X(1))
  call print_scalar('X2', X(2))
  call print_scalar('X3', X(3))
  call print_scalar('X4', X(4))
  call print_scalar('SCALE', SCALE)
  call print_int('INFO', INFO)
  call end_test()

end program
