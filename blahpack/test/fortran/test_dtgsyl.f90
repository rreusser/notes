program test_dtgsyl
  use test_utils
  implicit none

  integer, parameter :: MAXN = 10, LWORK = 400
  double precision :: A(MAXN, MAXN), B(MAXN, MAXN)
  double precision :: C(MAXN, MAXN), D(MAXN, MAXN)
  double precision :: E(MAXN, MAXN), F(MAXN, MAXN)
  double precision :: SCALE, DIF, WORK(LWORK)
  integer :: IWORK(MAXN*3), INFO
  integer :: M, N

  ! Test 1: 2x2 diagonal, IJOB=0, TRANS='N'
  A = 0.0d0; B = 0.0d0; C = 0.0d0; D = 0.0d0; E = 0.0d0; F = 0.0d0
  M = 2; N = 2

  A(1,1) = 1.0d0; A(1,2) = 0.5d0
  A(2,2) = 2.0d0

  B(1,1) = 3.0d0; B(1,2) = 0.3d0
  B(2,2) = 4.0d0

  C(1,1) = 1.0d0; C(1,2) = 2.0d0
  C(2,1) = 3.0d0; C(2,2) = 4.0d0

  D(1,1) = 1.0d0; D(1,2) = 0.2d0
  D(2,2) = 1.5d0

  E(1,1) = 1.0d0; E(1,2) = 0.1d0
  E(2,2) = 2.0d0

  F(1,1) = 5.0d0; F(1,2) = 6.0d0
  F(2,1) = 7.0d0; F(2,2) = 8.0d0

  call DTGSYL('N', 0, M, N, A, MAXN, B, MAXN, C, MAXN, &
              D, MAXN, E, MAXN, F, MAXN, SCALE, DIF, WORK, LWORK, &
              IWORK, INFO)
  call begin_test('notrans_2x2')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call print_matrix('C', C, MAXN, M, N)
  call print_matrix('F', F, MAXN, M, N)
  call end_test()

  ! Test 2: 3x2 with 2x2 block in A, IJOB=0
  A = 0.0d0; B = 0.0d0; C = 0.0d0; D = 0.0d0; E = 0.0d0; F = 0.0d0
  M = 3; N = 2

  A(1,1) = 1.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0
  A(2,1) = -0.5d0; A(2,2) = 1.0d0; A(2,3) = 0.2d0
  A(3,3) = 3.0d0

  B(1,1) = 2.0d0; B(1,2) = 0.4d0
  B(2,2) = 5.0d0

  C(1,1) = 1.0d0; C(1,2) = 2.0d0
  C(2,1) = 3.0d0; C(2,2) = 4.0d0
  C(3,1) = 5.0d0; C(3,2) = 6.0d0

  D(1,1) = 1.0d0; D(1,2) = 0.1d0; D(1,3) = 0.2d0
  D(2,2) = 1.5d0; D(2,3) = 0.3d0
  D(3,3) = 2.0d0

  E(1,1) = 1.0d0; E(1,2) = 0.2d0
  E(2,2) = 3.0d0

  F(1,1) = 7.0d0; F(1,2) = 8.0d0
  F(2,1) = 9.0d0; F(2,2) = 10.0d0
  F(3,1) = 11.0d0; F(3,2) = 12.0d0

  call DTGSYL('N', 0, M, N, A, MAXN, B, MAXN, C, MAXN, &
              D, MAXN, E, MAXN, F, MAXN, SCALE, DIF, WORK, LWORK, &
              IWORK, INFO)
  call begin_test('notrans_3x2_quasi')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call print_matrix('C', C, MAXN, M, N)
  call print_matrix('F', F, MAXN, M, N)
  call end_test()

  ! Test 3: Transposed 2x2
  A = 0.0d0; B = 0.0d0; C = 0.0d0; D = 0.0d0; E = 0.0d0; F = 0.0d0
  M = 2; N = 2

  A(1,1) = 1.0d0; A(1,2) = 0.5d0
  A(2,2) = 2.0d0

  B(1,1) = 3.0d0; B(1,2) = 0.3d0
  B(2,2) = 4.0d0

  C(1,1) = 1.0d0; C(1,2) = 2.0d0
  C(2,1) = 3.0d0; C(2,2) = 4.0d0

  D(1,1) = 1.0d0; D(1,2) = 0.2d0
  D(2,2) = 1.5d0

  E(1,1) = 1.0d0; E(1,2) = 0.1d0
  E(2,2) = 2.0d0

  F(1,1) = 5.0d0; F(1,2) = 6.0d0
  F(2,1) = 7.0d0; F(2,2) = 8.0d0

  call DTGSYL('T', 0, M, N, A, MAXN, B, MAXN, C, MAXN, &
              D, MAXN, E, MAXN, F, MAXN, SCALE, DIF, WORK, LWORK, &
              IWORK, INFO)
  call begin_test('trans_2x2')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call print_matrix('C', C, MAXN, M, N)
  call print_matrix('F', F, MAXN, M, N)
  call end_test()

  ! Test 4: 3x3 with quasi-triangular blocks, IJOB=0
  A = 0.0d0; B = 0.0d0; C = 0.0d0; D = 0.0d0; E = 0.0d0; F = 0.0d0
  M = 3; N = 3

  A(1,1) = 1.0d0; A(1,2) = 0.4d0; A(1,3) = 0.1d0
  A(2,1) = -0.4d0; A(2,2) = 1.0d0; A(2,3) = 0.2d0
  A(3,3) = 5.0d0

  B(1,1) = 2.0d0; B(1,2) = 0.3d0; B(1,3) = 0.1d0
  B(2,1) = -0.3d0; B(2,2) = 2.0d0; B(2,3) = 0.2d0
  B(3,3) = 6.0d0

  C(1,1) = 1.0d0; C(1,2) = 2.0d0; C(1,3) = 3.0d0
  C(2,1) = 4.0d0; C(2,2) = 5.0d0; C(2,3) = 6.0d0
  C(3,1) = 7.0d0; C(3,2) = 8.0d0; C(3,3) = 9.0d0

  D(1,1) = 1.0d0; D(1,2) = 0.1d0; D(1,3) = 0.05d0
  D(2,2) = 1.5d0; D(2,3) = 0.2d0
  D(3,3) = 2.0d0

  E(1,1) = 1.0d0; E(1,2) = 0.2d0; E(1,3) = 0.1d0
  E(2,2) = 2.5d0; E(2,3) = 0.15d0
  E(3,3) = 3.0d0

  F(1,1) = 10.0d0; F(1,2) = 11.0d0; F(1,3) = 12.0d0
  F(2,1) = 13.0d0; F(2,2) = 14.0d0; F(2,3) = 15.0d0
  F(3,1) = 16.0d0; F(3,2) = 17.0d0; F(3,3) = 18.0d0

  call DTGSYL('N', 0, M, N, A, MAXN, B, MAXN, C, MAXN, &
              D, MAXN, E, MAXN, F, MAXN, SCALE, DIF, WORK, LWORK, &
              IWORK, INFO)
  call begin_test('notrans_3x3_quasi')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call print_matrix('C', C, MAXN, M, N)
  call print_matrix('F', F, MAXN, M, N)
  call end_test()

end program
