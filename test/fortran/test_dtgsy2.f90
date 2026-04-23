program test_dtgsy2
  use test_utils
  implicit none

  integer, parameter :: MAXN = 10
  double precision :: A(MAXN, MAXN), B(MAXN, MAXN)
  double precision :: C(MAXN, MAXN), D(MAXN, MAXN)
  double precision :: E(MAXN, MAXN), F(MAXN, MAXN)
  double precision :: SCALE, RDSUM, RDSCAL
  integer :: IWORK(MAXN*3), PQ, INFO
  integer :: M, N

  ! Test 1: 1x1 diagonal blocks (M=2, N=2), IJOB=0, TRANS='N'
  ! A = upper triangular (Schur form), B = upper triangular
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

  call DTGSY2('N', 0, M, N, A, MAXN, B, MAXN, C, MAXN, &
              D, MAXN, E, MAXN, F, MAXN, SCALE, RDSUM, RDSCAL, &
              IWORK, PQ, INFO)
  call begin_test('notrans_2x2_diag')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call print_matrix('C', C, MAXN, M, N)
  call print_matrix('F', F, MAXN, M, N)
  call print_int('pq', PQ)
  call end_test()

  ! Test 2: M=3, N=2 with a 2x2 block in A (quasi-triangular)
  A = 0.0d0; B = 0.0d0; C = 0.0d0; D = 0.0d0; E = 0.0d0; F = 0.0d0
  M = 3; N = 2

  ! A has a 2x2 block at (1:2, 1:2)
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

  call DTGSY2('N', 0, M, N, A, MAXN, B, MAXN, C, MAXN, &
              D, MAXN, E, MAXN, F, MAXN, SCALE, RDSUM, RDSCAL, &
              IWORK, PQ, INFO)
  call begin_test('notrans_3x2_quasi')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call print_matrix('C', C, MAXN, M, N)
  call print_matrix('F', F, MAXN, M, N)
  call print_int('pq', PQ)
  call end_test()

  ! Test 3: Transposed system, 2x2
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

  call DTGSY2('T', 0, M, N, A, MAXN, B, MAXN, C, MAXN, &
              D, MAXN, E, MAXN, F, MAXN, SCALE, RDSUM, RDSCAL, &
              IWORK, PQ, INFO)
  call begin_test('trans_2x2')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call print_matrix('C', C, MAXN, M, N)
  call print_matrix('F', F, MAXN, M, N)
  call end_test()

  ! Test 4: M=2, N=3 with 2x2 block in B
  A = 0.0d0; B = 0.0d0; C = 0.0d0; D = 0.0d0; E = 0.0d0; F = 0.0d0
  M = 2; N = 3

  A(1,1) = 2.0d0; A(1,2) = 0.3d0
  A(2,2) = 4.0d0

  ! B has a 2x2 block at (1:2, 1:2)
  B(1,1) = 1.0d0; B(1,2) = 0.6d0; B(1,3) = 0.1d0
  B(2,1) = -0.6d0; B(2,2) = 1.0d0; B(2,3) = 0.2d0
  B(3,3) = 3.0d0

  C(1,1) = 1.0d0; C(1,2) = 2.0d0; C(1,3) = 3.0d0
  C(2,1) = 4.0d0; C(2,2) = 5.0d0; C(2,3) = 6.0d0

  D(1,1) = 1.0d0; D(1,2) = 0.1d0
  D(2,2) = 2.0d0

  E(1,1) = 1.0d0; E(1,2) = 0.3d0; E(1,3) = 0.1d0
  E(2,2) = 2.0d0; E(2,3) = 0.2d0
  E(3,3) = 1.5d0

  F(1,1) = 7.0d0; F(1,2) = 8.0d0; F(1,3) = 9.0d0
  F(2,1) = 10.0d0; F(2,2) = 11.0d0; F(2,3) = 12.0d0

  call DTGSY2('N', 0, M, N, A, MAXN, B, MAXN, C, MAXN, &
              D, MAXN, E, MAXN, F, MAXN, SCALE, RDSUM, RDSCAL, &
              IWORK, PQ, INFO)
  call begin_test('notrans_2x3_bblock')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call print_matrix('C', C, MAXN, M, N)
  call print_matrix('F', F, MAXN, M, N)
  call end_test()

  ! Test 5: M=3, N=3 both have 2x2 blocks
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

  call DTGSY2('N', 0, M, N, A, MAXN, B, MAXN, C, MAXN, &
              D, MAXN, E, MAXN, F, MAXN, SCALE, RDSUM, RDSCAL, &
              IWORK, PQ, INFO)
  call begin_test('notrans_3x3_both_quasi')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call print_matrix('C', C, MAXN, M, N)
  call print_matrix('F', F, MAXN, M, N)
  call end_test()

  ! Test 6: Transpose with 3x2 quasi-triangular A
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

  call DTGSY2('T', 0, M, N, A, MAXN, B, MAXN, C, MAXN, &
              D, MAXN, E, MAXN, F, MAXN, SCALE, RDSUM, RDSCAL, &
              IWORK, PQ, INFO)
  call begin_test('trans_3x2_quasi')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call print_matrix('C', C, MAXN, M, N)
  call print_matrix('F', F, MAXN, M, N)
  call end_test()

  ! Test 7: Transpose with 2x3 B has 2x2 block
  A = 0.0d0; B = 0.0d0; C = 0.0d0; D = 0.0d0; E = 0.0d0; F = 0.0d0
  M = 2; N = 3

  A(1,1) = 2.0d0; A(1,2) = 0.3d0
  A(2,2) = 4.0d0

  B(1,1) = 1.0d0; B(1,2) = 0.6d0; B(1,3) = 0.1d0
  B(2,1) = -0.6d0; B(2,2) = 1.0d0; B(2,3) = 0.2d0
  B(3,3) = 3.0d0

  C(1,1) = 1.0d0; C(1,2) = 2.0d0; C(1,3) = 3.0d0
  C(2,1) = 4.0d0; C(2,2) = 5.0d0; C(2,3) = 6.0d0

  D(1,1) = 1.0d0; D(1,2) = 0.1d0
  D(2,2) = 2.0d0

  E(1,1) = 1.0d0; E(1,2) = 0.3d0; E(1,3) = 0.1d0
  E(2,2) = 2.0d0; E(2,3) = 0.2d0
  E(3,3) = 1.5d0

  F(1,1) = 7.0d0; F(1,2) = 8.0d0; F(1,3) = 9.0d0
  F(2,1) = 10.0d0; F(2,2) = 11.0d0; F(2,3) = 12.0d0

  call DTGSY2('T', 0, M, N, A, MAXN, B, MAXN, C, MAXN, &
              D, MAXN, E, MAXN, F, MAXN, SCALE, RDSUM, RDSCAL, &
              IWORK, PQ, INFO)
  call begin_test('trans_2x3_bblock')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call print_matrix('C', C, MAXN, M, N)
  call print_matrix('F', F, MAXN, M, N)
  call end_test()

  ! Test 8: Transpose with 3x3 both quasi-triangular
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

  call DTGSY2('T', 0, M, N, A, MAXN, B, MAXN, C, MAXN, &
              D, MAXN, E, MAXN, F, MAXN, SCALE, RDSUM, RDSCAL, &
              IWORK, PQ, INFO)
  call begin_test('trans_3x3_both_quasi')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call print_matrix('C', C, MAXN, M, N)
  call print_matrix('F', F, MAXN, M, N)
  call end_test()

  ! Test 9: N=1 edge case
  A = 0.0d0; B = 0.0d0; C = 0.0d0; D = 0.0d0; E = 0.0d0; F = 0.0d0
  M = 2; N = 1

  A(1,1) = 3.0d0; A(1,2) = 0.5d0
  A(2,2) = 7.0d0

  B(1,1) = 2.0d0

  C(1,1) = 1.0d0
  C(2,1) = 4.0d0

  D(1,1) = 1.0d0; D(1,2) = 0.1d0
  D(2,2) = 1.5d0

  E(1,1) = 1.0d0

  F(1,1) = 5.0d0
  F(2,1) = 8.0d0

  call DTGSY2('N', 0, M, N, A, MAXN, B, MAXN, C, MAXN, &
              D, MAXN, E, MAXN, F, MAXN, SCALE, RDSUM, RDSCAL, &
              IWORK, PQ, INFO)
  call begin_test('notrans_2x1')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call print_matrix('C', C, MAXN, M, N)
  call print_matrix('F', F, MAXN, M, N)
  call end_test()

  ! Test 10: M=1 edge case
  A = 0.0d0; B = 0.0d0; C = 0.0d0; D = 0.0d0; E = 0.0d0; F = 0.0d0
  M = 1; N = 2

  A(1,1) = 3.0d0

  B(1,1) = 2.0d0; B(1,2) = 0.5d0
  B(2,2) = 5.0d0

  C(1,1) = 1.0d0; C(1,2) = 2.0d0

  D(1,1) = 1.5d0

  E(1,1) = 1.0d0; E(1,2) = 0.1d0
  E(2,2) = 3.0d0

  F(1,1) = 5.0d0; F(1,2) = 6.0d0

  call DTGSY2('N', 0, M, N, A, MAXN, B, MAXN, C, MAXN, &
              D, MAXN, E, MAXN, F, MAXN, SCALE, RDSUM, RDSCAL, &
              IWORK, PQ, INFO)
  call begin_test('notrans_1x2')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call print_matrix('C', C, MAXN, M, N)
  call print_matrix('F', F, MAXN, M, N)
  call end_test()

end program
