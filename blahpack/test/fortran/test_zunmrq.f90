program test_zunmrq
  use test_utils
  implicit none

  integer, parameter :: MAXN = 20
  complex*16 :: A(MAXN, MAXN), TAU(MAXN), C(MAXN, MAXN), WORK(MAXN*MAXN)
  double precision :: C_r(MAXN*MAXN*2)
  equivalence (C, C_r)
  integer :: INFO, M, N, K, LDA, LDC, LWORK

  LWORK = MAXN * MAXN

  ! Test 1: Left, no-transpose, 4x3 C, K=3 (blocked path if NB < K)
  ! Build 3x4 for RQ factorization
  A(1,1) = dcmplx(1.0d0, 0.0d0); A(1,2) = dcmplx(2.0d0, 1.0d0)
  A(1,3) = dcmplx(3.0d0, 0.5d0); A(1,4) = dcmplx(1.0d0, -0.5d0)
  A(2,1) = dcmplx(4.0d0, 1.0d0); A(2,2) = dcmplx(5.0d0, 0.0d0)
  A(2,3) = dcmplx(6.0d0, -1.0d0); A(2,4) = dcmplx(2.0d0, 0.0d0)
  A(3,1) = dcmplx(7.0d0, -1.0d0); A(3,2) = dcmplx(8.0d0, 0.5d0)
  A(3,3) = dcmplx(9.0d0, 0.0d0); A(3,4) = dcmplx(3.0d0, 1.0d0)
  LDA = MAXN; K = 3; M = 4; N = 3
  call ZGERQF(K, M, A, LDA, TAU, WORK, LWORK, INFO)

  C = dcmplx(0.0d0, 0.0d0)
  C(1,1) = dcmplx(1.0d0, 0.0d0)
  C(2,2) = dcmplx(1.0d0, 0.0d0)
  C(3,3) = dcmplx(1.0d0, 0.0d0)
  C(4,1) = dcmplx(0.5d0, 0.3d0)
  C(4,2) = dcmplx(0.2d0, -0.1d0)
  C(4,3) = dcmplx(-0.3d0, 0.4d0)
  LDC = MAXN

  call ZUNMRQ('L', 'N', M, N, K, A, LDA, TAU, C, LDC, WORK, LWORK, INFO)
  call begin_test('left_notrans_4x3')
  call print_int('info', INFO)
  call print_array('C', C_r, 2*M*N)
  call end_test()

  ! Test 2: Left, conjugate-transpose, 4x3 C, K=3
  A(1,1) = dcmplx(1.0d0, 0.0d0); A(1,2) = dcmplx(2.0d0, 1.0d0)
  A(1,3) = dcmplx(3.0d0, 0.5d0); A(1,4) = dcmplx(1.0d0, -0.5d0)
  A(2,1) = dcmplx(4.0d0, 1.0d0); A(2,2) = dcmplx(5.0d0, 0.0d0)
  A(2,3) = dcmplx(6.0d0, -1.0d0); A(2,4) = dcmplx(2.0d0, 0.0d0)
  A(3,1) = dcmplx(7.0d0, -1.0d0); A(3,2) = dcmplx(8.0d0, 0.5d0)
  A(3,3) = dcmplx(9.0d0, 0.0d0); A(3,4) = dcmplx(3.0d0, 1.0d0)
  call ZGERQF(K, M, A, LDA, TAU, WORK, LWORK, INFO)

  C = dcmplx(0.0d0, 0.0d0)
  C(1,1) = dcmplx(1.0d0, 0.0d0)
  C(2,2) = dcmplx(1.0d0, 0.0d0)
  C(3,3) = dcmplx(1.0d0, 0.0d0)
  C(4,1) = dcmplx(0.5d0, 0.3d0)
  C(4,2) = dcmplx(0.2d0, -0.1d0)
  C(4,3) = dcmplx(-0.3d0, 0.4d0)

  call ZUNMRQ('L', 'C', M, N, K, A, LDA, TAU, C, LDC, WORK, LWORK, INFO)
  call begin_test('left_conjtrans_4x3')
  call print_int('info', INFO)
  call print_array('C', C_r, 2*M*N)
  call end_test()

  ! Test 3: Right, no-transpose, 3x4 C, K=3
  A(1,1) = dcmplx(1.0d0, 0.0d0); A(1,2) = dcmplx(2.0d0, 1.0d0)
  A(1,3) = dcmplx(3.0d0, 0.5d0); A(1,4) = dcmplx(1.0d0, -0.5d0)
  A(2,1) = dcmplx(4.0d0, 1.0d0); A(2,2) = dcmplx(5.0d0, 0.0d0)
  A(2,3) = dcmplx(6.0d0, -1.0d0); A(2,4) = dcmplx(2.0d0, 0.0d0)
  A(3,1) = dcmplx(7.0d0, -1.0d0); A(3,2) = dcmplx(8.0d0, 0.5d0)
  A(3,3) = dcmplx(9.0d0, 0.0d0); A(3,4) = dcmplx(3.0d0, 1.0d0)
  call ZGERQF(K, 4, A, LDA, TAU, WORK, LWORK, INFO)

  C = dcmplx(0.0d0, 0.0d0)
  C(1,1) = dcmplx(1.0d0, 0.5d0); C(1,2) = dcmplx(0.0d0, 0.0d0)
  C(1,3) = dcmplx(2.0d0, 0.0d0); C(1,4) = dcmplx(-1.0d0, 0.5d0)
  C(2,1) = dcmplx(0.0d0, 0.0d0); C(2,2) = dcmplx(1.0d0, -0.5d0)
  C(2,3) = dcmplx(3.0d0, 1.0d0); C(2,4) = dcmplx(0.5d0, 0.0d0)
  C(3,1) = dcmplx(0.3d0, -0.2d0); C(3,2) = dcmplx(0.7d0, 0.1d0)
  C(3,3) = dcmplx(-0.5d0, 0.0d0); C(3,4) = dcmplx(1.0d0, 1.0d0)
  M = 3; N = 4

  call ZUNMRQ('R', 'N', M, N, K, A, LDA, TAU, C, LDC, WORK, LWORK, INFO)
  call begin_test('right_notrans_3x4')
  call print_int('info', INFO)
  call print_array('C', C_r, 2*M*N)
  call end_test()

  ! Test 4: Right, conjugate-transpose
  A(1,1) = dcmplx(1.0d0, 0.0d0); A(1,2) = dcmplx(2.0d0, 1.0d0)
  A(1,3) = dcmplx(3.0d0, 0.5d0); A(1,4) = dcmplx(1.0d0, -0.5d0)
  A(2,1) = dcmplx(4.0d0, 1.0d0); A(2,2) = dcmplx(5.0d0, 0.0d0)
  A(2,3) = dcmplx(6.0d0, -1.0d0); A(2,4) = dcmplx(2.0d0, 0.0d0)
  A(3,1) = dcmplx(7.0d0, -1.0d0); A(3,2) = dcmplx(8.0d0, 0.5d0)
  A(3,3) = dcmplx(9.0d0, 0.0d0); A(3,4) = dcmplx(3.0d0, 1.0d0)
  call ZGERQF(K, 4, A, LDA, TAU, WORK, LWORK, INFO)

  C = dcmplx(0.0d0, 0.0d0)
  C(1,1) = dcmplx(1.0d0, 0.5d0); C(1,2) = dcmplx(0.0d0, 0.0d0)
  C(1,3) = dcmplx(2.0d0, 0.0d0); C(1,4) = dcmplx(-1.0d0, 0.5d0)
  C(2,1) = dcmplx(0.0d0, 0.0d0); C(2,2) = dcmplx(1.0d0, -0.5d0)
  C(2,3) = dcmplx(3.0d0, 1.0d0); C(2,4) = dcmplx(0.5d0, 0.0d0)
  C(3,1) = dcmplx(0.3d0, -0.2d0); C(3,2) = dcmplx(0.7d0, 0.1d0)
  C(3,3) = dcmplx(-0.5d0, 0.0d0); C(3,4) = dcmplx(1.0d0, 1.0d0)

  call ZUNMRQ('R', 'C', M, N, K, A, LDA, TAU, C, LDC, WORK, LWORK, INFO)
  call begin_test('right_conjtrans_3x4')
  call print_int('info', INFO)
  call print_array('C', C_r, 2*M*N)
  call end_test()

  ! Test 5: Quick return
  call ZUNMRQ('L', 'N', 0, 0, 0, A, LDA, TAU, C, LDC, WORK, LWORK, INFO)
  call begin_test('quick_return')
  call print_int('info', INFO)
  call end_test()

end program
