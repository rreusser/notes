program test_zunmr2
  use test_utils
  implicit none

  integer, parameter :: MAXN = 20
  complex*16 :: A(MAXN, MAXN), TAU(MAXN), C(MAXN, MAXN), WORK(MAXN*MAXN)
  double precision :: A_r(MAXN*MAXN*2), TAU_r(MAXN*2), C_r(MAXN*MAXN*2)
  equivalence (A, A_r)
  equivalence (TAU, TAU_r)
  equivalence (C, C_r)
  integer :: INFO, M, N, K, LDA, LDC

  ! Test 1: Left, no-transpose, 3x3 C, K=2
  ! Build a 2x3 matrix for RQ factorization
  A(1,1) = dcmplx(1.0d0, 0.0d0); A(1,2) = dcmplx(2.0d0, 1.0d0); A(1,3) = dcmplx(3.0d0, 0.5d0)
  A(2,1) = dcmplx(4.0d0, 1.0d0); A(2,2) = dcmplx(5.0d0, 0.0d0); A(2,3) = dcmplx(6.0d0, -1.0d0)
  LDA = MAXN; K = 2; M = 3; N = 2
  call ZGERQF(K, M, A, LDA, TAU, WORK, MAXN*MAXN, INFO)

  ! Set C = Identity 3x2
  C = dcmplx(0.0d0, 0.0d0)
  C(1,1) = dcmplx(1.0d0, 0.0d0)
  C(2,2) = dcmplx(1.0d0, 0.0d0)
  C(3,1) = dcmplx(0.5d0, 0.3d0)
  C(3,2) = dcmplx(0.2d0, -0.1d0)
  LDC = MAXN

  ! Apply Q from left: C <- Q * C
  call ZUNMR2('L', 'N', M, N, K, A, LDA, TAU, C, LDC, WORK, INFO)
  call begin_test('left_notrans_3x2')
  call print_int('info', INFO)
  call print_array('C', C_r, 2*M*N)
  call end_test()

  ! Test 2: Left, conjugate-transpose, 3x2 C, K=2
  A(1,1) = dcmplx(1.0d0, 0.0d0); A(1,2) = dcmplx(2.0d0, 1.0d0); A(1,3) = dcmplx(3.0d0, 0.5d0)
  A(2,1) = dcmplx(4.0d0, 1.0d0); A(2,2) = dcmplx(5.0d0, 0.0d0); A(2,3) = dcmplx(6.0d0, -1.0d0)
  call ZGERQF(K, M, A, LDA, TAU, WORK, MAXN*MAXN, INFO)

  C = dcmplx(0.0d0, 0.0d0)
  C(1,1) = dcmplx(1.0d0, 0.0d0)
  C(2,2) = dcmplx(1.0d0, 0.0d0)
  C(3,1) = dcmplx(0.5d0, 0.3d0)
  C(3,2) = dcmplx(0.2d0, -0.1d0)

  call ZUNMR2('L', 'C', M, N, K, A, LDA, TAU, C, LDC, WORK, INFO)
  call begin_test('left_conjtrans_3x2')
  call print_int('info', INFO)
  call print_array('C', C_r, 2*M*N)
  call end_test()

  ! Test 3: Right, no-transpose, 2x3 C, K=2
  A(1,1) = dcmplx(1.0d0, 0.0d0); A(1,2) = dcmplx(2.0d0, 1.0d0); A(1,3) = dcmplx(3.0d0, 0.5d0)
  A(2,1) = dcmplx(4.0d0, 1.0d0); A(2,2) = dcmplx(5.0d0, 0.0d0); A(2,3) = dcmplx(6.0d0, -1.0d0)
  call ZGERQF(K, 3, A, LDA, TAU, WORK, MAXN*MAXN, INFO)

  C = dcmplx(0.0d0, 0.0d0)
  C(1,1) = dcmplx(1.0d0, 0.5d0); C(1,2) = dcmplx(0.0d0, 0.0d0); C(1,3) = dcmplx(2.0d0, 0.0d0)
  C(2,1) = dcmplx(0.0d0, 0.0d0); C(2,2) = dcmplx(1.0d0, -0.5d0); C(2,3) = dcmplx(3.0d0, 1.0d0)
  M = 2; N = 3

  call ZUNMR2('R', 'N', M, N, K, A, LDA, TAU, C, LDC, WORK, INFO)
  call begin_test('right_notrans_2x3')
  call print_int('info', INFO)
  call print_array('C', C_r, 2*M*N)
  call end_test()

  ! Test 4: Right, conjugate-transpose, 2x3 C, K=2
  A(1,1) = dcmplx(1.0d0, 0.0d0); A(1,2) = dcmplx(2.0d0, 1.0d0); A(1,3) = dcmplx(3.0d0, 0.5d0)
  A(2,1) = dcmplx(4.0d0, 1.0d0); A(2,2) = dcmplx(5.0d0, 0.0d0); A(2,3) = dcmplx(6.0d0, -1.0d0)
  call ZGERQF(K, 3, A, LDA, TAU, WORK, MAXN*MAXN, INFO)

  C = dcmplx(0.0d0, 0.0d0)
  C(1,1) = dcmplx(1.0d0, 0.5d0); C(1,2) = dcmplx(0.0d0, 0.0d0); C(1,3) = dcmplx(2.0d0, 0.0d0)
  C(2,1) = dcmplx(0.0d0, 0.0d0); C(2,2) = dcmplx(1.0d0, -0.5d0); C(2,3) = dcmplx(3.0d0, 1.0d0)

  call ZUNMR2('R', 'C', M, N, K, A, LDA, TAU, C, LDC, WORK, INFO)
  call begin_test('right_conjtrans_2x3')
  call print_int('info', INFO)
  call print_array('C', C_r, 2*M*N)
  call end_test()

  ! Test 5: Quick return M=0
  call ZUNMR2('L', 'N', 0, 2, 0, A, LDA, TAU, C, LDC, WORK, INFO)
  call begin_test('m_zero')
  call print_int('info', INFO)
  call end_test()

  ! Test 6: K=1 single reflector
  A(1,1) = dcmplx(2.0d0, 1.0d0); A(1,2) = dcmplx(3.0d0, -1.0d0)
  call ZGERQF(1, 2, A, LDA, TAU, WORK, MAXN*MAXN, INFO)

  C = dcmplx(0.0d0, 0.0d0)
  C(1,1) = dcmplx(1.0d0, 0.0d0)
  C(2,1) = dcmplx(0.0d0, 1.0d0)

  call ZUNMR2('L', 'N', 2, 1, 1, A, LDA, TAU, C, LDC, WORK, INFO)
  call begin_test('single_reflector')
  call print_int('info', INFO)
  call print_array('C', C_r, 2*2*1)
  call end_test()

end program
