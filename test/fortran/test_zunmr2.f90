program test_zunmr2
  use test_utils
  implicit none

  ! Use tight LDA/LDC so flat output captures all data contiguously
  integer, parameter :: MAXWK = 400
  complex*16 :: A(20, 20), TAU(20), C(20, 20), WORK(MAXWK)
  ! For tight-packed output
  complex*16 :: Cout(20)
  double precision :: Cout_r(40)
  equivalence (Cout, Cout_r)
  complex*16 :: Aout(20)
  double precision :: Aout_r(40)
  equivalence (Aout, Aout_r)
  complex*16 :: TAUout(10)
  double precision :: TAUout_r(20)
  equivalence (TAUout, TAUout_r)
  integer :: INFO, M, N, K, LDA, LDC, i, j

  ! Test 1: Left, no-transpose, M=3, N=2, K=2
  ! Build a 2x3 matrix for RQ factorization (K=2 rows, 3 cols)
  A = dcmplx(0.0d0, 0.0d0)
  A(1,1) = dcmplx(1.0d0, 0.0d0); A(1,2) = dcmplx(2.0d0, 1.0d0); A(1,3) = dcmplx(3.0d0, 0.5d0)
  A(2,1) = dcmplx(4.0d0, 1.0d0); A(2,2) = dcmplx(5.0d0, 0.0d0); A(2,3) = dcmplx(6.0d0, -1.0d0)
  LDA = 20; K = 2; M = 3; N = 2
  call ZGERQF(K, M, A, LDA, TAU, WORK, MAXWK, INFO)

  ! Save A and TAU for the test (so the JS test can use the same input)
  do i = 1, K
    do j = 1, M
      Aout((i-1)*M+j) = A(i, j)
    end do
  end do
  do i = 1, K
    TAUout(i) = TAU(i)
  end do

  C = dcmplx(0.0d0, 0.0d0)
  C(1,1) = dcmplx(1.0d0, 0.0d0)
  C(2,2) = dcmplx(1.0d0, 0.0d0)
  C(3,1) = dcmplx(0.5d0, 0.3d0)
  C(3,2) = dcmplx(0.2d0, -0.1d0)
  LDC = 20

  call ZUNMR2('L', 'N', M, N, K, A, LDA, TAU, C, LDC, WORK, INFO)
  ! Pack output C(1:M, 1:N) column-major tight
  do j = 1, N
    do i = 1, M
      Cout((j-1)*M+i) = C(i, j)
    end do
  end do
  call begin_test('left_notrans_3x2')
  call print_int('info', INFO)
  call print_array('C', Cout_r, 2*M*N)
  call print_array('A', Aout_r, 2*K*M)
  call print_array('TAU', TAUout_r, 2*K)
  call end_test()

  ! Test 2: Left, conjugate-transpose, M=3, N=2, K=2
  A = dcmplx(0.0d0, 0.0d0)
  A(1,1) = dcmplx(1.0d0, 0.0d0); A(1,2) = dcmplx(2.0d0, 1.0d0); A(1,3) = dcmplx(3.0d0, 0.5d0)
  A(2,1) = dcmplx(4.0d0, 1.0d0); A(2,2) = dcmplx(5.0d0, 0.0d0); A(2,3) = dcmplx(6.0d0, -1.0d0)
  call ZGERQF(K, M, A, LDA, TAU, WORK, MAXWK, INFO)

  C = dcmplx(0.0d0, 0.0d0)
  C(1,1) = dcmplx(1.0d0, 0.0d0)
  C(2,2) = dcmplx(1.0d0, 0.0d0)
  C(3,1) = dcmplx(0.5d0, 0.3d0)
  C(3,2) = dcmplx(0.2d0, -0.1d0)

  call ZUNMR2('L', 'C', M, N, K, A, LDA, TAU, C, LDC, WORK, INFO)
  do j = 1, N
    do i = 1, M
      Cout((j-1)*M+i) = C(i, j)
    end do
  end do
  call begin_test('left_conjtrans_3x2')
  call print_int('info', INFO)
  call print_array('C', Cout_r, 2*M*N)
  call end_test()

  ! Test 3: Right, no-transpose, M=2, N=3, K=2
  A = dcmplx(0.0d0, 0.0d0)
  A(1,1) = dcmplx(1.0d0, 0.0d0); A(1,2) = dcmplx(2.0d0, 1.0d0); A(1,3) = dcmplx(3.0d0, 0.5d0)
  A(2,1) = dcmplx(4.0d0, 1.0d0); A(2,2) = dcmplx(5.0d0, 0.0d0); A(2,3) = dcmplx(6.0d0, -1.0d0)
  call ZGERQF(K, 3, A, LDA, TAU, WORK, MAXWK, INFO)

  C = dcmplx(0.0d0, 0.0d0)
  C(1,1) = dcmplx(1.0d0, 0.5d0); C(1,2) = dcmplx(0.0d0, 0.0d0); C(1,3) = dcmplx(2.0d0, 0.0d0)
  C(2,1) = dcmplx(0.0d0, 0.0d0); C(2,2) = dcmplx(1.0d0, -0.5d0); C(2,3) = dcmplx(3.0d0, 1.0d0)
  M = 2; N = 3

  call ZUNMR2('R', 'N', M, N, K, A, LDA, TAU, C, LDC, WORK, INFO)
  do j = 1, N
    do i = 1, M
      Cout((j-1)*M+i) = C(i, j)
    end do
  end do
  call begin_test('right_notrans_2x3')
  call print_int('info', INFO)
  call print_array('C', Cout_r, 2*M*N)
  call end_test()

  ! Test 4: Right, conjugate-transpose, M=2, N=3, K=2
  A = dcmplx(0.0d0, 0.0d0)
  A(1,1) = dcmplx(1.0d0, 0.0d0); A(1,2) = dcmplx(2.0d0, 1.0d0); A(1,3) = dcmplx(3.0d0, 0.5d0)
  A(2,1) = dcmplx(4.0d0, 1.0d0); A(2,2) = dcmplx(5.0d0, 0.0d0); A(2,3) = dcmplx(6.0d0, -1.0d0)
  call ZGERQF(K, 3, A, LDA, TAU, WORK, MAXWK, INFO)

  C = dcmplx(0.0d0, 0.0d0)
  C(1,1) = dcmplx(1.0d0, 0.5d0); C(1,2) = dcmplx(0.0d0, 0.0d0); C(1,3) = dcmplx(2.0d0, 0.0d0)
  C(2,1) = dcmplx(0.0d0, 0.0d0); C(2,2) = dcmplx(1.0d0, -0.5d0); C(2,3) = dcmplx(3.0d0, 1.0d0)

  call ZUNMR2('R', 'C', M, N, K, A, LDA, TAU, C, LDC, WORK, INFO)
  do j = 1, N
    do i = 1, M
      Cout((j-1)*M+i) = C(i, j)
    end do
  end do
  call begin_test('right_conjtrans_2x3')
  call print_int('info', INFO)
  call print_array('C', Cout_r, 2*M*N)
  call end_test()

  ! Test 5: Quick return M=0
  call ZUNMR2('L', 'N', 0, 2, 0, A, LDA, TAU, C, LDC, WORK, INFO)
  call begin_test('m_zero')
  call print_int('info', INFO)
  call end_test()

  ! Test 6: K=1 single reflector, M=2, N=1
  A = dcmplx(0.0d0, 0.0d0)
  A(1,1) = dcmplx(2.0d0, 1.0d0); A(1,2) = dcmplx(3.0d0, -1.0d0)
  call ZGERQF(1, 2, A, LDA, TAU, WORK, MAXWK, INFO)

  C = dcmplx(0.0d0, 0.0d0)
  C(1,1) = dcmplx(1.0d0, 0.0d0)
  C(2,1) = dcmplx(0.0d0, 1.0d0)

  call ZUNMR2('L', 'N', 2, 1, 1, A, LDA, TAU, C, LDC, WORK, INFO)
  Cout(1) = C(1,1)
  Cout(2) = C(2,1)
  call begin_test('single_reflector')
  call print_int('info', INFO)
  call print_array('C', Cout_r, 2*2*1)
  call end_test()

end program
