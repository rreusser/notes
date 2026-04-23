program test_zunmrq
  use test_utils
  implicit none

  integer, parameter :: MAXWK = 80000
  integer, parameter :: MN = 40
  complex*16 :: A(MN, MN), TAU(MN), C(MN, MN), WORK(MAXWK)
  complex*16 :: Cout(MN*MN)
  double precision :: Cout_r(2*MN*MN)
  equivalence (Cout, Cout_r)
  complex*16 :: Aout(MN*MN)
  double precision :: Aout_r(2*MN*MN)
  equivalence (Aout, Aout_r)
  complex*16 :: TAUout(MN)
  double precision :: TAUout_r(2*MN)
  equivalence (TAUout, TAUout_r)
  integer :: INFO, M, N, K, LDA, LDC, LWORK, i, j

  LWORK = MAXWK
  LDA = MN
  LDC = MN

  ! Test 1: Left, no-transpose, M=4, N=3, K=3
  A = dcmplx(0.0d0, 0.0d0)
  A(1,1) = dcmplx(1.0d0, 0.0d0); A(1,2) = dcmplx(2.0d0, 1.0d0)
  A(1,3) = dcmplx(3.0d0, 0.5d0); A(1,4) = dcmplx(1.0d0, -0.5d0)
  A(2,1) = dcmplx(4.0d0, 1.0d0); A(2,2) = dcmplx(5.0d0, 0.0d0)
  A(2,3) = dcmplx(6.0d0, -1.0d0); A(2,4) = dcmplx(2.0d0, 0.0d0)
  A(3,1) = dcmplx(7.0d0, -1.0d0); A(3,2) = dcmplx(8.0d0, 0.5d0)
  A(3,3) = dcmplx(9.0d0, 0.0d0); A(3,4) = dcmplx(3.0d0, 1.0d0)
  K = 3; M = 4; N = 3
  call ZGERQF(K, M, A, LDA, TAU, WORK, LWORK, INFO)

  ! Save A(1:K, 1:M) and TAU(1:K) packed
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
  C(3,3) = dcmplx(1.0d0, 0.0d0)
  C(4,1) = dcmplx(0.5d0, 0.3d0)
  C(4,2) = dcmplx(0.2d0, -0.1d0)
  C(4,3) = dcmplx(-0.3d0, 0.4d0)

  call ZUNMRQ('L', 'N', M, N, K, A, LDA, TAU, C, LDC, WORK, LWORK, INFO)
  do j = 1, N
    do i = 1, M
      Cout((j-1)*M+i) = C(i, j)
    end do
  end do
  call begin_test('left_notrans_4x3')
  call print_int('info', INFO)
  call print_array('C', Cout_r, 2*M*N)
  call print_array('A', Aout_r, 2*K*M)
  call print_array('TAU', TAUout_r, 2*K)
  call end_test()

  ! Test 2: Left, conjugate-transpose, M=4, N=3, K=3
  A = dcmplx(0.0d0, 0.0d0)
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
  do j = 1, N
    do i = 1, M
      Cout((j-1)*M+i) = C(i, j)
    end do
  end do
  call begin_test('left_conjtrans_4x3')
  call print_int('info', INFO)
  call print_array('C', Cout_r, 2*M*N)
  call end_test()

  ! Test 3: Right, no-transpose, M=3, N=4, K=3
  A = dcmplx(0.0d0, 0.0d0)
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
  do j = 1, N
    do i = 1, M
      Cout((j-1)*M+i) = C(i, j)
    end do
  end do
  call begin_test('right_notrans_3x4')
  call print_int('info', INFO)
  call print_array('C', Cout_r, 2*M*N)
  call end_test()

  ! Test 4: Right, conjugate-transpose, M=3, N=4, K=3
  A = dcmplx(0.0d0, 0.0d0)
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
  do j = 1, N
    do i = 1, M
      Cout((j-1)*M+i) = C(i, j)
    end do
  end do
  call begin_test('right_conjtrans_3x4')
  call print_int('info', INFO)
  call print_array('C', Cout_r, 2*M*N)
  call end_test()

  ! Test 5: Quick return
  call ZUNMRQ('L', 'N', 0, 0, 0, A, LDA, TAU, C, LDC, WORK, LWORK, INFO)
  call begin_test('quick_return')
  call print_int('info', INFO)
  call end_test()

  ! Test 6: Large K=35 to exercise blocked path (NB=32)
  ! K=35 rows, NQ=M=38 cols => RQ factorize 35x38, apply Q from left to 38x3 C
  K = 35; M = 38; N = 3
  A = dcmplx(0.0d0, 0.0d0)
  do i = 1, K
    do j = 1, M
      A(i, j) = dcmplx(dble(mod(i*7+j*3, 19)) - 9.0d0, dble(mod(i*5+j*2, 13)) - 6.0d0)
    end do
  end do
  call ZGERQF(K, M, A, LDA, TAU, WORK, LWORK, INFO)

  ! Save packed A(1:K, 1:M) row-major and TAU(1:K)
  do i = 1, K
    do j = 1, M
      Aout((i-1)*M+j) = A(i, j)
    end do
  end do
  do i = 1, K
    TAUout(i) = TAU(i)
  end do

  C = dcmplx(0.0d0, 0.0d0)
  do i = 1, M
    do j = 1, N
      C(i, j) = dcmplx(dble(mod(i*3+j*7, 11)) - 5.0d0, dble(mod(i*2+j*5, 7)) - 3.0d0)
    end do
  end do

  call ZUNMRQ('L', 'N', M, N, K, A, LDA, TAU, C, LDC, WORK, LWORK, INFO)
  do j = 1, N
    do i = 1, M
      Cout((j-1)*M+i) = C(i, j)
    end do
  end do
  call begin_test('large_left_notrans')
  call print_int('info', INFO)
  call print_int('K', K)
  call print_int('M', M)
  call print_int('N', N)
  call print_array('C', Cout_r, 2*M*N)
  call print_array('A', Aout_r, 2*K*M)
  call print_array('TAU', TAUout_r, 2*K)
  call end_test()

  ! Test 7: Large K=35, blocked path, right side conjugate-transpose
  K = 35; M = 3; N = 38
  A = dcmplx(0.0d0, 0.0d0)
  do i = 1, K
    do j = 1, N
      A(i, j) = dcmplx(dble(mod(i*7+j*3, 19)) - 9.0d0, dble(mod(i*5+j*2, 13)) - 6.0d0)
    end do
  end do
  call ZGERQF(K, N, A, LDA, TAU, WORK, LWORK, INFO)

  do i = 1, K
    do j = 1, N
      Aout((i-1)*N+j) = A(i, j)
    end do
  end do
  do i = 1, K
    TAUout(i) = TAU(i)
  end do

  C = dcmplx(0.0d0, 0.0d0)
  do i = 1, M
    do j = 1, N
      C(i, j) = dcmplx(dble(mod(i*3+j*7, 11)) - 5.0d0, dble(mod(i*2+j*5, 7)) - 3.0d0)
    end do
  end do

  call ZUNMRQ('R', 'C', M, N, K, A, LDA, TAU, C, LDC, WORK, LWORK, INFO)
  do j = 1, N
    do i = 1, M
      Cout((j-1)*M+i) = C(i, j)
    end do
  end do
  call begin_test('large_right_conjtrans')
  call print_int('info', INFO)
  call print_int('K', K)
  call print_int('M', M)
  call print_int('N', N)
  call print_array('C', Cout_r, 2*M*N)
  call print_array('A', Aout_r, 2*K*N)
  call print_array('TAU', TAUout_r, 2*K)
  call end_test()

end program
