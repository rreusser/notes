program test_zlatrz
  use test_utils
  implicit none

  integer, parameter :: LDA = 8
  complex*16 :: A(LDA, LDA), TAU(LDA), WORK(LDA)
  complex*16 :: Apk(LDA * LDA), TAUpk(LDA)
  double precision :: Apk_r(2 * LDA * LDA), TAUpk_r(2 * LDA)
  equivalence (Apk, Apk_r)
  equivalence (TAUpk, TAUpk_r)

  integer :: M, N, L, i, j

  ! Test 1: 3x5 upper trapezoidal, L = N-M = 2
  M = 3; N = 5; L = 2
  A = (0.0d0, 0.0d0)
  A(1,1) = ( 4.0d0,  0.5d0); A(1,2) = ( 1.0d0, -0.2d0); A(1,3) = ( 2.0d0, 0.3d0)
  A(1,4) = ( 3.0d0,  0.1d0); A(1,5) = ( 1.0d0, -0.4d0)
  A(2,2) = ( 5.0d0,  0.3d0); A(2,3) = ( 1.0d0, 0.1d0)
  A(2,4) = ( 2.0d0,  0.2d0); A(2,5) = ( 4.0d0, -0.5d0)
  A(3,3) = ( 6.0d0, 0.4d0)
  A(3,4) = ( 1.0d0, -0.2d0); A(3,5) = ( 2.0d0,  0.6d0)
  TAU = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  call ZLATRZ(M, N, L, A, LDA, TAU, WORK)
  call begin_test('3x5_l2')
  do j = 1, N
    do i = 1, M
      Apk(i + (j-1)*M) = A(i, j)
    end do
  end do
  do i = 1, M
    TAUpk(i) = TAU(i)
  end do
  call print_array('A', Apk_r, 2*M*N)
  call print_array('TAU', TAUpk_r, 2*M)
  call end_test()

  ! Test 2: 4x6 upper trapezoidal, L = 2
  M = 4; N = 6; L = 2
  A = (0.0d0, 0.0d0)
  A(1,1) = ( 5.0d0, 0.1d0); A(1,2) = ( 1.0d0, -0.3d0); A(1,3) = ( 2.0d0, 0.5d0)
  A(1,4) = ( 3.0d0, 0.2d0); A(1,5) = ( 1.0d0,  0.4d0); A(1,6) = ( 2.0d0, -0.1d0)
  A(2,2) = ( 6.0d0, -0.2d0); A(2,3) = ( 1.0d0, 0.3d0)
  A(2,4) = ( 2.0d0, -0.4d0); A(2,5) = ( 3.0d0,  0.1d0); A(2,6) = ( 1.0d0, -0.3d0)
  A(3,3) = ( 7.0d0, 0.4d0)
  A(3,4) = ( 1.0d0,  0.2d0); A(3,5) = ( 2.0d0, -0.5d0); A(3,6) = ( 3.0d0, 0.3d0)
  A(4,4) = ( 8.0d0, -0.1d0)
  A(4,5) = ( 1.0d0, 0.6d0); A(4,6) = ( 2.0d0,  0.2d0)
  TAU = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  call ZLATRZ(M, N, L, A, LDA, TAU, WORK)
  call begin_test('4x6_l2')
  do j = 1, N
    do i = 1, M
      Apk(i + (j-1)*M) = A(i, j)
    end do
  end do
  do i = 1, M
    TAUpk(i) = TAU(i)
  end do
  call print_array('A', Apk_r, 2*M*N)
  call print_array('TAU', TAUpk_r, 2*M)
  call end_test()

  ! Test 3: M = N (square) — TAU should be zero
  M = 3; N = 3; L = 0
  A = (0.0d0, 0.0d0)
  A(1,1) = ( 3.0d0, 0.5d0); A(1,2) = ( 1.0d0, -0.2d0); A(1,3) = ( 2.0d0, 0.3d0)
  A(2,2) = ( 4.0d0,  0.1d0); A(2,3) = ( 1.0d0, 0.4d0)
  A(3,3) = ( 5.0d0, -0.2d0)
  TAU(1) = (7.0d0, 7.0d0); TAU(2) = (7.0d0, 7.0d0); TAU(3) = (7.0d0, 7.0d0)
  WORK = (0.0d0, 0.0d0)
  call ZLATRZ(M, N, L, A, LDA, TAU, WORK)
  call begin_test('square_3x3')
  do j = 1, N
    do i = 1, M
      Apk(i + (j-1)*M) = A(i, j)
    end do
  end do
  do i = 1, M
    TAUpk(i) = TAU(i)
  end do
  call print_array('A', Apk_r, 2*M*N)
  call print_array('TAU', TAUpk_r, 2*M)
  call end_test()

  ! Test 4: M = 0 quick return
  TAU(1) = (9.0d0, -1.0d0)
  WORK = (0.0d0, 0.0d0)
  call ZLATRZ(0, 3, 3, A, LDA, TAU, WORK)
  call begin_test('m_zero')
  TAUpk(1) = TAU(1)
  call print_array('TAU', TAUpk_r, 2)
  call end_test()

  ! Test 5: 1x3 single row
  M = 1; N = 3; L = 2
  A = (0.0d0, 0.0d0)
  A(1,1) = (3.0d0, 0.7d0); A(1,2) = (1.0d0, -0.4d0); A(1,3) = (2.0d0, 0.5d0)
  TAU = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  call ZLATRZ(M, N, L, A, LDA, TAU, WORK)
  call begin_test('1x3_l2')
  do j = 1, N
    do i = 1, M
      Apk(i + (j-1)*M) = A(i, j)
    end do
  end do
  TAUpk(1) = TAU(1)
  call print_array('A', Apk_r, 2*M*N)
  call print_array('TAU', TAUpk_r, 2*M)
  call end_test()

  ! Test 6: 2x4 with L = 2
  M = 2; N = 4; L = 2
  A = (0.0d0, 0.0d0)
  A(1,1) = ( 2.0d0,  0.2d0); A(1,2) = ( 1.0d0, -0.1d0); A(1,3) = ( 3.0d0, 0.4d0)
  A(1,4) = ( 1.0d0, -0.3d0)
  A(2,2) = ( 3.0d0,  0.3d0); A(2,3) = ( 1.0d0, -0.2d0)
  A(2,4) = ( 2.0d0,  0.5d0)
  TAU = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  call ZLATRZ(M, N, L, A, LDA, TAU, WORK)
  call begin_test('2x4_l2')
  do j = 1, N
    do i = 1, M
      Apk(i + (j-1)*M) = A(i, j)
    end do
  end do
  do i = 1, M
    TAUpk(i) = TAU(i)
  end do
  call print_array('A', Apk_r, 2*M*N)
  call print_array('TAU', TAUpk_r, 2*M)
  call end_test()

  ! Test 7: 3x7 with L = 4 (large L)
  M = 3; N = 7; L = 4
  A = (0.0d0, 0.0d0)
  A(1,1) = ( 4.0d0, 0.2d0); A(1,2) = ( 1.0d0, -0.1d0); A(1,3) = ( 2.0d0, 0.3d0)
  A(1,4) = ( 1.0d0, -0.4d0); A(1,5) = ( 2.0d0, 0.5d0); A(1,6) = ( 3.0d0, -0.2d0); A(1,7) = ( 1.0d0, 0.4d0)
  A(2,2) = ( 5.0d0,  0.3d0); A(2,3) = ( 1.0d0, -0.2d0)
  A(2,4) = ( 2.0d0,  0.1d0); A(2,5) = ( 1.0d0, -0.3d0); A(2,6) = ( 3.0d0,  0.4d0); A(2,7) = ( 2.0d0, -0.1d0)
  A(3,3) = ( 6.0d0,  0.4d0)
  A(3,4) = ( 1.0d0, -0.5d0); A(3,5) = ( 3.0d0,  0.2d0); A(3,6) = ( 2.0d0, -0.3d0); A(3,7) = ( 4.0d0,  0.6d0)
  TAU = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  call ZLATRZ(M, N, L, A, LDA, TAU, WORK)
  call begin_test('3x7_l4')
  do j = 1, N
    do i = 1, M
      Apk(i + (j-1)*M) = A(i, j)
    end do
  end do
  do i = 1, M
    TAUpk(i) = TAU(i)
  end do
  call print_array('A', Apk_r, 2*M*N)
  call print_array('TAU', TAUpk_r, 2*M)
  call end_test()

end program
