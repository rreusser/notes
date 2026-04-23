program test_dggrqf
  use test_utils
  implicit none

  integer, parameter :: MAXN = 6
  integer, parameter :: LDA = MAXN, LDB = MAXN
  double precision :: A(LDA, MAXN), B(LDB, MAXN)
  double precision :: TAUA(MAXN), TAUB(MAXN), WORK(10000)
  integer :: INFO, M, P, N

  ! ---------------------------------------------------------------
  ! Test 1: Basic 3x3, A is 3x3, B is 3x3
  ! A = [2 1 3; 1 4 2; 3 2 5]
  ! B = [1 2 1; 3 1 2; 2 3 1]
  ! ---------------------------------------------------------------
  M = 3; P = 3; N = 3
  A = 0.0d0; B = 0.0d0; TAUA = 0.0d0; TAUB = 0.0d0; WORK = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0
  A(3,1) = 3.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0
  B(1,1) = 1.0d0; B(1,2) = 2.0d0; B(1,3) = 1.0d0
  B(2,1) = 3.0d0; B(2,2) = 1.0d0; B(2,3) = 2.0d0
  B(3,1) = 2.0d0; B(3,2) = 3.0d0; B(3,3) = 1.0d0
  call DGGRQF(M, P, N, A, LDA, TAUA, B, LDB, TAUB, WORK, 10000, INFO)
  call begin_test('basic_3x3')
  call print_int('info', INFO)
  call print_matrix('A', A, LDA, M, N)
  call print_array('TAUA', TAUA, min(M, N))
  call print_matrix('B', B, LDB, P, N)
  call print_array('TAUB', TAUB, min(P, N))
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 2: M < N: M=2, P=3, N=4 (A is 2x4, B is 3x4)
  ! ---------------------------------------------------------------
  M = 2; P = 3; N = 4
  A = 0.0d0; B = 0.0d0; TAUA = 0.0d0; TAUB = 0.0d0; WORK = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0; A(1,4) = 1.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0; A(2,4) = 3.0d0
  B(1,1) = 1.0d0; B(1,2) = 2.0d0; B(1,3) = 1.0d0; B(1,4) = 3.0d0
  B(2,1) = 3.0d0; B(2,2) = 1.0d0; B(2,3) = 2.0d0; B(2,4) = 1.0d0
  B(3,1) = 2.0d0; B(3,2) = 3.0d0; B(3,3) = 1.0d0; B(3,4) = 2.0d0
  call DGGRQF(M, P, N, A, LDA, TAUA, B, LDB, TAUB, WORK, 10000, INFO)
  call begin_test('m_lt_n')
  call print_int('info', INFO)
  call print_matrix('A', A, LDA, M, N)
  call print_array('TAUA', TAUA, min(M, N))
  call print_matrix('B', B, LDB, P, N)
  call print_array('TAUB', TAUB, min(P, N))
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 3: M > N: M=4, P=3, N=3 (A is 4x3, B is 3x3)
  ! ---------------------------------------------------------------
  M = 4; P = 3; N = 3
  A = 0.0d0; B = 0.0d0; TAUA = 0.0d0; TAUB = 0.0d0; WORK = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0
  A(3,1) = 3.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0
  A(4,1) = 1.0d0; A(4,2) = 3.0d0; A(4,3) = 1.0d0
  B(1,1) = 1.0d0; B(1,2) = 2.0d0; B(1,3) = 1.0d0
  B(2,1) = 3.0d0; B(2,2) = 1.0d0; B(2,3) = 2.0d0
  B(3,1) = 2.0d0; B(3,2) = 3.0d0; B(3,3) = 1.0d0
  call DGGRQF(M, P, N, A, LDA, TAUA, B, LDB, TAUB, WORK, 10000, INFO)
  call begin_test('m_gt_n')
  call print_int('info', INFO)
  call print_matrix('A', A, LDA, M, N)
  call print_array('TAUA', TAUA, min(M, N))
  call print_matrix('B', B, LDB, P, N)
  call print_array('TAUB', TAUB, min(P, N))
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 4: Quick return: M=0
  ! ---------------------------------------------------------------
  M = 0; P = 3; N = 3
  A = 0.0d0; B = 0.0d0; TAUA = 0.0d0; TAUB = 0.0d0; WORK = 0.0d0
  B(1,1) = 1.0d0; B(1,2) = 2.0d0; B(1,3) = 1.0d0
  B(2,1) = 3.0d0; B(2,2) = 1.0d0; B(2,3) = 2.0d0
  B(3,1) = 2.0d0; B(3,2) = 3.0d0; B(3,3) = 1.0d0
  call DGGRQF(M, P, N, A, LDA, TAUA, B, LDB, TAUB, WORK, 10000, INFO)
  call begin_test('m_zero')
  call print_int('info', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 5: N=1, M=1, P=1 (minimal case)
  ! ---------------------------------------------------------------
  M = 1; P = 1; N = 1
  A = 0.0d0; B = 0.0d0; TAUA = 0.0d0; TAUB = 0.0d0; WORK = 0.0d0
  A(1,1) = 5.0d0
  B(1,1) = 3.0d0
  call DGGRQF(M, P, N, A, LDA, TAUA, B, LDB, TAUB, WORK, 10000, INFO)
  call begin_test('m_one')
  call print_int('info', INFO)
  call print_matrix('A', A, LDA, M, N)
  call print_array('TAUA', TAUA, min(M, N))
  call print_matrix('B', B, LDB, P, N)
  call print_array('TAUB', TAUB, min(P, N))
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 6: P > N: M=3, P=5, N=3 (A is 3x3, B is 5x3)
  ! ---------------------------------------------------------------
  M = 3; P = 5; N = 3
  A = 0.0d0; B = 0.0d0; TAUA = 0.0d0; TAUB = 0.0d0; WORK = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 3.0d0
  A(2,1) = 4.0d0; A(2,2) = 5.0d0; A(2,3) = 6.0d0
  A(3,1) = 7.0d0; A(3,2) = 8.0d0; A(3,3) = 9.0d0
  B(1,1) = 1.0d0; B(1,2) = 0.5d0; B(1,3) = 2.0d0
  B(2,1) = 0.5d0; B(2,2) = 3.0d0; B(2,3) = 1.0d0
  B(3,1) = 2.0d0; B(3,2) = 1.0d0; B(3,3) = 1.0d0
  B(4,1) = 1.0d0; B(4,2) = 2.0d0; B(4,3) = 0.5d0
  B(5,1) = 3.0d0; B(5,2) = 1.0d0; B(5,3) = 2.0d0
  call DGGRQF(M, P, N, A, LDA, TAUA, B, LDB, TAUB, WORK, 10000, INFO)
  call begin_test('p_gt_n')
  call print_int('info', INFO)
  call print_matrix('A', A, LDA, M, N)
  call print_array('TAUA', TAUA, min(M, N))
  call print_matrix('B', B, LDB, P, N)
  call print_array('TAUB', TAUB, min(P, N))
  call end_test()

end program
