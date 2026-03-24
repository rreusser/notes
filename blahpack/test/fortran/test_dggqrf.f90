program test_dggqrf
  use test_utils
  implicit none

  integer, parameter :: MAXN = 6
  integer, parameter :: LDA = MAXN, LDB = MAXN
  double precision :: A(LDA, MAXN), B(LDB, MAXN)
  double precision :: TAUA(MAXN), TAUB(MAXN), WORK(10000)
  integer :: INFO, N, M, P

  ! ---------------------------------------------------------------
  ! Test 1: Basic 3x3, A and B both 3x3
  ! A = [2 1 3; 1 4 2; 3 2 5]
  ! B = [1 2 1; 3 1 2; 2 3 1]
  ! ---------------------------------------------------------------
  N = 3; M = 3; P = 3
  A = 0.0d0; B = 0.0d0; TAUA = 0.0d0; TAUB = 0.0d0; WORK = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0
  A(3,1) = 3.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0
  B(1,1) = 1.0d0; B(1,2) = 2.0d0; B(1,3) = 1.0d0
  B(2,1) = 3.0d0; B(2,2) = 1.0d0; B(2,3) = 2.0d0
  B(3,1) = 2.0d0; B(3,2) = 3.0d0; B(3,3) = 1.0d0
  call DGGQRF(N, M, P, A, LDA, TAUA, B, LDB, TAUB, WORK, 10000, INFO)
  call begin_test('basic_3x3')
  call print_int('info', INFO)
  call print_matrix('A', A, LDA, N, M)
  call print_array('TAUA', TAUA, min(N, M))
  call print_matrix('B', B, LDB, N, P)
  call print_array('TAUB', TAUB, min(N, P))
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 2: M > N: N=3, M=4, P=3
  ! A is 3x4, B is 3x3
  ! ---------------------------------------------------------------
  N = 3; M = 4; P = 3
  A = 0.0d0; B = 0.0d0; TAUA = 0.0d0; TAUB = 0.0d0; WORK = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0; A(1,4) = 1.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0; A(2,4) = 3.0d0
  A(3,1) = 3.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0; A(3,4) = 2.0d0
  B(1,1) = 1.0d0; B(1,2) = 2.0d0; B(1,3) = 1.0d0
  B(2,1) = 3.0d0; B(2,2) = 1.0d0; B(2,3) = 2.0d0
  B(3,1) = 2.0d0; B(3,2) = 3.0d0; B(3,3) = 1.0d0
  call DGGQRF(N, M, P, A, LDA, TAUA, B, LDB, TAUB, WORK, 10000, INFO)
  call begin_test('m_gt_n')
  call print_int('info', INFO)
  call print_matrix('A', A, LDA, N, M)
  call print_array('TAUA', TAUA, min(N, M))
  call print_matrix('B', B, LDB, N, P)
  call print_array('TAUB', TAUB, min(N, P))
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 3: M < N: N=4, M=3, P=4
  ! A is 4x3, B is 4x4
  ! ---------------------------------------------------------------
  N = 4; M = 3; P = 4
  A = 0.0d0; B = 0.0d0; TAUA = 0.0d0; TAUB = 0.0d0; WORK = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0
  A(3,1) = 3.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0
  A(4,1) = 1.0d0; A(4,2) = 3.0d0; A(4,3) = 1.0d0
  B(1,1) = 1.0d0; B(1,2) = 2.0d0; B(1,3) = 1.0d0; B(1,4) = 3.0d0
  B(2,1) = 3.0d0; B(2,2) = 1.0d0; B(2,3) = 2.0d0; B(2,4) = 1.0d0
  B(3,1) = 2.0d0; B(3,2) = 3.0d0; B(3,3) = 1.0d0; B(3,4) = 2.0d0
  B(4,1) = 1.0d0; B(4,2) = 2.0d0; B(4,3) = 3.0d0; B(4,4) = 1.0d0
  call DGGQRF(N, M, P, A, LDA, TAUA, B, LDB, TAUB, WORK, 10000, INFO)
  call begin_test('m_lt_n')
  call print_int('info', INFO)
  call print_matrix('A', A, LDA, N, M)
  call print_array('TAUA', TAUA, min(N, M))
  call print_matrix('B', B, LDB, N, P)
  call print_array('TAUB', TAUB, min(N, P))
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 4: N=0 (quick return)
  ! ---------------------------------------------------------------
  N = 0; M = 3; P = 3
  A = 0.0d0; B = 0.0d0; TAUA = 0.0d0; TAUB = 0.0d0; WORK = 0.0d0
  call DGGQRF(N, M, P, A, LDA, TAUA, B, LDB, TAUB, WORK, 10000, INFO)
  call begin_test('n_zero')
  call print_int('info', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 5: N=1, M=1, P=1 (minimal case)
  ! ---------------------------------------------------------------
  N = 1; M = 1; P = 1
  A = 0.0d0; B = 0.0d0; TAUA = 0.0d0; TAUB = 0.0d0; WORK = 0.0d0
  A(1,1) = 5.0d0
  B(1,1) = 3.0d0
  call DGGQRF(N, M, P, A, LDA, TAUA, B, LDB, TAUB, WORK, 10000, INFO)
  call begin_test('n_one')
  call print_int('info', INFO)
  call print_matrix('A', A, LDA, N, M)
  call print_array('TAUA', TAUA, min(N, M))
  call print_matrix('B', B, LDB, N, P)
  call print_array('TAUB', TAUB, min(N, P))
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 6: Tall-skinny: N=5, M=2, P=3
  ! ---------------------------------------------------------------
  N = 5; M = 2; P = 3
  A = 0.0d0; B = 0.0d0; TAUA = 0.0d0; TAUB = 0.0d0; WORK = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0
  A(2,1) = 3.0d0; A(2,2) = 1.0d0
  A(3,1) = 2.0d0; A(3,2) = 3.0d0
  A(4,1) = 1.0d0; A(4,2) = 1.0d0
  A(5,1) = 2.0d0; A(5,2) = 2.0d0
  B(1,1) = 1.0d0; B(1,2) = 0.5d0; B(1,3) = 2.0d0
  B(2,1) = 0.5d0; B(2,2) = 3.0d0; B(2,3) = 1.0d0
  B(3,1) = 2.0d0; B(3,2) = 1.0d0; B(3,3) = 1.0d0
  B(4,1) = 1.0d0; B(4,2) = 2.0d0; B(4,3) = 0.5d0
  B(5,1) = 3.0d0; B(5,2) = 1.0d0; B(5,3) = 2.0d0
  call DGGQRF(N, M, P, A, LDA, TAUA, B, LDB, TAUB, WORK, 10000, INFO)
  call begin_test('tall_skinny')
  call print_int('info', INFO)
  call print_matrix('A', A, LDA, N, M)
  call print_array('TAUA', TAUA, min(N, M))
  call print_matrix('B', B, LDB, N, P)
  call print_array('TAUB', TAUB, min(N, P))
  call end_test()

end program
