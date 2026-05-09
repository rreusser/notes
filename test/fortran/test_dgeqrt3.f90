program test_dgeqrt3
  use test_utils
  implicit none

  double precision :: A(8, 8), T(8, 8)
  integer :: INFO

  ! Test 1: M=4, N=1 (base case: N=1)
  A = 0.0d0; T = 0.0d0
  A(1,1) = 2.0d0; A(2,1) = 1.5d0; A(3,1) = 0.5d0; A(4,1) = -1.25d0
  call DGEQRT3(4, 1, A, 8, T, 8, INFO)
  call begin_test('m4_n1')
  call print_matrix('A', A, 8, 4, 1)
  call print_matrix('T', T, 8, 1, 1)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 2: M=1, N=1 (degenerate base case)
  A = 0.0d0; T = 0.0d0
  A(1,1) = 3.5d0
  call DGEQRT3(1, 1, A, 8, T, 8, INFO)
  call begin_test('m1_n1')
  call print_matrix('A', A, 8, 1, 1)
  call print_matrix('T', T, 8, 1, 1)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 3: M=4, N=2 (single recursive split)
  A = 0.0d0; T = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.5d0
  A(2,1) = 0.7d0; A(2,2) = 3.0d0
  A(3,1) = 0.5d0; A(3,2) = 1.1d0
  A(4,1) = -1.0d0; A(4,2) = 0.4d0
  call DGEQRT3(4, 2, A, 8, T, 8, INFO)
  call begin_test('m4_n2')
  call print_matrix('A', A, 8, 4, 2)
  call print_matrix('T', T, 8, 2, 2)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 4: M=5, N=3 (N1=1, N2=2)
  A = 0.0d0; T = 0.0d0
  A(1,1) = 4.0d0; A(1,2) = 1.0d0; A(1,3) = 0.5d0
  A(2,1) = 0.5d0; A(2,2) = 3.5d0; A(2,3) = 1.2d0
  A(3,1) = 0.3d0; A(3,2) = 0.8d0; A(3,3) = 4.5d0
  A(4,1) = -0.25d0; A(4,2) = 0.6d0; A(4,3) = 1.1d0
  A(5,1) = 0.75d0; A(5,2) = -0.4d0; A(5,3) = 0.9d0
  call DGEQRT3(5, 3, A, 8, T, 8, INFO)
  call begin_test('m5_n3')
  call print_matrix('A', A, 8, 5, 3)
  call print_matrix('T', T, 8, 3, 3)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 5: M=6, N=4 (N1=2, N2=2 — even split with deeper recursion)
  A = 0.0d0; T = 0.0d0
  A(1,1) = 3.0d0; A(1,2) = 0.6d0; A(1,3) = 0.4d0; A(1,4) = 0.2d0
  A(2,1) = 0.5d0; A(2,2) = 4.0d0; A(2,3) = 0.7d0; A(2,4) = 0.3d0
  A(3,1) = 0.2d0; A(3,2) = 0.5d0; A(3,3) = 3.5d0; A(3,4) = 0.8d0
  A(4,1) = 0.4d0; A(4,2) = 0.3d0; A(4,3) = 0.5d0; A(4,4) = 4.5d0
  A(5,1) = 0.1d0; A(5,2) = -0.2d0; A(5,3) = 0.6d0; A(5,4) = 1.1d0
  A(6,1) = -0.3d0; A(6,2) = 0.5d0; A(6,3) = 0.1d0; A(6,4) = -0.5d0
  call DGEQRT3(6, 4, A, 8, T, 8, INFO)
  call begin_test('m6_n4')
  call print_matrix('A', A, 8, 6, 4)
  call print_matrix('T', T, 8, 4, 4)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 6: M=7, N=5 (N1=2, N2=3 — uneven split)
  A = 0.0d0; T = 0.0d0
  A(1,1) = 5.0d0; A(1,2) = 0.7d0; A(1,3) = 0.3d0; A(1,4) = -0.1d0; A(1,5) = 0.4d0
  A(2,1) = 0.6d0; A(2,2) = 4.5d0; A(2,3) = 0.8d0; A(2,4) =  0.5d0; A(2,5) = -0.3d0
  A(3,1) = 0.3d0; A(3,2) = 0.4d0; A(3,3) = 5.5d0; A(3,4) =  0.9d0; A(3,5) = 0.7d0
  A(4,1) = 0.2d0; A(4,2) = 0.5d0; A(4,3) = 0.6d0; A(4,4) =  4.8d0; A(4,5) = 1.0d0
  A(5,1) = 0.1d0; A(5,2) = 0.3d0; A(5,3) = 0.5d0; A(5,4) =  0.7d0; A(5,5) = 5.2d0
  A(6,1) = 0.2d0; A(6,2) = -0.4d0; A(6,3) = 0.3d0; A(6,4) =  0.6d0; A(6,5) = 0.9d0
  A(7,1) = 0.5d0; A(7,2) = 0.1d0; A(7,3) = -0.2d0; A(7,4) = 0.4d0; A(7,5) = 0.6d0
  call DGEQRT3(7, 5, A, 8, T, 8, INFO)
  call begin_test('m7_n5')
  call print_matrix('A', A, 8, 7, 5)
  call print_matrix('T', T, 8, 5, 5)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 7: M=N=4 (square)
  A = 0.0d0; T = 0.0d0
  A(1,1) = 4.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0; A(1,4) = 0.2d0
  A(2,1) = 0.6d0; A(2,2) = 3.5d0; A(2,3) = 0.4d0; A(2,4) = 0.1d0
  A(3,1) = 0.2d0; A(3,2) = 0.4d0; A(3,3) = 4.5d0; A(3,4) = 0.7d0
  A(4,1) = 0.3d0; A(4,2) = 0.2d0; A(4,3) = 0.5d0; A(4,4) = 5.0d0
  call DGEQRT3(4, 4, A, 8, T, 8, INFO)
  call begin_test('m4_n4')
  call print_matrix('A', A, 8, 4, 4)
  call print_matrix('T', T, 8, 4, 4)
  call print_int('INFO', INFO)
  call end_test()

end program
