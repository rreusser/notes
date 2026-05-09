program test_dgelqt3
  use test_utils
  implicit none

  double precision :: A(8, 8), T(8, 8)
  integer :: INFO

  ! Test 1: M=1, N=4 (base case)
  A = 0.0d0; T = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.5d0; A(1,3) = 0.5d0; A(1,4) = -1.25d0
  call DGELQT3(1, 4, A, 8, T, 8, INFO)
  call begin_test('m1_n4')
  call print_matrix('A', A, 8, 1, 4)
  call print_matrix('T', T, 8, 1, 1)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 2: M=1, N=1 (degenerate base case)
  A = 0.0d0; T = 0.0d0
  A(1,1) = 3.5d0
  call DGELQT3(1, 1, A, 8, T, 8, INFO)
  call begin_test('m1_n1')
  call print_matrix('A', A, 8, 1, 1)
  call print_matrix('T', T, 8, 1, 1)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 3: M=2, N=4 (single recursive split)
  A = 0.0d0; T = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.5d0; A(1,3) = 0.5d0; A(1,4) = -1.0d0
  A(2,1) = 0.7d0; A(2,2) = 3.0d0; A(2,3) = 1.1d0; A(2,4) =  0.4d0
  call DGELQT3(2, 4, A, 8, T, 8, INFO)
  call begin_test('m2_n4')
  call print_matrix('A', A, 8, 2, 4)
  call print_matrix('T', T, 8, 2, 2)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 4: M=3, N=5 (M1=1, M2=2)
  A = 0.0d0; T = 0.0d0
  A(1,1) = 4.0d0; A(1,2) = 1.0d0; A(1,3) = 0.5d0; A(1,4) = -0.25d0; A(1,5) = 0.75d0
  A(2,1) = 0.5d0; A(2,2) = 3.5d0; A(2,3) = 1.2d0; A(2,4) =  0.6d0;  A(2,5) = -0.4d0
  A(3,1) = 0.3d0; A(3,2) = 0.8d0; A(3,3) = 4.5d0; A(3,4) =  1.1d0;  A(3,5) = 0.9d0
  call DGELQT3(3, 5, A, 8, T, 8, INFO)
  call begin_test('m3_n5')
  call print_matrix('A', A, 8, 3, 5)
  call print_matrix('T', T, 8, 3, 3)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 5: M=4, N=6 (M1=2, M2=2 — even split with deeper recursion)
  A = 0.0d0; T = 0.0d0
  A(1,1) = 3.0d0; A(1,2) = 0.6d0; A(1,3) = 0.4d0; A(1,4) = 0.2d0; A(1,5) = 0.1d0; A(1,6) = -0.3d0
  A(2,1) = 0.5d0; A(2,2) = 4.0d0; A(2,3) = 0.7d0; A(2,4) = 0.3d0; A(2,5) = -0.2d0; A(2,6) = 0.5d0
  A(3,1) = 0.2d0; A(3,2) = 0.5d0; A(3,3) = 3.5d0; A(3,4) = 0.8d0; A(3,5) = 0.6d0; A(3,6) = 0.1d0
  A(4,1) = 0.4d0; A(4,2) = 0.3d0; A(4,3) = 0.5d0; A(4,4) = 4.5d0; A(4,5) = 1.1d0; A(4,6) = -0.5d0
  call DGELQT3(4, 6, A, 8, T, 8, INFO)
  call begin_test('m4_n6')
  call print_matrix('A', A, 8, 4, 6)
  call print_matrix('T', T, 8, 4, 4)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 6: M=5, N=7 (M1=2, M2=3 — uneven split)
  A = 0.0d0; T = 0.0d0
  A(1,1) = 5.0d0; A(1,2) = 0.7d0; A(1,3) = 0.3d0; A(1,4) = -0.1d0; A(1,5) = 0.4d0; A(1,6) = 0.2d0; A(1,7) = 0.5d0
  A(2,1) = 0.6d0; A(2,2) = 4.5d0; A(2,3) = 0.8d0; A(2,4) =  0.5d0; A(2,5) = -0.3d0; A(2,6) = 0.7d0; A(2,7) = 0.1d0
  A(3,1) = 0.3d0; A(3,2) = 0.4d0; A(3,3) = 5.5d0; A(3,4) =  0.9d0; A(3,5) = 0.7d0; A(3,6) = -0.2d0; A(3,7) = 0.4d0
  A(4,1) = 0.2d0; A(4,2) = 0.5d0; A(4,3) = 0.6d0; A(4,4) =  4.8d0; A(4,5) = 1.0d0; A(4,6) = 0.3d0; A(4,7) = -0.4d0
  A(5,1) = 0.1d0; A(5,2) = 0.3d0; A(5,3) = 0.5d0; A(5,4) =  0.7d0; A(5,5) = 5.2d0; A(5,6) = 0.9d0; A(5,7) = 0.6d0
  call DGELQT3(5, 7, A, 8, T, 8, INFO)
  call begin_test('m5_n7')
  call print_matrix('A', A, 8, 5, 7)
  call print_matrix('T', T, 8, 5, 5)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 7: M=N=4 (square)
  A = 0.0d0; T = 0.0d0
  A(1,1) = 4.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0; A(1,4) = 0.2d0
  A(2,1) = 0.6d0; A(2,2) = 3.5d0; A(2,3) = 0.4d0; A(2,4) = 0.1d0
  A(3,1) = 0.2d0; A(3,2) = 0.4d0; A(3,3) = 4.5d0; A(3,4) = 0.7d0
  A(4,1) = 0.3d0; A(4,2) = 0.2d0; A(4,3) = 0.5d0; A(4,4) = 5.0d0
  call DGELQT3(4, 4, A, 8, T, 8, INFO)
  call begin_test('m4_n4')
  call print_matrix('A', A, 8, 4, 4)
  call print_matrix('T', T, 8, 4, 4)
  call print_int('INFO', INFO)
  call end_test()

end program
