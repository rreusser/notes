program test_dgeqrt2
  use test_utils
  implicit none

  double precision :: A(6, 6), T(6, 6)
  integer :: INFO

  ! Test 1: M=4, N=3 (well-conditioned tall)
  A = 0.0d0; T = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0
  A(3,1) = 3.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0
  A(4,1) = 1.0d0; A(4,2) = 3.0d0; A(4,3) = 1.0d0
  call DGEQRT2(4, 3, A, 6, T, 6, INFO)
  call begin_test('m4_n3')
  call print_matrix('A', A, 6, 4, 3)
  call print_matrix('T', T, 6, 3, 3)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 2: M=N=3 (square)
  A = 0.0d0; T = 0.0d0
  A(1,1) = 4.0d0; A(1,2) = 1.0d0; A(1,3) = 2.0d0
  A(2,1) = 1.0d0; A(2,2) = 5.0d0; A(2,3) = 3.0d0
  A(3,1) = 2.0d0; A(3,2) = 3.0d0; A(3,3) = 6.0d0
  call DGEQRT2(3, 3, A, 6, T, 6, INFO)
  call begin_test('m3_n3')
  call print_matrix('A', A, 6, 3, 3)
  call print_matrix('T', T, 6, 3, 3)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 3: M=5, N=2
  A = 0.0d0; T = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0
  A(2,1) = 3.0d0; A(2,2) = 4.0d0
  A(3,1) = 5.0d0; A(3,2) = 6.0d0
  A(4,1) = 2.0d0; A(4,2) = 1.0d0
  A(5,1) = 4.0d0; A(5,2) = 3.0d0
  call DGEQRT2(5, 2, A, 6, T, 6, INFO)
  call begin_test('m5_n2')
  call print_matrix('A', A, 6, 5, 2)
  call print_matrix('T', T, 6, 2, 2)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 4: M=N=1
  A = 0.0d0; T = 0.0d0
  A(1,1) = 7.5d0
  call DGEQRT2(1, 1, A, 6, T, 6, INFO)
  call begin_test('m1_n1')
  call print_matrix('A', A, 6, 1, 1)
  call print_matrix('T', T, 6, 1, 1)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 5: M=3, N=1
  A = 0.0d0; T = 0.0d0
  A(1,1) = 1.0d0
  A(2,1) = 2.0d0
  A(3,1) = 2.0d0
  call DGEQRT2(3, 1, A, 6, T, 6, INFO)
  call begin_test('m3_n1')
  call print_matrix('A', A, 6, 3, 1)
  call print_matrix('T', T, 6, 1, 1)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 6: M=N=4 (square, larger)
  A = 0.0d0; T = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 0.5d0; A(1,3) = 1.0d0; A(1,4) = 0.3d0
  A(2,1) = 1.0d0; A(2,2) = 3.0d0; A(2,3) = 0.7d0; A(2,4) = 0.4d0
  A(3,1) = 0.5d0; A(3,2) = 0.8d0; A(3,3) = 4.0d0; A(3,4) = 0.6d0
  A(4,1) = 0.2d0; A(4,2) = 0.9d0; A(4,3) = 1.2d0; A(4,4) = 5.0d0
  call DGEQRT2(4, 4, A, 6, T, 6, INFO)
  call begin_test('m4_n4')
  call print_matrix('A', A, 6, 4, 4)
  call print_matrix('T', T, 6, 4, 4)
  call print_int('INFO', INFO)
  call end_test()

end program
