program test_dtplqt2
  use test_utils
  implicit none

  double precision :: A(6, 6), B(6, 6), T(6, 6)
  integer :: INFO

  ! Test 1: M=3, N=4, L=0 (B fully rectangular)
  A = 0.0d0; B = 0.0d0; T = 0.0d0
  A(1,1) = 2.0d0
  A(2,1) = 0.5d0; A(2,2) = 3.0d0
  A(3,1) = 0.25d0; A(3,2) = 0.75d0; A(3,3) = 4.0d0
  B(1,1) = 1.0d0; B(1,2) = 0.5d0; B(1,3) = 0.25d0; B(1,4) = 0.125d0
  B(2,1) = 0.3d0; B(2,2) = 1.1d0; B(2,3) = 0.6d0;  B(2,4) = 0.2d0
  B(3,1) = 0.7d0; B(3,2) = 0.4d0; B(3,3) = 1.2d0;  B(3,4) = 0.9d0
  call DTPLQT2(3, 4, 0, A, 6, B, 6, T, 6, INFO)
  call begin_test('m3_n4_l0')
  call print_matrix('A', A, 6, 3, 3)
  call print_matrix('B', B, 6, 3, 4)
  call print_matrix('T', T, 6, 3, 3)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 2: M=3, N=4, L=3 (B last L cols lower trapezoidal)
  A = 0.0d0; B = 0.0d0; T = 0.0d0
  A(1,1) = 2.0d0
  A(2,1) = 0.5d0; A(2,2) = 3.0d0
  A(3,1) = 0.25d0; A(3,2) = 0.75d0; A(3,3) = 4.0d0
  B(1,1) = 1.0d0; B(1,2) = 0.5d0
  B(2,1) = 0.3d0; B(2,2) = 1.1d0; B(2,3) = 0.6d0
  B(3,1) = 0.7d0; B(3,2) = 0.4d0; B(3,3) = 1.2d0; B(3,4) = 0.9d0
  call DTPLQT2(3, 4, 3, A, 6, B, 6, T, 6, INFO)
  call begin_test('m3_n4_l3')
  call print_matrix('A', A, 6, 3, 3)
  call print_matrix('B', B, 6, 3, 4)
  call print_matrix('T', T, 6, 3, 3)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 3: M=3, N=3, L=2
  A = 0.0d0; B = 0.0d0; T = 0.0d0
  A(1,1) = 3.0d0
  A(2,1) = 0.5d0; A(2,2) = 2.5d0
  A(3,1) = 0.25d0; A(3,2) = 0.75d0; A(3,3) = 4.5d0
  B(1,1) = 0.9d0
  B(2,1) = 0.2d0; B(2,2) = 1.3d0
  B(3,1) = 0.6d0; B(3,2) = 0.4d0; B(3,3) = 1.1d0
  call DTPLQT2(3, 3, 2, A, 6, B, 6, T, 6, INFO)
  call begin_test('m3_n3_l2')
  call print_matrix('A', A, 6, 3, 3)
  call print_matrix('B', B, 6, 3, 3)
  call print_matrix('T', T, 6, 3, 3)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 4: M=N=L=3 (B fully lower triangular)
  A = 0.0d0; B = 0.0d0; T = 0.0d0
  A(1,1) = 2.0d0
  A(2,1) = 0.3d0; A(2,2) = 3.0d0
  A(3,1) = 0.1d0; A(3,2) = 0.2d0; A(3,3) = 4.0d0
  B(1,1) = 1.1d0
  B(2,1) = 0.4d0; B(2,2) = 1.5d0
  B(3,1) = 0.6d0; B(3,2) = 0.3d0; B(3,3) = 1.7d0
  call DTPLQT2(3, 3, 3, A, 6, B, 6, T, 6, INFO)
  call begin_test('m3_n3_l3')
  call print_matrix('A', A, 6, 3, 3)
  call print_matrix('B', B, 6, 3, 3)
  call print_matrix('T', T, 6, 3, 3)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 5: M=1, N=3, L=1
  A = 0.0d0; B = 0.0d0; T = 0.0d0
  A(1,1) = 5.0d0
  B(1,1) = 1.0d0; B(1,2) = 2.0d0; B(1,3) = 3.0d0
  call DTPLQT2(1, 3, 1, A, 6, B, 6, T, 6, INFO)
  call begin_test('m1_n3_l1')
  call print_matrix('A', A, 6, 1, 1)
  call print_matrix('B', B, 6, 1, 3)
  call print_matrix('T', T, 6, 1, 1)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 6: M=4, N=2, L=2 (tall)
  A = 0.0d0; B = 0.0d0; T = 0.0d0
  A(1,1) = 2.0d0
  A(2,1) = 0.5d0; A(2,2) = 3.0d0
  A(3,1) = 0.2d0; A(3,2) = 0.4d0; A(3,3) = 2.5d0
  A(4,1) = 0.1d0; A(4,2) = 0.3d0; A(4,3) = 0.6d0; A(4,4) = 3.5d0
  B(1,1) = 1.0d0
  B(2,1) = 0.5d0; B(2,2) = 1.2d0
  B(3,1) = 0.7d0; B(3,2) = 0.8d0
  B(4,1) = 0.3d0; B(4,2) = 0.6d0
  call DTPLQT2(4, 2, 2, A, 6, B, 6, T, 6, INFO)
  call begin_test('m4_n2_l2')
  call print_matrix('A', A, 6, 4, 4)
  call print_matrix('B', B, 6, 4, 2)
  call print_matrix('T', T, 6, 4, 4)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 7: M=0 (quick return)
  A = 0.0d0; B = 0.0d0; T = 0.0d0
  call DTPLQT2(0, 3, 0, A, 6, B, 6, T, 6, INFO)
  call begin_test('m_zero')
  call print_int('INFO', INFO)
  call end_test()

  ! Test 8: N=0 (quick return)
  A = 0.0d0; B = 0.0d0; T = 0.0d0
  call DTPLQT2(3, 0, 0, A, 6, B, 6, T, 6, INFO)
  call begin_test('n_zero')
  call print_int('INFO', INFO)
  call end_test()

end program
