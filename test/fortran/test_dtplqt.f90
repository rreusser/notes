program test_dtplqt
  use test_utils
  implicit none

  double precision :: A(8, 8), B(8, 8), T(8, 8)
  double precision :: WORK(64)
  integer :: INFO

  ! Test 1: M=4, N=5, L=0, MB=2 (B fully rectangular, two blocks)
  A = 0.0d0; B = 0.0d0; T = 0.0d0; WORK = 0.0d0
  A(1,1) = 2.0d0
  A(2,1) = 0.5d0;  A(2,2) = 3.0d0
  A(3,1) = 0.25d0; A(3,2) = 0.75d0; A(3,3) = 4.0d0
  A(4,1) = 0.1d0;  A(4,2) = 0.2d0;  A(4,3) = 0.3d0;  A(4,4) = 5.0d0
  B(1,1) = 1.0d0;  B(1,2) = 0.5d0;  B(1,3) = 0.25d0; B(1,4) = 0.1d0;  B(1,5) = 0.05d0
  B(2,1) = 0.3d0;  B(2,2) = 1.1d0;  B(2,3) = 0.6d0;  B(2,4) = 0.2d0;  B(2,5) = 0.4d0
  B(3,1) = 0.7d0;  B(3,2) = 0.4d0;  B(3,3) = 1.2d0;  B(3,4) = 0.9d0;  B(3,5) = 0.15d0
  B(4,1) = 0.2d0;  B(4,2) = 0.3d0;  B(4,3) = 0.4d0;  B(4,4) = 1.3d0;  B(4,5) = 0.6d0
  call DTPLQT(4, 5, 0, 2, A, 8, B, 8, T, 8, WORK, INFO)
  call begin_test('m4_n5_l0_mb2')
  call print_matrix('A', A, 8, 4, 4)
  call print_matrix('B', B, 8, 4, 5)
  call print_matrix('T', T, 8, 2, 4)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 2: M=4, N=5, L=2, MB=2 (B has trapezoidal block, two blocks)
  A = 0.0d0; B = 0.0d0; T = 0.0d0; WORK = 0.0d0
  A(1,1) = 2.0d0
  A(2,1) = 0.5d0;  A(2,2) = 3.0d0
  A(3,1) = 0.25d0; A(3,2) = 0.75d0; A(3,3) = 4.0d0
  A(4,1) = 0.1d0;  A(4,2) = 0.2d0;  A(4,3) = 0.3d0;  A(4,4) = 5.0d0
  B(1,1) = 1.0d0;  B(1,2) = 0.5d0;  B(1,3) = 0.25d0; B(1,4) = 0.1d0
  B(2,1) = 0.3d0;  B(2,2) = 1.1d0;  B(2,3) = 0.6d0;  B(2,4) = 0.2d0;  B(2,5) = 0.4d0
  B(3,1) = 0.7d0;  B(3,2) = 0.4d0;  B(3,3) = 1.2d0;  B(3,4) = 0.9d0;  B(3,5) = 0.15d0
  B(4,1) = 0.2d0;  B(4,2) = 0.3d0;  B(4,3) = 0.4d0;  B(4,4) = 1.3d0;  B(4,5) = 0.6d0
  call DTPLQT(4, 5, 2, 2, A, 8, B, 8, T, 8, WORK, INFO)
  call begin_test('m4_n5_l2_mb2')
  call print_matrix('A', A, 8, 4, 4)
  call print_matrix('B', B, 8, 4, 5)
  call print_matrix('T', T, 8, 2, 4)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 3: M=5, N=4, L=0, MB=2 (single block panels: 2,2,1)
  A = 0.0d0; B = 0.0d0; T = 0.0d0; WORK = 0.0d0
  A(1,1) = 2.0d0
  A(2,1) = 0.5d0;  A(2,2) = 3.0d0
  A(3,1) = 0.25d0; A(3,2) = 0.75d0; A(3,3) = 4.0d0
  A(4,1) = 0.1d0;  A(4,2) = 0.2d0;  A(4,3) = 0.3d0;  A(4,4) = 5.0d0
  A(5,1) = 0.05d0; A(5,2) = 0.15d0; A(5,3) = 0.25d0; A(5,4) = 0.35d0; A(5,5) = 6.0d0
  B(1,1) = 1.0d0;  B(1,2) = 0.5d0;  B(1,3) = 0.25d0; B(1,4) = 0.1d0
  B(2,1) = 0.3d0;  B(2,2) = 1.1d0;  B(2,3) = 0.6d0;  B(2,4) = 0.2d0
  B(3,1) = 0.7d0;  B(3,2) = 0.4d0;  B(3,3) = 1.2d0;  B(3,4) = 0.9d0
  B(4,1) = 0.2d0;  B(4,2) = 0.3d0;  B(4,3) = 0.4d0;  B(4,4) = 1.3d0
  B(5,1) = 0.15d0; B(5,2) = 0.25d0; B(5,3) = 0.35d0; B(5,4) = 0.45d0
  call DTPLQT(5, 4, 0, 2, A, 8, B, 8, T, 8, WORK, INFO)
  call begin_test('m5_n4_l0_mb2')
  call print_matrix('A', A, 8, 5, 5)
  call print_matrix('B', B, 8, 5, 4)
  call print_matrix('T', T, 8, 2, 5)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 4: M=5, N=5, L=5, MB=2 (B fully lower triangular)
  A = 0.0d0; B = 0.0d0; T = 0.0d0; WORK = 0.0d0
  A(1,1) = 2.0d0
  A(2,1) = 0.5d0;  A(2,2) = 3.0d0
  A(3,1) = 0.25d0; A(3,2) = 0.75d0; A(3,3) = 4.0d0
  A(4,1) = 0.1d0;  A(4,2) = 0.2d0;  A(4,3) = 0.3d0;  A(4,4) = 5.0d0
  A(5,1) = 0.05d0; A(5,2) = 0.15d0; A(5,3) = 0.25d0; A(5,4) = 0.35d0; A(5,5) = 6.0d0
  B(1,1) = 1.0d0
  B(2,1) = 0.3d0;  B(2,2) = 1.1d0
  B(3,1) = 0.7d0;  B(3,2) = 0.4d0;  B(3,3) = 1.2d0
  B(4,1) = 0.2d0;  B(4,2) = 0.3d0;  B(4,3) = 0.4d0;  B(4,4) = 1.3d0
  B(5,1) = 0.15d0; B(5,2) = 0.25d0; B(5,3) = 0.35d0; B(5,4) = 0.45d0; B(5,5) = 1.4d0
  call DTPLQT(5, 5, 5, 2, A, 8, B, 8, T, 8, WORK, INFO)
  call begin_test('m5_n5_l5_mb2')
  call print_matrix('A', A, 8, 5, 5)
  call print_matrix('B', B, 8, 5, 5)
  call print_matrix('T', T, 8, 2, 5)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 5: MB=1 (every block is unblocked; exercises pure unblocked-driver behavior)
  A = 0.0d0; B = 0.0d0; T = 0.0d0; WORK = 0.0d0
  A(1,1) = 2.0d0
  A(2,1) = 0.5d0;  A(2,2) = 3.0d0
  A(3,1) = 0.25d0; A(3,2) = 0.75d0; A(3,3) = 4.0d0
  B(1,1) = 1.0d0;  B(1,2) = 0.5d0
  B(2,1) = 0.3d0;  B(2,2) = 1.1d0; B(2,3) = 0.6d0
  B(3,1) = 0.7d0;  B(3,2) = 0.4d0; B(3,3) = 1.2d0; B(3,4) = 0.9d0
  call DTPLQT(3, 4, 3, 1, A, 8, B, 8, T, 8, WORK, INFO)
  call begin_test('m3_n4_l3_mb1')
  call print_matrix('A', A, 8, 3, 3)
  call print_matrix('B', B, 8, 3, 4)
  call print_matrix('T', T, 8, 1, 3)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 6: MB=M (single block, no blocked update)
  A = 0.0d0; B = 0.0d0; T = 0.0d0; WORK = 0.0d0
  A(1,1) = 2.0d0
  A(2,1) = 0.5d0;  A(2,2) = 3.0d0
  A(3,1) = 0.25d0; A(3,2) = 0.75d0; A(3,3) = 4.0d0
  B(1,1) = 1.0d0;  B(1,2) = 0.5d0;  B(1,3) = 0.25d0
  B(2,1) = 0.3d0;  B(2,2) = 1.1d0;  B(2,3) = 0.6d0
  B(3,1) = 0.7d0;  B(3,2) = 0.4d0;  B(3,3) = 1.2d0
  call DTPLQT(3, 3, 0, 3, A, 8, B, 8, T, 8, WORK, INFO)
  call begin_test('m3_n3_l0_mb3')
  call print_matrix('A', A, 8, 3, 3)
  call print_matrix('B', B, 8, 3, 3)
  call print_matrix('T', T, 8, 3, 3)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 7: M=0 (quick return)
  A = 0.0d0; B = 0.0d0; T = 0.0d0; WORK = 0.0d0
  call DTPLQT(0, 3, 0, 1, A, 8, B, 8, T, 8, WORK, INFO)
  call begin_test('m_zero')
  call print_int('INFO', INFO)
  call end_test()

  ! Test 8: N=0 (quick return)
  A = 0.0d0; B = 0.0d0; T = 0.0d0; WORK = 0.0d0
  call DTPLQT(3, 0, 0, 2, A, 8, B, 8, T, 8, WORK, INFO)
  call begin_test('n_zero')
  call print_int('INFO', INFO)
  call end_test()

  ! Test 9: M=6, N=4, L=0, MB=3 (two equal-sized blocks of size 3)
  A = 0.0d0; B = 0.0d0; T = 0.0d0; WORK = 0.0d0
  A(1,1) = 2.0d0
  A(2,1) = 0.5d0;  A(2,2) = 3.0d0
  A(3,1) = 0.25d0; A(3,2) = 0.75d0; A(3,3) = 4.0d0
  A(4,1) = 0.1d0;  A(4,2) = 0.2d0;  A(4,3) = 0.3d0;  A(4,4) = 5.0d0
  A(5,1) = 0.05d0; A(5,2) = 0.15d0; A(5,3) = 0.25d0; A(5,4) = 0.35d0; A(5,5) = 6.0d0
  A(6,1) = 0.04d0; A(6,2) = 0.14d0; A(6,3) = 0.24d0; A(6,4) = 0.34d0; A(6,5) = 0.44d0; A(6,6) = 7.0d0
  B(1,1) = 1.0d0;  B(1,2) = 0.5d0;  B(1,3) = 0.25d0; B(1,4) = 0.1d0
  B(2,1) = 0.3d0;  B(2,2) = 1.1d0;  B(2,3) = 0.6d0;  B(2,4) = 0.2d0
  B(3,1) = 0.7d0;  B(3,2) = 0.4d0;  B(3,3) = 1.2d0;  B(3,4) = 0.9d0
  B(4,1) = 0.2d0;  B(4,2) = 0.3d0;  B(4,3) = 0.4d0;  B(4,4) = 1.3d0
  B(5,1) = 0.15d0; B(5,2) = 0.25d0; B(5,3) = 0.35d0; B(5,4) = 0.45d0
  B(6,1) = 0.13d0; B(6,2) = 0.23d0; B(6,3) = 0.33d0; B(6,4) = 0.43d0
  call DTPLQT(6, 4, 0, 3, A, 8, B, 8, T, 8, WORK, INFO)
  call begin_test('m6_n4_l0_mb3')
  call print_matrix('A', A, 8, 6, 6)
  call print_matrix('B', B, 8, 6, 4)
  call print_matrix('T', T, 8, 3, 6)
  call print_int('INFO', INFO)
  call end_test()

end program
