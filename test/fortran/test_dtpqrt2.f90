program test_dtpqrt2
  use test_utils
  implicit none

  double precision :: A(6, 6), B(6, 6), T(6, 6)
  integer :: INFO

  ! Test 1: M=4, N=3, L=0 (B fully rectangular, M-L = 4 rows of B1)
  ! A is 3x3 upper triangular; B is 4x3 rectangular
  A = 0.0d0; B = 0.0d0; T = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 0.5d0; A(1,3) = 0.25d0
                  A(2,2) = 3.0d0; A(2,3) = 0.75d0
                                  A(3,3) = 4.0d0
  B(1,1) = 1.0d0; B(1,2) = 0.5d0; B(1,3) = 0.25d0
  B(2,1) = 0.3d0; B(2,2) = 1.1d0; B(2,3) = 0.6d0
  B(3,1) = 0.7d0; B(3,2) = 0.4d0; B(3,3) = 1.2d0
  B(4,1) = 0.2d0; B(4,2) = 0.9d0; B(4,3) = 0.8d0
  call DTPQRT2(4, 3, 0, A, 6, B, 6, T, 6, INFO)
  call begin_test('m4_n3_l0')
  call print_matrix('A', A, 6, 3, 3)
  call print_matrix('B', B, 6, 4, 3)
  call print_matrix('T', T, 6, 3, 3)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 2: M=4, N=3, L=3 (B last L rows upper trapezoidal — B is 1 rect row + 3 trap rows)
  A = 0.0d0; B = 0.0d0; T = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 0.5d0; A(1,3) = 0.25d0
                  A(2,2) = 3.0d0; A(2,3) = 0.75d0
                                  A(3,3) = 4.0d0
  ! M-L = 1 rectangular row
  B(1,1) = 1.0d0; B(1,2) = 0.5d0; B(1,3) = 0.25d0
  ! L=3 upper trapezoidal rows (first L rows of an N-by-N upper triangular)
  B(2,1) = 1.1d0; B(2,2) = 0.6d0; B(2,3) = 0.2d0
                  B(3,2) = 1.2d0; B(3,3) = 0.9d0
                                  B(4,3) = 1.5d0
  call DTPQRT2(4, 3, 3, A, 6, B, 6, T, 6, INFO)
  call begin_test('m4_n3_l3')
  call print_matrix('A', A, 6, 3, 3)
  call print_matrix('B', B, 6, 4, 3)
  call print_matrix('T', T, 6, 3, 3)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 3: M=3, N=3, L=2
  A = 0.0d0; B = 0.0d0; T = 0.0d0
  A(1,1) = 3.0d0; A(1,2) = 0.5d0; A(1,3) = 0.25d0
                  A(2,2) = 2.5d0; A(2,3) = 0.75d0
                                  A(3,3) = 4.5d0
  ! M-L = 1 rectangular row
  B(1,1) = 0.9d0; B(1,2) = 0.2d0; B(1,3) = 0.6d0
  ! L=2 upper trapezoidal rows (first L rows of 3x3 upper triangular, padded)
  B(2,2) = 1.3d0; B(2,3) = 0.4d0
                  B(3,3) = 1.1d0
  call DTPQRT2(3, 3, 2, A, 6, B, 6, T, 6, INFO)
  call begin_test('m3_n3_l2')
  call print_matrix('A', A, 6, 3, 3)
  call print_matrix('B', B, 6, 3, 3)
  call print_matrix('T', T, 6, 3, 3)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 4: M=N=L=3 (B fully upper triangular stuck under A)
  A = 0.0d0; B = 0.0d0; T = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 0.3d0; A(1,3) = 0.1d0
                  A(2,2) = 3.0d0; A(2,3) = 0.2d0
                                  A(3,3) = 4.0d0
  B(1,1) = 1.1d0; B(1,2) = 0.4d0; B(1,3) = 0.6d0
                  B(2,2) = 1.5d0; B(2,3) = 0.3d0
                                  B(3,3) = 1.7d0
  call DTPQRT2(3, 3, 3, A, 6, B, 6, T, 6, INFO)
  call begin_test('m3_n3_l3')
  call print_matrix('A', A, 6, 3, 3)
  call print_matrix('B', B, 6, 3, 3)
  call print_matrix('T', T, 6, 3, 3)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 5: M=3, N=1, L=1
  A = 0.0d0; B = 0.0d0; T = 0.0d0
  A(1,1) = 5.0d0
  B(1,1) = 1.0d0
  B(2,1) = 2.0d0
  B(3,1) = 3.0d0
  call DTPQRT2(3, 1, 1, A, 6, B, 6, T, 6, INFO)
  call begin_test('m3_n1_l1')
  call print_matrix('A', A, 6, 1, 1)
  call print_matrix('B', B, 6, 3, 1)
  call print_matrix('T', T, 6, 1, 1)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 6: M=2, N=4, L=2 (wide; few B rows but L=2 trapezoidal head)
  A = 0.0d0; B = 0.0d0; T = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 0.5d0; A(1,3) = 0.2d0; A(1,4) = 0.1d0
                  A(2,2) = 3.0d0; A(2,3) = 0.4d0; A(2,4) = 0.3d0
                                  A(3,3) = 2.5d0; A(3,4) = 0.6d0
                                                  A(4,4) = 3.5d0
  ! M=L=2: B is 2x4, all rows trapezoidal (first 2 rows of 4x4 upper triangular)
  B(1,1) = 1.0d0; B(1,2) = 0.5d0; B(1,3) = 0.7d0; B(1,4) = 0.3d0
                  B(2,2) = 1.2d0; B(2,3) = 0.8d0; B(2,4) = 0.6d0
  call DTPQRT2(2, 4, 2, A, 6, B, 6, T, 6, INFO)
  call begin_test('m2_n4_l2')
  call print_matrix('A', A, 6, 4, 4)
  call print_matrix('B', B, 6, 2, 4)
  call print_matrix('T', T, 6, 4, 4)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 7: M=0 (quick return)
  A = 0.0d0; B = 0.0d0; T = 0.0d0
  call DTPQRT2(0, 3, 0, A, 6, B, 6, T, 6, INFO)
  call begin_test('m_zero')
  call print_int('INFO', INFO)
  call end_test()

  ! Test 8: N=0 (quick return)
  A = 0.0d0; B = 0.0d0; T = 0.0d0
  call DTPQRT2(3, 0, 0, A, 6, B, 6, T, 6, INFO)
  call begin_test('n_zero')
  call print_int('INFO', INFO)
  call end_test()

end program
