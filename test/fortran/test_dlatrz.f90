program test_dlatrz
  use test_utils
  implicit none

  double precision :: A(8, 8), TAU(8), WORK(8)

  ! Test 1: 3x5 upper trapezoidal, L = N-M = 2
  A = 0.0d0
  A(1,1) = 4.0d0; A(1,2) = 1.0d0; A(1,3) = 2.0d0; A(1,4) = 3.0d0; A(1,5) = 1.0d0
                  A(2,2) = 5.0d0; A(2,3) = 1.0d0; A(2,4) = 2.0d0; A(2,5) = 4.0d0
                                  A(3,3) = 6.0d0; A(3,4) = 1.0d0; A(3,5) = 2.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DLATRZ(3, 5, 2, A, 8, TAU, WORK)
  call begin_test('3x5_l2')
  call print_matrix('A', A, 8, 3, 5)
  call print_array('TAU', TAU, 3)
  call end_test()

  ! Test 2: 4x6 upper trapezoidal, L = 2
  A = 0.0d0
  A(1,1) = 5.0d0; A(1,2) = 1.0d0; A(1,3) = 2.0d0; A(1,4) = 3.0d0; A(1,5) = 1.0d0; A(1,6) = 2.0d0
                  A(2,2) = 6.0d0; A(2,3) = 1.0d0; A(2,4) = 2.0d0; A(2,5) = 3.0d0; A(2,6) = 1.0d0
                                  A(3,3) = 7.0d0; A(3,4) = 1.0d0; A(3,5) = 2.0d0; A(3,6) = 3.0d0
                                                  A(4,4) = 8.0d0; A(4,5) = 1.0d0; A(4,6) = 2.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DLATRZ(4, 6, 2, A, 8, TAU, WORK)
  call begin_test('4x6_l2')
  call print_matrix('A', A, 8, 4, 6)
  call print_array('TAU', TAU, 4)
  call end_test()

  ! Test 3: M = N (square) — TAU should be zero
  A = 0.0d0
  A(1,1) = 3.0d0; A(1,2) = 1.0d0; A(1,3) = 2.0d0
                  A(2,2) = 4.0d0; A(2,3) = 1.0d0
                                  A(3,3) = 5.0d0
  TAU = 7.0d0; WORK = 0.0d0
  call DLATRZ(3, 3, 0, A, 8, TAU, WORK)
  call begin_test('square_3x3')
  call print_matrix('A', A, 8, 3, 3)
  call print_array('TAU', TAU, 3)
  call end_test()

  ! Test 4: M = 0 quick return
  TAU = 0.0d0; WORK = 0.0d0
  call DLATRZ(0, 3, 0, A, 8, TAU, WORK)
  call begin_test('m_zero')
  call print_array('TAU', TAU, 1)
  call end_test()

  ! Test 5: 1x3 single row
  A = 0.0d0
  A(1,1) = 3.0d0; A(1,2) = 1.0d0; A(1,3) = 2.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DLATRZ(1, 3, 2, A, 8, TAU, WORK)
  call begin_test('1x3_l2')
  call print_matrix('A', A, 8, 1, 3)
  call print_array('TAU', TAU, 1)
  call end_test()

  ! Test 6: 2x4 with L=2
  A = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0; A(1,4) = 1.0d0
                  A(2,2) = 3.0d0; A(2,3) = 1.0d0; A(2,4) = 2.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DLATRZ(2, 4, 2, A, 8, TAU, WORK)
  call begin_test('2x4_l2')
  call print_matrix('A', A, 8, 2, 4)
  call print_array('TAU', TAU, 2)
  call end_test()

end program
