program test_dsytd2
  use test_utils
  implicit none

  integer :: INFO
  double precision :: A4(4,4), A5(5,5), A2(2,2), A3(3,3), A1(1,1)
  double precision :: D(5), E(4), TAU(4)

  ! Test 1: UPLO='U', 4x4 well-conditioned SPD matrix
  !  4  1  2  1
  !  1  5  1  2
  !  2  1  6  1
  !  1  2  1  7
  A4(1,1) = 4.0d0; A4(1,2) = 1.0d0; A4(1,3) = 2.0d0; A4(1,4) = 1.0d0
  A4(2,1) = 1.0d0; A4(2,2) = 5.0d0; A4(2,3) = 1.0d0; A4(2,4) = 2.0d0
  A4(3,1) = 2.0d0; A4(3,2) = 1.0d0; A4(3,3) = 6.0d0; A4(3,4) = 1.0d0
  A4(4,1) = 1.0d0; A4(4,2) = 2.0d0; A4(4,3) = 1.0d0; A4(4,4) = 7.0d0
  D = 0.0d0; E = 0.0d0; TAU = 0.0d0
  call DSYTD2('U', 4, A4, 4, D, E, TAU, INFO)
  call begin_test('upper_4x4')
  call print_int('info', INFO)
  call print_matrix('A', A4, 4, 4, 4)
  call print_array('d', D, 4)
  call print_array('e', E, 3)
  call print_array('tau', TAU, 3)
  call end_test()

  ! Test 2: UPLO='L', 4x4 same symmetric matrix
  A4(1,1) = 4.0d0; A4(1,2) = 1.0d0; A4(1,3) = 2.0d0; A4(1,4) = 1.0d0
  A4(2,1) = 1.0d0; A4(2,2) = 5.0d0; A4(2,3) = 1.0d0; A4(2,4) = 2.0d0
  A4(3,1) = 2.0d0; A4(3,2) = 1.0d0; A4(3,3) = 6.0d0; A4(3,4) = 1.0d0
  A4(4,1) = 1.0d0; A4(4,2) = 2.0d0; A4(4,3) = 1.0d0; A4(4,4) = 7.0d0
  D = 0.0d0; E = 0.0d0; TAU = 0.0d0
  call DSYTD2('L', 4, A4, 4, D, E, TAU, INFO)
  call begin_test('lower_4x4')
  call print_int('info', INFO)
  call print_matrix('A', A4, 4, 4, 4)
  call print_array('d', D, 4)
  call print_array('e', E, 3)
  call print_array('tau', TAU, 3)
  call end_test()

  ! Test 3: N=1 upper edge case
  A1(1,1) = 3.0d0
  D(1) = 0.0d0
  call DSYTD2('U', 1, A1, 1, D, E, TAU, INFO)
  call begin_test('n_one_upper')
  call print_int('info', INFO)
  call print_scalar('A11', A1(1,1))
  call print_scalar('d1', D(1))
  call end_test()

  ! Test 4: N=1 lower
  A1(1,1) = 3.0d0
  D(1) = 0.0d0
  call DSYTD2('L', 1, A1, 1, D, E, TAU, INFO)
  call begin_test('n_one_lower')
  call print_int('info', INFO)
  call print_scalar('A11', A1(1,1))
  call print_scalar('d1', D(1))
  call end_test()

  ! Test 5: N=0 quick return
  call DSYTD2('U', 0, A1, 1, D, E, TAU, INFO)
  call begin_test('n_zero')
  call print_int('info', INFO)
  call end_test()

  ! Test 6: UPLO='U', 5x5 matrix
  A5(1,1) = 10.0d0; A5(1,2) = 3.0d0; A5(1,3) = 1.0d0; A5(1,4) = 0.5d0; A5(1,5) = 0.2d0
  A5(2,1) = 3.0d0;  A5(2,2) = 8.0d0; A5(2,3) = 2.0d0; A5(2,4) = 1.0d0; A5(2,5) = 0.5d0
  A5(3,1) = 1.0d0;  A5(3,2) = 2.0d0; A5(3,3) = 6.0d0; A5(3,4) = 3.0d0; A5(3,5) = 1.0d0
  A5(4,1) = 0.5d0;  A5(4,2) = 1.0d0; A5(4,3) = 3.0d0; A5(4,4) = 9.0d0; A5(4,5) = 2.0d0
  A5(5,1) = 0.2d0;  A5(5,2) = 0.5d0; A5(5,3) = 1.0d0; A5(5,4) = 2.0d0; A5(5,5) = 7.0d0
  D = 0.0d0; E = 0.0d0; TAU = 0.0d0
  call DSYTD2('U', 5, A5, 5, D, E, TAU, INFO)
  call begin_test('upper_5x5')
  call print_int('info', INFO)
  call print_matrix('A', A5, 5, 5, 5)
  call print_array('d', D, 5)
  call print_array('e', E, 4)
  call print_array('tau', TAU, 4)
  call end_test()

  ! Test 7: UPLO='L', 5x5 same matrix
  A5(1,1) = 10.0d0; A5(1,2) = 3.0d0; A5(1,3) = 1.0d0; A5(1,4) = 0.5d0; A5(1,5) = 0.2d0
  A5(2,1) = 3.0d0;  A5(2,2) = 8.0d0; A5(2,3) = 2.0d0; A5(2,4) = 1.0d0; A5(2,5) = 0.5d0
  A5(3,1) = 1.0d0;  A5(3,2) = 2.0d0; A5(3,3) = 6.0d0; A5(3,4) = 3.0d0; A5(3,5) = 1.0d0
  A5(4,1) = 0.5d0;  A5(4,2) = 1.0d0; A5(4,3) = 3.0d0; A5(4,4) = 9.0d0; A5(4,5) = 2.0d0
  A5(5,1) = 0.2d0;  A5(5,2) = 0.5d0; A5(5,3) = 1.0d0; A5(5,4) = 2.0d0; A5(5,5) = 7.0d0
  D = 0.0d0; E = 0.0d0; TAU = 0.0d0
  call DSYTD2('L', 5, A5, 5, D, E, TAU, INFO)
  call begin_test('lower_5x5')
  call print_int('info', INFO)
  call print_matrix('A', A5, 5, 5, 5)
  call print_array('d', D, 5)
  call print_array('e', E, 4)
  call print_array('tau', TAU, 4)
  call end_test()

  ! Test 8: UPLO='U', 2x2 matrix (minimal nontrivial)
  A2(1,1) = 4.0d0; A2(1,2) = 3.0d0
  A2(2,1) = 3.0d0; A2(2,2) = 5.0d0
  D = 0.0d0; E = 0.0d0; TAU = 0.0d0
  call DSYTD2('U', 2, A2, 2, D, E, TAU, INFO)
  call begin_test('upper_2x2')
  call print_int('info', INFO)
  call print_matrix('A', A2, 2, 2, 2)
  call print_array('d', D, 2)
  call print_array('e', E, 1)
  call print_array('tau', TAU, 1)
  call end_test()

  ! Test 9: UPLO='L', 2x2 matrix
  A2(1,1) = 4.0d0; A2(1,2) = 3.0d0
  A2(2,1) = 3.0d0; A2(2,2) = 5.0d0
  D = 0.0d0; E = 0.0d0; TAU = 0.0d0
  call DSYTD2('L', 2, A2, 2, D, E, TAU, INFO)
  call begin_test('lower_2x2')
  call print_int('info', INFO)
  call print_matrix('A', A2, 2, 2, 2)
  call print_array('d', D, 2)
  call print_array('e', E, 1)
  call print_array('tau', TAU, 1)
  call end_test()

  ! Test 10: UPLO='U', diagonal matrix (all off-diag zero -> tau=0)
  A3(1,1) = 2.0d0; A3(1,2) = 0.0d0; A3(1,3) = 0.0d0
  A3(2,1) = 0.0d0; A3(2,2) = 5.0d0; A3(2,3) = 0.0d0
  A3(3,1) = 0.0d0; A3(3,2) = 0.0d0; A3(3,3) = 8.0d0
  D = 0.0d0; E = 0.0d0; TAU = 0.0d0
  call DSYTD2('U', 3, A3, 3, D, E, TAU, INFO)
  call begin_test('upper_diagonal')
  call print_int('info', INFO)
  call print_array('d', D, 3)
  call print_array('e', E, 2)
  call print_array('tau', TAU, 2)
  call end_test()

  ! Test 11: UPLO='L', diagonal matrix
  A3(1,1) = 2.0d0; A3(1,2) = 0.0d0; A3(1,3) = 0.0d0
  A3(2,1) = 0.0d0; A3(2,2) = 5.0d0; A3(2,3) = 0.0d0
  A3(3,1) = 0.0d0; A3(3,2) = 0.0d0; A3(3,3) = 8.0d0
  D = 0.0d0; E = 0.0d0; TAU = 0.0d0
  call DSYTD2('L', 3, A3, 3, D, E, TAU, INFO)
  call begin_test('lower_diagonal')
  call print_int('info', INFO)
  call print_array('d', D, 3)
  call print_array('e', E, 2)
  call print_array('tau', TAU, 2)
  call end_test()

end program
