program test_dsptrd
  use test_utils
  implicit none

  integer :: INFO
  double precision :: AP(30), D(10), E(10), TAU(10)

  ! Test 1: UPLO='U', 4x4 well-conditioned SPD matrix
  !  4  1  2  1
  !  1  5  1  2
  !  2  1  6  1
  !  1  2  1  7
  ! Upper packed (column-major upper triangle):
  ! col 1: 4
  ! col 2: 1, 5
  ! col 3: 2, 1, 6
  ! col 4: 1, 2, 1, 7
  AP = 0.0d0; D = 0.0d0; E = 0.0d0; TAU = 0.0d0
  AP(1) = 4.0d0
  AP(2) = 1.0d0; AP(3) = 5.0d0
  AP(4) = 2.0d0; AP(5) = 1.0d0; AP(6) = 6.0d0
  AP(7) = 1.0d0; AP(8) = 2.0d0; AP(9) = 1.0d0; AP(10) = 7.0d0
  call DSPTRD('U', 4, AP, D, E, TAU, INFO)
  call begin_test('upper_4x4')
  call print_int('info', INFO)
  call print_array('AP', AP, 10)
  call print_array('d', D, 4)
  call print_array('e', E, 3)
  call print_array('tau', TAU, 3)
  call end_test()

  ! Test 2: UPLO='L', 4x4 same symmetric matrix
  ! Lower packed (column-major lower triangle):
  ! col 1: 4, 1, 2, 1
  ! col 2: 5, 1, 2
  ! col 3: 6, 1
  ! col 4: 7
  AP = 0.0d0; D = 0.0d0; E = 0.0d0; TAU = 0.0d0
  AP(1) = 4.0d0; AP(2) = 1.0d0; AP(3) = 2.0d0; AP(4) = 1.0d0
  AP(5) = 5.0d0; AP(6) = 1.0d0; AP(7) = 2.0d0
  AP(8) = 6.0d0; AP(9) = 1.0d0
  AP(10) = 7.0d0
  call DSPTRD('L', 4, AP, D, E, TAU, INFO)
  call begin_test('lower_4x4')
  call print_int('info', INFO)
  call print_array('AP', AP, 10)
  call print_array('d', D, 4)
  call print_array('e', E, 3)
  call print_array('tau', TAU, 3)
  call end_test()

  ! Test 3: UPLO='U', 3x3 matrix
  !  2  3  1
  !  3  5  4
  !  1  4  8
  ! Upper packed: 2, 3, 5, 1, 4, 8
  AP = 0.0d0; D = 0.0d0; E = 0.0d0; TAU = 0.0d0
  AP(1) = 2.0d0
  AP(2) = 3.0d0; AP(3) = 5.0d0
  AP(4) = 1.0d0; AP(5) = 4.0d0; AP(6) = 8.0d0
  call DSPTRD('U', 3, AP, D, E, TAU, INFO)
  call begin_test('upper_3x3')
  call print_int('info', INFO)
  call print_array('AP', AP, 6)
  call print_array('d', D, 3)
  call print_array('e', E, 2)
  call print_array('tau', TAU, 2)
  call end_test()

  ! Test 4: UPLO='L', 3x3 same matrix
  ! Lower packed: 2, 3, 1, 5, 4, 8
  AP = 0.0d0; D = 0.0d0; E = 0.0d0; TAU = 0.0d0
  AP(1) = 2.0d0; AP(2) = 3.0d0; AP(3) = 1.0d0
  AP(4) = 5.0d0; AP(5) = 4.0d0
  AP(6) = 8.0d0
  call DSPTRD('L', 3, AP, D, E, TAU, INFO)
  call begin_test('lower_3x3')
  call print_int('info', INFO)
  call print_array('AP', AP, 6)
  call print_array('d', D, 3)
  call print_array('e', E, 2)
  call print_array('tau', TAU, 2)
  call end_test()

  ! Test 5: N=1 upper
  AP = 0.0d0; D = 0.0d0; E = 0.0d0; TAU = 0.0d0
  AP(1) = 3.0d0
  call DSPTRD('U', 1, AP, D, E, TAU, INFO)
  call begin_test('n_one_upper')
  call print_int('info', INFO)
  call print_scalar('AP1', AP(1))
  call print_scalar('d1', D(1))
  call end_test()

  ! Test 6: N=1 lower
  AP = 0.0d0; D = 0.0d0; E = 0.0d0; TAU = 0.0d0
  AP(1) = 3.0d0
  call DSPTRD('L', 1, AP, D, E, TAU, INFO)
  call begin_test('n_one_lower')
  call print_int('info', INFO)
  call print_scalar('AP1', AP(1))
  call print_scalar('d1', D(1))
  call end_test()

  ! Test 7: N=0 quick return
  call DSPTRD('U', 0, AP, D, E, TAU, INFO)
  call begin_test('n_zero')
  call print_int('info', INFO)
  call end_test()

  ! Test 8: UPLO='U', diagonal matrix 3x3 (all off-diag zero -> tau=0)
  ! Upper packed: 2, 0, 5, 0, 0, 8
  AP = 0.0d0; D = 0.0d0; E = 0.0d0; TAU = 0.0d0
  AP(1) = 2.0d0
  AP(2) = 0.0d0; AP(3) = 5.0d0
  AP(4) = 0.0d0; AP(5) = 0.0d0; AP(6) = 8.0d0
  call DSPTRD('U', 3, AP, D, E, TAU, INFO)
  call begin_test('upper_diagonal')
  call print_int('info', INFO)
  call print_array('d', D, 3)
  call print_array('e', E, 2)
  call print_array('tau', TAU, 2)
  call end_test()

  ! Test 9: UPLO='L', diagonal matrix 3x3
  ! Lower packed: 2, 0, 0, 5, 0, 8
  AP = 0.0d0; D = 0.0d0; E = 0.0d0; TAU = 0.0d0
  AP(1) = 2.0d0; AP(2) = 0.0d0; AP(3) = 0.0d0
  AP(4) = 5.0d0; AP(5) = 0.0d0
  AP(6) = 8.0d0
  call DSPTRD('L', 3, AP, D, E, TAU, INFO)
  call begin_test('lower_diagonal')
  call print_int('info', INFO)
  call print_array('d', D, 3)
  call print_array('e', E, 2)
  call print_array('tau', TAU, 2)
  call end_test()

end program
