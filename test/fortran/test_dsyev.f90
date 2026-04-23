program test_dsyev
  use test_utils
  implicit none

  integer :: INFO, LWORK, i, j, N
  double precision :: A4(4,4), A4_orig(4,4), A3(3,3), A1(1,1)
  double precision :: W4(4), W3(3), W1(1)
  double precision :: WORK(1000)

  LWORK = 1000

  ! Test 1: JOBZ='V', UPLO='L', 4x4 SPD matrix — eigenvalues + eigenvectors
  A4(1,1) = 4.0d0; A4(1,2) = 1.0d0; A4(1,3) = 2.0d0; A4(1,4) = 0.5d0
  A4(2,1) = 1.0d0; A4(2,2) = 5.0d0; A4(2,3) = 1.0d0; A4(2,4) = 1.5d0
  A4(3,1) = 2.0d0; A4(3,2) = 1.0d0; A4(3,3) = 6.0d0; A4(3,4) = 0.5d0
  A4(4,1) = 0.5d0; A4(4,2) = 1.5d0; A4(4,3) = 0.5d0; A4(4,4) = 7.0d0
  W4 = 0.0d0
  call DSYEV('V', 'L', 4, A4, 4, W4, WORK, LWORK, INFO)
  call begin_test('jobz_v_uplo_l_4x4')
  call print_int('info', INFO)
  call print_array('w', W4, 4)
  call print_matrix('Z', A4, 4, 4, 4)
  call end_test()

  ! Test 2: JOBZ='V', UPLO='U', 4x4 SPD matrix
  A4(1,1) = 4.0d0; A4(1,2) = 1.0d0; A4(1,3) = 2.0d0; A4(1,4) = 0.5d0
  A4(2,1) = 1.0d0; A4(2,2) = 5.0d0; A4(2,3) = 1.0d0; A4(2,4) = 1.5d0
  A4(3,1) = 2.0d0; A4(3,2) = 1.0d0; A4(3,3) = 6.0d0; A4(3,4) = 0.5d0
  A4(4,1) = 0.5d0; A4(4,2) = 1.5d0; A4(4,3) = 0.5d0; A4(4,4) = 7.0d0
  W4 = 0.0d0
  call DSYEV('V', 'U', 4, A4, 4, W4, WORK, LWORK, INFO)
  call begin_test('jobz_v_uplo_u_4x4')
  call print_int('info', INFO)
  call print_array('w', W4, 4)
  call print_matrix('Z', A4, 4, 4, 4)
  call end_test()

  ! Test 3: JOBZ='N', UPLO='L', 4x4 — eigenvalues only
  A4(1,1) = 4.0d0; A4(1,2) = 1.0d0; A4(1,3) = 2.0d0; A4(1,4) = 0.5d0
  A4(2,1) = 1.0d0; A4(2,2) = 5.0d0; A4(2,3) = 1.0d0; A4(2,4) = 1.5d0
  A4(3,1) = 2.0d0; A4(3,2) = 1.0d0; A4(3,3) = 6.0d0; A4(3,4) = 0.5d0
  A4(4,1) = 0.5d0; A4(4,2) = 1.5d0; A4(4,3) = 0.5d0; A4(4,4) = 7.0d0
  W4 = 0.0d0
  call DSYEV('N', 'L', 4, A4, 4, W4, WORK, LWORK, INFO)
  call begin_test('jobz_n_uplo_l_4x4')
  call print_int('info', INFO)
  call print_array('w', W4, 4)
  call end_test()

  ! Test 4: JOBZ='N', UPLO='U', 4x4 — eigenvalues only
  A4(1,1) = 4.0d0; A4(1,2) = 1.0d0; A4(1,3) = 2.0d0; A4(1,4) = 0.5d0
  A4(2,1) = 1.0d0; A4(2,2) = 5.0d0; A4(2,3) = 1.0d0; A4(2,4) = 1.5d0
  A4(3,1) = 2.0d0; A4(3,2) = 1.0d0; A4(3,3) = 6.0d0; A4(3,4) = 0.5d0
  A4(4,1) = 0.5d0; A4(4,2) = 1.5d0; A4(4,3) = 0.5d0; A4(4,4) = 7.0d0
  W4 = 0.0d0
  call DSYEV('N', 'U', 4, A4, 4, W4, WORK, LWORK, INFO)
  call begin_test('jobz_n_uplo_u_4x4')
  call print_int('info', INFO)
  call print_array('w', W4, 4)
  call end_test()

  ! Test 5: 3x3 matrix, JOBZ='V', UPLO='L'
  A3(1,1) = 5.0d0; A3(1,2) = 1.0d0; A3(1,3) = 2.0d0
  A3(2,1) = 1.0d0; A3(2,2) = 4.0d0; A3(2,3) = 1.0d0
  A3(3,1) = 2.0d0; A3(3,2) = 1.0d0; A3(3,3) = 6.0d0
  W3 = 0.0d0
  call DSYEV('V', 'L', 3, A3, 3, W3, WORK, LWORK, INFO)
  call begin_test('jobz_v_uplo_l_3x3')
  call print_int('info', INFO)
  call print_array('w', W3, 3)
  call print_matrix('Z', A3, 3, 3, 3)
  call end_test()

  ! Test 6: 3x3 matrix, JOBZ='V', UPLO='U'
  A3(1,1) = 5.0d0; A3(1,2) = 1.0d0; A3(1,3) = 2.0d0
  A3(2,1) = 1.0d0; A3(2,2) = 4.0d0; A3(2,3) = 1.0d0
  A3(3,1) = 2.0d0; A3(3,2) = 1.0d0; A3(3,3) = 6.0d0
  W3 = 0.0d0
  call DSYEV('V', 'U', 3, A3, 3, W3, WORK, LWORK, INFO)
  call begin_test('jobz_v_uplo_u_3x3')
  call print_int('info', INFO)
  call print_array('w', W3, 3)
  call print_matrix('Z', A3, 3, 3, 3)
  call end_test()

  ! Test 7: N=1, JOBZ='V'
  A1(1,1) = 3.5d0
  W1 = 0.0d0
  call DSYEV('V', 'L', 1, A1, 1, W1, WORK, LWORK, INFO)
  call begin_test('n1_jobz_v')
  call print_int('info', INFO)
  call print_scalar('w1', W1(1))
  call print_scalar('a11', A1(1,1))
  call end_test()

  ! Test 8: N=1, JOBZ='N'
  A1(1,1) = 7.25d0
  W1 = 0.0d0
  call DSYEV('N', 'U', 1, A1, 1, W1, WORK, LWORK, INFO)
  call begin_test('n1_jobz_n')
  call print_int('info', INFO)
  call print_scalar('w1', W1(1))
  call end_test()

  ! Test 9: N=0, JOBZ='V'
  call DSYEV('V', 'L', 0, A1, 1, W1, WORK, LWORK, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call end_test()

  ! Test 10: diagonal matrix (eigenvalues = diagonal entries, sorted)
  A4(1,1) = 3.0d0; A4(1,2) = 0.0d0; A4(1,3) = 0.0d0; A4(1,4) = 0.0d0
  A4(2,1) = 0.0d0; A4(2,2) = 1.0d0; A4(2,3) = 0.0d0; A4(2,4) = 0.0d0
  A4(3,1) = 0.0d0; A4(3,2) = 0.0d0; A4(3,3) = 4.0d0; A4(3,4) = 0.0d0
  A4(4,1) = 0.0d0; A4(4,2) = 0.0d0; A4(4,3) = 0.0d0; A4(4,4) = 2.0d0
  W4 = 0.0d0
  call DSYEV('V', 'L', 4, A4, 4, W4, WORK, LWORK, INFO)
  call begin_test('diagonal_4x4')
  call print_int('info', INFO)
  call print_array('w', W4, 4)
  call print_matrix('Z', A4, 4, 4, 4)
  call end_test()

  ! Test 11: JOBZ='N', UPLO='L', 3x3 — eigenvalues only
  A3(1,1) = 5.0d0; A3(1,2) = 1.0d0; A3(1,3) = 2.0d0
  A3(2,1) = 1.0d0; A3(2,2) = 4.0d0; A3(2,3) = 1.0d0
  A3(3,1) = 2.0d0; A3(3,2) = 1.0d0; A3(3,3) = 6.0d0
  W3 = 0.0d0
  call DSYEV('N', 'L', 3, A3, 3, W3, WORK, LWORK, INFO)
  call begin_test('jobz_n_uplo_l_3x3')
  call print_int('info', INFO)
  call print_array('w', W3, 3)
  call end_test()

end program
