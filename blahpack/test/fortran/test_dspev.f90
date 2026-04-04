program test_dspev
  use test_utils
  implicit none

  integer :: INFO, i, j, N
  double precision :: AP(30), W(10), Z(10,10), WORK(1000)

  ! Test 1: JOBZ='V', UPLO='U', 4x4 SPD matrix — eigenvalues + eigenvectors
  !  4  1  2  1
  !  1  5  1  2
  !  2  1  6  1
  !  1  2  1  7
  ! Upper packed (column-major upper triangle):
  ! col 1: 4
  ! col 2: 1, 5
  ! col 3: 2, 1, 6
  ! col 4: 1, 2, 1, 7
  AP = 0.0d0; W = 0.0d0; Z = 0.0d0
  AP(1) = 4.0d0
  AP(2) = 1.0d0; AP(3) = 5.0d0
  AP(4) = 2.0d0; AP(5) = 1.0d0; AP(6) = 6.0d0
  AP(7) = 1.0d0; AP(8) = 2.0d0; AP(9) = 1.0d0; AP(10) = 7.0d0
  call DSPEV('V', 'U', 4, AP, W, Z, 4, WORK, INFO)
  call begin_test('jobz_v_uplo_u_4x4')
  call print_int('info', INFO)
  call print_array('w', W, 4)
  call print_matrix('Z', Z, 4, 4, 4)
  call end_test()

  ! Test 2: JOBZ='V', UPLO='L', 4x4 same matrix
  ! Lower packed (column-major lower triangle):
  ! col 1: 4, 1, 2, 1
  ! col 2: 5, 1, 2
  ! col 3: 6, 1
  ! col 4: 7
  AP = 0.0d0; W = 0.0d0; Z = 0.0d0
  AP(1) = 4.0d0; AP(2) = 1.0d0; AP(3) = 2.0d0; AP(4) = 1.0d0
  AP(5) = 5.0d0; AP(6) = 1.0d0; AP(7) = 2.0d0
  AP(8) = 6.0d0; AP(9) = 1.0d0
  AP(10) = 7.0d0
  call DSPEV('V', 'L', 4, AP, W, Z, 4, WORK, INFO)
  call begin_test('jobz_v_uplo_l_4x4')
  call print_int('info', INFO)
  call print_array('w', W, 4)
  call print_matrix('Z', Z, 4, 4, 4)
  call end_test()

  ! Test 3: JOBZ='N', UPLO='U', 4x4 — eigenvalues only
  AP = 0.0d0; W = 0.0d0; Z = 0.0d0
  AP(1) = 4.0d0
  AP(2) = 1.0d0; AP(3) = 5.0d0
  AP(4) = 2.0d0; AP(5) = 1.0d0; AP(6) = 6.0d0
  AP(7) = 1.0d0; AP(8) = 2.0d0; AP(9) = 1.0d0; AP(10) = 7.0d0
  call DSPEV('N', 'U', 4, AP, W, Z, 1, WORK, INFO)
  call begin_test('jobz_n_uplo_u_4x4')
  call print_int('info', INFO)
  call print_array('w', W, 4)
  call end_test()

  ! Test 4: JOBZ='N', UPLO='L', 4x4 — eigenvalues only
  AP = 0.0d0; W = 0.0d0; Z = 0.0d0
  AP(1) = 4.0d0; AP(2) = 1.0d0; AP(3) = 2.0d0; AP(4) = 1.0d0
  AP(5) = 5.0d0; AP(6) = 1.0d0; AP(7) = 2.0d0
  AP(8) = 6.0d0; AP(9) = 1.0d0
  AP(10) = 7.0d0
  call DSPEV('N', 'L', 4, AP, W, Z, 1, WORK, INFO)
  call begin_test('jobz_n_uplo_l_4x4')
  call print_int('info', INFO)
  call print_array('w', W, 4)
  call end_test()

  ! Test 5: JOBZ='V', UPLO='L', 3x3
  !  5  1  2
  !  1  4  1
  !  2  1  6
  ! Lower packed: 5, 1, 2, 4, 1, 6
  AP = 0.0d0; W = 0.0d0; Z = 0.0d0
  AP(1) = 5.0d0; AP(2) = 1.0d0; AP(3) = 2.0d0
  AP(4) = 4.0d0; AP(5) = 1.0d0
  AP(6) = 6.0d0
  call DSPEV('V', 'L', 3, AP, W, Z, 3, WORK, INFO)
  call begin_test('jobz_v_uplo_l_3x3')
  call print_int('info', INFO)
  call print_array('w', W, 3)
  call print_matrix('Z', Z, 3, 3, 3)
  call end_test()

  ! Test 6: JOBZ='V', UPLO='U', 3x3
  ! Upper packed: 5, 1, 4, 2, 1, 6
  AP = 0.0d0; W = 0.0d0; Z = 0.0d0
  AP(1) = 5.0d0
  AP(2) = 1.0d0; AP(3) = 4.0d0
  AP(4) = 2.0d0; AP(5) = 1.0d0; AP(6) = 6.0d0
  call DSPEV('V', 'U', 3, AP, W, Z, 3, WORK, INFO)
  call begin_test('jobz_v_uplo_u_3x3')
  call print_int('info', INFO)
  call print_array('w', W, 3)
  call print_matrix('Z', Z, 3, 3, 3)
  call end_test()

  ! Test 7: N=1, JOBZ='V'
  AP = 0.0d0; W = 0.0d0; Z = 0.0d0
  AP(1) = 3.5d0
  call DSPEV('V', 'L', 1, AP, W, Z, 1, WORK, INFO)
  call begin_test('n1_jobz_v')
  call print_int('info', INFO)
  call print_scalar('w1', W(1))
  call print_scalar('z11', Z(1,1))
  call end_test()

  ! Test 8: N=1, JOBZ='N'
  AP = 0.0d0; W = 0.0d0; Z = 0.0d0
  AP(1) = 7.25d0
  call DSPEV('N', 'U', 1, AP, W, Z, 1, WORK, INFO)
  call begin_test('n1_jobz_n')
  call print_int('info', INFO)
  call print_scalar('w1', W(1))
  call end_test()

  ! Test 9: N=0, JOBZ='V'
  call DSPEV('V', 'L', 0, AP, W, Z, 1, WORK, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call end_test()

  ! Test 10: diagonal matrix 4x4, JOBZ='V', UPLO='L'
  ! diag(3, 1, 4, 2)
  ! Lower packed: 3, 0, 0, 0, 1, 0, 0, 4, 0, 2
  AP = 0.0d0; W = 0.0d0; Z = 0.0d0
  AP(1) = 3.0d0; AP(2) = 0.0d0; AP(3) = 0.0d0; AP(4) = 0.0d0
  AP(5) = 1.0d0; AP(6) = 0.0d0; AP(7) = 0.0d0
  AP(8) = 4.0d0; AP(9) = 0.0d0
  AP(10) = 2.0d0
  call DSPEV('V', 'L', 4, AP, W, Z, 4, WORK, INFO)
  call begin_test('diagonal_4x4')
  call print_int('info', INFO)
  call print_array('w', W, 4)
  call print_matrix('Z', Z, 4, 4, 4)
  call end_test()

  ! Test 11: JOBZ='N', UPLO='L', 3x3 — eigenvalues only
  AP = 0.0d0; W = 0.0d0; Z = 0.0d0
  AP(1) = 5.0d0; AP(2) = 1.0d0; AP(3) = 2.0d0
  AP(4) = 4.0d0; AP(5) = 1.0d0
  AP(6) = 6.0d0
  call DSPEV('N', 'L', 3, AP, W, Z, 1, WORK, INFO)
  call begin_test('jobz_n_uplo_l_3x3')
  call print_int('info', INFO)
  call print_array('w', W, 3)
  call end_test()

end program
