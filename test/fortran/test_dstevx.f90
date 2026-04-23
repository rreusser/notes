program test_dstevx
  use test_utils
  implicit none

  integer, parameter :: MAXN = 6
  integer, parameter :: LDZ = MAXN
  double precision :: D(MAXN), E(MAXN), W(MAXN), Z(LDZ, MAXN), WORK(5*MAXN)
  integer :: IWORK(5*MAXN), IFAIL(MAXN)
  integer :: INFO, M, N

  ! ---------------------------------------------------------------
  ! Test 1: JOBZ='V', RANGE='A', 4x4 tridiagonal
  ! Matrix:
  !   [ 2  1  0  0 ]
  !   [ 1  2  1  0 ]
  !   [ 0  1  2  1 ]
  !   [ 0  0  1  2 ]
  ! ---------------------------------------------------------------
  N = 4
  D(1) = 2.0d0; D(2) = 2.0d0; D(3) = 2.0d0; D(4) = 2.0d0
  E(1) = 1.0d0; E(2) = 1.0d0; E(3) = 1.0d0
  W = 0.0d0; Z = 0.0d0; WORK = 0.0d0; IWORK = 0; IFAIL = 0
  call DSTEVX('V', 'A', N, D, E, 0.0d0, 0.0d0, 0, 0, 0.0d0, &
               M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('vectors_all_4x4')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_array('w', W, M)
  call print_matrix('z', Z, LDZ, N, M)
  call print_int_array('ifail', IFAIL, M)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 2: JOBZ='N', RANGE='A', eigenvalues only
  ! Same 4x4 matrix
  ! ---------------------------------------------------------------
  N = 4
  D(1) = 2.0d0; D(2) = 2.0d0; D(3) = 2.0d0; D(4) = 2.0d0
  E(1) = 1.0d0; E(2) = 1.0d0; E(3) = 1.0d0
  W = 0.0d0; WORK = 0.0d0; IWORK = 0; IFAIL = 0
  call DSTEVX('N', 'A', N, D, E, 0.0d0, 0.0d0, 0, 0, 0.0d0, &
               M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('novec_all_4x4')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_array('w', W, M)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 3: JOBZ='V', RANGE='V', value range (1.5, 3.5]
  ! Same 4x4 matrix (eigenvalues ~ 0.586, 1.0, 3.0, 3.414)
  ! Should find eigenvalues in (1.5, 3.5]
  ! ---------------------------------------------------------------
  N = 4
  D(1) = 2.0d0; D(2) = 2.0d0; D(3) = 2.0d0; D(4) = 2.0d0
  E(1) = 1.0d0; E(2) = 1.0d0; E(3) = 1.0d0
  W = 0.0d0; Z = 0.0d0; WORK = 0.0d0; IWORK = 0; IFAIL = 0
  call DSTEVX('V', 'V', N, D, E, 1.5d0, 3.5d0, 0, 0, 0.0d0, &
               M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('vectors_value_4x4')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_array('w', W, M)
  call print_matrix('z', Z, LDZ, N, M)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 4: JOBZ='V', RANGE='I', index range IL=2, IU=3
  ! Same 4x4 matrix, should return 2nd and 3rd eigenvalues
  ! ---------------------------------------------------------------
  N = 4
  D(1) = 2.0d0; D(2) = 2.0d0; D(3) = 2.0d0; D(4) = 2.0d0
  E(1) = 1.0d0; E(2) = 1.0d0; E(3) = 1.0d0
  W = 0.0d0; Z = 0.0d0; WORK = 0.0d0; IWORK = 0; IFAIL = 0
  call DSTEVX('V', 'I', N, D, E, 0.0d0, 0.0d0, 2, 3, 0.0d0, &
               M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('vectors_index_4x4')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_array('w', W, M)
  call print_matrix('z', Z, LDZ, N, M)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 5: N=0, quick return
  ! ---------------------------------------------------------------
  N = 0
  W = 0.0d0; WORK = 0.0d0; IWORK = 0; IFAIL = 0; M = -1
  call DSTEVX('V', 'A', N, D, E, 0.0d0, 0.0d0, 0, 0, 0.0d0, &
               M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('n_zero')
  call print_int('info', INFO)
  call print_int('m', M)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 6: N=1
  ! ---------------------------------------------------------------
  N = 1
  D(1) = 3.5d0
  W = 0.0d0; Z = 0.0d0; WORK = 0.0d0; IWORK = 0; IFAIL = 0
  call DSTEVX('V', 'A', N, D, E, 0.0d0, 0.0d0, 0, 0, 0.0d0, &
               M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('n_one')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_array('w', W, M)
  call print_matrix('z', Z, LDZ, N, M)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 7: N=1, RANGE='V', value NOT in range
  ! ---------------------------------------------------------------
  N = 1
  D(1) = 3.5d0
  W = 0.0d0; Z = 0.0d0; WORK = 0.0d0; IWORK = 0; IFAIL = 0
  call DSTEVX('V', 'V', N, D, E, 5.0d0, 10.0d0, 0, 0, 0.0d0, &
               M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('n_one_out_of_range')
  call print_int('info', INFO)
  call print_int('m', M)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 8: JOBZ='N', RANGE='I', index range with eigenvalues only
  ! ---------------------------------------------------------------
  N = 4
  D(1) = 2.0d0; D(2) = 2.0d0; D(3) = 2.0d0; D(4) = 2.0d0
  E(1) = 1.0d0; E(2) = 1.0d0; E(3) = 1.0d0
  W = 0.0d0; WORK = 0.0d0; IWORK = 0; IFAIL = 0
  call DSTEVX('N', 'I', N, D, E, 0.0d0, 0.0d0, 1, 2, 0.0d0, &
               M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('novec_index_4x4')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_array('w', W, M)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 9: 6x6 non-uniform tridiagonal matrix, all eigenvalues
  ! ---------------------------------------------------------------
  N = 6
  D(1) = 1.0d0; D(2) = 3.0d0; D(3) = 2.0d0; D(4) = 4.0d0; D(5) = 1.5d0; D(6) = 2.5d0
  E(1) = 0.5d0; E(2) = 1.0d0; E(3) = 0.3d0; E(4) = 0.8d0; E(5) = 0.6d0
  W = 0.0d0; Z = 0.0d0; WORK = 0.0d0; IWORK = 0; IFAIL = 0
  call DSTEVX('V', 'A', N, D, E, 0.0d0, 0.0d0, 0, 0, 0.0d0, &
               M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('vectors_all_6x6')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_array('w', W, M)
  call print_matrix('z', Z, LDZ, N, M)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 10: JOBZ='V', RANGE='V' with ABSTOL > 0
  ! ---------------------------------------------------------------
  N = 4
  D(1) = 2.0d0; D(2) = 2.0d0; D(3) = 2.0d0; D(4) = 2.0d0
  E(1) = 1.0d0; E(2) = 1.0d0; E(3) = 1.0d0
  W = 0.0d0; Z = 0.0d0; WORK = 0.0d0; IWORK = 0; IFAIL = 0
  call DSTEVX('V', 'V', N, D, E, 0.0d0, 5.0d0, 0, 0, 1.0d-12, &
               M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('vectors_value_abstol')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_array('w', W, M)
  call print_matrix('z', Z, LDZ, N, M)
  call end_test()

end program
