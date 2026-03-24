program test_dtrsen
  use test_utils
  implicit none

  double precision :: T(4,4), Q(4,4), WR(4), WI(4), S, SEP
  double precision :: WORK(100)
  integer :: IWORK(100), M, INFO
  logical :: SELECT(4)

  ! Test 1: JOB='N', COMPQ='N', SORT='N', basic 4x4
  ! A quasi-triangular matrix with 2x2 block
  T = 0.0d0; Q = 0.0d0
  T(1,1) = 1.0d0; T(1,2) = 2.0d0; T(1,3) = 0.5d0; T(1,4) = 0.3d0
  T(2,2) = 3.0d0; T(2,3) = 1.0d0; T(2,4) = 0.4d0
  T(3,3) = 5.0d0; T(3,4) = 2.0d0
  T(4,3) = -1.0d0; T(4,4) = 5.0d0
  Q(1,1) = 1.0d0; Q(2,2) = 1.0d0; Q(3,3) = 1.0d0; Q(4,4) = 1.0d0
  SELECT = (/ .true., .false., .true., .true. /)
  call DTRSEN('N', 'V', SELECT, 4, T, 4, Q, 4, WR, WI, M, S, SEP, &
              WORK, 100, IWORK, 100, INFO)
  call begin_test('N V select 4x4')
  call print_array('T', T, 16)
  call print_array('Q', Q, 16)
  call print_array('WR', WR, 4)
  call print_array('WI', WI, 4)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! Test 2: No reordering needed (all selected)
  T = 0.0d0; Q = 0.0d0
  T(1,1) = 1.0d0; T(1,2) = 2.0d0
  T(2,2) = 3.0d0
  Q(1,1) = 1.0d0; Q(2,2) = 1.0d0
  SELECT(1:2) = (/ .true., .true. /)
  call DTRSEN('N', 'V', SELECT, 2, T, 4, Q, 4, WR, WI, M, S, SEP, &
              WORK, 100, IWORK, 100, INFO)
  call begin_test('all selected 2x2')
  call print_array('T', T, 16)
  call print_array('WR', WR, 4)
  call print_array('WI', WI, 4)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! Test 3: None selected
  T = 0.0d0; Q = 0.0d0
  T(1,1) = 1.0d0; T(1,2) = 2.0d0
  T(2,2) = 3.0d0
  Q(1,1) = 1.0d0; Q(2,2) = 1.0d0
  SELECT(1:2) = (/ .false., .false. /)
  call DTRSEN('N', 'V', SELECT, 2, T, 4, Q, 4, WR, WI, M, S, SEP, &
              WORK, 100, IWORK, 100, INFO)
  call begin_test('none selected')
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! Test 4: JOB='E' (compute S)
  T = 0.0d0; Q = 0.0d0
  T(1,1) = 1.0d0; T(1,2) = 2.0d0; T(1,3) = 0.5d0
  T(2,2) = 3.0d0; T(2,3) = 1.0d0
  T(3,3) = 5.0d0
  Q(1,1) = 1.0d0; Q(2,2) = 1.0d0; Q(3,3) = 1.0d0
  SELECT(1:3) = (/ .true., .false., .false. /)
  call DTRSEN('E', 'V', SELECT, 3, T, 4, Q, 4, WR, WI, M, S, SEP, &
              WORK, 100, IWORK, 100, INFO)
  call begin_test('E compute S')
  call print_array('T', T, 16)
  call print_scalar('S', S)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! Test 5: JOB='V' (compute SEP)
  T = 0.0d0; Q = 0.0d0
  T(1,1) = 1.0d0; T(1,2) = 2.0d0; T(1,3) = 0.5d0
  T(2,2) = 3.0d0; T(2,3) = 1.0d0
  T(3,3) = 5.0d0
  Q(1,1) = 1.0d0; Q(2,2) = 1.0d0; Q(3,3) = 1.0d0
  SELECT(1:3) = (/ .true., .false., .false. /)
  call DTRSEN('V', 'V', SELECT, 3, T, 4, Q, 4, WR, WI, M, S, SEP, &
              WORK, 100, IWORK, 100, INFO)
  call begin_test('V compute SEP')
  call print_array('T', T, 16)
  call print_scalar('SEP', SEP)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! Test 6: JOB='B' (compute both S and SEP)
  T = 0.0d0; Q = 0.0d0
  T(1,1) = 1.0d0; T(1,2) = 2.0d0; T(1,3) = 0.5d0
  T(2,2) = 3.0d0; T(2,3) = 1.0d0
  T(3,3) = 5.0d0
  Q(1,1) = 1.0d0; Q(2,2) = 1.0d0; Q(3,3) = 1.0d0
  SELECT(1:3) = (/ .true., .false., .false. /)
  call DTRSEN('B', 'V', SELECT, 3, T, 4, Q, 4, WR, WI, M, S, SEP, &
              WORK, 100, IWORK, 100, INFO)
  call begin_test('B compute both')
  call print_array('T', T, 16)
  call print_scalar('S', S)
  call print_scalar('SEP', SEP)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! Test 7: N=0
  call DTRSEN('N', 'V', SELECT, 0, T, 4, Q, 4, WR, WI, M, S, SEP, &
              WORK, 100, IWORK, 100, INFO)
  call begin_test('N=0')
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! Test 8: Select 2x2 block (eigenvalues 3,4 = complex pair)
  T = 0.0d0; Q = 0.0d0
  T(1,1) = 1.0d0; T(1,2) = 0.5d0; T(1,3) = 0.3d0; T(1,4) = 0.2d0
  T(2,2) = 2.0d0; T(2,3) = 0.4d0; T(2,4) = 0.1d0
  T(3,3) = 4.0d0; T(3,4) = 1.5d0
  T(4,3) = -1.5d0; T(4,4) = 4.0d0
  Q(1,1) = 1.0d0; Q(2,2) = 1.0d0; Q(3,3) = 1.0d0; Q(4,4) = 1.0d0
  SELECT = (/ .false., .false., .true., .true. /)
  call DTRSEN('N', 'V', SELECT, 4, T, 4, Q, 4, WR, WI, M, S, SEP, &
              WORK, 100, IWORK, 100, INFO)
  call begin_test('select complex pair')
  call print_array('T', T, 16)
  call print_array('Q', Q, 16)
  call print_array('WR', WR, 4)
  call print_array('WI', WI, 4)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

end program
