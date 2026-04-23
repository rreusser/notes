program test_dlasy2
  use test_utils
  implicit none

  double precision :: TL(2,2), TR(2,2), B(2,2), X(2,2), SCALE, XNORM
  integer :: INFO

  ! Test 1: N1=1, N2=1
  TL = 0.0d0; TR = 0.0d0; B = 0.0d0; X = 0.0d0
  TL(1,1) = 3.0d0
  TR(1,1) = 2.0d0
  B(1,1) = 10.0d0
  call DLASY2(.false., .false., 1, 1, 1, TL, 2, TR, 2, B, 2, SCALE, X, 2, XNORM, INFO)
  call begin_test('n1=1 n2=1 basic')
  call print_scalar('scale', SCALE)
  call print_scalar('xnorm', XNORM)
  call print_array('X', X, 4)
  call print_int('info', INFO)
  call end_test()

  ! Test 2: N1=1, N2=2
  TL = 0.0d0; TR = 0.0d0; B = 0.0d0; X = 0.0d0
  TL(1,1) = 2.0d0
  TR(1,1) = 1.0d0; TR(1,2) = 0.5d0; TR(2,1) = -0.5d0; TR(2,2) = 1.0d0
  B(1,1) = 3.0d0; B(1,2) = 4.0d0
  call DLASY2(.false., .false., 1, 1, 2, TL, 2, TR, 2, B, 2, SCALE, X, 2, XNORM, INFO)
  call begin_test('n1=1 n2=2')
  call print_scalar('scale', SCALE)
  call print_scalar('xnorm', XNORM)
  call print_array('X', X, 4)
  call print_int('info', INFO)
  call end_test()

  ! Test 3: N1=2, N2=1
  TL = 0.0d0; TR = 0.0d0; B = 0.0d0; X = 0.0d0
  TL(1,1) = 2.0d0; TL(1,2) = 0.5d0; TL(2,1) = -0.5d0; TL(2,2) = 2.0d0
  TR(1,1) = 1.0d0
  B(1,1) = 3.0d0; B(2,1) = 4.0d0
  call DLASY2(.false., .false., 1, 2, 1, TL, 2, TR, 2, B, 2, SCALE, X, 2, XNORM, INFO)
  call begin_test('n1=2 n2=1')
  call print_scalar('scale', SCALE)
  call print_scalar('xnorm', XNORM)
  call print_array('X', X, 4)
  call print_int('info', INFO)
  call end_test()

  ! Test 4: N1=2, N2=2
  TL = 0.0d0; TR = 0.0d0; B = 0.0d0; X = 0.0d0
  TL(1,1) = 1.0d0; TL(1,2) = 0.5d0; TL(2,1) = -0.5d0; TL(2,2) = 1.0d0
  TR(1,1) = 2.0d0; TR(1,2) = 0.3d0; TR(2,1) = -0.3d0; TR(2,2) = 2.0d0
  B(1,1) = 1.0d0; B(1,2) = 2.0d0; B(2,1) = 3.0d0; B(2,2) = 4.0d0
  call DLASY2(.false., .false., 1, 2, 2, TL, 2, TR, 2, B, 2, SCALE, X, 2, XNORM, INFO)
  call begin_test('n1=2 n2=2')
  call print_scalar('scale', SCALE)
  call print_scalar('xnorm', XNORM)
  call print_array('X', X, 4)
  call print_int('info', INFO)
  call end_test()

  ! ---------- NEW TESTS ----------

  ! Test 5: N1=1 N2=1 ltranl=true ltranr=true isgn=-1
  TL = 0.0d0; TR = 0.0d0; B = 0.0d0; X = 0.0d0
  TL(1,1) = 3.0d0
  TR(1,1) = 2.0d0
  B(1,1) = 10.0d0
  call DLASY2(.true., .true., -1, 1, 1, TL, 2, TR, 2, B, 2, SCALE, X, 2, XNORM, INFO)
  call begin_test('n1=1 n2=1 ltranl=T ltranr=T isgn=-1')
  call print_scalar('scale', SCALE)
  call print_scalar('xnorm', XNORM)
  call print_array('X', X, 4)
  call print_int('info', INFO)
  call end_test()

  ! Test 6: N1=1 N2=2 ltranr=true
  TL = 0.0d0; TR = 0.0d0; B = 0.0d0; X = 0.0d0
  TL(1,1) = 2.0d0
  TR(1,1) = 1.0d0; TR(1,2) = 0.5d0; TR(2,1) = -0.5d0; TR(2,2) = 1.0d0
  B(1,1) = 3.0d0; B(1,2) = 4.0d0
  call DLASY2(.false., .true., 1, 1, 2, TL, 2, TR, 2, B, 2, SCALE, X, 2, XNORM, INFO)
  call begin_test('n1=1 n2=2 ltranr=T')
  call print_scalar('scale', SCALE)
  call print_scalar('xnorm', XNORM)
  call print_array('X', X, 4)
  call print_int('info', INFO)
  call end_test()

  ! Test 7: N1=2 N2=1 ltranl=true
  TL = 0.0d0; TR = 0.0d0; B = 0.0d0; X = 0.0d0
  TL(1,1) = 2.0d0; TL(1,2) = 0.5d0; TL(2,1) = -0.5d0; TL(2,2) = 2.0d0
  TR(1,1) = 1.0d0
  B(1,1) = 3.0d0; B(2,1) = 4.0d0
  call DLASY2(.true., .false., 1, 2, 1, TL, 2, TR, 2, B, 2, SCALE, X, 2, XNORM, INFO)
  call begin_test('n1=2 n2=1 ltranl=T')
  call print_scalar('scale', SCALE)
  call print_scalar('xnorm', XNORM)
  call print_array('X', X, 4)
  call print_int('info', INFO)
  call end_test()

  ! Test 8: N1=2 N2=2 ltranl=true ltranr=true
  TL = 0.0d0; TR = 0.0d0; B = 0.0d0; X = 0.0d0
  TL(1,1) = 1.0d0; TL(1,2) = 0.5d0; TL(2,1) = -0.5d0; TL(2,2) = 1.0d0
  TR(1,1) = 2.0d0; TR(1,2) = 0.3d0; TR(2,1) = -0.3d0; TR(2,2) = 2.0d0
  B(1,1) = 1.0d0; B(1,2) = 2.0d0; B(2,1) = 3.0d0; B(2,2) = 4.0d0
  call DLASY2(.true., .true., 1, 2, 2, TL, 2, TR, 2, B, 2, SCALE, X, 2, XNORM, INFO)
  call begin_test('n1=2 n2=2 ltranl=T ltranr=T')
  call print_scalar('scale', SCALE)
  call print_scalar('xnorm', XNORM)
  call print_array('X', X, 4)
  call print_int('info', INFO)
  call end_test()

  ! Test 9: N1=2 N2=2 ltranl=true ltranr=false isgn=-1
  TL = 0.0d0; TR = 0.0d0; B = 0.0d0; X = 0.0d0
  TL(1,1) = 1.0d0; TL(1,2) = 0.5d0; TL(2,1) = -0.5d0; TL(2,2) = 1.0d0
  TR(1,1) = 2.0d0; TR(1,2) = 0.3d0; TR(2,1) = -0.3d0; TR(2,2) = 2.0d0
  B(1,1) = 5.0d0; B(1,2) = 6.0d0; B(2,1) = 7.0d0; B(2,2) = 8.0d0
  call DLASY2(.true., .false., -1, 2, 2, TL, 2, TR, 2, B, 2, SCALE, X, 2, XNORM, INFO)
  call begin_test('n1=2 n2=2 ltranl=T isgn=-1')
  call print_scalar('scale', SCALE)
  call print_scalar('xnorm', XNORM)
  call print_array('X', X, 4)
  call print_int('info', INFO)
  call end_test()

  ! Test 10: N1=2 N2=2 ltranl=false ltranr=true isgn=-1
  TL = 0.0d0; TR = 0.0d0; B = 0.0d0; X = 0.0d0
  TL(1,1) = 1.0d0; TL(1,2) = 0.5d0; TL(2,1) = -0.5d0; TL(2,2) = 1.0d0
  TR(1,1) = 2.0d0; TR(1,2) = 0.3d0; TR(2,1) = -0.3d0; TR(2,2) = 2.0d0
  B(1,1) = 5.0d0; B(1,2) = 6.0d0; B(2,1) = 7.0d0; B(2,2) = 8.0d0
  call DLASY2(.false., .true., -1, 2, 2, TL, 2, TR, 2, B, 2, SCALE, X, 2, XNORM, INFO)
  call begin_test('n1=2 n2=2 ltranr=T isgn=-1')
  call print_scalar('scale', SCALE)
  call print_scalar('xnorm', XNORM)
  call print_array('X', X, 4)
  call print_int('info', INFO)
  call end_test()

  ! Test 11: N1=1 N2=1 isgn=-1
  TL = 0.0d0; TR = 0.0d0; B = 0.0d0; X = 0.0d0
  TL(1,1) = 3.0d0
  TR(1,1) = 2.0d0
  B(1,1) = 10.0d0
  call DLASY2(.false., .false., -1, 1, 1, TL, 2, TR, 2, B, 2, SCALE, X, 2, XNORM, INFO)
  call begin_test('n1=1 n2=1 isgn=-1')
  call print_scalar('scale', SCALE)
  call print_scalar('xnorm', XNORM)
  call print_array('X', X, 4)
  call print_int('info', INFO)
  call end_test()

  ! Test 12: N1=1 N2=2 isgn=-1
  TL = 0.0d0; TR = 0.0d0; B = 0.0d0; X = 0.0d0
  TL(1,1) = 2.0d0
  TR(1,1) = 1.0d0; TR(1,2) = 0.5d0; TR(2,1) = -0.5d0; TR(2,2) = 1.0d0
  B(1,1) = 3.0d0; B(1,2) = 4.0d0
  call DLASY2(.false., .false., -1, 1, 2, TL, 2, TR, 2, B, 2, SCALE, X, 2, XNORM, INFO)
  call begin_test('n1=1 n2=2 isgn=-1')
  call print_scalar('scale', SCALE)
  call print_scalar('xnorm', XNORM)
  call print_array('X', X, 4)
  call print_int('info', INFO)
  call end_test()

  ! Test 13: N1=2 N2=1 isgn=-1
  TL = 0.0d0; TR = 0.0d0; B = 0.0d0; X = 0.0d0
  TL(1,1) = 2.0d0; TL(1,2) = 0.5d0; TL(2,1) = -0.5d0; TL(2,2) = 2.0d0
  TR(1,1) = 1.0d0
  B(1,1) = 3.0d0; B(2,1) = 4.0d0
  call DLASY2(.false., .false., -1, 2, 1, TL, 2, TR, 2, B, 2, SCALE, X, 2, XNORM, INFO)
  call begin_test('n1=2 n2=1 isgn=-1')
  call print_scalar('scale', SCALE)
  call print_scalar('xnorm', XNORM)
  call print_array('X', X, 4)
  call print_int('info', INFO)
  call end_test()

  ! Test 14: N1=1 N2=1 near-singular (TL + sgn*TR ~ 0), triggers bet<=SMLNUM
  TL = 0.0d0; TR = 0.0d0; B = 0.0d0; X = 0.0d0
  TL(1,1) = 1.0d-300
  TR(1,1) = -1.0d-300
  B(1,1) = 1.0d0
  call DLASY2(.false., .false., 1, 1, 1, TL, 2, TR, 2, B, 2, SCALE, X, 2, XNORM, INFO)
  call begin_test('n1=1 n2=1 near-singular')
  call print_scalar('scale', SCALE)
  call print_scalar('xnorm', XNORM)
  call print_array('X', X, 4)
  call print_int('info', INFO)
  call end_test()

end program
