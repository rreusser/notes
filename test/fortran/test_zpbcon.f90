program test_zpbcon
  use test_utils
  implicit none
  double precision :: ab_r(120), work_r(400), rwork(20)
  complex*16 :: ab(3, 10), work(200)
  equivalence (ab, ab_r)
  equivalence (work, work_r)
  integer :: info, n, kd
  double precision :: anorm, rcond

  ! ============================================================
  ! Test 1: 4x4 HPD banded, upper, KD=1
  ! Full HPD:
  !   [4    1+i   0     0   ]
  !   [1-i  5     1+i   0   ]
  !   [0    1-i   6     2+i ]
  !   [0    0     2-i   7   ]
  n = 4; kd = 1
  ab = (0.0d0, 0.0d0)
  ! Upper banded (LDAB=KD+1=2, using 3 rows for headroom)
  ! Row kd = superdiag, Row kd+1 = main
  ab(1,1) = (0.0d0, 0.0d0); ab(2,1) = (4.0d0, 0.0d0)
  ab(1,2) = (1.0d0, 1.0d0); ab(2,2) = (5.0d0, 0.0d0)
  ab(1,3) = (1.0d0, 1.0d0); ab(2,3) = (6.0d0, 0.0d0)
  ab(1,4) = (2.0d0, 1.0d0); ab(2,4) = (7.0d0, 0.0d0)
  anorm = 11.0d0

  call ZPBTRF('U', n, kd, ab, 3, info)
  if (info .ne. 0) then
    print *, 'zpbtrf failed: info=', info
    stop
  end if
  call ZPBCON('U', n, kd, ab, 3, anorm, rcond, work, rwork, info)
  call begin_test('upper_kd1')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 2: 4x4 HPD banded, lower, KD=1
  n = 4; kd = 1
  ab = (0.0d0, 0.0d0)
  ! Lower banded: Row 1=main, Row 2=sub
  ab(1,1) = (4.0d0, 0.0d0); ab(2,1) = (1.0d0, -1.0d0)
  ab(1,2) = (5.0d0, 0.0d0); ab(2,2) = (1.0d0, -1.0d0)
  ab(1,3) = (6.0d0, 0.0d0); ab(2,3) = (2.0d0, -1.0d0)
  ab(1,4) = (7.0d0, 0.0d0)
  anorm = 11.0d0

  call ZPBTRF('L', n, kd, ab, 3, info)
  if (info .ne. 0) then
    print *, 'zpbtrf failed: info=', info
    stop
  end if
  call ZPBCON('L', n, kd, ab, 3, anorm, rcond, work, rwork, info)
  call begin_test('lower_kd1')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 3: 4x4 HPD banded, upper, KD=2
  n = 4; kd = 2
  ab = (0.0d0, 0.0d0)
  ab(3,1) = (10.0d0, 0.0d0)
  ab(2,2) = (2.0d0, 1.0d0); ab(3,2) = (10.0d0, 0.0d0)
  ab(1,3) = (1.0d0, 0.0d0); ab(2,3) = (3.0d0, 1.0d0); ab(3,3) = (10.0d0, 0.0d0)
  ab(1,4) = (1.0d0, 0.0d0); ab(2,4) = (2.0d0, 1.0d0); ab(3,4) = (10.0d0, 0.0d0)
  anorm = 18.0d0

  call ZPBTRF('U', n, kd, ab, 3, info)
  call ZPBCON('U', n, kd, ab, 3, anorm, rcond, work, rwork, info)
  call begin_test('upper_kd2')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 4: N=0 (rcond = 1)
  n = 0; kd = 0; anorm = 0.0d0
  call ZPBCON('U', n, kd, ab, 3, anorm, rcond, work, rwork, info)
  call begin_test('n_zero')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 5: N=1
  n = 1; kd = 0
  ab = (0.0d0, 0.0d0)
  ab(1,1) = (4.0d0, 0.0d0)
  anorm = 4.0d0
  call ZPBTRF('U', n, kd, ab, 3, info)
  call ZPBCON('U', n, kd, ab, 3, anorm, rcond, work, rwork, info)
  call begin_test('n_one')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 6: anorm = 0 (rcond = 0)
  n = 4; kd = 1
  ab = (0.0d0, 0.0d0)
  ab(2,1) = (4.0d0, 0.0d0)
  ab(1,2) = (1.0d0, 0.0d0); ab(2,2) = (5.0d0, 0.0d0)
  ab(1,3) = (1.0d0, 0.0d0); ab(2,3) = (6.0d0, 0.0d0)
  ab(1,4) = (2.0d0, 0.0d0); ab(2,4) = (7.0d0, 0.0d0)
  call ZPBTRF('U', n, kd, ab, 3, info)
  anorm = 0.0d0
  call ZPBCON('U', n, kd, ab, 3, anorm, rcond, work, rwork, info)
  call begin_test('anorm_zero')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

end program
