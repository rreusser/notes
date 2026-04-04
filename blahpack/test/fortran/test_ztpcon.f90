program test_ztpcon
  use test_utils
  implicit none

  integer, parameter :: MAXN = 10
  integer, parameter :: MAXAP = MAXN*(MAXN+1)/2
  complex*16 :: ap(MAXAP), work(2*MAXN)
  double precision :: ap_r(2*MAXAP), work_r(4*MAXN)
  equivalence (ap, ap_r)
  equivalence (work, work_r)
  double precision :: rwork(MAXN), rcond
  integer :: info, n

  ! Test 1: 3x3 upper triangular, non-unit, 1-norm (packed)
  ! A = [[4+i, 1+i, 0.5], [0, 3, 1-i], [0, 0, 2+i]]
  ! Packed upper (column-major): ap(1)=A(1,1), ap(2)=A(1,2), ap(3)=A(2,2),
  !   ap(4)=A(1,3), ap(5)=A(2,3), ap(6)=A(3,3)
  n = 3
  ap = (0.0d0, 0.0d0)
  ap(1) = (4.0d0, 1.0d0)
  ap(2) = (1.0d0, 1.0d0); ap(3) = (3.0d0, 0.0d0)
  ap(4) = (0.5d0, 0.0d0); ap(5) = (1.0d0, -1.0d0); ap(6) = (2.0d0, 1.0d0)
  call ztpcon('1', 'U', 'N', n, ap, rcond, work, rwork, info)
  call begin_test('upper_nonunit_1norm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call print_array('AP', ap_r, 2*6)
  call print_array('WORK', work_r, 2*2*n)
  call print_array('RWORK', rwork, n)
  call end_test()

  ! Test 2: same matrix, inf-norm
  ap = (0.0d0, 0.0d0)
  ap(1) = (4.0d0, 1.0d0)
  ap(2) = (1.0d0, 1.0d0); ap(3) = (3.0d0, 0.0d0)
  ap(4) = (0.5d0, 0.0d0); ap(5) = (1.0d0, -1.0d0); ap(6) = (2.0d0, 1.0d0)
  call ztpcon('I', 'U', 'N', n, ap, rcond, work, rwork, info)
  call begin_test('upper_nonunit_Inorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 3: 3x3 lower triangular, non-unit, 1-norm (packed)
  ! A = [[3+i, 0, 0], [1, 4-i, 0], [0.5+i, 1-i, 2]]
  ! Packed lower (column-major): ap(1)=A(1,1), ap(2)=A(2,1), ap(3)=A(3,1),
  !   ap(4)=A(2,2), ap(5)=A(3,2), ap(6)=A(3,3)
  n = 3
  ap = (0.0d0, 0.0d0)
  ap(1) = (3.0d0, 1.0d0); ap(2) = (1.0d0, 0.0d0); ap(3) = (0.5d0, 1.0d0)
  ap(4) = (4.0d0, -1.0d0); ap(5) = (1.0d0, -1.0d0)
  ap(6) = (2.0d0, 0.0d0)
  call ztpcon('1', 'L', 'N', n, ap, rcond, work, rwork, info)
  call begin_test('lower_nonunit_1norm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call print_array('AP', ap_r, 2*6)
  call print_array('WORK', work_r, 2*2*n)
  call print_array('RWORK', rwork, n)
  call end_test()

  ! Test 4: 3x3 upper triangular, unit diagonal, 1-norm
  ! A = [[1, 1+i, 0.5], [0, 1, 1-i], [0, 0, 1]] (unit diag)
  n = 3
  ap = (0.0d0, 0.0d0)
  ap(1) = (1.0d0, 0.0d0)
  ap(2) = (1.0d0, 1.0d0); ap(3) = (1.0d0, 0.0d0)
  ap(4) = (0.5d0, 0.0d0); ap(5) = (1.0d0, -1.0d0); ap(6) = (1.0d0, 0.0d0)
  call ztpcon('1', 'U', 'U', n, ap, rcond, work, rwork, info)
  call begin_test('upper_unit_1norm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 5: 3x3 identity packed upper (rcond=1)
  n = 3
  ap = (0.0d0, 0.0d0)
  ap(1) = (1.0d0, 0.0d0); ap(3) = (1.0d0, 0.0d0); ap(6) = (1.0d0, 0.0d0)
  call ztpcon('1', 'U', 'N', n, ap, rcond, work, rwork, info)
  call begin_test('identity')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 6: N=0 (rcond=1)
  call ztpcon('1', 'U', 'N', 0, ap, rcond, work, rwork, info)
  call begin_test('n_zero')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 7: 4x4 lower triangular, non-unit, inf-norm (packed)
  ! A = [[5+i, 0, 0, 0], [1, 4-i, 0, 0], [0, 1+i, 3, 0], [0, 0, 1, 2+i]]
  ! Packed lower: ap(1)=A(1,1), ap(2)=A(2,1), ap(3)=A(3,1), ap(4)=A(4,1),
  !   ap(5)=A(2,2), ap(6)=A(3,2), ap(7)=A(4,2),
  !   ap(8)=A(3,3), ap(9)=A(4,3), ap(10)=A(4,4)
  n = 4
  ap = (0.0d0, 0.0d0)
  ap(1) = (5.0d0, 1.0d0)
  ap(2) = (1.0d0, 0.0d0); ap(3) = (0.0d0, 0.0d0); ap(4) = (0.0d0, 0.0d0)
  ap(5) = (4.0d0, -1.0d0); ap(6) = (1.0d0, 1.0d0); ap(7) = (0.0d0, 0.0d0)
  ap(8) = (3.0d0, 0.0d0); ap(9) = (1.0d0, 0.0d0)
  ap(10) = (2.0d0, 1.0d0)
  call ztpcon('I', 'L', 'N', n, ap, rcond, work, rwork, info)
  call begin_test('4x4_lower_Inorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call print_array('AP', ap_r, 2*10)
  call print_array('WORK', work_r, 2*2*n)
  call print_array('RWORK', rwork, n)
  call end_test()

  ! Test 8: lower, unit diagonal, inf-norm
  n = 3
  ap = (0.0d0, 0.0d0)
  ap(1) = (1.0d0, 0.0d0)
  ap(2) = (0.5d0, 0.5d0); ap(3) = (0.0d0, 0.0d0)
  ap(4) = (1.0d0, 0.0d0); ap(5) = (0.5d0, -0.5d0)
  ap(6) = (1.0d0, 0.0d0)
  call ztpcon('I', 'L', 'U', n, ap, rcond, work, rwork, info)
  call begin_test('lower_unit_Inorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

end program
