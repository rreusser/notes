program test_zppcon
  use test_utils
  implicit none

  integer, parameter :: MAXN = 10
  integer, parameter :: MAXAP = MAXN*(MAXN+1)/2
  complex*16 :: ap(MAXAP), work(2*MAXN)
  double precision :: ap_r(2*MAXAP), work_r(4*MAXN)
  equivalence (ap, ap_r)
  equivalence (work, work_r)
  double precision :: rwork(MAXN), anorm, rcond
  integer :: info, n
  double precision :: zlanhp

  ! Test 1: 3x3 HPD matrix, upper packed
  ! A = [[4, 1+i, 0], [1-i, 3, 1], [0, 1, 2]]
  ! Upper packed (column-major): A(1,1), A(1,2), A(2,2), A(1,3), A(2,3), A(3,3)
  n = 3
  ap(1) = (4.0d0, 0.0d0)
  ap(2) = (1.0d0, 1.0d0)
  ap(3) = (3.0d0, 0.0d0)
  ap(4) = (0.0d0, 0.0d0)
  ap(5) = (1.0d0, 0.0d0)
  ap(6) = (2.0d0, 0.0d0)
  anorm = zlanhp('1', 'U', n, ap, rwork)
  call zpptrf('U', n, ap, info)
  call zppcon('U', n, ap, anorm, rcond, work, rwork, info)
  call begin_test('upper_3x3')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call print_array('ap_r', ap_r, 2*n*(n+1)/2)
  call print_array('work_r', work_r, 4*n)
  call end_test()

  ! Test 2: same matrix, lower packed
  ! Lower packed (column-major): A(1,1), A(2,1), A(3,1), A(2,2), A(3,2), A(3,3)
  n = 3
  ap(1) = (4.0d0, 0.0d0)
  ap(2) = (1.0d0, -1.0d0)
  ap(3) = (0.0d0, 0.0d0)
  ap(4) = (3.0d0, 0.0d0)
  ap(5) = (1.0d0, 0.0d0)
  ap(6) = (2.0d0, 0.0d0)
  anorm = zlanhp('1', 'L', n, ap, rwork)
  call zpptrf('L', n, ap, info)
  call zppcon('L', n, ap, anorm, rcond, work, rwork, info)
  call begin_test('lower_3x3')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call print_array('ap_r', ap_r, 2*n*(n+1)/2)
  call print_array('work_r', work_r, 4*n)
  call end_test()

  ! Test 3: 3x3 identity (rcond=1), upper
  n = 3
  ap(1) = (1.0d0, 0.0d0)
  ap(2) = (0.0d0, 0.0d0)
  ap(3) = (1.0d0, 0.0d0)
  ap(4) = (0.0d0, 0.0d0)
  ap(5) = (0.0d0, 0.0d0)
  ap(6) = (1.0d0, 0.0d0)
  anorm = zlanhp('1', 'U', n, ap, rwork)
  call zpptrf('U', n, ap, info)
  call zppcon('U', n, ap, anorm, rcond, work, rwork, info)
  call begin_test('identity')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 4: N=0 (rcond=1)
  call zppcon('U', 0, ap, 0.0d0, rcond, work, rwork, info)
  call begin_test('n_zero')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 5: anorm=0 (rcond=0)
  n = 3
  ap(1) = (1.0d0, 0.0d0)
  ap(2) = (0.0d0, 0.0d0)
  ap(3) = (1.0d0, 0.0d0)
  ap(4) = (0.0d0, 0.0d0)
  ap(5) = (0.0d0, 0.0d0)
  ap(6) = (1.0d0, 0.0d0)
  call zpptrf('U', n, ap, info)
  call zppcon('U', n, ap, 0.0d0, rcond, work, rwork, info)
  call begin_test('anorm_zero')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 6: 4x4 HPD matrix, upper packed
  ! A = [[5, 1+i, 0, 0], [1-i, 4, 1+i, 0], [0, 1-i, 3, 1], [0, 0, 1, 2]]
  ! Upper packed: A(1,1), A(1,2), A(2,2), A(1,3), A(2,3), A(3,3), A(1,4), A(2,4), A(3,4), A(4,4)
  n = 4
  ap(1) = (5.0d0, 0.0d0)
  ap(2) = (1.0d0, 1.0d0)
  ap(3) = (4.0d0, 0.0d0)
  ap(4) = (0.0d0, 0.0d0)
  ap(5) = (1.0d0, 1.0d0)
  ap(6) = (3.0d0, 0.0d0)
  ap(7) = (0.0d0, 0.0d0)
  ap(8) = (0.0d0, 0.0d0)
  ap(9) = (1.0d0, 0.0d0)
  ap(10) = (2.0d0, 0.0d0)
  anorm = zlanhp('1', 'U', n, ap, rwork)
  call zpptrf('U', n, ap, info)
  call zppcon('U', n, ap, anorm, rcond, work, rwork, info)
  call begin_test('4x4_upper')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call print_array('ap_r', ap_r, 2*n*(n+1)/2)
  call print_array('work_r', work_r, 4*n)
  call end_test()

  ! Test 7: 4x4 HPD matrix, lower packed
  ! Lower packed: A(1,1), A(2,1), A(3,1), A(4,1), A(2,2), A(3,2), A(4,2), A(3,3), A(4,3), A(4,4)
  n = 4
  ap(1)  = (5.0d0, 0.0d0)
  ap(2)  = (1.0d0,-1.0d0)
  ap(3)  = (0.0d0, 0.0d0)
  ap(4)  = (0.0d0, 0.0d0)
  ap(5)  = (4.0d0, 0.0d0)
  ap(6)  = (1.0d0,-1.0d0)
  ap(7)  = (0.0d0, 0.0d0)
  ap(8)  = (3.0d0, 0.0d0)
  ap(9)  = (1.0d0, 0.0d0)
  ap(10) = (2.0d0, 0.0d0)
  anorm = zlanhp('1', 'L', n, ap, rwork)
  call zpptrf('L', n, ap, info)
  call zppcon('L', n, ap, anorm, rcond, work, rwork, info)
  call begin_test('4x4_lower')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call print_array('ap_r', ap_r, 2*n*(n+1)/2)
  call print_array('work_r', work_r, 4*n)
  call end_test()

  ! Test 8: N=1 (edge case)
  n = 1
  ap(1) = (3.0d0, 0.0d0)
  anorm = zlanhp('1', 'U', n, ap, rwork)
  call zpptrf('U', n, ap, info)
  call zppcon('U', n, ap, anorm, rcond, work, rwork, info)
  call begin_test('n_one')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

end program
