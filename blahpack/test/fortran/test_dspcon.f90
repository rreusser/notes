program test_dspcon
  use test_utils
  implicit none
  double precision :: ap(100), work(100), anorm, rcond
  integer :: ipiv(10), iwork(10), info, n
  double precision :: dlansp

  ! Test 1: 3x3 well-conditioned symmetric matrix (upper, packed)
  ! A = [[4, 1, 1], [1, 3, 1], [1, 1, 2]]
  ! Upper packed (column-major): A(1,1), A(1,2), A(2,2), A(1,3), A(2,3), A(3,3)
  n = 3
  ap = 0.0d0
  ap(1) = 4.0d0  ! A(1,1)
  ap(2) = 1.0d0  ! A(1,2)
  ap(3) = 3.0d0  ! A(2,2)
  ap(4) = 1.0d0  ! A(1,3)
  ap(5) = 1.0d0  ! A(2,3)
  ap(6) = 2.0d0  ! A(3,3)
  anorm = dlansp('1', 'U', n, ap, work)
  call dsptrf('U', n, ap, ipiv, info)
  call dspcon('U', n, ap, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('upper_well_cond')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call print_array('ap', ap, 6)
  call print_int_array('ipiv', ipiv, n)
  call end_test()

  ! Test 2: Same matrix, lower packed
  ! Lower packed: A(1,1), A(2,1), A(3,1), A(2,2), A(3,2), A(3,3)
  n = 3
  ap = 0.0d0
  ap(1) = 4.0d0  ! A(1,1)
  ap(2) = 1.0d0  ! A(2,1)
  ap(3) = 1.0d0  ! A(3,1)
  ap(4) = 3.0d0  ! A(2,2)
  ap(5) = 1.0d0  ! A(3,2)
  ap(6) = 2.0d0  ! A(3,3)
  anorm = dlansp('1', 'L', n, ap, work)
  call dsptrf('L', n, ap, ipiv, info)
  call dspcon('L', n, ap, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('lower_well_cond')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call print_array('ap', ap, 6)
  call print_int_array('ipiv', ipiv, n)
  call end_test()

  ! Test 3: 3x3 identity (upper, packed), rcond should be 1
  n = 3
  ap = 0.0d0
  ap(1) = 1.0d0  ! A(1,1)
  ap(2) = 0.0d0  ! A(1,2)
  ap(3) = 1.0d0  ! A(2,2)
  ap(4) = 0.0d0  ! A(1,3)
  ap(5) = 0.0d0  ! A(2,3)
  ap(6) = 1.0d0  ! A(3,3)
  anorm = dlansp('1', 'U', n, ap, work)
  call dsptrf('U', n, ap, ipiv, info)
  call dspcon('U', n, ap, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('identity_upper')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 4: 3x3 identity (lower, packed)
  n = 3
  ap = 0.0d0
  ap(1) = 1.0d0  ! A(1,1)
  ap(2) = 0.0d0  ! A(2,1)
  ap(3) = 0.0d0  ! A(3,1)
  ap(4) = 1.0d0  ! A(2,2)
  ap(5) = 0.0d0  ! A(3,2)
  ap(6) = 1.0d0  ! A(3,3)
  anorm = dlansp('1', 'L', n, ap, work)
  call dsptrf('L', n, ap, ipiv, info)
  call dspcon('L', n, ap, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('identity_lower')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 5: ill-conditioned diagonal matrix (upper, packed)
  ! A = diag(1, 1, 1e-15)
  n = 3
  ap = 0.0d0
  ap(1) = 1.0d0     ! A(1,1)
  ap(2) = 0.0d0     ! A(1,2)
  ap(3) = 1.0d0     ! A(2,2)
  ap(4) = 0.0d0     ! A(1,3)
  ap(5) = 0.0d0     ! A(2,3)
  ap(6) = 1.0d-15   ! A(3,3)
  anorm = dlansp('1', 'U', n, ap, work)
  call dsptrf('U', n, ap, ipiv, info)
  call dspcon('U', n, ap, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('ill_cond_upper')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 6: singular matrix (upper, packed) - rank 1
  ! A = [[1, 2, 3], [2, 4, 6], [3, 6, 9]]
  ! Upper packed: A(1,1)=1, A(1,2)=2, A(2,2)=4, A(1,3)=3, A(2,3)=6, A(3,3)=9
  n = 3
  ap = 0.0d0
  ap(1) = 1.0d0
  ap(2) = 2.0d0
  ap(3) = 4.0d0
  ap(4) = 3.0d0
  ap(5) = 6.0d0
  ap(6) = 9.0d0
  anorm = dlansp('1', 'U', n, ap, work)
  call dsptrf('U', n, ap, ipiv, info)
  call dspcon('U', n, ap, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('singular_upper')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 7: N=0
  call dspcon('U', 0, ap, ipiv, 0.0d0, rcond, work, iwork, info)
  call begin_test('n_zero')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 8: N=1 (upper)
  n = 1
  ap(1) = 5.0d0
  anorm = 5.0d0
  call dsptrf('U', n, ap, ipiv, info)
  call dspcon('U', n, ap, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('n_one_upper')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 9: 4x4 well-conditioned (upper, packed)
  ! A = [[10, 1, 2, 0], [1, 8, 1, 1], [2, 1, 6, 1], [0, 1, 1, 5]]
  ! Upper packed: A(1,1), A(1,2), A(2,2), A(1,3), A(2,3), A(3,3), A(1,4), A(2,4), A(3,4), A(4,4)
  n = 4
  ap = 0.0d0
  ap(1) = 10.0d0   ! A(1,1)
  ap(2) = 1.0d0    ! A(1,2)
  ap(3) = 8.0d0    ! A(2,2)
  ap(4) = 2.0d0    ! A(1,3)
  ap(5) = 1.0d0    ! A(2,3)
  ap(6) = 6.0d0    ! A(3,3)
  ap(7) = 0.0d0    ! A(1,4)
  ap(8) = 1.0d0    ! A(2,4)
  ap(9) = 1.0d0    ! A(3,4)
  ap(10) = 5.0d0   ! A(4,4)
  anorm = dlansp('1', 'U', n, ap, work)
  call dsptrf('U', n, ap, ipiv, info)
  call dspcon('U', n, ap, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('4x4_upper')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call print_array('ap', ap, 10)
  call print_int_array('ipiv', ipiv, n)
  call end_test()

  ! Test 10: 4x4 well-conditioned (lower, packed)
  ! Same matrix, lower packed: A(1,1), A(2,1), A(3,1), A(4,1), A(2,2), A(3,2), A(4,2), A(3,3), A(4,3), A(4,4)
  n = 4
  ap = 0.0d0
  ap(1) = 10.0d0   ! A(1,1)
  ap(2) = 1.0d0    ! A(2,1)
  ap(3) = 2.0d0    ! A(3,1)
  ap(4) = 0.0d0    ! A(4,1)
  ap(5) = 8.0d0    ! A(2,2)
  ap(6) = 1.0d0    ! A(3,2)
  ap(7) = 1.0d0    ! A(4,2)
  ap(8) = 6.0d0    ! A(3,3)
  ap(9) = 1.0d0    ! A(4,3)
  ap(10) = 5.0d0   ! A(4,4)
  anorm = dlansp('1', 'L', n, ap, work)
  call dsptrf('L', n, ap, ipiv, info)
  call dspcon('L', n, ap, ipiv, anorm, rcond, work, iwork, info)
  call begin_test('4x4_lower')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call print_array('ap', ap, 10)
  call print_int_array('ipiv', ipiv, n)
  call end_test()

end program
