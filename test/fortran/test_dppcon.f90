program test_dppcon
  use test_utils
  implicit none
  double precision :: ap(100), work(100), anorm, rcond
  integer :: iwork(10), info, n
  double precision :: dlansp

  ! Test 1: 3x3 identity, upper packed
  ! Upper packed: A(1,1)=1, A(1,2)=0, A(2,2)=1, A(1,3)=0, A(2,3)=0, A(3,3)=1
  n = 3
  ap = 0.0d0
  ap(1) = 1.0d0
  ap(3) = 1.0d0
  ap(6) = 1.0d0
  anorm = dlansp('1', 'U', n, ap, work)
  call dpptrf('U', n, ap, info)
  call dppcon('U', n, ap, anorm, rcond, work, iwork, info)
  call begin_test('identity_upper')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 2: 3x3 identity, lower packed
  ! Lower packed: A(1,1)=1, A(2,1)=0, A(3,1)=0, A(2,2)=1, A(3,2)=0, A(3,3)=1
  n = 3
  ap = 0.0d0
  ap(1) = 1.0d0
  ap(4) = 1.0d0
  ap(6) = 1.0d0
  anorm = dlansp('1', 'L', n, ap, work)
  call dpptrf('L', n, ap, info)
  call dppcon('L', n, ap, anorm, rcond, work, iwork, info)
  call begin_test('identity_lower')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 3: 3x3 well-conditioned SPD, upper packed
  ! A = [[4, 1, 1], [1, 3, 1], [1, 1, 2]]
  ! Upper packed: A(1,1)=4, A(1,2)=1, A(2,2)=3, A(1,3)=1, A(2,3)=1, A(3,3)=2
  n = 3
  ap = 0.0d0
  ap(1) = 4.0d0
  ap(2) = 1.0d0
  ap(3) = 3.0d0
  ap(4) = 1.0d0
  ap(5) = 1.0d0
  ap(6) = 2.0d0
  anorm = dlansp('1', 'U', n, ap, work)
  call dpptrf('U', n, ap, info)
  call dppcon('U', n, ap, anorm, rcond, work, iwork, info)
  call begin_test('well_cond_upper')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 4: Same matrix, lower packed
  ! Lower packed: A(1,1)=4, A(2,1)=1, A(3,1)=1, A(2,2)=3, A(3,2)=1, A(3,3)=2
  n = 3
  ap = 0.0d0
  ap(1) = 4.0d0
  ap(2) = 1.0d0
  ap(3) = 1.0d0
  ap(4) = 3.0d0
  ap(5) = 1.0d0
  ap(6) = 2.0d0
  anorm = dlansp('1', 'L', n, ap, work)
  call dpptrf('L', n, ap, info)
  call dppcon('L', n, ap, anorm, rcond, work, iwork, info)
  call begin_test('well_cond_lower')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 5: N=0
  call dppcon('U', 0, ap, 0.0d0, rcond, work, iwork, info)
  call begin_test('n_zero')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 6: anorm=0
  n = 3
  ap = 0.0d0
  ap(1) = 1.0d0
  ap(3) = 1.0d0
  ap(6) = 1.0d0
  call dpptrf('U', n, ap, info)
  call dppcon('U', n, ap, 0.0d0, rcond, work, iwork, info)
  call begin_test('anorm_zero')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 7: N=1
  n = 1
  ap(1) = 5.0d0
  anorm = 5.0d0
  call dpptrf('U', n, ap, info)
  call dppcon('U', n, ap, anorm, rcond, work, iwork, info)
  call begin_test('n_one')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 8: 4x4 well-conditioned SPD, upper packed
  ! A = [[10, 1, 2, 0], [1, 8, 1, 1], [2, 1, 6, 1], [0, 1, 1, 5]]
  ! Upper packed: 10, 1, 8, 2, 1, 6, 0, 1, 1, 5
  n = 4
  ap = 0.0d0
  ap(1) = 10.0d0
  ap(2) = 1.0d0
  ap(3) = 8.0d0
  ap(4) = 2.0d0
  ap(5) = 1.0d0
  ap(6) = 6.0d0
  ap(7) = 0.0d0
  ap(8) = 1.0d0
  ap(9) = 1.0d0
  ap(10) = 5.0d0
  anorm = dlansp('1', 'U', n, ap, work)
  call dpptrf('U', n, ap, info)
  call dppcon('U', n, ap, anorm, rcond, work, iwork, info)
  call begin_test('4x4_upper')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 9: 4x4 well-conditioned SPD, lower packed
  ! Same matrix, lower packed: 10, 1, 2, 0, 8, 1, 1, 6, 1, 5
  n = 4
  ap = 0.0d0
  ap(1) = 10.0d0
  ap(2) = 1.0d0
  ap(3) = 2.0d0
  ap(4) = 0.0d0
  ap(5) = 8.0d0
  ap(6) = 1.0d0
  ap(7) = 1.0d0
  ap(8) = 6.0d0
  ap(9) = 1.0d0
  ap(10) = 5.0d0
  anorm = dlansp('1', 'L', n, ap, work)
  call dpptrf('L', n, ap, info)
  call dppcon('L', n, ap, anorm, rcond, work, iwork, info)
  call begin_test('4x4_lower')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 10: ill-conditioned diagonal, upper packed
  ! A = diag(1, 1, 1e-15)
  n = 3
  ap = 0.0d0
  ap(1) = 1.0d0
  ap(3) = 1.0d0
  ap(6) = 1.0d-15
  anorm = dlansp('1', 'U', n, ap, work)
  call dpptrf('U', n, ap, info)
  call dppcon('U', n, ap, anorm, rcond, work, iwork, info)
  call begin_test('ill_cond')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

end program
