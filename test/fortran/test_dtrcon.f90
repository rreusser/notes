program test_dtrcon
  use test_utils
  implicit none
  double precision :: a(6, 6)
  double precision :: work(18)
  integer :: iwork(6)
  double precision :: rcond
  integer :: info, n, lda

  lda = 6

  ! Well-conditioned upper triangular 4x4 matrix (diagonally dominant):
  !   [ 10  -1   2  -1 ]
  !   [  0   8  -2   1 ]
  !   [  0   0  12  -3 ]
  !   [  0   0   0   6 ]
  n = 4
  a = 0.0d0
  a(1,1) = 10.0d0
  a(1,2) = -1.0d0
  a(2,2) =  8.0d0
  a(1,3) =  2.0d0
  a(2,3) = -2.0d0
  a(3,3) = 12.0d0
  a(1,4) = -1.0d0
  a(2,4) =  1.0d0
  a(3,4) = -3.0d0
  a(4,4) =  6.0d0

  ! Test 1: upper, non-unit, 1-norm
  call dtrcon('1', 'U', 'N', n, a, lda, rcond, work, iwork, info)
  call begin_test('upper_nonunit_onenorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 2: upper, non-unit, inf-norm
  call dtrcon('I', 'U', 'N', n, a, lda, rcond, work, iwork, info)
  call begin_test('upper_nonunit_infnorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 3: upper, unit diagonal, 1-norm
  call dtrcon('1', 'U', 'U', n, a, lda, rcond, work, iwork, info)
  call begin_test('upper_unit_onenorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 4: upper, unit diagonal, inf-norm
  call dtrcon('I', 'U', 'U', n, a, lda, rcond, work, iwork, info)
  call begin_test('upper_unit_infnorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Well-conditioned lower triangular 4x4 matrix:
  !   [  5   0   0   0 ]
  !   [ -2   7   0   0 ]
  !   [  1  -1   9   0 ]
  !   [ -1   2  -3  11 ]
  a = 0.0d0
  a(1,1) =  5.0d0
  a(2,1) = -2.0d0
  a(3,1) =  1.0d0
  a(4,1) = -1.0d0
  a(2,2) =  7.0d0
  a(3,2) = -1.0d0
  a(4,2) =  2.0d0
  a(3,3) =  9.0d0
  a(4,3) = -3.0d0
  a(4,4) = 11.0d0

  ! Test 5: lower, non-unit, 1-norm
  call dtrcon('1', 'L', 'N', n, a, lda, rcond, work, iwork, info)
  call begin_test('lower_nonunit_onenorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 6: lower, non-unit, inf-norm
  call dtrcon('I', 'L', 'N', n, a, lda, rcond, work, iwork, info)
  call begin_test('lower_nonunit_infnorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 7: lower, unit diagonal, 1-norm
  call dtrcon('1', 'L', 'U', n, a, lda, rcond, work, iwork, info)
  call begin_test('lower_unit_onenorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 8: lower, unit diagonal, inf-norm
  call dtrcon('I', 'L', 'U', n, a, lda, rcond, work, iwork, info)
  call begin_test('lower_unit_infnorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 9: edge case N=0
  call dtrcon('1', 'U', 'N', 0, a, lda, rcond, work, iwork, info)
  call begin_test('edge_n0')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 10: edge case N=1
  a = 0.0d0
  a(1,1) = 3.0d0
  call dtrcon('1', 'U', 'N', 1, a, lda, rcond, work, iwork, info)
  call begin_test('edge_n1')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 11: ill-conditioned matrix (near-singular)
  !   [ 1e12   0     0   ]
  !   [  0     1     0   ]
  !   [  0     0    1e-12 ]
  n = 3
  a = 0.0d0
  a(1,1) = 1.0d12
  a(2,2) = 1.0d0
  a(3,3) = 1.0d-12

  call dtrcon('1', 'U', 'N', n, a, lda, rcond, work, iwork, info)
  call begin_test('ill_conditioned_onenorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  call dtrcon('I', 'U', 'N', n, a, lda, rcond, work, iwork, info)
  call begin_test('ill_conditioned_infnorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 13: identity matrix (perfect condition)
  n = 3
  a = 0.0d0
  a(1,1) = 1.0d0
  a(2,2) = 1.0d0
  a(3,3) = 1.0d0

  call dtrcon('1', 'U', 'N', n, a, lda, rcond, work, iwork, info)
  call begin_test('identity_onenorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  call dtrcon('I', 'U', 'N', n, a, lda, rcond, work, iwork, info)
  call begin_test('identity_infnorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

end program
