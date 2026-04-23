program test_dtpcon
  use test_utils
  implicit none
  ! N*(N+1)/2 for N=4 is 10; N=3 is 6; N=1 is 1
  double precision :: ap(10)
  double precision :: work(12)
  integer :: iwork(4)
  double precision :: rcond
  integer :: info, n

  ! Well-conditioned upper triangular 4x4 matrix (diagonally dominant):
  !   [ 10  -1   2  -1 ]
  !   [  0   8  -2   1 ]
  !   [  0   0  12  -3 ]
  !   [  0   0   0   6 ]
  ! Packed upper (column-major): col1=[10], col2=[-1,8], col3=[2,-2,12], col4=[-1,1,-3,6]
  n = 4
  ap(1) = 10.0d0
  ap(2) = -1.0d0
  ap(3) = 8.0d0
  ap(4) = 2.0d0
  ap(5) = -2.0d0
  ap(6) = 12.0d0
  ap(7) = -1.0d0
  ap(8) = 1.0d0
  ap(9) = -3.0d0
  ap(10) = 6.0d0

  ! Test 1: upper, non-unit, 1-norm
  call dtpcon('1', 'U', 'N', n, ap, rcond, work, iwork, info)
  call begin_test('upper_nonunit_onenorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 2: upper, non-unit, inf-norm
  call dtpcon('I', 'U', 'N', n, ap, rcond, work, iwork, info)
  call begin_test('upper_nonunit_infnorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 3: upper, unit diagonal, 1-norm
  call dtpcon('1', 'U', 'U', n, ap, rcond, work, iwork, info)
  call begin_test('upper_unit_onenorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 4: upper, unit diagonal, inf-norm
  call dtpcon('I', 'U', 'U', n, ap, rcond, work, iwork, info)
  call begin_test('upper_unit_infnorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Well-conditioned lower triangular 4x4 matrix:
  !   [  5   0   0   0 ]
  !   [ -2   7   0   0 ]
  !   [  1  -1   9   0 ]
  !   [ -1   2  -3  11 ]
  ! Packed lower (column-major): col1=[5,-2,1,-1], col2=[7,-1,2], col3=[9,-3], col4=[11]
  ap(1) = 5.0d0
  ap(2) = -2.0d0
  ap(3) = 1.0d0
  ap(4) = -1.0d0
  ap(5) = 7.0d0
  ap(6) = -1.0d0
  ap(7) = 2.0d0
  ap(8) = 9.0d0
  ap(9) = -3.0d0
  ap(10) = 11.0d0

  ! Test 5: lower, non-unit, 1-norm
  call dtpcon('1', 'L', 'N', n, ap, rcond, work, iwork, info)
  call begin_test('lower_nonunit_onenorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 6: lower, non-unit, inf-norm
  call dtpcon('I', 'L', 'N', n, ap, rcond, work, iwork, info)
  call begin_test('lower_nonunit_infnorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 7: lower, unit diagonal, 1-norm
  call dtpcon('1', 'L', 'U', n, ap, rcond, work, iwork, info)
  call begin_test('lower_unit_onenorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 8: lower, unit diagonal, inf-norm
  call dtpcon('I', 'L', 'U', n, ap, rcond, work, iwork, info)
  call begin_test('lower_unit_infnorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 9: edge case N=0
  call dtpcon('1', 'U', 'N', 0, ap, rcond, work, iwork, info)
  call begin_test('edge_n0')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 10: edge case N=1
  ap(1) = 3.0d0
  call dtpcon('1', 'U', 'N', 1, ap, rcond, work, iwork, info)
  call begin_test('edge_n1')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 11: ill-conditioned matrix (near-singular)
  !   [ 1e12   0     0   ]
  !   [  0     1     0   ]
  !   [  0     0    1e-12 ]
  ! Packed upper: [1e12, 0, 1, 0, 0, 1e-12]
  n = 3
  ap(1) = 1.0d12
  ap(2) = 0.0d0
  ap(3) = 1.0d0
  ap(4) = 0.0d0
  ap(5) = 0.0d0
  ap(6) = 1.0d-12

  call dtpcon('1', 'U', 'N', n, ap, rcond, work, iwork, info)
  call begin_test('ill_conditioned_onenorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  call dtpcon('I', 'U', 'N', n, ap, rcond, work, iwork, info)
  call begin_test('ill_conditioned_infnorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! Test 13: identity matrix (perfect condition)
  n = 3
  ap(1) = 1.0d0
  ap(2) = 0.0d0
  ap(3) = 1.0d0
  ap(4) = 0.0d0
  ap(5) = 0.0d0
  ap(6) = 1.0d0

  call dtpcon('1', 'U', 'N', n, ap, rcond, work, iwork, info)
  call begin_test('identity_onenorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  call dtpcon('I', 'U', 'N', n, ap, rcond, work, iwork, info)
  call begin_test('identity_infnorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

end program
