program test_zlansy
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4
  complex*16 :: A(NMAX, NMAX)
  double precision :: WORK(NMAX), val
  integer :: n
  double precision :: ZLANSY
  external ZLANSY

  n = 4
  A = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 1.0d0)
  A(1,2) = (1.0d0, 2.0d0)
  A(1,3) = (3.0d0, -1.0d0)
  A(1,4) = (0.5d0, 0.5d0)
  A(2,2) = (5.0d0, -1.0d0)
  A(2,3) = (2.0d0, 1.0d0)
  A(2,4) = (1.0d0, -2.0d0)
  A(3,3) = (4.0d0, 2.0d0)
  A(3,4) = (3.0d0, 0.0d0)
  A(4,4) = (6.0d0, -3.0d0)

  ! Test 1: Max norm, upper
  val = ZLANSY('M', 'U', n, A, NMAX, WORK)
  call begin_test('max_upper')
  call print_scalar('val', val)
  call end_test()

  ! Test 2: One norm, upper
  WORK = 0.0d0
  val = ZLANSY('1', 'U', n, A, NMAX, WORK)
  call begin_test('one_upper')
  call print_scalar('val', val)
  call end_test()

  ! Test 3: Inf norm, upper
  WORK = 0.0d0
  val = ZLANSY('I', 'U', n, A, NMAX, WORK)
  call begin_test('inf_upper')
  call print_scalar('val', val)
  call end_test()

  ! Test 4: Frobenius norm, upper
  val = ZLANSY('F', 'U', n, A, NMAX, WORK)
  call begin_test('fro_upper')
  call print_scalar('val', val)
  call end_test()

  ! Test 5: Lower triangle version
  A = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 1.0d0)
  A(2,1) = (1.0d0, 2.0d0)
  A(2,2) = (5.0d0, -1.0d0)
  A(3,1) = (3.0d0, -1.0d0)
  A(3,2) = (2.0d0, 1.0d0)
  A(3,3) = (4.0d0, 2.0d0)
  A(4,1) = (0.5d0, 0.5d0)
  A(4,2) = (1.0d0, -2.0d0)
  A(4,3) = (3.0d0, 0.0d0)
  A(4,4) = (6.0d0, -3.0d0)

  WORK = 0.0d0
  val = ZLANSY('1', 'L', n, A, NMAX, WORK)
  call begin_test('one_lower')
  call print_scalar('val', val)
  call end_test()

  val = ZLANSY('F', 'L', n, A, NMAX, WORK)
  call begin_test('fro_lower')
  call print_scalar('val', val)
  call end_test()

  ! Test 6: N=0
  val = ZLANSY('M', 'U', 0, A, NMAX, WORK)
  call begin_test('n0')
  call print_scalar('val', val)
  call end_test()

end program
