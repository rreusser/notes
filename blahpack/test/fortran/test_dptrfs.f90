program test_dptrfs
  use test_utils
  implicit none
  double precision :: d(10), e(10), df(10), ef(10)
  double precision :: b(50), x(50), ferr(10), berr(10), work(20)
  integer :: info

  ! ---------------------------------------------------------------
  ! Test 1: Basic 5x5, single RHS
  ! A has diagonal D=[4,3,2,3,4], off-diagonal E=[2,-1.5,0.5,-0.75]
  ! (symmetric positive definite tridiagonal)
  ! True solution x = [1, 2, 3, 4, 5]
  d(1) = 4.0d0; d(2) = 3.0d0; d(3) = 2.0d0; d(4) = 3.0d0; d(5) = 4.0d0
  e(1) = 2.0d0; e(2) = -1.5d0; e(3) = 0.5d0; e(4) = -0.75d0

  ! Compute b = A*x by hand:
  ! b(1) = 4*1 + 2*2 = 8
  ! b(2) = 2*1 + 3*2 + (-1.5)*3 = 3.5
  ! b(3) = (-1.5)*2 + 2*3 + 0.5*4 = 5.0
  ! b(4) = 0.5*3 + 3*4 + (-0.75)*5 = 9.75
  ! b(5) = (-0.75)*4 + 4*5 = 17.0
  b(1) = 8.0d0
  b(2) = 3.5d0
  b(3) = 5.0d0
  b(4) = 9.75d0
  b(5) = 17.0d0

  ! Factorize: dpttrf
  df(1:5) = d(1:5)
  ef(1:4) = e(1:4)
  call dpttrf(5, df, ef, info)

  ! Solve: dpttrs gives initial solution
  x(1:5) = b(1:5)
  call dpttrs(5, 1, df, ef, x, 5, info)

  ! Refine
  call dptrfs(5, 1, d, e, df, ef, b, 5, x, 5, ferr, berr, work, info)

  call begin_test('basic_5x5')
  call print_array('x', x, 5)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_int('info', info)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 2: Multiple RHS (NRHS=2)
  ! Same A matrix
  ! x1 = [1,2,3,4,5], x2 = [5,4,3,2,1]
  d(1) = 4.0d0; d(2) = 3.0d0; d(3) = 2.0d0; d(4) = 3.0d0; d(5) = 4.0d0
  e(1) = 2.0d0; e(2) = -1.5d0; e(3) = 0.5d0; e(4) = -0.75d0

  ! b1 already computed above
  ! b2 = A*[5,4,3,2,1]:
  ! b2(1) = 4*5 + 2*4 = 28
  ! b2(2) = 2*5 + 3*4 + (-1.5)*3 = 17.5
  ! b2(3) = (-1.5)*4 + 2*3 + 0.5*2 = 1.0
  ! b2(4) = 0.5*3 + 3*2 + (-0.75)*1 = 6.75
  ! b2(5) = (-0.75)*2 + 4*1 = 2.5
  ! Column-major: column 1 at 1-5, column 2 at 6-10
  b(1) = 8.0d0; b(2) = 3.5d0; b(3) = 5.0d0; b(4) = 9.75d0; b(5) = 17.0d0
  b(6) = 28.0d0; b(7) = 17.5d0; b(8) = 1.0d0; b(9) = 6.75d0; b(10) = 2.5d0

  df(1:5) = d(1:5)
  ef(1:4) = e(1:4)
  call dpttrf(5, df, ef, info)

  x(1:10) = b(1:10)
  call dpttrs(5, 2, df, ef, x, 5, info)

  call dptrfs(5, 2, d, e, df, ef, b, 5, x, 5, ferr, berr, work, info)

  call begin_test('multi_rhs_2')
  call print_array('x', x, 10)
  call print_array('ferr', ferr, 2)
  call print_array('berr', berr, 2)
  call print_int('info', info)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 3: N=1
  d(1) = 5.0d0
  b(1) = 15.0d0

  df(1) = d(1)
  call dpttrf(1, df, ef, info)

  x(1) = b(1)
  call dpttrs(1, 1, df, ef, x, 1, info)

  call dptrfs(1, 1, d, e, df, ef, b, 1, x, 1, ferr, berr, work, info)

  call begin_test('n_eq_1')
  call print_array('x', x, 1)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_int('info', info)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 4: N=0 (quick return)
  call dptrfs(0, 1, d, e, df, ef, b, 1, x, 1, ferr, berr, work, info)

  call begin_test('n_eq_0')
  call print_int('info', info)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 5: NRHS=0 (quick return)
  call dptrfs(5, 0, d, e, df, ef, b, 5, x, 5, ferr, berr, work, info)

  call begin_test('nrhs_eq_0')
  call print_int('info', info)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 6: Perturbed solution (test actual refinement)
  ! Start with a slightly wrong initial solution and verify refinement improves it
  d(1) = 4.0d0; d(2) = 3.0d0; d(3) = 2.0d0; d(4) = 3.0d0; d(5) = 4.0d0
  e(1) = 2.0d0; e(2) = -1.5d0; e(3) = 0.5d0; e(4) = -0.75d0

  b(1) = 8.0d0; b(2) = 3.5d0; b(3) = 5.0d0; b(4) = 9.75d0; b(5) = 17.0d0

  df(1:5) = d(1:5)
  ef(1:4) = e(1:4)
  call dpttrf(5, df, ef, info)

  ! Solve to get good solution, then perturb
  x(1:5) = b(1:5)
  call dpttrs(5, 1, df, ef, x, 5, info)
  ! Add small perturbation
  x(1) = x(1) + 1.0d-10
  x(3) = x(3) - 1.0d-10
  x(5) = x(5) + 1.0d-10

  call dptrfs(5, 1, d, e, df, ef, b, 5, x, 5, ferr, berr, work, info)

  call begin_test('perturbed')
  call print_array('x', x, 5)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_int('info', info)
  call end_test()

end program
