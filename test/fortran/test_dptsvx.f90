program test_dptsvx
  use test_utils
  implicit none
  double precision :: d(20), e(20), df(20), ef(20)
  double precision :: b(100), x(100)
  double precision :: ferr(10), berr(10), work(200), rcond
  integer :: info

  ! Test 1: FACT='N', N=4, NRHS=1, SPD tridiagonal
  ! D = [4, 5, 6, 7], E = [1, 2, 3]
  ! b = A*[1,1,1,1] = [5, 8, 11, 10]
  d = 0.0d0; e = 0.0d0; df = 0.0d0; ef = 0.0d0
  b = 0.0d0; x = 0.0d0
  d(1) = 4.0d0; d(2) = 5.0d0; d(3) = 6.0d0; d(4) = 7.0d0
  e(1) = 1.0d0; e(2) = 2.0d0; e(3) = 3.0d0
  b(1) = 5.0d0; b(2) = 8.0d0; b(3) = 11.0d0; b(4) = 10.0d0
  call dptsvx('N', 4, 1, d, e, df, ef, b, 4, x, 4, &
              rcond, ferr, berr, work, info)
  call begin_test('fact_n_4x4')
  call print_array('x', x, 4)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_array('df', df, 4)
  call print_array('ef', ef, 3)
  call end_test()

  ! Test 2: FACT='N', N=3, NRHS=1
  ! D = [10, 10, 10], E = [1, 1]
  ! b = A*[1,2,3] = [12, 24, 32]
  d = 0.0d0; e = 0.0d0; df = 0.0d0; ef = 0.0d0
  b = 0.0d0; x = 0.0d0
  d(1) = 10.0d0; d(2) = 10.0d0; d(3) = 10.0d0
  e(1) = 1.0d0; e(2) = 1.0d0
  b(1) = 12.0d0; b(2) = 24.0d0; b(3) = 32.0d0
  call dptsvx('N', 3, 1, d, e, df, ef, b, 3, x, 3, &
              rcond, ferr, berr, work, info)
  call begin_test('fact_n_3x3')
  call print_array('x', x, 3)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

  ! Test 3: FACT='F', reuse factorization from test 1
  ! First factor manually, then call with FACT='F'
  d = 0.0d0; e = 0.0d0; df = 0.0d0; ef = 0.0d0
  b = 0.0d0; x = 0.0d0
  d(1) = 4.0d0; d(2) = 5.0d0; d(3) = 6.0d0; d(4) = 7.0d0
  e(1) = 1.0d0; e(2) = 2.0d0; e(3) = 3.0d0
  ! Copy and factor
  df(1:4) = d(1:4)
  ef(1:3) = e(1:3)
  call dpttrf(4, df, ef, info)
  ! Now solve with FACT='F'
  b(1) = 5.0d0; b(2) = 8.0d0; b(3) = 11.0d0; b(4) = 10.0d0
  call dptsvx('F', 4, 1, d, e, df, ef, b, 4, x, 4, &
              rcond, ferr, berr, work, info)
  call begin_test('fact_f_4x4')
  call print_array('x', x, 4)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

  ! Test 4: N=0 quick return
  d = 0.0d0; e = 0.0d0; df = 0.0d0; ef = 0.0d0
  b = 0.0d0; x = 0.0d0
  call dptsvx('N', 0, 1, d, e, df, ef, b, 1, x, 1, &
              rcond, ferr, berr, work, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 5: N=1, single element
  d = 0.0d0; e = 0.0d0; df = 0.0d0; ef = 0.0d0
  b = 0.0d0; x = 0.0d0
  d(1) = 4.0d0
  b(1) = 8.0d0
  call dptsvx('N', 1, 1, d, e, df, ef, b, 1, x, 1, &
              rcond, ferr, berr, work, info)
  call begin_test('n_one')
  call print_array('x', x, 1)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

  ! Test 6: Not positive definite (should return INFO > 0)
  ! D = [4, -1, 6], E = [1, 2]
  d = 0.0d0; e = 0.0d0; df = 0.0d0; ef = 0.0d0
  b = 0.0d0; x = 0.0d0
  d(1) = 4.0d0; d(2) = -1.0d0; d(3) = 6.0d0
  e(1) = 1.0d0; e(2) = 2.0d0
  b(1) = 1.0d0; b(2) = 1.0d0; b(3) = 1.0d0
  call dptsvx('N', 3, 1, d, e, df, ef, b, 3, x, 3, &
              rcond, ferr, berr, work, info)
  call begin_test('not_posdef')
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call end_test()

  ! Test 7: Multi-RHS (2 right-hand sides), N=3
  ! D = [10, 10, 10], E = [1, 1]
  ! b(:,1) = A*[1,1,1] = [11,12,11], b(:,2) = A*[2,3,4] = [23,35,43]
  d = 0.0d0; e = 0.0d0; df = 0.0d0; ef = 0.0d0
  b = 0.0d0; x = 0.0d0
  d(1) = 10.0d0; d(2) = 10.0d0; d(3) = 10.0d0
  e(1) = 1.0d0; e(2) = 1.0d0
  b(1) = 11.0d0; b(2) = 12.0d0; b(3) = 11.0d0
  b(4) = 23.0d0; b(5) = 35.0d0; b(6) = 43.0d0
  call dptsvx('N', 3, 2, d, e, df, ef, b, 3, x, 3, &
              rcond, ferr, berr, work, info)
  call begin_test('multi_rhs')
  call print_array('x', x, 6)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 2)
  call print_array('berr', berr, 2)
  call end_test()

  ! Test 8: Larger system N=5, NRHS=1
  ! D = [10, 20, 30, 20, 10], E = [1, 2, 3, 2]
  ! b = A*[1,1,1,1,1] = [11, 23, 35, 25, 12]
  d = 0.0d0; e = 0.0d0; df = 0.0d0; ef = 0.0d0
  b = 0.0d0; x = 0.0d0
  d(1) = 10.0d0; d(2) = 20.0d0; d(3) = 30.0d0
  d(4) = 20.0d0; d(5) = 10.0d0
  e(1) = 1.0d0; e(2) = 2.0d0; e(3) = 3.0d0; e(4) = 2.0d0
  b(1) = 11.0d0; b(2) = 23.0d0; b(3) = 35.0d0
  b(4) = 25.0d0; b(5) = 12.0d0
  call dptsvx('N', 5, 1, d, e, df, ef, b, 5, x, 5, &
              rcond, ferr, berr, work, info)
  call begin_test('n5_nrhs1')
  call print_array('x', x, 5)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

  ! Test 9: N=2 system
  ! D = [4, 5], E = [1]
  ! b = A*[1,1] = [5, 6]
  d = 0.0d0; e = 0.0d0; df = 0.0d0; ef = 0.0d0
  b = 0.0d0; x = 0.0d0
  d(1) = 4.0d0; d(2) = 5.0d0
  e(1) = 1.0d0
  b(1) = 5.0d0; b(2) = 6.0d0
  call dptsvx('N', 2, 1, d, e, df, ef, b, 2, x, 2, &
              rcond, ferr, berr, work, info)
  call begin_test('n2_nrhs1')
  call print_array('x', x, 2)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

end program
