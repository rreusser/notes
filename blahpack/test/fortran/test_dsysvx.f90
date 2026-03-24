program test_dsysvx
  use test_utils
  implicit none
  double precision :: a(100), af(100), b(100), x(100)
  double precision :: ferr(10), berr(10), work(1000), rcond
  integer :: ipiv(20), iwork(20), info, lwork

  ! Test 1: FACT='N', upper, 3x3 positive definite
  ! A = [4 2 1; 2 5 3; 1 3 6] stored upper col-major
  a = 0.0d0; af = 0.0d0; b = 0.0d0; x = 0.0d0
  a(1) = 4.0d0; a(2) = 0.0d0; a(3) = 0.0d0
  a(4) = 2.0d0; a(5) = 5.0d0; a(6) = 0.0d0
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 6.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  lwork = 100
  call dsysvx('N', 'U', 3, 1, a, 3, af, 3, ipiv, b, 3, x, 3, &
              rcond, ferr, berr, work, lwork, iwork, info)
  call begin_test('fact_n_upper')
  call print_array('x', x, 3)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

  ! Test 2: FACT='N', lower, 3x3 positive definite
  ! A = [4 2 1; 2 5 3; 1 3 6] stored lower col-major
  a = 0.0d0; af = 0.0d0; b = 0.0d0; x = 0.0d0
  a(1) = 4.0d0; a(2) = 2.0d0; a(3) = 1.0d0
  a(4) = 0.0d0; a(5) = 5.0d0; a(6) = 3.0d0
  a(7) = 0.0d0; a(8) = 0.0d0; a(9) = 6.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  lwork = 100
  call dsysvx('N', 'L', 3, 1, a, 3, af, 3, ipiv, b, 3, x, 3, &
              rcond, ferr, berr, work, lwork, iwork, info)
  call begin_test('fact_n_lower')
  call print_array('x', x, 3)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

  ! Test 3: FACT='F', upper, reuse factorization from test 1
  ! First factorize manually, then call with FACT='F'
  a = 0.0d0; af = 0.0d0; b = 0.0d0; x = 0.0d0
  a(1) = 4.0d0; a(2) = 0.0d0; a(3) = 0.0d0
  a(4) = 2.0d0; a(5) = 5.0d0; a(6) = 0.0d0
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 6.0d0
  ! Copy A to AF and factorize
  af(1:9) = a(1:9)
  call dsytrf('U', 3, af, 3, ipiv, work, lwork, info)
  ! Now solve with FACT='F'
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  call dsysvx('F', 'U', 3, 1, a, 3, af, 3, ipiv, b, 3, x, 3, &
              rcond, ferr, berr, work, lwork, iwork, info)
  call begin_test('fact_f_upper')
  call print_array('x', x, 3)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

  ! Test 4: N=0 quick return
  lwork = 100
  call dsysvx('N', 'U', 0, 1, a, 1, af, 1, ipiv, b, 1, x, 1, &
              rcond, ferr, berr, work, lwork, iwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 5: Singular matrix (should return INFO > 0)
  ! A = [1 0; 0 0] singular
  a = 0.0d0; af = 0.0d0; b = 0.0d0; x = 0.0d0
  a(1) = 1.0d0; a(2) = 0.0d0
  a(3) = 0.0d0; a(4) = 0.0d0
  b(1) = 1.0d0; b(2) = 1.0d0
  lwork = 100
  call dsysvx('N', 'U', 2, 1, a, 2, af, 2, ipiv, b, 2, x, 2, &
              rcond, ferr, berr, work, lwork, iwork, info)
  call begin_test('singular')
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call end_test()

  ! Test 6: Multi-RHS (2 right-hand sides)
  a = 0.0d0; af = 0.0d0; b = 0.0d0; x = 0.0d0
  a(1) = 4.0d0; a(2) = 0.0d0; a(3) = 0.0d0
  a(4) = 2.0d0; a(5) = 5.0d0; a(6) = 0.0d0
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 6.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  b(4) = 4.0d0; b(5) = 5.0d0; b(6) = 6.0d0
  lwork = 100
  call dsysvx('N', 'U', 3, 2, a, 3, af, 3, ipiv, b, 3, x, 3, &
              rcond, ferr, berr, work, lwork, iwork, info)
  call begin_test('multi_rhs')
  call print_array('x', x, 6)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 2)
  call print_array('berr', berr, 2)
  call end_test()

  ! Test 7: Ill-conditioned (Hilbert-like 3x3, upper)
  a = 0.0d0; af = 0.0d0; b = 0.0d0; x = 0.0d0
  a(1) = 1.0d0; a(2) = 0.0d0; a(3) = 0.0d0
  a(4) = 0.5d0; a(5) = 1.0d0/3.0d0; a(6) = 0.0d0
  a(7) = 1.0d0/3.0d0; a(8) = 0.25d0; a(9) = 0.2d0
  b(1) = 1.0d0; b(2) = 1.0d0; b(3) = 1.0d0
  lwork = 100
  call dsysvx('N', 'U', 3, 1, a, 3, af, 3, ipiv, b, 3, x, 3, &
              rcond, ferr, berr, work, lwork, iwork, info)
  call begin_test('ill_conditioned')
  call print_array('x', x, 3)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

  ! Test 8: LWORK query (LWORK=-1)
  a = 0.0d0
  a(1) = 4.0d0; a(5) = 5.0d0; a(9) = 6.0d0
  call dsysvx('N', 'U', 3, 1, a, 3, af, 3, ipiv, b, 3, x, 3, &
              rcond, ferr, berr, work, -1, iwork, info)
  call begin_test('lwork_query')
  call print_int('info', info)
  call print_scalar('lwork_opt', work(1))
  call end_test()

end program
