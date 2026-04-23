program test_dposvx
  use test_utils
  implicit none
  double precision :: a(100), af(100), b(100), x(100)
  double precision :: s(10), ferr(10), berr(10), work(400)
  integer :: iwork(10), info, n, nrhs
  double precision :: rcond
  character :: fact, uplo, equed

  ! Test 1: FACT='N', UPLO='U', 3x3 well-conditioned SPD, 1 RHS
  n = 3
  nrhs = 1
  fact = 'N'
  uplo = 'U'
  ! A = [[4,1,0.5],[1,3,1],[0.5,1,2]] (col-major, upper stored)
  a(1) = 4.0d0; a(2) = 1.0d0; a(3) = 0.5d0
  a(4) = 1.0d0; a(5) = 3.0d0; a(6) = 1.0d0
  a(7) = 0.5d0; a(8) = 1.0d0; a(9) = 2.0d0
  b(1) = 5.5d0; b(2) = 5.0d0; b(3) = 3.5d0
  equed = 'N'
  call dposvx(fact, uplo, n, nrhs, a, n, af, n, equed, &
              s, b, n, x, n, rcond, ferr, berr, work, iwork, info)
  call begin_test('fact_N_upper')
  call print_array('x', x, n)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call print_char('equed', equed)
  call end_test()

  ! Test 2: FACT='N', UPLO='L', same system
  n = 3
  nrhs = 1
  fact = 'N'
  uplo = 'L'
  a(1) = 4.0d0; a(2) = 1.0d0; a(3) = 0.5d0
  a(4) = 1.0d0; a(5) = 3.0d0; a(6) = 1.0d0
  a(7) = 0.5d0; a(8) = 1.0d0; a(9) = 2.0d0
  b(1) = 5.5d0; b(2) = 5.0d0; b(3) = 3.5d0
  equed = 'N'
  call dposvx(fact, uplo, n, nrhs, a, n, af, n, equed, &
              s, b, n, x, n, rcond, ferr, berr, work, iwork, info)
  call begin_test('fact_N_lower')
  call print_array('x', x, n)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call print_char('equed', equed)
  call end_test()

  ! Test 3: FACT='E', equilibrate before solving (poorly scaled SPD)
  n = 3
  nrhs = 1
  fact = 'E'
  uplo = 'U'
  ! Poorly scaled SPD: diag(100, 1, 0.01) + small off-diag
  a(1) = 100.0d0; a(2) = 0.0d0; a(3) = 0.0d0
  a(4) = 1.0d0;   a(5) = 1.0d0; a(6) = 0.0d0
  a(7) = 0.1d0;   a(8) = 0.05d0; a(9) = 0.01d0
  ! b = A * [1,1,1]
  b(1) = 101.1d0; b(2) = 1.05d0; b(3) = 0.16d0
  equed = 'N'
  call dposvx(fact, uplo, n, nrhs, a, n, af, n, equed, &
              s, b, n, x, n, rcond, ferr, berr, work, iwork, info)
  call begin_test('fact_E')
  call print_array('x', x, n)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call print_char('equed', equed)
  call print_array('s', s, n)
  call end_test()

  ! Test 4: FACT='F', pre-factored matrix
  ! First factor via FACT='N'
  n = 3
  nrhs = 1
  fact = 'N'
  uplo = 'U'
  a(1) = 4.0d0; a(2) = 1.0d0; a(3) = 0.5d0
  a(4) = 1.0d0; a(5) = 3.0d0; a(6) = 1.0d0
  a(7) = 0.5d0; a(8) = 1.0d0; a(9) = 2.0d0
  b(1) = 5.5d0; b(2) = 5.0d0; b(3) = 3.5d0
  equed = 'N'
  call dposvx(fact, uplo, n, nrhs, a, n, af, n, equed, &
              s, b, n, x, n, rcond, ferr, berr, work, iwork, info)
  ! Now use FACT='F'
  a(1) = 4.0d0; a(2) = 1.0d0; a(3) = 0.5d0
  a(4) = 1.0d0; a(5) = 3.0d0; a(6) = 1.0d0
  a(7) = 0.5d0; a(8) = 1.0d0; a(9) = 2.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  fact = 'F'
  call dposvx(fact, uplo, n, nrhs, a, n, af, n, equed, &
              s, b, n, x, n, rcond, ferr, berr, work, iwork, info)
  call begin_test('fact_F')
  call print_array('x', x, n)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call print_char('equed', equed)
  call end_test()

  ! Test 5: Not positive definite (info > 0)
  n = 3
  nrhs = 1
  fact = 'N'
  uplo = 'U'
  a(1) = 1.0d0; a(2) = 0.0d0; a(3) = 0.0d0
  a(4) = 2.0d0; a(5) = -1.0d0; a(6) = 0.0d0
  a(7) = 3.0d0; a(8) = 4.0d0; a(9) = 5.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  equed = 'N'
  call dposvx(fact, uplo, n, nrhs, a, n, af, n, equed, &
              s, b, n, x, n, rcond, ferr, berr, work, iwork, info)
  call begin_test('not_posdef')
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call end_test()

  ! Test 6: N=0 (quick return)
  n = 0
  nrhs = 1
  fact = 'N'
  uplo = 'U'
  equed = 'N'
  call dposvx(fact, uplo, n, nrhs, a, 1, af, 1, equed, &
              s, b, 1, x, 1, rcond, ferr, berr, work, iwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 7: Multiple RHS (nrhs=2)
  n = 3
  nrhs = 2
  fact = 'N'
  uplo = 'U'
  a(1) = 4.0d0; a(2) = 1.0d0; a(3) = 0.5d0
  a(4) = 1.0d0; a(5) = 3.0d0; a(6) = 1.0d0
  a(7) = 0.5d0; a(8) = 1.0d0; a(9) = 2.0d0
  b(1) = 5.5d0; b(2) = 5.0d0; b(3) = 3.5d0
  b(4) = 1.0d0; b(5) = 2.0d0; b(6) = 3.0d0
  equed = 'N'
  call dposvx(fact, uplo, n, nrhs, a, n, af, n, equed, &
              s, b, n, x, n, rcond, ferr, berr, work, iwork, info)
  call begin_test('multi_rhs')
  call print_array('x', x, n*nrhs)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 8: FACT='E' with UPLO='L'
  n = 3
  nrhs = 1
  fact = 'E'
  uplo = 'L'
  a(1) = 100.0d0; a(2) = 1.0d0; a(3) = 0.1d0
  a(4) = 0.0d0;   a(5) = 1.0d0; a(6) = 0.05d0
  a(7) = 0.0d0;   a(8) = 0.0d0; a(9) = 0.01d0
  b(1) = 101.1d0; b(2) = 1.05d0; b(3) = 0.16d0
  equed = 'N'
  call dposvx(fact, uplo, n, nrhs, a, n, af, n, equed, &
              s, b, n, x, n, rcond, ferr, berr, work, iwork, info)
  call begin_test('fact_E_lower')
  call print_array('x', x, n)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call print_char('equed', equed)
  call end_test()

end program
