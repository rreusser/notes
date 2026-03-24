program test_dgesvx
  use test_utils
  implicit none
  double precision :: a(100), af(100), b(100), x(100)
  double precision :: r(10), c(10), ferr(10), berr(10), work(400)
  integer :: ipiv(10), iwork(10), info, n, nrhs
  double precision :: rcond
  character :: fact, trans, equed

  ! Test 1: FACT='N', TRANS='N', 3x3 well-conditioned, 1 RHS
  n = 3
  nrhs = 1
  fact = 'N'
  trans = 'N'
  ! A = [[4,1,1],[1,3,1],[1,1,2]] (col-major)
  a(1) = 4.0d0; a(2) = 1.0d0; a(3) = 1.0d0
  a(4) = 1.0d0; a(5) = 3.0d0; a(6) = 1.0d0
  a(7) = 1.0d0; a(8) = 1.0d0; a(9) = 2.0d0
  ! b = [1, 2, 3]
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  equed = 'N'
  call dgesvx(fact, trans, n, nrhs, a, n, af, n, ipiv, equed, &
              r, c, b, n, x, n, rcond, ferr, berr, work, iwork, info)
  call begin_test('fact_N_trans_N')
  call print_array('x', x, n)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call print_char('equed', equed)
  call print_scalar('rpvgrw', work(1))
  call end_test()

  ! Test 2: FACT='N', TRANS='T', same system
  n = 3
  nrhs = 1
  fact = 'N'
  trans = 'T'
  a(1) = 4.0d0; a(2) = 1.0d0; a(3) = 1.0d0
  a(4) = 1.0d0; a(5) = 3.0d0; a(6) = 1.0d0
  a(7) = 1.0d0; a(8) = 1.0d0; a(9) = 2.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  equed = 'N'
  call dgesvx(fact, trans, n, nrhs, a, n, af, n, ipiv, equed, &
              r, c, b, n, x, n, rcond, ferr, berr, work, iwork, info)
  call begin_test('fact_N_trans_T')
  call print_array('x', x, n)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call print_char('equed', equed)
  call end_test()

  ! Test 3: FACT='E', equilibrate before solving
  n = 3
  nrhs = 1
  fact = 'E'
  trans = 'N'
  ! Use a poorly scaled matrix
  a(1) = 1.0d6; a(2) = 1.0d0; a(3) = 1.0d0
  a(4) = 1.0d0; a(5) = 1.0d-3; a(6) = 1.0d0
  a(7) = 1.0d0; a(8) = 1.0d0; a(9) = 1.0d3
  b(1) = 1.0d6 + 2.0d0; b(2) = 2.001d0; b(3) = 1.002d3
  equed = 'N'
  call dgesvx(fact, trans, n, nrhs, a, n, af, n, ipiv, equed, &
              r, c, b, n, x, n, rcond, ferr, berr, work, iwork, info)
  call begin_test('fact_E')
  call print_array('x', x, n)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call print_char('equed', equed)
  call end_test()

  ! Test 4: FACT='F' with pre-factored matrix
  ! First factor A via FACT='N'
  n = 3
  nrhs = 1
  fact = 'N'
  trans = 'N'
  a(1) = 4.0d0; a(2) = 1.0d0; a(3) = 1.0d0
  a(4) = 1.0d0; a(5) = 3.0d0; a(6) = 1.0d0
  a(7) = 1.0d0; a(8) = 1.0d0; a(9) = 2.0d0
  b(1) = 6.0d0; b(2) = 5.0d0; b(3) = 4.0d0
  equed = 'N'
  call dgesvx(fact, trans, n, nrhs, a, n, af, n, ipiv, equed, &
              r, c, b, n, x, n, rcond, ferr, berr, work, iwork, info)
  ! Now use FACT='F' with the factored AF and IPIV
  ! Restore A for norm computation
  a(1) = 4.0d0; a(2) = 1.0d0; a(3) = 1.0d0
  a(4) = 1.0d0; a(5) = 3.0d0; a(6) = 1.0d0
  a(7) = 1.0d0; a(8) = 1.0d0; a(9) = 2.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  fact = 'F'
  call dgesvx(fact, trans, n, nrhs, a, n, af, n, ipiv, equed, &
              r, c, b, n, x, n, rcond, ferr, berr, work, iwork, info)
  call begin_test('fact_F')
  call print_array('x', x, n)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call print_char('equed', equed)
  call end_test()

  ! Test 5: singular matrix (info > 0)
  n = 3
  nrhs = 1
  fact = 'N'
  trans = 'N'
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0
  a(4) = 1.0d0; a(5) = 2.0d0; a(6) = 3.0d0
  a(7) = 1.0d0; a(8) = 2.0d0; a(9) = 3.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  equed = 'N'
  call dgesvx(fact, trans, n, nrhs, a, n, af, n, ipiv, equed, &
              r, c, b, n, x, n, rcond, ferr, berr, work, iwork, info)
  call begin_test('singular')
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_scalar('rpvgrw', work(1))
  call end_test()

  ! Test 6: N=0 (quick return)
  n = 0
  nrhs = 1
  fact = 'N'
  trans = 'N'
  equed = 'N'
  call dgesvx(fact, trans, n, nrhs, a, 1, af, 1, ipiv, equed, &
              r, c, b, 1, x, 1, rcond, ferr, berr, work, iwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 7: Multiple RHS (nrhs=2)
  n = 3
  nrhs = 2
  fact = 'N'
  trans = 'N'
  a(1) = 4.0d0; a(2) = 1.0d0; a(3) = 1.0d0
  a(4) = 1.0d0; a(5) = 3.0d0; a(6) = 1.0d0
  a(7) = 1.0d0; a(8) = 1.0d0; a(9) = 2.0d0
  ! b col 1 = [1,2,3], col 2 = [6,5,4]
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  b(4) = 6.0d0; b(5) = 5.0d0; b(6) = 4.0d0
  equed = 'N'
  call dgesvx(fact, trans, n, nrhs, a, n, af, n, ipiv, equed, &
              r, c, b, n, x, n, rcond, ferr, berr, work, iwork, info)
  call begin_test('multi_rhs')
  call print_array('x', x, n*nrhs)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 8: FACT='E' with TRANS='T'
  n = 3
  nrhs = 1
  fact = 'E'
  trans = 'T'
  a(1) = 1.0d6; a(2) = 1.0d0; a(3) = 1.0d0
  a(4) = 1.0d0; a(5) = 1.0d-3; a(6) = 1.0d0
  a(7) = 1.0d0; a(8) = 1.0d0; a(9) = 1.0d3
  b(1) = 1.0d6 + 2.0d0; b(2) = 2.001d0; b(3) = 1.002d3
  equed = 'N'
  call dgesvx(fact, trans, n, nrhs, a, n, af, n, ipiv, equed, &
              r, c, b, n, x, n, rcond, ferr, berr, work, iwork, info)
  call begin_test('fact_E_trans_T')
  call print_array('x', x, n)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call print_char('equed', equed)
  call end_test()

end program
