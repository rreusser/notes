program test_zposvx
  use test_utils
  implicit none
  integer, parameter :: MAXN = 10
  complex*16 :: a(MAXN*MAXN), af(MAXN*MAXN), b(MAXN*MAXN), x(MAXN*MAXN)
  complex*16 :: work(2*MAXN)
  double precision :: a_r(2*MAXN*MAXN), x_r(2*MAXN*MAXN), b_r(2*MAXN*MAXN)
  equivalence (a, a_r)
  equivalence (x, x_r)
  equivalence (b, b_r)
  double precision :: s(MAXN), ferr(MAXN), berr(MAXN), rwork(MAXN)
  double precision :: rcond
  integer :: info, n, nrhs
  character :: fact, uplo, equed

  ! ============================================================
  ! Test 1: FACT='N', UPLO='U', 3x3 HPD, 1 RHS
  ! A = [[4, 1+i, 0], [1-i, 3, 1], [0, 1, 2]]
  ! ============================================================
  n = 3
  nrhs = 1
  a = (0.0d0, 0.0d0)
  a(1) = (4.0d0, 0.0d0);  a(4) = (1.0d0, 1.0d0);  a(7) = (0.0d0, 0.0d0)
  a(2) = (1.0d0, -1.0d0); a(5) = (3.0d0, 0.0d0);  a(8) = (1.0d0, 0.0d0)
  a(3) = (0.0d0, 0.0d0);  a(6) = (1.0d0, 0.0d0);  a(9) = (2.0d0, 0.0d0)
  b(1) = (5.0d0, 1.0d0); b(2) = (5.0d0, -1.0d0); b(3) = (3.0d0, 0.0d0)
  equed = 'N'
  call zposvx('N', 'U', n, nrhs, a, n, af, n, equed, s, b, n, x, n, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('fact_N_upper')
  call print_array('x', x_r, 2*n)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call print_char('equed', equed)
  call end_test()

  ! ============================================================
  ! Test 2: FACT='N', UPLO='L', same system
  ! ============================================================
  n = 3
  nrhs = 1
  a = (0.0d0, 0.0d0)
  a(1) = (4.0d0, 0.0d0);  a(4) = (1.0d0, 1.0d0);  a(7) = (0.0d0, 0.0d0)
  a(2) = (1.0d0, -1.0d0); a(5) = (3.0d0, 0.0d0);  a(8) = (1.0d0, 0.0d0)
  a(3) = (0.0d0, 0.0d0);  a(6) = (1.0d0, 0.0d0);  a(9) = (2.0d0, 0.0d0)
  b(1) = (5.0d0, 1.0d0); b(2) = (5.0d0, -1.0d0); b(3) = (3.0d0, 0.0d0)
  equed = 'N'
  call zposvx('N', 'L', n, nrhs, a, n, af, n, equed, s, b, n, x, n, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('fact_N_lower')
  call print_array('x', x_r, 2*n)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call print_char('equed', equed)
  call end_test()

  ! ============================================================
  ! Test 3: FACT='E', equilibrate (poorly scaled HPD)
  ! A = [[100, 1+i, 0.1], [1-i, 1, 0.05+0.05i], [0.1, 0.05-0.05i, 0.01]]
  ! ============================================================
  n = 3
  nrhs = 1
  a = (0.0d0, 0.0d0)
  a(1) = (100.0d0, 0.0d0);    a(4) = (1.0d0, 1.0d0);       a(7) = (0.1d0, 0.0d0)
  a(2) = (1.0d0, -1.0d0);     a(5) = (1.0d0, 0.0d0);       a(8) = (0.05d0, 0.05d0)
  a(3) = (0.1d0, 0.0d0);      a(6) = (0.05d0, -0.05d0);    a(9) = (0.01d0, 0.0d0)
  ! b = A * [1+0i, 1+0i, 1+0i]
  b(1) = (101.1d0, 1.0d0); b(2) = (1.05d0, -0.95d0); b(3) = (0.16d0, -0.05d0)
  equed = 'N'
  call zposvx('E', 'U', n, nrhs, a, n, af, n, equed, s, b, n, x, n, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('fact_E')
  call print_array('x', x_r, 2*n)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call print_char('equed', equed)
  call print_array('s', s, n)
  call end_test()

  ! ============================================================
  ! Test 4: FACT='F', pre-factored matrix
  ! First factor with FACT='N', then use FACT='F'
  ! ============================================================
  n = 3
  nrhs = 1
  a = (0.0d0, 0.0d0)
  a(1) = (4.0d0, 0.0d0);  a(4) = (1.0d0, 1.0d0);  a(7) = (0.0d0, 0.0d0)
  a(2) = (1.0d0, -1.0d0); a(5) = (3.0d0, 0.0d0);  a(8) = (1.0d0, 0.0d0)
  a(3) = (0.0d0, 0.0d0);  a(6) = (1.0d0, 0.0d0);  a(9) = (2.0d0, 0.0d0)
  b(1) = (5.0d0, 1.0d0); b(2) = (5.0d0, -1.0d0); b(3) = (3.0d0, 0.0d0)
  equed = 'N'
  call zposvx('N', 'U', n, nrhs, a, n, af, n, equed, s, b, n, x, n, &
              rcond, ferr, berr, work, rwork, info)
  ! Now re-solve with different RHS using FACT='F'
  a = (0.0d0, 0.0d0)
  a(1) = (4.0d0, 0.0d0);  a(4) = (1.0d0, 1.0d0);  a(7) = (0.0d0, 0.0d0)
  a(2) = (1.0d0, -1.0d0); a(5) = (3.0d0, 0.0d0);  a(8) = (1.0d0, 0.0d0)
  a(3) = (0.0d0, 0.0d0);  a(6) = (1.0d0, 0.0d0);  a(9) = (2.0d0, 0.0d0)
  b(1) = (1.0d0, 0.0d0); b(2) = (2.0d0, 1.0d0); b(3) = (3.0d0, 0.0d0)
  call zposvx('F', 'U', n, nrhs, a, n, af, n, equed, s, b, n, x, n, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('fact_F')
  call print_array('x', x_r, 2*n)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call print_char('equed', equed)
  call end_test()

  ! ============================================================
  ! Test 5: Not positive definite (info > 0)
  ! ============================================================
  n = 3
  nrhs = 1
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0);  a(4) = (2.0d0, 0.0d0);  a(7) = (3.0d0, 0.0d0)
  a(2) = (2.0d0, 0.0d0);  a(5) = (-1.0d0, 0.0d0); a(8) = (4.0d0, 0.0d0)
  a(3) = (3.0d0, 0.0d0);  a(6) = (4.0d0, 0.0d0);  a(9) = (5.0d0, 0.0d0)
  b(1) = (1.0d0, 0.0d0); b(2) = (2.0d0, 0.0d0); b(3) = (3.0d0, 0.0d0)
  equed = 'N'
  call zposvx('N', 'U', n, nrhs, a, n, af, n, equed, s, b, n, x, n, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('not_posdef')
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call end_test()

  ! ============================================================
  ! Test 6: N=0 (quick return)
  ! ============================================================
  equed = 'N'
  call zposvx('N', 'U', 0, 1, a, 1, af, 1, equed, s, b, 1, x, 1, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 7: Multiple RHS (nrhs=2)
  ! ============================================================
  n = 3
  nrhs = 2
  a = (0.0d0, 0.0d0)
  a(1) = (4.0d0, 0.0d0);  a(4) = (1.0d0, 1.0d0);  a(7) = (0.0d0, 0.0d0)
  a(2) = (1.0d0, -1.0d0); a(5) = (3.0d0, 0.0d0);  a(8) = (1.0d0, 0.0d0)
  a(3) = (0.0d0, 0.0d0);  a(6) = (1.0d0, 0.0d0);  a(9) = (2.0d0, 0.0d0)
  b(1) = (5.0d0, 1.0d0); b(2) = (5.0d0, -1.0d0); b(3) = (3.0d0, 0.0d0)
  b(4) = (1.0d0, 0.0d0); b(5) = (2.0d0, 1.0d0);  b(6) = (3.0d0, 0.0d0)
  equed = 'N'
  call zposvx('N', 'U', n, nrhs, a, n, af, n, equed, s, b, n, x, n, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('multi_rhs')
  call print_array('x', x_r, 2*n*nrhs)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 8: FACT='E' with UPLO='L'
  ! ============================================================
  n = 3
  nrhs = 1
  a = (0.0d0, 0.0d0)
  a(1) = (100.0d0, 0.0d0);  a(4) = (1.0d0, 1.0d0);       a(7) = (0.1d0, 0.0d0)
  a(2) = (1.0d0, -1.0d0);   a(5) = (1.0d0, 0.0d0);       a(8) = (0.05d0, 0.05d0)
  a(3) = (0.1d0, 0.0d0);    a(6) = (0.05d0, -0.05d0);    a(9) = (0.01d0, 0.0d0)
  b(1) = (101.1d0, 1.0d0); b(2) = (1.05d0, -0.95d0); b(3) = (0.16d0, -0.05d0)
  equed = 'N'
  call zposvx('E', 'L', n, nrhs, a, n, af, n, equed, s, b, n, x, n, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('fact_E_lower')
  call print_array('x', x_r, 2*n)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call print_char('equed', equed)
  call end_test()

end program
