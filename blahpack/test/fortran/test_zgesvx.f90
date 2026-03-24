program test_zgesvx
  use test_utils
  implicit none
  double precision :: a_r(200), af_r(200), b_r(200), x_r(200), work_r(400)
  complex*16 :: a(100), af(100), b(100), x(100), work(200)
  equivalence (a, a_r)
  equivalence (af, af_r)
  equivalence (b, b_r)
  equivalence (x, x_r)
  equivalence (work, work_r)
  double precision :: r(10), c(10), ferr(10), berr(10), rwork(200)
  integer :: ipiv(10), info, n, nrhs, i, j
  double precision :: rcond
  character :: fact, trans, equed

  ! Test 1: FACT='N', TRANS='N', 3x3 well-conditioned, 1 RHS
  n = 3; nrhs = 1; fact = 'N'; trans = 'N'; equed = 'N'
  a(1) = (4.0d0, 1.0d0);   a(2) = (1.0d0, -1.0d0);  a(3) = (0.5d0, 0.2d0)
  a(4) = (1.0d0, 0.5d0);   a(5) = (3.0d0, 2.0d0);   a(6) = (1.0d0, -0.5d0)
  a(7) = (0.5d0, 0.1d0);   a(8) = (1.0d0, 0.3d0);   a(9) = (2.0d0, 1.0d0)
  ! b = A * [1;1;1]
  do i = 1, n
    b(i) = (0.0d0, 0.0d0)
    do j = 1, n
      b(i) = b(i) + a(i + (j-1)*n)
    end do
  end do
  call zgesvx(fact, trans, n, nrhs, a, n, af, n, ipiv, equed, &
              r, c, b, n, x, n, rcond, ferr, berr, work, rwork, info)
  call begin_test('fact_N_trans_N')
  call print_array('x', x_r, 2*n)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call print_char('equed', equed)
  call print_scalar('rpvgrw', rwork(1))
  call end_test()

  ! Test 2: FACT='N', TRANS='C' (conjugate-transpose)
  n = 3; nrhs = 1; fact = 'N'; trans = 'C'; equed = 'N'
  a(1) = (4.0d0, 1.0d0);   a(2) = (1.0d0, -1.0d0);  a(3) = (0.5d0, 0.2d0)
  a(4) = (1.0d0, 0.5d0);   a(5) = (3.0d0, 2.0d0);   a(6) = (1.0d0, -0.5d0)
  a(7) = (0.5d0, 0.1d0);   a(8) = (1.0d0, 0.3d0);   a(9) = (2.0d0, 1.0d0)
  ! b = A^H * [1;1;1]
  do i = 1, n
    b(i) = (0.0d0, 0.0d0)
    do j = 1, n
      b(i) = b(i) + conjg(a(j + (i-1)*n))
    end do
  end do
  call zgesvx(fact, trans, n, nrhs, a, n, af, n, ipiv, equed, &
              r, c, b, n, x, n, rcond, ferr, berr, work, rwork, info)
  call begin_test('fact_N_trans_C')
  call print_array('x', x_r, 2*n)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call print_char('equed', equed)
  call end_test()

  ! Test 3: FACT='E', equilibrate before solving
  n = 3; nrhs = 1; fact = 'E'; trans = 'N'; equed = 'N'
  a(1) = (1.0d6, 0.0d0);  a(2) = (1.0d0, 0.0d0);  a(3) = (1.0d0, 0.0d0)
  a(4) = (1.0d0, 0.0d0);  a(5) = (1.0d-3, 0.0d0); a(6) = (1.0d0, 0.0d0)
  a(7) = (1.0d0, 0.0d0);  a(8) = (1.0d0, 0.0d0);  a(9) = (1.0d3, 0.0d0)
  b(1) = dcmplx(1.0d6 + 2.0d0, 0.0d0)
  b(2) = dcmplx(2.001d0, 0.0d0)
  b(3) = dcmplx(1.002d3, 0.0d0)
  call zgesvx(fact, trans, n, nrhs, a, n, af, n, ipiv, equed, &
              r, c, b, n, x, n, rcond, ferr, berr, work, rwork, info)
  call begin_test('fact_E')
  call print_array('x', x_r, 2*n)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call print_char('equed', equed)
  call end_test()

  ! Test 4: FACT='F' with pre-factored matrix
  n = 3; nrhs = 1; fact = 'N'; trans = 'N'; equed = 'N'
  a(1) = (4.0d0, 1.0d0);   a(2) = (1.0d0, -1.0d0);  a(3) = (0.5d0, 0.2d0)
  a(4) = (1.0d0, 0.5d0);   a(5) = (3.0d0, 2.0d0);   a(6) = (1.0d0, -0.5d0)
  a(7) = (0.5d0, 0.1d0);   a(8) = (1.0d0, 0.3d0);   a(9) = (2.0d0, 1.0d0)
  do i = 1, n
    b(i) = (0.0d0, 0.0d0)
    do j = 1, n
      b(i) = b(i) + a(i + (j-1)*n)
    end do
  end do
  call zgesvx(fact, trans, n, nrhs, a, n, af, n, ipiv, equed, &
              r, c, b, n, x, n, rcond, ferr, berr, work, rwork, info)
  ! Now use FACT='F' with the factored AF and IPIV, new RHS
  a(1) = (4.0d0, 1.0d0);   a(2) = (1.0d0, -1.0d0);  a(3) = (0.5d0, 0.2d0)
  a(4) = (1.0d0, 0.5d0);   a(5) = (3.0d0, 2.0d0);   a(6) = (1.0d0, -0.5d0)
  a(7) = (0.5d0, 0.1d0);   a(8) = (1.0d0, 0.3d0);   a(9) = (2.0d0, 1.0d0)
  ! b = A * [2+1i; -1+1i; 0.5-0.5i]
  do i = 1, n
    b(i) = a(i)*dcmplx(2.0d0,1.0d0) + a(n+i)*dcmplx(-1.0d0,1.0d0) + &
           a(2*n+i)*dcmplx(0.5d0,-0.5d0)
  end do
  fact = 'F'
  call zgesvx(fact, trans, n, nrhs, a, n, af, n, ipiv, equed, &
              r, c, b, n, x, n, rcond, ferr, berr, work, rwork, info)
  call begin_test('fact_F')
  call print_array('x', x_r, 2*n)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call print_char('equed', equed)
  call end_test()

  ! Test 5: singular matrix (info > 0)
  n = 3; nrhs = 1; fact = 'N'; trans = 'N'; equed = 'N'
  a(1) = (1.0d0, 0.0d0); a(2) = (2.0d0, 0.0d0); a(3) = (3.0d0, 0.0d0)
  a(4) = (1.0d0, 0.0d0); a(5) = (2.0d0, 0.0d0); a(6) = (3.0d0, 0.0d0)
  a(7) = (1.0d0, 0.0d0); a(8) = (2.0d0, 0.0d0); a(9) = (3.0d0, 0.0d0)
  b(1) = (1.0d0, 0.0d0); b(2) = (2.0d0, 0.0d0); b(3) = (3.0d0, 0.0d0)
  call zgesvx(fact, trans, n, nrhs, a, n, af, n, ipiv, equed, &
              r, c, b, n, x, n, rcond, ferr, berr, work, rwork, info)
  call begin_test('singular')
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_scalar('rpvgrw', rwork(1))
  call end_test()

  ! Test 6: N=0 quick return
  n = 0; nrhs = 1; fact = 'N'; trans = 'N'; equed = 'N'
  call zgesvx(fact, trans, n, nrhs, a, 1, af, 1, ipiv, equed, &
              r, c, b, 1, x, 1, rcond, ferr, berr, work, rwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 7: Multiple RHS (nrhs=2)
  n = 3; nrhs = 2; fact = 'N'; trans = 'N'; equed = 'N'
  a(1) = (4.0d0, 1.0d0);   a(2) = (1.0d0, -1.0d0);  a(3) = (0.5d0, 0.2d0)
  a(4) = (1.0d0, 0.5d0);   a(5) = (3.0d0, 2.0d0);   a(6) = (1.0d0, -0.5d0)
  a(7) = (0.5d0, 0.1d0);   a(8) = (1.0d0, 0.3d0);   a(9) = (2.0d0, 1.0d0)
  ! RHS 1: b = A * [1;1;1]
  do i = 1, n
    b(i) = (0.0d0, 0.0d0)
    do j = 1, n
      b(i) = b(i) + a(i + (j-1)*n)
    end do
  end do
  ! RHS 2: b = A * [2+1i; -1+1i; 0.5-0.5i]
  do i = 1, n
    b(n+i) = a(i)*dcmplx(2.0d0,1.0d0) + a(n+i)*dcmplx(-1.0d0,1.0d0) + &
             a(2*n+i)*dcmplx(0.5d0,-0.5d0)
  end do
  call zgesvx(fact, trans, n, nrhs, a, n, af, n, ipiv, equed, &
              r, c, b, n, x, n, rcond, ferr, berr, work, rwork, info)
  call begin_test('multi_rhs')
  call print_array('x', x_r, 2*n*nrhs)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 8: FACT='E' with TRANS='C'
  n = 3; nrhs = 1; fact = 'E'; trans = 'C'; equed = 'N'
  a(1) = (1.0d6, 0.0d0);  a(2) = (1.0d0, 0.0d0);  a(3) = (1.0d0, 0.0d0)
  a(4) = (1.0d0, 0.0d0);  a(5) = (1.0d-3, 0.0d0); a(6) = (1.0d0, 0.0d0)
  a(7) = (1.0d0, 0.0d0);  a(8) = (1.0d0, 0.0d0);  a(9) = (1.0d3, 0.0d0)
  b(1) = dcmplx(1.0d6 + 2.0d0, 0.0d0)
  b(2) = dcmplx(2.001d0, 0.0d0)
  b(3) = dcmplx(1.002d3, 0.0d0)
  call zgesvx(fact, trans, n, nrhs, a, n, af, n, ipiv, equed, &
              r, c, b, n, x, n, rcond, ferr, berr, work, rwork, info)
  call begin_test('fact_E_trans_C')
  call print_array('x', x_r, 2*n)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call print_char('equed', equed)
  call end_test()

end program
