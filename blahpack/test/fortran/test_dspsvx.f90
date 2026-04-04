program test_dspsvx
  use test_utils
  implicit none
  double precision :: ap(100), afp(100), b(100), x(100)
  double precision :: ferr(10), berr(10), work(1000), rcond
  integer :: ipiv(20), iwork(20), info

  ! Test 1: FACT='N', UPLO='U', 3x3 SPD, 1 RHS
  ! A = [ 4  2  1;
  !       2  5  3;
  !       1  3  6 ]
  ! Upper packed (col-major): A(1,1), A(1,2), A(2,2), A(1,3), A(2,3), A(3,3)
  ! b = A*[1;1;1] = [7; 10; 10]
  ap = 0.0d0; afp = 0.0d0; b = 0.0d0; x = 0.0d0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 5.0d0
  ap(4) = 1.0d0; ap(5) = 3.0d0; ap(6) = 6.0d0
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 10.0d0
  call dspsvx('N', 'U', 3, 1, ap, afp, ipiv, b, 3, x, 3, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('fact_n_upper')
  call print_array('x', x, 3)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_array('afp', afp, 6)
  call print_int_array('ipiv', ipiv, 3)
  call end_test()

  ! Test 2: FACT='N', UPLO='L', 3x3 SPD, 1 RHS
  ! Same matrix, lower packed: A(1,1), A(2,1), A(3,1), A(2,2), A(3,2), A(3,3)
  ! b = A*[1;1;1] = [7; 10; 10]
  ap = 0.0d0; afp = 0.0d0; b = 0.0d0; x = 0.0d0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 1.0d0
  ap(4) = 5.0d0; ap(5) = 3.0d0; ap(6) = 6.0d0
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 10.0d0
  call dspsvx('N', 'L', 3, 1, ap, afp, ipiv, b, 3, x, 3, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('fact_n_lower')
  call print_array('x', x, 3)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_array('afp', afp, 6)
  call print_int_array('ipiv', ipiv, 3)
  call end_test()

  ! Test 3: FACT='F', UPLO='U', reuse factorization from test 1
  ! First factor manually with dsptrf, then call dspsvx with FACT='F'
  ap = 0.0d0; afp = 0.0d0; b = 0.0d0; x = 0.0d0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 5.0d0
  ap(4) = 1.0d0; ap(5) = 3.0d0; ap(6) = 6.0d0
  ! Copy AP to AFP and factorize
  afp(1:6) = ap(1:6)
  call dsptrf('U', 3, afp, ipiv, info)
  ! Now solve with FACT='F'
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 10.0d0
  call dspsvx('F', 'U', 3, 1, ap, afp, ipiv, b, 3, x, 3, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('fact_f_upper')
  call print_array('x', x, 3)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

  ! Test 4: FACT='F', UPLO='L', reuse factorization
  ap = 0.0d0; afp = 0.0d0; b = 0.0d0; x = 0.0d0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 1.0d0
  ap(4) = 5.0d0; ap(5) = 3.0d0; ap(6) = 6.0d0
  afp(1:6) = ap(1:6)
  call dsptrf('L', 3, afp, ipiv, info)
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 10.0d0
  call dspsvx('F', 'L', 3, 1, ap, afp, ipiv, b, 3, x, 3, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('fact_f_lower')
  call print_array('x', x, 3)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

  ! Test 5: N=0 quick return
  call dspsvx('N', 'U', 0, 1, ap, afp, ipiv, b, 1, x, 1, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: N=1, UPLO='U'
  ap(1) = 4.0d0
  b(1) = 8.0d0
  afp = 0.0d0; x = 0.0d0; ipiv = 0
  call dspsvx('N', 'U', 1, 1, ap, afp, ipiv, b, 1, x, 1, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('n_one_upper')
  call print_array('x', x, 1)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

  ! Test 7: N=1, UPLO='L'
  ap(1) = 5.0d0
  b(1) = 15.0d0
  afp = 0.0d0; x = 0.0d0; ipiv = 0
  call dspsvx('N', 'L', 1, 1, ap, afp, ipiv, b, 1, x, 1, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('n_one_lower')
  call print_array('x', x, 1)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

  ! Test 8: Singular matrix (INFO > 0), UPLO='U'
  ! A = [1  2; 2  4] (rank 1)
  ! Upper packed: A(1,1), A(1,2), A(2,2)
  ap = 0.0d0; afp = 0.0d0; b = 0.0d0; x = 0.0d0; ipiv = 0
  ap(1) = 1.0d0; ap(2) = 2.0d0; ap(3) = 4.0d0
  b(1) = 1.0d0; b(2) = 2.0d0
  call dspsvx('N', 'U', 2, 1, ap, afp, ipiv, b, 2, x, 2, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('singular')
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call end_test()

  ! Test 9: Ill-conditioned 3x3 Hilbert-like, UPLO='U'
  ! A = [1    0.5   0.333;
  !      0.5  0.333 0.25;
  !      0.333 0.25 0.2 ]
  ! Upper packed: A(1,1), A(1,2), A(2,2), A(1,3), A(2,3), A(3,3)
  ap = 0.0d0; afp = 0.0d0; b = 0.0d0; x = 0.0d0; ipiv = 0
  ap(1) = 1.0d0; ap(2) = 0.5d0; ap(3) = 1.0d0/3.0d0
  ap(4) = 1.0d0/3.0d0; ap(5) = 0.25d0; ap(6) = 0.2d0
  b(1) = 1.0d0; b(2) = 1.0d0; b(3) = 1.0d0
  call dspsvx('N', 'U', 3, 1, ap, afp, ipiv, b, 3, x, 3, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('ill_conditioned')
  call print_array('x', x, 3)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

  ! Test 10: Multi-RHS (2 right-hand sides), UPLO='U'
  ap = 0.0d0; afp = 0.0d0; b = 0.0d0; x = 0.0d0; ipiv = 0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 5.0d0
  ap(4) = 1.0d0; ap(5) = 3.0d0; ap(6) = 6.0d0
  ! b(:,1) = A*[1;1;1] = [7;10;10], b(:,2) = A*[2;3;4] = [18;31;35]
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 10.0d0
  b(4) = 18.0d0; b(5) = 31.0d0; b(6) = 35.0d0
  call dspsvx('N', 'U', 3, 2, ap, afp, ipiv, b, 3, x, 3, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('multi_rhs')
  call print_array('x', x, 6)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 2)
  call print_array('berr', berr, 2)
  call end_test()

  ! Test 11: Multi-RHS (2 right-hand sides), UPLO='L'
  ap = 0.0d0; afp = 0.0d0; b = 0.0d0; x = 0.0d0; ipiv = 0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 1.0d0
  ap(4) = 5.0d0; ap(5) = 3.0d0; ap(6) = 6.0d0
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 10.0d0
  b(4) = 18.0d0; b(5) = 31.0d0; b(6) = 35.0d0
  call dspsvx('N', 'L', 3, 2, ap, afp, ipiv, b, 3, x, 3, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('multi_rhs_lower')
  call print_array('x', x, 6)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 2)
  call print_array('berr', berr, 2)
  call end_test()

end program
