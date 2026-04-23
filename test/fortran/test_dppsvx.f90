program test_dppsvx
  use test_utils
  implicit none
  double precision :: ap(100), afp(100), b(100), x(100)
  double precision :: s(20), ferr(10), berr(10), work(1000), rcond
  integer :: iwork(20), info
  character :: equed

  ! Test 1: FACT='N', UPLO='U', 3x3 SPD, 1 RHS
  ! A = [ 4  2  1;
  !       2  5  3;
  !       1  3  6 ]
  ! Upper packed (col-major): A(1,1), A(1,2), A(2,2), A(1,3), A(2,3), A(3,3)
  ! b = A*[1;1;1] = [7; 10; 10]
  ap = 0.0d0; afp = 0.0d0; b = 0.0d0; x = 0.0d0; s = 0.0d0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 5.0d0
  ap(4) = 1.0d0; ap(5) = 3.0d0; ap(6) = 6.0d0
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 10.0d0
  equed = 'N'
  call dppsvx('N', 'U', 3, 1, ap, afp, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('fact_n_upper')
  call print_array('x', x, 3)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_array('afp', afp, 6)
  call print_char('equed', equed)
  call end_test()

  ! Test 2: FACT='N', UPLO='L', 3x3 SPD, 1 RHS
  ! Same matrix, lower packed: A(1,1), A(2,1), A(3,1), A(2,2), A(3,2), A(3,3)
  ap = 0.0d0; afp = 0.0d0; b = 0.0d0; x = 0.0d0; s = 0.0d0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 1.0d0
  ap(4) = 5.0d0; ap(5) = 3.0d0; ap(6) = 6.0d0
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 10.0d0
  equed = 'N'
  call dppsvx('N', 'L', 3, 1, ap, afp, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('fact_n_lower')
  call print_array('x', x, 3)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_array('afp', afp, 6)
  call print_char('equed', equed)
  call end_test()

  ! Test 3: FACT='F', UPLO='U', reuse factorization from manual dpptrf
  ap = 0.0d0; afp = 0.0d0; b = 0.0d0; x = 0.0d0; s = 0.0d0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 5.0d0
  ap(4) = 1.0d0; ap(5) = 3.0d0; ap(6) = 6.0d0
  ! Copy AP to AFP and factorize
  afp(1:6) = ap(1:6)
  call dpptrf('U', 3, afp, info)
  ! Now solve with FACT='F', EQUED='N'
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 10.0d0
  equed = 'N'
  call dppsvx('F', 'U', 3, 1, ap, afp, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('fact_f_upper')
  call print_array('x', x, 3)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

  ! Test 4: FACT='F', UPLO='L', reuse factorization
  ap = 0.0d0; afp = 0.0d0; b = 0.0d0; x = 0.0d0; s = 0.0d0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 1.0d0
  ap(4) = 5.0d0; ap(5) = 3.0d0; ap(6) = 6.0d0
  afp(1:6) = ap(1:6)
  call dpptrf('L', 3, afp, info)
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 10.0d0
  equed = 'N'
  call dppsvx('F', 'L', 3, 1, ap, afp, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('fact_f_lower')
  call print_array('x', x, 3)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

  ! Test 5: N=0 quick return
  equed = 'N'
  call dppsvx('N', 'U', 0, 1, ap, afp, equed, s, b, 1, x, 1, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: N=1, UPLO='U'
  ap(1) = 4.0d0
  b(1) = 8.0d0
  afp = 0.0d0; x = 0.0d0; s = 0.0d0
  equed = 'N'
  call dppsvx('N', 'U', 1, 1, ap, afp, equed, s, b, 1, x, 1, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('n_one_upper')
  call print_array('x', x, 1)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

  ! Test 7: FACT='E' (equilibrate), UPLO='U', 3x3 SPD, 1 RHS
  ! A = [ 4  2  1;
  !       2  5  3;
  !       1  3  6 ]
  ! b = A*[1;1;1] = [7; 10; 10]
  ap = 0.0d0; afp = 0.0d0; b = 0.0d0; x = 0.0d0; s = 0.0d0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 5.0d0
  ap(4) = 1.0d0; ap(5) = 3.0d0; ap(6) = 6.0d0
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 10.0d0
  equed = 'N'
  call dppsvx('E', 'U', 3, 1, ap, afp, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('fact_e_upper')
  call print_array('x', x, 3)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_array('afp', afp, 6)
  call print_array('s', s, 3)
  call print_char('equed', equed)
  call end_test()

  ! Test 8: FACT='E', UPLO='L', 3x3 SPD, 1 RHS
  ap = 0.0d0; afp = 0.0d0; b = 0.0d0; x = 0.0d0; s = 0.0d0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 1.0d0
  ap(4) = 5.0d0; ap(5) = 3.0d0; ap(6) = 6.0d0
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 10.0d0
  equed = 'N'
  call dppsvx('E', 'L', 3, 1, ap, afp, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('fact_e_lower')
  call print_array('x', x, 3)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_array('afp', afp, 6)
  call print_array('s', s, 3)
  call print_char('equed', equed)
  call end_test()

  ! Test 9: FACT='F', EQUED='Y' (pre-equilibrated), UPLO='U'
  ! First equilibrate manually, then factor, then solve with FACT='F'
  ap = 0.0d0; afp = 0.0d0; b = 0.0d0; x = 0.0d0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 5.0d0
  ap(4) = 1.0d0; ap(5) = 3.0d0; ap(6) = 6.0d0
  ! Compute equilibration
  s = 0.0d0
  s(1) = 0.5d0; s(2) = 1.0d0/sqrt(5.0d0); s(3) = 1.0d0/sqrt(6.0d0)
  ! Equilibrate AP: AP_eq(i,j) = s(i)*AP(i,j)*s(j)
  ! Upper packed: (1,1),(1,2),(2,2),(1,3),(2,3),(3,3)
  afp(1) = s(1)*ap(1)*s(1)
  afp(2) = s(1)*ap(2)*s(2)
  afp(3) = s(2)*ap(3)*s(2)
  afp(4) = s(1)*ap(4)*s(3)
  afp(5) = s(2)*ap(5)*s(3)
  afp(6) = s(3)*ap(6)*s(3)
  ! Factor the equilibrated matrix
  call dpptrf('U', 3, afp, info)
  ! b = A*[1;1;1] = [7;10;10], equilibrate: b_eq(i) = s(i)*b(i)
  b(1) = s(1)*7.0d0; b(2) = s(2)*10.0d0; b(3) = s(3)*10.0d0
  equed = 'Y'
  call dppsvx('F', 'U', 3, 1, ap, afp, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('fact_f_equed_y_upper')
  call print_array('x', x, 3)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

  ! Test 10: Multi-RHS (2 right-hand sides), UPLO='U', FACT='N'
  ap = 0.0d0; afp = 0.0d0; b = 0.0d0; x = 0.0d0; s = 0.0d0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 5.0d0
  ap(4) = 1.0d0; ap(5) = 3.0d0; ap(6) = 6.0d0
  ! b(:,1) = A*[1;1;1] = [7;10;10], b(:,2) = A*[2;3;4] = [18;31;35]
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 10.0d0
  b(4) = 18.0d0; b(5) = 31.0d0; b(6) = 35.0d0
  equed = 'N'
  call dppsvx('N', 'U', 3, 2, ap, afp, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('multi_rhs')
  call print_array('x', x, 6)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 2)
  call print_array('berr', berr, 2)
  call end_test()

  ! Test 11: Multi-RHS, UPLO='L', FACT='N'
  ap = 0.0d0; afp = 0.0d0; b = 0.0d0; x = 0.0d0; s = 0.0d0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 1.0d0
  ap(4) = 5.0d0; ap(5) = 3.0d0; ap(6) = 6.0d0
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 10.0d0
  b(4) = 18.0d0; b(5) = 31.0d0; b(6) = 35.0d0
  equed = 'N'
  call dppsvx('N', 'L', 3, 2, ap, afp, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('multi_rhs_lower')
  call print_array('x', x, 6)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 2)
  call print_array('berr', berr, 2)
  call end_test()

  ! Test 12: FACT='E' with multi-RHS, UPLO='U'
  ap = 0.0d0; afp = 0.0d0; b = 0.0d0; x = 0.0d0; s = 0.0d0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 5.0d0
  ap(4) = 1.0d0; ap(5) = 3.0d0; ap(6) = 6.0d0
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 10.0d0
  b(4) = 18.0d0; b(5) = 31.0d0; b(6) = 35.0d0
  equed = 'N'
  call dppsvx('E', 'U', 3, 2, ap, afp, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('fact_e_multi_rhs')
  call print_array('x', x, 6)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 2)
  call print_array('berr', berr, 2)
  call print_array('s', s, 3)
  call print_char('equed', equed)
  call end_test()

  ! Test 13: Not positive definite (should fail in dpptrf)
  ! A = [ 1  2; 2  1 ] (eigenvalues: 3, -1 => not pos def)
  ! Upper packed: A(1,1), A(1,2), A(2,2)
  ap = 0.0d0; afp = 0.0d0; b = 0.0d0; x = 0.0d0; s = 0.0d0
  ap(1) = 1.0d0; ap(2) = 2.0d0; ap(3) = 1.0d0
  b(1) = 1.0d0; b(2) = 2.0d0
  equed = 'N'
  call dppsvx('N', 'U', 2, 1, ap, afp, equed, s, b, 2, x, 2, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('not_pos_def')
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call end_test()

  ! Test 14: 4x4 SPD, FACT='N', UPLO='U', 1 RHS
  ! A = [10  2  1  0;
  !       2 12  3  1;
  !       1  3 15  4;
  !       0  1  4 20]
  ! Upper packed: (1,1),(1,2),(2,2),(1,3),(2,3),(3,3),(1,4),(2,4),(3,4),(4,4)
  ap = 0.0d0; afp = 0.0d0; b = 0.0d0; x = 0.0d0; s = 0.0d0
  ap(1) = 10.0d0; ap(2) = 2.0d0; ap(3) = 12.0d0
  ap(4) = 1.0d0; ap(5) = 3.0d0; ap(6) = 15.0d0
  ap(7) = 0.0d0; ap(8) = 1.0d0; ap(9) = 4.0d0; ap(10) = 20.0d0
  ! b = A*[1;1;1;1] = [13;18;23;25]
  b(1) = 13.0d0; b(2) = 18.0d0; b(3) = 23.0d0; b(4) = 25.0d0
  equed = 'N'
  call dppsvx('N', 'U', 4, 1, ap, afp, equed, s, b, 4, x, 4, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('n4_upper')
  call print_array('x', x, 4)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

end program
