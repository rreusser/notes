program test_zspsvx
  use test_utils
  implicit none
  complex*16 :: ap(100), afp(100), b(100), x(100), work(1000)
  double precision :: ap_r(200), afp_r(200), b_r(200), x_r(200)
  double precision :: ferr(10), berr(10), rwork(1000), rcond
  equivalence (ap, ap_r)
  equivalence (afp, afp_r)
  equivalence (b, b_r)
  equivalence (x, x_r)
  integer :: ipiv(20), info

  ! Test 1: FACT='N', UPLO='U', 3x3 complex symmetric, 1 RHS
  ! A = [ (4,1)   (2,-1)   (1,2);
  !       (2,-1)  (5,0.5) (3,-1);
  !       (1,2)   (3,-1)   (6,1) ]
  ! Upper packed (col-major): A(1,1), A(1,2), A(2,2), A(1,3), A(2,3), A(3,3)
  ! b = A*[1;1;1]:
  ! row 1: (4,1)+(2,-1)+(1,2) = (7,2)
  ! row 2: (2,-1)+(5,0.5)+(3,-1) = (10,-1.5)
  ! row 3: (1,2)+(3,-1)+(6,1) = (10,2)
  ap = (0.0d0, 0.0d0); afp = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  ap(1) = (4.0d0, 1.0d0)
  ap(2) = (2.0d0, -1.0d0)
  ap(3) = (5.0d0, 0.5d0)
  ap(4) = (1.0d0, 2.0d0)
  ap(5) = (3.0d0, -1.0d0)
  ap(6) = (6.0d0, 1.0d0)
  b(1) = (7.0d0, 2.0d0)
  b(2) = (10.0d0, -1.5d0)
  b(3) = (10.0d0, 2.0d0)
  ipiv = 0
  call zspsvx('N', 'U', 3, 1, ap, afp, ipiv, b, 3, x, 3, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('fact_n_upper')
  call print_array('x', x_r, 6)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_array('afp', afp_r, 12)
  call print_int_array('ipiv', ipiv, 3)
  call end_test()

  ! Test 2: FACT='N', UPLO='L', 3x3 complex symmetric, 1 RHS
  ! Same matrix, lower packed: A(1,1), A(2,1), A(3,1), A(2,2), A(3,2), A(3,3)
  ap = (0.0d0, 0.0d0); afp = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  ap(1) = (4.0d0, 1.0d0)
  ap(2) = (2.0d0, -1.0d0)
  ap(3) = (1.0d0, 2.0d0)
  ap(4) = (5.0d0, 0.5d0)
  ap(5) = (3.0d0, -1.0d0)
  ap(6) = (6.0d0, 1.0d0)
  b(1) = (7.0d0, 2.0d0)
  b(2) = (10.0d0, -1.5d0)
  b(3) = (10.0d0, 2.0d0)
  ipiv = 0
  call zspsvx('N', 'L', 3, 1, ap, afp, ipiv, b, 3, x, 3, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('fact_n_lower')
  call print_array('x', x_r, 6)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_array('afp', afp_r, 12)
  call print_int_array('ipiv', ipiv, 3)
  call end_test()

  ! Test 3: FACT='F', UPLO='U', reuse factorization from manual zsptrf
  ap = (0.0d0, 0.0d0); afp = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  ap(1) = (4.0d0, 1.0d0)
  ap(2) = (2.0d0, -1.0d0)
  ap(3) = (5.0d0, 0.5d0)
  ap(4) = (1.0d0, 2.0d0)
  ap(5) = (3.0d0, -1.0d0)
  ap(6) = (6.0d0, 1.0d0)
  afp(1:6) = ap(1:6)
  call zsptrf('U', 3, afp, ipiv, info)
  b(1) = (7.0d0, 2.0d0)
  b(2) = (10.0d0, -1.5d0)
  b(3) = (10.0d0, 2.0d0)
  call zspsvx('F', 'U', 3, 1, ap, afp, ipiv, b, 3, x, 3, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('fact_f_upper')
  call print_array('x', x_r, 6)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

  ! Test 4: FACT='F', UPLO='L', reuse factorization
  ap = (0.0d0, 0.0d0); afp = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  ap(1) = (4.0d0, 1.0d0)
  ap(2) = (2.0d0, -1.0d0)
  ap(3) = (1.0d0, 2.0d0)
  ap(4) = (5.0d0, 0.5d0)
  ap(5) = (3.0d0, -1.0d0)
  ap(6) = (6.0d0, 1.0d0)
  afp(1:6) = ap(1:6)
  call zsptrf('L', 3, afp, ipiv, info)
  b(1) = (7.0d0, 2.0d0)
  b(2) = (10.0d0, -1.5d0)
  b(3) = (10.0d0, 2.0d0)
  call zspsvx('F', 'L', 3, 1, ap, afp, ipiv, b, 3, x, 3, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('fact_f_lower')
  call print_array('x', x_r, 6)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

  ! Test 5: N=0 quick return
  call zspsvx('N', 'U', 0, 1, ap, afp, ipiv, b, 1, x, 1, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: N=1, UPLO='U'
  ap(1) = (4.0d0, 1.0d0)
  b(1) = (8.0d0, 2.0d0)
  afp = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0); ipiv = 0
  call zspsvx('N', 'U', 1, 1, ap, afp, ipiv, b, 1, x, 1, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('n_one_upper')
  call print_array('x', x_r, 2)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

  ! Test 7: N=1, UPLO='L'
  ap(1) = (5.0d0, 2.0d0)
  b(1) = (15.0d0, 6.0d0)
  afp = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0); ipiv = 0
  call zspsvx('N', 'L', 1, 1, ap, afp, ipiv, b, 1, x, 1, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('n_one_lower')
  call print_array('x', x_r, 2)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

  ! Test 8: Singular matrix (INFO > 0)
  ! A = [ (1,0) (1,0); (1,0) (1,0) ] (rank 1)
  ! Upper packed: A(1,1), A(1,2), A(2,2)
  ap = (0.0d0, 0.0d0); afp = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0); ipiv = 0
  ap(1) = (1.0d0, 0.0d0); ap(2) = (1.0d0, 0.0d0); ap(3) = (1.0d0, 0.0d0)
  b(1) = (1.0d0, 0.0d0); b(2) = (2.0d0, 0.0d0)
  call zspsvx('N', 'U', 2, 1, ap, afp, ipiv, b, 2, x, 2, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('singular')
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call end_test()

  ! Test 9: Multi-RHS (2 right-hand sides), UPLO='U'
  ap = (0.0d0, 0.0d0); afp = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0); ipiv = 0
  ap(1) = (4.0d0, 1.0d0)
  ap(2) = (2.0d0, -1.0d0)
  ap(3) = (5.0d0, 0.5d0)
  ap(4) = (1.0d0, 2.0d0)
  ap(5) = (3.0d0, -1.0d0)
  ap(6) = (6.0d0, 1.0d0)
  ! b(:,1) = A*[1;1;1] = [(7,2); (10,-1.5); (10,2)]
  b(1) = (7.0d0, 2.0d0)
  b(2) = (10.0d0, -1.5d0)
  b(3) = (10.0d0, 2.0d0)
  ! b(:,2) = A*[2+i; 3-i; 4+0i]:
  ! row 1: (4,1)*(2,1) + (2,-1)*(3,-1) + (1,2)*(4,0) = (7,6) + (5,-5) + (4,8) = (16,9)
  ! row 2: (2,-1)*(2,1) + (5,0.5)*(3,-1) + (3,-1)*(4,0) = (5,0) + (15.5,-3.5) + (12,-4) = (32.5,-7.5)
  ! row 3: (1,2)*(2,1) + (3,-1)*(3,-1) + (6,1)*(4,0) = (0,5) + (8,-6) + (24,4) = (32,3)
  b(4) = (4.0d0, 1.0d0)*(2.0d0, 1.0d0) + (2.0d0, -1.0d0)*(3.0d0, -1.0d0) &
       + (1.0d0, 2.0d0)*(4.0d0, 0.0d0)
  b(5) = (2.0d0, -1.0d0)*(2.0d0, 1.0d0) + (5.0d0, 0.5d0)*(3.0d0, -1.0d0) &
       + (3.0d0, -1.0d0)*(4.0d0, 0.0d0)
  b(6) = (1.0d0, 2.0d0)*(2.0d0, 1.0d0) + (3.0d0, -1.0d0)*(3.0d0, -1.0d0) &
       + (6.0d0, 1.0d0)*(4.0d0, 0.0d0)
  call zspsvx('N', 'U', 3, 2, ap, afp, ipiv, b, 3, x, 3, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('multi_rhs')
  call print_array('x', x_r, 12)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 2)
  call print_array('berr', berr, 2)
  call end_test()

  ! Test 10: Multi-RHS, UPLO='L'
  ap = (0.0d0, 0.0d0); afp = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0); ipiv = 0
  ap(1) = (4.0d0, 1.0d0)
  ap(2) = (2.0d0, -1.0d0)
  ap(3) = (1.0d0, 2.0d0)
  ap(4) = (5.0d0, 0.5d0)
  ap(5) = (3.0d0, -1.0d0)
  ap(6) = (6.0d0, 1.0d0)
  b(1) = (7.0d0, 2.0d0)
  b(2) = (10.0d0, -1.5d0)
  b(3) = (10.0d0, 2.0d0)
  b(4) = (4.0d0, 1.0d0)*(2.0d0, 1.0d0) + (2.0d0, -1.0d0)*(3.0d0, -1.0d0) &
       + (1.0d0, 2.0d0)*(4.0d0, 0.0d0)
  b(5) = (2.0d0, -1.0d0)*(2.0d0, 1.0d0) + (5.0d0, 0.5d0)*(3.0d0, -1.0d0) &
       + (3.0d0, -1.0d0)*(4.0d0, 0.0d0)
  b(6) = (1.0d0, 2.0d0)*(2.0d0, 1.0d0) + (3.0d0, -1.0d0)*(3.0d0, -1.0d0) &
       + (6.0d0, 1.0d0)*(4.0d0, 0.0d0)
  call zspsvx('N', 'L', 3, 2, ap, afp, ipiv, b, 3, x, 3, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('multi_rhs_lower')
  call print_array('x', x_r, 12)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 2)
  call print_array('berr', berr, 2)
  call end_test()

end program
