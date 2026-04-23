program test_dpbsvx
  use test_utils
  implicit none
  ! AB and AFB: (KD+1) x N band storage, max KD=3, max N=5 => max rows=4, max cols=5
  double precision :: ab(100), afb(100), b(100), x(100)
  double precision :: s(20), ferr(10), berr(10), work(1000), rcond
  integer :: iwork(20), info
  character :: equed

  ! Test 1: FACT='N', UPLO='U', 3x3 SPD, KD=2, 1 RHS
  ! A = [ 4  2  1;
  !       2  5  3;
  !       1  3  6 ]
  ! Upper band storage (KD+1=3 rows, 3 cols):
  !   row 0 (KD-2 super): *   *   1     => AB(1,:)
  !   row 1 (KD-1 super): *   2   3     => AB(2,:)
  !   row 2 (diagonal):   4   5   6     => AB(3,:)
  ! Fortran col-major: AB(1,1)=*,AB(2,1)=*,AB(3,1)=4, AB(1,2)=*,AB(2,2)=2,AB(3,2)=5, AB(1,3)=1,AB(2,3)=3,AB(3,3)=6
  ! b = A*[1;1;1] = [7; 10; 10]
  ab = 0.0d0; afb = 0.0d0; b = 0.0d0; x = 0.0d0; s = 0.0d0
  ab(3) = 4.0d0                             ! AB(3,1) = A(1,1)
  ab(5) = 2.0d0; ab(6) = 5.0d0             ! AB(2,2)=A(1,2), AB(3,2)=A(2,2)
  ab(7) = 1.0d0; ab(8) = 3.0d0; ab(9) = 6.0d0 ! AB(1,3)=A(1,3), AB(2,3)=A(2,3), AB(3,3)=A(3,3)
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 10.0d0
  equed = 'N'
  call dpbsvx('N', 'U', 3, 2, 1, ab, 3, afb, 3, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('fact_n_upper')
  call print_array('x', x, 3)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_array('afb', afb, 9)
  call print_char('equed', equed)
  call end_test()

  ! Test 2: FACT='N', UPLO='L', 3x3 SPD, KD=2, 1 RHS
  ! Lower band storage (KD+1=3 rows, 3 cols):
  !   row 0 (diagonal):   4   5   6     => AB(1,:)
  !   row 1 (1st sub):    2   3   *     => AB(2,:)
  !   row 2 (2nd sub):    1   *   *     => AB(3,:)
  ! Fortran col-major: AB(1,1)=4,AB(2,1)=2,AB(3,1)=1, AB(1,2)=5,AB(2,2)=3,AB(3,2)=*,  AB(1,3)=6,AB(2,3)=*,AB(3,3)=*
  ab = 0.0d0; afb = 0.0d0; b = 0.0d0; x = 0.0d0; s = 0.0d0
  ab(1) = 4.0d0; ab(2) = 2.0d0; ab(3) = 1.0d0  ! col 1
  ab(4) = 5.0d0; ab(5) = 3.0d0                  ! col 2
  ab(7) = 6.0d0                                  ! col 3
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 10.0d0
  equed = 'N'
  call dpbsvx('N', 'L', 3, 2, 1, ab, 3, afb, 3, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('fact_n_lower')
  call print_array('x', x, 3)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_array('afb', afb, 9)
  call print_char('equed', equed)
  call end_test()

  ! Test 3: FACT='F', UPLO='U', reuse factorization, KD=2
  ab = 0.0d0; afb = 0.0d0; b = 0.0d0; x = 0.0d0; s = 0.0d0
  ab(3) = 4.0d0
  ab(5) = 2.0d0; ab(6) = 5.0d0
  ab(7) = 1.0d0; ab(8) = 3.0d0; ab(9) = 6.0d0
  ! Copy AB to AFB and factorize
  afb(1:9) = ab(1:9)
  call dpbtrf('U', 3, 2, afb, 3, info)
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 10.0d0
  equed = 'N'
  call dpbsvx('F', 'U', 3, 2, 1, ab, 3, afb, 3, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('fact_f_upper')
  call print_array('x', x, 3)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

  ! Test 4: FACT='F', UPLO='L', reuse factorization, KD=2
  ab = 0.0d0; afb = 0.0d0; b = 0.0d0; x = 0.0d0; s = 0.0d0
  ab(1) = 4.0d0; ab(2) = 2.0d0; ab(3) = 1.0d0
  ab(4) = 5.0d0; ab(5) = 3.0d0
  ab(7) = 6.0d0
  afb(1:9) = ab(1:9)
  call dpbtrf('L', 3, 2, afb, 3, info)
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 10.0d0
  equed = 'N'
  call dpbsvx('F', 'L', 3, 2, 1, ab, 3, afb, 3, equed, s, b, 3, x, 3, &
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
  call dpbsvx('N', 'U', 0, 0, 1, ab, 1, afb, 1, equed, s, b, 1, x, 1, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: N=1, KD=0, UPLO='U'
  ab = 0.0d0; afb = 0.0d0; x = 0.0d0; s = 0.0d0
  ab(1) = 4.0d0
  b(1) = 8.0d0
  equed = 'N'
  call dpbsvx('N', 'U', 1, 0, 1, ab, 1, afb, 1, equed, s, b, 1, x, 1, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('n_one_upper')
  call print_array('x', x, 1)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

  ! Test 7: FACT='E' (equilibrate), UPLO='U', KD=2, 3x3, 1 RHS
  ab = 0.0d0; afb = 0.0d0; b = 0.0d0; x = 0.0d0; s = 0.0d0
  ab(3) = 4.0d0
  ab(5) = 2.0d0; ab(6) = 5.0d0
  ab(7) = 1.0d0; ab(8) = 3.0d0; ab(9) = 6.0d0
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 10.0d0
  equed = 'N'
  call dpbsvx('E', 'U', 3, 2, 1, ab, 3, afb, 3, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('fact_e_upper')
  call print_array('x', x, 3)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_array('afb', afb, 9)
  call print_array('s', s, 3)
  call print_char('equed', equed)
  call end_test()

  ! Test 8: FACT='E', UPLO='L', KD=2, 3x3, 1 RHS
  ab = 0.0d0; afb = 0.0d0; b = 0.0d0; x = 0.0d0; s = 0.0d0
  ab(1) = 4.0d0; ab(2) = 2.0d0; ab(3) = 1.0d0
  ab(4) = 5.0d0; ab(5) = 3.0d0
  ab(7) = 6.0d0
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 10.0d0
  equed = 'N'
  call dpbsvx('E', 'L', 3, 2, 1, ab, 3, afb, 3, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('fact_e_lower')
  call print_array('x', x, 3)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_array('afb', afb, 9)
  call print_array('s', s, 3)
  call print_char('equed', equed)
  call end_test()

  ! Test 9: FACT='F', EQUED='Y' (pre-equilibrated), UPLO='U', KD=2
  ab = 0.0d0; afb = 0.0d0; b = 0.0d0; x = 0.0d0
  ab(3) = 4.0d0
  ab(5) = 2.0d0; ab(6) = 5.0d0
  ab(7) = 1.0d0; ab(8) = 3.0d0; ab(9) = 6.0d0
  ! Compute equilibration: s(i) = 1/sqrt(A(i,i))
  s(1) = 0.5d0; s(2) = 1.0d0/sqrt(5.0d0); s(3) = 1.0d0/sqrt(6.0d0)
  ! Equilibrate AB: AB_eq = s(i)*A(i,j)*s(j) — need to apply to band storage
  ! For upper band, AB(KD+1+i-j, j) = A(i,j), so we equilibrate each entry
  afb = 0.0d0
  ! A(1,1) at AB(3,1): s(1)*4*s(1) = 0.5*4*0.5 = 1.0
  afb(3) = s(1)*4.0d0*s(1)
  ! A(1,2) at AB(2,2): s(1)*2*s(2)
  afb(5) = s(1)*2.0d0*s(2)
  ! A(2,2) at AB(3,2): s(2)*5*s(2)
  afb(6) = s(2)*5.0d0*s(2)
  ! A(1,3) at AB(1,3): s(1)*1*s(3)
  afb(7) = s(1)*1.0d0*s(3)
  ! A(2,3) at AB(2,3): s(2)*3*s(3)
  afb(8) = s(2)*3.0d0*s(3)
  ! A(3,3) at AB(3,3): s(3)*6*s(3)
  afb(9) = s(3)*6.0d0*s(3)
  call dpbtrf('U', 3, 2, afb, 3, info)
  ! b = A*[1;1;1] = [7;10;10], equilibrate: b_eq(i) = s(i)*b(i)
  b(1) = s(1)*7.0d0; b(2) = s(2)*10.0d0; b(3) = s(3)*10.0d0
  equed = 'Y'
  call dpbsvx('F', 'U', 3, 2, 1, ab, 3, afb, 3, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('fact_f_equed_y_upper')
  call print_array('x', x, 3)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

  ! Test 10: Multi-RHS (2 right-hand sides), UPLO='U', FACT='N', KD=2
  ab = 0.0d0; afb = 0.0d0; b = 0.0d0; x = 0.0d0; s = 0.0d0
  ab(3) = 4.0d0
  ab(5) = 2.0d0; ab(6) = 5.0d0
  ab(7) = 1.0d0; ab(8) = 3.0d0; ab(9) = 6.0d0
  ! b(:,1) = A*[1;1;1] = [7;10;10], b(:,2) = A*[2;3;4] = [18;31;35]
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 10.0d0
  b(4) = 18.0d0; b(5) = 31.0d0; b(6) = 35.0d0
  equed = 'N'
  call dpbsvx('N', 'U', 3, 2, 2, ab, 3, afb, 3, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('multi_rhs')
  call print_array('x', x, 6)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 2)
  call print_array('berr', berr, 2)
  call end_test()

  ! Test 11: Multi-RHS, UPLO='L', FACT='N', KD=2
  ab = 0.0d0; afb = 0.0d0; b = 0.0d0; x = 0.0d0; s = 0.0d0
  ab(1) = 4.0d0; ab(2) = 2.0d0; ab(3) = 1.0d0
  ab(4) = 5.0d0; ab(5) = 3.0d0
  ab(7) = 6.0d0
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 10.0d0
  b(4) = 18.0d0; b(5) = 31.0d0; b(6) = 35.0d0
  equed = 'N'
  call dpbsvx('N', 'L', 3, 2, 2, ab, 3, afb, 3, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('multi_rhs_lower')
  call print_array('x', x, 6)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 2)
  call print_array('berr', berr, 2)
  call end_test()

  ! Test 12: FACT='E' with multi-RHS, UPLO='U', KD=2
  ab = 0.0d0; afb = 0.0d0; b = 0.0d0; x = 0.0d0; s = 0.0d0
  ab(3) = 4.0d0
  ab(5) = 2.0d0; ab(6) = 5.0d0
  ab(7) = 1.0d0; ab(8) = 3.0d0; ab(9) = 6.0d0
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 10.0d0
  b(4) = 18.0d0; b(5) = 31.0d0; b(6) = 35.0d0
  equed = 'N'
  call dpbsvx('E', 'U', 3, 2, 2, ab, 3, afb, 3, equed, s, b, 3, x, 3, &
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

  ! Test 13: Not positive definite (should fail in dpbtrf)
  ! A = [ 1  2; 2  1 ] KD=1
  ! Upper band (KD+1=2 rows, 2 cols):
  !   row 0 (super): *  2
  !   row 1 (diag):  1  1
  ab = 0.0d0; afb = 0.0d0; b = 0.0d0; x = 0.0d0; s = 0.0d0
  ab(2) = 1.0d0  ! AB(2,1) = diag(1)
  ab(3) = 2.0d0  ! AB(1,2) = A(1,2)
  ab(4) = 1.0d0  ! AB(2,2) = diag(2)
  b(1) = 1.0d0; b(2) = 2.0d0
  equed = 'N'
  call dpbsvx('N', 'U', 2, 1, 1, ab, 2, afb, 2, equed, s, b, 2, x, 2, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('not_pos_def')
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call end_test()

  ! Test 14: 4x4 SPD with KD=1 (tridiagonal band), FACT='N', UPLO='U'
  ! A = [10  2  0  0;
  !       2 12  3  0;
  !       0  3 15  4;
  !       0  0  4 20]
  ! Upper band (KD+1=2 rows, 4 cols):
  !   row 0 (super): *  2  3  4
  !   row 1 (diag):  10 12 15 20
  ab = 0.0d0; afb = 0.0d0; b = 0.0d0; x = 0.0d0; s = 0.0d0
  ! col 1: AB(1,1)=*, AB(2,1)=10
  ab(2) = 10.0d0
  ! col 2: AB(1,2)=2, AB(2,2)=12
  ab(3) = 2.0d0; ab(4) = 12.0d0
  ! col 3: AB(1,3)=3, AB(2,3)=15
  ab(5) = 3.0d0; ab(6) = 15.0d0
  ! col 4: AB(1,4)=4, AB(2,4)=20
  ab(7) = 4.0d0; ab(8) = 20.0d0
  ! b = A*[1;1;1;1] = [12;17;22;24]
  b(1) = 12.0d0; b(2) = 17.0d0; b(3) = 22.0d0; b(4) = 24.0d0
  equed = 'N'
  call dpbsvx('N', 'U', 4, 1, 1, ab, 2, afb, 2, equed, s, b, 4, x, 4, &
              rcond, ferr, berr, work, iwork, info)
  call begin_test('n4_upper_kd1')
  call print_array('x', x, 4)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

end program
