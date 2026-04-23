program test_zppsvx
  use test_utils
  implicit none
  complex*16 :: ap(100), afp(100), b(100), x(100), work(1000)
  double precision :: ap_r(200), afp_r(200), b_r(200), x_r(200), work_r(2000)
  equivalence (ap, ap_r)
  equivalence (afp, afp_r)
  equivalence (b, b_r)
  equivalence (x, x_r)
  equivalence (work, work_r)
  double precision :: s(20), ferr(10), berr(10), rwork(1000), rcond
  integer :: info
  character :: equed

  ! Hermitian positive definite 3x3 matrix:
  ! A = [10  3-i  1+2i;  3+i  8  2-i;  1-2i  2+i  6]
  ! Upper packed (col-major):
  !   A(1,1)=10, A(1,2)=3-i, A(2,2)=8, A(1,3)=1+2i, A(2,3)=2-i, A(3,3)=6

  ! Test 1: FACT='N', UPLO='U', 3x3 HPD, 1 RHS
  ! b = (1+i, 2-i, 3+0.5i)
  ap = (0.0d0, 0.0d0); afp = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0); s = 0.0d0
  ap(1) = (10.0d0, 0.0d0)
  ap(2) = (3.0d0, -1.0d0); ap(3) = (8.0d0, 0.0d0)
  ap(4) = (1.0d0, 2.0d0); ap(5) = (2.0d0, -1.0d0); ap(6) = (6.0d0, 0.0d0)
  b(1) = (1.0d0, 1.0d0); b(2) = (2.0d0, -1.0d0); b(3) = (3.0d0, 0.5d0)
  equed = 'N'
  call zppsvx('N', 'U', 3, 1, ap, afp, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('fact_n_upper')
  call print_array('x', x_r, 6)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_array('afp', afp_r, 12)
  call print_char('equed', equed)
  call end_test()

  ! Test 2: FACT='N', UPLO='L', 3x3 HPD, 1 RHS
  ap = (0.0d0, 0.0d0); afp = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0); s = 0.0d0
  ap(1) = (10.0d0, 0.0d0); ap(2) = (3.0d0, 1.0d0); ap(3) = (1.0d0, -2.0d0)
  ap(4) = (8.0d0, 0.0d0); ap(5) = (2.0d0, 1.0d0)
  ap(6) = (6.0d0, 0.0d0)
  b(1) = (1.0d0, 1.0d0); b(2) = (2.0d0, -1.0d0); b(3) = (3.0d0, 0.5d0)
  equed = 'N'
  call zppsvx('N', 'L', 3, 1, ap, afp, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('fact_n_lower')
  call print_array('x', x_r, 6)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_array('afp', afp_r, 12)
  call print_char('equed', equed)
  call end_test()

  ! Test 3: FACT='F', UPLO='U', reuse factorization from zpptrf
  ap = (0.0d0, 0.0d0); afp = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0); s = 0.0d0
  ap(1) = (10.0d0, 0.0d0)
  ap(2) = (3.0d0, -1.0d0); ap(3) = (8.0d0, 0.0d0)
  ap(4) = (1.0d0, 2.0d0); ap(5) = (2.0d0, -1.0d0); ap(6) = (6.0d0, 0.0d0)
  afp(1:6) = ap(1:6)
  call zpptrf('U', 3, afp, info)
  b(1) = (1.0d0, 1.0d0); b(2) = (2.0d0, -1.0d0); b(3) = (3.0d0, 0.5d0)
  equed = 'N'
  call zppsvx('F', 'U', 3, 1, ap, afp, equed, s, b, 3, x, 3, &
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
  b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0); s = 0.0d0
  ap(1) = (10.0d0, 0.0d0); ap(2) = (3.0d0, 1.0d0); ap(3) = (1.0d0, -2.0d0)
  ap(4) = (8.0d0, 0.0d0); ap(5) = (2.0d0, 1.0d0)
  ap(6) = (6.0d0, 0.0d0)
  afp(1:6) = ap(1:6)
  call zpptrf('L', 3, afp, info)
  b(1) = (1.0d0, 1.0d0); b(2) = (2.0d0, -1.0d0); b(3) = (3.0d0, 0.5d0)
  equed = 'N'
  call zppsvx('F', 'L', 3, 1, ap, afp, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('fact_f_lower')
  call print_array('x', x_r, 6)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

  ! Test 5: N=0 quick return
  equed = 'N'
  call zppsvx('N', 'U', 0, 1, ap, afp, equed, s, b, 1, x, 1, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: N=1, UPLO='U'
  ap = (0.0d0, 0.0d0); afp = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0); s = 0.0d0
  ap(1) = (4.0d0, 0.0d0)
  b(1) = (8.0d0, 4.0d0)
  equed = 'N'
  call zppsvx('N', 'U', 1, 1, ap, afp, equed, s, b, 1, x, 1, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('n_one_upper')
  call print_array('x', x_r, 2)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

  ! Test 7: FACT='E' (equilibrate), UPLO='U', 3x3 HPD, 1 RHS
  ap = (0.0d0, 0.0d0); afp = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0); s = 0.0d0
  ap(1) = (10.0d0, 0.0d0)
  ap(2) = (3.0d0, -1.0d0); ap(3) = (8.0d0, 0.0d0)
  ap(4) = (1.0d0, 2.0d0); ap(5) = (2.0d0, -1.0d0); ap(6) = (6.0d0, 0.0d0)
  b(1) = (1.0d0, 1.0d0); b(2) = (2.0d0, -1.0d0); b(3) = (3.0d0, 0.5d0)
  equed = 'N'
  call zppsvx('E', 'U', 3, 1, ap, afp, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('fact_e_upper')
  call print_array('x', x_r, 6)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_array('afp', afp_r, 12)
  call print_array('s', s, 3)
  call print_char('equed', equed)
  call end_test()

  ! Test 8: FACT='E', UPLO='L', 3x3 HPD, 1 RHS
  ap = (0.0d0, 0.0d0); afp = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0); s = 0.0d0
  ap(1) = (10.0d0, 0.0d0); ap(2) = (3.0d0, 1.0d0); ap(3) = (1.0d0, -2.0d0)
  ap(4) = (8.0d0, 0.0d0); ap(5) = (2.0d0, 1.0d0)
  ap(6) = (6.0d0, 0.0d0)
  b(1) = (1.0d0, 1.0d0); b(2) = (2.0d0, -1.0d0); b(3) = (3.0d0, 0.5d0)
  equed = 'N'
  call zppsvx('E', 'L', 3, 1, ap, afp, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('fact_e_lower')
  call print_array('x', x_r, 6)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_array('afp', afp_r, 12)
  call print_array('s', s, 3)
  call print_char('equed', equed)
  call end_test()

  ! Test 9: FACT='F', EQUED='Y' (pre-equilibrated), UPLO='U'
  ap = (0.0d0, 0.0d0); afp = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  ap(1) = (10.0d0, 0.0d0)
  ap(2) = (3.0d0, -1.0d0); ap(3) = (8.0d0, 0.0d0)
  ap(4) = (1.0d0, 2.0d0); ap(5) = (2.0d0, -1.0d0); ap(6) = (6.0d0, 0.0d0)
  ! Compute equilibration factors: s(i) = 1/sqrt(Re(A(i,i)))
  s(1) = 1.0d0/sqrt(10.0d0)
  s(2) = 1.0d0/sqrt(8.0d0)
  s(3) = 1.0d0/sqrt(6.0d0)
  ! Equilibrate AP: afp(i,j) = s(i)*ap(i,j)*s(j)
  ! Upper packed: (1,1),(1,2),(2,2),(1,3),(2,3),(3,3)
  afp(1) = s(1)*ap(1)*s(1)
  afp(2) = s(1)*ap(2)*s(2)
  afp(3) = s(2)*ap(3)*s(2)
  afp(4) = s(1)*ap(4)*s(3)
  afp(5) = s(2)*ap(5)*s(3)
  afp(6) = s(3)*ap(6)*s(3)
  ! Factor the equilibrated matrix
  call zpptrf('U', 3, afp, info)
  ! b = (1+i, 2-i, 3+0.5i), equilibrate: b_eq(i) = s(i)*b(i)
  b(1) = s(1)*(1.0d0, 1.0d0)
  b(2) = s(2)*(2.0d0, -1.0d0)
  b(3) = s(3)*(3.0d0, 0.5d0)
  equed = 'Y'
  call zppsvx('F', 'U', 3, 1, ap, afp, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('fact_f_equed_y_upper')
  call print_array('x', x_r, 6)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

  ! Test 10: Multi-RHS (2 right-hand sides), UPLO='U', FACT='N'
  ap = (0.0d0, 0.0d0); afp = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0); s = 0.0d0
  ap(1) = (10.0d0, 0.0d0)
  ap(2) = (3.0d0, -1.0d0); ap(3) = (8.0d0, 0.0d0)
  ap(4) = (1.0d0, 2.0d0); ap(5) = (2.0d0, -1.0d0); ap(6) = (6.0d0, 0.0d0)
  ! col1: (1+i, 2-i, 3+0.5i), col2: (5-2i, -1+3i, 4+i)
  b(1) = (1.0d0, 1.0d0); b(2) = (2.0d0, -1.0d0); b(3) = (3.0d0, 0.5d0)
  b(4) = (5.0d0, -2.0d0); b(5) = (-1.0d0, 3.0d0); b(6) = (4.0d0, 1.0d0)
  equed = 'N'
  call zppsvx('N', 'U', 3, 2, ap, afp, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('multi_rhs')
  call print_array('x', x_r, 12)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 2)
  call print_array('berr', berr, 2)
  call end_test()

  ! Test 11: Multi-RHS, UPLO='L', FACT='N'
  ap = (0.0d0, 0.0d0); afp = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0); s = 0.0d0
  ap(1) = (10.0d0, 0.0d0); ap(2) = (3.0d0, 1.0d0); ap(3) = (1.0d0, -2.0d0)
  ap(4) = (8.0d0, 0.0d0); ap(5) = (2.0d0, 1.0d0)
  ap(6) = (6.0d0, 0.0d0)
  b(1) = (1.0d0, 1.0d0); b(2) = (2.0d0, -1.0d0); b(3) = (3.0d0, 0.5d0)
  b(4) = (5.0d0, -2.0d0); b(5) = (-1.0d0, 3.0d0); b(6) = (4.0d0, 1.0d0)
  equed = 'N'
  call zppsvx('N', 'L', 3, 2, ap, afp, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('multi_rhs_lower')
  call print_array('x', x_r, 12)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 2)
  call print_array('berr', berr, 2)
  call end_test()

  ! Test 12: Not positive definite (should fail in zpptrf)
  ! A = [1  2+i;  2-i  1] — not HPD
  ap = (0.0d0, 0.0d0); afp = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0); s = 0.0d0
  ap(1) = (1.0d0, 0.0d0)
  ap(2) = (2.0d0, 1.0d0)
  ap(3) = (1.0d0, 0.0d0)
  b(1) = (1.0d0, 0.0d0); b(2) = (2.0d0, 0.0d0)
  equed = 'N'
  call zppsvx('N', 'U', 2, 1, ap, afp, equed, s, b, 2, x, 2, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('not_hpd')
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call end_test()

  ! Test 13: FACT='E' with multi-RHS, UPLO='U'
  ap = (0.0d0, 0.0d0); afp = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0); s = 0.0d0
  ap(1) = (10.0d0, 0.0d0)
  ap(2) = (3.0d0, -1.0d0); ap(3) = (8.0d0, 0.0d0)
  ap(4) = (1.0d0, 2.0d0); ap(5) = (2.0d0, -1.0d0); ap(6) = (6.0d0, 0.0d0)
  b(1) = (1.0d0, 1.0d0); b(2) = (2.0d0, -1.0d0); b(3) = (3.0d0, 0.5d0)
  b(4) = (5.0d0, -2.0d0); b(5) = (-1.0d0, 3.0d0); b(6) = (4.0d0, 1.0d0)
  equed = 'N'
  call zppsvx('E', 'U', 3, 2, ap, afp, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('fact_e_multi_rhs')
  call print_array('x', x_r, 12)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 2)
  call print_array('berr', berr, 2)
  call print_array('s', s, 3)
  call print_char('equed', equed)
  call end_test()

end program
