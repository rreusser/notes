program test_zpbsvx
  use test_utils
  implicit none

  complex*16 :: ab(200), afb(200), b(200), x(200), work(200)
  double precision :: ab_r(400), afb_r(400), b_r(400), x_r(400)
  equivalence (ab, ab_r)
  equivalence (afb, afb_r)
  equivalence (b, b_r)
  equivalence (x, x_r)
  double precision :: s(20), ferr(20), berr(20), rwork(200), rcond
  integer :: info
  character :: equed

  ! Test 1: FACT='N', UPLO='U', N=3, KD=1, NRHS=1
  ! Hermitian positive definite banded: diag=4, super=(-1,0.5)
  ! Upper band storage (KD+1=2 rows, 3 cols):
  !   row 0 (super): *       (-1,0.5)  (-1,0.5)
  !   row 1 (diag):  (4,0)   (4,0)     (4,0)
  ab = (0.0d0, 0.0d0); afb = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  s = 0.0d0
  ab(1) = (0.0d0, 0.0d0); ab(2) = (4.0d0, 0.0d0)
  ab(3) = (-1.0d0, 0.5d0); ab(4) = (4.0d0, 0.0d0)
  ab(5) = (-1.0d0, 0.5d0); ab(6) = (4.0d0, 0.0d0)
  ! b = (1+i, 2-i, 3+0i)
  b(1) = (1.0d0, 1.0d0); b(2) = (2.0d0, -1.0d0); b(3) = (3.0d0, 0.0d0)
  equed = 'N'
  call zpbsvx('N', 'U', 3, 1, 1, ab, 2, afb, 2, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('fact_n_upper')
  call print_array('x', x_r, 6)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_array('afb', afb_r, 12)
  call print_char('equed', equed)
  call end_test()

  ! Test 2: FACT='N', UPLO='L', N=3, KD=1, NRHS=1
  ! Lower band storage (KD+1=2 rows, 3 cols):
  !   row 0 (diag):  (4,0)      (4,0)      (4,0)
  !   row 1 (sub):   (-1,-0.5)  (-1,-0.5)  *
  ab = (0.0d0, 0.0d0); afb = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  s = 0.0d0
  ab(1) = (4.0d0, 0.0d0); ab(2) = (-1.0d0, -0.5d0)
  ab(3) = (4.0d0, 0.0d0); ab(4) = (-1.0d0, -0.5d0)
  ab(5) = (4.0d0, 0.0d0); ab(6) = (0.0d0, 0.0d0)
  b(1) = (1.0d0, 1.0d0); b(2) = (2.0d0, -1.0d0); b(3) = (3.0d0, 0.0d0)
  equed = 'N'
  call zpbsvx('N', 'L', 3, 1, 1, ab, 2, afb, 2, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('fact_n_lower')
  call print_array('x', x_r, 6)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_array('afb', afb_r, 12)
  call print_char('equed', equed)
  call end_test()

  ! Test 3: FACT='F', UPLO='U', reuse factorization, KD=1
  ab = (0.0d0, 0.0d0); afb = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  s = 0.0d0
  ab(1) = (0.0d0, 0.0d0); ab(2) = (4.0d0, 0.0d0)
  ab(3) = (-1.0d0, 0.5d0); ab(4) = (4.0d0, 0.0d0)
  ab(5) = (-1.0d0, 0.5d0); ab(6) = (4.0d0, 0.0d0)
  afb(1:6) = ab(1:6)
  call zpbtrf('U', 3, 1, afb, 2, info)
  b(1) = (1.0d0, 1.0d0); b(2) = (2.0d0, -1.0d0); b(3) = (3.0d0, 0.0d0)
  equed = 'N'
  call zpbsvx('F', 'U', 3, 1, 1, ab, 2, afb, 2, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('fact_f_upper')
  call print_array('x', x_r, 6)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

  ! Test 4: FACT='F', UPLO='L', reuse factorization, KD=1
  ab = (0.0d0, 0.0d0); afb = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  s = 0.0d0
  ab(1) = (4.0d0, 0.0d0); ab(2) = (-1.0d0, -0.5d0)
  ab(3) = (4.0d0, 0.0d0); ab(4) = (-1.0d0, -0.5d0)
  ab(5) = (4.0d0, 0.0d0); ab(6) = (0.0d0, 0.0d0)
  afb(1:6) = ab(1:6)
  call zpbtrf('L', 3, 1, afb, 2, info)
  b(1) = (1.0d0, 1.0d0); b(2) = (2.0d0, -1.0d0); b(3) = (3.0d0, 0.0d0)
  equed = 'N'
  call zpbsvx('F', 'L', 3, 1, 1, ab, 2, afb, 2, equed, s, b, 3, x, 3, &
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
  call zpbsvx('N', 'U', 0, 0, 1, ab, 1, afb, 1, equed, s, b, 1, x, 1, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: N=1, KD=0, UPLO='U'
  ab = (0.0d0, 0.0d0); afb = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  s = 0.0d0
  ab(1) = (4.0d0, 0.0d0)
  b(1) = (8.0d0, 4.0d0)
  equed = 'N'
  call zpbsvx('N', 'U', 1, 0, 1, ab, 1, afb, 1, equed, s, b, 1, x, 1, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('n_one_upper')
  call print_array('x', x_r, 2)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

  ! Test 7: FACT='E' (equilibrate), UPLO='U', KD=1, 3x3, 1 RHS
  ab = (0.0d0, 0.0d0); afb = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  s = 0.0d0
  ab(1) = (0.0d0, 0.0d0); ab(2) = (4.0d0, 0.0d0)
  ab(3) = (-1.0d0, 0.5d0); ab(4) = (4.0d0, 0.0d0)
  ab(5) = (-1.0d0, 0.5d0); ab(6) = (4.0d0, 0.0d0)
  b(1) = (1.0d0, 1.0d0); b(2) = (2.0d0, -1.0d0); b(3) = (3.0d0, 0.0d0)
  equed = 'N'
  call zpbsvx('E', 'U', 3, 1, 1, ab, 2, afb, 2, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('fact_e_upper')
  call print_array('x', x_r, 6)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_array('afb', afb_r, 12)
  call print_array('s', s, 3)
  call print_char('equed', equed)
  call end_test()

  ! Test 8: FACT='E', UPLO='L', KD=1, 3x3, 1 RHS
  ab = (0.0d0, 0.0d0); afb = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  s = 0.0d0
  ab(1) = (4.0d0, 0.0d0); ab(2) = (-1.0d0, -0.5d0)
  ab(3) = (4.0d0, 0.0d0); ab(4) = (-1.0d0, -0.5d0)
  ab(5) = (4.0d0, 0.0d0); ab(6) = (0.0d0, 0.0d0)
  b(1) = (1.0d0, 1.0d0); b(2) = (2.0d0, -1.0d0); b(3) = (3.0d0, 0.0d0)
  equed = 'N'
  call zpbsvx('E', 'L', 3, 1, 1, ab, 2, afb, 2, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('fact_e_lower')
  call print_array('x', x_r, 6)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_array('afb', afb_r, 12)
  call print_array('s', s, 3)
  call print_char('equed', equed)
  call end_test()

  ! Test 9: Multi-RHS (2 right-hand sides), UPLO='U', FACT='N', KD=1
  ab = (0.0d0, 0.0d0); afb = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  s = 0.0d0
  ab(1) = (0.0d0, 0.0d0); ab(2) = (4.0d0, 0.0d0)
  ab(3) = (-1.0d0, 0.5d0); ab(4) = (4.0d0, 0.0d0)
  ab(5) = (-1.0d0, 0.5d0); ab(6) = (4.0d0, 0.0d0)
  ! b(:,1) = (1+i, 2-i, 3+0i), b(:,2) = (5+i, 4-i, 3+0i)
  b(1) = (1.0d0, 1.0d0); b(2) = (2.0d0, -1.0d0); b(3) = (3.0d0, 0.0d0)
  b(4) = (5.0d0, 1.0d0); b(5) = (4.0d0, -1.0d0); b(6) = (3.0d0, 0.0d0)
  equed = 'N'
  call zpbsvx('N', 'U', 3, 1, 2, ab, 2, afb, 2, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('multi_rhs')
  call print_array('x', x_r, 12)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 2)
  call print_array('berr', berr, 2)
  call end_test()

  ! Test 10: Multi-RHS, UPLO='L', FACT='N', KD=1
  ab = (0.0d0, 0.0d0); afb = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  s = 0.0d0
  ab(1) = (4.0d0, 0.0d0); ab(2) = (-1.0d0, -0.5d0)
  ab(3) = (4.0d0, 0.0d0); ab(4) = (-1.0d0, -0.5d0)
  ab(5) = (4.0d0, 0.0d0); ab(6) = (0.0d0, 0.0d0)
  b(1) = (1.0d0, 1.0d0); b(2) = (2.0d0, -1.0d0); b(3) = (3.0d0, 0.0d0)
  b(4) = (5.0d0, 1.0d0); b(5) = (4.0d0, -1.0d0); b(6) = (3.0d0, 0.0d0)
  equed = 'N'
  call zpbsvx('N', 'L', 3, 1, 2, ab, 2, afb, 2, equed, s, b, 3, x, 3, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('multi_rhs_lower')
  call print_array('x', x_r, 12)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 2)
  call print_array('berr', berr, 2)
  call end_test()

  ! Test 11: Not positive definite (should fail in zpbtrf)
  ! A = [ 1  (2+i); (2-i)  1 ] KD=1
  ab = (0.0d0, 0.0d0); afb = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  s = 0.0d0
  ab(2) = (1.0d0, 0.0d0)  ! diag(1)
  ab(3) = (2.0d0, 1.0d0)  ! super(1,2)
  ab(4) = (1.0d0, 0.0d0)  ! diag(2)
  b(1) = (1.0d0, 0.0d0); b(2) = (1.0d0, 0.0d0)
  equed = 'N'
  call zpbsvx('N', 'U', 2, 1, 1, ab, 2, afb, 2, equed, s, b, 2, x, 2, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('not_pos_def')
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call end_test()

  ! Test 12: FACT='E' with multi-RHS, UPLO='U', KD=1
  ab = (0.0d0, 0.0d0); afb = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  s = 0.0d0
  ab(1) = (0.0d0, 0.0d0); ab(2) = (4.0d0, 0.0d0)
  ab(3) = (-1.0d0, 0.5d0); ab(4) = (4.0d0, 0.0d0)
  ab(5) = (-1.0d0, 0.5d0); ab(6) = (4.0d0, 0.0d0)
  b(1) = (1.0d0, 1.0d0); b(2) = (2.0d0, -1.0d0); b(3) = (3.0d0, 0.0d0)
  b(4) = (5.0d0, 1.0d0); b(5) = (4.0d0, -1.0d0); b(6) = (3.0d0, 0.0d0)
  equed = 'N'
  call zpbsvx('E', 'U', 3, 1, 2, ab, 2, afb, 2, equed, s, b, 3, x, 3, &
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

  ! Test 13: Larger matrix N=4, KD=2 (pentadiagonal), FACT='N', UPLO='U'
  ! Hermitian positive definite band with KD=2
  ! diag=6, super1=(-0.5,-0.5), super2=(0.25,0)
  ab = (0.0d0, 0.0d0); afb = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  s = 0.0d0
  ! Column 1: *, *, (6,0)
  ab(1) = (0.0d0, 0.0d0); ab(2) = (0.0d0, 0.0d0); ab(3) = (6.0d0, 0.0d0)
  ! Column 2: *, (-0.5,-0.5), (6,0)
  ab(4) = (0.0d0, 0.0d0); ab(5) = (-0.5d0, -0.5d0); ab(6) = (6.0d0, 0.0d0)
  ! Column 3: (0.25,0), (-0.5,-0.5), (6,0)
  ab(7) = (0.25d0, 0.0d0); ab(8) = (-0.5d0, -0.5d0); ab(9) = (6.0d0, 0.0d0)
  ! Column 4: (0.25,0), (-0.5,-0.5), (6,0)
  ab(10) = (0.25d0, 0.0d0); ab(11) = (-0.5d0, -0.5d0); ab(12) = (6.0d0, 0.0d0)
  b(1) = (1.0d0, 2.0d0); b(2) = (2.0d0, -1.0d0)
  b(3) = (3.0d0, 0.0d0); b(4) = (4.0d0, 1.0d0)
  equed = 'N'
  call zpbsvx('N', 'U', 4, 2, 1, ab, 3, afb, 3, equed, s, b, 4, x, 4, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('n4_upper_kd2')
  call print_array('x', x_r, 8)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

end program
