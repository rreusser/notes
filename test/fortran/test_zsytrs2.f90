program test_zsytrs2
  use test_utils
  implicit none

  complex*16 :: a(100), b(100), work(200)
  double precision :: a_r(200), b_r(200)
  equivalence (a, a_r)
  equivalence (b, b_r)
  integer :: ipiv(10), info

  ! Test 1: 4x4 system, UPLO='L', 1 RHS
  ! A = [ (4,1)   (2,1)   (1,0)   (0,0);
  !       (2,1)   (5,2)   (2,1)   (1,0);
  !       (1,0)   (2,1)   (6,1)   (3,1);
  !       (0,0)   (1,0)   (3,1)   (8,2) ]
  ! A is symmetric (NOT Hermitian): A(i,j) = A(j,i)
  ! b = A*[1+0i; 1+0i; 1+0i; 1+0i]
  a = (0.0d0, 0.0d0)
  a(1) = (4.0d0, 1.0d0); a(2) = (2.0d0, 1.0d0); a(3) = (1.0d0, 0.0d0); a(4) = (0.0d0, 0.0d0)
  a(5) = (0.0d0, 0.0d0); a(6) = (5.0d0, 2.0d0); a(7) = (2.0d0, 1.0d0); a(8) = (1.0d0, 0.0d0)
  a(9) = (0.0d0, 0.0d0); a(10) = (0.0d0, 0.0d0); a(11) = (6.0d0, 1.0d0); a(12) = (3.0d0, 1.0d0)
  a(13) = (0.0d0, 0.0d0); a(14) = (0.0d0, 0.0d0); a(15) = (0.0d0, 0.0d0); a(16) = (8.0d0, 2.0d0)
  b = (0.0d0, 0.0d0)
  ! b = A*[1;1;1;1]
  b(1) = (7.0d0, 2.0d0)    ! 4+1i + 2+1i + 1+0i + 0+0i
  b(2) = (10.0d0, 4.0d0)   ! 2+1i + 5+2i + 2+1i + 1+0i
  b(3) = (12.0d0, 3.0d0)   ! 1+0i + 2+1i + 6+1i + 3+1i
  b(4) = (12.0d0, 3.0d0)   ! 0+0i + 1+0i + 3+1i + 8+2i
  call zsytrf('L', 4, a, 4, ipiv, work, 200, info)
  call zsytrs2('L', 4, 1, a, 4, ipiv, b, 4, work, info)
  call begin_test('4x4_lower_1rhs')
  call print_array('b', b_r, 8)
  call print_int('info', info)
  call print_int_array('ipiv', ipiv, 4)
  call end_test()

  ! Test 2: 4x4 system, UPLO='U', 1 RHS
  a = (0.0d0, 0.0d0)
  a(1) = (4.0d0, 1.0d0)
  a(5) = (2.0d0, 1.0d0); a(6) = (5.0d0, 2.0d0)
  a(9) = (1.0d0, 0.0d0); a(10) = (2.0d0, 1.0d0); a(11) = (6.0d0, 1.0d0)
  a(13) = (0.0d0, 0.0d0); a(14) = (1.0d0, 0.0d0); a(15) = (3.0d0, 1.0d0); a(16) = (8.0d0, 2.0d0)
  b = (0.0d0, 0.0d0)
  b(1) = (7.0d0, 2.0d0)
  b(2) = (10.0d0, 4.0d0)
  b(3) = (12.0d0, 3.0d0)
  b(4) = (12.0d0, 3.0d0)
  call zsytrf('U', 4, a, 4, ipiv, work, 200, info)
  call zsytrs2('U', 4, 1, a, 4, ipiv, b, 4, work, info)
  call begin_test('4x4_upper_1rhs')
  call print_array('b', b_r, 8)
  call print_int('info', info)
  call print_int_array('ipiv', ipiv, 4)
  call end_test()

  ! Test 3: 4x4 indefinite with 2x2 pivots, UPLO='L', 1 RHS
  ! A = [ (0,0)   (1,1)   (2,1)   (3,0);
  !       (1,1)   (0,0)   (4,2)   (5,1);
  !       (2,1)   (4,2)   (0,0)   (6,3);
  !       (3,0)   (5,1)   (6,3)   (0,0) ]
  ! b = A*[1+0i;1+0i;1+0i;1+0i]
  a = (0.0d0, 0.0d0)
  a(1) = (0.0d0, 0.0d0); a(2) = (1.0d0, 1.0d0); a(3) = (2.0d0, 1.0d0); a(4) = (3.0d0, 0.0d0)
  a(5) = (0.0d0, 0.0d0); a(6) = (0.0d0, 0.0d0); a(7) = (4.0d0, 2.0d0); a(8) = (5.0d0, 1.0d0)
  a(9) = (0.0d0, 0.0d0); a(10) = (0.0d0, 0.0d0); a(11) = (0.0d0, 0.0d0); a(12) = (6.0d0, 3.0d0)
  a(13) = (0.0d0, 0.0d0); a(14) = (0.0d0, 0.0d0); a(15) = (0.0d0, 0.0d0); a(16) = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  b(1) = (6.0d0, 2.0d0)    ! 0+0i + 1+1i + 2+1i + 3+0i
  b(2) = (10.0d0, 4.0d0)   ! 1+1i + 0+0i + 4+2i + 5+1i
  b(3) = (12.0d0, 6.0d0)   ! 2+1i + 4+2i + 0+0i + 6+3i
  b(4) = (14.0d0, 4.0d0)   ! 3+0i + 5+1i + 6+3i + 0+0i
  call zsytrf('L', 4, a, 4, ipiv, work, 200, info)
  call zsytrs2('L', 4, 1, a, 4, ipiv, b, 4, work, info)
  call begin_test('4x4_indef_lower_1rhs')
  call print_array('b', b_r, 8)
  call print_int('info', info)
  call print_int_array('ipiv', ipiv, 4)
  call end_test()

  ! Test 4: 4x4 indefinite, UPLO='U', 1 RHS
  a = (0.0d0, 0.0d0)
  a(1) = (0.0d0, 0.0d0)
  a(5) = (1.0d0, 1.0d0); a(6) = (0.0d0, 0.0d0)
  a(9) = (2.0d0, 1.0d0); a(10) = (4.0d0, 2.0d0); a(11) = (0.0d0, 0.0d0)
  a(13) = (3.0d0, 0.0d0); a(14) = (5.0d0, 1.0d0); a(15) = (6.0d0, 3.0d0); a(16) = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  b(1) = (6.0d0, 2.0d0)
  b(2) = (10.0d0, 4.0d0)
  b(3) = (12.0d0, 6.0d0)
  b(4) = (14.0d0, 4.0d0)
  call zsytrf('U', 4, a, 4, ipiv, work, 200, info)
  call zsytrs2('U', 4, 1, a, 4, ipiv, b, 4, work, info)
  call begin_test('4x4_indef_upper_1rhs')
  call print_array('b', b_r, 8)
  call print_int('info', info)
  call print_int_array('ipiv', ipiv, 4)
  call end_test()

  ! Test 5: multiple RHS (NRHS=2), UPLO='L'
  ! A = [ (4,1)  (2,1)  (1,0);
  !       (2,1)  (5,2)  (2,1);
  !       (1,0)  (2,1)  (6,1) ]
  ! b col 1 = A*[1;1;1], b col 2 = A*[2;2;2]
  a = (0.0d0, 0.0d0)
  a(1) = (4.0d0, 1.0d0); a(2) = (2.0d0, 1.0d0); a(3) = (1.0d0, 0.0d0)
  a(4) = (0.0d0, 0.0d0); a(5) = (5.0d0, 2.0d0); a(6) = (2.0d0, 1.0d0)
  a(7) = (0.0d0, 0.0d0); a(8) = (0.0d0, 0.0d0); a(9) = (6.0d0, 1.0d0)
  b = (0.0d0, 0.0d0)
  b(1) = (7.0d0, 2.0d0); b(2) = (9.0d0, 4.0d0); b(3) = (9.0d0, 2.0d0)
  b(4) = (14.0d0, 4.0d0); b(5) = (18.0d0, 8.0d0); b(6) = (18.0d0, 4.0d0)
  call zsytrf('L', 3, a, 3, ipiv, work, 200, info)
  call zsytrs2('L', 3, 2, a, 3, ipiv, b, 3, work, info)
  call begin_test('3x3_lower_2rhs')
  call print_array('b', b_r, 12)
  call print_int('info', info)
  call end_test()

  ! Test 6: multiple RHS (NRHS=2), UPLO='U'
  a = (0.0d0, 0.0d0)
  a(1) = (4.0d0, 1.0d0)
  a(4) = (2.0d0, 1.0d0); a(5) = (5.0d0, 2.0d0)
  a(7) = (1.0d0, 0.0d0); a(8) = (2.0d0, 1.0d0); a(9) = (6.0d0, 1.0d0)
  b = (0.0d0, 0.0d0)
  b(1) = (7.0d0, 2.0d0); b(2) = (9.0d0, 4.0d0); b(3) = (9.0d0, 2.0d0)
  b(4) = (14.0d0, 4.0d0); b(5) = (18.0d0, 8.0d0); b(6) = (18.0d0, 4.0d0)
  call zsytrf('U', 3, a, 3, ipiv, work, 200, info)
  call zsytrs2('U', 3, 2, a, 3, ipiv, b, 3, work, info)
  call begin_test('3x3_upper_2rhs')
  call print_array('b', b_r, 12)
  call print_int('info', info)
  call end_test()

  ! Test 7: N=0
  call zsytrs2('L', 0, 1, a, 1, ipiv, b, 1, work, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 8: NRHS=0
  call zsytrs2('L', 3, 0, a, 3, ipiv, b, 3, work, info)
  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

  ! Test 9: N=1, UPLO='L'
  a(1) = (4.0d0, 1.0d0)
  b(1) = (8.0d0, 2.0d0)
  call zsytrf('L', 1, a, 1, ipiv, work, 200, info)
  call zsytrs2('L', 1, 1, a, 1, ipiv, b, 1, work, info)
  call begin_test('n_one_lower')
  call print_array('b', b_r, 2)
  call print_int('info', info)
  call end_test()

  ! Test 10: N=1, UPLO='U'
  a(1) = (4.0d0, 1.0d0)
  b(1) = (8.0d0, 2.0d0)
  call zsytrf('U', 1, a, 1, ipiv, work, 200, info)
  call zsytrs2('U', 1, 1, a, 1, ipiv, b, 1, work, info)
  call begin_test('n_one_upper')
  call print_array('b', b_r, 2)
  call print_int('info', info)
  call end_test()

  ! Test 11: 5x5 mixed pivots, UPLO='L'
  ! A = [ (1,1)   (-2,1)  (0,0)   (3,1)   (1,0);
  !       (-2,1)  (0,0)   (4,2)   (-1,1)  (2,0);
  !       (0,0)   (4,2)   (-3,1)  (2,0)   (0,0);
  !       (3,1)   (-1,1)  (2,0)   (1,1)   (-2,1);
  !       (1,0)   (2,0)   (0,0)   (-2,1)  (4,2) ]
  ! b = A*[1+0i;2+0i;3+0i;4+0i;5+0i]
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 1.0d0); a(2) = (-2.0d0, 1.0d0); a(3) = (0.0d0, 0.0d0)
  a(4) = (3.0d0, 1.0d0); a(5) = (1.0d0, 0.0d0)
  a(6) = (0.0d0, 0.0d0); a(7) = (0.0d0, 0.0d0); a(8) = (4.0d0, 2.0d0)
  a(9) = (-1.0d0, 1.0d0); a(10) = (2.0d0, 0.0d0)
  a(11) = (0.0d0, 0.0d0); a(12) = (0.0d0, 0.0d0); a(13) = (-3.0d0, 1.0d0)
  a(14) = (2.0d0, 0.0d0); a(15) = (0.0d0, 0.0d0)
  a(16) = (0.0d0, 0.0d0); a(17) = (0.0d0, 0.0d0); a(18) = (0.0d0, 0.0d0)
  a(19) = (1.0d0, 1.0d0); a(20) = (-2.0d0, 1.0d0)
  a(21) = (0.0d0, 0.0d0); a(22) = (0.0d0, 0.0d0); a(23) = (0.0d0, 0.0d0)
  a(24) = (0.0d0, 0.0d0); a(25) = (4.0d0, 2.0d0)
  b = (0.0d0, 0.0d0)
  ! b = A*[1;2;3;4;5]
  ! row 1: 1*(1,1) + 2*(-2,1) + 3*(0,0) + 4*(3,1) + 5*(1,0) = (1-4+0+12+5, 1+2+0+4+0) = (14,7)
  ! row 2: 1*(-2,1) + 2*(0,0) + 3*(4,2) + 4*(-1,1) + 5*(2,0) = (-2+0+12-4+10, 1+0+6+4+0) = (16,11)
  ! row 3: 1*(0,0) + 2*(4,2) + 3*(-3,1) + 4*(2,0) + 5*(0,0) = (0+8-9+8+0, 0+4+3+0+0) = (7,7)
  ! row 4: 1*(3,1) + 2*(-1,1) + 3*(2,0) + 4*(1,1) + 5*(-2,1) = (3-2+6+4-10, 1+2+0+4+5) = (1,12)
  ! row 5: 1*(1,0) + 2*(2,0) + 3*(0,0) + 4*(-2,1) + 5*(4,2) = (1+4+0-8+20, 0+0+0+4+10) = (17,14)
  b(1) = (14.0d0, 7.0d0)
  b(2) = (16.0d0, 11.0d0)
  b(3) = (7.0d0, 7.0d0)
  b(4) = (1.0d0, 12.0d0)
  b(5) = (17.0d0, 14.0d0)
  call zsytrf('L', 5, a, 5, ipiv, work, 200, info)
  call zsytrs2('L', 5, 1, a, 5, ipiv, b, 5, work, info)
  call begin_test('5x5_lower_solve')
  call print_array('b', b_r, 10)
  call print_int('info', info)
  call print_int_array('ipiv', ipiv, 5)
  call end_test()

  ! Test 12: 5x5 mixed pivots, UPLO='U'
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 1.0d0)
  a(6) = (-2.0d0, 1.0d0); a(7) = (0.0d0, 0.0d0)
  a(11) = (0.0d0, 0.0d0); a(12) = (4.0d0, 2.0d0); a(13) = (-3.0d0, 1.0d0)
  a(16) = (3.0d0, 1.0d0); a(17) = (-1.0d0, 1.0d0); a(18) = (2.0d0, 0.0d0); a(19) = (1.0d0, 1.0d0)
  a(21) = (1.0d0, 0.0d0); a(22) = (2.0d0, 0.0d0); a(23) = (0.0d0, 0.0d0); a(24) = (-2.0d0, 1.0d0); a(25) = (4.0d0, 2.0d0)
  b = (0.0d0, 0.0d0)
  b(1) = (14.0d0, 7.0d0)
  b(2) = (16.0d0, 11.0d0)
  b(3) = (7.0d0, 7.0d0)
  b(4) = (1.0d0, 12.0d0)
  b(5) = (17.0d0, 14.0d0)
  call zsytrf('U', 5, a, 5, ipiv, work, 200, info)
  call zsytrs2('U', 5, 1, a, 5, ipiv, b, 5, work, info)
  call begin_test('5x5_upper_solve')
  call print_array('b', b_r, 10)
  call print_int('info', info)
  call print_int_array('ipiv', ipiv, 5)
  call end_test()

end program
