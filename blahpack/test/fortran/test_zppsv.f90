program test_zppsv
  use test_utils
  implicit none
  complex*16 :: ap(100), b(100)
  double precision :: ap_r(200), b_r(200)
  equivalence (ap, ap_r)
  equivalence (b, b_r)
  integer :: info

  ! Hermitian positive definite 3x3 matrix:
  ! A = [10  3-i  1+2i;  3+i  8  2-i;  1-2i  2+i  6]
  ! Upper packed (col-major):
  !   A(1,1)=10, A(1,2)=3-i, A(2,2)=8, A(1,3)=1+2i, A(2,3)=2-i, A(3,3)=6
  ! Lower packed (col-major):
  !   A(1,1)=10, A(2,1)=3+i, A(3,1)=1-2i, A(2,2)=8, A(3,2)=2+i, A(3,3)=6

  ! Test 1: 3x3 HPD, UPLO='U', 1 RHS
  ! b = (1+i, 2-i, 3+0.5i)
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  ap(1) = (10.0d0, 0.0d0)
  ap(2) = (3.0d0, -1.0d0); ap(3) = (8.0d0, 0.0d0)
  ap(4) = (1.0d0, 2.0d0); ap(5) = (2.0d0, -1.0d0); ap(6) = (6.0d0, 0.0d0)
  b(1) = (1.0d0, 1.0d0); b(2) = (2.0d0, -1.0d0); b(3) = (3.0d0, 0.5d0)
  call zppsv('U', 3, 1, ap, b, 3, info)
  call begin_test('3x3_upper_1rhs')
  call print_array('b', b_r, 6)
  call print_array('ap', ap_r, 12)
  call print_int('info', info)
  call end_test()

  ! Test 2: 3x3 HPD, UPLO='L', 1 RHS
  ! Same matrix, lower packed, same RHS
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  ap(1) = (10.0d0, 0.0d0); ap(2) = (3.0d0, 1.0d0); ap(3) = (1.0d0, -2.0d0)
  ap(4) = (8.0d0, 0.0d0); ap(5) = (2.0d0, 1.0d0)
  ap(6) = (6.0d0, 0.0d0)
  b(1) = (1.0d0, 1.0d0); b(2) = (2.0d0, -1.0d0); b(3) = (3.0d0, 0.5d0)
  call zppsv('L', 3, 1, ap, b, 3, info)
  call begin_test('3x3_lower_1rhs')
  call print_array('b', b_r, 6)
  call print_array('ap', ap_r, 12)
  call print_int('info', info)
  call end_test()

  ! Test 3: 3x3 HPD, UPLO='L', 2 RHS
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  ap(1) = (10.0d0, 0.0d0); ap(2) = (3.0d0, 1.0d0); ap(3) = (1.0d0, -2.0d0)
  ap(4) = (8.0d0, 0.0d0); ap(5) = (2.0d0, 1.0d0)
  ap(6) = (6.0d0, 0.0d0)
  ! col1: (1+0i, 0, 0), col2: (0, 1+0i, 0) — solve for first two columns of A^{-1}
  b(1) = (1.0d0, 0.0d0); b(2) = (0.0d0, 0.0d0); b(3) = (0.0d0, 0.0d0)
  b(4) = (0.0d0, 0.0d0); b(5) = (1.0d0, 0.0d0); b(6) = (0.0d0, 0.0d0)
  call zppsv('L', 3, 2, ap, b, 3, info)
  call begin_test('3x3_lower_2rhs')
  call print_array('b', b_r, 12)
  call print_int('info', info)
  call end_test()

  ! Test 4: not positive definite (info > 0), UPLO='U'
  ! A = [1  2+i;  2-i  1]  — not HPD
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  ap(1) = (1.0d0, 0.0d0)
  ap(2) = (2.0d0, 1.0d0)
  ap(3) = (1.0d0, 0.0d0)
  b(1) = (1.0d0, 0.0d0); b(2) = (2.0d0, 0.0d0)
  call zppsv('U', 2, 1, ap, b, 2, info)
  call begin_test('not_hpd_upper')
  call print_int('info', info)
  call end_test()

  ! Test 5: N=0 quick return
  call zppsv('L', 0, 1, ap, b, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: NRHS=0 quick return
  ap(1) = (5.0d0, 0.0d0)
  call zppsv('L', 1, 0, ap, b, 1, info)
  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

  ! Test 7: N=1, UPLO='L'
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  ap(1) = (4.0d0, 0.0d0)
  b(1) = (8.0d0, 4.0d0)
  call zppsv('L', 1, 1, ap, b, 1, info)
  call begin_test('n_one_lower')
  call print_array('b', b_r, 2)
  call print_array('ap', ap_r, 2)
  call print_int('info', info)
  call end_test()

  ! Test 8: N=1, UPLO='U'
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  ap(1) = (9.0d0, 0.0d0)
  b(1) = (27.0d0, -9.0d0)
  call zppsv('U', 1, 1, ap, b, 1, info)
  call begin_test('n_one_upper')
  call print_array('b', b_r, 2)
  call print_array('ap', ap_r, 2)
  call print_int('info', info)
  call end_test()

  ! Test 9: 3x3 UPLO='U', 2 RHS
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  ap(1) = (10.0d0, 0.0d0)
  ap(2) = (3.0d0, -1.0d0); ap(3) = (8.0d0, 0.0d0)
  ap(4) = (1.0d0, 2.0d0); ap(5) = (2.0d0, -1.0d0); ap(6) = (6.0d0, 0.0d0)
  ! col1: (1+i, 2-i, 3+0.5i), col2: (5-2i, -1+3i, 4+i)
  b(1) = (1.0d0, 1.0d0); b(2) = (2.0d0, -1.0d0); b(3) = (3.0d0, 0.5d0)
  b(4) = (5.0d0, -2.0d0); b(5) = (-1.0d0, 3.0d0); b(6) = (4.0d0, 1.0d0)
  call zppsv('U', 3, 2, ap, b, 3, info)
  call begin_test('3x3_upper_2rhs')
  call print_array('b', b_r, 12)
  call print_array('ap', ap_r, 12)
  call print_int('info', info)
  call end_test()

end program
