program test_zpptrs
  use test_utils
  implicit none
  complex*16 :: ap(100), b(100)
  double precision :: ap_r(200), b_r(200)
  equivalence (ap, ap_r)
  equivalence (b, b_r)
  integer :: info

  ! Hermitian positive definite 3x3 matrix:
  ! A = [10  3-i  1+2i;  3+i  8  2-i;  1-2i  2+i  6]
  ! Upper packed (col-major upper triangle):
  !   col1: A(1,1)=10
  !   col2: A(1,2)=3-i, A(2,2)=8
  !   col3: A(1,3)=1+2i, A(2,3)=2-i, A(3,3)=6
  ! Lower packed (col-major lower triangle):
  !   col1: A(1,1)=10, A(2,1)=3+i, A(3,1)=1-2i
  !   col2: A(2,2)=8, A(3,2)=2+i
  !   col3: A(3,3)=6

  ! Test 1: upper, single RHS
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  ap(1) = (10.0d0, 0.0d0)
  ap(2) = (3.0d0, -1.0d0); ap(3) = (8.0d0, 0.0d0)
  ap(4) = (1.0d0, 2.0d0); ap(5) = (2.0d0, -1.0d0); ap(6) = (6.0d0, 0.0d0)
  call zpptrf('U', 3, ap, info)
  b(1) = (1.0d0, 1.0d0); b(2) = (2.0d0, -1.0d0); b(3) = (3.0d0, 0.5d0)
  call zpptrs('U', 3, 1, ap, b, 3, info)
  call begin_test('upper_single_rhs')
  call print_array('AP', ap_r, 12)
  call print_array('x', b_r, 6)
  call print_int('info', info)
  call end_test()

  ! Test 2: lower, single RHS
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  ap(1) = (10.0d0, 0.0d0); ap(2) = (3.0d0, 1.0d0); ap(3) = (1.0d0, -2.0d0)
  ap(4) = (8.0d0, 0.0d0); ap(5) = (2.0d0, 1.0d0)
  ap(6) = (6.0d0, 0.0d0)
  call zpptrf('L', 3, ap, info)
  b(1) = (1.0d0, 1.0d0); b(2) = (2.0d0, -1.0d0); b(3) = (3.0d0, 0.5d0)
  call zpptrs('L', 3, 1, ap, b, 3, info)
  call begin_test('lower_single_rhs')
  call print_array('AP', ap_r, 12)
  call print_array('x', b_r, 6)
  call print_int('info', info)
  call end_test()

  ! Test 3: lower, multiple RHS (NRHS=2)
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  ap(1) = (10.0d0, 0.0d0); ap(2) = (3.0d0, 1.0d0); ap(3) = (1.0d0, -2.0d0)
  ap(4) = (8.0d0, 0.0d0); ap(5) = (2.0d0, 1.0d0)
  ap(6) = (6.0d0, 0.0d0)
  call zpptrf('L', 3, ap, info)
  ! B is 3x2 col-major: first column (1+0i, 0, 0), second column (0, 1+0i, 0)
  b(1) = (1.0d0, 0.0d0); b(2) = (0.0d0, 0.0d0); b(3) = (0.0d0, 0.0d0)
  b(4) = (0.0d0, 0.0d0); b(5) = (1.0d0, 0.0d0); b(6) = (0.0d0, 0.0d0)
  call zpptrs('L', 3, 2, ap, b, 3, info)
  call begin_test('lower_multi_rhs')
  call print_array('x', b_r, 12)
  call print_int('info', info)
  call end_test()

  ! Test 4: upper, multiple RHS (NRHS=3), compute A^{-1}
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  ap(1) = (10.0d0, 0.0d0)
  ap(2) = (3.0d0, -1.0d0); ap(3) = (8.0d0, 0.0d0)
  ap(4) = (1.0d0, 2.0d0); ap(5) = (2.0d0, -1.0d0); ap(6) = (6.0d0, 0.0d0)
  call zpptrf('U', 3, ap, info)
  ! B = I (3x3 identity, col-major complex)
  b(1) = (1.0d0, 0.0d0); b(2) = (0.0d0, 0.0d0); b(3) = (0.0d0, 0.0d0)
  b(4) = (0.0d0, 0.0d0); b(5) = (1.0d0, 0.0d0); b(6) = (0.0d0, 0.0d0)
  b(7) = (0.0d0, 0.0d0); b(8) = (0.0d0, 0.0d0); b(9) = (1.0d0, 0.0d0)
  call zpptrs('U', 3, 3, ap, b, 3, info)
  call begin_test('upper_multi_rhs_3')
  call print_array('x', b_r, 18)
  call print_int('info', info)
  call end_test()

  ! Test 5: N=0 quick return
  info = -99
  call zpptrs('U', 0, 1, ap, b, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: NRHS=0 quick return
  info = -99
  call zpptrs('L', 3, 0, ap, b, 3, info)
  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

  ! Test 7: 1x1 system
  ! A = (4, 0), packed: AP = [(4,0)]
  ! zpptrf: L = [(2,0)]
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  ap(1) = (4.0d0, 0.0d0)
  call zpptrf('L', 1, ap, info)
  b(1) = (6.0d0, 3.0d0)
  call zpptrs('L', 1, 1, ap, b, 1, info)
  call begin_test('one_by_one')
  call print_array('x', b_r, 2)
  call print_int('info', info)
  call end_test()

  ! Test 8: upper, single RHS with different RHS values
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  ap(1) = (10.0d0, 0.0d0)
  ap(2) = (3.0d0, -1.0d0); ap(3) = (8.0d0, 0.0d0)
  ap(4) = (1.0d0, 2.0d0); ap(5) = (2.0d0, -1.0d0); ap(6) = (6.0d0, 0.0d0)
  call zpptrf('U', 3, ap, info)
  b(1) = (5.0d0, -2.0d0); b(2) = (-1.0d0, 3.0d0); b(3) = (4.0d0, 1.0d0)
  call zpptrs('U', 3, 1, ap, b, 3, info)
  call begin_test('upper_single_rhs_2')
  call print_array('x', b_r, 6)
  call print_int('info', info)
  call end_test()

  ! Test 9: upper, 1x1 system
  ap = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  ap(1) = (9.0d0, 0.0d0)
  call zpptrf('U', 1, ap, info)
  b(1) = (18.0d0, -9.0d0)
  call zpptrs('U', 1, 1, ap, b, 1, info)
  call begin_test('upper_one_by_one')
  call print_array('x', b_r, 2)
  call print_int('info', info)
  call end_test()

end program
