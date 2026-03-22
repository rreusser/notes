program test_zposv
  use test_utils
  implicit none
  complex*16 :: a(100), b(100)
  double precision :: a_r(200), b_r(200)
  equivalence (a, a_r)
  equivalence (b, b_r)
  integer :: info

  ! Hermitian positive definite 3x3 matrix (col-major):
  ! A = [10  3-i  1+2i;  3+i  8  2-i;  1-2i  2+i  6]

  ! Test 1: lower, 3x3, single RHS
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  a(1) = (10.0d0, 0.0d0); a(2) = (3.0d0, 1.0d0); a(3) = (1.0d0, -2.0d0)
  a(4) = (3.0d0, -1.0d0); a(5) = (8.0d0, 0.0d0); a(6) = (2.0d0, 1.0d0)
  a(7) = (1.0d0, 2.0d0); a(8) = (2.0d0, -1.0d0); a(9) = (6.0d0, 0.0d0)
  b(1) = (1.0d0, 1.0d0); b(2) = (2.0d0, -1.0d0); b(3) = (3.0d0, 0.5d0)
  call zposv('L', 3, 1, a, 3, b, 3, info)
  call begin_test('lower_3x3')
  call print_array('x', b_r, 6)
  call print_int('info', info)
  call end_test()

  ! Test 2: upper, 3x3, single RHS
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  a(1) = (10.0d0, 0.0d0); a(2) = (3.0d0, 1.0d0); a(3) = (1.0d0, -2.0d0)
  a(4) = (3.0d0, -1.0d0); a(5) = (8.0d0, 0.0d0); a(6) = (2.0d0, 1.0d0)
  a(7) = (1.0d0, 2.0d0); a(8) = (2.0d0, -1.0d0); a(9) = (6.0d0, 0.0d0)
  b(1) = (1.0d0, 1.0d0); b(2) = (2.0d0, -1.0d0); b(3) = (3.0d0, 0.5d0)
  call zposv('U', 3, 1, a, 3, b, 3, info)
  call begin_test('upper_3x3')
  call print_array('x', b_r, 6)
  call print_int('info', info)
  call end_test()

  ! Test 3: not positive definite (info > 0)
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0); a(2) = (2.0d0, 1.0d0); a(3) = (3.0d0, 0.0d0)
  a(4) = (2.0d0, -1.0d0); a(5) = (1.0d0, 0.0d0); a(6) = (4.0d0, 0.0d0)
  a(7) = (3.0d0, 0.0d0); a(8) = (4.0d0, 0.0d0); a(9) = (1.0d0, 0.0d0)
  b(1) = (1.0d0, 0.0d0); b(2) = (1.0d0, 0.0d0); b(3) = (1.0d0, 0.0d0)
  call zposv('L', 3, 1, a, 3, b, 3, info)
  call begin_test('not_posdef')
  call print_int('info', info)
  call end_test()

  ! Test 4: N=0 quick return
  call zposv('L', 0, 1, a, 1, b, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 5: identity matrix
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0); a(5) = (1.0d0, 0.0d0); a(9) = (1.0d0, 0.0d0)
  b(1) = (3.0d0, 1.0d0); b(2) = (5.0d0, -2.0d0); b(3) = (7.0d0, 0.5d0)
  call zposv('L', 3, 1, a, 3, b, 3, info)
  call begin_test('identity')
  call print_array('x', b_r, 6)
  call print_int('info', info)
  call end_test()

  ! Test 6: multiple RHS (NRHS=2)
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  a(1) = (10.0d0, 0.0d0); a(2) = (3.0d0, 1.0d0); a(3) = (1.0d0, -2.0d0)
  a(4) = (3.0d0, -1.0d0); a(5) = (8.0d0, 0.0d0); a(6) = (2.0d0, 1.0d0)
  a(7) = (1.0d0, 2.0d0); a(8) = (2.0d0, -1.0d0); a(9) = (6.0d0, 0.0d0)
  b(1) = (1.0d0, 0.0d0); b(2) = (0.0d0, 0.0d0); b(3) = (0.0d0, 0.0d0)
  b(4) = (0.0d0, 0.0d0); b(5) = (1.0d0, 0.0d0); b(6) = (0.0d0, 0.0d0)
  call zposv('L', 3, 2, a, 3, b, 3, info)
  call begin_test('multi_rhs')
  call print_array('x', b_r, 12)
  call print_int('info', info)
  call end_test()

  ! Test 7: NRHS=0
  a = (0.0d0, 0.0d0)
  a(1) = (4.0d0, 0.0d0); a(5) = (5.0d0, 0.0d0); a(9) = (9.0d0, 0.0d0)
  call zposv('L', 3, 0, a, 3, b, 3, info)
  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

end program
