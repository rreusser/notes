program test_dpptrs
  use test_utils
  implicit none
  ! Packed storage: N*(N+1)/2 elements for N=4 -> 10 elements max
  double precision :: ap(100), b(100)
  integer :: info, n

  ! Use SPD matrix A = [4 2 1; 2 5 3; 1 3 9] (3x3)
  ! Upper packed: column-major upper triangle
  !   col1: A(1,1) = 4
  !   col2: A(1,2) = 2, A(2,2) = 5
  !   col3: A(1,3) = 1, A(2,3) = 3, A(3,3) = 9
  !   AP = [4, 2, 5, 1, 3, 9]
  ! Lower packed: column-major lower triangle
  !   col1: A(1,1) = 4, A(2,1) = 2, A(3,1) = 1
  !   col2: A(2,2) = 5, A(3,2) = 3
  !   col3: A(3,3) = 9
  !   AP = [4, 2, 1, 5, 3, 9]

  ! Test 1: upper, single RHS
  n = 3
  ap = 0.0d0; b = 0.0d0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 5.0d0
  ap(4) = 1.0d0; ap(5) = 3.0d0; ap(6) = 9.0d0
  call dpptrf('U', n, ap, info)
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  call dpptrs('U', n, 1, ap, b, n, info)
  call begin_test('upper_single_rhs')
  call print_array('x', b, 3)
  call print_int('info', info)
  call end_test()

  ! Test 2: lower, single RHS
  ap = 0.0d0; b = 0.0d0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 1.0d0
  ap(4) = 5.0d0; ap(5) = 3.0d0; ap(6) = 9.0d0
  call dpptrf('L', n, ap, info)
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  call dpptrs('L', n, 1, ap, b, n, info)
  call begin_test('lower_single_rhs')
  call print_array('x', b, 3)
  call print_int('info', info)
  call end_test()

  ! Test 3: lower, multiple RHS (NRHS=2)
  ap = 0.0d0; b = 0.0d0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 1.0d0
  ap(4) = 5.0d0; ap(5) = 3.0d0; ap(6) = 9.0d0
  call dpptrf('L', n, ap, info)
  ! B is 3x2 col-major with LDB=3
  b(1) = 1.0d0; b(2) = 0.0d0; b(3) = 0.0d0
  b(4) = 0.0d0; b(5) = 1.0d0; b(6) = 0.0d0
  call dpptrs('L', n, 2, ap, b, n, info)
  call begin_test('lower_multi_rhs')
  call print_array('x', b, 6)
  call print_int('info', info)
  call end_test()

  ! Test 4: upper, multiple RHS (NRHS=3), compute inverse
  ap = 0.0d0; b = 0.0d0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 5.0d0
  ap(4) = 1.0d0; ap(5) = 3.0d0; ap(6) = 9.0d0
  call dpptrf('U', n, ap, info)
  ! B = 3x3 identity
  b(1) = 1.0d0; b(2) = 0.0d0; b(3) = 0.0d0
  b(4) = 0.0d0; b(5) = 1.0d0; b(6) = 0.0d0
  b(7) = 0.0d0; b(8) = 0.0d0; b(9) = 1.0d0
  call dpptrs('U', n, 3, ap, b, n, info)
  call begin_test('upper_multi_rhs_3')
  call print_array('x', b, 9)
  call print_int('info', info)
  call end_test()

  ! Test 5: N=0 quick return
  info = -99
  call dpptrs('U', 0, 1, ap, b, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: NRHS=0 quick return
  info = -99
  call dpptrs('L', 3, 0, ap, b, 3, info)
  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

  ! Test 7: 1x1 system
  ! A = [4], packed: AP = [4]
  ! Cholesky: L = [2], so AP_factored = [2]
  ap(1) = 4.0d0
  call dpptrf('L', 1, ap, info)
  b(1) = 6.0d0
  call dpptrs('L', 1, 1, ap, b, 1, info)
  call begin_test('one_by_one')
  call print_array('x', b, 1)
  call print_int('info', info)
  call end_test()

  ! Test 8: 4x4 system, upper, single RHS
  ! A = [10 2 3 1; 2 12 1 4; 3 1 15 2; 1 4 2 20] (diag dominant -> SPD)
  ! Upper packed: col-major upper triangle
  ! col1: 10
  ! col2: 2, 12
  ! col3: 3, 1, 15
  ! col4: 1, 4, 2, 20
  n = 4
  ap = 0.0d0; b = 0.0d0
  ap(1)  = 10.0d0
  ap(2)  = 2.0d0;  ap(3)  = 12.0d0
  ap(4)  = 3.0d0;  ap(5)  = 1.0d0;  ap(6)  = 15.0d0
  ap(7)  = 1.0d0;  ap(8)  = 4.0d0;  ap(9)  = 2.0d0;  ap(10) = 20.0d0
  call dpptrf('U', n, ap, info)
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0; b(4) = 4.0d0
  call dpptrs('U', n, 1, ap, b, n, info)
  call begin_test('upper_4x4')
  call print_array('x', b, 4)
  call print_int('info', info)
  call end_test()

  ! Test 9: 4x4 system, lower, single RHS
  ! Same matrix in lower packed format
  ! col1: 10, 2, 3, 1
  ! col2: 12, 1, 4
  ! col3: 15, 2
  ! col4: 20
  n = 4
  ap = 0.0d0; b = 0.0d0
  ap(1)  = 10.0d0; ap(2)  = 2.0d0;  ap(3)  = 3.0d0;  ap(4)  = 1.0d0
  ap(5)  = 12.0d0; ap(6)  = 1.0d0;  ap(7)  = 4.0d0
  ap(8)  = 15.0d0; ap(9)  = 2.0d0
  ap(10) = 20.0d0
  call dpptrf('L', n, ap, info)
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0; b(4) = 4.0d0
  call dpptrs('L', n, 1, ap, b, n, info)
  call begin_test('lower_4x4')
  call print_array('x', b, 4)
  call print_int('info', info)
  call end_test()

end program
