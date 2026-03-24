program test_dlagtm
  use test_utils
  implicit none
  double precision :: dl(10), d(10), du(10), x(10, 4), b(10, 4)

  ! Test 1: no-transpose, alpha=1, beta=0, N=4, NRHS=1
  ! A = [2  -1   0   0]
  !     [3   4  -2   0]
  !     [0   1   5  -3]
  !     [0   0   2   6]
  dl(1) = 3.0d0; dl(2) = 1.0d0; dl(3) = 2.0d0
  d(1) = 2.0d0; d(2) = 4.0d0; d(3) = 5.0d0; d(4) = 6.0d0
  du(1) = -1.0d0; du(2) = -2.0d0; du(3) = -3.0d0
  x(1, 1) = 1.0d0; x(2, 1) = 2.0d0; x(3, 1) = 3.0d0; x(4, 1) = 4.0d0
  b = 0.0d0
  call dlagtm('N', 4, 1, 1.0d0, dl, d, du, x, 10, 0.0d0, b, 10)
  call begin_test('notrans_alpha1_beta0')
  call print_array('b', b(1:4, 1), 4)
  call end_test()

  ! Test 2: transpose, alpha=1, beta=0, N=4, NRHS=1
  b = 0.0d0
  call dlagtm('T', 4, 1, 1.0d0, dl, d, du, x, 10, 0.0d0, b, 10)
  call begin_test('trans_alpha1_beta0')
  call print_array('b', b(1:4, 1), 4)
  call end_test()

  ! Test 3: no-transpose, alpha=-1, beta=0, N=4, NRHS=1
  b = 0.0d0
  call dlagtm('N', 4, 1, -1.0d0, dl, d, du, x, 10, 0.0d0, b, 10)
  call begin_test('notrans_alpham1_beta0')
  call print_array('b', b(1:4, 1), 4)
  call end_test()

  ! Test 4: transpose, alpha=-1, beta=0, N=4, NRHS=1
  b = 0.0d0
  call dlagtm('T', 4, 1, -1.0d0, dl, d, du, x, 10, 0.0d0, b, 10)
  call begin_test('trans_alpham1_beta0')
  call print_array('b', b(1:4, 1), 4)
  call end_test()

  ! Test 5: no-transpose, alpha=1, beta=1 (accumulate into B)
  b(1:4, 1) = (/10.0d0, 20.0d0, 30.0d0, 40.0d0/)
  call dlagtm('N', 4, 1, 1.0d0, dl, d, du, x, 10, 1.0d0, b, 10)
  call begin_test('notrans_alpha1_beta1')
  call print_array('b', b(1:4, 1), 4)
  call end_test()

  ! Test 6: no-transpose, alpha=1, beta=-1 (negate B first)
  b(1:4, 1) = (/10.0d0, 20.0d0, 30.0d0, 40.0d0/)
  call dlagtm('N', 4, 1, 1.0d0, dl, d, du, x, 10, -1.0d0, b, 10)
  call begin_test('notrans_alpha1_betam1')
  call print_array('b', b(1:4, 1), 4)
  call end_test()

  ! Test 7: multiple RHS, no-transpose, alpha=1, beta=0
  x(1, 1) = 1.0d0; x(2, 1) = 0.0d0; x(3, 1) = 0.0d0; x(4, 1) = 1.0d0
  x(1, 2) = 2.0d0; x(2, 2) = 3.0d0; x(3, 2) = 4.0d0; x(4, 2) = 5.0d0
  b = 0.0d0
  call dlagtm('N', 4, 2, 1.0d0, dl, d, du, x, 10, 0.0d0, b, 10)
  call begin_test('notrans_multi_rhs')
  call print_array('b1', b(1:4, 1), 4)
  call print_array('b2', b(1:4, 2), 4)
  call end_test()

  ! Test 8: N=1
  d(1) = 5.0d0
  x(1, 1) = 3.0d0
  b = 0.0d0
  call dlagtm('N', 1, 1, 1.0d0, dl, d, du, x, 10, 0.0d0, b, 10)
  call begin_test('n_one')
  call print_array('b', b(1:1, 1), 1)
  call end_test()

  ! Test 9: N=1 transpose
  b = 0.0d0
  call dlagtm('T', 1, 1, 1.0d0, dl, d, du, x, 10, 0.0d0, b, 10)
  call begin_test('n_one_trans')
  call print_array('b', b(1:1, 1), 1)
  call end_test()

  ! Test 10: N=0 (quick return)
  b(1:4, 1) = (/99.0d0, 99.0d0, 99.0d0, 99.0d0/)
  call dlagtm('N', 0, 1, 1.0d0, dl, d, du, x, 10, 0.0d0, b, 10)
  call begin_test('n_zero')
  call print_array('b', b(1:4, 1), 4)
  call end_test()

  ! Test 11: transpose, multiple RHS
  dl(1) = 3.0d0; dl(2) = 1.0d0; dl(3) = 2.0d0
  d(1) = 2.0d0; d(2) = 4.0d0; d(3) = 5.0d0; d(4) = 6.0d0
  du(1) = -1.0d0; du(2) = -2.0d0; du(3) = -3.0d0
  x(1, 1) = 1.0d0; x(2, 1) = 2.0d0; x(3, 1) = 3.0d0; x(4, 1) = 4.0d0
  x(1, 2) = 5.0d0; x(2, 2) = 6.0d0; x(3, 2) = 7.0d0; x(4, 2) = 8.0d0
  b = 0.0d0
  call dlagtm('T', 4, 2, 1.0d0, dl, d, du, x, 10, 0.0d0, b, 10)
  call begin_test('trans_multi_rhs')
  call print_array('b1', b(1:4, 1), 4)
  call print_array('b2', b(1:4, 2), 4)
  call end_test()

  ! Test 12: alpha=-1, transpose, multiple RHS, beta=-1
  b(1:4, 1) = (/1.0d0, 1.0d0, 1.0d0, 1.0d0/)
  b(1:4, 2) = (/2.0d0, 2.0d0, 2.0d0, 2.0d0/)
  call dlagtm('T', 4, 2, -1.0d0, dl, d, du, x, 10, -1.0d0, b, 10)
  call begin_test('trans_alpham1_betam1_multi_rhs')
  call print_array('b1', b(1:4, 1), 4)
  call print_array('b2', b(1:4, 2), 4)
  call end_test()

end program
