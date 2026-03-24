program test_dgtsv
  use test_utils
  implicit none
  double precision :: dl(10), d(10), du(10), b(10, 4)
  integer :: info

  ! Test 1: basic 5x5 tridiagonal, single RHS
  ! Matrix:
  !   2  -1   0   0   0
  !  -1   2  -1   0   0
  !   0  -1   2  -1   0
  !   0   0  -1   2  -1
  !   0   0   0  -1   2
  dl(1:4) = (/-1.0d0, -1.0d0, -1.0d0, -1.0d0/)
  d(1:5)  = (/2.0d0, 2.0d0, 2.0d0, 2.0d0, 2.0d0/)
  du(1:4) = (/-1.0d0, -1.0d0, -1.0d0, -1.0d0/)
  b(1:5, 1) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0/)
  call dgtsv(5, 1, dl, d, du, b, 10, info)
  call begin_test('basic_5x5_single_rhs')
  call print_array('d', d, 5)
  call print_array('dl', dl, 4)
  call print_array('du', du, 4)
  call print_array('b', b(1:5, 1), 5)
  call print_int('info', info)
  call end_test()

  ! Test 2: 4x4 with 2 RHS
  dl(1:3) = (/1.0d0, 1.0d0, 1.0d0/)
  d(1:4)  = (/3.0d0, 3.0d0, 3.0d0, 3.0d0/)
  du(1:3) = (/1.0d0, 1.0d0, 1.0d0/)
  b(1:4, 1) = (/1.0d0, 0.0d0, 0.0d0, 1.0d0/)
  b(1:4, 2) = (/2.0d0, 1.0d0, 1.0d0, 2.0d0/)
  call dgtsv(4, 2, dl, d, du, b, 10, info)
  call begin_test('multi_rhs')
  call print_array('d', d, 4)
  call print_array('dl', dl, 3)
  call print_array('du', du, 3)
  call print_array('b1', b(1:4, 1), 4)
  call print_array('b2', b(1:4, 2), 4)
  call print_int('info', info)
  call end_test()

  ! Test 3: n=1
  d(1) = 5.0d0
  b(1, 1) = 10.0d0
  call dgtsv(1, 1, dl, d, du, b, 10, info)
  call begin_test('n_one')
  call print_array('d', d, 1)
  call print_array('b', b(1:1, 1), 1)
  call print_int('info', info)
  call end_test()

  ! Test 4: n=0 (quick return)
  call dgtsv(0, 1, dl, d, du, b, 10, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 5: singular matrix (zero diagonal element)
  ! Matrix:
  !   0   1   0
  !   0   2   1
  !   0   0   3
  ! D(1)=0 so should return INFO=1
  dl(1:2) = (/0.0d0, 0.0d0/)
  d(1:3)  = (/0.0d0, 2.0d0, 3.0d0/)
  du(1:2) = (/1.0d0, 1.0d0/)
  b(1:3, 1) = (/1.0d0, 2.0d0, 3.0d0/)
  call dgtsv(3, 1, dl, d, du, b, 10, info)
  call begin_test('singular')
  call print_int('info', info)
  call end_test()

  ! Test 6: pivoting case — subdiag larger than diagonal forces row swaps
  ! Matrix:
  !   1  2   0   0
  !   5  3   4   0
  !   0  7   2   6
  !   0  0   9   1
  dl(1:3) = (/5.0d0, 7.0d0, 9.0d0/)
  d(1:4)  = (/1.0d0, 3.0d0, 2.0d0, 1.0d0/)
  du(1:3) = (/2.0d0, 4.0d0, 6.0d0/)
  b(1:4, 1) = (/5.0d0, 12.0d0, 15.0d0, 10.0d0/)
  call dgtsv(4, 1, dl, d, du, b, 10, info)
  call begin_test('pivoting')
  call print_array('d', d, 4)
  call print_array('dl', dl, 3)
  call print_array('du', du, 3)
  call print_array('b', b(1:4, 1), 4)
  call print_int('info', info)
  call end_test()

  ! Test 7: 3 RHS (exercises the nrhs > 2 code path)
  dl(1:2) = (/1.0d0, 1.0d0/)
  d(1:3)  = (/4.0d0, 4.0d0, 4.0d0/)
  du(1:2) = (/1.0d0, 1.0d0/)
  b(1:3, 1) = (/6.0d0, 9.0d0, 9.0d0/)
  b(1:3, 2) = (/1.0d0, 2.0d0, 3.0d0/)
  b(1:3, 3) = (/10.0d0, 5.0d0, 10.0d0/)
  call dgtsv(3, 3, dl, d, du, b, 10, info)
  call begin_test('three_rhs')
  call print_array('d', d, 3)
  call print_array('dl', dl, 2)
  call print_array('du', du, 2)
  call print_array('b1', b(1:3, 1), 3)
  call print_array('b2', b(1:3, 2), 3)
  call print_array('b3', b(1:3, 3), 3)
  call print_int('info', info)
  call end_test()

  ! Test 8: pivoting with multiple RHS
  dl(1:3) = (/5.0d0, 7.0d0, 9.0d0/)
  d(1:4)  = (/1.0d0, 3.0d0, 2.0d0, 1.0d0/)
  du(1:3) = (/2.0d0, 4.0d0, 6.0d0/)
  b(1:4, 1) = (/5.0d0, 12.0d0, 15.0d0, 10.0d0/)
  b(1:4, 2) = (/3.0d0, 7.0d0, 9.0d0, 10.0d0/)
  call dgtsv(4, 2, dl, d, du, b, 10, info)
  call begin_test('pivoting_multi_rhs')
  call print_array('d', d, 4)
  call print_array('dl', dl, 3)
  call print_array('du', du, 3)
  call print_array('b1', b(1:4, 1), 4)
  call print_array('b2', b(1:4, 2), 4)
  call print_int('info', info)
  call end_test()

end program
