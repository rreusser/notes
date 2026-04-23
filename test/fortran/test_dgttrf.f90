program test_dgttrf
  use test_utils
  implicit none
  double precision :: dl(10), d(10), du(10), du2(10)
  integer :: ipiv(10), info

  ! ============================================================
  ! Test 1: 5x5 tridiagonal
  ! A = [2  -1   0   0   0]
  !     [-1   2  -1   0   0]
  !     [ 0  -1   2  -1   0]
  !     [ 0   0  -1   2  -1]
  !     [ 0   0   0  -1   2]
  dl(1) = -1.0d0; dl(2) = -1.0d0; dl(3) = -1.0d0; dl(4) = -1.0d0
  d(1) = 2.0d0; d(2) = 2.0d0; d(3) = 2.0d0; d(4) = 2.0d0; d(5) = 2.0d0
  du(1) = -1.0d0; du(2) = -1.0d0; du(3) = -1.0d0; du(4) = -1.0d0
  du2 = 0.0d0
  ipiv = 0
  call DGTTRF(5, dl, d, du, du2, ipiv, info)
  call begin_test('basic_5x5')
  call print_array('dl', dl, 4)
  call print_array('d', d, 5)
  call print_array('du', du, 4)
  call print_array('du2', du2, 3)
  call print_int_array('ipiv', ipiv, 5)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 2: N=1
  d(1) = 5.0d0
  ipiv = 0
  call DGTTRF(1, dl, d, du, du2, ipiv, info)
  call begin_test('n_one')
  call print_array('d', d, 1)
  call print_int_array('ipiv', ipiv, 1)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 3: N=2
  dl(1) = 3.0d0
  d(1) = 4.0d0; d(2) = 7.0d0
  du(1) = 1.0d0
  du2 = 0.0d0
  ipiv = 0
  call DGTTRF(2, dl, d, du, du2, ipiv, info)
  call begin_test('n_two')
  call print_array('dl', dl, 1)
  call print_array('d', d, 2)
  call print_array('du', du, 1)
  call print_int_array('ipiv', ipiv, 2)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 4: N=2 with pivoting (|dl| > |d|)
  dl(1) = 5.0d0
  d(1) = 2.0d0; d(2) = 3.0d0
  du(1) = 1.0d0
  du2 = 0.0d0
  ipiv = 0
  call DGTTRF(2, dl, d, du, du2, ipiv, info)
  call begin_test('n_two_pivot')
  call print_array('dl', dl, 1)
  call print_array('d', d, 2)
  call print_array('du', du, 1)
  call print_int_array('ipiv', ipiv, 2)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 5: Singular matrix (zero on diagonal)
  dl(1) = 1.0d0; dl(2) = 1.0d0
  d(1) = 0.0d0; d(2) = 0.0d0; d(3) = 1.0d0
  du(1) = 0.0d0; du(2) = 1.0d0
  du2 = 0.0d0
  ipiv = 0
  call DGTTRF(3, dl, d, du, du2, ipiv, info)
  call begin_test('singular')
  call print_array('dl', dl, 2)
  call print_array('d', d, 3)
  call print_array('du', du, 2)
  call print_array('du2', du2, 1)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 6: N=0 quick return
  call DGTTRF(0, dl, d, du, du2, ipiv, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 7: 5x5 with pivoting forced on multiple rows
  ! Use values where |dl(i)| > |d(i)| to force pivoting
  dl(1) = 10.0d0; dl(2) = 10.0d0; dl(3) = 10.0d0; dl(4) = 10.0d0
  d(1) = 1.0d0; d(2) = 1.0d0; d(3) = 1.0d0; d(4) = 1.0d0; d(5) = 1.0d0
  du(1) = 2.0d0; du(2) = 2.0d0; du(3) = 2.0d0; du(4) = 2.0d0
  du2 = 0.0d0
  ipiv = 0
  call DGTTRF(5, dl, d, du, du2, ipiv, info)
  call begin_test('pivot_5x5')
  call print_array('dl', dl, 4)
  call print_array('d', d, 5)
  call print_array('du', du, 4)
  call print_array('du2', du2, 3)
  call print_int_array('ipiv', ipiv, 5)
  call print_int('info', info)
  call end_test()

end program
