program test_dgtrfs
  use test_utils
  implicit none
  double precision :: dl(10), d(10), du(10)
  double precision :: dlf(10), df(10), duf(10), du2(10)
  integer :: ipiv(10)
  double precision :: b(10, 4), x(10, 4)
  double precision :: ferr(4), berr(4)
  double precision :: work(40)
  integer :: iwork(10), info

  ! Test 1: basic 4x4, no-transpose, 1 RHS
  ! Original tridiagonal: dl=[3,1,2], d=[2,4,5,6], du=[-1,-2,-3]
  ! First factor with dgttrf
  dl(1) = 3.0d0; dl(2) = 1.0d0; dl(3) = 2.0d0
  d(1) = 2.0d0; d(2) = 4.0d0; d(3) = 5.0d0; d(4) = 6.0d0
  du(1) = -1.0d0; du(2) = -2.0d0; du(3) = -3.0d0
  ! Copy to factor arrays
  dlf(1) = 3.0d0; dlf(2) = 1.0d0; dlf(3) = 2.0d0
  df(1) = 2.0d0; df(2) = 4.0d0; df(3) = 5.0d0; df(4) = 6.0d0
  duf(1) = -1.0d0; duf(2) = -2.0d0; duf(3) = -3.0d0
  call dgttrf(4, dlf, df, duf, du2, ipiv, info)

  ! RHS: b = A * [1,2,3,4] => compute by hand
  ! row1: 2*1 + (-1)*2 = 0
  ! row2: 3*1 + 4*2 + (-2)*3 = 5
  ! row3: 1*2 + 5*3 + (-3)*4 = 5
  ! row4: 2*3 + 6*4 = 30
  b(1, 1) = 0.0d0; b(2, 1) = 5.0d0; b(3, 1) = 5.0d0; b(4, 1) = 30.0d0

  ! Solve: x should be close to [1,2,3,4]
  x(1, 1) = b(1, 1); x(2, 1) = b(2, 1); x(3, 1) = b(3, 1); x(4, 1) = b(4, 1)
  call dgttrs('N', 4, 1, dlf, df, duf, du2, ipiv, x, 10, info)

  ! Now refine
  call dgtrfs('N', 4, 1, dl, d, du, dlf, df, duf, du2, ipiv, b, 10, x, 10, &
              ferr, berr, work, iwork, info)
  call begin_test('basic_notrans')
  call print_int('info', info)
  call print_array('x', x(1:4, 1), 4)
  call print_array('ferr', ferr(1:1), 1)
  call print_array('berr', berr(1:1), 1)
  call end_test()

  ! Test 2: transpose, 1 RHS
  ! b = A^T * [1,2,3,4]
  ! row1: 2*1 + 3*2 = 8
  ! row2: (-1)*1 + 4*2 + 1*3 = 10
  ! row3: (-2)*2 + 5*3 + 2*4 = 19
  ! row4: (-3)*3 + 6*4 = 15
  b(1, 1) = 8.0d0; b(2, 1) = 10.0d0; b(3, 1) = 19.0d0; b(4, 1) = 15.0d0
  x(1, 1) = b(1, 1); x(2, 1) = b(2, 1); x(3, 1) = b(3, 1); x(4, 1) = b(4, 1)
  call dgttrs('T', 4, 1, dlf, df, duf, du2, ipiv, x, 10, info)
  call dgtrfs('T', 4, 1, dl, d, du, dlf, df, duf, du2, ipiv, b, 10, x, 10, &
              ferr, berr, work, iwork, info)
  call begin_test('basic_trans')
  call print_int('info', info)
  call print_array('x', x(1:4, 1), 4)
  call print_array('ferr', ferr(1:1), 1)
  call print_array('berr', berr(1:1), 1)
  call end_test()

  ! Test 3: multiple RHS, no-transpose
  b(1, 1) = 0.0d0; b(2, 1) = 5.0d0; b(3, 1) = 5.0d0; b(4, 1) = 30.0d0
  ! Second rhs: A * [2, 0, 1, 3] = [2*2+(-1)*0, 3*2+4*0+(-2)*1, 1*0+5*1+(-3)*3, 2*1+6*3]
  ! = [4, 4, -4, 20]
  b(1, 2) = 4.0d0; b(2, 2) = 4.0d0; b(3, 2) = -4.0d0; b(4, 2) = 20.0d0

  x(1:4, 1) = b(1:4, 1); x(1:4, 2) = b(1:4, 2)
  call dgttrs('N', 4, 2, dlf, df, duf, du2, ipiv, x, 10, info)
  call dgtrfs('N', 4, 2, dl, d, du, dlf, df, duf, du2, ipiv, b, 10, x, 10, &
              ferr, berr, work, iwork, info)
  call begin_test('multi_rhs_notrans')
  call print_int('info', info)
  call print_array('x1', x(1:4, 1), 4)
  call print_array('x2', x(1:4, 2), 4)
  call print_array('ferr', ferr(1:2), 2)
  call print_array('berr', berr(1:2), 2)
  call end_test()

  ! Test 4: N=1
  d(1) = 5.0d0
  dl(1) = 0.0d0; du(1) = 0.0d0
  df(1) = 5.0d0; dlf(1) = 0.0d0; duf(1) = 0.0d0
  du2(1) = 0.0d0; ipiv(1) = 1
  b(1, 1) = 10.0d0
  x(1, 1) = 2.0d0
  call dgtrfs('N', 1, 1, dl, d, du, dlf, df, duf, du2, ipiv, b, 10, x, 10, &
              ferr, berr, work, iwork, info)
  call begin_test('n_one')
  call print_int('info', info)
  call print_array('x', x(1:1, 1), 1)
  call print_array('ferr', ferr(1:1), 1)
  call print_array('berr', berr(1:1), 1)
  call end_test()

  ! Test 5: N=0 (quick return)
  call dgtrfs('N', 0, 1, dl, d, du, dlf, df, duf, du2, ipiv, b, 10, x, 10, &
              ferr, berr, work, iwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: 5x5 with pivoting, no-transpose
  dl(1) = 5.0d0; dl(2) = 7.0d0; dl(3) = 9.0d0; dl(4) = 2.0d0
  d(1) = 1.0d0; d(2) = 3.0d0; d(3) = 2.0d0; d(4) = 1.0d0; d(5) = 8.0d0
  du(1) = 2.0d0; du(2) = 4.0d0; du(3) = 6.0d0; du(4) = 3.0d0
  dlf(1:4) = dl(1:4); df(1:5) = d(1:5); duf(1:4) = du(1:4)
  call dgttrf(5, dlf, df, duf, du2, ipiv, info)

  ! b = A * [1,1,1,1,1]
  ! row1: 1*1 + 2*1 = 3
  ! row2: 5*1 + 3*1 + 4*1 = 12
  ! row3: 7*1 + 2*1 + 6*1 = 15
  ! row4: 9*1 + 1*1 + 3*1 = 13
  ! row5: 2*1 + 8*1 = 10
  b(1, 1) = 3.0d0; b(2, 1) = 12.0d0; b(3, 1) = 15.0d0
  b(4, 1) = 13.0d0; b(5, 1) = 10.0d0
  x(1:5, 1) = b(1:5, 1)
  call dgttrs('N', 5, 1, dlf, df, duf, du2, ipiv, x, 10, info)
  call dgtrfs('N', 5, 1, dl, d, du, dlf, df, duf, du2, ipiv, b, 10, x, 10, &
              ferr, berr, work, iwork, info)
  call begin_test('pivot_5x5_notrans')
  call print_int('info', info)
  call print_array('x', x(1:5, 1), 5)
  call print_array('ferr', ferr(1:1), 1)
  call print_array('berr', berr(1:1), 1)
  call end_test()

end program
