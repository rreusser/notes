program test_zgtrfs
  use test_utils
  implicit none
  double precision :: dl_r(20), d_r(20), du_r(20)
  double precision :: dlf_r(20), df_r(20), duf_r(20), du2_r(20)
  double precision :: b_r(200), x_r(200), work_r(200)
  complex*16 :: dl(10), d(10), du(10)
  complex*16 :: dlf(10), df(10), duf(10), du2(10)
  complex*16 :: b(10, 4), x(10, 4), work(100)
  equivalence (dl, dl_r)
  equivalence (d, d_r)
  equivalence (du, du_r)
  equivalence (dlf, dlf_r)
  equivalence (df, df_r)
  equivalence (duf, duf_r)
  equivalence (du2, du2_r)
  equivalence (b, b_r)
  equivalence (x, x_r)
  equivalence (work, work_r)
  integer :: ipiv(10), info, n, nrhs
  double precision :: ferr(4), berr(4), rwork(100)

  ! Test 1: TRANS='N', 4x4 complex tridiagonal, 1 RHS
  ! dl = [(2+i), (1-i), (3+0.5i)]
  ! d  = [(4+i), (5+2i), (3+i), (6-i)]
  ! du = [(1+0.5i), (-1+i), (2+i)]
  ! exact x = [(1,0), (1,0), (1,0), (1,0)]
  n = 4; nrhs = 1
  dl(1) = (2.0d0, 1.0d0); dl(2) = (1.0d0, -1.0d0); dl(3) = (3.0d0, 0.5d0)
  d(1) = (4.0d0, 1.0d0); d(2) = (5.0d0, 2.0d0); d(3) = (3.0d0, 1.0d0); d(4) = (6.0d0, -1.0d0)
  du(1) = (1.0d0, 0.5d0); du(2) = (-1.0d0, 1.0d0); du(3) = (2.0d0, 1.0d0)
  ! b = A * [1;1;1;1]:
  ! row1: d(1)*1 + du(1)*1 = (4+i) + (1+0.5i) = (5, 1.5)
  ! row2: dl(1)*1 + d(2)*1 + du(2)*1 = (2+i) + (5+2i) + (-1+i) = (6, 4)
  ! row3: dl(2)*1 + d(3)*1 + du(3)*1 = (1-i) + (3+i) + (2+i) = (6, 1)
  ! row4: dl(3)*1 + d(4)*1 = (3+0.5i) + (6-i) = (9, -0.5)
  b(1, 1) = (5.0d0, 1.5d0)
  b(2, 1) = (6.0d0, 4.0d0)
  b(3, 1) = (6.0d0, 1.0d0)
  b(4, 1) = (9.0d0, -0.5d0)

  ! Factor
  dlf(1:3) = dl(1:3); df(1:4) = d(1:4); duf(1:3) = du(1:3)
  call ZGTTRF(n, dlf, df, duf, du2, ipiv, info)

  ! Solve
  x(1:4, 1) = b(1:4, 1)
  call ZGTTRS('N', n, nrhs, dlf, df, duf, du2, ipiv, x, 10, info)

  ! Refine
  call ZGTRFS('N', n, nrhs, dl, d, du, dlf, df, duf, du2, ipiv, &
              b, 10, x, 10, ferr, berr, work, rwork, info)
  call begin_test('basic_notrans')
  call print_int('info', info)
  call print_array('x', x_r, 2*n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call end_test()

  ! Test 2: TRANS='C' (conjugate transpose), same matrix, 1 RHS
  ! b = A^H * [1;1;1;1]
  ! row1: conj(d(1))*1 + conj(dl(1))*1 = (4-i) + (2-i) = (6, -2)
  ! row2: conj(du(1))*1 + conj(d(2))*1 + conj(dl(2))*1 = (1-0.5i) + (5-2i) + (1+i) = (7, -1.5)
  ! row3: conj(du(2))*1 + conj(d(3))*1 + conj(dl(3))*1 = (-1-i) + (3-i) + (3-0.5i) = (5, -2.5)
  ! row4: conj(du(3))*1 + conj(d(4))*1 = (2-i) + (6+i) = (8, 0)
  n = 4; nrhs = 1
  b(1, 1) = (6.0d0, -2.0d0)
  b(2, 1) = (7.0d0, -1.5d0)
  b(3, 1) = (5.0d0, -2.5d0)
  b(4, 1) = (8.0d0, 0.0d0)
  dlf(1:3) = dl(1:3); df(1:4) = d(1:4); duf(1:3) = du(1:3)
  call ZGTTRF(n, dlf, df, duf, du2, ipiv, info)
  x(1:4, 1) = b(1:4, 1)
  call ZGTTRS('C', n, nrhs, dlf, df, duf, du2, ipiv, x, 10, info)
  call ZGTRFS('C', n, nrhs, dl, d, du, dlf, df, duf, du2, ipiv, &
              b, 10, x, 10, ferr, berr, work, rwork, info)
  call begin_test('basic_conjtrans')
  call print_int('info', info)
  call print_array('x', x_r, 2*n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call end_test()

  ! Test 3: Multiple RHS (nrhs=2), TRANS='N'
  n = 4; nrhs = 2
  ! RHS 1: same as test 1
  b(1, 1) = (5.0d0, 1.5d0)
  b(2, 1) = (6.0d0, 4.0d0)
  b(3, 1) = (6.0d0, 1.0d0)
  b(4, 1) = (9.0d0, -0.5d0)
  ! RHS 2: b = A * [(1+i), (2-i), (0.5+0.5i), (1,0)]
  ! row1: d(1)*(1+i) + du(1)*(2-i)
  !     = (4+i)*(1+i) + (1+0.5i)*(2-i)
  !     = (3+5i) + (2.5+0i) = (5.5, 5)
  ! row2: dl(1)*(1+i) + d(2)*(2-i) + du(2)*(0.5+0.5i)
  !     = (2+i)*(1+i) + (5+2i)*(2-i) + (-1+i)*(0.5+0.5i)
  !     = (1+3i) + (12-i) + (-1+0i) = (12, 2)
  ! row3: dl(2)*(2-i) + d(3)*(0.5+0.5i) + du(3)*(1+0i)
  !     = (1-i)*(2-i) + (3+i)*(0.5+0.5i) + (2+i)
  !     = (1-3i) + (1+2i) + (2+i) = (4, 0)
  ! row4: dl(3)*(0.5+0.5i) + d(4)*(1+0i)
  !     = (3+0.5i)*(0.5+0.5i) + (6-i)
  !     = (1.25+1.75i) + (6-i) = (7.25, 0.75)
  b(1, 2) = (5.5d0, 5.0d0)
  b(2, 2) = (12.0d0, 2.0d0)
  b(3, 2) = (4.0d0, 0.0d0)
  b(4, 2) = (7.25d0, 0.75d0)
  dlf(1:3) = dl(1:3); df(1:4) = d(1:4); duf(1:3) = du(1:3)
  call ZGTTRF(n, dlf, df, duf, du2, ipiv, info)
  x(1:4, 1) = b(1:4, 1); x(1:4, 2) = b(1:4, 2)
  call ZGTTRS('N', n, nrhs, dlf, df, duf, du2, ipiv, x, 10, info)
  call ZGTRFS('N', n, nrhs, dl, d, du, dlf, df, duf, du2, ipiv, &
              b, 10, x, 10, ferr, berr, work, rwork, info)
  call begin_test('multi_rhs_notrans')
  call print_int('info', info)
  ! x is 10x4 complex, column-major. Column j starts at x(1,j).
  ! x_r has 20 doubles per column (10 complex * 2 reals).
  ! So x(:,1) = x_r(1:2*n), x(:,2) = x_r(21:20+2*n)
  call print_array('x1', x_r(1:2*n), 2*n)
  call print_array('x2', x_r(21:20+2*n), 2*n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call end_test()

  ! Test 4: N=1
  n = 1; nrhs = 1
  d(1) = (3.0d0, 2.0d0)
  b(1, 1) = (3.0d0, 2.0d0) ! x = [1+0i]
  df(1) = d(1)
  call ZGTTRF(n, dlf, df, duf, du2, ipiv, info)
  x(1, 1) = b(1, 1)
  call ZGTTRS('N', n, nrhs, dlf, df, duf, du2, ipiv, x, 10, info)
  call ZGTRFS('N', n, nrhs, dl, d, du, dlf, df, duf, du2, ipiv, &
              b, 10, x, 10, ferr, berr, work, rwork, info)
  call begin_test('n_one')
  call print_int('info', info)
  call print_array('x', x_r, 2*n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call end_test()

  ! Test 5: N=0 (quick return)
  call ZGTRFS('N', 0, 1, dl, d, du, dlf, df, duf, du2, ipiv, &
              b, 10, x, 10, ferr, berr, work, rwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: 5x5 with pivoting, TRANS='N'
  n = 5; nrhs = 1
  dl(1) = (5.0d0, 1.0d0); dl(2) = (7.0d0, -2.0d0); dl(3) = (1.0d0, 3.0d0); dl(4) = (2.0d0, 0.5d0)
  d(1) = (1.0d0, 0.5d0); d(2) = (3.0d0, 1.0d0); d(3) = (2.0d0, -1.0d0); d(4) = (1.0d0, 2.0d0); d(5) = (8.0d0, 0.0d0)
  du(1) = (2.0d0, -1.0d0); du(2) = (4.0d0, 0.0d0); du(3) = (6.0d0, 1.0d0); du(4) = (3.0d0, -0.5d0)
  ! b = A * [1;1;1;1;1]
  b(1, 1) = d(1) + du(1)
  b(2, 1) = dl(1) + d(2) + du(2)
  b(3, 1) = dl(2) + d(3) + du(3)
  b(4, 1) = dl(3) + d(4) + du(4)
  b(5, 1) = dl(4) + d(5)
  dlf(1:4) = dl(1:4); df(1:5) = d(1:5); duf(1:4) = du(1:4)
  call ZGTTRF(n, dlf, df, duf, du2, ipiv, info)
  x(1:5, 1) = b(1:5, 1)
  call ZGTTRS('N', n, nrhs, dlf, df, duf, du2, ipiv, x, 10, info)
  call ZGTRFS('N', n, nrhs, dl, d, du, dlf, df, duf, du2, ipiv, &
              b, 10, x, 10, ferr, berr, work, rwork, info)
  call begin_test('pivot_5x5_notrans')
  call print_int('info', info)
  call print_array('x', x_r, 2*n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call end_test()

end program
