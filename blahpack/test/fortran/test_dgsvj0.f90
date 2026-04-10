program test_dgsvj0
  use test_utils
  implicit none
  double precision :: a(128), v(128), d(16), sva(16), work(128)
  double precision :: eps, sfmin, tol
  integer :: info, i
  integer :: m, n, mv, lda, ldv

  eps = 2.220446049250313d-16
  sfmin = 2.2250738585072014d-308
  tol = 1.0d-10

  ! Test 1: JOBV='N', M=4, N=3, identity-ish
  m = 4
  n = 3
  lda = 4
  ldv = 1
  mv = 0
  a = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0; a(4) = 4.0d0
  a(5) = 5.0d0; a(6) = 6.0d0; a(7) = 7.0d0; a(8) = 8.0d0
  a(9) = 9.0d0; a(10) = 10.0d0; a(11) = 11.0d0; a(12) = 12.0d0
  d(1) = 1.0d0; d(2) = 1.0d0; d(3) = 1.0d0
  ! Initial sva = column norms * d(p)
  sva(1) = sqrt(1.0d0*1+4.0d0+9.0d0+16.0d0)
  sva(2) = sqrt(25.0d0+36.0d0+49.0d0+64.0d0)
  sva(3) = sqrt(81.0d0+100.0d0+121.0d0+144.0d0)
  v = 0.0d0
  work = 0.0d0
  call dgsvj0('N', m, n, a, lda, d, sva, mv, v, ldv, eps, sfmin, tol, 5, work, m, info)
  call begin_test('novec_4x3')
  call print_array('a', a, 12)
  call print_array('d', d, 3)
  call print_array('sva', sva, 3)
  call print_int('info', info)
  call end_test()

  ! Test 2: JOBV='V', M=5, N=4
  m = 5
  n = 4
  lda = 5
  ldv = 4
  mv = 0
  a = 0.0d0
  do i = 1, 20
    a(i) = dble(mod(i*7, 11)) - 5.0d0
  end do
  d(1:4) = 1.0d0
  do i = 1, 4
    sva(i) = sqrt(a((i-1)*5+1)**2 + a((i-1)*5+2)**2 + a((i-1)*5+3)**2 + a((i-1)*5+4)**2 + a((i-1)*5+5)**2)
  end do
  v = 0.0d0
  v(1) = 1.0d0; v(6) = 1.0d0; v(11) = 1.0d0; v(16) = 1.0d0
  work = 0.0d0
  call dgsvj0('V', m, n, a, lda, d, sva, mv, v, ldv, eps, sfmin, tol, 5, work, m, info)
  call begin_test('vec_5x4')
  call print_array('a', a, 20)
  call print_array('v', v, 16)
  call print_array('d', d, 4)
  call print_array('sva', sva, 4)
  call print_int('info', info)
  call end_test()

  ! Test 3: JOBV='A', apply V with mv=3
  m = 4
  n = 3
  lda = 4
  ldv = 3
  mv = 3
  a = 0.0d0
  a(1) = 2.0d0; a(2) = 1.0d0; a(3) = 0.0d0; a(4) = 0.0d0
  a(5) = 1.0d0; a(6) = 2.0d0; a(7) = 1.0d0; a(8) = 0.0d0
  a(9) = 0.0d0; a(10) = 1.0d0; a(11) = 2.0d0; a(12) = 1.0d0
  d(1) = 1.0d0; d(2) = 1.0d0; d(3) = 1.0d0
  sva(1) = sqrt(5.0d0)
  sva(2) = sqrt(6.0d0)
  sva(3) = sqrt(6.0d0)
  v = 0.0d0
  v(1) = 1.0d0; v(5) = 1.0d0; v(9) = 1.0d0  ! identity 3x3
  work = 0.0d0
  call dgsvj0('A', m, n, a, lda, d, sva, mv, v, ldv, eps, sfmin, tol, 3, work, m, info)
  call begin_test('apply_4x3')
  call print_array('a', a, 12)
  call print_array('v', v, 9)
  call print_array('d', d, 3)
  call print_array('sva', sva, 3)
  call print_int('info', info)
  call end_test()

  ! Test 4: N=1 edge case, JOBV='N'
  m = 3
  n = 1
  lda = 3
  ldv = 1
  mv = 0
  a = 0.0d0
  a(1) = 3.0d0; a(2) = 4.0d0; a(3) = 0.0d0
  d(1) = 1.0d0
  sva(1) = 5.0d0
  v = 0.0d0
  work = 0.0d0
  call dgsvj0('N', m, n, a, lda, d, sva, mv, v, ldv, eps, sfmin, tol, 2, work, m, info)
  call begin_test('novec_n1')
  call print_array('a', a, 3)
  call print_array('d', d, 1)
  call print_array('sva', sva, 1)
  call print_int('info', info)
  call end_test()

  ! Test 5: Wider N > KBL (KBL=8, so N=9 triggers block structure)
  m = 10
  n = 9
  lda = 10
  ldv = 9
  mv = 0
  a = 0.0d0
  do i = 1, 90
    a(i) = dble(mod(i*37 + 13, 29)) - 14.0d0 + sin(dble(i)*0.11d0)
  end do
  d(1:9) = 1.0d0
  do i = 1, 9
    sva(i) = sqrt(sum(a((i-1)*10+1:(i-1)*10+10)**2))
  end do
  v = 0.0d0
  do i = 1, 9
    v((i-1)*9+i) = 1.0d0
  end do
  work = 0.0d0
  call dgsvj0('V', m, n, a, lda, d, sva, mv, v, ldv, eps, sfmin, tol, 4, work, m, info)
  call begin_test('vec_10x9_block')
  call print_array('a', a, 90)
  call print_array('v', v, 81)
  call print_array('d', d, 9)
  call print_array('sva', sva, 9)
  call print_int('info', info)
  call end_test()

end program
