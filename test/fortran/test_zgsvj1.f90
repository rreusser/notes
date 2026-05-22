program test_zgsvj1
  use test_utils
  implicit none
  ! Pack 2D arrays into 1D blocks of size M*N (so EQUIVALENCE stride matches).
  double precision :: a_r(512), v_r(512), d_r(64), work_r(512)
  complex*16 :: a(256), v(256), d(32), work(256)
  double precision :: sva(32)
  equivalence (a, a_r)
  equivalence (v, v_r)
  equivalence (d, d_r)
  equivalence (work, work_r)
  double precision :: eps, sfmin, tol
  integer :: info, i
  integer :: m, n, n1, mv, lda, ldv

  eps = 2.220446049250313d-16
  sfmin = 2.2250738585072014d-308
  tol = 1.0d-10

  ! Test 1: JOBV='N', M=4, N=3, n1=1 — first column rotated against next two
  m = 4
  n = 3
  n1 = 1
  lda = 4
  ldv = 1
  mv = 0
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.5d0); a(2) = (2.0d0, -0.5d0); a(3) = (3.0d0, 1.0d0); a(4) = (4.0d0, -1.0d0)
  a(5) = (5.0d0, 0.25d0); a(6) = (6.0d0, -0.25d0); a(7) = (7.0d0, 0.75d0); a(8) = (8.0d0, -0.75d0)
  a(9) = (9.0d0, 0.0d0); a(10) = (10.0d0, 0.1d0); a(11) = (11.0d0, -0.2d0); a(12) = (12.0d0, 0.3d0)
  d(1) = (1.0d0, 0.0d0); d(2) = (1.0d0, 0.0d0); d(3) = (1.0d0, 0.0d0)
  do i = 1, n
    sva(i) = sqrt(sum(dble(a((i-1)*lda+1:(i-1)*lda+m))**2 + dimag(a((i-1)*lda+1:(i-1)*lda+m))**2))
  end do
  v = (0.0d0, 0.0d0)
  work = (0.0d0, 0.0d0)
  call zgsvj1('N', m, n, n1, a, lda, d, sva, mv, v, ldv, eps, sfmin, tol, 5, work, m, info)
  call begin_test('novec_4x3_n1_1')
  call print_array('a', a_r, 2*12)
  call print_array('d', d_r, 2*3)
  call print_array('sva', sva, 3)
  call print_int('info', info)
  call end_test()

  ! Test 2: JOBV='V', M=5, N=4, n1=2 — first 2 cols rotated against last 2
  m = 5
  n = 4
  n1 = 2
  lda = 5
  ldv = 4
  mv = 0
  a = (0.0d0, 0.0d0)
  do i = 1, 20
    a(i) = dcmplx(dble(mod(i*7, 11)) - 5.0d0, dble(mod(i*5, 7)) - 3.0d0)
  end do
  d(1:4) = (1.0d0, 0.0d0)
  do i = 1, 4
    sva(i) = sqrt(sum(dble(a((i-1)*5+1:(i-1)*5+5))**2 + dimag(a((i-1)*5+1:(i-1)*5+5))**2))
  end do
  v = (0.0d0, 0.0d0)
  v(1) = (1.0d0, 0.0d0); v(6) = (1.0d0, 0.0d0); v(11) = (1.0d0, 0.0d0); v(16) = (1.0d0, 0.0d0)
  work = (0.0d0, 0.0d0)
  call zgsvj1('V', m, n, n1, a, lda, d, sva, mv, v, ldv, eps, sfmin, tol, 5, work, m, info)
  call begin_test('vec_5x4_n1_2')
  call print_array('a', a_r, 2*20)
  call print_array('v', v_r, 2*16)
  call print_array('d', d_r, 2*4)
  call print_array('sva', sva, 4)
  call print_int('info', info)
  call end_test()

  ! Test 3: JOBV='A', apply, mv=3, n1=1
  m = 4
  n = 3
  n1 = 1
  lda = 4
  ldv = 3
  mv = 3
  a = (0.0d0, 0.0d0)
  a(1)  = (2.0d0, 0.0d0); a(2)  = (1.0d0, 0.2d0); a(3)  = (0.0d0, 0.0d0); a(4)  = (0.0d0, 0.0d0)
  a(5)  = (1.0d0, -0.2d0); a(6)  = (2.0d0, 0.0d0); a(7)  = (1.0d0, 0.1d0); a(8)  = (0.0d0, 0.0d0)
  a(9)  = (0.0d0, 0.0d0); a(10) = (1.0d0, -0.1d0); a(11) = (2.0d0, 0.0d0); a(12) = (1.0d0, 0.3d0)
  d(1) = (1.0d0, 0.0d0); d(2) = (1.0d0, 0.0d0); d(3) = (1.0d0, 0.0d0)
  do i = 1, n
    sva(i) = sqrt(sum(dble(a((i-1)*lda+1:(i-1)*lda+m))**2 + dimag(a((i-1)*lda+1:(i-1)*lda+m))**2))
  end do
  v = (0.0d0, 0.0d0)
  v(1) = (1.0d0, 0.0d0); v(5) = (1.0d0, 0.0d0); v(9) = (1.0d0, 0.0d0)
  work = (0.0d0, 0.0d0)
  call zgsvj1('A', m, n, n1, a, lda, d, sva, mv, v, ldv, eps, sfmin, tol, 3, work, m, info)
  call begin_test('apply_4x3_n1_1')
  call print_array('a', a_r, 2*12)
  call print_array('v', v_r, 2*9)
  call print_array('d', d_r, 2*3)
  call print_array('sva', sva, 3)
  call print_int('info', info)
  call end_test()

  ! Test 4: Wider N=14, n1=5 to exercise nblr=1, nblc=2 column tiling.
  m = 14
  n = 14
  n1 = 5
  lda = 14
  ldv = 14
  mv = 0
  a = (0.0d0, 0.0d0)
  do i = 1, m * n
    a(i) = dcmplx(dble(mod(i*37 + 13, 29)) - 14.0d0 + sin(dble(i)*0.11d0), &
                   cos(dble(i)*0.07d0) - 0.3d0*sin(dble(i)*0.19d0))
  end do
  d(1:n) = (1.0d0, 0.0d0)
  do i = 1, n
    sva(i) = sqrt(sum(dble(a((i-1)*lda+1:(i-1)*lda+m))**2 + dimag(a((i-1)*lda+1:(i-1)*lda+m))**2))
  end do
  v = (0.0d0, 0.0d0)
  do i = 1, n
    v((i-1)*ldv+i) = (1.0d0, 0.0d0)
  end do
  work = (0.0d0, 0.0d0)
  call zgsvj1('V', m, n, n1, a, lda, d, sva, mv, v, ldv, eps, sfmin, tol, 4, work, m, info)
  call begin_test('vec_14x14_block')
  call print_array('a', a_r, 2*m*n)
  call print_array('v', v_r, 2*n*n)
  call print_array('d', d_r, 2*n)
  call print_array('sva', sva, n)
  call print_int('info', info)
  call end_test()

  ! Test 5: JOBV='N', n1=N (no off-diagonal block; nblc=0 → vacuous loop).
  m = 6
  n = 4
  n1 = 4
  lda = 6
  ldv = 1
  mv = 0
  a = (0.0d0, 0.0d0)
  do i = 1, m * n
    a(i) = dcmplx(sin(dble(i) * 0.7d0) + 0.5d0, cos(dble(i) * 0.4d0) - 0.2d0)
  end do
  d(1:n) = (1.0d0, 0.0d0)
  do i = 1, n
    sva(i) = sqrt(sum(dble(a((i-1)*lda+1:(i-1)*lda+m))**2 + dimag(a((i-1)*lda+1:(i-1)*lda+m))**2))
  end do
  v = (0.0d0, 0.0d0)
  work = (0.0d0, 0.0d0)
  call zgsvj1('N', m, n, n1, a, lda, d, sva, mv, v, ldv, eps, sfmin, tol, 3, work, m, info)
  call begin_test('novec_n1_eq_n')
  call print_array('a', a_r, 2*m*n)
  call print_array('d', d_r, 2*n)
  call print_array('sva', sva, n)
  call print_int('info', info)
  call end_test()

  ! Test 6: JOBV='N', n1=0 (no first block; emptsw=0, immediate convergence).
  m = 4
  n = 3
  n1 = 0
  lda = 4
  ldv = 1
  mv = 0
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.5d0); a(2) = (2.0d0, -0.5d0); a(3) = (3.0d0, 1.0d0); a(4) = (4.0d0, -1.0d0)
  a(5) = (5.0d0, 0.25d0); a(6) = (6.0d0, -0.25d0); a(7) = (7.0d0, 0.75d0); a(8) = (8.0d0, -0.75d0)
  a(9) = (9.0d0, 0.0d0); a(10) = (10.0d0, 0.1d0); a(11) = (11.0d0, -0.2d0); a(12) = (12.0d0, 0.3d0)
  d(1) = (1.0d0, 0.0d0); d(2) = (1.0d0, 0.0d0); d(3) = (1.0d0, 0.0d0)
  do i = 1, n
    sva(i) = sqrt(sum(dble(a((i-1)*lda+1:(i-1)*lda+m))**2 + dimag(a((i-1)*lda+1:(i-1)*lda+m))**2))
  end do
  v = (0.0d0, 0.0d0)
  work = (0.0d0, 0.0d0)
  call zgsvj1('N', m, n, n1, a, lda, d, sva, mv, v, ldv, eps, sfmin, tol, 2, work, m, info)
  call begin_test('novec_n1_0')
  call print_array('a', a_r, 2*12)
  call print_array('d', d_r, 2*3)
  call print_array('sva', sva, 3)
  call print_int('info', info)
  call end_test()

end program
