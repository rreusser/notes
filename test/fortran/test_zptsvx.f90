program test_zptsvx
  use test_utils
  implicit none
  integer, parameter :: MAXN = 20
  double precision :: d(MAXN), df(MAXN)
  complex*16 :: e(MAXN), ef(MAXN)
  complex*16 :: b(MAXN*5), x(MAXN*5), work(MAXN)
  double precision :: e_r(2*MAXN), ef_r(2*MAXN)
  double precision :: b_r(2*MAXN*5), x_r(2*MAXN*5), work_r(2*MAXN)
  equivalence (e, e_r)
  equivalence (ef, ef_r)
  equivalence (b, b_r)
  equivalence (x, x_r)
  equivalence (work, work_r)
  double precision :: ferr(5), berr(5), rwork(MAXN), rcond
  integer :: info, n, nrhs

  ! Test 1: FACT='N', N=4, NRHS=1
  ! Hermitian positive definite tridiagonal:
  ! D = [4, 5, 6, 7], E = [(1+0.5i), (0.5-0.3i), (0.2+0.1i)]
  ! b = A * [1+i, 2-i, 1+0.5i, -0.5+i]
  n = 4; nrhs = 1
  d = 0.0d0; e = (0.0d0, 0.0d0); df = 0.0d0; ef = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  d(1) = 4.0d0; d(2) = 5.0d0; d(3) = 6.0d0; d(4) = 7.0d0
  e(1) = dcmplx(1.0d0, 0.5d0)
  e(2) = dcmplx(0.5d0, -0.3d0)
  e(3) = dcmplx(0.2d0, 0.1d0)
  ! b = A * x_true where x_true = (1+i, 2-i, 1+0.5i, -0.5+i)
  ! A(1,1)=4, A(1,2)=e(1), A(2,1)=conj(e(1)), etc.
  b(1) = d(1)*dcmplx(1.0d0, 1.0d0) + e(1)*dcmplx(2.0d0, -1.0d0)
  b(2) = dconjg(e(1))*dcmplx(1.0d0, 1.0d0) + d(2)*dcmplx(2.0d0, -1.0d0) &
       + e(2)*dcmplx(1.0d0, 0.5d0)
  b(3) = dconjg(e(2))*dcmplx(2.0d0, -1.0d0) + d(3)*dcmplx(1.0d0, 0.5d0) &
       + e(3)*dcmplx(-0.5d0, 1.0d0)
  b(4) = dconjg(e(3))*dcmplx(1.0d0, 0.5d0) + d(4)*dcmplx(-0.5d0, 1.0d0)
  call zptsvx('N', n, nrhs, d, e, df, ef, b, n, x, n, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('fact_n_4x4')
  call print_array('x', x_r, 2*n)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_array('df', df, n)
  call print_array('ef', ef_r, 2*(n-1))
  call end_test()

  ! Test 2: FACT='N', N=3, NRHS=1
  n = 3; nrhs = 1
  d = 0.0d0; e = (0.0d0, 0.0d0); df = 0.0d0; ef = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  d(1) = 10.0d0; d(2) = 10.0d0; d(3) = 10.0d0
  e(1) = dcmplx(1.0d0, 1.0d0)
  e(2) = dcmplx(1.0d0, -1.0d0)
  ! b = A * [1+i, 2, 3-i]
  b(1) = d(1)*dcmplx(1.0d0, 1.0d0) + e(1)*dcmplx(2.0d0, 0.0d0)
  b(2) = dconjg(e(1))*dcmplx(1.0d0, 1.0d0) + d(2)*dcmplx(2.0d0, 0.0d0) &
       + e(2)*dcmplx(3.0d0, -1.0d0)
  b(3) = dconjg(e(2))*dcmplx(2.0d0, 0.0d0) + d(3)*dcmplx(3.0d0, -1.0d0)
  call zptsvx('N', n, nrhs, d, e, df, ef, b, n, x, n, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('fact_n_3x3')
  call print_array('x', x_r, 2*n)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call end_test()

  ! Test 3: FACT='F' (pre-factored), N=4, NRHS=1
  n = 4; nrhs = 1
  d = 0.0d0; e = (0.0d0, 0.0d0); df = 0.0d0; ef = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  d(1) = 4.0d0; d(2) = 5.0d0; d(3) = 6.0d0; d(4) = 7.0d0
  e(1) = dcmplx(1.0d0, 0.5d0)
  e(2) = dcmplx(0.5d0, -0.3d0)
  e(3) = dcmplx(0.2d0, 0.1d0)
  ! Pre-factor
  df(1:n) = d(1:n)
  ef(1:n-1) = e(1:n-1)
  call zpttrf(n, df, ef, info)
  ! Compute b = A * [1+i, 2-i, 1+0.5i, -0.5+i]
  b(1) = d(1)*dcmplx(1.0d0, 1.0d0) + e(1)*dcmplx(2.0d0, -1.0d0)
  b(2) = dconjg(e(1))*dcmplx(1.0d0, 1.0d0) + d(2)*dcmplx(2.0d0, -1.0d0) &
       + e(2)*dcmplx(1.0d0, 0.5d0)
  b(3) = dconjg(e(2))*dcmplx(2.0d0, -1.0d0) + d(3)*dcmplx(1.0d0, 0.5d0) &
       + e(3)*dcmplx(-0.5d0, 1.0d0)
  b(4) = dconjg(e(3))*dcmplx(1.0d0, 0.5d0) + d(4)*dcmplx(-0.5d0, 1.0d0)
  call zptsvx('F', n, nrhs, d, e, df, ef, b, n, x, n, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('fact_f_4x4')
  call print_array('x', x_r, 2*n)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call end_test()

  ! Test 4: N=0 quick return
  d = 0.0d0; e = (0.0d0, 0.0d0); df = 0.0d0; ef = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  call zptsvx('N', 0, 1, d, e, df, ef, b, 1, x, 1, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 5: N=1, single element
  n = 1; nrhs = 1
  d = 0.0d0; e = (0.0d0, 0.0d0); df = 0.0d0; ef = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  d(1) = 4.0d0
  b(1) = dcmplx(8.0d0, 4.0d0)
  call zptsvx('N', n, nrhs, d, e, df, ef, b, n, x, n, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('n_one')
  call print_array('x', x_r, 2)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call end_test()

  ! Test 6: Not positive definite (D(2) < 0)
  n = 3; nrhs = 1
  d = 0.0d0; e = (0.0d0, 0.0d0); df = 0.0d0; ef = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  d(1) = 4.0d0; d(2) = -1.0d0; d(3) = 6.0d0
  e(1) = dcmplx(1.0d0, 0.0d0)
  e(2) = dcmplx(2.0d0, 0.0d0)
  b(1) = dcmplx(1.0d0, 0.0d0)
  b(2) = dcmplx(1.0d0, 0.0d0)
  b(3) = dcmplx(1.0d0, 0.0d0)
  call zptsvx('N', n, nrhs, d, e, df, ef, b, n, x, n, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('not_posdef')
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call end_test()

  ! Test 7: Multi-RHS (2 right-hand sides), N=3
  n = 3; nrhs = 2
  d = 0.0d0; e = (0.0d0, 0.0d0); df = 0.0d0; ef = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  d(1) = 10.0d0; d(2) = 10.0d0; d(3) = 10.0d0
  e(1) = dcmplx(1.0d0, 1.0d0)
  e(2) = dcmplx(1.0d0, -1.0d0)
  ! b(:,1) = A * [1+i, 1, 1-i]
  b(1) = d(1)*dcmplx(1.0d0, 1.0d0) + e(1)*dcmplx(1.0d0, 0.0d0)
  b(2) = dconjg(e(1))*dcmplx(1.0d0, 1.0d0) + d(2)*dcmplx(1.0d0, 0.0d0) &
       + e(2)*dcmplx(1.0d0, -1.0d0)
  b(3) = dconjg(e(2))*dcmplx(1.0d0, 0.0d0) + d(3)*dcmplx(1.0d0, -1.0d0)
  ! b(:,2) = A * [2, 3+i, 4-2i]
  b(4) = d(1)*dcmplx(2.0d0, 0.0d0) + e(1)*dcmplx(3.0d0, 1.0d0)
  b(5) = dconjg(e(1))*dcmplx(2.0d0, 0.0d0) + d(2)*dcmplx(3.0d0, 1.0d0) &
       + e(2)*dcmplx(4.0d0, -2.0d0)
  b(6) = dconjg(e(2))*dcmplx(3.0d0, 1.0d0) + d(3)*dcmplx(4.0d0, -2.0d0)
  call zptsvx('N', n, nrhs, d, e, df, ef, b, n, x, n, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('multi_rhs')
  call print_array('x', x_r, 2*n*nrhs)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call end_test()

  ! Test 8: N=5, NRHS=1, larger system
  n = 5; nrhs = 1
  d = 0.0d0; e = (0.0d0, 0.0d0); df = 0.0d0; ef = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  d(1) = 10.0d0; d(2) = 20.0d0; d(3) = 30.0d0; d(4) = 20.0d0; d(5) = 10.0d0
  e(1) = dcmplx(1.0d0, 0.5d0)
  e(2) = dcmplx(2.0d0, -1.0d0)
  e(3) = dcmplx(1.5d0, 0.5d0)
  e(4) = dcmplx(0.5d0, -0.5d0)
  ! b = A * [1, 1+i, 2, 1-i, 1]
  b(1) = d(1)*dcmplx(1.0d0, 0.0d0) + e(1)*dcmplx(1.0d0, 1.0d0)
  b(2) = dconjg(e(1))*dcmplx(1.0d0, 0.0d0) + d(2)*dcmplx(1.0d0, 1.0d0) &
       + e(2)*dcmplx(2.0d0, 0.0d0)
  b(3) = dconjg(e(2))*dcmplx(1.0d0, 1.0d0) + d(3)*dcmplx(2.0d0, 0.0d0) &
       + e(3)*dcmplx(1.0d0, -1.0d0)
  b(4) = dconjg(e(3))*dcmplx(2.0d0, 0.0d0) + d(4)*dcmplx(1.0d0, -1.0d0) &
       + e(4)*dcmplx(1.0d0, 0.0d0)
  b(5) = dconjg(e(4))*dcmplx(1.0d0, -1.0d0) + d(5)*dcmplx(1.0d0, 0.0d0)
  call zptsvx('N', n, nrhs, d, e, df, ef, b, n, x, n, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('n5_nrhs1')
  call print_array('x', x_r, 2*n)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call end_test()

  ! Test 9: N=2, NRHS=1
  n = 2; nrhs = 1
  d = 0.0d0; e = (0.0d0, 0.0d0); df = 0.0d0; ef = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
  d(1) = 4.0d0; d(2) = 5.0d0
  e(1) = dcmplx(1.0d0, 1.0d0)
  ! b = A * [1+i, 2-i]
  b(1) = d(1)*dcmplx(1.0d0, 1.0d0) + e(1)*dcmplx(2.0d0, -1.0d0)
  b(2) = dconjg(e(1))*dcmplx(1.0d0, 1.0d0) + d(2)*dcmplx(2.0d0, -1.0d0)
  call zptsvx('N', n, nrhs, d, e, df, ef, b, n, x, n, &
              rcond, ferr, berr, work, rwork, info)
  call begin_test('n2_nrhs1')
  call print_array('x', x_r, 2*n)
  call print_int('info', info)
  call print_scalar('rcond', rcond)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call end_test()

end program
