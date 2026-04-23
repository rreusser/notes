program test_zptrfs
  use test_utils
  implicit none

  integer, parameter :: MAXN = 10
  double precision :: d(MAXN), df(MAXN)
  complex*16 :: e(MAXN), ef(MAXN), b(MAXN*5), x(MAXN*5), work(MAXN)
  double precision :: e_r(2*MAXN), ef_r(2*MAXN), b_r(2*MAXN*5), x_r(2*MAXN*5), work_r(2*MAXN)
  equivalence (e, e_r)
  equivalence (ef, ef_r)
  equivalence (b, b_r)
  equivalence (x, x_r)
  equivalence (work, work_r)
  double precision :: ferr(5), berr(5), rwork(MAXN)
  integer :: info, n, nrhs, i

  ! ---------------------------------------------------------------
  ! Test 1: UPLO='U', N=5, NRHS=1 (basic)
  n = 5; nrhs = 1

  d(1) = 4.0d0; d(2) = 3.0d0; d(3) = 2.0d0; d(4) = 5.0d0; d(5) = 4.0d0
  e(1) = dcmplx(0.5d0, 0.1d0)
  e(2) = dcmplx(-0.3d0, 0.2d0)
  e(3) = dcmplx(0.4d0, -0.1d0)
  e(4) = dcmplx(-0.2d0, 0.3d0)

  ! Compute b = A*x_true where x_true = (1+i, 2-i, 3+0.5i, -1+2i, 0.5-0.5i)
  ! For Hermitian tridiag with UPLO='U': A(i,i) = d(i), A(i,i+1) = e(i), A(i+1,i) = conj(e(i))
  b(1) = d(1)*dcmplx(1.0d0, 1.0d0) + e(1)*dcmplx(2.0d0, -1.0d0)
  b(2) = dconjg(e(1))*dcmplx(1.0d0, 1.0d0) + d(2)*dcmplx(2.0d0, -1.0d0) + e(2)*dcmplx(3.0d0, 0.5d0)
  b(3) = dconjg(e(2))*dcmplx(2.0d0, -1.0d0) + d(3)*dcmplx(3.0d0, 0.5d0) + e(3)*dcmplx(-1.0d0, 2.0d0)
  b(4) = dconjg(e(3))*dcmplx(3.0d0, 0.5d0) + d(4)*dcmplx(-1.0d0, 2.0d0) + e(4)*dcmplx(0.5d0, -0.5d0)
  b(5) = dconjg(e(4))*dcmplx(-1.0d0, 2.0d0) + d(5)*dcmplx(0.5d0, -0.5d0)

  ! Factor
  df(1:n) = d(1:n)
  ef(1:n-1) = e(1:n-1)
  call zpttrf(n, df, ef, info)

  ! Solve to get x
  x(1:n) = b(1:n)
  call zpttrs('U', n, nrhs, df, ef, x, n, info)

  ! Perturb x slightly
  x(1) = x(1) + dcmplx(1.0d-10, -1.0d-10)
  x(3) = x(3) - dcmplx(1.0d-10, 1.0d-10)

  ! Refine
  call zptrfs('U', n, nrhs, d, e, df, ef, b, n, x, n, ferr, berr, work, rwork, info)

  call begin_test('upper_n5_nrhs1')
  call print_int('info', info)
  call print_array('x', x_r, 2*n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 2: UPLO='L', N=5, NRHS=1 (basic lower)
  n = 5; nrhs = 1

  d(1) = 4.0d0; d(2) = 3.0d0; d(3) = 2.0d0; d(4) = 5.0d0; d(5) = 4.0d0
  e(1) = dcmplx(0.5d0, 0.1d0)
  e(2) = dcmplx(-0.3d0, 0.2d0)
  e(3) = dcmplx(0.4d0, -0.1d0)
  e(4) = dcmplx(-0.2d0, 0.3d0)

  ! Same b and x_true as test 1 (Hermitian, so A is same regardless of UPLO)
  b(1) = d(1)*dcmplx(1.0d0, 1.0d0) + e(1)*dcmplx(2.0d0, -1.0d0)
  b(2) = dconjg(e(1))*dcmplx(1.0d0, 1.0d0) + d(2)*dcmplx(2.0d0, -1.0d0) + e(2)*dcmplx(3.0d0, 0.5d0)
  b(3) = dconjg(e(2))*dcmplx(2.0d0, -1.0d0) + d(3)*dcmplx(3.0d0, 0.5d0) + e(3)*dcmplx(-1.0d0, 2.0d0)
  b(4) = dconjg(e(3))*dcmplx(3.0d0, 0.5d0) + d(4)*dcmplx(-1.0d0, 2.0d0) + e(4)*dcmplx(0.5d0, -0.5d0)
  b(5) = dconjg(e(4))*dcmplx(-1.0d0, 2.0d0) + d(5)*dcmplx(0.5d0, -0.5d0)

  df(1:n) = d(1:n)
  ef(1:n-1) = e(1:n-1)
  call zpttrf(n, df, ef, info)

  x(1:n) = b(1:n)
  call zpttrs('L', n, nrhs, df, ef, x, n, info)

  x(2) = x(2) + dcmplx(1.0d-10, 1.0d-10)
  x(4) = x(4) - dcmplx(1.0d-10, -1.0d-10)

  call zptrfs('L', n, nrhs, d, e, df, ef, b, n, x, n, ferr, berr, work, rwork, info)

  call begin_test('lower_n5_nrhs1')
  call print_int('info', info)
  call print_array('x', x_r, 2*n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 3: UPLO='U', N=4, NRHS=2 (multiple RHS)
  n = 4; nrhs = 2

  d(1) = 5.0d0; d(2) = 4.0d0; d(3) = 3.0d0; d(4) = 6.0d0
  e(1) = dcmplx(1.0d0, 0.5d0)
  e(2) = dcmplx(-0.5d0, 0.3d0)
  e(3) = dcmplx(0.2d0, -0.4d0)

  ! x_true col1 = (1, 2i, -1+i, 3)
  ! x_true col2 = (i, 1-i, 2, -1+0.5i)
  ! b = A*x_true for each column
  ! Column 1:
  b(1) = d(1)*dcmplx(1.0d0, 0.0d0) + e(1)*dcmplx(0.0d0, 2.0d0)
  b(2) = dconjg(e(1))*dcmplx(1.0d0, 0.0d0) + d(2)*dcmplx(0.0d0, 2.0d0) + e(2)*dcmplx(-1.0d0, 1.0d0)
  b(3) = dconjg(e(2))*dcmplx(0.0d0, 2.0d0) + d(3)*dcmplx(-1.0d0, 1.0d0) + e(3)*dcmplx(3.0d0, 0.0d0)
  b(4) = dconjg(e(3))*dcmplx(-1.0d0, 1.0d0) + d(4)*dcmplx(3.0d0, 0.0d0)
  ! Column 2: (stored at b(5..8) column-major LDB=4)
  b(5) = d(1)*dcmplx(0.0d0, 1.0d0) + e(1)*dcmplx(1.0d0, -1.0d0)
  b(6) = dconjg(e(1))*dcmplx(0.0d0, 1.0d0) + d(2)*dcmplx(1.0d0, -1.0d0) + e(2)*dcmplx(2.0d0, 0.0d0)
  b(7) = dconjg(e(2))*dcmplx(1.0d0, -1.0d0) + d(3)*dcmplx(2.0d0, 0.0d0) + e(3)*dcmplx(-1.0d0, 0.5d0)
  b(8) = dconjg(e(3))*dcmplx(2.0d0, 0.0d0) + d(4)*dcmplx(-1.0d0, 0.5d0)

  df(1:n) = d(1:n)
  ef(1:n-1) = e(1:n-1)
  call zpttrf(n, df, ef, info)

  x(1:n*nrhs) = b(1:n*nrhs)
  call zpttrs('U', n, nrhs, df, ef, x, n, info)

  ! Perturb
  x(1) = x(1) + dcmplx(1.0d-10, 0.0d0)
  x(5) = x(5) - dcmplx(0.0d0, 1.0d-10)

  call zptrfs('U', n, nrhs, d, e, df, ef, b, n, x, n, ferr, berr, work, rwork, info)

  call begin_test('upper_n4_nrhs2')
  call print_int('info', info)
  call print_array('x', x_r, 2*n*nrhs)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 4: N=1, NRHS=1
  n = 1; nrhs = 1

  d(1) = 3.0d0
  b(1) = dcmplx(9.0d0, 6.0d0)

  df(1) = d(1)
  call zpttrf(n, df, ef, info)

  x(1) = b(1)
  call zpttrs('U', n, nrhs, df, ef, x, n, info)

  call zptrfs('U', n, nrhs, d, e, df, ef, b, n, x, n, ferr, berr, work, rwork, info)

  call begin_test('n_eq_1')
  call print_int('info', info)
  call print_array('x', x_r, 2)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 5: N=0 (quick return)
  n = 0; nrhs = 1

  call zptrfs('U', n, nrhs, d, e, df, ef, b, 1, x, 1, ferr, berr, work, rwork, info)

  call begin_test('n_eq_0')
  call print_int('info', info)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 6: NRHS=0 (quick return)
  n = 5; nrhs = 0

  call zptrfs('U', n, nrhs, d, e, df, ef, b, n, x, n, ferr, berr, work, rwork, info)

  call begin_test('nrhs_eq_0')
  call print_int('info', info)
  call end_test()

end program
