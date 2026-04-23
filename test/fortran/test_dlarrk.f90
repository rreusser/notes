program test_dlarrk
  use test_utils
  implicit none
  double precision :: d(10), e2(10), pivmin, reltol, gl, gu, w, werr
  integer :: n, iw, info

  pivmin = 1.0d-300
  reltol = 1.0d-12

  ! Test 1: N=0 (quick return)
  n = 0
  call dlarrk(n, 1, -1.0d0, 1.0d0, d, e2, pivmin, reltol, w, werr, info)
  call begin_test('n0_quick')
  call print_int('info', info)
  call print_scalar('w', w)
  call print_scalar('werr', werr)
  call end_test()

  ! Test 2: N=1, diagonal matrix T=[2.0], eigenvalue is 2.0
  n = 1
  d(1) = 2.0d0
  gl = 0.0d0
  gu = 4.0d0
  call dlarrk(n, 1, gl, gu, d, e2, pivmin, reltol, w, werr, info)
  call begin_test('n1')
  call print_int('info', info)
  call print_scalar('w', w)
  call print_scalar('werr', werr)
  call end_test()

  ! Test 3: N=2, T = [[1,1],[1,4]], eigenvalues ~0.697 and 4.303
  ! E2(1) = 1.0
  n = 2
  d(1) = 1.0d0
  d(2) = 4.0d0
  e2(1) = 1.0d0
  gl = 0.0d0
  gu = 5.0d0
  call dlarrk(n, 1, gl, gu, d, e2, pivmin, reltol, w, werr, info)
  call begin_test('n2_iw1')
  call print_int('info', info)
  call print_scalar('w', w)
  call print_scalar('werr', werr)
  call end_test()

  call dlarrk(n, 2, gl, gu, d, e2, pivmin, reltol, w, werr, info)
  call begin_test('n2_iw2')
  call print_int('info', info)
  call print_scalar('w', w)
  call print_scalar('werr', werr)
  call end_test()

  ! Test 4: N=5, T with d = [4, 3, 2, 1, 5], e = [1, 1, 1, 1], so e2 = [1,1,1,1]
  n = 5
  d(1) = 4.0d0
  d(2) = 3.0d0
  d(3) = 2.0d0
  d(4) = 1.0d0
  d(5) = 5.0d0
  e2(1) = 1.0d0
  e2(2) = 1.0d0
  e2(3) = 1.0d0
  e2(4) = 1.0d0
  gl = -10.0d0
  gu = 10.0d0
  call dlarrk(n, 1, gl, gu, d, e2, pivmin, reltol, w, werr, info)
  call begin_test('n5_iw1')
  call print_int('info', info)
  call print_scalar('w', w)
  call print_scalar('werr', werr)
  call end_test()

  call dlarrk(n, 3, gl, gu, d, e2, pivmin, reltol, w, werr, info)
  call begin_test('n5_iw3')
  call print_int('info', info)
  call print_scalar('w', w)
  call print_scalar('werr', werr)
  call end_test()

  call dlarrk(n, 5, gl, gu, d, e2, pivmin, reltol, w, werr, info)
  call begin_test('n5_iw5')
  call print_int('info', info)
  call print_scalar('w', w)
  call print_scalar('werr', werr)
  call end_test()

  ! Test 5: N=4 negative eigenvalues, d=[-5,-3,-7,-1], e=[0.5,0.5,0.5]
  n = 4
  d(1) = -5.0d0
  d(2) = -3.0d0
  d(3) = -7.0d0
  d(4) = -1.0d0
  e2(1) = 0.25d0
  e2(2) = 0.25d0
  e2(3) = 0.25d0
  gl = -10.0d0
  gu = 0.0d0
  call dlarrk(n, 2, gl, gu, d, e2, pivmin, reltol, w, werr, info)
  call begin_test('n4_neg_iw2')
  call print_int('info', info)
  call print_scalar('w', w)
  call print_scalar('werr', werr)
  call end_test()

  ! Test 6: N=3, diagonal, eigenvalues exactly at 1, 2, 3
  n = 3
  d(1) = 1.0d0
  d(2) = 2.0d0
  d(3) = 3.0d0
  e2(1) = 0.0d0
  e2(2) = 0.0d0
  gl = 0.0d0
  gu = 4.0d0
  call dlarrk(n, 2, gl, gu, d, e2, pivmin, reltol, w, werr, info)
  call begin_test('n3_diag_iw2')
  call print_int('info', info)
  call print_scalar('w', w)
  call print_scalar('werr', werr)
  call end_test()

  ! Test 7: N=1, loose tolerance exercising small itmax
  n = 1
  d(1) = 1.0d0
  gl = -1.0d0
  gu = 3.0d0
  call dlarrk(n, 1, gl, gu, d, e2, 1.0d-16, 1.0d-2, w, werr, info)
  call begin_test('n1_loose')
  call print_int('info', info)
  call print_scalar('w', w)
  call print_scalar('werr', werr)
  call end_test()

end program
