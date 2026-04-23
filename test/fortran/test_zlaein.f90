program test_zlaein
  use test_utils
  implicit none
  integer, parameter :: NMAX = 5
  complex*16 :: H(NMAX, NMAX), B(NMAX, NMAX), v(NMAX)
  complex*16 :: w
  double precision :: H_r(2*NMAX*NMAX), B_r(2*NMAX*NMAX), v_r(2*NMAX)
  double precision :: rwork(NMAX)
  double precision :: eps3, smlnum, eps
  double precision :: dlamch
  external dlamch
  integer :: info, n
  logical :: rightv, noinit
  equivalence (H, H_r)
  equivalence (B, B_r)
  equivalence (v, v_r)

  eps = dlamch('P')
  smlnum = dlamch('S') / eps
  ! Use a generous eps3 to encourage convergence in tests (mimics hnorm*ulp
  ! scaled up, so inverse iteration converges even when w is only an
  ! approximate eigenvalue).
  eps3 = 1.0d-4

  ! Test 1: 3x3 upper Hessenberg, right eigenvector, noinit=T
  n = 3
  H = (0.0d0, 0.0d0)
  H(1,1) = (2.0d0, 1.0d0)
  H(1,2) = (1.0d0, 0.5d0)
  H(1,3) = (0.5d0, 0.0d0)
  H(2,1) = (0.1d0, 0.0d0)
  H(2,2) = (3.0d0, 0.0d0)
  H(2,3) = (1.0d0, -1.0d0)
  H(3,2) = (0.05d0, 0.0d0)
  H(3,3) = (4.0d0, -1.0d0)
  w = (3.9d0, -0.95d0)
  rightv = .true.
  noinit = .true.
  v = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  rwork = 0.0d0

  call zlaein(rightv, noinit, n, H, NMAX, w, v, B, NMAX, rwork, eps3, smlnum, info)
  call begin_test('right_noinit_3x3')
  call print_array('v', v_r, 2*n)
  call print_int('info', info)
  call end_test()

  ! Test 2: 3x3, left eigenvector, noinit=T
  n = 3
  H = (0.0d0, 0.0d0)
  H(1,1) = (2.0d0, 1.0d0)
  H(1,2) = (1.0d0, 0.5d0)
  H(1,3) = (0.5d0, 0.0d0)
  H(2,1) = (0.1d0, 0.0d0)
  H(2,2) = (3.0d0, 0.0d0)
  H(2,3) = (1.0d0, -1.0d0)
  H(3,2) = (0.05d0, 0.0d0)
  H(3,3) = (4.0d0, -1.0d0)
  w = (3.9d0, -0.95d0)
  rightv = .false.
  noinit = .true.
  v = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  rwork = 0.0d0

  call zlaein(rightv, noinit, n, H, NMAX, w, v, B, NMAX, rwork, eps3, smlnum, info)
  call begin_test('left_noinit_3x3')
  call print_array('v', v_r, 2*n)
  call print_int('info', info)
  call end_test()

  ! Test 3: 3x3, right, with initial vector
  n = 3
  H = (0.0d0, 0.0d0)
  H(1,1) = (2.0d0, 1.0d0)
  H(1,2) = (1.0d0, 0.5d0)
  H(1,3) = (0.5d0, 0.0d0)
  H(2,1) = (0.1d0, 0.0d0)
  H(2,2) = (3.0d0, 0.0d0)
  H(2,3) = (1.0d0, -1.0d0)
  H(3,2) = (0.05d0, 0.0d0)
  H(3,3) = (4.0d0, -1.0d0)
  w = (3.9d0, -0.95d0)
  rightv = .true.
  noinit = .false.
  v(1) = (0.1d0, 0.0d0)
  v(2) = (0.2d0, 0.1d0)
  v(3) = (0.9d0, -0.2d0)
  B = (0.0d0, 0.0d0)
  rwork = 0.0d0

  call zlaein(rightv, noinit, n, H, NMAX, w, v, B, NMAX, rwork, eps3, smlnum, info)
  call begin_test('right_init_3x3')
  call print_array('v', v_r, 2*n)
  call print_int('info', info)
  call end_test()

  ! Test 4: 4x4 right eigenvector, noinit=T
  n = 4
  H = (0.0d0, 0.0d0)
  H(1,1) = (1.0d0, 0.5d0)
  H(1,2) = (0.5d0, 0.0d0)
  H(1,3) = (0.2d0, 0.1d0)
  H(1,4) = (0.1d0, 0.0d0)
  H(2,1) = (0.3d0, 0.0d0)
  H(2,2) = (2.0d0, -0.5d0)
  H(2,3) = (0.8d0, 0.2d0)
  H(2,4) = (0.3d0, 0.1d0)
  H(3,2) = (0.2d0, 0.0d0)
  H(3,3) = (3.0d0, 1.0d0)
  H(3,4) = (0.7d0, -0.3d0)
  H(4,3) = (0.15d0, 0.0d0)
  H(4,4) = (4.0d0, 0.5d0)
  w = (3.95d0, 0.45d0)
  rightv = .true.
  noinit = .true.
  v = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  rwork = 0.0d0

  call zlaein(rightv, noinit, n, H, NMAX, w, v, B, NMAX, rwork, eps3, smlnum, info)
  call begin_test('right_noinit_4x4')
  call print_array('v', v_r, 2*n)
  call print_int('info', info)
  call end_test()

  ! Test 5: 4x4 left eigenvector, noinit=T
  n = 4
  H = (0.0d0, 0.0d0)
  H(1,1) = (1.0d0, 0.5d0)
  H(1,2) = (0.5d0, 0.0d0)
  H(1,3) = (0.2d0, 0.1d0)
  H(1,4) = (0.1d0, 0.0d0)
  H(2,1) = (0.3d0, 0.0d0)
  H(2,2) = (2.0d0, -0.5d0)
  H(2,3) = (0.8d0, 0.2d0)
  H(2,4) = (0.3d0, 0.1d0)
  H(3,2) = (0.2d0, 0.0d0)
  H(3,3) = (3.0d0, 1.0d0)
  H(3,4) = (0.7d0, -0.3d0)
  H(4,3) = (0.15d0, 0.0d0)
  H(4,4) = (4.0d0, 0.5d0)
  w = (3.95d0, 0.45d0)
  rightv = .false.
  noinit = .true.
  v = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  rwork = 0.0d0

  call zlaein(rightv, noinit, n, H, NMAX, w, v, B, NMAX, rwork, eps3, smlnum, info)
  call begin_test('left_noinit_4x4')
  call print_array('v', v_r, 2*n)
  call print_int('info', info)
  call end_test()

  ! Test 6: 2x2, right
  n = 2
  H = (0.0d0, 0.0d0)
  H(1,1) = (1.0d0, 0.5d0)
  H(1,2) = (0.7d0, -0.2d0)
  H(2,1) = (0.3d0, 0.0d0)
  H(2,2) = (2.5d0, 0.3d0)
  w = (2.45d0, 0.28d0)
  rightv = .true.
  noinit = .true.
  v = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  rwork = 0.0d0

  call zlaein(rightv, noinit, n, H, NMAX, w, v, B, NMAX, rwork, eps3, smlnum, info)
  call begin_test('right_noinit_2x2')
  call print_array('v', v_r, 2*n)
  call print_int('info', info)
  call end_test()

  ! Test 7: 3x3 diagonal (w near exact eigenvalue) - should converge
  n = 3
  H = (0.0d0, 0.0d0)
  H(1,1) = (1.0d0, 0.0d0)
  H(2,2) = (2.0d0, 0.0d0)
  H(3,3) = (3.0d0, 0.0d0)
  ! Add small subdiag
  H(2,1) = (0.001d0, 0.0d0)
  H(3,2) = (0.001d0, 0.0d0)
  ! Small off-diag entries
  H(1,2) = (0.01d0, 0.0d0)
  H(1,3) = (0.01d0, 0.0d0)
  H(2,3) = (0.01d0, 0.0d0)
  w = (2.0001d0, 0.0d0)
  rightv = .true.
  noinit = .true.
  v = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  rwork = 0.0d0
  call zlaein(rightv, noinit, n, H, NMAX, w, v, B, NMAX, rwork, eps3, smlnum, info)
  call begin_test('right_near_eig_diag')
  call print_array('v', v_r, 2*n)
  call print_int('info', info)
  call end_test()

  ! Test 8: same but left
  n = 3
  H = (0.0d0, 0.0d0)
  H(1,1) = (1.0d0, 0.0d0)
  H(2,2) = (2.0d0, 0.0d0)
  H(3,3) = (3.0d0, 0.0d0)
  H(2,1) = (0.001d0, 0.0d0)
  H(3,2) = (0.001d0, 0.0d0)
  H(1,2) = (0.01d0, 0.0d0)
  H(1,3) = (0.01d0, 0.0d0)
  H(2,3) = (0.01d0, 0.0d0)
  w = (2.0001d0, 0.0d0)
  rightv = .false.
  noinit = .true.
  v = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  rwork = 0.0d0
  call zlaein(rightv, noinit, n, H, NMAX, w, v, B, NMAX, rwork, eps3, smlnum, info)
  call begin_test('left_near_eig_diag')
  call print_array('v', v_r, 2*n)
  call print_int('info', info)
  call end_test()

end program
