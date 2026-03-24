program test_zpttrs
  use test_utils
  implicit none

  double precision :: d(10)
  complex*16 :: e(10), b(50)
  double precision :: e_r(20), b_r(100)
  equivalence (e, e_r)
  equivalence (b, b_r)
  integer :: info

  ! ---------------------------------------------------------------
  ! Test 1: UPLO='U' (upper, U^H*D*U), N=4, NRHS=1

  d(1) = 4.0d0; d(2) = 3.0d0; d(3) = 2.0d0; d(4) = 5.0d0
  e(1) = dcmplx(0.5d0, 0.1d0)
  e(2) = dcmplx(-0.3d0, 0.2d0)
  e(3) = dcmplx(0.4d0, -0.1d0)

  b(1) = dcmplx(2.0d0, 1.0d0)
  b(2) = dcmplx(3.0d0, -1.0d0)
  b(3) = dcmplx(1.0d0, 2.0d0)
  b(4) = dcmplx(4.0d0, 0.0d0)

  call zpttrs('U', 4, 1, d, e, b, 4, info)

  call begin_test('upper_n4_nrhs1')
  call print_int('info', info)
  call print_array('b', b_r, 8)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 2: UPLO='L' (lower, L*D*L^H), N=4, NRHS=1

  d(1) = 4.0d0; d(2) = 3.0d0; d(3) = 2.0d0; d(4) = 5.0d0
  e(1) = dcmplx(0.5d0, 0.1d0)
  e(2) = dcmplx(-0.3d0, 0.2d0)
  e(3) = dcmplx(0.4d0, -0.1d0)

  b(1) = dcmplx(2.0d0, 1.0d0)
  b(2) = dcmplx(3.0d0, -1.0d0)
  b(3) = dcmplx(1.0d0, 2.0d0)
  b(4) = dcmplx(4.0d0, 0.0d0)

  call zpttrs('L', 4, 1, d, e, b, 4, info)

  call begin_test('lower_n4_nrhs1')
  call print_int('info', info)
  call print_array('b', b_r, 8)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 3: UPLO='U', N=4, NRHS=3 (multiple RHS)

  d(1) = 4.0d0; d(2) = 3.0d0; d(3) = 2.0d0; d(4) = 5.0d0
  e(1) = dcmplx(0.5d0, 0.1d0)
  e(2) = dcmplx(-0.3d0, 0.2d0)
  e(3) = dcmplx(0.4d0, -0.1d0)

  ! Column 1
  b(1) = dcmplx(2.0d0, 1.0d0)
  b(2) = dcmplx(3.0d0, -1.0d0)
  b(3) = dcmplx(1.0d0, 2.0d0)
  b(4) = dcmplx(4.0d0, 0.0d0)
  ! Column 2
  b(5) = dcmplx(1.0d0, 0.0d0)
  b(6) = dcmplx(0.0d0, 1.0d0)
  b(7) = dcmplx(-1.0d0, 0.0d0)
  b(8) = dcmplx(0.0d0, -1.0d0)
  ! Column 3
  b(9)  = dcmplx(5.0d0, 3.0d0)
  b(10) = dcmplx(-2.0d0, 4.0d0)
  b(11) = dcmplx(3.0d0, -1.0d0)
  b(12) = dcmplx(1.0d0, 1.0d0)

  call zpttrs('U', 4, 3, d, e, b, 4, info)

  call begin_test('upper_n4_nrhs3')
  call print_int('info', info)
  call print_array('b', b_r, 24)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 4: UPLO='L', N=4, NRHS=3 (multiple RHS)

  d(1) = 4.0d0; d(2) = 3.0d0; d(3) = 2.0d0; d(4) = 5.0d0
  e(1) = dcmplx(0.5d0, 0.1d0)
  e(2) = dcmplx(-0.3d0, 0.2d0)
  e(3) = dcmplx(0.4d0, -0.1d0)

  ! Column 1
  b(1) = dcmplx(2.0d0, 1.0d0)
  b(2) = dcmplx(3.0d0, -1.0d0)
  b(3) = dcmplx(1.0d0, 2.0d0)
  b(4) = dcmplx(4.0d0, 0.0d0)
  ! Column 2
  b(5) = dcmplx(1.0d0, 0.0d0)
  b(6) = dcmplx(0.0d0, 1.0d0)
  b(7) = dcmplx(-1.0d0, 0.0d0)
  b(8) = dcmplx(0.0d0, -1.0d0)
  ! Column 3
  b(9)  = dcmplx(5.0d0, 3.0d0)
  b(10) = dcmplx(-2.0d0, 4.0d0)
  b(11) = dcmplx(3.0d0, -1.0d0)
  b(12) = dcmplx(1.0d0, 1.0d0)

  call zpttrs('L', 4, 3, d, e, b, 4, info)

  call begin_test('lower_n4_nrhs3')
  call print_int('info', info)
  call print_array('b', b_r, 24)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 5: N=1, NRHS=1

  d(1) = 3.0d0
  b(1) = dcmplx(9.0d0, 6.0d0)

  call zpttrs('U', 1, 1, d, e, b, 1, info)

  call begin_test('n_eq_1')
  call print_int('info', info)
  call print_array('b', b_r, 2)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 6: N=0 (quick return)

  b(1) = dcmplx(42.0d0, 7.0d0)

  call zpttrs('U', 0, 1, d, e, b, 1, info)

  call begin_test('n_eq_0')
  call print_int('info', info)
  call print_array('b', b_r, 2)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 7: NRHS=0 (quick return)

  d(1) = 4.0d0; d(2) = 3.0d0
  e(1) = dcmplx(0.5d0, 0.0d0)
  b(1) = dcmplx(42.0d0, 7.0d0)

  call zpttrs('U', 2, 0, d, e, b, 2, info)

  call begin_test('nrhs_eq_0')
  call print_int('info', info)
  call print_array('b', b_r, 2)
  call end_test()

end program
