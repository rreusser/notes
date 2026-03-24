program test_zptts2
  use test_utils
  implicit none

  double precision :: d(10)
  complex*16 :: e(10), b(50)
  double precision :: e_r(20), b_r(100)
  equivalence (e, e_r)
  equivalence (b, b_r)
  integer :: i

  ! ---------------------------------------------------------------
  ! Test 1: IUPLO=0 (lower, L*D*L^H), N=4, NRHS=1
  !
  ! Use a factored tridiagonal system. D is real diagonal, E is complex
  ! subdiagonal of unit lower bidiagonal L.
  !
  ! D = [4, 3, 2, 5]
  ! E = [(0.5, 0.1), (-0.3, 0.2), (0.4, -0.1)]
  !
  ! We set b = some values and let Fortran solve, then record the result.

  d(1) = 4.0d0; d(2) = 3.0d0; d(3) = 2.0d0; d(4) = 5.0d0
  e(1) = dcmplx(0.5d0, 0.1d0)
  e(2) = dcmplx(-0.3d0, 0.2d0)
  e(3) = dcmplx(0.4d0, -0.1d0)

  ! b = [(2, 1), (3, -1), (1, 2), (4, 0)]
  b(1) = dcmplx(2.0d0, 1.0d0)
  b(2) = dcmplx(3.0d0, -1.0d0)
  b(3) = dcmplx(1.0d0, 2.0d0)
  b(4) = dcmplx(4.0d0, 0.0d0)

  call zptts2(0, 4, 1, d, e, b, 4)

  call begin_test('lower_n4_nrhs1')
  call print_array('d', d, 4)
  call print_array('e', e_r, 6)
  call print_array('b', b_r, 8)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 2: IUPLO=1 (upper, U^H*D*U), N=4, NRHS=1
  !
  ! Same D, E, but with upper factorization.

  d(1) = 4.0d0; d(2) = 3.0d0; d(3) = 2.0d0; d(4) = 5.0d0
  e(1) = dcmplx(0.5d0, 0.1d0)
  e(2) = dcmplx(-0.3d0, 0.2d0)
  e(3) = dcmplx(0.4d0, -0.1d0)

  b(1) = dcmplx(2.0d0, 1.0d0)
  b(2) = dcmplx(3.0d0, -1.0d0)
  b(3) = dcmplx(1.0d0, 2.0d0)
  b(4) = dcmplx(4.0d0, 0.0d0)

  call zptts2(1, 4, 1, d, e, b, 4)

  call begin_test('upper_n4_nrhs1')
  call print_array('d', d, 4)
  call print_array('e', e_r, 6)
  call print_array('b', b_r, 8)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 3: IUPLO=0 (lower), N=4, NRHS=3 (multiple RHS, exercises NRHS>2 path)

  d(1) = 4.0d0; d(2) = 3.0d0; d(3) = 2.0d0; d(4) = 5.0d0
  e(1) = dcmplx(0.5d0, 0.1d0)
  e(2) = dcmplx(-0.3d0, 0.2d0)
  e(3) = dcmplx(0.4d0, -0.1d0)

  ! Column 1 of B
  b(1) = dcmplx(2.0d0, 1.0d0)
  b(2) = dcmplx(3.0d0, -1.0d0)
  b(3) = dcmplx(1.0d0, 2.0d0)
  b(4) = dcmplx(4.0d0, 0.0d0)
  ! Column 2 of B
  b(5) = dcmplx(1.0d0, 0.0d0)
  b(6) = dcmplx(0.0d0, 1.0d0)
  b(7) = dcmplx(-1.0d0, 0.0d0)
  b(8) = dcmplx(0.0d0, -1.0d0)
  ! Column 3 of B
  b(9)  = dcmplx(5.0d0, 3.0d0)
  b(10) = dcmplx(-2.0d0, 4.0d0)
  b(11) = dcmplx(3.0d0, -1.0d0)
  b(12) = dcmplx(1.0d0, 1.0d0)

  call zptts2(0, 4, 3, d, e, b, 4)

  call begin_test('lower_n4_nrhs3')
  call print_array('b', b_r, 24)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 4: IUPLO=1 (upper), N=4, NRHS=3 (multiple RHS, exercises NRHS>2 path)

  d(1) = 4.0d0; d(2) = 3.0d0; d(3) = 2.0d0; d(4) = 5.0d0
  e(1) = dcmplx(0.5d0, 0.1d0)
  e(2) = dcmplx(-0.3d0, 0.2d0)
  e(3) = dcmplx(0.4d0, -0.1d0)

  ! Column 1 of B
  b(1) = dcmplx(2.0d0, 1.0d0)
  b(2) = dcmplx(3.0d0, -1.0d0)
  b(3) = dcmplx(1.0d0, 2.0d0)
  b(4) = dcmplx(4.0d0, 0.0d0)
  ! Column 2 of B
  b(5) = dcmplx(1.0d0, 0.0d0)
  b(6) = dcmplx(0.0d0, 1.0d0)
  b(7) = dcmplx(-1.0d0, 0.0d0)
  b(8) = dcmplx(0.0d0, -1.0d0)
  ! Column 3 of B
  b(9)  = dcmplx(5.0d0, 3.0d0)
  b(10) = dcmplx(-2.0d0, 4.0d0)
  b(11) = dcmplx(3.0d0, -1.0d0)
  b(12) = dcmplx(1.0d0, 1.0d0)

  call zptts2(1, 4, 3, d, e, b, 4)

  call begin_test('upper_n4_nrhs3')
  call print_array('b', b_r, 24)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 5: N=1, NRHS=1
  d(1) = 3.0d0
  b(1) = dcmplx(9.0d0, 6.0d0)

  call zptts2(0, 1, 1, d, e, b, 1)

  call begin_test('n_eq_1')
  call print_array('b', b_r, 2)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 6: N=1, NRHS=2
  d(1) = 4.0d0
  b(1) = dcmplx(8.0d0, 4.0d0)
  b(2) = dcmplx(12.0d0, -8.0d0)

  call zptts2(0, 1, 2, d, e, b, 1)

  call begin_test('n_eq_1_multi_rhs')
  call print_array('b', b_r, 4)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 7: N=0 (quick return)
  b(1) = dcmplx(42.0d0, 7.0d0)

  call zptts2(0, 0, 1, d, e, b, 1)

  call begin_test('n_eq_0')
  call print_array('b', b_r, 2)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 8: IUPLO=0 (lower), N=4, NRHS=2 (exercises NRHS<=2 path with j-loop)

  d(1) = 4.0d0; d(2) = 3.0d0; d(3) = 2.0d0; d(4) = 5.0d0
  e(1) = dcmplx(0.5d0, 0.1d0)
  e(2) = dcmplx(-0.3d0, 0.2d0)
  e(3) = dcmplx(0.4d0, -0.1d0)

  ! Column 1 of B
  b(1) = dcmplx(2.0d0, 1.0d0)
  b(2) = dcmplx(3.0d0, -1.0d0)
  b(3) = dcmplx(1.0d0, 2.0d0)
  b(4) = dcmplx(4.0d0, 0.0d0)
  ! Column 2 of B
  b(5) = dcmplx(1.0d0, 0.0d0)
  b(6) = dcmplx(0.0d0, 1.0d0)
  b(7) = dcmplx(-1.0d0, 0.0d0)
  b(8) = dcmplx(0.0d0, -1.0d0)

  call zptts2(0, 4, 2, d, e, b, 4)

  call begin_test('lower_n4_nrhs2')
  call print_array('b', b_r, 16)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 9: IUPLO=1 (upper), N=4, NRHS=2

  d(1) = 4.0d0; d(2) = 3.0d0; d(3) = 2.0d0; d(4) = 5.0d0
  e(1) = dcmplx(0.5d0, 0.1d0)
  e(2) = dcmplx(-0.3d0, 0.2d0)
  e(3) = dcmplx(0.4d0, -0.1d0)

  ! Column 1 of B
  b(1) = dcmplx(2.0d0, 1.0d0)
  b(2) = dcmplx(3.0d0, -1.0d0)
  b(3) = dcmplx(1.0d0, 2.0d0)
  b(4) = dcmplx(4.0d0, 0.0d0)
  ! Column 2 of B
  b(5) = dcmplx(1.0d0, 0.0d0)
  b(6) = dcmplx(0.0d0, 1.0d0)
  b(7) = dcmplx(-1.0d0, 0.0d0)
  b(8) = dcmplx(0.0d0, -1.0d0)

  call zptts2(1, 4, 2, d, e, b, 4)

  call begin_test('upper_n4_nrhs2')
  call print_array('b', b_r, 16)
  call end_test()

end program
