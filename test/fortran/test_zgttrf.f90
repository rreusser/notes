program test_zgttrf
  use test_utils
  implicit none

  integer, parameter :: NMAX = 10
  complex*16 :: DL(NMAX), D(NMAX), DU(NMAX), DU2(NMAX)
  double precision :: DL_r(2*NMAX), D_r(2*NMAX), DU_r(2*NMAX), DU2_r(2*NMAX)
  equivalence (DL, DL_r)
  equivalence (D, D_r)
  equivalence (DU, DU_r)
  equivalence (DU2, DU2_r)
  integer :: IPIV(NMAX), INFO, n

  ! Test 1: N=4, no pivoting needed (diag dominant)
  n = 4
  DL(1) = dcmplx(1.0d0, 0.0d0); DL(2) = dcmplx(1.0d0, 0.5d0); DL(3) = dcmplx(0.5d0, 0.0d0)
  D(1) = dcmplx(4.0d0, 1.0d0); D(2) = dcmplx(5.0d0, -1.0d0); D(3) = dcmplx(6.0d0, 0.0d0); D(4) = dcmplx(3.0d0, 2.0d0)
  DU(1) = dcmplx(2.0d0, 0.0d0); DU(2) = dcmplx(1.0d0, -1.0d0); DU(3) = dcmplx(0.5d0, 0.5d0)
  DU2 = dcmplx(0.0d0, 0.0d0)
  IPIV = 0
  call ZGTTRF(n, DL, D, DU, DU2, IPIV, INFO)
  call begin_test('basic_4')
  call print_int('info', INFO)
  call print_array('DL', DL_r, 2*(n-1))
  call print_array('D', D_r, 2*n)
  call print_array('DU', DU_r, 2*(n-1))
  call print_array('DU2', DU2_r, 2*(n-2))
  call print_int_array('ipiv', IPIV, n)
  call end_test()

  ! Test 2: N=4 with pivoting (small diagonal, large subdiag)
  n = 4
  DL(1) = dcmplx(10.0d0, 5.0d0); DL(2) = dcmplx(8.0d0, 0.0d0); DL(3) = dcmplx(7.0d0, -3.0d0)
  D(1) = dcmplx(0.1d0, 0.0d0); D(2) = dcmplx(0.2d0, 0.1d0); D(3) = dcmplx(0.3d0, 0.0d0); D(4) = dcmplx(0.4d0, -0.1d0)
  DU(1) = dcmplx(1.0d0, 1.0d0); DU(2) = dcmplx(2.0d0, 0.0d0); DU(3) = dcmplx(3.0d0, -1.0d0)
  DU2 = dcmplx(0.0d0, 0.0d0)
  IPIV = 0
  call ZGTTRF(n, DL, D, DU, DU2, IPIV, INFO)
  call begin_test('pivot_4')
  call print_int('info', INFO)
  call print_array('DL', DL_r, 2*(n-1))
  call print_array('D', D_r, 2*n)
  call print_array('DU', DU_r, 2*(n-1))
  call print_array('DU2', DU2_r, 2*(n-2))
  call print_int_array('ipiv', IPIV, n)
  call end_test()

  ! Test 3: N=1
  n = 1
  D(1) = dcmplx(3.0d0, 2.0d0)
  IPIV = 0
  call ZGTTRF(n, DL, D, DU, DU2, IPIV, INFO)
  call begin_test('n1')
  call print_int('info', INFO)
  call print_array('D', D_r, 2*n)
  call print_int_array('ipiv', IPIV, n)
  call end_test()

  ! Test 4: N=2
  n = 2
  DL(1) = dcmplx(3.0d0, 1.0d0)
  D(1) = dcmplx(5.0d0, -1.0d0); D(2) = dcmplx(4.0d0, 2.0d0)
  DU(1) = dcmplx(2.0d0, 0.0d0)
  DU2 = dcmplx(0.0d0, 0.0d0)
  IPIV = 0
  call ZGTTRF(n, DL, D, DU, DU2, IPIV, INFO)
  call begin_test('n2_nopivot')
  call print_int('info', INFO)
  call print_array('DL', DL_r, 2*(n-1))
  call print_array('D', D_r, 2*n)
  call print_array('DU', DU_r, 2*(n-1))
  call print_int_array('ipiv', IPIV, n)
  call end_test()

  ! Test 5: N=2 with pivot
  n = 2
  DL(1) = dcmplx(10.0d0, 5.0d0)
  D(1) = dcmplx(0.1d0, 0.0d0); D(2) = dcmplx(3.0d0, 0.0d0)
  DU(1) = dcmplx(1.0d0, 0.0d0)
  DU2 = dcmplx(0.0d0, 0.0d0)
  IPIV = 0
  call ZGTTRF(n, DL, D, DU, DU2, IPIV, INFO)
  call begin_test('n2_pivot')
  call print_int('info', INFO)
  call print_array('DL', DL_r, 2*(n-1))
  call print_array('D', D_r, 2*n)
  call print_array('DU', DU_r, 2*(n-1))
  call print_int_array('ipiv', IPIV, n)
  call end_test()

  ! Test 6: N=0
  call ZGTTRF(0, DL, D, DU, DU2, IPIV, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call end_test()

  ! Test 7: Singular (zero diagonal)
  n = 3
  DL(1) = dcmplx(0.0d0, 0.0d0); DL(2) = dcmplx(0.0d0, 0.0d0)
  D(1) = dcmplx(0.0d0, 0.0d0); D(2) = dcmplx(0.0d0, 0.0d0); D(3) = dcmplx(0.0d0, 0.0d0)
  DU(1) = dcmplx(0.0d0, 0.0d0); DU(2) = dcmplx(0.0d0, 0.0d0)
  DU2 = dcmplx(0.0d0, 0.0d0)
  IPIV = 0
  call ZGTTRF(n, DL, D, DU, DU2, IPIV, INFO)
  call begin_test('singular')
  call print_int('info', INFO)
  call end_test()

end program
