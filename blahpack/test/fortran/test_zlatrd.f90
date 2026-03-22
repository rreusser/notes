program test_zlatrd
  use test_utils
  implicit none

  complex*16 :: A6(6,6), W6(6,3), TAU(6)
  double precision :: A6_r(72), W6_r(36), TAU_r(12)
  double precision :: E(6)
  equivalence (A6, A6_r)
  equivalence (W6, W6_r)
  equivalence (TAU, TAU_r)
  integer :: i, j

  ! Test 1: UPLO='U', 6x6 Hermitian, NB=3
  ! Build a diagonally dominant Hermitian matrix
  A6(1,1) = dcmplx(10.0d0, 0.0d0)
  A6(1,2) = dcmplx(1.0d0, 2.0d0)
  A6(1,3) = dcmplx(0.5d0, -1.0d0)
  A6(1,4) = dcmplx(0.3d0, 0.4d0)
  A6(1,5) = dcmplx(0.1d0, -0.2d0)
  A6(1,6) = dcmplx(0.2d0, 0.1d0)

  A6(2,1) = dcmplx(1.0d0, -2.0d0)
  A6(2,2) = dcmplx(9.0d0, 0.0d0)
  A6(2,3) = dcmplx(1.5d0, 1.0d0)
  A6(2,4) = dcmplx(0.7d0, -0.3d0)
  A6(2,5) = dcmplx(0.4d0, 0.5d0)
  A6(2,6) = dcmplx(0.3d0, -0.2d0)

  A6(3,1) = dcmplx(0.5d0, 1.0d0)
  A6(3,2) = dcmplx(1.5d0, -1.0d0)
  A6(3,3) = dcmplx(8.0d0, 0.0d0)
  A6(3,4) = dcmplx(2.0d0, 1.5d0)
  A6(3,5) = dcmplx(0.6d0, -0.4d0)
  A6(3,6) = dcmplx(0.5d0, 0.3d0)

  A6(4,1) = dcmplx(0.3d0, -0.4d0)
  A6(4,2) = dcmplx(0.7d0, 0.3d0)
  A6(4,3) = dcmplx(2.0d0, -1.5d0)
  A6(4,4) = dcmplx(7.0d0, 0.0d0)
  A6(4,5) = dcmplx(1.0d0, 0.8d0)
  A6(4,6) = dcmplx(0.4d0, -0.6d0)

  A6(5,1) = dcmplx(0.1d0, 0.2d0)
  A6(5,2) = dcmplx(0.4d0, -0.5d0)
  A6(5,3) = dcmplx(0.6d0, 0.4d0)
  A6(5,4) = dcmplx(1.0d0, -0.8d0)
  A6(5,5) = dcmplx(6.0d0, 0.0d0)
  A6(5,6) = dcmplx(1.5d0, 1.0d0)

  A6(6,1) = dcmplx(0.2d0, -0.1d0)
  A6(6,2) = dcmplx(0.3d0, 0.2d0)
  A6(6,3) = dcmplx(0.5d0, -0.3d0)
  A6(6,4) = dcmplx(0.4d0, 0.6d0)
  A6(6,5) = dcmplx(1.5d0, -1.0d0)
  A6(6,6) = dcmplx(5.0d0, 0.0d0)

  E = 0.0d0
  TAU = dcmplx(0.0d0, 0.0d0)
  W6 = dcmplx(0.0d0, 0.0d0)

  call ZLATRD('U', 6, 3, A6, 6, E, TAU, W6, 6)
  call begin_test('upper_6x6_nb3')
  call print_array('A', A6_r, 72)
  call print_array('e', E, 5)
  call print_array('tau', TAU_r, 10)
  call print_array('W', W6_r, 36)
  call end_test()

  ! Test 2: UPLO='L', 6x6 same Hermitian, NB=3
  A6(1,1) = dcmplx(10.0d0, 0.0d0)
  A6(1,2) = dcmplx(1.0d0, 2.0d0)
  A6(1,3) = dcmplx(0.5d0, -1.0d0)
  A6(1,4) = dcmplx(0.3d0, 0.4d0)
  A6(1,5) = dcmplx(0.1d0, -0.2d0)
  A6(1,6) = dcmplx(0.2d0, 0.1d0)

  A6(2,1) = dcmplx(1.0d0, -2.0d0)
  A6(2,2) = dcmplx(9.0d0, 0.0d0)
  A6(2,3) = dcmplx(1.5d0, 1.0d0)
  A6(2,4) = dcmplx(0.7d0, -0.3d0)
  A6(2,5) = dcmplx(0.4d0, 0.5d0)
  A6(2,6) = dcmplx(0.3d0, -0.2d0)

  A6(3,1) = dcmplx(0.5d0, 1.0d0)
  A6(3,2) = dcmplx(1.5d0, -1.0d0)
  A6(3,3) = dcmplx(8.0d0, 0.0d0)
  A6(3,4) = dcmplx(2.0d0, 1.5d0)
  A6(3,5) = dcmplx(0.6d0, -0.4d0)
  A6(3,6) = dcmplx(0.5d0, 0.3d0)

  A6(4,1) = dcmplx(0.3d0, -0.4d0)
  A6(4,2) = dcmplx(0.7d0, 0.3d0)
  A6(4,3) = dcmplx(2.0d0, -1.5d0)
  A6(4,4) = dcmplx(7.0d0, 0.0d0)
  A6(4,5) = dcmplx(1.0d0, 0.8d0)
  A6(4,6) = dcmplx(0.4d0, -0.6d0)

  A6(5,1) = dcmplx(0.1d0, 0.2d0)
  A6(5,2) = dcmplx(0.4d0, -0.5d0)
  A6(5,3) = dcmplx(0.6d0, 0.4d0)
  A6(5,4) = dcmplx(1.0d0, -0.8d0)
  A6(5,5) = dcmplx(6.0d0, 0.0d0)
  A6(5,6) = dcmplx(1.5d0, 1.0d0)

  A6(6,1) = dcmplx(0.2d0, -0.1d0)
  A6(6,2) = dcmplx(0.3d0, 0.2d0)
  A6(6,3) = dcmplx(0.5d0, -0.3d0)
  A6(6,4) = dcmplx(0.4d0, 0.6d0)
  A6(6,5) = dcmplx(1.5d0, -1.0d0)
  A6(6,6) = dcmplx(5.0d0, 0.0d0)

  E = 0.0d0
  TAU = dcmplx(0.0d0, 0.0d0)
  W6 = dcmplx(0.0d0, 0.0d0)

  call ZLATRD('L', 6, 3, A6, 6, E, TAU, W6, 6)
  call begin_test('lower_6x6_nb3')
  call print_array('A', A6_r, 72)
  call print_array('e', E, 5)
  call print_array('tau', TAU_r, 10)
  call print_array('W', W6_r, 36)
  call end_test()

end program
