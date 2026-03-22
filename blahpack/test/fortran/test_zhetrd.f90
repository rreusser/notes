program test_zhetrd
  use test_utils
  implicit none

  integer :: INFO, LWORK
  complex*16 :: A4(4,4), A1(1,1), TAU(35), WORK(1225)
  complex*16 :: A35(35,35)
  double precision :: A4_r(32), A1_r(2), TAU_r(70)
  double precision :: A35_r(2450), TAU35_r(68)
  double precision :: D(35), E(35)
  equivalence (A4, A4_r)
  equivalence (A1, A1_r)
  equivalence (TAU, TAU_r)
  equivalence (A35, A35_r)
  integer :: i, j

  ! Test 1: UPLO='U', 4x4 Hermitian matrix (uses unblocked path)
  A4(1,1) = dcmplx(4.0d0, 0.0d0)
  A4(1,2) = dcmplx(1.0d0, 1.0d0)
  A4(1,3) = dcmplx(2.0d0, -1.0d0)
  A4(1,4) = dcmplx(0.5d0, 0.5d0)
  A4(2,1) = dcmplx(1.0d0, -1.0d0)
  A4(2,2) = dcmplx(5.0d0, 0.0d0)
  A4(2,3) = dcmplx(1.0d0, 2.0d0)
  A4(2,4) = dcmplx(1.0d0, -1.0d0)
  A4(3,1) = dcmplx(2.0d0, 1.0d0)
  A4(3,2) = dcmplx(1.0d0, -2.0d0)
  A4(3,3) = dcmplx(6.0d0, 0.0d0)
  A4(3,4) = dcmplx(2.0d0, 1.0d0)
  A4(4,1) = dcmplx(0.5d0, -0.5d0)
  A4(4,2) = dcmplx(1.0d0, 1.0d0)
  A4(4,3) = dcmplx(2.0d0, -1.0d0)
  A4(4,4) = dcmplx(7.0d0, 0.0d0)

  D = 0.0d0; E = 0.0d0; TAU = dcmplx(0.0d0, 0.0d0)
  LWORK = 1225
  call ZHETRD('U', 4, A4, 4, D, E, TAU, WORK, LWORK, INFO)
  call begin_test('upper_4x4')
  call print_int('info', INFO)
  call print_array('A', A4_r, 32)
  call print_array('d', D, 4)
  call print_array('e', E, 3)
  call print_array('tau', TAU_r, 6)
  call end_test()

  ! Test 2: UPLO='L', 4x4 same Hermitian matrix
  A4(1,1) = dcmplx(4.0d0, 0.0d0)
  A4(1,2) = dcmplx(1.0d0, 1.0d0)
  A4(1,3) = dcmplx(2.0d0, -1.0d0)
  A4(1,4) = dcmplx(0.5d0, 0.5d0)
  A4(2,1) = dcmplx(1.0d0, -1.0d0)
  A4(2,2) = dcmplx(5.0d0, 0.0d0)
  A4(2,3) = dcmplx(1.0d0, 2.0d0)
  A4(2,4) = dcmplx(1.0d0, -1.0d0)
  A4(3,1) = dcmplx(2.0d0, 1.0d0)
  A4(3,2) = dcmplx(1.0d0, -2.0d0)
  A4(3,3) = dcmplx(6.0d0, 0.0d0)
  A4(3,4) = dcmplx(2.0d0, 1.0d0)
  A4(4,1) = dcmplx(0.5d0, -0.5d0)
  A4(4,2) = dcmplx(1.0d0, 1.0d0)
  A4(4,3) = dcmplx(2.0d0, -1.0d0)
  A4(4,4) = dcmplx(7.0d0, 0.0d0)

  D = 0.0d0; E = 0.0d0; TAU = dcmplx(0.0d0, 0.0d0)
  call ZHETRD('L', 4, A4, 4, D, E, TAU, WORK, LWORK, INFO)
  call begin_test('lower_4x4')
  call print_int('info', INFO)
  call print_array('A', A4_r, 32)
  call print_array('d', D, 4)
  call print_array('e', E, 3)
  call print_array('tau', TAU_r, 6)
  call end_test()

  ! Test 3: N=1
  A1(1,1) = dcmplx(3.0d0, 0.0d0)
  D(1) = 0.0d0
  call ZHETRD('U', 1, A1, 1, D, E, TAU, WORK, LWORK, INFO)
  call begin_test('n_one')
  call print_int('info', INFO)
  call print_scalar('d1', D(1))
  call end_test()

  ! Test 4: N=0
  call ZHETRD('U', 0, A1, 1, D, E, TAU, WORK, LWORK, INFO)
  call begin_test('n_zero')
  call print_int('info', INFO)
  call end_test()

  ! Test 5: UPLO='U', 35x35 Hermitian (blocked path, NB=32 or ILAENV)
  ! Build diagonally dominant Hermitian matrix
  do j = 1, 35
    do i = 1, 35
      if (i == j) then
        A35(i,j) = dcmplx(dble(35 + i), 0.0d0)
      else if (i < j) then
        A35(i,j) = dcmplx(1.0d0/dble(j-i+1), 0.5d0/dble(j-i+1))
        A35(j,i) = dcmplx(1.0d0/dble(j-i+1), -0.5d0/dble(j-i+1))
      end if
    end do
  end do
  D = 0.0d0; E = 0.0d0; TAU = dcmplx(0.0d0, 0.0d0)
  call ZHETRD('U', 35, A35, 35, D, E, TAU, WORK, LWORK, INFO)
  call begin_test('upper_35x35')
  call print_int('info', INFO)
  call print_array('A', A35_r, 2450)
  call print_array('d', D, 35)
  call print_array('e', E, 34)
  call end_test()

  ! Test 6: UPLO='L', 35x35 Hermitian (blocked path)
  do j = 1, 35
    do i = 1, 35
      if (i == j) then
        A35(i,j) = dcmplx(dble(35 + i), 0.0d0)
      else if (i < j) then
        A35(i,j) = dcmplx(1.0d0/dble(j-i+1), 0.5d0/dble(j-i+1))
        A35(j,i) = dcmplx(1.0d0/dble(j-i+1), -0.5d0/dble(j-i+1))
      end if
    end do
  end do
  D = 0.0d0; E = 0.0d0; TAU = dcmplx(0.0d0, 0.0d0)
  call ZHETRD('L', 35, A35, 35, D, E, TAU, WORK, LWORK, INFO)
  call begin_test('lower_35x35')
  call print_int('info', INFO)
  call print_array('A', A35_r, 2450)
  call print_array('d', D, 35)
  call print_array('e', E, 34)
  call end_test()

end program
