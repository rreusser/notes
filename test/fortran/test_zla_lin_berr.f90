program test_zla_lin_berr
  use test_utils
  implicit none

  ! Case 1 storage: n=4, nrhs=2
  complex*16 :: res1(4, 2)
  double precision :: res1_r(2*4*2)
  equivalence (res1, res1_r)
  double precision :: ayb1(4, 2)
  double precision :: berr1(2)

  ! Case 2 storage: real-only residual, n=3, nrhs=2
  complex*16 :: res2(3, 2)
  double precision :: res2_r(2*3*2)
  equivalence (res2, res2_r)
  double precision :: ayb2(3, 2)
  double precision :: berr2(2)

  ! Case 3 storage: n=1, nrhs=1
  complex*16 :: res3(1, 1)
  double precision :: res3_r(2)
  equivalence (res3, res3_r)
  double precision :: ayb3(1, 1)
  double precision :: berr3(1)

  ! Case 4 storage: zero AYB column, n=3, nrhs=2
  complex*16 :: res4(3, 2)
  double precision :: res4_r(2*3*2)
  equivalence (res4, res4_r)
  double precision :: ayb4(3, 2)
  double precision :: berr4(2)

  ! Case 5 storage: nrhs=0 quick return
  complex*16 :: res5(3, 1)
  double precision :: ayb5(3, 1)
  double precision :: berr5(1)

  integer :: n, nz, nrhs

  ! Test 1: nominal complex, n=4, nrhs=2
  n = 4
  nz = 4
  nrhs = 2
  res1(1, 1) = ( 1.0d-10,  2.0d-10)
  res1(2, 1) = (-3.0d-10,  4.0d-10)
  res1(3, 1) = ( 5.0d-10, -6.0d-10)
  res1(4, 1) = ( 7.0d-10,  8.0d-10)
  res1(1, 2) = ( 9.0d-10, -1.0d-10)
  res1(2, 2) = (-2.0d-10,  3.0d-10)
  res1(3, 2) = ( 4.0d-10,  5.0d-10)
  res1(4, 2) = ( 6.0d-10, -7.0d-10)
  ayb1(1:4, 1) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0/)
  ayb1(1:4, 2) = (/10.0d0, 20.0d0, 30.0d0, 40.0d0/)
  berr1 = -1.0d0
  call ZLA_LIN_BERR(n, nz, nrhs, res1, ayb1, berr1)
  call begin_test('basic_complex')
  call print_array('berr', berr1, nrhs)
  call end_test()

  ! Test 2: real-only residual (imag == 0)
  n = 3
  nz = 3
  nrhs = 2
  res2(1, 1) = (1.0d-8, 0.0d0)
  res2(2, 1) = (2.0d-8, 0.0d0)
  res2(3, 1) = (3.0d-8, 0.0d0)
  res2(1, 2) = (4.0d-8, 0.0d0)
  res2(2, 2) = (5.0d-8, 0.0d0)
  res2(3, 2) = (6.0d-8, 0.0d0)
  ayb2(1:3, 1) = (/1.0d0, 2.0d0, 3.0d0/)
  ayb2(1:3, 2) = (/4.0d0, 5.0d0, 6.0d0/)
  berr2 = -1.0d0
  call ZLA_LIN_BERR(n, nz, nrhs, res2, ayb2, berr2)
  call begin_test('real_only')
  call print_array('berr', berr2, nrhs)
  call end_test()

  ! Test 3: n=1, nrhs=1
  n = 1
  nz = 1
  nrhs = 1
  res3(1, 1) = (1.0d-6, -2.0d-6)
  ayb3(1, 1) = 2.0d0
  berr3 = -1.0d0
  call ZLA_LIN_BERR(n, nz, nrhs, res3, ayb3, berr3)
  call begin_test('n_one')
  call print_array('berr', berr3, nrhs)
  call end_test()

  ! Test 4: zero AYB entries (skipped in accumulation)
  n = 3
  nz = 3
  nrhs = 2
  res4(1, 1) = (1.0d-8, 2.0d-8)
  res4(2, 1) = (3.0d-8, 4.0d-8)
  res4(3, 1) = (5.0d-8, 6.0d-8)
  res4(1, 2) = (7.0d-8, 8.0d-8)
  res4(2, 2) = (9.0d-8, 1.0d-8)
  res4(3, 2) = (2.0d-8, 3.0d-8)
  ayb4(1:3, 1) = (/0.0d0, 1.0d0, 0.0d0/)
  ayb4(1:3, 2) = (/0.0d0, 0.0d0, 0.0d0/)
  berr4 = -1.0d0
  call ZLA_LIN_BERR(n, nz, nrhs, res4, ayb4, berr4)
  call begin_test('zero_ayb')
  call print_array('berr', berr4, nrhs)
  call end_test()

  ! Test 5: nrhs=0 quick return
  n = 3
  nz = 3
  nrhs = 0
  res5 = (0.0d0, 0.0d0)
  ayb5 = 0.0d0
  berr5 = 99.0d0
  call ZLA_LIN_BERR(n, nz, nrhs, res5, ayb5, berr5)
  call begin_test('nrhs_zero')
  call print_array('berr', berr5, 1)
  call end_test()

end program
