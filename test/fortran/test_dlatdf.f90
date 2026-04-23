program test_dlatdf
  use test_utils
  implicit none
  double precision :: Z(4,4), RHS(4), RDSUM, RDSCAL
  integer :: IPIV(4), JPIV(4), INFO, N

  ! Test 1: IJOB=1, 2x2 system
  Z(1,1) = 4.0d0; Z(1,2) = 3.0d0
  Z(2,1) = 2.0d0; Z(2,2) = 1.0d0
  call DGETC2(2, Z, 4, IPIV, JPIV, INFO)
  RHS(1) = 1.0d0; RHS(2) = 1.0d0
  RDSUM = 0.0d0; RDSCAL = 1.0d0
  call DLATDF(1, 2, Z, 4, RHS, RDSUM, RDSCAL, IPIV, JPIV)
  call begin_test('ijob1_2x2')
  call print_array('rhs', RHS, 2)
  call print_scalar('rdsum', RDSUM)
  call print_scalar('rdscal', RDSCAL)
  call print_int_array('ipiv', IPIV, 2)
  call print_int_array('jpiv', JPIV, 2)
  call end_test()

  ! Test 2: IJOB=2, 2x2 system
  Z(1,1) = 4.0d0; Z(1,2) = 3.0d0
  Z(2,1) = 2.0d0; Z(2,2) = 1.0d0
  call DGETC2(2, Z, 4, IPIV, JPIV, INFO)
  RHS(1) = 1.0d0; RHS(2) = 1.0d0
  RDSUM = 0.0d0; RDSCAL = 1.0d0
  call DLATDF(2, 2, Z, 4, RHS, RDSUM, RDSCAL, IPIV, JPIV)
  call begin_test('ijob2_2x2')
  call print_array('rhs', RHS, 2)
  call print_scalar('rdsum', RDSUM)
  call print_scalar('rdscal', RDSCAL)
  call end_test()

  ! Test 3: IJOB=1, 3x3 system
  Z(1,1) = 5.0d0; Z(1,2) = 7.0d0; Z(1,3) = 6.0d0
  Z(2,1) = 7.0d0; Z(2,2) = 10.0d0; Z(2,3) = 8.0d0
  Z(3,1) = 6.0d0; Z(3,2) = 8.0d0; Z(3,3) = 10.0d0
  call DGETC2(3, Z, 4, IPIV, JPIV, INFO)
  RHS(1) = 1.0d0; RHS(2) = -1.0d0; RHS(3) = 0.5d0
  RDSUM = 1.0d0; RDSCAL = 1.0d0
  call DLATDF(1, 3, Z, 4, RHS, RDSUM, RDSCAL, IPIV, JPIV)
  call begin_test('ijob1_3x3')
  call print_array('rhs', RHS, 3)
  call print_scalar('rdsum', RDSUM)
  call print_scalar('rdscal', RDSCAL)
  call end_test()

  ! Test 4: IJOB=2, 3x3 system
  Z(1,1) = 5.0d0; Z(1,2) = 7.0d0; Z(1,3) = 6.0d0
  Z(2,1) = 7.0d0; Z(2,2) = 10.0d0; Z(2,3) = 8.0d0
  Z(3,1) = 6.0d0; Z(3,2) = 8.0d0; Z(3,3) = 10.0d0
  call DGETC2(3, Z, 4, IPIV, JPIV, INFO)
  RHS(1) = 1.0d0; RHS(2) = -1.0d0; RHS(3) = 0.5d0
  RDSUM = 1.0d0; RDSCAL = 1.0d0
  call DLATDF(2, 3, Z, 4, RHS, RDSUM, RDSCAL, IPIV, JPIV)
  call begin_test('ijob2_3x3')
  call print_array('rhs', RHS, 3)
  call print_scalar('rdsum', RDSUM)
  call print_scalar('rdscal', RDSCAL)
  call end_test()

  ! Test 5: IJOB=1, 4x4 system
  Z(1,1) = 5.0d0; Z(1,2) = 7.0d0; Z(1,3) = 6.0d0; Z(1,4) = 5.0d0
  Z(2,1) = 7.0d0; Z(2,2) = 10.0d0; Z(2,3) = 8.0d0; Z(2,4) = 7.0d0
  Z(3,1) = 6.0d0; Z(3,2) = 8.0d0; Z(3,3) = 10.0d0; Z(3,4) = 9.0d0
  Z(4,1) = 5.0d0; Z(4,2) = 7.0d0; Z(4,3) = 9.0d0; Z(4,4) = 10.0d0
  call DGETC2(4, Z, 4, IPIV, JPIV, INFO)
  RHS(1) = 1.0d0; RHS(2) = -1.0d0; RHS(3) = 2.0d0; RHS(4) = -0.5d0
  RDSUM = 0.0d0; RDSCAL = 1.0d0
  call DLATDF(1, 4, Z, 4, RHS, RDSUM, RDSCAL, IPIV, JPIV)
  call begin_test('ijob1_4x4')
  call print_array('rhs', RHS, 4)
  call print_scalar('rdsum', RDSUM)
  call print_scalar('rdscal', RDSCAL)
  call end_test()

  ! Test 6: IJOB=2, 4x4 system
  Z(1,1) = 5.0d0; Z(1,2) = 7.0d0; Z(1,3) = 6.0d0; Z(1,4) = 5.0d0
  Z(2,1) = 7.0d0; Z(2,2) = 10.0d0; Z(2,3) = 8.0d0; Z(2,4) = 7.0d0
  Z(3,1) = 6.0d0; Z(3,2) = 8.0d0; Z(3,3) = 10.0d0; Z(3,4) = 9.0d0
  Z(4,1) = 5.0d0; Z(4,2) = 7.0d0; Z(4,3) = 9.0d0; Z(4,4) = 10.0d0
  call DGETC2(4, Z, 4, IPIV, JPIV, INFO)
  RHS(1) = 1.0d0; RHS(2) = -1.0d0; RHS(3) = 2.0d0; RHS(4) = -0.5d0
  RDSUM = 0.0d0; RDSCAL = 1.0d0
  call DLATDF(2, 4, Z, 4, RHS, RDSUM, RDSCAL, IPIV, JPIV)
  call begin_test('ijob2_4x4')
  call print_array('rhs', RHS, 4)
  call print_scalar('rdsum', RDSUM)
  call print_scalar('rdscal', RDSCAL)
  call end_test()

end program
