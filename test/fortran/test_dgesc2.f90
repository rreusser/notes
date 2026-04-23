program test_dgesc2
  use test_utils
  implicit none
  double precision :: A(4,4), RHS(4), SCALE
  integer :: IPIV(4), JPIV(4), INFO
  integer :: N

  ! Test 1: Simple 2x2 system
  ! First factor with dgetc2, then solve with dgesc2
  A(1,1) = 4.0d0; A(1,2) = 3.0d0
  A(2,1) = 2.0d0; A(2,2) = 1.0d0
  call DGETC2(2, A, 4, IPIV, JPIV, INFO)
  ! RHS = [10, 4] => x = [1, 2] for original A*x = [10, 4]
  RHS(1) = 10.0d0; RHS(2) = 4.0d0
  call DGESC2(2, A, 4, RHS, IPIV, JPIV, SCALE)
  call begin_test('basic_2x2')
  call print_scalar('scale', SCALE)
  call print_array('rhs', RHS, 2)
  call end_test()

  ! Test 2: 3x3 system
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 1.0d0
  A(2,1) = 4.0d0; A(2,2) = 3.0d0; A(2,3) = 3.0d0
  A(3,1) = 8.0d0; A(3,2) = 7.0d0; A(3,3) = 9.0d0
  call DGETC2(3, A, 4, IPIV, JPIV, INFO)
  RHS(1) = 4.0d0; RHS(2) = 10.0d0; RHS(3) = 24.0d0
  call DGESC2(3, A, 4, RHS, IPIV, JPIV, SCALE)
  call begin_test('basic_3x3')
  call print_scalar('scale', SCALE)
  call print_array('rhs', RHS, 3)
  call end_test()

  ! Test 3: 4x4 system
  A(1,1) = 5.0d0; A(1,2) = 7.0d0; A(1,3) = 6.0d0; A(1,4) = 5.0d0
  A(2,1) = 7.0d0; A(2,2) = 10.0d0; A(2,3) = 8.0d0; A(2,4) = 7.0d0
  A(3,1) = 6.0d0; A(3,2) = 8.0d0; A(3,3) = 10.0d0; A(3,4) = 9.0d0
  A(4,1) = 5.0d0; A(4,2) = 7.0d0; A(4,3) = 9.0d0; A(4,4) = 10.0d0
  call DGETC2(4, A, 4, IPIV, JPIV, INFO)
  RHS(1) = 23.0d0; RHS(2) = 32.0d0; RHS(3) = 33.0d0; RHS(4) = 31.0d0
  call DGESC2(4, A, 4, RHS, IPIV, JPIV, SCALE)
  call begin_test('basic_4x4')
  call print_scalar('scale', SCALE)
  call print_array('rhs', RHS, 4)
  call end_test()

  ! Test 4: Identity-like system (N=1)
  A(1,1) = 3.0d0
  call DGETC2(1, A, 4, IPIV, JPIV, INFO)
  RHS(1) = 9.0d0
  call DGESC2(1, A, 4, RHS, IPIV, JPIV, SCALE)
  call begin_test('n_equals_1')
  call print_scalar('scale', SCALE)
  call print_array('rhs', RHS, 1)
  call end_test()

end program
