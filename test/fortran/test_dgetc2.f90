program test_dgetc2
  use test_utils
  implicit none
  double precision :: A(4,4), Acopy(4,4)
  integer :: IPIV(4), JPIV(4), INFO
  integer :: i, j

  ! Test 1: Simple 2x2 matrix
  A(1,1) = 1.0d0; A(1,2) = 2.0d0
  A(2,1) = 3.0d0; A(2,2) = 4.0d0
  call DGETC2(2, A, 4, IPIV, JPIV, INFO)
  call begin_test('basic_2x2')
  call print_int('info', INFO)
  call print_matrix('A', A, 4, 2, 2)
  call print_int_array('ipiv', IPIV, 2)
  call print_int_array('jpiv', JPIV, 2)
  call end_test()

  ! Test 2: 3x3 matrix
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 3.0d0
  A(2,1) = 4.0d0; A(2,2) = 5.0d0; A(2,3) = 6.0d0
  A(3,1) = 7.0d0; A(3,2) = 8.0d0; A(3,3) = 10.0d0
  call DGETC2(3, A, 4, IPIV, JPIV, INFO)
  call begin_test('basic_3x3')
  call print_int('info', INFO)
  call print_matrix('A', A, 4, 3, 3)
  call print_int_array('ipiv', IPIV, 3)
  call print_int_array('jpiv', JPIV, 3)
  call end_test()

  ! Test 3: 4x4 matrix with complete pivoting needed
  A(1,1) = 0.1d0; A(1,2) = 0.2d0; A(1,3) = 0.3d0; A(1,4) = 10.0d0
  A(2,1) = 0.4d0; A(2,2) = 0.5d0; A(2,3) = 0.6d0; A(2,4) = 0.7d0
  A(3,1) = 0.8d0; A(3,2) = 0.9d0; A(3,3) = 1.0d0; A(3,4) = 1.1d0
  A(4,1) = 1.2d0; A(4,2) = 1.3d0; A(4,3) = 1.4d0; A(4,4) = 1.5d0
  call DGETC2(4, A, 4, IPIV, JPIV, INFO)
  call begin_test('basic_4x4')
  call print_int('info', INFO)
  call print_matrix('A', A, 4, 4, 4)
  call print_int_array('ipiv', IPIV, 4)
  call print_int_array('jpiv', JPIV, 4)
  call end_test()

  ! Test 4: N=1
  A(1,1) = 5.0d0
  call DGETC2(1, A, 4, IPIV, JPIV, INFO)
  call begin_test('n_equals_1')
  call print_int('info', INFO)
  call print_matrix('A', A, 4, 1, 1)
  call print_int_array('ipiv', IPIV, 1)
  call print_int_array('jpiv', JPIV, 1)
  call end_test()

  ! Test 5: Near-singular matrix
  A(1,1) = 1.0d-200; A(1,2) = 1.0d0
  A(2,1) = 1.0d0;    A(2,2) = 1.0d0
  call DGETC2(2, A, 4, IPIV, JPIV, INFO)
  call begin_test('near_singular')
  call print_int('info', INFO)
  call print_matrix('A', A, 4, 2, 2)
  call print_int_array('ipiv', IPIV, 2)
  call print_int_array('jpiv', JPIV, 2)
  call end_test()

end program
