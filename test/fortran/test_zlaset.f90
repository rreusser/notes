program test_zlaset
  use test_utils
  implicit none

  complex*16 :: a(4, 4)
  double precision :: a_real(32)
  equivalence (a, a_real)
  integer :: lda

  lda = 4

  ! Test 1: full matrix 3x3, alpha=(1,2), beta=(3,4)
  a = (0.0d0, 0.0d0)
  call zlaset('A', 3, 3, (1.0d0, 2.0d0), (3.0d0, 4.0d0), a, lda)
  call begin_test('zlaset_full')
  ! Print 3 columns of 3 rows each = 9 complex elements = 18 doubles
  ! But we need to output the column-major flat representation
  ! Column 1: a(1:3,1), Column 2: a(1:3,2), Column 3: a(1:3,3)
  ! Using LDA=4, the doubles in column j start at offset (j-1)*4*2
  call print_array('a_col1', a_real(1), 6)
  call print_array('a_col2', a_real(9), 6)
  call print_array('a_col3', a_real(17), 6)
  call end_test()

  ! Test 2: upper triangular 3x3, alpha=(1,0), beta=(5,0)
  a = (0.0d0, 0.0d0)
  call zlaset('U', 3, 3, (1.0d0, 0.0d0), (5.0d0, 0.0d0), a, lda)
  call begin_test('zlaset_upper')
  call print_array('a_col1', a_real(1), 6)
  call print_array('a_col2', a_real(9), 6)
  call print_array('a_col3', a_real(17), 6)
  call end_test()

  ! Test 3: lower triangular 3x3, alpha=(2,1), beta=(7,3)
  a = (0.0d0, 0.0d0)
  call zlaset('L', 3, 3, (2.0d0, 1.0d0), (7.0d0, 3.0d0), a, lda)
  call begin_test('zlaset_lower')
  call print_array('a_col1', a_real(1), 6)
  call print_array('a_col2', a_real(9), 6)
  call print_array('a_col3', a_real(17), 6)
  call end_test()

  ! Test 4: 1x1 matrix
  a = (0.0d0, 0.0d0)
  call zlaset('A', 1, 1, (10.0d0, 20.0d0), (30.0d0, 40.0d0), a, lda)
  call begin_test('zlaset_1x1')
  call print_array('a', a_real(1), 2)
  call end_test()

  ! Test 5: 0x0 matrix (no-op)
  a = (99.0d0, 99.0d0)
  call zlaset('A', 0, 0, (1.0d0, 2.0d0), (3.0d0, 4.0d0), a, lda)
  call begin_test('zlaset_0x0')
  call print_array('a', a_real(1), 2)
  call end_test()

  ! Test 6: rectangular 2x3
  a = (0.0d0, 0.0d0)
  call zlaset('A', 2, 3, (1.0d0, -1.0d0), (5.0d0, -5.0d0), a, lda)
  call begin_test('zlaset_2x3')
  call print_array('a_col1', a_real(1), 4)
  call print_array('a_col2', a_real(9), 4)
  call print_array('a_col3', a_real(17), 4)
  call end_test()

  ! Test 7: rectangular 3x2
  a = (0.0d0, 0.0d0)
  call zlaset('A', 3, 2, (2.0d0, 3.0d0), (8.0d0, 9.0d0), a, lda)
  call begin_test('zlaset_3x2')
  call print_array('a_col1', a_real(1), 6)
  call print_array('a_col2', a_real(9), 6)
  call end_test()

  ! Test 8: upper 4x4 identity-like
  a = (0.0d0, 0.0d0)
  call zlaset('U', 4, 4, (0.0d0, 0.0d0), (1.0d0, 0.0d0), a, lda)
  call begin_test('zlaset_upper_identity')
  call print_array('a_col1', a_real(1), 8)
  call print_array('a_col2', a_real(9), 8)
  call print_array('a_col3', a_real(17), 8)
  call print_array('a_col4', a_real(25), 8)
  call end_test()

end program
