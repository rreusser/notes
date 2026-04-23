program test_dgetri
  use test_utils
  implicit none

  double precision :: A(16), WORK(64)
  integer :: IPIV(4), INFO, LDA
  integer :: i

  ! Test 1: 3x3 matrix inverse
  ! A = [2 1 1; 4 3 3; 8 7 9] (column-major)
  A(1) = 2.0d0; A(2) = 4.0d0; A(3) = 8.0d0
  A(4) = 1.0d0; A(5) = 3.0d0; A(6) = 7.0d0
  A(7) = 1.0d0; A(8) = 3.0d0; A(9) = 9.0d0
  LDA = 3

  call DGETRF(3, 3, A, LDA, IPIV, INFO)
  call DGETRI(3, A, LDA, IPIV, WORK, 64, INFO)

  call begin_test('3x3 inverse')
  call print_int('info', INFO)
  call print_array('A', A, 9)
  call print_array('ipiv', dble(IPIV(1:3)), 3)
  call end_test()

  ! Test 2: 4x4 matrix inverse
  ! A = [5 7 6 5; 7 10 8 7; 6 8 10 9; 5 7 9 10] (column-major, symmetric positive definite)
  A(1) = 5.0d0;  A(2) = 7.0d0;  A(3) = 6.0d0;  A(4) = 5.0d0
  A(5) = 7.0d0;  A(6) = 10.0d0; A(7) = 8.0d0;  A(8) = 7.0d0
  A(9) = 6.0d0;  A(10) = 8.0d0; A(11) = 10.0d0; A(12) = 9.0d0
  A(13) = 5.0d0; A(14) = 7.0d0; A(15) = 9.0d0; A(16) = 10.0d0
  LDA = 4

  call DGETRF(4, 4, A, LDA, IPIV, INFO)
  call DGETRI(4, A, LDA, IPIV, WORK, 64, INFO)

  call begin_test('4x4 inverse')
  call print_int('info', INFO)
  call print_array('A', A, 16)
  call print_array('ipiv', dble(IPIV(1:4)), 4)
  call end_test()

  ! Test 3: N=1 edge case
  A(1) = 4.0d0
  LDA = 1
  call DGETRF(1, 1, A, LDA, IPIV, INFO)
  call DGETRI(1, A, LDA, IPIV, WORK, 64, INFO)

  call begin_test('N=1')
  call print_int('info', INFO)
  call print_array('A', A, 1)
  call end_test()

  ! Test 4: N=0 quick return
  LDA = 1
  call DGETRI(0, A, LDA, IPIV, WORK, 64, INFO)

  call begin_test('N=0')
  call print_int('info', INFO)
  call end_test()

  ! Test 5: 3x3 with a different matrix to test different pivot patterns
  ! A = [1 2 3; 4 5 6; 7 8 0] (column-major)
  A(1) = 1.0d0; A(2) = 4.0d0; A(3) = 7.0d0
  A(4) = 2.0d0; A(5) = 5.0d0; A(6) = 8.0d0
  A(7) = 3.0d0; A(8) = 6.0d0; A(9) = 0.0d0
  LDA = 3

  call DGETRF(3, 3, A, LDA, IPIV, INFO)
  call DGETRI(3, A, LDA, IPIV, WORK, 64, INFO)

  call begin_test('3x3 different pivots')
  call print_int('info', INFO)
  call print_array('A', A, 9)
  call print_array('ipiv', dble(IPIV(1:3)), 3)
  call end_test()

end program
