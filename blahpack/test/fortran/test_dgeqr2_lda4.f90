program test_dgeqr2_lda4
  use test_utils
  implicit none

  double precision :: A(4, 3), TAU(3), WORK(3)
  integer :: INFO

  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 3.0d0
  A(2,1) = 4.0d0; A(2,2) = 5.0d0; A(2,3) = 6.0d0
  A(3,1) = 7.0d0; A(3,2) = 8.0d0; A(3,3) = 9.0d0
  A(4,1) = 10.0d0; A(4,2) = 11.0d0; A(4,3) = 12.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGEQR2(4, 3, A, 4, TAU, WORK, INFO)
  call begin_test('4x3_lda4')
  call print_matrix('A', A, 4, 4, 3)
  call print_array('TAU', TAU, 3)
  call print_int('INFO', INFO)
  call end_test()

end program
