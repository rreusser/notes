program test_dtrttp
  use test_utils
  implicit none
  double precision :: A(16), AP(10)
  integer :: INFO, i

  ! Test 1: lower triangular 3x3
  ! A = [1 4 7; 2 5 8; 3 6 9] column-major in full storage
  do i = 1, 9
    A(i) = dble(i)
  end do
  AP = 0.0d0
  call dtrttp('L', 3, A, 3, AP, INFO)
  call begin_test('lower_3x3')
  call print_int('info', INFO)
  call print_array('AP', AP, 6)
  call end_test()

  ! Test 2: upper triangular 3x3
  AP = 0.0d0
  call dtrttp('U', 3, A, 3, AP, INFO)
  call begin_test('upper_3x3')
  call print_int('info', INFO)
  call print_array('AP', AP, 6)
  call end_test()

  ! Test 3: lower triangular 4x4
  do i = 1, 16
    A(i) = dble(i)
  end do
  AP = 0.0d0
  call dtrttp('L', 4, A, 4, AP, INFO)
  call begin_test('lower_4x4')
  call print_int('info', INFO)
  call print_array('AP', AP, 10)
  call end_test()

  ! Test 4: upper triangular 4x4
  AP = 0.0d0
  call dtrttp('U', 4, A, 4, AP, INFO)
  call begin_test('upper_4x4')
  call print_int('info', INFO)
  call print_array('AP', AP, 10)
  call end_test()

  ! Test 5: N=0 (quick return)
  AP = -1.0d0
  call dtrttp('L', 0, A, 1, AP, INFO)
  call begin_test('n_zero')
  call print_int('info', INFO)
  call print_array('AP', AP, 1)
  call end_test()

  ! Test 6: N=1 lower
  A(1) = 42.0d0
  AP(1) = 0.0d0
  call dtrttp('L', 1, A, 1, AP, INFO)
  call begin_test('n_one_lower')
  call print_int('info', INFO)
  call print_array('AP', AP, 1)
  call end_test()

  ! Test 7: N=1 upper
  A(1) = 42.0d0
  AP(1) = 0.0d0
  call dtrttp('U', 1, A, 1, AP, INFO)
  call begin_test('n_one_upper')
  call print_int('info', INFO)
  call print_array('AP', AP, 1)
  call end_test()

  ! Test 8: lower 3x3 with LDA > N (LDA=4)
  ! A stored column-major with LDA=4:
  ! col 1: A(1..4) = 1,2,3,99
  ! col 2: A(5..8) = 5,6,7,99
  ! col 3: A(9..12) = 9,10,11,99
  A(1) = 1.0d0
  A(2) = 2.0d0
  A(3) = 3.0d0
  A(4) = 99.0d0
  A(5) = 5.0d0
  A(6) = 6.0d0
  A(7) = 7.0d0
  A(8) = 99.0d0
  A(9) = 9.0d0
  A(10) = 10.0d0
  A(11) = 11.0d0
  A(12) = 99.0d0
  AP = 0.0d0
  call dtrttp('L', 3, A, 4, AP, INFO)
  call begin_test('lower_3x3_lda4')
  call print_int('info', INFO)
  call print_array('AP', AP, 6)
  call end_test()

  ! Test 9: upper 3x3 with LDA > N (LDA=4)
  AP = 0.0d0
  call dtrttp('U', 3, A, 4, AP, INFO)
  call begin_test('upper_3x3_lda4')
  call print_int('info', INFO)
  call print_array('AP', AP, 6)
  call end_test()

end program
