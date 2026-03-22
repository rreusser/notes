program test_dlaset
  use test_utils
  implicit none

  integer, parameter :: LDA = 5
  double precision :: A(LDA, LDA)
  integer :: i, j

  ! ---------------------------------------------------------------
  ! Test 1: UPLO='U', 4x4 square matrix
  ! ---------------------------------------------------------------
  do j = 1, 4
    do i = 1, 4
      A(i, j) = -1.0d0
    end do
  end do
  call DLASET('U', 4, 4, 2.0d0, 5.0d0, A, LDA)
  call begin_test('upper_4x4')
  call print_matrix('A', A, LDA, 4, 4)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 2: UPLO='L', 4x4 square matrix
  ! ---------------------------------------------------------------
  do j = 1, 4
    do i = 1, 4
      A(i, j) = -1.0d0
    end do
  end do
  call DLASET('L', 4, 4, 3.0d0, 7.0d0, A, LDA)
  call begin_test('lower_4x4')
  call print_matrix('A', A, LDA, 4, 4)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 3: Full matrix, 4x4
  ! ---------------------------------------------------------------
  do j = 1, 4
    do i = 1, 4
      A(i, j) = -1.0d0
    end do
  end do
  call DLASET('X', 4, 4, 1.0d0, 9.0d0, A, LDA)
  call begin_test('full_4x4')
  call print_matrix('A', A, LDA, 4, 4)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 4: Rectangular M > N (5x3), full
  ! ---------------------------------------------------------------
  do j = 1, 3
    do i = 1, 5
      A(i, j) = -1.0d0
    end do
  end do
  call DLASET('X', 5, 3, 4.0d0, 8.0d0, A, LDA)
  call begin_test('full_5x3')
  call print_matrix('A', A, LDA, 5, 3)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 5: Rectangular M < N (3x5), full
  ! ---------------------------------------------------------------
  do j = 1, 5
    do i = 1, 3
      A(i, j) = -1.0d0
    end do
  end do
  call DLASET('X', 3, 5, 6.0d0, 2.0d0, A, LDA)
  call begin_test('full_3x5')
  call print_matrix('A', A, LDA, 3, 5)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 6: Upper triangular, rectangular M > N (5x3)
  ! ---------------------------------------------------------------
  do j = 1, 3
    do i = 1, 5
      A(i, j) = -1.0d0
    end do
  end do
  call DLASET('U', 5, 3, 2.0d0, 5.0d0, A, LDA)
  call begin_test('upper_5x3')
  call print_matrix('A', A, LDA, 5, 3)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 7: Lower triangular, rectangular M < N (3x5)
  ! ---------------------------------------------------------------
  do j = 1, 5
    do i = 1, 3
      A(i, j) = -1.0d0
    end do
  end do
  call DLASET('L', 3, 5, 3.0d0, 7.0d0, A, LDA)
  call begin_test('lower_3x5')
  call print_matrix('A', A, LDA, 3, 5)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 8: M=0 quick return
  ! ---------------------------------------------------------------
  do j = 1, 4
    do i = 1, 4
      A(i, j) = -1.0d0
    end do
  end do
  call DLASET('X', 0, 4, 1.0d0, 1.0d0, A, LDA)
  call begin_test('m_zero')
  call print_matrix('A', A, LDA, 4, 4)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 9: N=0 quick return
  ! ---------------------------------------------------------------
  do j = 1, 4
    do i = 1, 4
      A(i, j) = -1.0d0
    end do
  end do
  call DLASET('X', 4, 0, 1.0d0, 1.0d0, A, LDA)
  call begin_test('n_zero')
  call print_matrix('A', A, LDA, 4, 4)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 10: N=1 column vector, full
  ! ---------------------------------------------------------------
  do i = 1, 4
    A(i, 1) = -1.0d0
  end do
  call DLASET('X', 4, 1, 3.0d0, 7.0d0, A, LDA)
  call begin_test('full_4x1')
  call print_matrix('A', A, LDA, 4, 1)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 11: Upper, M < N (3x5)
  ! ---------------------------------------------------------------
  do j = 1, 5
    do i = 1, 3
      A(i, j) = -1.0d0
    end do
  end do
  call DLASET('U', 3, 5, 2.0d0, 5.0d0, A, LDA)
  call begin_test('upper_3x5')
  call print_matrix('A', A, LDA, 3, 5)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 12: Lower, M > N (5x3)
  ! ---------------------------------------------------------------
  do j = 1, 3
    do i = 1, 5
      A(i, j) = -1.0d0
    end do
  end do
  call DLASET('L', 5, 3, 3.0d0, 7.0d0, A, LDA)
  call begin_test('lower_5x3')
  call print_matrix('A', A, LDA, 5, 3)
  call end_test()

end program
