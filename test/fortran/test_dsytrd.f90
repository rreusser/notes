program test_dsytrd
  use test_utils
  implicit none

  integer :: INFO, LWORK, i, j
  double precision :: A4(4,4), A1(1,1)
  double precision :: D4(4), E4(4), TAU4(4)
  double precision :: A35(35,35)
  double precision :: D35(35), E35(35), TAU35(35)
  double precision :: WORK(35*32)

  LWORK = 35*32

  ! Test 1: UPLO='U', 4x4 well-conditioned SPD matrix (unblocked, N <= NB)
  A4(1,1) = 4.0d0; A4(1,2) = 1.0d0; A4(1,3) = 2.0d0; A4(1,4) = 1.0d0
  A4(2,1) = 1.0d0; A4(2,2) = 5.0d0; A4(2,3) = 1.0d0; A4(2,4) = 2.0d0
  A4(3,1) = 2.0d0; A4(3,2) = 1.0d0; A4(3,3) = 6.0d0; A4(3,4) = 1.0d0
  A4(4,1) = 1.0d0; A4(4,2) = 2.0d0; A4(4,3) = 1.0d0; A4(4,4) = 7.0d0
  D4 = 0.0d0; E4 = 0.0d0; TAU4 = 0.0d0
  call DSYTRD('U', 4, A4, 4, D4, E4, TAU4, WORK, LWORK, INFO)
  call begin_test('upper_4x4')
  call print_int('info', INFO)
  call print_matrix('A', A4, 4, 4, 4)
  call print_array('d', D4, 4)
  call print_array('e', E4, 3)
  call print_array('tau', TAU4, 3)
  call end_test()

  ! Test 2: UPLO='L', 4x4 same symmetric matrix (unblocked)
  A4(1,1) = 4.0d0; A4(1,2) = 1.0d0; A4(1,3) = 2.0d0; A4(1,4) = 1.0d0
  A4(2,1) = 1.0d0; A4(2,2) = 5.0d0; A4(2,3) = 1.0d0; A4(2,4) = 2.0d0
  A4(3,1) = 2.0d0; A4(3,2) = 1.0d0; A4(3,3) = 6.0d0; A4(3,4) = 1.0d0
  A4(4,1) = 1.0d0; A4(4,2) = 2.0d0; A4(4,3) = 1.0d0; A4(4,4) = 7.0d0
  D4 = 0.0d0; E4 = 0.0d0; TAU4 = 0.0d0
  call DSYTRD('L', 4, A4, 4, D4, E4, TAU4, WORK, LWORK, INFO)
  call begin_test('lower_4x4')
  call print_int('info', INFO)
  call print_matrix('A', A4, 4, 4, 4)
  call print_array('d', D4, 4)
  call print_array('e', E4, 3)
  call print_array('tau', TAU4, 3)
  call end_test()

  ! Test 3: N=1 upper edge case
  A1(1,1) = 3.0d0
  D4(1) = 0.0d0
  call DSYTRD('U', 1, A1, 1, D4, E4, TAU4, WORK, LWORK, INFO)
  call begin_test('n_one_upper')
  call print_int('info', INFO)
  call print_scalar('d1', D4(1))
  call end_test()

  ! Test 4: N=1 lower edge case
  A1(1,1) = 5.0d0
  D4(1) = 0.0d0
  call DSYTRD('L', 1, A1, 1, D4, E4, TAU4, WORK, LWORK, INFO)
  call begin_test('n_one_lower')
  call print_int('info', INFO)
  call print_scalar('d1', D4(1))
  call end_test()

  ! Test 5: N=0 quick return
  call DSYTRD('U', 0, A1, 1, D4, E4, TAU4, WORK, LWORK, INFO)
  call begin_test('n_zero')
  call print_int('info', INFO)
  call end_test()

  ! Test 6: UPLO='U', 35x35 diagonally dominant symmetric matrix (blocked path, N > NB=32)
  ! Build A = diag(1..35) + 0.1 * symmetric random-ish off-diagonal
  do j = 1, 35
    do i = 1, 35
      if (i == j) then
        A35(i,j) = dble(i) + 35.0d0
      else
        ! Deterministic symmetric off-diagonal values
        A35(i,j) = 0.1d0 * dble(mod(i*j + i + j, 7)) / 7.0d0
      end if
    end do
  end do
  ! Ensure symmetric
  do j = 1, 35
    do i = j+1, 35
      A35(i,j) = A35(j,i)
    end do
  end do
  D35 = 0.0d0; E35 = 0.0d0; TAU35 = 0.0d0
  call DSYTRD('U', 35, A35, 35, D35, E35, TAU35, WORK, LWORK, INFO)
  call begin_test('upper_35x35')
  call print_int('info', INFO)
  call print_array('d', D35, 35)
  call print_array('e', E35, 34)
  call print_array('tau', TAU35, 34)
  call end_test()

  ! Test 7: UPLO='L', 35x35 same matrix (blocked path)
  do j = 1, 35
    do i = 1, 35
      if (i == j) then
        A35(i,j) = dble(i) + 35.0d0
      else
        A35(i,j) = 0.1d0 * dble(mod(i*j + i + j, 7)) / 7.0d0
      end if
    end do
  end do
  do j = 1, 35
    do i = j+1, 35
      A35(i,j) = A35(j,i)
    end do
  end do
  D35 = 0.0d0; E35 = 0.0d0; TAU35 = 0.0d0
  call DSYTRD('L', 35, A35, 35, D35, E35, TAU35, WORK, LWORK, INFO)
  call begin_test('lower_35x35')
  call print_int('info', INFO)
  call print_array('d', D35, 35)
  call print_array('e', E35, 34)
  call print_array('tau', TAU35, 34)
  call end_test()

end program
