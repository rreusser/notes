program test_dgeqlf
  use test_utils
  implicit none

  double precision :: A(200, 200), TAU(200), WORK(100000)
  integer :: INFO, i, j, M, N

  ! Test 1: 3x3 well-conditioned matrix
  A = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0
  A(3,1) = 3.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGEQLF(3, 3, A, 200, TAU, WORK, 100000, INFO)
  call begin_test('3x3')
  call print_matrix('A', A, 200, 3, 3)
  call print_array('TAU', TAU, 3)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 2: 4x3 tall matrix
  A = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0
  A(3,1) = 3.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0
  A(4,1) = 1.0d0; A(4,2) = 3.0d0; A(4,3) = 1.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGEQLF(4, 3, A, 200, TAU, WORK, 100000, INFO)
  call begin_test('4x3')
  call print_matrix('A', A, 200, 4, 3)
  call print_array('TAU', TAU, 3)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 3: 3x4 wide matrix
  A = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0; A(1,4) = 4.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0; A(2,4) = 1.0d0
  A(3,1) = 3.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0; A(3,4) = 2.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGEQLF(3, 4, A, 200, TAU, WORK, 100000, INFO)
  call begin_test('3x4')
  call print_matrix('A', A, 200, 3, 4)
  call print_array('TAU', TAU, 3)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 4: N=0
  A = 0.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGEQLF(3, 0, A, 200, TAU, WORK, 100000, INFO)
  call begin_test('n_zero')
  call print_int('INFO', INFO)
  call end_test()

  ! Test 5: Large matrix for blocked path (150x150) - well conditioned diagonal dominant
  M = 150
  N = 150
  A = 0.0d0
  do j = 1, N
    do i = 1, M
      if (i .eq. j) then
        A(i,j) = 10.0d0
      else
        A(i,j) = 1.0d0 / dble(abs(i - j) + 1)
      end if
    end do
  end do
  TAU = 0.0d0; WORK = 0.0d0
  call DGEQLF(M, N, A, 200, TAU, WORK, 100000, INFO)
  call begin_test('large_150x150')
  call print_matrix('A', A, 200, M, N)
  call print_array('TAU', TAU, min(M,N))
  call print_int('INFO', INFO)
  call end_test()

end program
