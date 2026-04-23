program test_dgeqrf
  use test_utils
  implicit none

  double precision :: A(70, 70), TAU(70), WORK(10000)
  integer :: INFO, i, j, M, N

  ! Test 1: 3x3 well-conditioned matrix
  A = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0
  A(3,1) = 3.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGEQRF(3, 3, A, 70, TAU, WORK, 10000, INFO)
  call begin_test('3x3')
  call print_matrix('A', A, 70, 3, 3)
  call print_array('TAU', TAU, 3)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 2: 4x3 well-conditioned matrix
  A = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0
  A(3,1) = 3.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0
  A(4,1) = 1.0d0; A(4,2) = 3.0d0; A(4,3) = 1.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGEQRF(4, 3, A, 70, TAU, WORK, 10000, INFO)
  call begin_test('4x3')
  call print_matrix('A', A, 70, 4, 3)
  call print_array('TAU', TAU, 3)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 3: N=0
  A = 0.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGEQRF(3, 0, A, 70, TAU, WORK, 10000, INFO)
  call begin_test('n_zero')
  call print_int('INFO', INFO)
  call end_test()

  ! Test 4: Large matrix for blocked path (65x65) - well conditioned diagonal dominant
  M = 65
  N = 65
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
  call DGEQRF(M, N, A, 70, TAU, WORK, 10000, INFO)
  call begin_test('large_65x65')
  call print_matrix('A', A, 70, M, N)
  call print_array('TAU', TAU, min(M,N))
  call print_int('INFO', INFO)
  call end_test()

end program
