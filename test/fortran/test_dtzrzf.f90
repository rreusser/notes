program test_dtzrzf
  use test_utils
  implicit none

  integer, parameter :: NMAX = 200
  double precision :: A(NMAX, NMAX), TAU(NMAX), WORK(20000)
  integer :: INFO, i, j, M, N

  ! Test 1: 3x5 upper trapezoidal (unblocked path)
  A = 0.0d0
  A(1,1) = 4.0d0; A(1,2) = 1.0d0; A(1,3) = 2.0d0; A(1,4) = 3.0d0; A(1,5) = 1.0d0
                  A(2,2) = 5.0d0; A(2,3) = 1.0d0; A(2,4) = 2.0d0; A(2,5) = 4.0d0
                                  A(3,3) = 6.0d0; A(3,4) = 1.0d0; A(3,5) = 2.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DTZRZF(3, 5, A, NMAX, TAU, WORK, 20000, INFO)
  call begin_test('3x5')
  call print_matrix('A', A, NMAX, 3, 5)
  call print_array('TAU', TAU, 3)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 2: 4x6 upper trapezoidal (unblocked path)
  A = 0.0d0
  A(1,1) = 5.0d0; A(1,2) = 1.0d0; A(1,3) = 2.0d0; A(1,4) = 3.0d0; A(1,5) = 1.0d0; A(1,6) = 2.0d0
                  A(2,2) = 6.0d0; A(2,3) = 1.0d0; A(2,4) = 2.0d0; A(2,5) = 3.0d0; A(2,6) = 1.0d0
                                  A(3,3) = 7.0d0; A(3,4) = 1.0d0; A(3,5) = 2.0d0; A(3,6) = 3.0d0
                                                  A(4,4) = 8.0d0; A(4,5) = 1.0d0; A(4,6) = 2.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DTZRZF(4, 6, A, NMAX, TAU, WORK, 20000, INFO)
  call begin_test('4x6')
  call print_matrix('A', A, NMAX, 4, 6)
  call print_array('TAU', TAU, 4)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 3: M = N (square) — TAU should be zero, A unchanged
  A = 0.0d0
  A(1,1) = 3.0d0; A(1,2) = 1.0d0; A(1,3) = 2.0d0
                  A(2,2) = 4.0d0; A(2,3) = 1.0d0
                                  A(3,3) = 5.0d0
  TAU = 7.0d0; WORK = 0.0d0
  call DTZRZF(3, 3, A, NMAX, TAU, WORK, 20000, INFO)
  call begin_test('square_3x3')
  call print_matrix('A', A, NMAX, 3, 3)
  call print_array('TAU', TAU, 3)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 4: M = 0 quick return
  A = 0.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DTZRZF(0, 5, A, NMAX, TAU, WORK, 20000, INFO)
  call begin_test('m_zero')
  call print_int('INFO', INFO)
  call end_test()

  ! Test 5: 1x4 single row
  A = 0.0d0
  A(1,1) = 3.0d0; A(1,2) = 1.0d0; A(1,3) = 2.0d0; A(1,4) = 1.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DTZRZF(1, 4, A, NMAX, TAU, WORK, 20000, INFO)
  call begin_test('1x4')
  call print_matrix('A', A, NMAX, 1, 4)
  call print_array('TAU', TAU, 1)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 6: 2x4 with L=2
  A = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0; A(1,4) = 1.0d0
                  A(2,2) = 3.0d0; A(2,3) = 1.0d0; A(2,4) = 2.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DTZRZF(2, 4, A, NMAX, TAU, WORK, 20000, INFO)
  call begin_test('2x4')
  call print_matrix('A', A, NMAX, 2, 4)
  call print_array('TAU', TAU, 2)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 7: Larger matrix to exercise blocked path crossover (M=40, N=80).
  ! Use upper trapezoidal with diagonal dominance for stability.
  M = 40
  N = 80
  A = 0.0d0
  do j = 1, N
    do i = 1, M
      if (j .ge. i) then
        if (i .eq. j) then
          A(i,j) = 10.0d0 + dble(i)
        else
          A(i,j) = 1.0d0 / dble(j - i + 1)
        end if
      end if
    end do
  end do
  TAU = 0.0d0; WORK = 0.0d0
  call DTZRZF(M, N, A, NMAX, TAU, WORK, 20000, INFO)
  call begin_test('large_40x80')
  call print_matrix('A', A, NMAX, M, N)
  call print_array('TAU', TAU, M)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 8: M=N=0 quick return
  TAU = 0.0d0; WORK = 0.0d0
  call DTZRZF(0, 0, A, NMAX, TAU, WORK, 20000, INFO)
  call begin_test('m_n_zero')
  call print_int('INFO', INFO)
  call end_test()

  ! Test 9: Square M=N=4 — TAU should all be zero
  A = 0.0d0
  A(1,1) = 4.0d0; A(1,2) = 1.0d0; A(1,3) = 2.0d0; A(1,4) = 3.0d0
                  A(2,2) = 5.0d0; A(2,3) = 1.0d0; A(2,4) = 2.0d0
                                  A(3,3) = 6.0d0; A(3,4) = 1.0d0
                                                  A(4,4) = 7.0d0
  TAU = 9.9d0; WORK = 0.0d0
  call DTZRZF(4, 4, A, NMAX, TAU, WORK, 20000, INFO)
  call begin_test('square_4x4')
  call print_matrix('A', A, NMAX, 4, 4)
  call print_array('TAU', TAU, 4)
  call print_int('INFO', INFO)
  call end_test()

end program
