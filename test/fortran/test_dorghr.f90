program test_dorghr
  use test_utils
  implicit none

  integer, parameter :: MAXN = 10, LWORK = 1024
  double precision :: A(MAXN, MAXN), TAU(MAXN), WORK(LWORK)
  double precision :: QtQ(MAXN, MAXN)
  integer :: INFO, i, j, k, N, ILO, IHI

  ! ---------------------------------------------------------------
  ! Test 1: Basic 5x5, ILO=1, IHI=5 (full range)
  ! ---------------------------------------------------------------
  N = 5; ILO = 1; IHI = 5
  A = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0; A(1,4) = 1.0d0; A(1,5) = 2.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0; A(2,4) = 3.0d0; A(2,5) = 1.0d0
  A(3,1) = 3.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0; A(3,4) = 2.0d0; A(3,5) = 3.0d0
  A(4,1) = 1.0d0; A(4,2) = 3.0d0; A(4,3) = 2.0d0; A(4,4) = 4.0d0; A(4,5) = 1.0d0
  A(5,1) = 2.0d0; A(5,2) = 1.0d0; A(5,3) = 3.0d0; A(5,4) = 1.0d0; A(5,5) = 5.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGEHRD(N, ILO, IHI, A, MAXN, TAU, WORK, LWORK, INFO)
  call DORGHR(N, ILO, IHI, A, MAXN, TAU, WORK, LWORK, INFO)
  ! Verify orthogonality: Q^T * Q = I
  QtQ = 0.0d0
  do i = 1, N
    do j = 1, N
      do k = 1, N
        QtQ(i, j) = QtQ(i, j) + A(k, i) * A(k, j)
      end do
    end do
  end do
  call begin_test('5x5_full')
  call print_matrix('Q', A, MAXN, N, N)
  call print_matrix('QtQ', QtQ, MAXN, N, N)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 2: 5x5, ILO=2, IHI=4 (partial range)
  ! ---------------------------------------------------------------
  N = 5; ILO = 2; IHI = 4
  A = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 3.0d0; A(1,4) = 1.0d0; A(1,5) = 2.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0; A(2,4) = 3.0d0; A(2,5) = 1.0d0
  A(3,1) = 3.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0; A(3,4) = 2.0d0; A(3,5) = 3.0d0
  A(4,1) = 1.0d0; A(4,2) = 3.0d0; A(4,3) = 2.0d0; A(4,4) = 4.0d0; A(4,5) = 1.0d0
  A(5,1) = 2.0d0; A(5,2) = 1.0d0; A(5,3) = 3.0d0; A(5,4) = 1.0d0; A(5,5) = 5.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGEHRD(N, ILO, IHI, A, MAXN, TAU, WORK, LWORK, INFO)
  call DORGHR(N, ILO, IHI, A, MAXN, TAU, WORK, LWORK, INFO)
  ! Verify orthogonality
  QtQ = 0.0d0
  do i = 1, N
    do j = 1, N
      do k = 1, N
        QtQ(i, j) = QtQ(i, j) + A(k, i) * A(k, j)
      end do
    end do
  end do
  call begin_test('5x5_partial')
  call print_matrix('Q', A, MAXN, N, N)
  call print_matrix('QtQ', QtQ, MAXN, N, N)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 3: N=0 quick return
  ! ---------------------------------------------------------------
  N = 0; ILO = 1; IHI = 0
  A = 0.0d0; TAU = 0.0d0; WORK = 0.0d0
  call DORGHR(N, ILO, IHI, A, MAXN, TAU, WORK, LWORK, INFO)
  call begin_test('n_zero')
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 4: N=1 (trivial, 1x1 identity)
  ! ---------------------------------------------------------------
  N = 1; ILO = 1; IHI = 1
  A = 0.0d0; A(1,1) = 99.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGEHRD(N, ILO, IHI, A, MAXN, TAU, WORK, LWORK, INFO)
  call DORGHR(N, ILO, IHI, A, MAXN, TAU, WORK, LWORK, INFO)
  call begin_test('n_one')
  call print_matrix('Q', A, MAXN, 1, 1)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 5: ILO=IHI (no reflectors, should be identity)
  ! ---------------------------------------------------------------
  N = 4; ILO = 2; IHI = 2
  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 3.0d0; A(1,4) = 4.0d0
  A(2,1) = 0.0d0; A(2,2) = 5.0d0; A(2,3) = 6.0d0; A(2,4) = 7.0d0
  A(3,1) = 0.0d0; A(3,2) = 0.0d0; A(3,3) = 8.0d0; A(3,4) = 9.0d0
  A(4,1) = 0.0d0; A(4,2) = 0.0d0; A(4,3) = 0.0d0; A(4,4) = 10.0d0
  TAU = 0.0d0; WORK = 0.0d0
  ! With ILO=IHI, dgehrd is a no-op, dorghr should produce identity
  call DGEHRD(N, ILO, IHI, A, MAXN, TAU, WORK, LWORK, INFO)
  call DORGHR(N, ILO, IHI, A, MAXN, TAU, WORK, LWORK, INFO)
  call begin_test('ilo_eq_ihi')
  call print_matrix('Q', A, MAXN, N, N)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 6: 4x4, ILO=1, IHI=4 (another full range)
  ! ---------------------------------------------------------------
  N = 4; ILO = 1; IHI = 4
  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 3.0d0; A(1,4) = 4.0d0
  A(2,1) = 5.0d0; A(2,2) = 6.0d0; A(2,3) = 7.0d0; A(2,4) = 8.0d0
  A(3,1) = 9.0d0; A(3,2) = 10.0d0; A(3,3) = 11.0d0; A(3,4) = 12.0d0
  A(4,1) = 13.0d0; A(4,2) = 14.0d0; A(4,3) = 15.0d0; A(4,4) = 16.0d0
  TAU = 0.0d0; WORK = 0.0d0
  call DGEHRD(N, ILO, IHI, A, MAXN, TAU, WORK, LWORK, INFO)
  call DORGHR(N, ILO, IHI, A, MAXN, TAU, WORK, LWORK, INFO)
  QtQ = 0.0d0
  do i = 1, N
    do j = 1, N
      do k = 1, N
        QtQ(i, j) = QtQ(i, j) + A(k, i) * A(k, j)
      end do
    end do
  end do
  call begin_test('4x4_full')
  call print_matrix('Q', A, MAXN, N, N)
  call print_matrix('QtQ', QtQ, MAXN, N, N)
  call print_int('INFO', INFO)
  call end_test()

end program
