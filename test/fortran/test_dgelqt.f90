program test_dgelqt
  use test_utils
  implicit none

  ! Use generously-sized buffers so any (M,N,MB) we test fits.
  integer, parameter :: NMAX = 16
  double precision :: A(NMAX, NMAX), T(NMAX, NMAX), WORK(NMAX*NMAX)
  integer :: INFO

  ! ---------------------------------------------------------------
  ! Test 1: M=4, N=6, MB=2 (M < N, multiple panels with trailing update)
  ! ---------------------------------------------------------------
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  A(1,1) = 3.0d0; A(1,2) = 0.6d0; A(1,3) = 0.4d0; A(1,4) = 0.2d0; A(1,5) = 0.1d0; A(1,6) = -0.3d0
  A(2,1) = 0.5d0; A(2,2) = 4.0d0; A(2,3) = 0.7d0; A(2,4) = 0.3d0; A(2,5) = -0.2d0; A(2,6) = 0.5d0
  A(3,1) = 0.2d0; A(3,2) = 0.5d0; A(3,3) = 3.5d0; A(3,4) = 0.8d0; A(3,5) = 0.6d0; A(3,6) = 0.1d0
  A(4,1) = 0.4d0; A(4,2) = 0.3d0; A(4,3) = 0.5d0; A(4,4) = 4.5d0; A(4,5) = 1.1d0; A(4,6) = -0.5d0
  call DGELQT(4, 6, 2, A, NMAX, T, NMAX, WORK, INFO)
  call begin_test('m4_n6_mb2')
  call print_matrix('A', A, NMAX, 4, 6)
  call print_matrix('T', T, NMAX, 2, 4)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 2: M=5, N=7, MB=3 (uneven block at end: K=5, IB=3 then IB=2)
  ! ---------------------------------------------------------------
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  A(1,1) = 5.0d0; A(1,2) = 0.7d0; A(1,3) = 0.3d0; A(1,4) = -0.1d0; A(1,5) = 0.4d0; A(1,6) = 0.2d0; A(1,7) = 0.5d0
  A(2,1) = 0.6d0; A(2,2) = 4.5d0; A(2,3) = 0.8d0; A(2,4) =  0.5d0; A(2,5) = -0.3d0; A(2,6) = 0.7d0; A(2,7) = 0.1d0
  A(3,1) = 0.3d0; A(3,2) = 0.4d0; A(3,3) = 5.5d0; A(3,4) =  0.9d0; A(3,5) = 0.7d0; A(3,6) = -0.2d0; A(3,7) = 0.4d0
  A(4,1) = 0.2d0; A(4,2) = 0.5d0; A(4,3) = 0.6d0; A(4,4) =  4.8d0; A(4,5) = 1.0d0; A(4,6) = 0.3d0; A(4,7) = -0.4d0
  A(5,1) = 0.1d0; A(5,2) = 0.3d0; A(5,3) = 0.5d0; A(5,4) =  0.7d0; A(5,5) = 5.2d0; A(5,6) = 0.9d0; A(5,7) = 0.6d0
  call DGELQT(5, 7, 3, A, NMAX, T, NMAX, WORK, INFO)
  call begin_test('m5_n7_mb3')
  call print_matrix('A', A, NMAX, 5, 7)
  call print_matrix('T', T, NMAX, 3, 5)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 3: M=6, N=4, MB=2 (M > N, K=4, multiple panels, trailing update from below)
  ! ---------------------------------------------------------------
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  A(1,1) = 4.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0; A(1,4) = 0.1d0
  A(2,1) = 0.5d0; A(2,2) = 3.5d0; A(2,3) = 0.4d0; A(2,4) = 0.2d0
  A(3,1) = 0.2d0; A(3,2) = 0.4d0; A(3,3) = 5.0d0; A(3,4) = 0.6d0
  A(4,1) = 0.6d0; A(4,2) = 0.3d0; A(4,3) = 0.5d0; A(4,4) = 4.5d0
  A(5,1) = 0.1d0; A(5,2) = 0.7d0; A(5,3) = 0.2d0; A(5,4) = 0.5d0
  A(6,1) = 0.3d0; A(6,2) = 0.2d0; A(6,3) = 0.6d0; A(6,4) = 0.4d0
  call DGELQT(6, 4, 2, A, NMAX, T, NMAX, WORK, INFO)
  call begin_test('m6_n4_mb2')
  call print_matrix('A', A, NMAX, 6, 4)
  call print_matrix('T', T, NMAX, 2, 4)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 4: M=N=5, MB=2 (square, K=5)
  ! ---------------------------------------------------------------
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  A(1,1) = 5.0d0; A(1,2) = 0.6d0; A(1,3) = 0.3d0; A(1,4) = 0.4d0; A(1,5) = 0.2d0
  A(2,1) = 0.5d0; A(2,2) = 4.5d0; A(2,3) = 0.7d0; A(2,4) = 0.2d0; A(2,5) = -0.1d0
  A(3,1) = 0.2d0; A(3,2) = 0.4d0; A(3,3) = 5.5d0; A(3,4) = 0.8d0; A(3,5) = 0.6d0
  A(4,1) = 0.3d0; A(4,2) = 0.5d0; A(4,3) = 0.4d0; A(4,4) = 4.8d0; A(4,5) = 1.0d0
  A(5,1) = 0.1d0; A(5,2) = 0.3d0; A(5,3) = 0.5d0; A(5,4) = 0.7d0; A(5,5) = 5.2d0
  call DGELQT(5, 5, 2, A, NMAX, T, NMAX, WORK, INFO)
  call begin_test('m5_n5_mb2')
  call print_matrix('A', A, NMAX, 5, 5)
  call print_matrix('T', T, NMAX, 2, 5)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 5: M=3, N=5, MB=3 (single block: K=3, MB=3 — no trailing update)
  ! ---------------------------------------------------------------
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  A(1,1) = 4.0d0; A(1,2) = 1.0d0; A(1,3) = 0.5d0; A(1,4) = -0.25d0; A(1,5) = 0.75d0
  A(2,1) = 0.5d0; A(2,2) = 3.5d0; A(2,3) = 1.2d0; A(2,4) =  0.6d0;  A(2,5) = -0.4d0
  A(3,1) = 0.3d0; A(3,2) = 0.8d0; A(3,3) = 4.5d0; A(3,4) =  1.1d0;  A(3,5) = 0.9d0
  call DGELQT(3, 5, 3, A, NMAX, T, NMAX, WORK, INFO)
  call begin_test('m3_n5_mb3')
  call print_matrix('A', A, NMAX, 3, 5)
  call print_matrix('T', T, NMAX, 3, 3)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 6: M=4, N=6, MB=1 (degenerate: each panel is 1 row)
  ! ---------------------------------------------------------------
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  A(1,1) = 3.0d0; A(1,2) = 0.6d0; A(1,3) = 0.4d0; A(1,4) = 0.2d0; A(1,5) = 0.1d0; A(1,6) = -0.3d0
  A(2,1) = 0.5d0; A(2,2) = 4.0d0; A(2,3) = 0.7d0; A(2,4) = 0.3d0; A(2,5) = -0.2d0; A(2,6) = 0.5d0
  A(3,1) = 0.2d0; A(3,2) = 0.5d0; A(3,3) = 3.5d0; A(3,4) = 0.8d0; A(3,5) = 0.6d0; A(3,6) = 0.1d0
  A(4,1) = 0.4d0; A(4,2) = 0.3d0; A(4,3) = 0.5d0; A(4,4) = 4.5d0; A(4,5) = 1.1d0; A(4,6) = -0.5d0
  call DGELQT(4, 6, 1, A, NMAX, T, NMAX, WORK, INFO)
  call begin_test('m4_n6_mb1')
  call print_matrix('A', A, NMAX, 4, 6)
  call print_matrix('T', T, NMAX, 1, 4)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 7: M=1, N=4, MB=1 (single-row matrix — K=1, single block)
  ! ---------------------------------------------------------------
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.5d0; A(1,3) = 0.5d0; A(1,4) = -1.25d0
  call DGELQT(1, 4, 1, A, NMAX, T, NMAX, WORK, INFO)
  call begin_test('m1_n4_mb1')
  call print_matrix('A', A, NMAX, 1, 4)
  call print_matrix('T', T, NMAX, 1, 1)
  call print_int('INFO', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 8: M=0, N=4, MB=1 (quick return on K=0)
  ! ---------------------------------------------------------------
  A = 0.0d0; T = 0.0d0; WORK = 0.0d0
  call DGELQT(0, 4, 1, A, NMAX, T, NMAX, WORK, INFO)
  call begin_test('m0_n4_mb1')
  call print_int('INFO', INFO)
  call end_test()

end program
