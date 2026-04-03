program test_zlarfx
  use test_utils
  implicit none

  integer, parameter :: NMAX = 12
  complex*16 :: C(NMAX, NMAX), V(NMAX), WORK(NMAX), TAU
  double precision :: Cpk_r(2*NMAX*NMAX)
  complex*16 :: Cpk(NMAX*NMAX)
  equivalence (Cpk, Cpk_r)
  integer :: I, J, M, N

  ! ============================================================
  ! Test 1: Left side, M=1, N=3 (unrolled M=1)
  ! ============================================================
  C = (0.0d0, 0.0d0); V = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  M = 1; N = 3
  V(1) = (1.0d0, 0.0d0)
  TAU = (1.5d0, 0.3d0)
  C(1,1) = (2.0d0, 1.0d0); C(1,2) = (3.0d0, -1.0d0); C(1,3) = (4.0d0, 0.5d0)
  call ZLARFX('L', M, N, V, TAU, C, NMAX, WORK)
  do J = 1, N
    do I = 1, M
      Cpk(I + (J-1)*M) = C(I, J)
    end do
  end do
  call begin_test('left M=1 N=3')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! ============================================================
  ! Test 2: Left side, M=2, N=3 (unrolled M=2)
  ! ============================================================
  C = (0.0d0, 0.0d0); V = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  M = 2; N = 3
  V(1) = (1.0d0, 0.0d0); V(2) = (0.5d0, 0.3d0)
  TAU = (1.6d0, -0.2d0)
  C(1,1) = (1.0d0, 2.0d0); C(1,2) = (2.0d0, 0.5d0); C(1,3) = (3.0d0, -1.0d0)
  C(2,1) = (4.0d0, -1.0d0); C(2,2) = (5.0d0, 3.0d0); C(2,3) = (6.0d0, 0.0d0)
  call ZLARFX('L', M, N, V, TAU, C, NMAX, WORK)
  do J = 1, N
    do I = 1, M
      Cpk(I + (J-1)*M) = C(I, J)
    end do
  end do
  call begin_test('left M=2 N=3')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! ============================================================
  ! Test 3: Left side, M=3, N=2 (unrolled M=3)
  ! ============================================================
  C = (0.0d0, 0.0d0); V = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  M = 3; N = 2
  V(1) = (1.0d0, 0.0d0); V(2) = (0.3d0, -0.5d0); V(3) = (-0.4d0, 0.2d0)
  TAU = (1.2d0, 0.4d0)
  C(1,1) = (2.0d0, 1.0d0); C(1,2) = (1.0d0, -0.5d0)
  C(2,1) = (3.0d0, -2.0d0); C(2,2) = (4.0d0, 1.0d0)
  C(3,1) = (5.0d0, 0.0d0); C(3,2) = (6.0d0, -3.0d0)
  call ZLARFX('L', M, N, V, TAU, C, NMAX, WORK)
  do J = 1, N
    do I = 1, M
      Cpk(I + (J-1)*M) = C(I, J)
    end do
  end do
  call begin_test('left M=3 N=2')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! ============================================================
  ! Test 4: Left side, M=4, N=2 (unrolled M=4)
  ! ============================================================
  C = (0.0d0, 0.0d0); V = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  M = 4; N = 2
  V(1) = (1.0d0, 0.0d0); V(2) = (0.3d0, 0.1d0)
  V(3) = (-0.4d0, 0.2d0); V(4) = (0.2d0, -0.3d0)
  TAU = (1.4d0, -0.1d0)
  do J = 1, N
    do I = 1, M
      C(I, J) = dcmplx(dble(I + (J-1)*M), 0.1d0*dble(I - J))
    end do
  end do
  call ZLARFX('L', M, N, V, TAU, C, NMAX, WORK)
  do J = 1, N
    do I = 1, M
      Cpk(I + (J-1)*M) = C(I, J)
    end do
  end do
  call begin_test('left M=4 N=2')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! ============================================================
  ! Test 5: Left side, M=5, N=2 (unrolled M=5)
  ! ============================================================
  C = (0.0d0, 0.0d0); V = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  M = 5; N = 2
  V(1) = (1.0d0, 0.0d0); V(2) = (0.2d0, -0.3d0)
  V(3) = (-0.3d0, 0.1d0); V(4) = (0.4d0, 0.2d0); V(5) = (-0.1d0, -0.4d0)
  TAU = (1.5d0, 0.2d0)
  do J = 1, N
    do I = 1, M
      C(I, J) = dcmplx(dble(I + (J-1)*M), 0.1d0*dble(I))
    end do
  end do
  call ZLARFX('L', M, N, V, TAU, C, NMAX, WORK)
  do J = 1, N
    do I = 1, M
      Cpk(I + (J-1)*M) = C(I, J)
    end do
  end do
  call begin_test('left M=5 N=2')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! ============================================================
  ! Test 6: Left side, M=6, N=2 (unrolled M=6)
  ! ============================================================
  C = (0.0d0, 0.0d0); V = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  M = 6; N = 2
  V(1) = (1.0d0, 0.0d0); V(2) = (0.2d0, -0.1d0)
  V(3) = (-0.3d0, 0.3d0); V(4) = (0.4d0, -0.2d0)
  V(5) = (-0.1d0, 0.4d0); V(6) = (0.5d0, 0.1d0)
  TAU = (1.3d0, -0.3d0)
  do J = 1, N
    do I = 1, M
      C(I, J) = dcmplx(dble(I + (J-1)*M), 0.2d0*dble(J))
    end do
  end do
  call ZLARFX('L', M, N, V, TAU, C, NMAX, WORK)
  do J = 1, N
    do I = 1, M
      Cpk(I + (J-1)*M) = C(I, J)
    end do
  end do
  call begin_test('left M=6 N=2')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! ============================================================
  ! Test 7: Left side, M=7, N=2 (unrolled M=7)
  ! ============================================================
  C = (0.0d0, 0.0d0); V = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  M = 7; N = 2
  V(1) = (1.0d0, 0.0d0); V(2) = (0.2d0, 0.1d0)
  V(3) = (-0.3d0, -0.2d0); V(4) = (0.4d0, 0.3d0)
  V(5) = (-0.1d0, -0.1d0); V(6) = (0.5d0, 0.2d0)
  V(7) = (-0.2d0, 0.4d0)
  TAU = (1.1d0, 0.5d0)
  do J = 1, N
    do I = 1, M
      C(I, J) = dcmplx(dble(I + (J-1)*M), -0.1d0*dble(I))
    end do
  end do
  call ZLARFX('L', M, N, V, TAU, C, NMAX, WORK)
  do J = 1, N
    do I = 1, M
      Cpk(I + (J-1)*M) = C(I, J)
    end do
  end do
  call begin_test('left M=7 N=2')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! ============================================================
  ! Test 8: Left side, M=8, N=2 (unrolled M=8)
  ! ============================================================
  C = (0.0d0, 0.0d0); V = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  M = 8; N = 2
  V(1) = (1.0d0, 0.0d0); V(2) = (0.1d0, -0.2d0)
  V(3) = (-0.2d0, 0.1d0); V(4) = (0.3d0, 0.3d0)
  V(5) = (-0.4d0, -0.1d0); V(6) = (0.15d0, 0.25d0)
  V(7) = (-0.25d0, -0.15d0); V(8) = (0.35d0, 0.05d0)
  TAU = (1.6d0, -0.4d0)
  do J = 1, N
    do I = 1, M
      C(I, J) = dcmplx(dble(I + (J-1)*M), 0.3d0*dble(I - J))
    end do
  end do
  call ZLARFX('L', M, N, V, TAU, C, NMAX, WORK)
  do J = 1, N
    do I = 1, M
      Cpk(I + (J-1)*M) = C(I, J)
    end do
  end do
  call begin_test('left M=8 N=2')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! ============================================================
  ! Test 9: Left side, M=9, N=2 (unrolled M=9)
  ! ============================================================
  C = (0.0d0, 0.0d0); V = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  M = 9; N = 2
  V(1) = (1.0d0, 0.0d0); V(2) = (0.1d0, 0.2d0)
  V(3) = (-0.2d0, -0.1d0); V(4) = (0.3d0, 0.1d0)
  V(5) = (-0.4d0, 0.2d0); V(6) = (0.15d0, -0.3d0)
  V(7) = (-0.25d0, 0.15d0); V(8) = (0.35d0, -0.05d0)
  V(9) = (-0.05d0, 0.35d0)
  TAU = (1.7d0, 0.1d0)
  do J = 1, N
    do I = 1, M
      C(I, J) = dcmplx(dble(I + (J-1)*M), -0.2d0*dble(J))
    end do
  end do
  call ZLARFX('L', M, N, V, TAU, C, NMAX, WORK)
  do J = 1, N
    do I = 1, M
      Cpk(I + (J-1)*M) = C(I, J)
    end do
  end do
  call begin_test('left M=9 N=2')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! ============================================================
  ! Test 10: Left side, M=10, N=2 (unrolled M=10)
  ! ============================================================
  C = (0.0d0, 0.0d0); V = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  M = 10; N = 2
  do I = 1, 10
    V(I) = dcmplx(0.1d0*dble(I), -0.05d0*dble(I))
  end do
  V(1) = (1.0d0, 0.0d0)
  TAU = (1.8d0, -0.2d0)
  do J = 1, N
    do I = 1, M
      C(I, J) = dcmplx(dble(I) + 0.5d0*dble(J), 0.1d0*dble(I*J))
    end do
  end do
  call ZLARFX('L', M, N, V, TAU, C, NMAX, WORK)
  do J = 1, N
    do I = 1, M
      Cpk(I + (J-1)*M) = C(I, J)
    end do
  end do
  call begin_test('left M=10 N=2')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! ============================================================
  ! Test 11: Right side, M=3, N=1 (unrolled N=1)
  ! ============================================================
  C = (0.0d0, 0.0d0); V = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  M = 3; N = 1
  V(1) = (1.0d0, 0.0d0)
  TAU = (1.5d0, 0.3d0)
  C(1,1) = (2.0d0, 1.0d0); C(2,1) = (3.0d0, -1.0d0); C(3,1) = (4.0d0, 0.5d0)
  call ZLARFX('R', M, N, V, TAU, C, NMAX, WORK)
  do J = 1, N
    do I = 1, M
      Cpk(I + (J-1)*M) = C(I, J)
    end do
  end do
  call begin_test('right M=3 N=1')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! ============================================================
  ! Test 12: Right side, M=3, N=2 (unrolled N=2)
  ! ============================================================
  C = (0.0d0, 0.0d0); V = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  M = 3; N = 2
  V(1) = (1.0d0, 0.0d0); V(2) = (0.5d0, -0.3d0)
  TAU = (1.6d0, 0.2d0)
  C(1,1) = (1.0d0, 0.5d0); C(1,2) = (2.0d0, -1.0d0)
  C(2,1) = (3.0d0, 1.0d0); C(2,2) = (4.0d0, 0.0d0)
  C(3,1) = (5.0d0, -2.0d0); C(3,2) = (6.0d0, 3.0d0)
  call ZLARFX('R', M, N, V, TAU, C, NMAX, WORK)
  do J = 1, N
    do I = 1, M
      Cpk(I + (J-1)*M) = C(I, J)
    end do
  end do
  call begin_test('right M=3 N=2')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! ============================================================
  ! Test 13: Right side, M=2, N=3 (unrolled N=3)
  ! ============================================================
  C = (0.0d0, 0.0d0); V = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  M = 2; N = 3
  V(1) = (1.0d0, 0.0d0); V(2) = (0.3d0, 0.5d0); V(3) = (-0.4d0, -0.2d0)
  TAU = (1.2d0, -0.4d0)
  C(1,1) = (2.0d0, 1.0d0); C(1,2) = (1.0d0, 0.5d0); C(1,3) = (3.0d0, -0.5d0)
  C(2,1) = (4.0d0, -2.0d0); C(2,2) = (5.0d0, 1.0d0); C(2,3) = (6.0d0, 3.0d0)
  call ZLARFX('R', M, N, V, TAU, C, NMAX, WORK)
  do J = 1, N
    do I = 1, M
      Cpk(I + (J-1)*M) = C(I, J)
    end do
  end do
  call begin_test('right M=2 N=3')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! ============================================================
  ! Test 14: Right side, M=2, N=4 (unrolled N=4)
  ! ============================================================
  C = (0.0d0, 0.0d0); V = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  M = 2; N = 4
  V(1) = (1.0d0, 0.0d0); V(2) = (0.3d0, 0.1d0)
  V(3) = (-0.4d0, 0.2d0); V(4) = (0.2d0, -0.3d0)
  TAU = (1.4d0, 0.1d0)
  do J = 1, N
    do I = 1, M
      C(I, J) = dcmplx(dble(I + (J-1)*M), 0.1d0*dble(I + J))
    end do
  end do
  call ZLARFX('R', M, N, V, TAU, C, NMAX, WORK)
  do J = 1, N
    do I = 1, M
      Cpk(I + (J-1)*M) = C(I, J)
    end do
  end do
  call begin_test('right M=2 N=4')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! ============================================================
  ! Test 15: Right side, M=2, N=5 (unrolled N=5)
  ! ============================================================
  C = (0.0d0, 0.0d0); V = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  M = 2; N = 5
  V(1) = (1.0d0, 0.0d0); V(2) = (0.2d0, -0.3d0)
  V(3) = (-0.3d0, 0.1d0); V(4) = (0.4d0, 0.2d0); V(5) = (-0.1d0, -0.4d0)
  TAU = (1.5d0, -0.2d0)
  do J = 1, N
    do I = 1, M
      C(I, J) = dcmplx(dble(I + (J-1)*M), 0.15d0*dble(I*J))
    end do
  end do
  call ZLARFX('R', M, N, V, TAU, C, NMAX, WORK)
  do J = 1, N
    do I = 1, M
      Cpk(I + (J-1)*M) = C(I, J)
    end do
  end do
  call begin_test('right M=2 N=5')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! ============================================================
  ! Test 16: Right side, M=2, N=6 (unrolled N=6)
  ! ============================================================
  C = (0.0d0, 0.0d0); V = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  M = 2; N = 6
  V(1) = (1.0d0, 0.0d0); V(2) = (0.2d0, -0.1d0)
  V(3) = (-0.3d0, 0.3d0); V(4) = (0.4d0, -0.2d0)
  V(5) = (-0.1d0, 0.4d0); V(6) = (0.5d0, 0.1d0)
  TAU = (1.3d0, 0.3d0)
  do J = 1, N
    do I = 1, M
      C(I, J) = dcmplx(dble(I + (J-1)*M), -0.1d0*dble(J))
    end do
  end do
  call ZLARFX('R', M, N, V, TAU, C, NMAX, WORK)
  do J = 1, N
    do I = 1, M
      Cpk(I + (J-1)*M) = C(I, J)
    end do
  end do
  call begin_test('right M=2 N=6')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! ============================================================
  ! Test 17: Right side, M=2, N=7 (unrolled N=7)
  ! ============================================================
  C = (0.0d0, 0.0d0); V = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  M = 2; N = 7
  V(1) = (1.0d0, 0.0d0); V(2) = (0.2d0, 0.1d0)
  V(3) = (-0.3d0, -0.2d0); V(4) = (0.4d0, 0.3d0)
  V(5) = (-0.1d0, -0.1d0); V(6) = (0.5d0, 0.2d0)
  V(7) = (-0.2d0, 0.4d0)
  TAU = (1.1d0, -0.5d0)
  do J = 1, N
    do I = 1, M
      C(I, J) = dcmplx(dble(I + (J-1)*M), 0.2d0*dble(I))
    end do
  end do
  call ZLARFX('R', M, N, V, TAU, C, NMAX, WORK)
  do J = 1, N
    do I = 1, M
      Cpk(I + (J-1)*M) = C(I, J)
    end do
  end do
  call begin_test('right M=2 N=7')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! ============================================================
  ! Test 18: Right side, M=2, N=8 (unrolled N=8)
  ! ============================================================
  C = (0.0d0, 0.0d0); V = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  M = 2; N = 8
  V(1) = (1.0d0, 0.0d0); V(2) = (0.1d0, -0.2d0)
  V(3) = (-0.2d0, 0.1d0); V(4) = (0.3d0, 0.3d0)
  V(5) = (-0.4d0, -0.1d0); V(6) = (0.15d0, 0.25d0)
  V(7) = (-0.25d0, -0.15d0); V(8) = (0.35d0, 0.05d0)
  TAU = (1.6d0, 0.4d0)
  do J = 1, N
    do I = 1, M
      C(I, J) = dcmplx(dble(I + (J-1)*M), -0.15d0*dble(I + J))
    end do
  end do
  call ZLARFX('R', M, N, V, TAU, C, NMAX, WORK)
  do J = 1, N
    do I = 1, M
      Cpk(I + (J-1)*M) = C(I, J)
    end do
  end do
  call begin_test('right M=2 N=8')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! ============================================================
  ! Test 19: Right side, M=2, N=9 (unrolled N=9)
  ! ============================================================
  C = (0.0d0, 0.0d0); V = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  M = 2; N = 9
  V(1) = (1.0d0, 0.0d0); V(2) = (0.1d0, 0.2d0)
  V(3) = (-0.2d0, -0.1d0); V(4) = (0.3d0, 0.1d0)
  V(5) = (-0.4d0, 0.2d0); V(6) = (0.15d0, -0.3d0)
  V(7) = (-0.25d0, 0.15d0); V(8) = (0.35d0, -0.05d0)
  V(9) = (-0.05d0, 0.35d0)
  TAU = (1.7d0, -0.1d0)
  do J = 1, N
    do I = 1, M
      C(I, J) = dcmplx(dble(I + (J-1)*M), 0.1d0*dble(I + J))
    end do
  end do
  call ZLARFX('R', M, N, V, TAU, C, NMAX, WORK)
  do J = 1, N
    do I = 1, M
      Cpk(I + (J-1)*M) = C(I, J)
    end do
  end do
  call begin_test('right M=2 N=9')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! ============================================================
  ! Test 20: Right side, M=2, N=10 (unrolled N=10)
  ! ============================================================
  C = (0.0d0, 0.0d0); V = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  M = 2; N = 10
  do I = 1, 10
    V(I) = dcmplx(0.1d0*dble(I), 0.05d0*dble(I))
  end do
  V(1) = (1.0d0, 0.0d0)
  TAU = (1.8d0, 0.2d0)
  do J = 1, N
    do I = 1, M
      C(I, J) = dcmplx(dble(I) + 0.5d0*dble(J), -0.1d0*dble(I*J))
    end do
  end do
  call ZLARFX('R', M, N, V, TAU, C, NMAX, WORK)
  do J = 1, N
    do I = 1, M
      Cpk(I + (J-1)*M) = C(I, J)
    end do
  end do
  call begin_test('right M=2 N=10')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! ============================================================
  ! Test 21: tau=0 does nothing
  ! ============================================================
  C = (0.0d0, 0.0d0); V = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  M = 3; N = 2
  V(1) = (1.0d0, 0.0d0); V(2) = (0.5d0, 0.3d0); V(3) = (-0.2d0, 0.1d0)
  C(1,1) = (1.0d0, 2.0d0); C(1,2) = (3.0d0, -1.0d0)
  C(2,1) = (4.0d0, 0.5d0); C(2,2) = (5.0d0, 2.0d0)
  C(3,1) = (6.0d0, -3.0d0); C(3,2) = (7.0d0, 1.0d0)
  call ZLARFX('L', M, N, V, (0.0d0, 0.0d0), C, NMAX, WORK)
  do J = 1, N
    do I = 1, M
      Cpk(I + (J-1)*M) = C(I, J)
    end do
  end do
  call begin_test('tau=0')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! ============================================================
  ! Test 22: Left side, M=12 (general case, calls zlarf)
  ! ============================================================
  C = (0.0d0, 0.0d0); V = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  M = 12; N = 3
  do I = 1, M
    V(I) = dcmplx(0.1d0*dble(I), -0.05d0*dble(I))
  end do
  V(1) = (1.0d0, 0.0d0)
  TAU = (1.4d0, -0.3d0)
  do J = 1, N
    do I = 1, M
      C(I, J) = dcmplx(dble(I + (J-1)*M), 0.2d0*dble(I - J))
    end do
  end do
  call ZLARFX('L', M, N, V, TAU, C, NMAX, WORK)
  do J = 1, N
    do I = 1, M
      Cpk(I + (J-1)*M) = C(I, J)
    end do
  end do
  call begin_test('left M=12 N=3 general')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! ============================================================
  ! Test 23: Right side, M=3, N=12 (general case, calls zlarf)
  ! ============================================================
  C = (0.0d0, 0.0d0); V = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0)
  M = 3; N = 12
  do I = 1, N
    V(I) = dcmplx(0.1d0*dble(I), 0.05d0*dble(I))
  end do
  V(1) = (1.0d0, 0.0d0)
  TAU = (1.4d0, 0.3d0)
  do J = 1, N
    do I = 1, M
      C(I, J) = dcmplx(dble(I + (J-1)*M), -0.1d0*dble(I + J))
    end do
  end do
  call ZLARFX('R', M, N, V, TAU, C, NMAX, WORK)
  do J = 1, N
    do I = 1, M
      Cpk(I + (J-1)*M) = C(I, J)
    end do
  end do
  call begin_test('right M=3 N=12 general')
  call print_int('M', M)
  call print_int('N', N)
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

end program
