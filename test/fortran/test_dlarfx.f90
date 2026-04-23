program test_dlarfx
  use test_utils
  implicit none

  double precision :: C(10, 10), V(10), WORK(10)
  double precision :: TAU
  integer :: I, J

  ! ============================================================
  ! Test 1: Left side, M=2, N=3
  ! ============================================================
  C = 0.0d0; V = 0.0d0; WORK = 0.0d0
  V(1) = 1.0d0; V(2) = 0.5d0
  TAU = 1.6d0
  C(1,1) = 1.0d0; C(1,2) = 2.0d0; C(1,3) = 3.0d0
  C(2,1) = 4.0d0; C(2,2) = 5.0d0; C(2,3) = 6.0d0
  call DLARFX('L', 2, 3, V, TAU, C, 10, WORK)
  call begin_test('left M=2 N=3')
  call print_matrix('C', C, 10, 2, 3)
  call end_test()

  ! ============================================================
  ! Test 2: Right side, M=3, N=2
  ! ============================================================
  C = 0.0d0; V = 0.0d0; WORK = 0.0d0
  V(1) = 1.0d0; V(2) = 0.5d0
  TAU = 1.6d0
  C(1,1) = 1.0d0; C(1,2) = 2.0d0
  C(2,1) = 3.0d0; C(2,2) = 4.0d0
  C(3,1) = 5.0d0; C(3,2) = 6.0d0
  call DLARFX('R', 3, 2, V, TAU, C, 10, WORK)
  call begin_test('right M=3 N=2')
  call print_matrix('C', C, 10, 3, 2)
  call end_test()

  ! ============================================================
  ! Test 3: tau=0 does nothing
  ! ============================================================
  C = 0.0d0; V = 0.0d0; WORK = 0.0d0
  V(1) = 1.0d0; V(2) = 0.5d0
  C(1,1) = 1.0d0; C(1,2) = 2.0d0
  C(2,1) = 3.0d0; C(2,2) = 4.0d0
  call DLARFX('L', 2, 2, V, 0.0d0, C, 10, WORK)
  call begin_test('tau=0')
  call print_matrix('C', C, 10, 2, 2)
  call end_test()

  ! ============================================================
  ! Test 4: Left side, M=3 (unrolled size 3 path)
  ! ============================================================
  C = 0.0d0; V = 0.0d0; WORK = 0.0d0
  V(1) = 1.0d0; V(2) = 0.3d0; V(3) = -0.5d0
  TAU = 1.2d0
  C(1,1) = 2.0d0; C(1,2) = 1.0d0
  C(2,1) = 3.0d0; C(2,2) = 4.0d0
  C(3,1) = 5.0d0; C(3,2) = 6.0d0
  call DLARFX('L', 3, 2, V, TAU, C, 10, WORK)
  call begin_test('left M=3 N=2')
  call print_matrix('C', C, 10, 3, 2)
  call end_test()

  ! ============================================================
  ! Test 5: Right side, M=2, N=3 (unrolled size 3 path)
  ! ============================================================
  C = 0.0d0; V = 0.0d0; WORK = 0.0d0
  V(1) = 1.0d0; V(2) = 0.3d0; V(3) = -0.5d0
  TAU = 1.2d0
  C(1,1) = 2.0d0; C(1,2) = 1.0d0; C(1,3) = 3.0d0
  C(2,1) = 4.0d0; C(2,2) = 5.0d0; C(2,3) = 6.0d0
  call DLARFX('R', 2, 3, V, TAU, C, 10, WORK)
  call begin_test('right M=2 N=3')
  call print_matrix('C', C, 10, 2, 3)
  call end_test()

  ! ============================================================
  ! Test 6: Left side, M=5 (unrolled size 5 path)
  ! ============================================================
  C = 0.0d0; V = 0.0d0; WORK = 0.0d0
  V(1) = 1.0d0; V(2) = 0.2d0; V(3) = -0.3d0; V(4) = 0.4d0; V(5) = -0.1d0
  TAU = 1.5d0
  do J = 1, 3
    do I = 1, 5
      C(I, J) = dble(I + (J-1)*5)
    end do
  end do
  call DLARFX('L', 5, 3, V, TAU, C, 10, WORK)
  call begin_test('left M=5 N=3')
  call print_matrix('C', C, 10, 5, 3)
  call end_test()

  ! ============================================================
  ! Test 7: Left side, M=10 (unrolled size 10 path)
  ! ============================================================
  C = 0.0d0; V = 0.0d0; WORK = 0.0d0
  do I = 1, 10
    V(I) = 0.1d0 * dble(I)
  end do
  V(1) = 1.0d0
  TAU = 1.8d0
  do J = 1, 4
    do I = 1, 10
      C(I, J) = dble(I) + 0.5d0 * dble(J)
    end do
  end do
  call DLARFX('L', 10, 4, V, TAU, C, 10, WORK)
  call begin_test('left M=10 N=4')
  call print_matrix('C', C, 10, 10, 4)
  call end_test()

  ! ============================================================
  ! Test 8: Right side, M=4, N=10 (unrolled size 10 path)
  ! ============================================================
  C = 0.0d0; V = 0.0d0; WORK = 0.0d0
  do I = 1, 10
    V(I) = 0.1d0 * dble(I)
  end do
  V(1) = 1.0d0
  TAU = 1.8d0
  do J = 1, 10
    do I = 1, 4
      C(I, J) = dble(I) + 0.5d0 * dble(J)
    end do
  end do
  call DLARFX('R', 4, 10, V, TAU, C, 10, WORK)
  call begin_test('right M=4 N=10')
  call print_matrix('C', C, 10, 4, 10)
  call end_test()

  ! ============================================================
  ! Test 9: Left side, M=4 (unrolled size 4)
  ! ============================================================
  C = 0.0d0; V = 0.0d0; WORK = 0.0d0
  V(1) = 1.0d0; V(2) = 0.3d0; V(3) = -0.4d0; V(4) = 0.2d0
  TAU = 1.4d0
  do J = 1, 2
    do I = 1, 4
      C(I, J) = dble(I + (J-1)*4)
    end do
  end do
  call DLARFX('L', 4, 2, V, TAU, C, 10, WORK)
  call begin_test('left M=4 N=2')
  call print_matrix('C', C, 10, 4, 2)
  call end_test()

  ! ============================================================
  ! Test 10: Left side, M=6 (unrolled size 6)
  ! ============================================================
  C = 0.0d0; V = 0.0d0; WORK = 0.0d0
  V(1) = 1.0d0; V(2) = 0.2d0; V(3) = -0.3d0
  V(4) = 0.4d0; V(5) = -0.1d0; V(6) = 0.5d0
  TAU = 1.3d0
  do J = 1, 3
    do I = 1, 6
      C(I, J) = dble(I + (J-1)*6)
    end do
  end do
  call DLARFX('L', 6, 3, V, TAU, C, 10, WORK)
  call begin_test('left M=6 N=3')
  call print_matrix('C', C, 10, 6, 3)
  call end_test()

  ! ============================================================
  ! Test 11: Left side, M=7 (unrolled size 7)
  ! ============================================================
  C = 0.0d0; V = 0.0d0; WORK = 0.0d0
  V(1) = 1.0d0; V(2) = 0.2d0; V(3) = -0.3d0
  V(4) = 0.4d0; V(5) = -0.1d0; V(6) = 0.5d0; V(7) = -0.2d0
  TAU = 1.1d0
  do J = 1, 2
    do I = 1, 7
      C(I, J) = dble(I + (J-1)*7)
    end do
  end do
  call DLARFX('L', 7, 2, V, TAU, C, 10, WORK)
  call begin_test('left M=7 N=2')
  call print_matrix('C', C, 10, 7, 2)
  call end_test()

  ! ============================================================
  ! Test 12: Left side, M=8 (unrolled size 8)
  ! ============================================================
  C = 0.0d0; V = 0.0d0; WORK = 0.0d0
  V(1) = 1.0d0; V(2) = 0.1d0; V(3) = -0.2d0
  V(4) = 0.3d0; V(5) = -0.4d0; V(6) = 0.15d0; V(7) = -0.25d0; V(8) = 0.35d0
  TAU = 1.6d0
  do J = 1, 2
    do I = 1, 8
      C(I, J) = dble(I + (J-1)*8)
    end do
  end do
  call DLARFX('L', 8, 2, V, TAU, C, 10, WORK)
  call begin_test('left M=8 N=2')
  call print_matrix('C', C, 10, 8, 2)
  call end_test()

  ! ============================================================
  ! Test 13: Left side, M=9 (unrolled size 9)
  ! ============================================================
  C = 0.0d0; V = 0.0d0; WORK = 0.0d0
  V(1) = 1.0d0; V(2) = 0.1d0; V(3) = -0.2d0
  V(4) = 0.3d0; V(5) = -0.4d0; V(6) = 0.15d0
  V(7) = -0.25d0; V(8) = 0.35d0; V(9) = -0.05d0
  TAU = 1.7d0
  do J = 1, 2
    do I = 1, 9
      C(I, J) = dble(I + (J-1)*9)
    end do
  end do
  call DLARFX('L', 9, 2, V, TAU, C, 10, WORK)
  call begin_test('left M=9 N=2')
  call print_matrix('C', C, 10, 9, 2)
  call end_test()

end program
