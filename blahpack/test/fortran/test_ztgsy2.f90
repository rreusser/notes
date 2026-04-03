program test_ztgsy2
  use test_utils
  implicit none

  integer, parameter :: MAXN = 4

  complex*16 :: A(MAXN, MAXN), B(MAXN, MAXN)
  complex*16 :: C(MAXN, MAXN), D(MAXN, MAXN)
  complex*16 :: E(MAXN, MAXN), F(MAXN, MAXN)
  double precision :: SCALE, RDSUM, RDSCAL
  integer :: INFO, M, N, i, j, idx

  ! Pack buffers for printing complex matrices via EQUIVALENCE
  complex*16 :: Cpk(MAXN*MAXN), Fpk(MAXN*MAXN)
  double precision :: Cpk_r(2*MAXN*MAXN), Fpk_r(2*MAXN*MAXN)
  equivalence (Cpk, Cpk_r)
  equivalence (Fpk, Fpk_r)

  ! ========================================
  ! Test 1: TRANS='N', IJOB=0, M=2, N=2
  ! ========================================
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  C = (0.0d0, 0.0d0); D = (0.0d0, 0.0d0)
  E = (0.0d0, 0.0d0); F = (0.0d0, 0.0d0)
  M = 2; N = 2

  A(1,1) = (1.0d0, 0.5d0); A(1,2) = (0.5d0, 0.2d0)
  A(2,2) = (2.0d0, -0.3d0)

  B(1,1) = (3.0d0, 0.1d0); B(1,2) = (0.3d0, -0.1d0)
  B(2,2) = (4.0d0, 0.2d0)

  C(1,1) = (1.0d0, 0.5d0); C(1,2) = (2.0d0, -0.5d0)
  C(2,1) = (3.0d0, 1.0d0); C(2,2) = (4.0d0, 0.3d0)

  D(1,1) = (1.0d0, 0.0d0); D(1,2) = (0.2d0, 0.1d0)
  D(2,2) = (1.5d0, -0.1d0)

  E(1,1) = (1.0d0, 0.0d0); E(1,2) = (0.1d0, 0.05d0)
  E(2,2) = (2.0d0, 0.1d0)

  F(1,1) = (5.0d0, 1.0d0); F(1,2) = (6.0d0, -1.0d0)
  F(2,1) = (7.0d0, 0.5d0); F(2,2) = (8.0d0, 0.2d0)

  call ZTGSY2('N', 0, M, N, A, MAXN, B, MAXN, C, MAXN, &
              D, MAXN, E, MAXN, F, MAXN, SCALE, RDSUM, RDSCAL, INFO)
  do j = 1, N
    do i = 1, M
      idx = i + (j-1)*M
      Cpk(idx) = C(i, j)
      Fpk(idx) = F(i, j)
    end do
  end do
  call begin_test('notrans_2x2_ijob0')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call print_array('C', Cpk_r, 2*M*N)
  call print_array('F', Fpk_r, 2*M*N)
  call end_test()

  ! ========================================
  ! Test 2: TRANS='N', IJOB=0, M=3, N=2
  ! ========================================
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  C = (0.0d0, 0.0d0); D = (0.0d0, 0.0d0)
  E = (0.0d0, 0.0d0); F = (0.0d0, 0.0d0)
  M = 3; N = 2

  A(1,1) = (1.0d0, 0.5d0); A(1,2) = (0.5d0, 0.2d0); A(1,3) = (0.3d0, 0.1d0)
  A(2,2) = (2.0d0, -0.3d0); A(2,3) = (0.2d0, 0.0d0)
  A(3,3) = (3.0d0, 0.4d0)

  B(1,1) = (2.0d0, 0.1d0); B(1,2) = (0.4d0, -0.2d0)
  B(2,2) = (5.0d0, 0.3d0)

  C(1,1) = (1.0d0, 0.5d0); C(1,2) = (2.0d0, -0.5d0)
  C(2,1) = (3.0d0, 1.0d0); C(2,2) = (4.0d0, 0.3d0)
  C(3,1) = (5.0d0, -0.2d0); C(3,2) = (6.0d0, 0.7d0)

  D(1,1) = (1.0d0, 0.0d0); D(1,2) = (0.1d0, 0.05d0); D(1,3) = (0.2d0, 0.1d0)
  D(2,2) = (1.5d0, -0.1d0); D(2,3) = (0.3d0, 0.0d0)
  D(3,3) = (2.0d0, 0.2d0)

  E(1,1) = (1.0d0, 0.0d0); E(1,2) = (0.2d0, 0.1d0)
  E(2,2) = (3.0d0, 0.1d0)

  F(1,1) = (7.0d0, 0.5d0); F(1,2) = (8.0d0, -1.0d0)
  F(2,1) = (9.0d0, 1.0d0); F(2,2) = (10.0d0, 0.3d0)
  F(3,1) = (11.0d0, -0.5d0); F(3,2) = (12.0d0, 0.2d0)

  call ZTGSY2('N', 0, M, N, A, MAXN, B, MAXN, C, MAXN, &
              D, MAXN, E, MAXN, F, MAXN, SCALE, RDSUM, RDSCAL, INFO)
  do j = 1, N
    do i = 1, M
      idx = i + (j-1)*M
      Cpk(idx) = C(i, j)
      Fpk(idx) = F(i, j)
    end do
  end do
  call begin_test('notrans_3x2_ijob0')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call print_array('C', Cpk_r, 2*M*N)
  call print_array('F', Fpk_r, 2*M*N)
  call end_test()

  ! ========================================
  ! Test 3: TRANS='N', IJOB=2, M=2, N=2 (with RDSUM/RDSCAL)
  ! ========================================
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  C = (0.0d0, 0.0d0); D = (0.0d0, 0.0d0)
  E = (0.0d0, 0.0d0); F = (0.0d0, 0.0d0)
  M = 2; N = 2

  A(1,1) = (1.0d0, 0.5d0); A(1,2) = (0.5d0, 0.2d0)
  A(2,2) = (2.0d0, -0.3d0)

  B(1,1) = (3.0d0, 0.1d0); B(1,2) = (0.3d0, -0.1d0)
  B(2,2) = (4.0d0, 0.2d0)

  C(1,1) = (1.0d0, 0.5d0); C(1,2) = (2.0d0, -0.5d0)
  C(2,1) = (3.0d0, 1.0d0); C(2,2) = (4.0d0, 0.3d0)

  D(1,1) = (1.0d0, 0.0d0); D(1,2) = (0.2d0, 0.1d0)
  D(2,2) = (1.5d0, -0.1d0)

  E(1,1) = (1.0d0, 0.0d0); E(1,2) = (0.1d0, 0.05d0)
  E(2,2) = (2.0d0, 0.1d0)

  F(1,1) = (5.0d0, 1.0d0); F(1,2) = (6.0d0, -1.0d0)
  F(2,1) = (7.0d0, 0.5d0); F(2,2) = (8.0d0, 0.2d0)

  RDSUM = 0.0d0; RDSCAL = 1.0d0
  call ZTGSY2('N', 2, M, N, A, MAXN, B, MAXN, C, MAXN, &
              D, MAXN, E, MAXN, F, MAXN, SCALE, RDSUM, RDSCAL, INFO)
  do j = 1, N
    do i = 1, M
      idx = i + (j-1)*M
      Cpk(idx) = C(i, j)
      Fpk(idx) = F(i, j)
    end do
  end do
  call begin_test('notrans_2x2_ijob2')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call print_array('C', Cpk_r, 2*M*N)
  call print_array('F', Fpk_r, 2*M*N)
  call print_scalar('rdsum', RDSUM)
  call print_scalar('rdscal', RDSCAL)
  call end_test()

  ! ========================================
  ! Test 4: TRANS='C' (conjugate-transpose), M=2, N=2
  ! ========================================
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  C = (0.0d0, 0.0d0); D = (0.0d0, 0.0d0)
  E = (0.0d0, 0.0d0); F = (0.0d0, 0.0d0)
  M = 2; N = 2

  A(1,1) = (1.0d0, 0.5d0); A(1,2) = (0.5d0, 0.2d0)
  A(2,2) = (2.0d0, -0.3d0)

  B(1,1) = (3.0d0, 0.1d0); B(1,2) = (0.3d0, -0.1d0)
  B(2,2) = (4.0d0, 0.2d0)

  C(1,1) = (1.0d0, 0.5d0); C(1,2) = (2.0d0, -0.5d0)
  C(2,1) = (3.0d0, 1.0d0); C(2,2) = (4.0d0, 0.3d0)

  D(1,1) = (1.0d0, 0.0d0); D(1,2) = (0.2d0, 0.1d0)
  D(2,2) = (1.5d0, -0.1d0)

  E(1,1) = (1.0d0, 0.0d0); E(1,2) = (0.1d0, 0.05d0)
  E(2,2) = (2.0d0, 0.1d0)

  F(1,1) = (5.0d0, 1.0d0); F(1,2) = (6.0d0, -1.0d0)
  F(2,1) = (7.0d0, 0.5d0); F(2,2) = (8.0d0, 0.2d0)

  call ZTGSY2('C', 0, M, N, A, MAXN, B, MAXN, C, MAXN, &
              D, MAXN, E, MAXN, F, MAXN, SCALE, RDSUM, RDSCAL, INFO)
  do j = 1, N
    do i = 1, M
      idx = i + (j-1)*M
      Cpk(idx) = C(i, j)
      Fpk(idx) = F(i, j)
    end do
  end do
  call begin_test('conjtrans_2x2')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call print_array('C', Cpk_r, 2*M*N)
  call print_array('F', Fpk_r, 2*M*N)
  call end_test()

  ! ========================================
  ! Test 5: TRANS='C', M=3, N=2
  ! ========================================
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  C = (0.0d0, 0.0d0); D = (0.0d0, 0.0d0)
  E = (0.0d0, 0.0d0); F = (0.0d0, 0.0d0)
  M = 3; N = 2

  A(1,1) = (1.0d0, 0.5d0); A(1,2) = (0.5d0, 0.2d0); A(1,3) = (0.3d0, 0.1d0)
  A(2,2) = (2.0d0, -0.3d0); A(2,3) = (0.2d0, 0.0d0)
  A(3,3) = (3.0d0, 0.4d0)

  B(1,1) = (2.0d0, 0.1d0); B(1,2) = (0.4d0, -0.2d0)
  B(2,2) = (5.0d0, 0.3d0)

  C(1,1) = (1.0d0, 0.5d0); C(1,2) = (2.0d0, -0.5d0)
  C(2,1) = (3.0d0, 1.0d0); C(2,2) = (4.0d0, 0.3d0)
  C(3,1) = (5.0d0, -0.2d0); C(3,2) = (6.0d0, 0.7d0)

  D(1,1) = (1.0d0, 0.0d0); D(1,2) = (0.1d0, 0.05d0); D(1,3) = (0.2d0, 0.1d0)
  D(2,2) = (1.5d0, -0.1d0); D(2,3) = (0.3d0, 0.0d0)
  D(3,3) = (2.0d0, 0.2d0)

  E(1,1) = (1.0d0, 0.0d0); E(1,2) = (0.2d0, 0.1d0)
  E(2,2) = (3.0d0, 0.1d0)

  F(1,1) = (7.0d0, 0.5d0); F(1,2) = (8.0d0, -1.0d0)
  F(2,1) = (9.0d0, 1.0d0); F(2,2) = (10.0d0, 0.3d0)
  F(3,1) = (11.0d0, -0.5d0); F(3,2) = (12.0d0, 0.2d0)

  call ZTGSY2('C', 0, M, N, A, MAXN, B, MAXN, C, MAXN, &
              D, MAXN, E, MAXN, F, MAXN, SCALE, RDSUM, RDSCAL, INFO)
  do j = 1, N
    do i = 1, M
      idx = i + (j-1)*M
      Cpk(idx) = C(i, j)
      Fpk(idx) = F(i, j)
    end do
  end do
  call begin_test('conjtrans_3x2')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call print_array('C', Cpk_r, 2*M*N)
  call print_array('F', Fpk_r, 2*M*N)
  call end_test()

  ! ========================================
  ! Test 6: TRANS='N', IJOB=0, M=1, N=2 (edge case)
  ! ========================================
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  C = (0.0d0, 0.0d0); D = (0.0d0, 0.0d0)
  E = (0.0d0, 0.0d0); F = (0.0d0, 0.0d0)
  M = 1; N = 2

  A(1,1) = (3.0d0, 0.5d0)

  B(1,1) = (2.0d0, 0.1d0); B(1,2) = (0.5d0, -0.2d0)
  B(2,2) = (5.0d0, 0.3d0)

  C(1,1) = (1.0d0, 0.5d0); C(1,2) = (2.0d0, -0.3d0)

  D(1,1) = (1.5d0, 0.1d0)

  E(1,1) = (1.0d0, 0.0d0); E(1,2) = (0.1d0, 0.05d0)
  E(2,2) = (3.0d0, 0.2d0)

  F(1,1) = (5.0d0, 1.0d0); F(1,2) = (6.0d0, -0.5d0)

  call ZTGSY2('N', 0, M, N, A, MAXN, B, MAXN, C, MAXN, &
              D, MAXN, E, MAXN, F, MAXN, SCALE, RDSUM, RDSCAL, INFO)
  do j = 1, N
    do i = 1, M
      idx = i + (j-1)*M
      Cpk(idx) = C(i, j)
      Fpk(idx) = F(i, j)
    end do
  end do
  call begin_test('notrans_1x2')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call print_array('C', Cpk_r, 2*M*N)
  call print_array('F', Fpk_r, 2*M*N)
  call end_test()

  ! ========================================
  ! Test 7: TRANS='N', IJOB=0, M=2, N=1 (edge case)
  ! ========================================
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  C = (0.0d0, 0.0d0); D = (0.0d0, 0.0d0)
  E = (0.0d0, 0.0d0); F = (0.0d0, 0.0d0)
  M = 2; N = 1

  A(1,1) = (3.0d0, 0.5d0); A(1,2) = (0.5d0, 0.1d0)
  A(2,2) = (7.0d0, -0.2d0)

  B(1,1) = (2.0d0, 0.3d0)

  C(1,1) = (1.0d0, 0.5d0)
  C(2,1) = (4.0d0, -0.3d0)

  D(1,1) = (1.0d0, 0.0d0); D(1,2) = (0.1d0, 0.05d0)
  D(2,2) = (1.5d0, 0.1d0)

  E(1,1) = (1.0d0, 0.0d0)

  F(1,1) = (5.0d0, 1.0d0)
  F(2,1) = (8.0d0, -0.5d0)

  call ZTGSY2('N', 0, M, N, A, MAXN, B, MAXN, C, MAXN, &
              D, MAXN, E, MAXN, F, MAXN, SCALE, RDSUM, RDSCAL, INFO)
  do j = 1, N
    do i = 1, M
      idx = i + (j-1)*M
      Cpk(idx) = C(i, j)
      Fpk(idx) = F(i, j)
    end do
  end do
  call begin_test('notrans_2x1')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call print_array('C', Cpk_r, 2*M*N)
  call print_array('F', Fpk_r, 2*M*N)
  call end_test()

  ! ========================================
  ! Test 8: TRANS='N', IJOB=0, M=3, N=3
  ! ========================================
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  C = (0.0d0, 0.0d0); D = (0.0d0, 0.0d0)
  E = (0.0d0, 0.0d0); F = (0.0d0, 0.0d0)
  M = 3; N = 3

  A(1,1) = (1.0d0, 0.5d0); A(1,2) = (0.4d0, 0.1d0); A(1,3) = (0.1d0, 0.05d0)
  A(2,2) = (2.0d0, -0.3d0); A(2,3) = (0.2d0, -0.1d0)
  A(3,3) = (5.0d0, 0.4d0)

  B(1,1) = (2.0d0, 0.1d0); B(1,2) = (0.3d0, -0.1d0); B(1,3) = (0.1d0, 0.05d0)
  B(2,2) = (3.0d0, 0.2d0); B(2,3) = (0.2d0, -0.1d0)
  B(3,3) = (6.0d0, -0.3d0)

  C(1,1) = (1.0d0, 0.5d0); C(1,2) = (2.0d0, -0.5d0); C(1,3) = (3.0d0, 0.2d0)
  C(2,1) = (4.0d0, 1.0d0); C(2,2) = (5.0d0, 0.3d0);  C(2,3) = (6.0d0, -0.4d0)
  C(3,1) = (7.0d0, -0.2d0); C(3,2) = (8.0d0, 0.7d0); C(3,3) = (9.0d0, 0.1d0)

  D(1,1) = (1.0d0, 0.0d0); D(1,2) = (0.1d0, 0.05d0); D(1,3) = (0.05d0, 0.02d0)
  D(2,2) = (1.5d0, -0.1d0); D(2,3) = (0.2d0, 0.1d0)
  D(3,3) = (2.0d0, 0.2d0)

  E(1,1) = (1.0d0, 0.0d0); E(1,2) = (0.2d0, 0.1d0); E(1,3) = (0.1d0, -0.05d0)
  E(2,2) = (2.5d0, 0.1d0); E(2,3) = (0.15d0, 0.05d0)
  E(3,3) = (3.0d0, 0.3d0)

  F(1,1) = (10.0d0, 0.5d0); F(1,2) = (11.0d0, -1.0d0); F(1,3) = (12.0d0, 0.3d0)
  F(2,1) = (13.0d0, 1.0d0); F(2,2) = (14.0d0, 0.3d0);  F(2,3) = (15.0d0, -0.5d0)
  F(3,1) = (16.0d0, -0.5d0); F(3,2) = (17.0d0, 0.2d0); F(3,3) = (18.0d0, 0.1d0)

  call ZTGSY2('N', 0, M, N, A, MAXN, B, MAXN, C, MAXN, &
              D, MAXN, E, MAXN, F, MAXN, SCALE, RDSUM, RDSCAL, INFO)
  do j = 1, N
    do i = 1, M
      idx = i + (j-1)*M
      Cpk(idx) = C(i, j)
      Fpk(idx) = F(i, j)
    end do
  end do
  call begin_test('notrans_3x3_ijob0')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call print_array('C', Cpk_r, 2*M*N)
  call print_array('F', Fpk_r, 2*M*N)
  call end_test()

  ! ========================================
  ! Test 9: TRANS='C', M=3, N=3
  ! ========================================
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  C = (0.0d0, 0.0d0); D = (0.0d0, 0.0d0)
  E = (0.0d0, 0.0d0); F = (0.0d0, 0.0d0)
  M = 3; N = 3

  A(1,1) = (1.0d0, 0.5d0); A(1,2) = (0.4d0, 0.1d0); A(1,3) = (0.1d0, 0.05d0)
  A(2,2) = (2.0d0, -0.3d0); A(2,3) = (0.2d0, -0.1d0)
  A(3,3) = (5.0d0, 0.4d0)

  B(1,1) = (2.0d0, 0.1d0); B(1,2) = (0.3d0, -0.1d0); B(1,3) = (0.1d0, 0.05d0)
  B(2,2) = (3.0d0, 0.2d0); B(2,3) = (0.2d0, -0.1d0)
  B(3,3) = (6.0d0, -0.3d0)

  C(1,1) = (1.0d0, 0.5d0); C(1,2) = (2.0d0, -0.5d0); C(1,3) = (3.0d0, 0.2d0)
  C(2,1) = (4.0d0, 1.0d0); C(2,2) = (5.0d0, 0.3d0);  C(2,3) = (6.0d0, -0.4d0)
  C(3,1) = (7.0d0, -0.2d0); C(3,2) = (8.0d0, 0.7d0); C(3,3) = (9.0d0, 0.1d0)

  D(1,1) = (1.0d0, 0.0d0); D(1,2) = (0.1d0, 0.05d0); D(1,3) = (0.05d0, 0.02d0)
  D(2,2) = (1.5d0, -0.1d0); D(2,3) = (0.2d0, 0.1d0)
  D(3,3) = (2.0d0, 0.2d0)

  E(1,1) = (1.0d0, 0.0d0); E(1,2) = (0.2d0, 0.1d0); E(1,3) = (0.1d0, -0.05d0)
  E(2,2) = (2.5d0, 0.1d0); E(2,3) = (0.15d0, 0.05d0)
  E(3,3) = (3.0d0, 0.3d0)

  F(1,1) = (10.0d0, 0.5d0); F(1,2) = (11.0d0, -1.0d0); F(1,3) = (12.0d0, 0.3d0)
  F(2,1) = (13.0d0, 1.0d0); F(2,2) = (14.0d0, 0.3d0);  F(2,3) = (15.0d0, -0.5d0)
  F(3,1) = (16.0d0, -0.5d0); F(3,2) = (17.0d0, 0.2d0); F(3,3) = (18.0d0, 0.1d0)

  call ZTGSY2('C', 0, M, N, A, MAXN, B, MAXN, C, MAXN, &
              D, MAXN, E, MAXN, F, MAXN, SCALE, RDSUM, RDSCAL, INFO)
  do j = 1, N
    do i = 1, M
      idx = i + (j-1)*M
      Cpk(idx) = C(i, j)
      Fpk(idx) = F(i, j)
    end do
  end do
  call begin_test('conjtrans_3x3')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call print_array('C', Cpk_r, 2*M*N)
  call print_array('F', Fpk_r, 2*M*N)
  call end_test()

  ! ========================================
  ! Test 10: TRANS='N', IJOB=2, M=3, N=2
  ! ========================================
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  C = (0.0d0, 0.0d0); D = (0.0d0, 0.0d0)
  E = (0.0d0, 0.0d0); F = (0.0d0, 0.0d0)
  M = 3; N = 2

  A(1,1) = (1.0d0, 0.5d0); A(1,2) = (0.5d0, 0.2d0); A(1,3) = (0.3d0, 0.1d0)
  A(2,2) = (2.0d0, -0.3d0); A(2,3) = (0.2d0, 0.0d0)
  A(3,3) = (3.0d0, 0.4d0)

  B(1,1) = (2.0d0, 0.1d0); B(1,2) = (0.4d0, -0.2d0)
  B(2,2) = (5.0d0, 0.3d0)

  C(1,1) = (1.0d0, 0.5d0); C(1,2) = (2.0d0, -0.5d0)
  C(2,1) = (3.0d0, 1.0d0); C(2,2) = (4.0d0, 0.3d0)
  C(3,1) = (5.0d0, -0.2d0); C(3,2) = (6.0d0, 0.7d0)

  D(1,1) = (1.0d0, 0.0d0); D(1,2) = (0.1d0, 0.05d0); D(1,3) = (0.2d0, 0.1d0)
  D(2,2) = (1.5d0, -0.1d0); D(2,3) = (0.3d0, 0.0d0)
  D(3,3) = (2.0d0, 0.2d0)

  E(1,1) = (1.0d0, 0.0d0); E(1,2) = (0.2d0, 0.1d0)
  E(2,2) = (3.0d0, 0.1d0)

  F(1,1) = (7.0d0, 0.5d0); F(1,2) = (8.0d0, -1.0d0)
  F(2,1) = (9.0d0, 1.0d0); F(2,2) = (10.0d0, 0.3d0)
  F(3,1) = (11.0d0, -0.5d0); F(3,2) = (12.0d0, 0.2d0)

  RDSUM = 1.0d0; RDSCAL = 1.0d0
  call ZTGSY2('N', 2, M, N, A, MAXN, B, MAXN, C, MAXN, &
              D, MAXN, E, MAXN, F, MAXN, SCALE, RDSUM, RDSCAL, INFO)
  do j = 1, N
    do i = 1, M
      idx = i + (j-1)*M
      Cpk(idx) = C(i, j)
      Fpk(idx) = F(i, j)
    end do
  end do
  call begin_test('notrans_3x2_ijob2')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call print_array('C', Cpk_r, 2*M*N)
  call print_array('F', Fpk_r, 2*M*N)
  call print_scalar('rdsum', RDSUM)
  call print_scalar('rdscal', RDSCAL)
  call end_test()

  ! ========================================
  ! Test 11: TRANS='C', M=1, N=1 (smallest case)
  ! ========================================
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  C = (0.0d0, 0.0d0); D = (0.0d0, 0.0d0)
  E = (0.0d0, 0.0d0); F = (0.0d0, 0.0d0)
  M = 1; N = 1

  A(1,1) = (2.0d0, 1.0d0)
  B(1,1) = (3.0d0, -0.5d0)
  C(1,1) = (1.0d0, 0.5d0)
  D(1,1) = (1.0d0, 0.0d0)
  E(1,1) = (1.0d0, 0.0d0)
  F(1,1) = (4.0d0, 2.0d0)

  call ZTGSY2('C', 0, M, N, A, MAXN, B, MAXN, C, MAXN, &
              D, MAXN, E, MAXN, F, MAXN, SCALE, RDSUM, RDSCAL, INFO)
  Cpk(1) = C(1,1)
  Fpk(1) = F(1,1)
  call begin_test('conjtrans_1x1')
  call print_int('info', INFO)
  call print_scalar('scale', SCALE)
  call print_array('C', Cpk_r, 2)
  call print_array('F', Fpk_r, 2)
  call end_test()

end program
