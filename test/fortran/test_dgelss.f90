program test_dgelss
  use test_utils
  implicit none

  ! Max sizes: A up to 6x6, B up to 6x3, S up to 6
  double precision :: A(36), B(18), S(6), WORK(1000)
  integer :: info, rank, lwork, i

  lwork = 1000

  ! ============================================================
  ! Test 1: Overdetermined full rank (4x2), single RHS
  ! A = [1 2; 3 4; 5 6; 7 8] (column-major), b = [1; 2; 3; 4]
  ! ============================================================
  A = 0.0d0
  B = 0.0d0
  A(1) = 1.0d0; A(2) = 3.0d0; A(3) = 5.0d0; A(4) = 7.0d0
  A(5) = 2.0d0; A(6) = 4.0d0; A(7) = 6.0d0; A(8) = 8.0d0
  B(1) = 1.0d0; B(2) = 2.0d0; B(3) = 3.0d0; B(4) = 4.0d0
  call dgelss(4, 2, 1, A, 4, B, 4, S, -1.0d0, rank, WORK, lwork, info)
  call begin_test('overdetermined_full_rank')
  call print_array('x', B, 2)
  call print_array('s', S, 2)
  call print_int('rank', rank)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 2: Overdetermined rank-deficient (4x2)
  ! A has rank 1: columns are proportional
  ! A = [1 2; 2 4; 3 6; 4 8], b = [1; 2; 3; 4]
  ! ============================================================
  A = 0.0d0
  B = 0.0d0
  A(1) = 1.0d0; A(2) = 2.0d0; A(3) = 3.0d0; A(4) = 4.0d0
  A(5) = 2.0d0; A(6) = 4.0d0; A(7) = 6.0d0; A(8) = 8.0d0
  B(1) = 1.0d0; B(2) = 2.0d0; B(3) = 3.0d0; B(4) = 4.0d0
  call dgelss(4, 2, 1, A, 4, B, 4, S, 0.01d0, rank, WORK, lwork, info)
  call begin_test('overdetermined_rank_deficient')
  call print_array('x', B, 2)
  call print_array('s', S, 2)
  call print_int('rank', rank)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 3: Underdetermined (2x4), single RHS
  ! A = [1 0 0 0; 0 1 0 0], b = [1; 2]
  ! Minimum norm solution: x = [1; 2; 0; 0]
  ! ============================================================
  A = 0.0d0
  B = 0.0d0
  A(1) = 1.0d0  ! A(1,1)
  A(4) = 1.0d0  ! A(2,2)
  B(1) = 1.0d0; B(2) = 2.0d0
  ! B must be sized max(M,N) x NRHS = 4x1, LDB=4
  call dgelss(2, 4, 1, A, 2, B, 4, S, -1.0d0, rank, WORK, lwork, info)
  call begin_test('underdetermined')
  call print_array('x', B, 4)
  call print_array('s', S, 2)
  call print_int('rank', rank)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 4: Square 3x3, single RHS
  ! A = [2 1 0; 1 3 1; 0 1 2], b = [1; 2; 3]
  ! Well-conditioned symmetric positive definite
  ! ============================================================
  A = 0.0d0
  B = 0.0d0
  ! Column 1
  A(1) = 2.0d0; A(2) = 1.0d0; A(3) = 0.0d0
  ! Column 2
  A(4) = 1.0d0; A(5) = 3.0d0; A(6) = 1.0d0
  ! Column 3
  A(7) = 0.0d0; A(8) = 1.0d0; A(9) = 2.0d0
  B(1) = 1.0d0; B(2) = 2.0d0; B(3) = 3.0d0
  call dgelss(3, 3, 1, A, 3, B, 3, S, -1.0d0, rank, WORK, lwork, info)
  call begin_test('square_3x3')
  call print_array('x', B, 3)
  call print_array('s', S, 3)
  call print_int('rank', rank)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 5: Multiple RHS (3x3, 2 RHS)
  ! A = [4 1 0; 1 3 1; 0 1 4], B = [1 4; 2 5; 3 6]
  ! ============================================================
  A = 0.0d0
  B = 0.0d0
  ! Column 1
  A(1) = 4.0d0; A(2) = 1.0d0; A(3) = 0.0d0
  ! Column 2
  A(4) = 1.0d0; A(5) = 3.0d0; A(6) = 1.0d0
  ! Column 3
  A(7) = 0.0d0; A(8) = 1.0d0; A(9) = 4.0d0
  ! RHS column 1
  B(1) = 1.0d0; B(2) = 2.0d0; B(3) = 3.0d0
  ! RHS column 2
  B(4) = 4.0d0; B(5) = 5.0d0; B(6) = 6.0d0
  call dgelss(3, 3, 2, A, 3, B, 3, S, -1.0d0, rank, WORK, lwork, info)
  call begin_test('multiple_rhs')
  call print_array('x', B, 6)
  call print_array('s', S, 3)
  call print_int('rank', rank)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 6: M=0 edge case
  ! ============================================================
  A = 0.0d0
  B = 0.0d0
  call dgelss(0, 3, 1, A, 1, B, 3, S, -1.0d0, rank, WORK, lwork, info)
  call begin_test('m_zero')
  call print_int('rank', rank)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 7: N=0 edge case
  ! ============================================================
  A = 0.0d0
  B = 0.0d0
  call dgelss(3, 0, 1, A, 3, B, 3, S, -1.0d0, rank, WORK, lwork, info)
  call begin_test('n_zero')
  call print_int('rank', rank)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 8: Overdetermined 6x2 with M >> N (triggers QR preconditioning path)
  ! Uses M=6, N=2 so M/N=3 > MNTHR_RATIO ~1.6
  ! A = [1 0; 0 1; 1 1; 2 1; 1 2; 0 0], b = [1; 1; 2; 3; 3; 0]
  ! ============================================================
  A = 0.0d0
  B = 0.0d0
  ! Column 1
  A(1) = 1.0d0; A(2) = 0.0d0; A(3) = 1.0d0; A(4) = 2.0d0; A(5) = 1.0d0; A(6) = 0.0d0
  ! Column 2
  A(7) = 0.0d0; A(8) = 1.0d0; A(9) = 1.0d0; A(10) = 1.0d0; A(11) = 2.0d0; A(12) = 0.0d0
  B(1) = 1.0d0; B(2) = 1.0d0; B(3) = 2.0d0; B(4) = 3.0d0; B(5) = 3.0d0; B(6) = 0.0d0
  call dgelss(6, 2, 1, A, 6, B, 6, S, -1.0d0, rank, WORK, lwork, info)
  call begin_test('overdetermined_tall')
  call print_array('x', B, 2)
  call print_array('s', S, 2)
  call print_int('rank', rank)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 9: Underdetermined with M << N (triggers LQ path)
  ! M=2, N=6
  ! ============================================================
  A = 0.0d0
  B = 0.0d0
  ! Row 1: [1 0 1 0 0 0]
  A(1) = 1.0d0; A(3) = 1.0d0
  ! Row 2: [0 1 0 1 0 0]
  A(4) = 1.0d0; A(6) = 1.0d0
  B(1) = 2.0d0; B(2) = 4.0d0
  ! LDB must be >= max(M,N) = 6
  call dgelss(2, 6, 1, A, 2, B, 6, S, -1.0d0, rank, WORK, lwork, info)
  call begin_test('underdetermined_wide')
  call print_array('x', B, 6)
  call print_array('s', S, 2)
  call print_int('rank', rank)
  call print_int('info', info)
  call end_test()

end program
