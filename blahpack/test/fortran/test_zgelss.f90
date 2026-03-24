program test_zgelss
  use test_utils
  implicit none

  ! Max sizes: A up to 6x6, B up to 6x3, S up to 6
  complex*16 :: A(36), B(18), WORK(2000)
  double precision :: A_r(72), B_r(36), S(6), RWORK(100)
  equivalence (A, A_r)
  equivalence (B, B_r)
  integer :: info, rank, lwork, i

  lwork = 2000

  ! ============================================================
  ! Test 1: Overdetermined full rank (4x2), single RHS
  ! A = [1+i, 2; 3, 4-i; 5+2i, 6; 7, 8+i], b = [1+i; 2; 3-i; 4]
  ! ============================================================
  A = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  ! Column 1
  A(1) = (1.0d0, 1.0d0); A(2) = (3.0d0, 0.0d0)
  A(3) = (5.0d0, 2.0d0); A(4) = (7.0d0, 0.0d0)
  ! Column 2
  A(5) = (2.0d0, 0.0d0); A(6) = (4.0d0, -1.0d0)
  A(7) = (6.0d0, 0.0d0); A(8) = (8.0d0, 1.0d0)
  B(1) = (1.0d0, 1.0d0); B(2) = (2.0d0, 0.0d0)
  B(3) = (3.0d0, -1.0d0); B(4) = (4.0d0, 0.0d0)
  call zgelss(4, 2, 1, A, 4, B, 4, S, -1.0d0, rank, WORK, lwork, RWORK, info)
  call begin_test('overdetermined_full_rank')
  call print_array('x', B_r, 4)
  call print_array('s', S, 2)
  call print_int('rank', rank)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 2: Overdetermined rank-deficient (4x2)
  ! A has rank 1: columns are proportional (col2 = 2*col1)
  ! ============================================================
  A = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  A(1) = (1.0d0, 0.0d0); A(2) = (2.0d0, 1.0d0)
  A(3) = (3.0d0, 0.0d0); A(4) = (4.0d0, -1.0d0)
  A(5) = (2.0d0, 0.0d0); A(6) = (4.0d0, 2.0d0)
  A(7) = (6.0d0, 0.0d0); A(8) = (8.0d0, -2.0d0)
  B(1) = (1.0d0, 0.0d0); B(2) = (2.0d0, 1.0d0)
  B(3) = (3.0d0, 0.0d0); B(4) = (4.0d0, -1.0d0)
  call zgelss(4, 2, 1, A, 4, B, 4, S, 0.01d0, rank, WORK, lwork, RWORK, info)
  call begin_test('overdetermined_rank_deficient')
  call print_array('x', B_r, 4)
  call print_array('s', S, 2)
  call print_int('rank', rank)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 3: Underdetermined (2x4), single RHS
  ! A = [1 0 0 0; 0 1+i 0 0], b = [1+i; 2]
  ! ============================================================
  A = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  A(1) = (1.0d0, 0.0d0)  ! A(1,1)
  A(4) = (1.0d0, 1.0d0)  ! A(2,2)
  B(1) = (1.0d0, 1.0d0); B(2) = (2.0d0, 0.0d0)
  ! B must be sized max(M,N) x NRHS = 4x1, LDB=4
  call zgelss(2, 4, 1, A, 2, B, 4, S, -1.0d0, rank, WORK, lwork, RWORK, info)
  call begin_test('underdetermined')
  call print_array('x', B_r, 8)
  call print_array('s', S, 2)
  call print_int('rank', rank)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 4: Square 3x3, single RHS, Hermitian positive definite
  ! A = [4 1-i 0; 1+i 5 2-i; 0 2+i 6], b = [1+i; 2; 3-i]
  ! ============================================================
  A = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  ! Column 1
  A(1) = (4.0d0, 0.0d0); A(2) = (1.0d0, 1.0d0); A(3) = (0.0d0, 0.0d0)
  ! Column 2
  A(4) = (1.0d0, -1.0d0); A(5) = (5.0d0, 0.0d0); A(6) = (2.0d0, 1.0d0)
  ! Column 3
  A(7) = (0.0d0, 0.0d0); A(8) = (2.0d0, -1.0d0); A(9) = (6.0d0, 0.0d0)
  B(1) = (1.0d0, 1.0d0); B(2) = (2.0d0, 0.0d0); B(3) = (3.0d0, -1.0d0)
  call zgelss(3, 3, 1, A, 3, B, 3, S, -1.0d0, rank, WORK, lwork, RWORK, info)
  call begin_test('square_3x3')
  call print_array('x', B_r, 6)
  call print_array('s', S, 3)
  call print_int('rank', rank)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 5: Multiple RHS (3x3, 2 RHS)
  ! A = [4 1 0; 1 5 2; 0 2 6], B = [1+i,2; 2,3-i; 3,4+i]
  ! ============================================================
  A = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  A(1) = (4.0d0, 0.0d0); A(2) = (1.0d0, 0.0d0); A(3) = (0.0d0, 0.0d0)
  A(4) = (1.0d0, 0.0d0); A(5) = (5.0d0, 0.0d0); A(6) = (2.0d0, 0.0d0)
  A(7) = (0.0d0, 0.0d0); A(8) = (2.0d0, 0.0d0); A(9) = (6.0d0, 0.0d0)
  ! RHS col 1
  B(1) = (1.0d0, 1.0d0); B(2) = (2.0d0, 0.0d0); B(3) = (3.0d0, 0.0d0)
  ! RHS col 2
  B(4) = (2.0d0, 0.0d0); B(5) = (3.0d0, -1.0d0); B(6) = (4.0d0, 1.0d0)
  call zgelss(3, 3, 2, A, 3, B, 3, S, -1.0d0, rank, WORK, lwork, RWORK, info)
  call begin_test('multiple_rhs')
  call print_array('x', B_r, 12)
  call print_array('s', S, 3)
  call print_int('rank', rank)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 6: M=0 edge case
  ! ============================================================
  A = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  call zgelss(0, 3, 1, A, 1, B, 3, S, -1.0d0, rank, WORK, lwork, RWORK, info)
  call begin_test('m_zero')
  call print_int('rank', rank)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 7: N=0 edge case
  ! ============================================================
  A = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  call zgelss(3, 0, 1, A, 3, B, 3, S, -1.0d0, rank, WORK, lwork, RWORK, info)
  call begin_test('n_zero')
  call print_int('rank', rank)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 8: Overdetermined 6x2 with M >> N (triggers QR preconditioning path)
  ! Uses M=6, N=2 so M/N=3 > MNTHR_RATIO ~1.6
  ! ============================================================
  A = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  ! Column 1
  A(1) = (1.0d0, 0.0d0); A(2) = (0.0d0, 1.0d0)
  A(3) = (1.0d0, 1.0d0); A(4) = (2.0d0, 0.0d0)
  A(5) = (1.0d0, -1.0d0); A(6) = (0.0d0, 0.0d0)
  ! Column 2
  A(7) = (0.0d0, 0.0d0); A(8) = (1.0d0, 0.0d0)
  A(9) = (1.0d0, -1.0d0); A(10) = (1.0d0, 1.0d0)
  A(11) = (2.0d0, 0.0d0); A(12) = (0.0d0, 0.0d0)
  B(1) = (1.0d0, 0.0d0); B(2) = (1.0d0, 1.0d0)
  B(3) = (2.0d0, 0.0d0); B(4) = (3.0d0, -1.0d0)
  B(5) = (3.0d0, 0.0d0); B(6) = (0.0d0, 0.0d0)
  call zgelss(6, 2, 1, A, 6, B, 6, S, -1.0d0, rank, WORK, lwork, RWORK, info)
  call begin_test('overdetermined_tall')
  call print_array('x', B_r, 4)
  call print_array('s', S, 2)
  call print_int('rank', rank)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 9: Underdetermined with M << N (triggers LQ path)
  ! M=2, N=6
  ! ============================================================
  A = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  ! Row 1: [1 0 1+i 0 0 0]
  A(1) = (1.0d0, 0.0d0); A(5) = (1.0d0, 1.0d0)
  ! Row 2: [0 1-i 0 1 0 0]
  A(4) = (1.0d0, -1.0d0); A(8) = (1.0d0, 0.0d0)
  B(1) = (2.0d0, 1.0d0); B(2) = (4.0d0, -1.0d0)
  ! LDB must be >= max(M,N) = 6
  call zgelss(2, 6, 1, A, 2, B, 6, S, -1.0d0, rank, WORK, lwork, RWORK, info)
  call begin_test('underdetermined_wide')
  call print_array('x', B_r, 12)
  call print_array('s', S, 2)
  call print_int('rank', rank)
  call print_int('info', info)
  call end_test()

end program
