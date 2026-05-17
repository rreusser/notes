program test_ztpmlqt
  use test_utils
  implicit none

  ! Build a compact-WY triangular-pentagonal LQ factorization with ZTPLQT,
  ! then apply the resulting Q (or Q**H) to a stacked matrix C = [A; B]
  ! (left) or C = [A B] (right) using ZTPMLQT.
  !
  ! Dimensions for each side (V is K-by-* — the LQ counterpart of the
  ! column-wise QR pentagonal V):
  !   SIDE='L': V is K-by-M, A is K-by-N, B is M-by-N
  !   SIDE='R': V is K-by-N, A is M-by-K, B is M-by-N

  ! Case A: M=4, N=3, K=3, L=2, MB=2 (left side; V is K-by-M=3x4)
  complex*16       :: AA_L(3, 4), BB_L(3, 4), TA(2, 3)
  complex*16       :: A_L(3, 3), B_L(4, 3)
  double precision :: AA_L_r(2, 3, 4), BB_L_r(2, 3, 4), TA_r(2, 2, 3)
  double precision :: A_L_r(2, 3, 3), B_L_r(2, 4, 3)
  equivalence (AA_L, AA_L_r); equivalence (BB_L, BB_L_r); equivalence (TA, TA_r)
  equivalence (A_L, A_L_r); equivalence (B_L, B_L_r)

  ! Case C (large/blocked): M=8, N=6, K=6, L=3, MB=3 (left)
  complex*16       :: AA_LB(6, 8), BB_LB(6, 8), TLB(3, 6)
  complex*16       :: A_LB(6, 6), B_LB(8, 6)
  double precision :: AA_LB_r(2, 6, 8), BB_LB_r(2, 6, 8), TLB_r(2, 3, 6)
  double precision :: A_LB_r(2, 6, 6), B_LB_r(2, 8, 6)
  equivalence (AA_LB, AA_LB_r); equivalence (BB_LB, BB_LB_r); equivalence (TLB, TLB_r)
  equivalence (A_LB, A_LB_r); equivalence (B_LB, B_LB_r)

  ! Case D: L=0 (purely rectangular V), M=5, N=4, K=3, MB=2 (left); V K-by-M=3x5
  complex*16       :: AA_L0(3, 5), BB_L0(3, 5), TL0(2, 3)
  double precision :: AA_L0_r(2, 3, 5), BB_L0_r(2, 3, 5), TL0_r(2, 2, 3)
  equivalence (AA_L0, AA_L0_r); equivalence (BB_L0, BB_L0_r); equivalence (TL0, TL0_r)

  ! Case E: L=K (fully triangular V), M=K=3, N=2, MB=2
  complex*16       :: AA_LK(3, 3), BB_LK(3, 3), TLK(2, 3)
  double precision :: AA_LK_r(2, 3, 3), BB_LK_r(2, 3, 3), TLK_r(2, 2, 3)
  equivalence (AA_LK, AA_LK_r); equivalence (BB_LK, BB_LK_r); equivalence (TLK, TLK_r)

  ! Case F: MB == K (single block iteration), M=5, N=4, K=4, L=2, MB=4 (left); V K-by-M=4x5
  complex*16       :: AA_S(4, 5), BB_S(4, 5), TSG(4, 4)
  double precision :: AA_S_r(2, 4, 5), BB_S_r(2, 4, 5), TSG_r(2, 4, 4)
  equivalence (AA_S, AA_S_r); equivalence (BB_S, BB_S_r); equivalence (TSG, TSG_r)

  complex*16 :: WORK(2048)
  integer :: info, i, j

  ! ========== Case A: M=4, N=3, K=3, L=2, MB=2, SIDE='L' ==========
  AA_L = (0.0d0, 0.0d0); BB_L = (0.0d0, 0.0d0); TA = (0.0d0, 0.0d0)
  ! Lower-triangular A (K x K = 3 x 3); leftmost K cols of AA_L
  AA_L(1,1) = (2.0d0,  0.1d0)
  AA_L(2,1) = (0.5d0, -0.2d0); AA_L(2,2) = (3.0d0,  0.3d0)
  AA_L(3,1) = (0.25d0, 0.1d0); AA_L(3,2) = (0.75d0, -0.4d0); AA_L(3,3) = (4.0d0, -0.2d0)
  ! Pentagonal B (K x N_dt = 3 x 4): N-L = 2 rectangular cols + L=2 trapezoidal
  BB_L(1,1) = (0.7d0, -0.1d0); BB_L(1,2) = (0.3d0,  0.2d0)
  BB_L(2,1) = (0.2d0,  0.3d0); BB_L(2,2) = (0.9d0, -0.1d0)
  BB_L(3,1) = (0.4d0, -0.2d0); BB_L(3,2) = (0.1d0,  0.4d0)
  ! Trapezoidal cols (cols N-L+1=3..N=4): first L=2 cols of K-by-K lower triangular
  BB_L(1,3) = (1.1d0,  0.2d0)
  BB_L(2,3) = (0.5d0, -0.3d0); BB_L(2,4) = (1.2d0,  0.1d0)
  BB_L(3,3) = (0.6d0,  0.4d0); BB_L(3,4) = (0.7d0, -0.2d0)
  call ZTPLQT(3, 4, 2, 2, AA_L, 3, BB_L, 3, TA, 2, WORK, info)

  call begin_test('factors_left_A')
  call print_int('info', info)
  call print_array('R', AA_L_r, 2*12)
  call print_array('V', BB_L_r, 2*12)
  call print_array('T', TA_r, 2*6)
  call end_test()

  ! Test: SIDE='L', TRANS='N'
  call init_AB_left(A_L, B_L)
  call ZTPMLQT('L', 'N', 4, 3, 3, 2, 2, BB_L, 3, TA, 2, A_L, 3, B_L, 4, WORK, info)
  call begin_test('left_notrans_A')
  call print_int('info', info)
  call print_array('A', A_L_r, 2*9)
  call print_array('B', B_L_r, 2*12)
  call end_test()

  ! Test: SIDE='L', TRANS='C'
  call init_AB_left(A_L, B_L)
  call ZTPMLQT('L', 'C', 4, 3, 3, 2, 2, BB_L, 3, TA, 2, A_L, 3, B_L, 4, WORK, info)
  call begin_test('left_ctrans_A')
  call print_int('info', info)
  call print_array('A', A_L_r, 2*9)
  call print_array('B', B_L_r, 2*12)
  call end_test()

  ! ========== Case B: M=3, N=4, K=4, L=2, MB=2, SIDE='R' ==========
  call run_case_B()

  ! ========== Case C: M=8, N=6, K=6, L=3, MB=3, SIDE='L' (blocked) ==========
  AA_LB = (0.0d0, 0.0d0); BB_LB = (0.0d0, 0.0d0); TLB = (0.0d0, 0.0d0)
  do j = 1, 6
    do i = j, 6
      AA_LB(i, j) = cmplx( sin(dble(i + 2*j)) + 1.5d0, 0.1d0*cos(dble(i*3 + j)), kind=8 )
      if ( i == j ) AA_LB(i,j) = AA_LB(i,j) + (2.0d0, 0.0d0)
    end do
  end do
  do j = 1, 5
    do i = 1, 6
      BB_LB(i, j) = cmplx( cos(dble(i + j*2)) + 0.2d0*dble(j), 0.15d0*sin(dble(i*2 + j)), kind=8 )
    end do
  end do
  do j = 1, 3
    do i = j, 6
      BB_LB(i, 5+j) = cmplx( sin(dble(i*3 + j)) + 1.0d0, 0.1d0*cos(dble(i + j*4)), kind=8 )
    end do
  end do
  call ZTPLQT(6, 8, 3, 3, AA_LB, 6, BB_LB, 6, TLB, 3, WORK, info)

  call begin_test('factors_left_C')
  call print_int('info', info)
  call print_array('R', AA_LB_r, 2*48)
  call print_array('V', BB_LB_r, 2*48)
  call print_array('T', TLB_r, 2*18)
  call end_test()

  call init_AB_left_LB(A_LB, B_LB)
  call ZTPMLQT('L', 'N', 8, 6, 6, 3, 3, BB_LB, 6, TLB, 3, A_LB, 6, B_LB, 8, WORK, info)
  call begin_test('left_notrans_C')
  call print_int('info', info)
  call print_array('A', A_LB_r, 2*36)
  call print_array('B', B_LB_r, 2*48)
  call end_test()

  call init_AB_left_LB(A_LB, B_LB)
  call ZTPMLQT('L', 'C', 8, 6, 6, 3, 3, BB_LB, 6, TLB, 3, A_LB, 6, B_LB, 8, WORK, info)
  call begin_test('left_ctrans_C')
  call print_int('info', info)
  call print_array('A', A_LB_r, 2*36)
  call print_array('B', B_LB_r, 2*48)
  call end_test()

  ! ========== Case D: L=0 (rectangular V), SIDE='L', M=5, N=4, K=3, MB=2 ==========
  AA_L0 = (0.0d0, 0.0d0); BB_L0 = (0.0d0, 0.0d0); TL0 = (0.0d0, 0.0d0)
  AA_L0(1,1) = (2.0d0,  0.1d0)
  AA_L0(2,1) = (0.4d0, -0.2d0); AA_L0(2,2) = (2.5d0,  0.3d0)
  AA_L0(3,1) = (0.3d0,  0.0d0); AA_L0(3,2) = (0.5d0, -0.1d0); AA_L0(3,3) = (3.0d0, 0.2d0)
  do j = 1, 5
    do i = 1, 3
      BB_L0(i, j) = cmplx( sin(dble(i + j*2)) + 0.1d0*dble(j), 0.1d0*cos(dble(i*2 + j)), kind=8 )
    end do
  end do
  call ZTPLQT(3, 5, 0, 2, AA_L0, 3, BB_L0, 3, TL0, 2, WORK, info)

  call begin_test('factors_left_D')
  call print_int('info', info)
  call print_array('R', AA_L0_r, 2*15)
  call print_array('V', BB_L0_r, 2*15)
  call print_array('T', TL0_r, 2*6)
  call end_test()

  call run_case_D()

  ! ========== Case E: L=K (fully triangular V), M=K=3, N=2, MB=2 ==========
  AA_LK = (0.0d0, 0.0d0); BB_LK = (0.0d0, 0.0d0); TLK = (0.0d0, 0.0d0)
  AA_LK(1,1) = (2.0d0,  0.1d0)
  AA_LK(2,1) = (0.5d0, -0.2d0); AA_LK(2,2) = (3.0d0,  0.3d0)
  AA_LK(3,1) = (0.3d0,  0.4d0); AA_LK(3,2) = (0.4d0, -0.1d0); AA_LK(3,3) = (4.0d0, -0.2d0)
  ! L=3 trapezoidal cols form a 3x3 lower triangular block
  BB_LK(1,1) = (1.2d0,  0.1d0)
  BB_LK(2,1) = (0.6d0, -0.2d0); BB_LK(2,2) = (1.5d0,  0.3d0)
  BB_LK(3,1) = (0.2d0,  0.0d0); BB_LK(3,2) = (0.4d0, -0.1d0); BB_LK(3,3) = (1.7d0, 0.2d0)
  call ZTPLQT(3, 3, 3, 2, AA_LK, 3, BB_LK, 3, TLK, 2, WORK, info)

  call begin_test('factors_left_E')
  call print_int('info', info)
  call print_array('R', AA_LK_r, 2*9)
  call print_array('V', BB_LK_r, 2*9)
  call print_array('T', TLK_r, 2*6)
  call end_test()

  call run_case_E()

  ! ========== Case F: MB == K (single inner iteration) ==========
  AA_S = (0.0d0, 0.0d0); BB_S = (0.0d0, 0.0d0); TSG = (0.0d0, 0.0d0)
  do j = 1, 4
    do i = j, 4
      AA_S(i, j) = cmplx( sin(dble(i*3 + j)) + 1.0d0, 0.1d0*cos(dble(i + j)), kind=8 )
      if ( i == j ) AA_S(i,j) = AA_S(i,j) + (1.5d0, 0.0d0)
    end do
  end do
  do j = 1, 3
    do i = 1, 4
      BB_S(i, j) = cmplx( cos(dble(i + j*3)) + 0.5d0, 0.1d0*sin(dble(i*2 + j)), kind=8 )
    end do
  end do
  do j = 1, 2
    do i = j, 4
      BB_S(i, 3+j) = cmplx( sin(dble(i + j*5)) + 1.0d0, 0.1d0*cos(dble(i*3 + j)), kind=8 )
    end do
  end do
  call ZTPLQT(4, 5, 2, 4, AA_S, 4, BB_S, 4, TSG, 4, WORK, info)

  call begin_test('factors_single')
  call print_int('info', info)
  call print_array('R', AA_S_r, 2*20)
  call print_array('V', BB_S_r, 2*20)
  call print_array('T', TSG_r, 2*16)
  call end_test()

  call run_case_F()

  ! ========== Quick returns ==========
  call ZTPMLQT('L', 'N', 0, 3, 3, 2, 2, BB_L, 3, TA, 2, AA_L, 3, BB_L, 4, WORK, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  call ZTPMLQT('L', 'N', 4, 0, 3, 2, 2, BB_L, 3, TA, 2, AA_L, 3, BB_L, 4, WORK, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  call ZTPMLQT('L', 'N', 4, 3, 0, 0, 1, BB_L, 3, TA, 2, AA_L, 3, BB_L, 4, WORK, info)
  call begin_test('k_zero')
  call print_int('info', info)
  call end_test()

contains

  subroutine init_AB_left(A_, B_)
    complex*16, intent(out) :: A_(3,3), B_(4,3)
    A_(1,1) = (1.0d0,  0.1d0); A_(1,2) = (2.0d0, -0.2d0); A_(1,3) = (3.0d0,  0.3d0)
    A_(2,1) = (4.0d0, -0.1d0); A_(2,2) = (5.0d0,  0.2d0); A_(2,3) = (6.0d0, -0.3d0)
    A_(3,1) = (7.0d0,  0.4d0); A_(3,2) = (8.0d0, -0.4d0); A_(3,3) = (9.0d0,  0.0d0)
    B_(1,1) = (1.0d0,  0.2d0); B_(1,2) = (-1.0d0, 0.1d0); B_(1,3) = (2.0d0, -0.1d0)
    B_(2,1) = (0.5d0, -0.3d0); B_(2,2) = (1.5d0, -0.2d0); B_(2,3) = (-2.5d0, 0.4d0)
    B_(3,1) = (-1.0d0, 0.0d0); B_(3,2) = (0.0d0,  0.5d0); B_(3,3) = (3.0d0, -0.1d0)
    B_(4,1) = (2.0d0,  0.3d0); B_(4,2) = (-2.0d0, -0.4d0); B_(4,3) = (1.0d0, 0.2d0)
  end subroutine

  subroutine init_AB_left_LB(A_, B_)
    complex*16, intent(out) :: A_(6,6), B_(8,6)
    integer :: ii, jj
    do jj = 1, 6
      do ii = 1, 6
        A_(ii,jj) = cmplx( sin(dble(ii*2 + jj)), 0.1d0*cos(dble(ii + jj*3)), kind=8 )
      end do
    end do
    do jj = 1, 6
      do ii = 1, 8
        B_(ii,jj) = cmplx( cos(dble(ii + jj*2)) + 0.5d0, 0.15d0*sin(dble(ii*3 + jj)), kind=8 )
      end do
    end do
  end subroutine

  subroutine run_case_B()
    ! M=3, N=4, K=4, L=2, MB=2, SIDE='R'
    complex*16       :: AAB(4, 4), BBV(4, 4), TB(2, 4)
    complex*16       :: A_R2(3, 4), B_R2(3, 4)
    double precision :: AAB_r(2, 4, 4), BBV_r(2, 4, 4), TB_r(2, 2, 4)
    double precision :: A_R2_r(2, 3, 4), B_R2_r(2, 3, 4)
    integer :: ii, jj, info2
    equivalence (AAB, AAB_r); equivalence (BBV, BBV_r); equivalence (TB, TB_r)
    equivalence (A_R2, A_R2_r); equivalence (B_R2, B_R2_r)

    AAB = (0.0d0, 0.0d0); BBV = (0.0d0, 0.0d0); TB = (0.0d0, 0.0d0)
    AAB(1,1) = (2.5d0,  0.1d0)
    AAB(2,1) = (0.6d0, -0.2d0); AAB(2,2) = (3.5d0,  0.3d0)
    AAB(3,1) = (0.2d0,  0.0d0); AAB(3,2) = (0.7d0, -0.1d0); AAB(3,3) = (2.0d0, 0.2d0)
    AAB(4,1) = (0.4d0,  0.3d0); AAB(4,2) = (0.3d0, -0.2d0); AAB(4,3) = (0.5d0, 0.1d0); AAB(4,4) = (4.0d0, -0.3d0)
    BBV(1,1) = (0.7d0, -0.1d0); BBV(1,2) = (0.3d0,  0.2d0)
    BBV(2,1) = (0.2d0,  0.3d0); BBV(2,2) = (0.9d0, -0.1d0)
    BBV(3,1) = (0.4d0, -0.2d0); BBV(3,2) = (0.1d0,  0.4d0)
    BBV(4,1) = (0.5d0,  0.0d0); BBV(4,2) = (0.6d0, -0.3d0)
    BBV(1,3) = (1.1d0,  0.2d0)
    BBV(2,3) = (0.5d0, -0.3d0); BBV(2,4) = (1.2d0,  0.1d0)
    BBV(3,3) = (0.6d0,  0.4d0); BBV(3,4) = (0.7d0, -0.2d0)
    BBV(4,3) = (0.7d0, -0.1d0); BBV(4,4) = (0.4d0,  0.3d0)
    call ZTPLQT(4, 4, 2, 2, AAB, 4, BBV, 4, TB, 2, WORK, info2)

    call begin_test('factors_right_B')
    call print_int('info', info2)
    call print_array('R', AAB_r, 2*16)
    call print_array('V', BBV_r, 2*16)
    call print_array('T', TB_r, 2*8)
    call end_test()

    do jj = 1, 4
      do ii = 1, 3
        A_R2(ii,jj) = cmplx( 1.0d0 + dble(ii) + 0.1d0*dble(jj), 0.05d0*dble(ii - jj), kind=8 )
        B_R2(ii,jj) = cmplx( -1.0d0 + 0.5d0*dble(ii) + dble(jj), 0.05d0*dble(ii + jj), kind=8 )
      end do
    end do
    call ZTPMLQT('R', 'N', 3, 4, 4, 2, 2, BBV, 4, TB, 2, A_R2, 3, B_R2, 3, WORK, info2)
    call begin_test('right_notrans_B')
    call print_int('info', info2)
    call print_array('A', A_R2_r, 2*12)
    call print_array('B', B_R2_r, 2*12)
    call end_test()

    do jj = 1, 4
      do ii = 1, 3
        A_R2(ii,jj) = cmplx( 1.0d0 + dble(ii) + 0.1d0*dble(jj), 0.05d0*dble(ii - jj), kind=8 )
        B_R2(ii,jj) = cmplx( -1.0d0 + 0.5d0*dble(ii) + dble(jj), 0.05d0*dble(ii + jj), kind=8 )
      end do
    end do
    call ZTPMLQT('R', 'C', 3, 4, 4, 2, 2, BBV, 4, TB, 2, A_R2, 3, B_R2, 3, WORK, info2)
    call begin_test('right_ctrans_B')
    call print_int('info', info2)
    call print_array('A', A_R2_r, 2*12)
    call print_array('B', B_R2_r, 2*12)
    call end_test()
  end subroutine

  subroutine run_case_D()
    complex*16       :: ADL(3, 4), BDL(5, 4)
    complex*16       :: ADR(3, 3), BDR(3, 5)
    double precision :: ADL_r(2, 3, 4), BDL_r(2, 5, 4)
    double precision :: ADR_r(2, 3, 3), BDR_r(2, 3, 5)
    integer :: ii, jj, info2
    equivalence (ADL, ADL_r); equivalence (BDL, BDL_r)
    equivalence (ADR, ADR_r); equivalence (BDR, BDR_r)

    do jj = 1, 4
      do ii = 1, 3
        ADL(ii, jj) = cmplx( dble(ii) + 0.5d0*dble(jj), 0.1d0*dble(ii - jj), kind=8 )
      end do
      do ii = 1, 5
        BDL(ii, jj) = cmplx( -dble(ii) + dble(jj), 0.1d0*dble(ii + jj), kind=8 )
      end do
    end do
    call ZTPMLQT('L', 'N', 5, 4, 3, 0, 2, BB_L0, 3, TL0, 2, ADL, 3, BDL, 5, WORK, info2)
    call begin_test('left_notrans_D')
    call print_int('info', info2)
    call print_array('A', ADL_r, 2*12)
    call print_array('B', BDL_r, 2*20)
    call end_test()

    do jj = 1, 4
      do ii = 1, 3
        ADL(ii, jj) = cmplx( dble(ii) + 0.5d0*dble(jj), 0.1d0*dble(ii - jj), kind=8 )
      end do
      do ii = 1, 5
        BDL(ii, jj) = cmplx( -dble(ii) + dble(jj), 0.1d0*dble(ii + jj), kind=8 )
      end do
    end do
    call ZTPMLQT('L', 'C', 5, 4, 3, 0, 2, BB_L0, 3, TL0, 2, ADL, 3, BDL, 5, WORK, info2)
    call begin_test('left_ctrans_D')
    call print_int('info', info2)
    call print_array('A', ADL_r, 2*12)
    call print_array('B', BDL_r, 2*20)
    call end_test()

    do jj = 1, 3
      do ii = 1, 3
        ADR(ii, jj) = cmplx( dble(ii) + 0.5d0*dble(jj), 0.1d0*dble(ii - jj), kind=8 )
      end do
    end do
    do jj = 1, 5
      do ii = 1, 3
        BDR(ii, jj) = cmplx( -dble(ii) + dble(jj), 0.1d0*dble(ii + jj), kind=8 )
      end do
    end do
    call ZTPMLQT('R', 'N', 3, 5, 3, 0, 2, BB_L0, 3, TL0, 2, ADR, 3, BDR, 3, WORK, info2)
    call begin_test('right_notrans_D')
    call print_int('info', info2)
    call print_array('A', ADR_r, 2*9)
    call print_array('B', BDR_r, 2*15)
    call end_test()

    do jj = 1, 3
      do ii = 1, 3
        ADR(ii, jj) = cmplx( dble(ii) + 0.5d0*dble(jj), 0.1d0*dble(ii - jj), kind=8 )
      end do
    end do
    do jj = 1, 5
      do ii = 1, 3
        BDR(ii, jj) = cmplx( -dble(ii) + dble(jj), 0.1d0*dble(ii + jj), kind=8 )
      end do
    end do
    call ZTPMLQT('R', 'C', 3, 5, 3, 0, 2, BB_L0, 3, TL0, 2, ADR, 3, BDR, 3, WORK, info2)
    call begin_test('right_ctrans_D')
    call print_int('info', info2)
    call print_array('A', ADR_r, 2*9)
    call print_array('B', BDR_r, 2*15)
    call end_test()
  end subroutine

  subroutine run_case_E()
    complex*16       :: AE(3, 2), BE(3, 2)
    double precision :: AE_r(2, 3, 2), BE_r(2, 3, 2)
    integer :: ii, jj, info2
    equivalence (AE, AE_r); equivalence (BE, BE_r)

    do jj = 1, 2
      do ii = 1, 3
        AE(ii, jj) = cmplx( 1.0d0 + dble(ii) + 0.3d0*dble(jj), 0.1d0*dble(ii + jj), kind=8 )
        BE(ii, jj) = cmplx( -2.0d0 + 0.5d0*dble(ii) + dble(jj), 0.05d0*dble(ii - jj), kind=8 )
      end do
    end do
    call ZTPMLQT('L', 'N', 3, 2, 3, 3, 2, BB_LK, 3, TLK, 2, AE, 3, BE, 3, WORK, info2)
    call begin_test('left_notrans_E')
    call print_int('info', info2)
    call print_array('A', AE_r, 2*6)
    call print_array('B', BE_r, 2*6)
    call end_test()

    do jj = 1, 2
      do ii = 1, 3
        AE(ii, jj) = cmplx( 1.0d0 + dble(ii) + 0.3d0*dble(jj), 0.1d0*dble(ii + jj), kind=8 )
        BE(ii, jj) = cmplx( -2.0d0 + 0.5d0*dble(ii) + dble(jj), 0.05d0*dble(ii - jj), kind=8 )
      end do
    end do
    call ZTPMLQT('L', 'C', 3, 2, 3, 3, 2, BB_LK, 3, TLK, 2, AE, 3, BE, 3, WORK, info2)
    call begin_test('left_ctrans_E')
    call print_int('info', info2)
    call print_array('A', AE_r, 2*6)
    call print_array('B', BE_r, 2*6)
    call end_test()
  end subroutine

  subroutine run_case_F()
    complex*16       :: AF(4, 4), BF(5, 4)
    double precision :: AF_r(2, 4, 4), BF_r(2, 5, 4)
    integer :: ii, jj, info2
    equivalence (AF, AF_r); equivalence (BF, BF_r)

    do jj = 1, 4
      do ii = 1, 4
        AF(ii, jj) = cmplx( sin(dble(ii + jj*2)), 0.1d0*cos(dble(ii*3 + jj)), kind=8 )
      end do
      do ii = 1, 5
        BF(ii, jj) = cmplx( cos(dble(ii*2 + jj)) + 0.3d0, 0.1d0*sin(dble(ii + jj*3)), kind=8 )
      end do
    end do
    call ZTPMLQT('L', 'N', 5, 4, 4, 2, 4, BB_S, 4, TSG, 4, AF, 4, BF, 5, WORK, info2)
    call begin_test('left_notrans_F')
    call print_int('info', info2)
    call print_array('A', AF_r, 2*16)
    call print_array('B', BF_r, 2*20)
    call end_test()

    do jj = 1, 4
      do ii = 1, 4
        AF(ii, jj) = cmplx( sin(dble(ii + jj*2)), 0.1d0*cos(dble(ii*3 + jj)), kind=8 )
      end do
      do ii = 1, 5
        BF(ii, jj) = cmplx( cos(dble(ii*2 + jj)) + 0.3d0, 0.1d0*sin(dble(ii + jj*3)), kind=8 )
      end do
    end do
    call ZTPMLQT('L', 'C', 5, 4, 4, 2, 4, BB_S, 4, TSG, 4, AF, 4, BF, 5, WORK, info2)
    call begin_test('left_ctrans_F')
    call print_int('info', info2)
    call print_array('A', AF_r, 2*16)
    call print_array('B', BF_r, 2*20)
    call end_test()
  end subroutine

end program
