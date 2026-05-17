program test_ztpmqrt
  use test_utils
  implicit none

  ! Build a compact-WY triangular-pentagonal QR factorization with ZTPQRT,
  ! then apply the resulting Q (or Q**H) to a stacked matrix C = [A; B]
  ! (left) or C = [A B] (right) using ZTPMQRT.

  ! Case A: M=4, N=3, K=3, L=2, NB=2 (left side)
  complex*16 :: AA_L(3, 3), BB_L(4, 3), TA(2, 3)
  complex*16 :: A_L(3, 3), B_L(4, 3)
  double precision :: AA_L_r(18), BB_L_r(24), TA_r(12)
  double precision :: A_L_r(18), B_L_r(24)
  equivalence (AA_L, AA_L_r)
  equivalence (BB_L, BB_L_r)
  equivalence (TA, TA_r)
  equivalence (A_L, A_L_r)
  equivalence (B_L, B_L_r)

  ! Case C (large/blocked): M=8, N=6, K=6, L=3, NB=3 (left)
  complex*16 :: AA_LB(6, 6), BB_LB(8, 6), TLB(3, 6)
  complex*16 :: A_LB(6, 6), B_LB(8, 6)
  double precision :: AA_LB_r(72), BB_LB_r(96), TLB_r(36)
  double precision :: A_LB_r(72), B_LB_r(96)
  equivalence (AA_LB, AA_LB_r)
  equivalence (BB_LB, BB_LB_r)
  equivalence (TLB, TLB_r)
  equivalence (A_LB, A_LB_r)
  equivalence (B_LB, B_LB_r)

  ! Case D: L=0 (purely rectangular V): M=5, N=4, K=3, NB=2
  complex*16 :: AA_L0(3, 3), BB_L0(5, 3), TL0(2, 3)
  double precision :: AA_L0_r(18), BB_L0_r(30), TL0_r(12)
  equivalence (AA_L0, AA_L0_r)
  equivalence (BB_L0, BB_L0_r)
  equivalence (TL0, TL0_r)

  ! Case E: L=K (fully trapezoidal/triangular V): M=K=3, N=2, NB=2
  complex*16 :: AA_LK(3, 3), BB_LK(3, 3), TLK(2, 3)
  double precision :: AA_LK_r(18), BB_LK_r(18), TLK_r(12)
  equivalence (AA_LK, AA_LK_r)
  equivalence (BB_LK, BB_LK_r)
  equivalence (TLK, TLK_r)

  ! Case F: NB == K (single block iteration)
  complex*16 :: AA_S(4, 4), BB_S(5, 4), TSG(4, 4)
  double precision :: AA_S_r(32), BB_S_r(40), TSG_r(32)
  equivalence (AA_S, AA_S_r)
  equivalence (BB_S, BB_S_r)
  equivalence (TSG, TSG_r)

  complex*16 :: WORK(2048)
  integer :: info, i, j

  ! ========== Case A: M=4, N=3, K=3, L=2, NB=2, SIDE='L' ==========
  AA_L = (0.0d0, 0.0d0); BB_L = (0.0d0, 0.0d0); TA = (0.0d0, 0.0d0)
  AA_L(1,1) = (2.0d0, 0.1d0); AA_L(1,2) = (0.5d0, -0.2d0); AA_L(1,3) = (0.25d0, 0.3d0)
                              AA_L(2,2) = (3.0d0, -0.4d0); AA_L(2,3) = (0.75d0, 0.5d0)
                                                           AA_L(3,3) = (4.0d0, -0.6d0)
  BB_L(1,1) = (0.7d0, 0.2d0);  BB_L(1,2) = (0.3d0, -0.1d0); BB_L(1,3) = (0.4d0, 0.5d0)
  BB_L(2,1) = (0.2d0, -0.3d0); BB_L(2,2) = (0.9d0, 0.4d0);  BB_L(2,3) = (0.1d0, -0.2d0)
  BB_L(3,1) = (1.1d0, 0.1d0);  BB_L(3,2) = (0.5d0, -0.5d0); BB_L(3,3) = (0.6d0, 0.3d0)
                               BB_L(4,2) = (1.2d0, 0.2d0);  BB_L(4,3) = (0.7d0, -0.4d0)
  call ztpqrt(4, 3, 2, 2, AA_L, 3, BB_L, 4, TA, 2, WORK, info)

  call begin_test('factors_left_A')
  call print_int('info', info)
  call print_array('R', AA_L_r, 18)
  call print_array('V', BB_L_r, 24)
  call print_array('T', TA_r, 12)
  call end_test()

  ! Test SIDE='L', TRANS='N'
  call init_AB_left(A_L, B_L)
  call ztpmqrt('L', 'N', 4, 3, 3, 2, 2, BB_L, 4, TA, 2, A_L, 3, B_L, 4, WORK, info)
  call begin_test('left_notrans_A')
  call print_int('info', info)
  call print_array('A', A_L_r, 18)
  call print_array('B', B_L_r, 24)
  call end_test()

  ! Test SIDE='L', TRANS='C'
  call init_AB_left(A_L, B_L)
  call ztpmqrt('L', 'C', 4, 3, 3, 2, 2, BB_L, 4, TA, 2, A_L, 3, B_L, 4, WORK, info)
  call begin_test('left_ctrans_A')
  call print_int('info', info)
  call print_array('A', A_L_r, 18)
  call print_array('B', B_L_r, 24)
  call end_test()

  ! ========== Case B: M=3, N=4, K=4, L=2, NB=2, SIDE='R' ==========
  call run_case_B()

  ! ========== Case C: M=8, N=6, K=6, L=3, NB=3, SIDE='L' (blocked) ==========
  AA_LB = (0.0d0, 0.0d0); BB_LB = (0.0d0, 0.0d0); TLB = (0.0d0, 0.0d0)
  do j = 1, 6
    do i = 1, j
      AA_LB(i, j) = dcmplx(sin(dble(i + 2*j)) + 1.5d0, cos(dble(i*3 + j)) * 0.3d0)
      if ( i == j ) AA_LB(i,j) = AA_LB(i,j) + (2.0d0, 0.0d0)
    end do
  end do
  do j = 1, 6
    do i = 1, 5
      BB_LB(i, j) = dcmplx(cos(dble(i*2 + j)) + 0.2d0 * dble(i), sin(dble(i + j*4)) * 0.4d0)
    end do
  end do
  do i = 1, 3
    do j = i, 6
      BB_LB(5+i, j) = dcmplx(sin(dble(i + j*3)) + 1.0d0, cos(dble(i*5 + j)) * 0.2d0)
    end do
  end do
  call ztpqrt(8, 6, 3, 3, AA_LB, 6, BB_LB, 8, TLB, 3, WORK, info)

  call begin_test('factors_left_C')
  call print_int('info', info)
  call print_array('R', AA_LB_r, 72)
  call print_array('V', BB_LB_r, 96)
  call print_array('T', TLB_r, 36)
  call end_test()

  call init_AB_left_LB(A_LB, B_LB)
  call ztpmqrt('L', 'N', 8, 6, 6, 3, 3, BB_LB, 8, TLB, 3, A_LB, 6, B_LB, 8, WORK, info)
  call begin_test('left_notrans_C')
  call print_int('info', info)
  call print_array('A', A_LB_r, 72)
  call print_array('B', B_LB_r, 96)
  call end_test()

  call init_AB_left_LB(A_LB, B_LB)
  call ztpmqrt('L', 'C', 8, 6, 6, 3, 3, BB_LB, 8, TLB, 3, A_LB, 6, B_LB, 8, WORK, info)
  call begin_test('left_ctrans_C')
  call print_int('info', info)
  call print_array('A', A_LB_r, 72)
  call print_array('B', B_LB_r, 96)
  call end_test()

  ! ========== Case D: L=0 (rectangular V), SIDE='L', M=5, N=4, K=3, NB=2 ==========
  AA_L0 = (0.0d0, 0.0d0); BB_L0 = (0.0d0, 0.0d0); TL0 = (0.0d0, 0.0d0)
  AA_L0(1,1) = (2.0d0, 0.1d0); AA_L0(1,2) = (0.4d0, -0.2d0); AA_L0(1,3) = (0.3d0, 0.4d0)
                               AA_L0(2,2) = (2.5d0, 0.3d0);  AA_L0(2,3) = (0.5d0, -0.1d0)
                                                             AA_L0(3,3) = (3.0d0, -0.5d0)
  do j = 1, 3
    do i = 1, 5
      BB_L0(i, j) = dcmplx(sin(dble(i + j*2)) + 0.1d0 * dble(i), cos(dble(i*2 + j)) * 0.3d0)
    end do
  end do
  call ztpqrt(5, 3, 0, 2, AA_L0, 3, BB_L0, 5, TL0, 2, WORK, info)

  call begin_test('factors_left_D')
  call print_int('info', info)
  call print_array('R', AA_L0_r, 18)
  call print_array('V', BB_L0_r, 30)
  call print_array('T', TL0_r, 12)
  call end_test()

  call run_case_D()

  ! ========== Case E: L=K (fully triangular V), M=K=3, N=2, NB=2 ==========
  AA_LK = (0.0d0, 0.0d0); BB_LK = (0.0d0, 0.0d0); TLK = (0.0d0, 0.0d0)
  AA_LK(1,1) = (2.0d0, 0.1d0); AA_LK(1,2) = (0.5d0, 0.2d0); AA_LK(1,3) = (0.3d0, -0.1d0)
                               AA_LK(2,2) = (3.0d0, -0.2d0); AA_LK(2,3) = (0.4d0, 0.3d0)
                                                             AA_LK(3,3) = (4.0d0, 0.4d0)
  BB_LK(1,1) = (1.2d0, 0.3d0); BB_LK(1,2) = (0.6d0, -0.2d0); BB_LK(1,3) = (0.2d0, 0.5d0)
                               BB_LK(2,2) = (1.5d0, 0.1d0);  BB_LK(2,3) = (0.4d0, -0.3d0)
                                                             BB_LK(3,3) = (1.7d0, 0.2d0)
  call ztpqrt(3, 3, 3, 2, AA_LK, 3, BB_LK, 3, TLK, 2, WORK, info)

  call begin_test('factors_left_E')
  call print_int('info', info)
  call print_array('R', AA_LK_r, 18)
  call print_array('V', BB_LK_r, 18)
  call print_array('T', TLK_r, 12)
  call end_test()

  call run_case_E()

  ! ========== Case F: NB == K = 4, single inner iteration. SIDE='L', M=5, N=4, K=4, L=2 ==========
  AA_S = (0.0d0, 0.0d0); BB_S = (0.0d0, 0.0d0); TSG = (0.0d0, 0.0d0)
  do j = 1, 4
    do i = 1, j
      AA_S(i, j) = dcmplx(sin(dble(i*3 + j)) + 1.0d0, cos(dble(i + j*2)) * 0.2d0)
      if ( i == j ) AA_S(i,j) = AA_S(i,j) + (1.5d0, 0.0d0)
    end do
  end do
  do j = 1, 4
    do i = 1, 3
      BB_S(i, j) = dcmplx(cos(dble(i + j*3)) + 0.5d0, sin(dble(i*4 + j)) * 0.3d0)
    end do
  end do
  do i = 1, 2
    do j = i, 4
      BB_S(3+i, j) = dcmplx(sin(dble(i*5 + j)) + 1.0d0, cos(dble(i + j*7)) * 0.4d0)
    end do
  end do
  call ztpqrt(5, 4, 2, 4, AA_S, 4, BB_S, 5, TSG, 4, WORK, info)

  call begin_test('factors_single')
  call print_int('info', info)
  call print_array('R', AA_S_r, 32)
  call print_array('V', BB_S_r, 40)
  call print_array('T', TSG_r, 32)
  call end_test()

  call run_case_F()

  ! ========== Quick returns ==========
  call ztpmqrt('L', 'N', 0, 3, 3, 2, 2, BB_L, 4, TA, 2, AA_L, 3, BB_L, 4, WORK, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  call ztpmqrt('L', 'N', 4, 0, 3, 2, 2, BB_L, 4, TA, 2, AA_L, 3, BB_L, 4, WORK, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  call ztpmqrt('L', 'N', 4, 3, 0, 0, 1, BB_L, 4, TA, 2, AA_L, 3, BB_L, 4, WORK, info)
  call begin_test('k_zero')
  call print_int('info', info)
  call end_test()

contains

  subroutine init_AB_left(A_, B_)
    complex*16, intent(out) :: A_(3,3), B_(4,3)
    integer :: ii, jj
    do jj = 1, 3
      do ii = 1, 3
        A_(ii,jj) = dcmplx(dble(3*(jj-1)+ii), 0.1d0 * dble(3*(jj-1)+ii))
      end do
      do ii = 1, 4
        B_(ii,jj) = dcmplx(0.5d0 * dble(ii) - dble(jj), 0.2d0 * dble(ii + jj))
      end do
    end do
  end subroutine

  subroutine init_AB_left_LB(A_, B_)
    complex*16, intent(out) :: A_(6,6), B_(8,6)
    integer :: ii, jj
    do jj = 1, 6
      do ii = 1, 6
        A_(ii,jj) = dcmplx(sin(dble(ii*2 + jj)), cos(dble(ii + jj*3)) * 0.3d0)
      end do
    end do
    do jj = 1, 6
      do ii = 1, 8
        B_(ii,jj) = dcmplx(cos(dble(ii + jj*2)) + 0.5d0, sin(dble(ii*3 + jj)) * 0.4d0)
      end do
    end do
  end subroutine

  subroutine run_case_B()
    ! M=3, N=4, K=4, L=2, NB=2, SIDE='R'
    ! V is 4x4 pentagonal (M_v = K = 4), A is M-by-K = 3-by-4, B is M-by-N = 3-by-4
    complex*16 :: AAB(4, 4), BBV(4, 4), TB(2, 4)
    complex*16 :: A_R2(3, 4), B_R2(3, 4)
    double precision :: AAB_r(32), BBV_r(32), TB_r(16)
    double precision :: A_R2_r(24), B_R2_r(24)
    equivalence (AAB, AAB_r)
    equivalence (BBV, BBV_r)
    equivalence (TB, TB_r)
    equivalence (A_R2, A_R2_r)
    equivalence (B_R2, B_R2_r)
    integer :: ii, jj, info2

    AAB = (0.0d0, 0.0d0); BBV = (0.0d0, 0.0d0); TB = (0.0d0, 0.0d0)
    AAB(1,1) = (2.5d0, 0.1d0); AAB(1,2) = (0.6d0, 0.2d0); AAB(1,3) = (0.2d0, -0.1d0); AAB(1,4) = (0.4d0, 0.3d0)
                               AAB(2,2) = (3.5d0, -0.3d0); AAB(2,3) = (0.7d0, 0.2d0); AAB(2,4) = (0.3d0, -0.4d0)
                                                           AAB(3,3) = (2.0d0, 0.5d0); AAB(3,4) = (0.5d0, -0.2d0)
                                                                                       AAB(4,4) = (4.0d0, 0.1d0)
    BBV(1,1) = (0.7d0, 0.1d0); BBV(1,2) = (0.3d0, -0.2d0); BBV(1,3) = (0.4d0, 0.3d0); BBV(1,4) = (0.5d0, 0.2d0)
    BBV(2,1) = (0.2d0, -0.4d0); BBV(2,2) = (0.9d0, 0.1d0); BBV(2,3) = (0.1d0, 0.5d0); BBV(2,4) = (0.6d0, -0.3d0)
    BBV(3,1) = (1.1d0, 0.2d0); BBV(3,2) = (0.5d0, -0.3d0); BBV(3,3) = (0.6d0, 0.4d0); BBV(3,4) = (0.7d0, 0.1d0)
                               BBV(4,2) = (1.2d0, 0.5d0); BBV(4,3) = (0.7d0, -0.2d0); BBV(4,4) = (0.4d0, 0.3d0)
    call ztpqrt(4, 4, 2, 2, AAB, 4, BBV, 4, TB, 2, WORK, info2)

    call begin_test('factors_right_B')
    call print_int('info', info2)
    call print_array('R', AAB_r, 32)
    call print_array('V', BBV_r, 32)
    call print_array('T', TB_r, 16)
    call end_test()

    do jj = 1, 4
      do ii = 1, 3
        A_R2(ii,jj) = dcmplx(1.0d0 + dble(ii) + 0.1d0*dble(jj), 0.2d0 * dble(ii + jj))
        B_R2(ii,jj) = dcmplx(-1.0d0 + 0.5d0*dble(ii) + dble(jj), 0.3d0 * dble(ii*jj))
      end do
    end do
    call ztpmqrt('R', 'N', 3, 4, 4, 2, 2, BBV, 4, TB, 2, A_R2, 3, B_R2, 3, WORK, info2)
    call begin_test('right_notrans_B')
    call print_int('info', info2)
    call print_array('A', A_R2_r, 24)
    call print_array('B', B_R2_r, 24)
    call end_test()

    do jj = 1, 4
      do ii = 1, 3
        A_R2(ii,jj) = dcmplx(1.0d0 + dble(ii) + 0.1d0*dble(jj), 0.2d0 * dble(ii + jj))
        B_R2(ii,jj) = dcmplx(-1.0d0 + 0.5d0*dble(ii) + dble(jj), 0.3d0 * dble(ii*jj))
      end do
    end do
    call ztpmqrt('R', 'C', 3, 4, 4, 2, 2, BBV, 4, TB, 2, A_R2, 3, B_R2, 3, WORK, info2)
    call begin_test('right_ctrans_B')
    call print_int('info', info2)
    call print_array('A', A_R2_r, 24)
    call print_array('B', B_R2_r, 24)
    call end_test()
  end subroutine

  subroutine run_case_D()
    ! Apply V/T (L=0) for both sides
    complex*16 :: AD(3, 4), BD(5, 4)
    complex*16 :: ADR(3, 3), BDR(3, 4)
    double precision :: ADL_r(24), BDL_r(40)
    double precision :: ADR_r(18), BDR_r(24)
    equivalence (AD, ADL_r)
    equivalence (BD, BDL_r)
    equivalence (ADR, ADR_r)
    equivalence (BDR, BDR_r)
    integer :: ii, jj, info2

    do jj = 1, 4
      do ii = 1, 3
        AD(ii, jj) = dcmplx(dble(ii) + 0.5d0*dble(jj), 0.1d0*dble(ii+jj))
      end do
      do ii = 1, 5
        BD(ii, jj) = dcmplx(-dble(ii) + dble(jj), 0.2d0*dble(ii*jj))
      end do
    end do
    call ztpmqrt('L', 'N', 5, 4, 3, 0, 2, BB_L0, 5, TL0, 2, AD, 3, BD, 5, WORK, info2)
    call begin_test('left_notrans_D')
    call print_int('info', info2)
    call print_array('A', ADL_r, 24)
    call print_array('B', BDL_r, 40)
    call end_test()

    do jj = 1, 4
      do ii = 1, 3
        AD(ii, jj) = dcmplx(dble(ii) + 0.5d0*dble(jj), 0.1d0*dble(ii+jj))
      end do
      do ii = 1, 5
        BD(ii, jj) = dcmplx(-dble(ii) + dble(jj), 0.2d0*dble(ii*jj))
      end do
    end do
    call ztpmqrt('L', 'C', 5, 4, 3, 0, 2, BB_L0, 5, TL0, 2, AD, 3, BD, 5, WORK, info2)
    call begin_test('left_ctrans_D')
    call print_int('info', info2)
    call print_array('A', ADL_r, 24)
    call print_array('B', BDL_r, 40)
    call end_test()

    ! Right side reuse with M=3, N=4, K=3, L=0 needs V of shape N-by-K = 5x3 (BB_L0)
    ! For SIDE='R', A is M-by-K = 3x3, B is M-by-N = 3x4.
    do jj = 1, 3
      do ii = 1, 3
        ADR(ii, jj) = dcmplx(dble(ii) + 0.5d0*dble(jj), 0.1d0*dble(ii+jj))
      end do
    end do
    do jj = 1, 4
      do ii = 1, 3
        BDR(ii, jj) = dcmplx(-dble(ii) + dble(jj), 0.2d0*dble(ii*jj))
      end do
    end do
    call ztpmqrt('R', 'N', 3, 4, 3, 0, 2, BB_L0, 5, TL0, 2, ADR, 3, BDR, 3, WORK, info2)
    call begin_test('right_notrans_D')
    call print_int('info', info2)
    call print_array('A', ADR_r, 18)
    call print_array('B', BDR_r, 24)
    call end_test()

    do jj = 1, 3
      do ii = 1, 3
        ADR(ii, jj) = dcmplx(dble(ii) + 0.5d0*dble(jj), 0.1d0*dble(ii+jj))
      end do
    end do
    do jj = 1, 4
      do ii = 1, 3
        BDR(ii, jj) = dcmplx(-dble(ii) + dble(jj), 0.2d0*dble(ii*jj))
      end do
    end do
    call ztpmqrt('R', 'C', 3, 4, 3, 0, 2, BB_L0, 5, TL0, 2, ADR, 3, BDR, 3, WORK, info2)
    call begin_test('right_ctrans_D')
    call print_int('info', info2)
    call print_array('A', ADR_r, 18)
    call print_array('B', BDR_r, 24)
    call end_test()
  end subroutine

  subroutine run_case_E()
    ! L=K, SIDE='L'. M=3, N=2, K=3.
    ! A is K-by-N = 3-by-2, B is M-by-N = 3-by-2.
    complex*16 :: AE(3, 2), BE(3, 2)
    double precision :: AE_r(12), BE_r(12)
    equivalence (AE, AE_r)
    equivalence (BE, BE_r)
    integer :: ii, jj, info2

    do jj = 1, 2
      do ii = 1, 3
        AE(ii, jj) = dcmplx(1.0d0 + dble(ii) + 0.3d0*dble(jj), 0.1d0*dble(ii+jj))
        BE(ii, jj) = dcmplx(-2.0d0 + 0.5d0*dble(ii) + dble(jj), 0.2d0*dble(ii*jj))
      end do
    end do
    call ztpmqrt('L', 'N', 3, 2, 3, 3, 2, BB_LK, 3, TLK, 2, AE, 3, BE, 3, WORK, info2)
    call begin_test('left_notrans_E')
    call print_int('info', info2)
    call print_array('A', AE_r, 12)
    call print_array('B', BE_r, 12)
    call end_test()

    do jj = 1, 2
      do ii = 1, 3
        AE(ii, jj) = dcmplx(1.0d0 + dble(ii) + 0.3d0*dble(jj), 0.1d0*dble(ii+jj))
        BE(ii, jj) = dcmplx(-2.0d0 + 0.5d0*dble(ii) + dble(jj), 0.2d0*dble(ii*jj))
      end do
    end do
    call ztpmqrt('L', 'C', 3, 2, 3, 3, 2, BB_LK, 3, TLK, 2, AE, 3, BE, 3, WORK, info2)
    call begin_test('left_ctrans_E')
    call print_int('info', info2)
    call print_array('A', AE_r, 12)
    call print_array('B', BE_r, 12)
    call end_test()
  end subroutine

  subroutine run_case_F()
    ! NB == K = 4, single inner iteration. SIDE='L', M=5, N=4, K=4, L=2.
    complex*16 :: AF(4, 4), BF(5, 4)
    double precision :: AF_r(32), BF_r(40)
    equivalence (AF, AF_r)
    equivalence (BF, BF_r)
    integer :: ii, jj, info2

    do jj = 1, 4
      do ii = 1, 4
        AF(ii, jj) = dcmplx(sin(dble(ii + jj*2)), cos(dble(ii*3 + jj))*0.2d0)
      end do
      do ii = 1, 5
        BF(ii, jj) = dcmplx(cos(dble(ii*2 + jj)) + 0.3d0, sin(dble(ii + jj*4))*0.3d0)
      end do
    end do
    call ztpmqrt('L', 'N', 5, 4, 4, 2, 4, BB_S, 5, TSG, 4, AF, 4, BF, 5, WORK, info2)
    call begin_test('left_notrans_F')
    call print_int('info', info2)
    call print_array('A', AF_r, 32)
    call print_array('B', BF_r, 40)
    call end_test()

    do jj = 1, 4
      do ii = 1, 4
        AF(ii, jj) = dcmplx(sin(dble(ii + jj*2)), cos(dble(ii*3 + jj))*0.2d0)
      end do
      do ii = 1, 5
        BF(ii, jj) = dcmplx(cos(dble(ii*2 + jj)) + 0.3d0, sin(dble(ii + jj*4))*0.3d0)
      end do
    end do
    call ztpmqrt('L', 'C', 5, 4, 4, 2, 4, BB_S, 5, TSG, 4, AF, 4, BF, 5, WORK, info2)
    call begin_test('left_ctrans_F')
    call print_int('info', info2)
    call print_array('A', AF_r, 32)
    call print_array('B', BF_r, 40)
    call end_test()
  end subroutine

end program
