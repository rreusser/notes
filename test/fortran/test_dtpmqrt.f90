program test_dtpmqrt
  use test_utils
  implicit none

  ! Build a compact-WY triangular-pentagonal QR factorization with DTPQRT,
  ! then apply the resulting Q to a stacked matrix C = [A; B] (left) or
  ! C = [A B] (right) using DTPMQRT.
  !
  ! Dimensions for each side:
  !   SIDE='L': V is M-by-K, A is K-by-N, B is M-by-N
  !   SIDE='R': V is N-by-K, A is M-by-K, B is M-by-N
  !
  ! L is the trapezoidal-rows count of V.

  ! Case A: M=4, N=3, K=3, L=2, NB=2 (left side)
  double precision :: AA_L(3, 3), BB_L(4, 3), TA(2, 3)
  double precision :: A_L(3, 3), B_L(4, 3)

  ! Case B: M=3, N=4, K=4, L=2, NB=2 (right side; V is N-by-K=4x4)
  double precision :: AA_R(4, 4), BB_R(3, 4), TR(2, 4)
  double precision :: A_R(3, 4), B_R(3, 4)

  ! Case C (large/blocked): M=8, N=6, K=6, L=3, NB=3 (left)
  double precision :: AA_LB(6, 6), BB_LB(8, 6), TLB(3, 6)
  double precision :: A_LB(6, 6), B_LB(8, 6)

  ! Case D: L=0 (purely rectangular V)
  double precision :: AA_L0(3, 3), BB_L0(5, 3), TL0(2, 3)
  double precision :: A_L0(3, 3), B_L0(5, 3)

  ! Case E: L=K (fully trapezoidal/triangular V)
  double precision :: AA_LK(3, 3), BB_LK(3, 3), TLK(2, 3)
  double precision :: A_LK(3, 3), B_LK(3, 3)

  ! Case F: NB == K (single block iteration)
  double precision :: AA_S(4, 4), BB_S(5, 4), TSG(4, 4)
  double precision :: A_S(4, 4), B_S(5, 4)

  double precision :: WORK(1024)
  integer :: info, i, j

  ! ========== Case A: M=4, N=3, K=3, L=2, NB=2, SIDE='L' ==========
  ! Build V/T via dtpqrt(M, N=K, L, NB, A, LDA, B, LDB, T, LDT, WORK, INFO)
  AA_L = 0.0d0; BB_L = 0.0d0; TA = 0.0d0
  ! Upper triangular A (KxK = 3x3)
  AA_L(1,1) = 2.0d0; AA_L(1,2) = 0.5d0; AA_L(1,3) = 0.25d0
                     AA_L(2,2) = 3.0d0; AA_L(2,3) = 0.75d0
                                        AA_L(3,3) = 4.0d0
  ! Pentagonal B (MxK=4x3): M-L = 2 rectangular rows + L=2 trapezoidal rows
  ! Rectangular block (rows 1..M-L=2):
  BB_L(1,1) = 0.7d0; BB_L(1,2) = 0.3d0; BB_L(1,3) = 0.4d0
  BB_L(2,1) = 0.2d0; BB_L(2,2) = 0.9d0; BB_L(2,3) = 0.1d0
  ! Trapezoidal block (rows M-L+1=3..M=4): first L=2 rows of upper triangular
  BB_L(3,1) = 1.1d0; BB_L(3,2) = 0.5d0; BB_L(3,3) = 0.6d0
                     BB_L(4,2) = 1.2d0; BB_L(4,3) = 0.7d0
  call dtpqrt(4, 3, 2, 2, AA_L, 3, BB_L, 4, TA, 2, WORK, info)

  call begin_test('factors_left_A')
  call print_int('info', info)
  call print_array('R', AA_L, 9)    ! upper R
  call print_array('V', BB_L, 12)   ! pentagonal V
  call print_array('T', TA, 6)
  call end_test()

  ! Test: SIDE='L', TRANS='N', apply Q to stacked C=[A; B] where A is K-by-N=3x3, B is M-by-N=4x3
  call init_AB_left(A_L, B_L)
  call dtpmqrt('L', 'N', 4, 3, 3, 2, 2, BB_L, 4, TA, 2, A_L, 3, B_L, 4, WORK, info)
  call begin_test('left_notrans_A')
  call print_int('info', info)
  call print_array('A', A_L, 9)
  call print_array('B', B_L, 12)
  call end_test()

  ! Test: SIDE='L', TRANS='T'
  call init_AB_left(A_L, B_L)
  call dtpmqrt('L', 'T', 4, 3, 3, 2, 2, BB_L, 4, TA, 2, A_L, 3, B_L, 4, WORK, info)
  call begin_test('left_trans_A')
  call print_int('info', info)
  call print_array('A', A_L, 9)
  call print_array('B', B_L, 12)
  call end_test()

  ! ========== Case B: M=3, N=4, K=4, L=2, NB=2, SIDE='R' ==========
  ! Build V/T using dtpqrt with M=N=4 (=row-dim of V), N=K=4, L=2
  AA_R = 0.0d0; BB_R = 0.0d0; TR = 0.0d0
  ! Upper triangular A (KxK=4x4)
  AA_R(1,1) = 2.5d0; AA_R(1,2) = 0.6d0; AA_R(1,3) = 0.2d0; AA_R(1,4) = 0.4d0
                     AA_R(2,2) = 3.5d0; AA_R(2,3) = 0.7d0; AA_R(2,4) = 0.3d0
                                        AA_R(3,3) = 2.0d0; AA_R(3,4) = 0.5d0
                                                           AA_R(4,4) = 4.0d0
  ! Pentagonal B (NxK=N=row-dim): rows are M_v=4 here? Actually for right side,
  ! we want V to be N-by-K = 4-by-4. So dtpqrt M_v=4, N_v=K=4, L=2.
  ! Wait — that conflicts with array shape BB_R(3,4). Need V of shape 4x4.
  ! Re-do with V shape 4-by-4:
  ! Drop these declarations and use a 4x4 pentagonal V.
  !
  ! (We will redeclare BB_R as needed below; for clarity use distinct arrays.)
  call run_case_B()

  ! ========== Case C: M=8, N=6, K=6, L=3, NB=3, SIDE='L' (blocked) ==========
  AA_LB = 0.0d0; BB_LB = 0.0d0; TLB = 0.0d0
  do j = 1, 6
    do i = 1, j
      AA_LB(i, j) = sin(dble(i + 2*j)) + 1.5d0
      if ( i == j ) AA_LB(i,j) = AA_LB(i,j) + 2.0d0
    end do
  end do
  ! Pentagonal B: M-L = 5 rectangular rows + L=3 trapezoidal rows of width K=6
  do j = 1, 6
    do i = 1, 5
      BB_LB(i, j) = cos(dble(i*2 + j)) + 0.2d0 * dble(i)
    end do
  end do
  ! Trapezoidal rows (rows M-L+1=6..M=8): first L=3 rows of 6x6 upper triangular
  do i = 1, 3
    do j = i, 6
      BB_LB(5+i, j) = sin(dble(i + j*3)) + 1.0d0
    end do
  end do
  call dtpqrt(8, 6, 3, 3, AA_LB, 6, BB_LB, 8, TLB, 3, WORK, info)

  call begin_test('factors_left_C')
  call print_int('info', info)
  call print_array('R', AA_LB, 36)
  call print_array('V', BB_LB, 48)
  call print_array('T', TLB, 18)
  call end_test()

  ! Apply for each (side='L', trans={N,T}). C = [A_LB; B_LB] with A_LB K-by-N=6x6, B_LB M-by-N=8x6.
  call init_AB_left_LB(A_LB, B_LB)
  call dtpmqrt('L', 'N', 8, 6, 6, 3, 3, BB_LB, 8, TLB, 3, A_LB, 6, B_LB, 8, WORK, info)
  call begin_test('left_notrans_C')
  call print_int('info', info)
  call print_array('A', A_LB, 36)
  call print_array('B', B_LB, 48)
  call end_test()

  call init_AB_left_LB(A_LB, B_LB)
  call dtpmqrt('L', 'T', 8, 6, 6, 3, 3, BB_LB, 8, TLB, 3, A_LB, 6, B_LB, 8, WORK, info)
  call begin_test('left_trans_C')
  call print_int('info', info)
  call print_array('A', A_LB, 36)
  call print_array('B', B_LB, 48)
  call end_test()

  ! ========== Case D: L=0 (rectangular V), SIDE='L', M=5, N=4, K=3, NB=2 ==========
  AA_L0 = 0.0d0; BB_L0 = 0.0d0; TL0 = 0.0d0
  AA_L0(1,1) = 2.0d0; AA_L0(1,2) = 0.4d0; AA_L0(1,3) = 0.3d0
                      AA_L0(2,2) = 2.5d0; AA_L0(2,3) = 0.5d0
                                          AA_L0(3,3) = 3.0d0
  do j = 1, 3
    do i = 1, 5
      BB_L0(i, j) = sin(dble(i + j*2)) + 0.1d0 * dble(i)
    end do
  end do
  call dtpqrt(5, 3, 0, 2, AA_L0, 3, BB_L0, 5, TL0, 2, WORK, info)

  call begin_test('factors_left_D')
  call print_int('info', info)
  call print_array('R', AA_L0, 9)
  call print_array('V', BB_L0, 15)
  call print_array('T', TL0, 6)
  call end_test()

  ! Apply: SIDE='L'. A is K-by-N = 3-by-4, B is M-by-N = 5-by-4 -> use scratch
  call run_case_D()

  ! ========== Case E: L=K (fully triangular V), M=K=3, N=2, NB=2 ==========
  AA_LK = 0.0d0; BB_LK = 0.0d0; TLK = 0.0d0
  AA_LK(1,1) = 2.0d0; AA_LK(1,2) = 0.5d0; AA_LK(1,3) = 0.3d0
                      AA_LK(2,2) = 3.0d0; AA_LK(2,3) = 0.4d0
                                          AA_LK(3,3) = 4.0d0
  ! M-L=0 rectangular rows; L=3 trapezoidal rows form a 3x3 upper triangular block
  BB_LK(1,1) = 1.2d0; BB_LK(1,2) = 0.6d0; BB_LK(1,3) = 0.2d0
                      BB_LK(2,2) = 1.5d0; BB_LK(2,3) = 0.4d0
                                          BB_LK(3,3) = 1.7d0
  call dtpqrt(3, 3, 3, 2, AA_LK, 3, BB_LK, 3, TLK, 2, WORK, info)

  call begin_test('factors_left_E')
  call print_int('info', info)
  call print_array('R', AA_LK, 9)
  call print_array('V', BB_LK, 9)
  call print_array('T', TLK, 6)
  call end_test()

  ! Apply: SIDE='L', M=3, N=2, K=3
  call run_case_E()

  ! ========== Case F: NB == K (single inner iteration), M=5, N=4, K=4, L=2, NB=4 ==========
  AA_S = 0.0d0; BB_S = 0.0d0; TSG = 0.0d0
  do j = 1, 4
    do i = 1, j
      AA_S(i, j) = sin(dble(i*3 + j)) + 1.0d0
      if ( i == j ) AA_S(i,j) = AA_S(i,j) + 1.5d0
    end do
  end do
  ! Pentagonal: M-L=3 rect rows + L=2 trapezoidal rows of width K=4
  do j = 1, 4
    do i = 1, 3
      BB_S(i, j) = cos(dble(i + j*3)) + 0.5d0
    end do
  end do
  do i = 1, 2
    do j = i, 4
      BB_S(3+i, j) = sin(dble(i*5 + j)) + 1.0d0
    end do
  end do
  call dtpqrt(5, 4, 2, 4, AA_S, 4, BB_S, 5, TSG, 4, WORK, info)

  call begin_test('factors_single')
  call print_int('info', info)
  call print_array('R', AA_S, 16)
  call print_array('V', BB_S, 20)
  call print_array('T', TSG, 16)
  call end_test()

  call run_case_F()

  ! ========== Quick returns ==========
  call dtpmqrt('L', 'N', 0, 3, 3, 2, 2, BB_L, 4, TA, 2, AA_L, 3, BB_L, 4, WORK, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  call dtpmqrt('L', 'N', 4, 0, 3, 2, 2, BB_L, 4, TA, 2, AA_L, 3, BB_L, 4, WORK, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  call dtpmqrt('L', 'N', 4, 3, 0, 0, 1, BB_L, 4, TA, 2, AA_L, 3, BB_L, 4, WORK, info)
  call begin_test('k_zero')
  call print_int('info', info)
  call end_test()

contains

  subroutine init_AB_left(A_, B_)
    double precision, intent(out) :: A_(3,3), B_(4,3)
    A_(1,1) = 1.0d0;  A_(1,2) = 2.0d0;  A_(1,3) = 3.0d0
    A_(2,1) = 4.0d0;  A_(2,2) = 5.0d0;  A_(2,3) = 6.0d0
    A_(3,1) = 7.0d0;  A_(3,2) = 8.0d0;  A_(3,3) = 9.0d0
    B_(1,1) = 1.0d0;  B_(1,2) = -1.0d0; B_(1,3) = 2.0d0
    B_(2,1) = 0.5d0;  B_(2,2) = 1.5d0;  B_(2,3) = -2.5d0
    B_(3,1) = -1.0d0; B_(3,2) = 0.0d0;  B_(3,3) = 3.0d0
    B_(4,1) = 2.0d0;  B_(4,2) = -2.0d0; B_(4,3) = 1.0d0
  end subroutine

  subroutine init_AB_left_LB(A_, B_)
    double precision, intent(out) :: A_(6,6), B_(8,6)
    integer :: ii, jj
    do jj = 1, 6
      do ii = 1, 6
        A_(ii,jj) = sin(dble(ii*2 + jj))
      end do
    end do
    do jj = 1, 6
      do ii = 1, 8
        B_(ii,jj) = cos(dble(ii + jj*2)) + 0.5d0
      end do
    end do
  end subroutine

  subroutine run_case_B()
    ! M=3, N=4, K=4, L=2, NB=2, SIDE='R'
    ! V is 4x4 pentagonal (M_v = K = 4), A is M-by-K = 3-by-4, B is M-by-N = 3-by-4
    double precision :: AAB(4, 4), BBV(4, 4), TB(2, 4)
    double precision :: A_R2(3, 4), B_R2(3, 4)
    integer :: ii, jj, info2

    AAB = 0.0d0; BBV = 0.0d0; TB = 0.0d0
    AAB(1,1) = 2.5d0; AAB(1,2) = 0.6d0; AAB(1,3) = 0.2d0; AAB(1,4) = 0.4d0
                      AAB(2,2) = 3.5d0; AAB(2,3) = 0.7d0; AAB(2,4) = 0.3d0
                                        AAB(3,3) = 2.0d0; AAB(3,4) = 0.5d0
                                                          AAB(4,4) = 4.0d0
    ! Pentagonal V: rows 1..M_v-L=2 rectangular, rows 3..4 trapezoidal of K=4 width.
    BBV(1,1) = 0.7d0; BBV(1,2) = 0.3d0; BBV(1,3) = 0.4d0; BBV(1,4) = 0.5d0
    BBV(2,1) = 0.2d0; BBV(2,2) = 0.9d0; BBV(2,3) = 0.1d0; BBV(2,4) = 0.6d0
    ! Trapezoidal rows: first L=2 rows of 4x4 upper triangular
    BBV(3,1) = 1.1d0; BBV(3,2) = 0.5d0; BBV(3,3) = 0.6d0; BBV(3,4) = 0.7d0
                      BBV(4,2) = 1.2d0; BBV(4,3) = 0.7d0; BBV(4,4) = 0.4d0
    call dtpqrt(4, 4, 2, 2, AAB, 4, BBV, 4, TB, 2, WORK, info2)

    call begin_test('factors_right_B')
    call print_int('info', info2)
    call print_array('R', AAB, 16)
    call print_array('V', BBV, 16)
    call print_array('T', TB, 8)
    call end_test()

    ! Initialize A (3x4) and B (3x4)
    do jj = 1, 4
      do ii = 1, 3
        A_R2(ii,jj) = 1.0d0 + dble(ii) + 0.1d0*dble(jj)
        B_R2(ii,jj) = -1.0d0 + 0.5d0*dble(ii) + dble(jj)
      end do
    end do
    call dtpmqrt('R', 'N', 3, 4, 4, 2, 2, BBV, 4, TB, 2, A_R2, 3, B_R2, 3, WORK, info2)
    call begin_test('right_notrans_B')
    call print_int('info', info2)
    call print_array('A', A_R2, 12)
    call print_array('B', B_R2, 12)
    call end_test()

    do jj = 1, 4
      do ii = 1, 3
        A_R2(ii,jj) = 1.0d0 + dble(ii) + 0.1d0*dble(jj)
        B_R2(ii,jj) = -1.0d0 + 0.5d0*dble(ii) + dble(jj)
      end do
    end do
    call dtpmqrt('R', 'T', 3, 4, 4, 2, 2, BBV, 4, TB, 2, A_R2, 3, B_R2, 3, WORK, info2)
    call begin_test('right_trans_B')
    call print_int('info', info2)
    call print_array('A', A_R2, 12)
    call print_array('B', B_R2, 12)
    call end_test()

    ! Same V/T applied to right with blocked-ish iteration through 'R'/'T' (uses backward iter).
    ! Already tested above.
  end subroutine

  subroutine run_case_D()
    ! Apply V/T (L=0) for both sides
    double precision :: AD(3, 4), BD(5, 4)
    double precision :: AD_R(3, 5), BD_R(3, 4)
    integer :: ii, jj, info2

    do jj = 1, 4
      do ii = 1, 3
        AD(ii, jj) = dble(ii) + 0.5d0*dble(jj)
      end do
      do ii = 1, 5
        BD(ii, jj) = -dble(ii) + dble(jj)
      end do
    end do
    call dtpmqrt('L', 'N', 5, 4, 3, 0, 2, BB_L0, 5, TL0, 2, AD, 3, BD, 5, WORK, info2)
    call begin_test('left_notrans_D')
    call print_int('info', info2)
    call print_array('A', AD, 12)
    call print_array('B', BD, 20)
    call end_test()

    do jj = 1, 4
      do ii = 1, 3
        AD(ii, jj) = dble(ii) + 0.5d0*dble(jj)
      end do
      do ii = 1, 5
        BD(ii, jj) = -dble(ii) + dble(jj)
      end do
    end do
    call dtpmqrt('L', 'T', 5, 4, 3, 0, 2, BB_L0, 5, TL0, 2, AD, 3, BD, 5, WORK, info2)
    call begin_test('left_trans_D')
    call print_int('info', info2)
    call print_array('A', AD, 12)
    call print_array('B', BD, 20)
    call end_test()

    ! Right side reuse with M=3, N=5, K=3, L=0 needs V of shape N-by-K = 5x3
    ! BB_L0 is 5x3 already, V dim matches.
    do jj = 1, 3
      do ii = 1, 3
        AD_R(ii, jj) = dble(ii) + 0.5d0*dble(jj)
      end do
    end do
    do jj = 1, 4
      do ii = 1, 3
        BD_R(ii, jj) = -dble(ii) + dble(jj)
      end do
    end do
    call dtpmqrt('R', 'N', 3, 4, 3, 0, 2, BB_L0, 5, TL0, 2, AD_R, 3, BD_R, 3, WORK, info2)
    call begin_test('right_notrans_D')
    call print_int('info', info2)
    call print_array('A', AD_R, 9)
    call print_array('B', BD_R, 12)
    call end_test()

    do jj = 1, 3
      do ii = 1, 3
        AD_R(ii, jj) = dble(ii) + 0.5d0*dble(jj)
      end do
    end do
    do jj = 1, 4
      do ii = 1, 3
        BD_R(ii, jj) = -dble(ii) + dble(jj)
      end do
    end do
    call dtpmqrt('R', 'T', 3, 4, 3, 0, 2, BB_L0, 5, TL0, 2, AD_R, 3, BD_R, 3, WORK, info2)
    call begin_test('right_trans_D')
    call print_int('info', info2)
    call print_array('A', AD_R, 9)
    call print_array('B', BD_R, 12)
    call end_test()
  end subroutine

  subroutine run_case_E()
    ! L=K, SIDE='L'. M=3, N=2, K=3.
    ! A is K-by-N = 3-by-2, B is M-by-N = 3-by-2.
    double precision :: AE(3, 2), BE(3, 2)
    integer :: ii, jj, info2

    do jj = 1, 2
      do ii = 1, 3
        AE(ii, jj) = 1.0d0 + dble(ii) + 0.3d0*dble(jj)
        BE(ii, jj) = -2.0d0 + 0.5d0*dble(ii) + dble(jj)
      end do
    end do
    call dtpmqrt('L', 'N', 3, 2, 3, 3, 2, BB_LK, 3, TLK, 2, AE, 3, BE, 3, WORK, info2)
    call begin_test('left_notrans_E')
    call print_int('info', info2)
    call print_array('A', AE, 6)
    call print_array('B', BE, 6)
    call end_test()

    do jj = 1, 2
      do ii = 1, 3
        AE(ii, jj) = 1.0d0 + dble(ii) + 0.3d0*dble(jj)
        BE(ii, jj) = -2.0d0 + 0.5d0*dble(ii) + dble(jj)
      end do
    end do
    call dtpmqrt('L', 'T', 3, 2, 3, 3, 2, BB_LK, 3, TLK, 2, AE, 3, BE, 3, WORK, info2)
    call begin_test('left_trans_E')
    call print_int('info', info2)
    call print_array('A', AE, 6)
    call print_array('B', BE, 6)
    call end_test()
  end subroutine

  subroutine run_case_F()
    ! NB == K = 4, single inner iteration. SIDE='L', M=5, N=4, K=4, L=2.
    double precision :: AF(4, 4), BF(5, 4)
    integer :: ii, jj, info2

    do jj = 1, 4
      do ii = 1, 4
        AF(ii, jj) = sin(dble(ii + jj*2))
      end do
      do ii = 1, 5
        BF(ii, jj) = cos(dble(ii*2 + jj)) + 0.3d0
      end do
    end do
    call dtpmqrt('L', 'N', 5, 4, 4, 2, 4, BB_S, 5, TSG, 4, AF, 4, BF, 5, WORK, info2)
    call begin_test('left_notrans_F')
    call print_int('info', info2)
    call print_array('A', AF, 16)
    call print_array('B', BF, 20)
    call end_test()

    do jj = 1, 4
      do ii = 1, 4
        AF(ii, jj) = sin(dble(ii + jj*2))
      end do
      do ii = 1, 5
        BF(ii, jj) = cos(dble(ii*2 + jj)) + 0.3d0
      end do
    end do
    call dtpmqrt('L', 'T', 5, 4, 4, 2, 4, BB_S, 5, TSG, 4, AF, 4, BF, 5, WORK, info2)
    call begin_test('left_trans_F')
    call print_int('info', info2)
    call print_array('A', AF, 16)
    call print_array('B', BF, 20)
    call end_test()
  end subroutine

end program
