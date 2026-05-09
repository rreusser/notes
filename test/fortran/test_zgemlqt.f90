program test_zgemlqt
  use test_utils
  implicit none

  ! Compact-WY LQ factors produced by ZGELQT, then applied by ZGEMLQT.
  ! In LQ: zgelqt(K, Q, mb, A, ...) produces V (rows of A) and T such
  ! that the product Q_unit = H(K) H(K-1) ... H(1) is order Q.
  ! For zgemlqt with SIDE='L', Q_unit must be order M, so we factor a
  ! K-by-M matrix; for SIDE='R', Q_unit is order N, so we factor a
  ! K-by-N matrix.

  integer :: INFO

  ! Small case: K=3, Q=4, MB=2 (left-side reflectors).
  call run_small_lq_left()

  ! Small right-side case: K=3, Q=3, MB=2.
  call run_small_lq_right()

  ! Multi-block case: K=8, Q=10, MB=3 (K not divisible by MB).
  call run_block_lq()

  ! Single-block case: MB == K.
  call run_single_lq()

contains

  ! ====================================================================
  ! SMALL CASE: factor 3x4 -> use as left-side Q (order M=4).
  ! Provides V (3-by-4), T (2-by-3), tested on identity and dense 4x4.
  ! ====================================================================
  subroutine run_small_lq_left()
    complex*16 :: A(3, 4), T(2, 3), WORK(4*32)
    complex*16 :: C44(4, 4), CR43(4, 3)
    double precision :: A_r(2, 3, 4), T_r(2, 2, 3)
    double precision :: C44_r(2, 4, 4), CR43_r(2, 4, 3)
    integer :: i
    equivalence (A, A_r); equivalence (T, T_r)
    equivalence (C44, C44_r); equivalence (CR43, CR43_r)

    A(1,1) = (1.0d0,  0.2d0); A(1,2) = (5.0d0, -0.1d0); A(1,3) = (9.0d0,  0.3d0); A(1,4) = (13.0d0, -0.2d0)
    A(2,1) = (2.0d0, -0.3d0); A(2,2) = (6.0d0,  0.4d0); A(2,3) = (10.0d0, -0.2d0); A(2,4) = (14.0d0,  0.1d0)
    A(3,1) = (3.0d0,  0.5d0); A(3,2) = (7.0d0, -0.3d0); A(3,3) = (11.0d0, 0.4d0); A(3,4) = (15.0d0, -0.5d0)
    T = (0.0d0, 0.0d0)
    call ZGELQT(3, 4, 2, A, 3, T, 2, WORK, INFO)
    call begin_test('lq_factors_small')
    call print_int('info', INFO)
    call print_array('v', A_r, 2*3*4)
    call print_array('t', T_r, 2*2*3)
    call end_test()

    ! left, no-trans: C := Q*I_4
    C44 = (0.0d0, 0.0d0)
    do i = 1, 4
      C44(i, i) = (1.0d0, 0.0d0)
    end do
    call ZGEMLQT('L', 'N', 4, 4, 3, 2, A, 3, T, 2, C44, 4, WORK, INFO)
    call begin_test('left_notrans')
    call print_int('info', INFO)
    call print_array('c', C44_r, 2*4*4)
    call end_test()

    ! left, conj-trans: C := Q^H * I_4
    C44 = (0.0d0, 0.0d0)
    do i = 1, 4
      C44(i, i) = (1.0d0, 0.0d0)
    end do
    call ZGEMLQT('L', 'C', 4, 4, 3, 2, A, 3, T, 2, C44, 4, WORK, INFO)
    call begin_test('left_ctrans')
    call print_int('info', INFO)
    call print_array('c', C44_r, 2*4*4)
    call end_test()

    ! left, no-trans on dense complex 4x4
    call set_dense44(C44)
    call ZGEMLQT('L', 'N', 4, 4, 3, 2, A, 3, T, 2, C44, 4, WORK, INFO)
    call begin_test('left_notrans_dense')
    call print_int('info', INFO)
    call print_array('c', C44_r, 2*4*4)
    call end_test()

    ! left, conj-trans on dense complex 4x4
    call set_dense44(C44)
    call ZGEMLQT('L', 'C', 4, 4, 3, 2, A, 3, T, 2, C44, 4, WORK, INFO)
    call begin_test('left_ctrans_dense')
    call print_int('info', INFO)
    call print_array('c', C44_r, 2*4*4)
    call end_test()

    ! Quick-return tests.
    call ZGEMLQT('L', 'N', 0, 4, 0, 2, A, 3, T, 2, C44, 4, WORK, INFO)
    call begin_test('m_zero')
    call print_int('info', INFO)
    call end_test()

    call ZGEMLQT('L', 'N', 4, 0, 0, 2, A, 3, T, 2, C44, 4, WORK, INFO)
    call begin_test('n_zero')
    call print_int('info', INFO)
    call end_test()

    call ZGEMLQT('L', 'N', 4, 4, 0, 2, A, 3, T, 2, C44, 4, WORK, INFO)
    call begin_test('k_zero')
    call print_int('info', INFO)
    call end_test()
  end subroutine

  subroutine set_dense44(C)
    complex*16 :: C(4, 4)
    C(1,1) = (1.0d0,  0.5d0); C(2,1) = (2.0d0, -0.3d0); C(3,1) = (-1.0d0,  0.4d0); C(4,1) = (3.0d0, -0.2d0)
    C(1,2) = (-2.0d0, 0.1d0); C(2,2) = (1.0d0,  0.4d0); C(3,2) = (4.0d0, -0.5d0);  C(4,2) = (0.0d0,  0.3d0)
    C(1,3) = (3.0d0, -0.4d0); C(2,3) = (-1.0d0, 0.2d0); C(3,3) = (2.0d0,  0.5d0);  C(4,3) = (1.0d0, -0.1d0)
    C(1,4) = (0.0d0,  0.3d0); C(2,4) = (5.0d0, -0.2d0); C(3,4) = (-2.0d0, 0.4d0);  C(4,4) = (4.0d0,  0.1d0)
  end subroutine

  ! ====================================================================
  ! SMALL RIGHT CASE: factor 3x3 -> use as right-side Q (order N=3),
  ! tested with rectangular C(4,3).
  ! ====================================================================
  subroutine run_small_lq_right()
    complex*16 :: AR(3, 3), TR(2, 3), WORK(4*32)
    complex*16 :: CR43(4, 3)
    double precision :: AR_r(2, 3, 3), TR_r(2, 2, 3)
    double precision :: CR43_r(2, 4, 3)
    equivalence (AR, AR_r); equivalence (TR, TR_r)
    equivalence (CR43, CR43_r)

    AR(1,1) = (2.0d0,  0.3d0); AR(1,2) = (-1.0d0, 0.2d0); AR(1,3) = (0.5d0,  0.4d0)
    AR(2,1) = (1.0d0, -0.5d0); AR(2,2) = (3.0d0, -0.1d0); AR(2,3) = (-2.0d0, 0.3d0)
    AR(3,1) = (-0.5d0, 0.4d0); AR(3,2) = (1.5d0,  0.2d0); AR(3,3) = (4.0d0, -0.3d0)
    TR = (0.0d0, 0.0d0)
    call ZGELQT(3, 3, 2, AR, 3, TR, 2, WORK, INFO)
    call begin_test('lq_factors_rsmall')
    call print_int('info', INFO)
    call print_array('v', AR_r, 2*3*3)
    call print_array('t', TR_r, 2*2*3)
    call end_test()

    ! right, no-trans on dense 4x3
    call set_rect43(CR43)
    call ZGEMLQT('R', 'N', 4, 3, 3, 2, AR, 3, TR, 2, CR43, 4, WORK, INFO)
    call begin_test('right_notrans_rect')
    call print_int('info', INFO)
    call print_array('c', CR43_r, 2*4*3)
    call end_test()

    ! right, conj-trans on dense 4x3
    call set_rect43(CR43)
    call ZGEMLQT('R', 'C', 4, 3, 3, 2, AR, 3, TR, 2, CR43, 4, WORK, INFO)
    call begin_test('right_ctrans_rect')
    call print_int('info', INFO)
    call print_array('c', CR43_r, 2*4*3)
    call end_test()
  end subroutine

  subroutine set_rect43(C)
    complex*16 :: C(4, 3)
    C(1,1) = (1.0d0,  0.3d0); C(2,1) = (0.0d0,  0.2d0); C(3,1) = (2.0d0, -0.1d0); C(4,1) = (-1.0d0, 0.4d0)
    C(1,2) = (2.0d0, -0.4d0); C(2,2) = (1.0d0,  0.5d0); C(3,2) = (-1.0d0, 0.2d0); C(4,2) = (3.0d0, -0.3d0)
    C(1,3) = (-1.0d0, 0.5d0); C(2,3) = (3.0d0, -0.2d0); C(3,3) = (0.0d0,  0.4d0); C(4,3) = (-2.0d0, 0.1d0)
  end subroutine

  ! ====================================================================
  ! MULTI-BLOCK CASE: K=8, Q=10, MB=3 (K not divisible by MB).
  ! Q_unit is order 10. Tested on a 10x10 dense matrix with all four
  ! SIDE x TRANS combinations.
  ! ====================================================================
  subroutine run_block_lq()
    complex*16 :: AL(8, 10), TL(3, 8), WORK(8*32)
    complex*16 :: CLL(10, 10)
    double precision :: AL_r(2, 8, 10), TL_r(2, 3, 8)
    double precision :: CLL_r(2, 10, 10)
    integer :: i, j
    equivalence (AL, AL_r); equivalence (TL, TL_r)
    equivalence (CLL, CLL_r)

    do j = 1, 10
      do i = 1, 8
        AL(i, j) = cmplx(sin(dble(i + 3*j)) + 0.1d0 * dble(i), &
                          0.2d0*cos(dble(2*i + j)), kind=8)
      end do
    end do
    TL = (0.0d0, 0.0d0)
    call ZGELQT(8, 10, 3, AL, 8, TL, 3, WORK, INFO)
    call begin_test('lq_factors_block')
    call print_int('info', INFO)
    call print_array('v', AL_r, 2*8*10)
    call print_array('t', TL_r, 2*3*8)
    call end_test()

    call fillCLL(CLL)
    call ZGEMLQT('L', 'N', 10, 10, 8, 3, AL, 8, TL, 3, CLL, 10, WORK, INFO)
    call begin_test('left_notrans_block')
    call print_int('info', INFO)
    call print_array('c', CLL_r, 2*10*10)
    call end_test()

    call fillCLL(CLL)
    call ZGEMLQT('L', 'C', 10, 10, 8, 3, AL, 8, TL, 3, CLL, 10, WORK, INFO)
    call begin_test('left_ctrans_block')
    call print_int('info', INFO)
    call print_array('c', CLL_r, 2*10*10)
    call end_test()

    call fillCLL(CLL)
    call ZGEMLQT('R', 'N', 10, 10, 8, 3, AL, 8, TL, 3, CLL, 10, WORK, INFO)
    call begin_test('right_notrans_block')
    call print_int('info', INFO)
    call print_array('c', CLL_r, 2*10*10)
    call end_test()

    call fillCLL(CLL)
    call ZGEMLQT('R', 'C', 10, 10, 8, 3, AL, 8, TL, 3, CLL, 10, WORK, INFO)
    call begin_test('right_ctrans_block')
    call print_int('info', INFO)
    call print_array('c', CLL_r, 2*10*10)
    call end_test()
  end subroutine

  subroutine fillCLL(C)
    complex*16 :: C(10, 10)
    integer :: i, j
    do j = 1, 10
      do i = 1, 10
        C(i, j) = cmplx(cos(dble(i) + dble(j)) + 0.5d0, &
                         0.3d0*sin(dble(i + 2*j)), kind=8)
      end do
    end do
  end subroutine

  ! ====================================================================
  ! SINGLE-BLOCK CASE: MB == K (one inner-loop iteration).
  ! ====================================================================
  subroutine run_single_lq()
    complex*16 :: AS(4, 5), TS(4, 4), WORK(5*32)
    complex*16 :: CSS(5, 5)
    double precision :: AS_r(2, 4, 5), TS_r(2, 4, 4)
    double precision :: CSS_r(2, 5, 5)
    integer :: i, j
    equivalence (AS, AS_r); equivalence (TS, TS_r)
    equivalence (CSS, CSS_r)

    do j = 1, 5
      do i = 1, 4
        AS(i, j) = cmplx(sin(dble(i*2 + j*3)) + 0.05d0 * dble(i), &
                          0.15d0*cos(dble(i + 2*j)), kind=8)
      end do
    end do
    TS = (0.0d0, 0.0d0)
    call ZGELQT(4, 5, 4, AS, 4, TS, 4, WORK, INFO)
    call begin_test('lq_factors_single')
    call print_int('info', INFO)
    call print_array('v', AS_r, 2*4*5)
    call print_array('t', TS_r, 2*4*4)
    call end_test()

    do j = 1, 5
      do i = 1, 5
        CSS(i, j) = cmplx(sin(dble(i + 2*j)), &
                           0.2d0*cos(dble(2*i + j)), kind=8)
      end do
    end do
    call ZGEMLQT('L', 'N', 5, 5, 4, 4, AS, 4, TS, 4, CSS, 5, WORK, INFO)
    call begin_test('left_notrans_single')
    call print_int('info', INFO)
    call print_array('c', CSS_r, 2*5*5)
    call end_test()

    do j = 1, 5
      do i = 1, 5
        CSS(i, j) = cmplx(sin(dble(i + 2*j)), &
                           0.2d0*cos(dble(2*i + j)), kind=8)
      end do
    end do
    call ZGEMLQT('L', 'C', 5, 5, 4, 4, AS, 4, TS, 4, CSS, 5, WORK, INFO)
    call begin_test('left_ctrans_single')
    call print_int('info', INFO)
    call print_array('c', CSS_r, 2*5*5)
    call end_test()
  end subroutine

end program
