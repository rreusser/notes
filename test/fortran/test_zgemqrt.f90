program test_zgemqrt
  use test_utils
  implicit none

  ! Compact WY QR factors produced by ZGEQRT, then applied by ZGEMQRT.
  ! Use container sizes that match the trimmed dimensions to keep
  ! EQUIVALENCE-style printing trivial.

  ! Small case: M=4, N=3, NB=2 (block-pass exercises both single-block
  ! and partial-block iterations).
  complex*16 :: A(4, 3), T(2, 3)
  complex*16 :: C44(4, 4), CR34(3, 4), CR44(4, 4)
  complex*16 :: WORK(4*32)
  double precision :: A_r(2*4*3), T_r(2*2*3)
  double precision :: C44_r(2*4*4), CR34_r(2*3*4), CR44_r(2*4*4)
  equivalence (A, A_r)
  equivalence (T, T_r)
  equivalence (C44, C44_r)
  equivalence (CR34, CR34_r)
  equivalence (CR44, CR44_r)

  ! Larger case: M=10, N=10, K=8, NB=3 (multi-block, K not divisible by NB).
  complex*16 :: AL(10, 8), TL(3, 8)
  complex*16 :: CLL(10, 10), CLR(10, 10)
  double precision :: AL_r(2*10*8), TL_r(2*3*8)
  double precision :: CLL_r(2*10*10), CLR_r(2*10*10)
  equivalence (AL, AL_r)
  equivalence (TL, TL_r)
  equivalence (CLL, CLL_r)
  equivalence (CLR, CLR_r)

  ! Single-block case: NB == K (no inner-loop iterations beyond one).
  complex*16 :: AS(5, 4), TS(4, 4)
  complex*16 :: CSS(5, 5)
  double precision :: AS_r(2*5*4), TS_r(2*4*4)
  double precision :: CSS_r(2*5*5)
  equivalence (AS, AS_r)
  equivalence (TS, TS_r)
  equivalence (CSS, CSS_r)

  integer :: info, i, j

  ! ===== Build a 4x3 compact-WY QR factorization =====
  A(1,1) = (1.0d0, 0.5d0);  A(2,1) = (2.0d0, -0.3d0); A(3,1) = (3.0d0, 0.2d0); A(4,1) = (4.0d0, -0.1d0)
  A(1,2) = (5.0d0, 0.4d0);  A(2,2) = (6.0d0, -0.5d0); A(3,2) = (7.0d0, 0.1d0); A(4,2) = (8.0d0, 0.3d0)
  A(1,3) = (9.0d0, -0.2d0); A(2,3) = (10.0d0, 0.6d0); A(3,3) = (11.0d0, -0.4d0); A(4,3) = (12.0d0, 0.7d0)
  T = (0.0d0, 0.0d0)
  call zgeqrt(4, 3, 2, A, 4, T, 2, WORK, info)

  call begin_test('qr_factors_small')
  call print_int('info', info)
  call print_array('v', A_r, 24)
  call print_array('t', T_r, 12)
  call end_test()

  ! Test 1: SIDE='L', TRANS='N' -> C := Q * I_4
  C44 = (0.0d0, 0.0d0)
  do i = 1, 4
    C44(i, i) = (1.0d0, 0.0d0)
  end do
  call zgemqrt('L', 'N', 4, 4, 3, 2, A, 4, T, 2, C44, 4, WORK, info)
  call begin_test('left_notrans')
  call print_int('info', info)
  call print_array('c', C44_r, 32)
  call end_test()

  ! Test 2: SIDE='L', TRANS='C' -> C := Q^H * I_4
  C44 = (0.0d0, 0.0d0)
  do i = 1, 4
    C44(i, i) = (1.0d0, 0.0d0)
  end do
  call zgemqrt('L', 'C', 4, 4, 3, 2, A, 4, T, 2, C44, 4, WORK, info)
  call begin_test('left_ctrans')
  call print_int('info', info)
  call print_array('c', C44_r, 32)
  call end_test()

  ! Test 3: SIDE='R', TRANS='N' with rectangular C(3,4) -> C := C * Q
  CR34(1,1) = (1.0d0, 0.0d0);  CR34(2,1) = (0.0d0, 1.0d0);  CR34(3,1) = (2.0d0, -0.5d0)
  CR34(1,2) = (2.0d0, 0.5d0);  CR34(2,2) = (1.0d0, -0.2d0); CR34(3,2) = (-1.0d0, 0.3d0)
  CR34(1,3) = (-1.0d0, 0.7d0); CR34(2,3) = (3.0d0, 0.0d0);  CR34(3,3) = (0.0d0, 1.5d0)
  CR34(1,4) = (4.0d0, -0.4d0); CR34(2,4) = (-2.0d0, 0.6d0); CR34(3,4) = (1.0d0, 0.1d0)
  call zgemqrt('R', 'N', 3, 4, 3, 2, A, 4, T, 2, CR34, 3, WORK, info)
  call begin_test('right_notrans_rect')
  call print_int('info', info)
  call print_array('c', CR34_r, 24)
  call end_test()

  ! Test 4: SIDE='R', TRANS='C' on rectangular C(3,4) -> C := C * Q^H
  CR34(1,1) = (1.0d0, 0.0d0);  CR34(2,1) = (0.0d0, 1.0d0);  CR34(3,1) = (2.0d0, -0.5d0)
  CR34(1,2) = (2.0d0, 0.5d0);  CR34(2,2) = (1.0d0, -0.2d0); CR34(3,2) = (-1.0d0, 0.3d0)
  CR34(1,3) = (-1.0d0, 0.7d0); CR34(2,3) = (3.0d0, 0.0d0);  CR34(3,3) = (0.0d0, 1.5d0)
  CR34(1,4) = (4.0d0, -0.4d0); CR34(2,4) = (-2.0d0, 0.6d0); CR34(3,4) = (1.0d0, 0.1d0)
  call zgemqrt('R', 'C', 3, 4, 3, 2, A, 4, T, 2, CR34, 3, WORK, info)
  call begin_test('right_ctrans_rect')
  call print_int('info', info)
  call print_array('c', CR34_r, 24)
  call end_test()

  ! Test 5: Left, no-trans on 4x4 dense complex (exercises blocked path)
  CR44(1,1) = (1.0d0, 0.2d0);  CR44(2,1) = (2.0d0, -0.5d0); CR44(3,1) = (-1.0d0, 0.8d0); CR44(4,1) = (3.0d0, -0.1d0)
  CR44(1,2) = (-2.0d0, 0.4d0); CR44(2,2) = (1.0d0, 0.6d0);  CR44(3,2) = (4.0d0, -0.7d0); CR44(4,2) = (0.0d0, 1.0d0)
  CR44(1,3) = (3.0d0, -0.3d0); CR44(2,3) = (-1.0d0, 0.9d0); CR44(3,3) = (2.0d0, 0.0d0);  CR44(4,3) = (1.0d0, -0.4d0)
  CR44(1,4) = (0.0d0, 0.5d0);  CR44(2,4) = (5.0d0, -0.2d0); CR44(3,4) = (-2.0d0, 0.7d0); CR44(4,4) = (4.0d0, 0.3d0)
  call zgemqrt('L', 'N', 4, 4, 3, 2, A, 4, T, 2, CR44, 4, WORK, info)
  call begin_test('left_notrans_dense')
  call print_int('info', info)
  call print_array('c', CR44_r, 32)
  call end_test()

  ! Test 6: Left, ctrans on the same 4x4 dense matrix
  CR44(1,1) = (1.0d0, 0.2d0);  CR44(2,1) = (2.0d0, -0.5d0); CR44(3,1) = (-1.0d0, 0.8d0); CR44(4,1) = (3.0d0, -0.1d0)
  CR44(1,2) = (-2.0d0, 0.4d0); CR44(2,2) = (1.0d0, 0.6d0);  CR44(3,2) = (4.0d0, -0.7d0); CR44(4,2) = (0.0d0, 1.0d0)
  CR44(1,3) = (3.0d0, -0.3d0); CR44(2,3) = (-1.0d0, 0.9d0); CR44(3,3) = (2.0d0, 0.0d0);  CR44(4,3) = (1.0d0, -0.4d0)
  CR44(1,4) = (0.0d0, 0.5d0);  CR44(2,4) = (5.0d0, -0.2d0); CR44(3,4) = (-2.0d0, 0.7d0); CR44(4,4) = (4.0d0, 0.3d0)
  call zgemqrt('L', 'C', 4, 4, 3, 2, A, 4, T, 2, CR44, 4, WORK, info)
  call begin_test('left_ctrans_dense')
  call print_int('info', info)
  call print_array('c', CR44_r, 32)
  call end_test()

  ! Quick-return tests: M=0, N=0, K=0
  call zgemqrt('L', 'N', 0, 4, 0, 2, A, 4, T, 2, C44, 4, WORK, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  call zgemqrt('L', 'N', 4, 0, 0, 2, A, 4, T, 2, C44, 4, WORK, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  call zgemqrt('L', 'N', 4, 4, 0, 2, A, 4, T, 2, C44, 4, WORK, info)
  call begin_test('k_zero')
  call print_int('info', info)
  call end_test()

  ! ===== Multi-block case: M=10, N=10, K=8, NB=3 (K%NB != 0) =====
  do j = 1, 8
    do i = 1, 10
      AL(i, j) = dcmplx(sin(dble(i + 3*j)) + 0.1d0 * dble(i), &
                        cos(dble(i*2 + j)) * 0.3d0)
    end do
  end do
  TL = (0.0d0, 0.0d0)
  call zgeqrt(10, 8, 3, AL, 10, TL, 3, WORK, info)

  call begin_test('qr_factors_block')
  call print_int('info', info)
  call print_array('v', AL_r, 160)
  call print_array('t', TL_r, 48)
  call end_test()

  ! Left, no-trans: applies Q to a non-identity 10x10 dense matrix
  do j = 1, 10
    do i = 1, 10
      CLL(i, j) = dcmplx(cos(dble(i) + dble(j)) + 0.5d0, &
                         sin(dble(i*3) - dble(j)) * 0.4d0)
    end do
  end do
  call zgemqrt('L', 'N', 10, 10, 8, 3, AL, 10, TL, 3, CLL, 10, WORK, info)
  call begin_test('left_notrans_block')
  call print_int('info', info)
  call print_array('c', CLL_r, 200)
  call end_test()

  ! Left, ctrans
  do j = 1, 10
    do i = 1, 10
      CLL(i, j) = dcmplx(cos(dble(i) + dble(j)) + 0.5d0, &
                         sin(dble(i*3) - dble(j)) * 0.4d0)
    end do
  end do
  call zgemqrt('L', 'C', 10, 10, 8, 3, AL, 10, TL, 3, CLL, 10, WORK, info)
  call begin_test('left_ctrans_block')
  call print_int('info', info)
  call print_array('c', CLL_r, 200)
  call end_test()

  ! Right, no-trans
  do j = 1, 10
    do i = 1, 10
      CLR(i, j) = dcmplx(cos(dble(i) + dble(j)) + 0.5d0, &
                         sin(dble(i*3) - dble(j)) * 0.4d0)
    end do
  end do
  call zgemqrt('R', 'N', 10, 10, 8, 3, AL, 10, TL, 3, CLR, 10, WORK, info)
  call begin_test('right_notrans_block')
  call print_int('info', info)
  call print_array('c', CLR_r, 200)
  call end_test()

  ! Right, ctrans
  do j = 1, 10
    do i = 1, 10
      CLR(i, j) = dcmplx(cos(dble(i) + dble(j)) + 0.5d0, &
                         sin(dble(i*3) - dble(j)) * 0.4d0)
    end do
  end do
  call zgemqrt('R', 'C', 10, 10, 8, 3, AL, 10, TL, 3, CLR, 10, WORK, info)
  call begin_test('right_ctrans_block')
  call print_int('info', info)
  call print_array('c', CLR_r, 200)
  call end_test()

  ! ===== Single-block case: NB == K (one inner iteration) =====
  do j = 1, 4
    do i = 1, 5
      AS(i, j) = dcmplx(sin(dble(i*2 + j*3)) + 0.05d0 * dble(i), &
                        cos(dble(i + j*2)) * 0.25d0)
    end do
  end do
  TS = (0.0d0, 0.0d0)
  call zgeqrt(5, 4, 4, AS, 5, TS, 4, WORK, info)

  call begin_test('qr_factors_single')
  call print_int('info', info)
  call print_array('v', AS_r, 40)
  call print_array('t', TS_r, 32)
  call end_test()

  ! Left, no-trans on a 5x5 dense complex matrix with NB == K
  do j = 1, 5
    do i = 1, 5
      CSS(i, j) = dcmplx(sin(dble(i + 2*j)), cos(dble(i*2 - j)) * 0.5d0)
    end do
  end do
  call zgemqrt('L', 'N', 5, 5, 4, 4, AS, 5, TS, 4, CSS, 5, WORK, info)
  call begin_test('left_notrans_single')
  call print_int('info', info)
  call print_array('c', CSS_r, 50)
  call end_test()

  ! Left, ctrans on the same 5x5 matrix
  do j = 1, 5
    do i = 1, 5
      CSS(i, j) = dcmplx(sin(dble(i + 2*j)), cos(dble(i*2 - j)) * 0.5d0)
    end do
  end do
  call zgemqrt('L', 'C', 5, 5, 4, 4, AS, 5, TS, 4, CSS, 5, WORK, info)
  call begin_test('left_ctrans_single')
  call print_int('info', info)
  call print_array('c', CSS_r, 50)
  call end_test()

end program
