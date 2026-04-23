program test_zunmr3
  use test_utils
  implicit none

  integer, parameter :: MAXWK = 400
  complex*16 :: A(5, 5), ASAVE(5, 5), TAU(5), TAUSAVE(5), WORK(MAXWK)
  complex*16 :: C(5, 5)
  ! Packed-output scratch buffers for fixture printing
  complex*16 :: Apk(15), TAUpk(5), Cpk(25)
  double precision :: Apk_r(30), TAUpk_r(10), Cpk_r(50)
  equivalence (Apk, Apk_r)
  equivalence (TAUpk, TAUpk_r)
  equivalence (Cpk, Cpk_r)
  integer :: info, i, j, M, N, K, L, LDA, LDC

  ! Build a complex 3x5 matrix A; ztzrzf requires M <= N.
  A = dcmplx(0.0d0, 0.0d0)
  A(1,1) = dcmplx( 1.0d0, 0.5d0)
  A(1,2) = dcmplx( 2.0d0,-0.2d0)
  A(1,3) = dcmplx( 3.0d0, 1.0d0)
  A(1,4) = dcmplx( 4.0d0, 0.3d0)
  A(1,5) = dcmplx( 5.0d0,-0.6d0)
  A(2,1) = dcmplx( 6.0d0, 0.1d0)
  A(2,2) = dcmplx( 7.0d0,-0.4d0)
  A(2,3) = dcmplx( 8.0d0, 0.9d0)
  A(2,4) = dcmplx( 9.0d0,-0.2d0)
  A(2,5) = dcmplx(10.0d0, 0.5d0)
  A(3,1) = dcmplx(11.0d0,-0.3d0)
  A(3,2) = dcmplx(12.0d0, 0.7d0)
  A(3,3) = dcmplx(13.0d0,-0.8d0)
  A(3,4) = dcmplx(14.0d0, 0.4d0)
  A(3,5) = dcmplx(15.0d0,-0.1d0)

  LDA = 5
  K = 3
  L = 2  ! N - M
  TAU = dcmplx(0.0d0, 0.0d0)
  call ztzrzf(3, 5, A, LDA, TAU, WORK, MAXWK, info)

  ASAVE = A
  TAUSAVE = TAU

  ! Pack the 3-by-5 sub-array of A column-major (K=3, 5 columns), and TAU length K
  do j = 1, 5
    do i = 1, K
      Apk((j-1)*K + i) = A(i, j)
    end do
  end do
  do i = 1, K
    TAUpk(i) = TAU(i)
  end do
  call begin_test('rz_factor')
  call print_int('info', info)
  call print_array('A', Apk_r, 2*K*5)
  call print_array('TAU', TAUpk_r, 2*K)
  call end_test()

  LDC = 5
  M = 5
  N = 5

  ! Test 1: Left, no-transpose, C = Q * I_5
  C = dcmplx(0.0d0, 0.0d0)
  do i = 1, M
    C(i,i) = dcmplx(1.0d0, 0.0d0)
  end do
  A = ASAVE; TAU = TAUSAVE
  call zunmr3('L', 'N', M, N, K, L, A, LDA, TAU, C, LDC, WORK, info)
  do j = 1, N
    do i = 1, M
      Cpk((j-1)*M + i) = C(i, j)
    end do
  end do
  call begin_test('left_notrans')
  call print_int('info', info)
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! Test 2: Left, conjugate-transpose, C = Q^H * I_5
  C = dcmplx(0.0d0, 0.0d0)
  do i = 1, M
    C(i,i) = dcmplx(1.0d0, 0.0d0)
  end do
  A = ASAVE; TAU = TAUSAVE
  call zunmr3('L', 'C', M, N, K, L, A, LDA, TAU, C, LDC, WORK, info)
  do j = 1, N
    do i = 1, M
      Cpk((j-1)*M + i) = C(i, j)
    end do
  end do
  call begin_test('left_conjtrans')
  call print_int('info', info)
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! Test 3: Right, no-transpose, C = I_5 * Q
  C = dcmplx(0.0d0, 0.0d0)
  do i = 1, M
    C(i,i) = dcmplx(1.0d0, 0.0d0)
  end do
  A = ASAVE; TAU = TAUSAVE
  call zunmr3('R', 'N', M, N, K, L, A, LDA, TAU, C, LDC, WORK, info)
  do j = 1, N
    do i = 1, M
      Cpk((j-1)*M + i) = C(i, j)
    end do
  end do
  call begin_test('right_notrans')
  call print_int('info', info)
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! Test 4: Right, conjugate-transpose, C = I_5 * Q^H
  C = dcmplx(0.0d0, 0.0d0)
  do i = 1, M
    C(i,i) = dcmplx(1.0d0, 0.0d0)
  end do
  A = ASAVE; TAU = TAUSAVE
  call zunmr3('R', 'C', M, N, K, L, A, LDA, TAU, C, LDC, WORK, info)
  do j = 1, N
    do i = 1, M
      Cpk((j-1)*M + i) = C(i, j)
    end do
  end do
  call begin_test('right_conjtrans')
  call print_int('info', info)
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! Test 5: Left, no-transpose with non-identity 5x3 C
  M = 5; N = 3
  do j = 1, N
    do i = 1, M
      C(i,j) = dcmplx(dble(i) + 0.5d0*dble(j) - 1.0d0, 0.25d0*dble(i) - 0.1d0*dble(j))
    end do
  end do
  A = ASAVE; TAU = TAUSAVE
  call zunmr3('L', 'N', M, N, K, L, A, LDA, TAU, C, LDC, WORK, info)
  do j = 1, N
    do i = 1, M
      Cpk((j-1)*M + i) = C(i, j)
    end do
  end do
  call begin_test('left_notrans_rect')
  call print_int('info', info)
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! Test 6: Right, conj-trans with non-identity 3x5 C
  M = 3; N = 5
  do j = 1, N
    do i = 1, M
      C(i,j) = dcmplx(dble(j) - 0.25d0*dble(i) + 1.0d0, -0.2d0*dble(j) + 0.15d0*dble(i))
    end do
  end do
  A = ASAVE; TAU = TAUSAVE
  call zunmr3('R', 'C', M, N, K, L, A, LDA, TAU, C, LDC, WORK, info)
  do j = 1, N
    do i = 1, M
      Cpk((j-1)*M + i) = C(i, j)
    end do
  end do
  call begin_test('right_conjtrans_rect')
  call print_int('info', info)
  call print_array('C', Cpk_r, 2*M*N)
  call end_test()

  ! Test 7: M=0 quick return
  call zunmr3('L', 'N', 0, 5, 0, 0, A, LDA, TAU, C, 1, WORK, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! Test 8: N=0 quick return
  call zunmr3('L', 'N', 5, 0, 0, 0, A, LDA, TAU, C, LDC, WORK, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 9: K=0 quick return
  call zunmr3('L', 'N', 5, 5, 0, 0, A, LDA, TAU, C, LDC, WORK, info)
  call begin_test('k_zero')
  call print_int('info', info)
  call end_test()

end program
