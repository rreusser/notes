program test_zla_porcond_x
  use test_utils
  implicit none

  integer, parameter :: NMAX = 6
  complex*16 :: A(NMAX, NMAX), AF(NMAX, NMAX), X(NMAX), WORK(2*NMAX)
  complex*16 :: Apk(NMAX*NMAX), AFpk(NMAX*NMAX)
  double precision :: Apk_r(2*NMAX*NMAX), AFpk_r(2*NMAX*NMAX), X_r(2*NMAX)
  equivalence (Apk, Apk_r)
  equivalence (AFpk, AFpk_r)
  equivalence (X, X_r)
  double precision :: RWORK(NMAX), tmp
  integer :: INFO, n, i, j
  double precision :: ZLA_PORCOND_X
  external :: ZLA_PORCOND_X, ZPOTRF

  ! ===== Test 1: UPLO='U', 4x4 Hermitian pos-def, real X =====
  n = 4
  A = (0.0d0, 0.0d0)
  A(1,1) = (10.0d0, 0.0d0)
  A(1,2) = (1.0d0, 2.0d0)
  A(1,3) = (3.0d0, -1.0d0)
  A(1,4) = (0.5d0, 0.5d0)
  A(2,2) = (12.0d0, 0.0d0)
  A(2,3) = (2.0d0, 1.0d0)
  A(2,4) = (1.0d0, -2.0d0)
  A(3,3) = (14.0d0, 0.0d0)
  A(3,4) = (3.0d0, 0.0d0)
  A(4,4) = (16.0d0, 0.0d0)
  AF = A
  call ZPOTRF('U', n, AF, NMAX, INFO)
  X(1) = (1.0d0, 0.0d0); X(2) = (1.0d0, 0.0d0)
  X(3) = (1.0d0, 0.0d0); X(4) = (1.0d0, 0.0d0)

  tmp = ZLA_PORCOND_X('U', n, A, NMAX, AF, NMAX, X, INFO, WORK, RWORK)
  do j = 1, n
    do i = 1, n
      AFpk(i + (j-1)*n) = AF(i, j)
      Apk(i + (j-1)*n) = A(i, j)
    end do
  end do
  call begin_test('upper_4x4_real_x')
  call print_int('n', n)
  call print_array('A', Apk_r, 2*n*n)
  call print_array('A_factored', AFpk_r, 2*n*n)
  call print_array('x', X_r, 2*n)
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 2: UPLO='U', 4x4, complex X =====
  X(1) = (2.0d0, 1.0d0); X(2) = (0.5d0, -0.5d0)
  X(3) = (3.0d0, 2.0d0); X(4) = (1.0d0, -1.0d0)
  tmp = ZLA_PORCOND_X('U', n, A, NMAX, AF, NMAX, X, INFO, WORK, RWORK)
  call begin_test('upper_4x4_complex_x')
  call print_array('x', X_r, 2*n)
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 3: UPLO='L', 4x4 Hermitian pos-def (lower), real X =====
  n = 4
  A = (0.0d0, 0.0d0)
  A(1,1) = (10.0d0, 0.0d0)
  A(2,1) = (1.0d0, -2.0d0)
  A(2,2) = (12.0d0, 0.0d0)
  A(3,1) = (3.0d0, 1.0d0)
  A(3,2) = (2.0d0, -1.0d0)
  A(3,3) = (14.0d0, 0.0d0)
  A(4,1) = (0.5d0, -0.5d0)
  A(4,2) = (1.0d0, 2.0d0)
  A(4,3) = (3.0d0, 0.0d0)
  A(4,4) = (16.0d0, 0.0d0)
  AF = A
  call ZPOTRF('L', n, AF, NMAX, INFO)
  X(1) = (1.0d0, 0.0d0); X(2) = (1.0d0, 0.0d0)
  X(3) = (1.0d0, 0.0d0); X(4) = (1.0d0, 0.0d0)
  tmp = ZLA_PORCOND_X('L', n, A, NMAX, AF, NMAX, X, INFO, WORK, RWORK)
  do j = 1, n
    do i = 1, n
      AFpk(i + (j-1)*n) = AF(i, j)
      Apk(i + (j-1)*n) = A(i, j)
    end do
  end do
  call begin_test('lower_4x4_real_x')
  call print_int('n', n)
  call print_array('A', Apk_r, 2*n*n)
  call print_array('A_factored', AFpk_r, 2*n*n)
  call print_array('x', X_r, 2*n)
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 4: UPLO='L', 4x4, complex X =====
  X(1) = (2.0d0, 1.0d0); X(2) = (0.5d0, -0.5d0)
  X(3) = (3.0d0, 2.0d0); X(4) = (1.0d0, -1.0d0)
  tmp = ZLA_PORCOND_X('L', n, A, NMAX, AF, NMAX, X, INFO, WORK, RWORK)
  call begin_test('lower_4x4_complex_x')
  call print_array('x', X_r, 2*n)
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 5: N=1, UPLO='U' =====
  n = 1
  A(1,1) = (5.0d0, 0.0d0)
  AF(1,1) = (5.0d0, 0.0d0)
  call ZPOTRF('U', n, AF, NMAX, INFO)
  X(1) = (2.0d0, 1.0d0)
  tmp = ZLA_PORCOND_X('U', n, A, NMAX, AF, NMAX, X, INFO, WORK, RWORK)
  Apk(1) = A(1,1)
  AFpk(1) = AF(1,1)
  call begin_test('n1_upper')
  call print_int('n', n)
  call print_array('A', Apk_r, 2*n*n)
  call print_array('A_factored', AFpk_r, 2*n*n)
  call print_array('x', X_r, 2*n)
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

  ! ===== Test 6: N=0 =====
  n = 0
  tmp = ZLA_PORCOND_X('U', n, A, NMAX, AF, NMAX, X, INFO, WORK, RWORK)
  call begin_test('n0')
  call print_int('n', n)
  call print_scalar('result', tmp)
  call print_int('info', INFO)
  call end_test()

end program
