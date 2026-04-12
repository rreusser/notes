program test_dla_porcond
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4
  double precision :: A(NMAX,NMAX), AF(NMAX,NMAX)
  double precision :: C(NMAX), WORK(3*NMAX)
  integer :: IWORK(NMAX), INFO, N
  double precision :: DLA_PORCOND, result
  external :: DLA_PORCOND, DPOTRF

  ! -------------------------------------------------------
  ! Test 1: UPLO='U', CMODE=1, 3x3 SPD matrix
  ! -------------------------------------------------------
  N = 3
  ! Diagonally dominant SPD matrix (column-major)
  A(1,1) = 4.0D0; A(1,2) = 1.0D0; A(1,3) = 0.5D0
  A(2,1) = 1.0D0; A(2,2) = 5.0D0; A(2,3) = 1.0D0
  A(3,1) = 0.5D0; A(3,2) = 1.0D0; A(3,3) = 6.0D0

  ! Copy A to AF and factor
  AF = A
  call DPOTRF('U', N, AF, NMAX, INFO)

  C(1) = 2.0D0; C(2) = 1.0D0; C(3) = 0.5D0

  result = DLA_PORCOND('U', N, A, NMAX, AF, NMAX, 1, C, INFO, &
    WORK, IWORK)
  call begin_test('uplo_U_cmode_1')
  call print_scalar('result', result)
  call print_int('info', INFO)
  call end_test()

  ! -------------------------------------------------------
  ! Test 2: UPLO='L', CMODE=1, same matrix
  ! -------------------------------------------------------
  AF = A
  call DPOTRF('L', N, AF, NMAX, INFO)

  result = DLA_PORCOND('L', N, A, NMAX, AF, NMAX, 1, C, INFO, &
    WORK, IWORK)
  call begin_test('uplo_L_cmode_1')
  call print_scalar('result', result)
  call print_int('info', INFO)
  call end_test()

  ! -------------------------------------------------------
  ! Test 3: UPLO='U', CMODE=0 (no scaling)
  ! -------------------------------------------------------
  AF = A
  call DPOTRF('U', N, AF, NMAX, INFO)

  result = DLA_PORCOND('U', N, A, NMAX, AF, NMAX, 0, C, INFO, &
    WORK, IWORK)
  call begin_test('uplo_U_cmode_0')
  call print_scalar('result', result)
  call print_int('info', INFO)
  call end_test()

  ! -------------------------------------------------------
  ! Test 4: UPLO='L', CMODE=-1 (divide by C)
  ! -------------------------------------------------------
  AF = A
  call DPOTRF('L', N, AF, NMAX, INFO)

  result = DLA_PORCOND('L', N, A, NMAX, AF, NMAX, -1, C, INFO, &
    WORK, IWORK)
  call begin_test('uplo_L_cmode_neg1')
  call print_scalar('result', result)
  call print_int('info', INFO)
  call end_test()

  ! -------------------------------------------------------
  ! Test 5: N=1 edge case
  ! -------------------------------------------------------
  N = 1
  A(1,1) = 9.0D0
  AF(1,1) = 3.0D0  ! sqrt(9)
  C(1) = 2.0D0

  result = DLA_PORCOND('U', N, A, NMAX, AF, NMAX, 1, C, INFO, &
    WORK, IWORK)
  call begin_test('n_eq_1')
  call print_scalar('result', result)
  call print_int('info', INFO)
  call end_test()

  ! -------------------------------------------------------
  ! Test 6: N=0 edge case
  ! -------------------------------------------------------
  N = 0
  result = DLA_PORCOND('U', N, A, NMAX, AF, NMAX, 1, C, INFO, &
    WORK, IWORK)
  call begin_test('n_eq_0')
  call print_scalar('result', result)
  call print_int('info', INFO)
  call end_test()

  ! -------------------------------------------------------
  ! Test 7: (skipped debug tests)

  ! -------------------------------------------------------
  ! Test 8: UPLO='U', CMODE=-1
  ! -------------------------------------------------------
  N = 3
  A(1,1) = 4.0D0; A(1,2) = 1.0D0; A(1,3) = 0.5D0
  A(2,1) = 1.0D0; A(2,2) = 5.0D0; A(2,3) = 1.0D0
  A(3,1) = 0.5D0; A(3,2) = 1.0D0; A(3,3) = 6.0D0
  AF = A
  call DPOTRF('U', N, AF, NMAX, INFO)

  C(1) = 2.0D0; C(2) = 1.0D0; C(3) = 0.5D0

  result = DLA_PORCOND('U', N, A, NMAX, AF, NMAX, -1, C, INFO, &
    WORK, IWORK)
  call begin_test('uplo_U_cmode_neg1')
  call print_scalar('result', result)
  call print_int('info', INFO)
  call end_test()

  ! -------------------------------------------------------
  ! Test 8: UPLO='L', CMODE=0
  ! -------------------------------------------------------
  AF = A
  call DPOTRF('L', N, AF, NMAX, INFO)

  result = DLA_PORCOND('L', N, A, NMAX, AF, NMAX, 0, C, INFO, &
    WORK, IWORK)
  call begin_test('uplo_L_cmode_0')
  call print_scalar('result', result)
  call print_int('info', INFO)
  call end_test()

end program
