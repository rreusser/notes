program test_dla_syrcond
  use test_utils
  implicit none

  integer, parameter :: NMAX = 3
  double precision :: A(NMAX, NMAX), AF(NMAX, NMAX)
  integer :: IPIV(NMAX)
  double precision :: C(NMAX), WORK(3*NMAX)
  integer :: IWORK(NMAX)
  integer :: N, INFO, CMODE, LWORK
  double precision :: DLA_SYRCOND, rcond
  external :: DLA_SYRCOND, DSYTRF

  N = 3
  LWORK = 64 * NMAX

  ! ============================================
  ! Test 1: UPLO='U', CMODE=1
  ! ============================================
  ! Symmetric indefinite matrix (upper triangle stored)
  A(1,1) = 2.0D0
  A(1,2) = -1.0D0
  A(1,3) = 0.5D0
  A(2,2) = 3.0D0
  A(2,3) = -0.5D0
  A(3,3) = 4.0D0
  ! Fill lower (symmetric)
  A(2,1) = A(1,2)
  A(3,1) = A(1,3)
  A(3,2) = A(2,3)

  C(1) = 1.0D0
  C(2) = 2.0D0
  C(3) = 0.5D0

  ! Factor
  AF = A
  call DSYTRF('U', N, AF, NMAX, IPIV, WORK, LWORK, INFO)
  if (INFO .ne. 0) then
    print *, 'DSYTRF failed with INFO=', INFO
    stop 1
  end if

  CMODE = 1
  rcond = DLA_SYRCOND('U', N, A, NMAX, AF, NMAX, IPIV, CMODE, C, &
                        INFO, WORK, IWORK)

  call begin_test('uplo_U_cmode1')
  call print_scalar('rcond', rcond)
  call print_int('info', INFO)
  call end_test()

  ! ============================================
  ! Test 2: UPLO='U', CMODE=0
  ! ============================================
  CMODE = 0
  rcond = DLA_SYRCOND('U', N, A, NMAX, AF, NMAX, IPIV, CMODE, C, &
                        INFO, WORK, IWORK)

  call begin_test('uplo_U_cmode0')
  call print_scalar('rcond', rcond)
  call print_int('info', INFO)
  call end_test()

  ! ============================================
  ! Test 3: UPLO='U', CMODE=-1
  ! ============================================
  CMODE = -1
  rcond = DLA_SYRCOND('U', N, A, NMAX, AF, NMAX, IPIV, CMODE, C, &
                        INFO, WORK, IWORK)

  call begin_test('uplo_U_cmode_neg1')
  call print_scalar('rcond', rcond)
  call print_int('info', INFO)
  call end_test()

  ! ============================================
  ! Test 4: UPLO='L', CMODE=1
  ! ============================================
  ! Factor with lower
  AF = A
  call DSYTRF('L', N, AF, NMAX, IPIV, WORK, LWORK, INFO)
  if (INFO .ne. 0) then
    print *, 'DSYTRF L failed with INFO=', INFO
    stop 1
  end if

  CMODE = 1
  rcond = DLA_SYRCOND('L', N, A, NMAX, AF, NMAX, IPIV, CMODE, C, &
                        INFO, WORK, IWORK)

  call begin_test('uplo_L_cmode1')
  call print_scalar('rcond', rcond)
  call print_int('info', INFO)
  call end_test()

  ! ============================================
  ! Test 5: UPLO='L', CMODE=0
  ! ============================================
  CMODE = 0
  rcond = DLA_SYRCOND('L', N, A, NMAX, AF, NMAX, IPIV, CMODE, C, &
                        INFO, WORK, IWORK)

  call begin_test('uplo_L_cmode0')
  call print_scalar('rcond', rcond)
  call print_int('info', INFO)
  call end_test()

  ! ============================================
  ! Test 6: UPLO='L', CMODE=-1
  ! ============================================
  CMODE = -1
  rcond = DLA_SYRCOND('L', N, A, NMAX, AF, NMAX, IPIV, CMODE, C, &
                        INFO, WORK, IWORK)

  call begin_test('uplo_L_cmode_neg1')
  call print_scalar('rcond', rcond)
  call print_int('info', INFO)
  call end_test()

  ! ============================================
  ! Test 7: UPLO='U', different C vector
  ! ============================================
  C(1) = 3.0D0
  C(2) = 0.1D0
  C(3) = 2.0D0

  ! Re-factor with upper
  AF = A
  call DSYTRF('U', N, AF, NMAX, IPIV, WORK, LWORK, INFO)

  CMODE = 1
  rcond = DLA_SYRCOND('U', N, A, NMAX, AF, NMAX, IPIV, CMODE, C, &
                        INFO, WORK, IWORK)

  call begin_test('uplo_U_diff_C')
  call print_scalar('rcond', rcond)
  call print_int('info', INFO)
  call end_test()

end program test_dla_syrcond
