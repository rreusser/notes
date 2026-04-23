program test_dstevr
  use test_utils
  implicit none

  integer, parameter :: NMAX = 5
  integer :: N, LDZ, LWORK, LIWORK, IL, IU, M, INFO, i
  double precision :: D(NMAX), E(NMAX), W(NMAX), Z(NMAX, NMAX), WORK(512)
  integer :: IWORK(512), ISUPPZ(2*NMAX)
  double precision :: VL, VU, ABSTOL
  double precision :: D_save(NMAX), E_save(NMAX)

  LWORK = 512
  LIWORK = 512
  ABSTOL = 0.0d0

  ! =====================================================
  ! Test 1: JOBZ='V', RANGE='A', 5x5 tridiagonal
  ! Diagonally dominant
  ! =====================================================
  N = 5
  LDZ = NMAX

  D(1) = 4.0d0;  D(2) = 6.0d0;  D(3) = 8.0d0;  D(4) = 10.0d0; D(5) = 12.0d0
  E(1) = 1.0d0;  E(2) = 0.5d0;  E(3) = 1.0d0;  E(4) = 0.5d0

  D_save(1:N) = D(1:N)
  E_save(1:N-1) = E(1:N-1)

  call DSTEVR('V', 'A', N, D, E, VL, VU, IL, IU, ABSTOL, &
              M, W, Z, LDZ, ISUPPZ, WORK, LWORK, IWORK, LIWORK, INFO)
  call begin_test('V_A')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('D_in', D_save, N)
  call print_array('E_in', E_save, N-1)
  call print_array('w', W, M)
  call print_matrix('Z', Z, LDZ, N, M)
  call end_test()

  ! =====================================================
  ! Test 2: JOBZ='N', RANGE='A'
  ! Eigenvalues only
  ! =====================================================
  N = 5

  D(1) = 4.0d0;  D(2) = 6.0d0;  D(3) = 8.0d0;  D(4) = 10.0d0; D(5) = 12.0d0
  E(1) = 1.0d0;  E(2) = 0.5d0;  E(3) = 1.0d0;  E(4) = 0.5d0

  call DSTEVR('N', 'A', N, D, E, VL, VU, IL, IU, ABSTOL, &
              M, W, Z, LDZ, ISUPPZ, WORK, LWORK, IWORK, LIWORK, INFO)
  call begin_test('N_A')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  call end_test()

  ! =====================================================
  ! Test 3: JOBZ='V', RANGE='V'
  ! Value range: eigenvalues in (5, 9)
  ! =====================================================
  N = 5

  D(1) = 4.0d0;  D(2) = 6.0d0;  D(3) = 8.0d0;  D(4) = 10.0d0; D(5) = 12.0d0
  E(1) = 1.0d0;  E(2) = 0.5d0;  E(3) = 1.0d0;  E(4) = 0.5d0

  VL = 5.0d0
  VU = 9.0d0
  call DSTEVR('V', 'V', N, D, E, VL, VU, IL, IU, ABSTOL, &
              M, W, Z, LDZ, ISUPPZ, WORK, LWORK, IWORK, LIWORK, INFO)
  call begin_test('V_V')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  if (M > 0) then
    call print_matrix('Z', Z, LDZ, N, M)
  end if
  call end_test()

  ! =====================================================
  ! Test 4: JOBZ='V', RANGE='I'
  ! Index range: eigenvalues 2 through 4
  ! =====================================================
  N = 5

  D(1) = 4.0d0;  D(2) = 6.0d0;  D(3) = 8.0d0;  D(4) = 10.0d0; D(5) = 12.0d0
  E(1) = 1.0d0;  E(2) = 0.5d0;  E(3) = 1.0d0;  E(4) = 0.5d0

  IL = 2
  IU = 4
  call DSTEVR('V', 'I', N, D, E, VL, VU, IL, IU, ABSTOL, &
              M, W, Z, LDZ, ISUPPZ, WORK, LWORK, IWORK, LIWORK, INFO)
  call begin_test('V_I')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  call print_matrix('Z', Z, LDZ, N, M)
  call end_test()

  ! =====================================================
  ! Test 5: N=0
  ! =====================================================
  N = 0
  call DSTEVR('V', 'A', N, D, E, VL, VU, IL, IU, ABSTOL, &
              M, W, Z, NMAX, ISUPPZ, WORK, LWORK, IWORK, LIWORK, INFO)
  call begin_test('N0')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! =====================================================
  ! Test 6: N=1
  ! =====================================================
  N = 1
  D(1) = 7.0d0
  call DSTEVR('V', 'A', N, D, E, VL, VU, IL, IU, ABSTOL, &
              M, W, Z, NMAX, ISUPPZ, WORK, LWORK, IWORK, LIWORK, INFO)
  call begin_test('N1')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  call print_matrix('Z', Z, NMAX, N, M)
  call end_test()

  ! =====================================================
  ! Test 7: JOBZ='N', RANGE='V'
  ! Eigenvalues only, value range
  ! =====================================================
  N = 5

  D(1) = 4.0d0;  D(2) = 6.0d0;  D(3) = 8.0d0;  D(4) = 10.0d0; D(5) = 12.0d0
  E(1) = 1.0d0;  E(2) = 0.5d0;  E(3) = 1.0d0;  E(4) = 0.5d0

  VL = 5.0d0
  VU = 9.0d0
  call DSTEVR('N', 'V', N, D, E, VL, VU, IL, IU, ABSTOL, &
              M, W, Z, LDZ, ISUPPZ, WORK, LWORK, IWORK, LIWORK, INFO)
  call begin_test('N_V')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  call end_test()

  ! =====================================================
  ! Test 8: JOBZ='N', RANGE='I'
  ! Eigenvalues only, index range
  ! =====================================================
  N = 5

  D(1) = 4.0d0;  D(2) = 6.0d0;  D(3) = 8.0d0;  D(4) = 10.0d0; D(5) = 12.0d0
  E(1) = 1.0d0;  E(2) = 0.5d0;  E(3) = 1.0d0;  E(4) = 0.5d0

  IL = 1
  IU = 3
  call DSTEVR('N', 'I', N, D, E, VL, VU, IL, IU, ABSTOL, &
              M, W, Z, LDZ, ISUPPZ, WORK, LWORK, IWORK, LIWORK, INFO)
  call begin_test('N_I')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  call end_test()

end program
