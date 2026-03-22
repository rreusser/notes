program test_dsteqr
  use test_utils
  implicit none

  integer, parameter :: MAXN = 6
  integer, parameter :: LDZ = MAXN
  double precision :: D(MAXN), E(MAXN), Z(LDZ, MAXN), WORK(2*MAXN)
  integer :: INFO, i, j, N

  ! ---------------------------------------------------------------
  ! Test 1: COMPZ='I', 4x4 tridiagonal matrix
  ! Eigenvalues + eigenvectors starting from identity
  ! Matrix:
  !   [ 2  1  0  0 ]
  !   [ 1  2  1  0 ]
  !   [ 0  1  2  1 ]
  !   [ 0  0  1  2 ]
  ! ---------------------------------------------------------------
  N = 4
  D(1) = 2.0d0; D(2) = 2.0d0; D(3) = 2.0d0; D(4) = 2.0d0
  E(1) = 1.0d0; E(2) = 1.0d0; E(3) = 1.0d0
  call DSTEQR('I', N, D, E, Z, LDZ, WORK, INFO)
  call begin_test('compz_I_4x4')
  call print_int('info', INFO)
  call print_array('d', D, N)
  call print_matrix('z', Z, LDZ, N, N)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 2: COMPZ='V', 4x4 with pre-initialized Z (identity)
  ! Same tridiagonal matrix
  ! ---------------------------------------------------------------
  N = 4
  D(1) = 2.0d0; D(2) = 2.0d0; D(3) = 2.0d0; D(4) = 2.0d0
  E(1) = 1.0d0; E(2) = 1.0d0; E(3) = 1.0d0
  ! Initialize Z to identity
  do j = 1, N
    do i = 1, N
      if (i .eq. j) then
        Z(i, j) = 1.0d0
      else
        Z(i, j) = 0.0d0
      end if
    end do
  end do
  call DSTEQR('V', N, D, E, Z, LDZ, WORK, INFO)
  call begin_test('compz_V_4x4')
  call print_int('info', INFO)
  call print_array('d', D, N)
  call print_matrix('z', Z, LDZ, N, N)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 3: COMPZ='N', eigenvalues only
  ! Same 4x4 tridiagonal matrix
  ! ---------------------------------------------------------------
  N = 4
  D(1) = 2.0d0; D(2) = 2.0d0; D(3) = 2.0d0; D(4) = 2.0d0
  E(1) = 1.0d0; E(2) = 1.0d0; E(3) = 1.0d0
  call DSTEQR('N', N, D, E, Z, LDZ, WORK, INFO)
  call begin_test('compz_N_4x4')
  call print_int('info', INFO)
  call print_array('d', D, N)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 4: N=1 edge case, COMPZ='I'
  ! ---------------------------------------------------------------
  N = 1
  D(1) = 5.0d0
  call DSTEQR('I', N, D, E, Z, LDZ, WORK, INFO)
  call begin_test('n1_compz_I')
  call print_int('info', INFO)
  call print_array('d', D, N)
  call print_matrix('z', Z, LDZ, N, N)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 5: N=2 edge case, COMPZ='I'
  ! ---------------------------------------------------------------
  N = 2
  D(1) = 3.0d0; D(2) = 1.0d0
  E(1) = 2.0d0
  call DSTEQR('I', N, D, E, Z, LDZ, WORK, INFO)
  call begin_test('n2_compz_I')
  call print_int('info', INFO)
  call print_array('d', D, N)
  call print_matrix('z', Z, LDZ, N, N)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 6: N=0 edge case
  ! ---------------------------------------------------------------
  N = 0
  call DSTEQR('I', N, D, E, Z, LDZ, WORK, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 7: Already-diagonal matrix, COMPZ='I'
  ! No off-diagonal elements
  ! ---------------------------------------------------------------
  N = 4
  D(1) = 4.0d0; D(2) = 1.0d0; D(3) = 3.0d0; D(4) = 2.0d0
  E(1) = 0.0d0; E(2) = 0.0d0; E(3) = 0.0d0
  call DSTEQR('I', N, D, E, Z, LDZ, WORK, INFO)
  call begin_test('diagonal_compz_I')
  call print_int('info', INFO)
  call print_array('d', D, N)
  call print_matrix('z', Z, LDZ, N, N)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 8: 6x6 matrix, COMPZ='I', bigger test
  ! ---------------------------------------------------------------
  N = 6
  D(1) = 4.0d0; D(2) = 3.0d0; D(3) = 2.0d0
  D(4) = 1.0d0; D(5) = 5.0d0; D(6) = 6.0d0
  E(1) = 1.0d0; E(2) = 0.5d0; E(3) = 0.25d0
  E(4) = 0.125d0; E(5) = 2.0d0
  call DSTEQR('I', N, D, E, Z, LDZ, WORK, INFO)
  call begin_test('n6_compz_I')
  call print_int('info', INFO)
  call print_array('d', D, N)
  call print_matrix('z', Z, LDZ, N, N)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 9: COMPZ='V', 4x4 with non-identity initial Z
  ! Z initialized to a simple rotation
  ! ---------------------------------------------------------------
  N = 4
  D(1) = 2.0d0; D(2) = 2.0d0; D(3) = 2.0d0; D(4) = 2.0d0
  E(1) = 1.0d0; E(2) = 1.0d0; E(3) = 1.0d0
  ! Initialize Z to a permutation matrix (swap cols 1 and 2)
  do j = 1, N
    do i = 1, N
      Z(i, j) = 0.0d0
    end do
  end do
  Z(1, 2) = 1.0d0
  Z(2, 1) = 1.0d0
  Z(3, 3) = 1.0d0
  Z(4, 4) = 1.0d0
  call DSTEQR('V', N, D, E, Z, LDZ, WORK, INFO)
  call begin_test('compz_V_permuted')
  call print_int('info', INFO)
  call print_array('d', D, N)
  call print_matrix('z', Z, LDZ, N, N)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 10: N=2 edge case, COMPZ='N' (eigenvalues only)
  ! ---------------------------------------------------------------
  N = 2
  D(1) = 3.0d0; D(2) = 1.0d0
  E(1) = 2.0d0
  call DSTEQR('N', N, D, E, Z, LDZ, WORK, INFO)
  call begin_test('n2_compz_N')
  call print_int('info', INFO)
  call print_array('d', D, N)
  call end_test()

end program
