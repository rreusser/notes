program test_dlaed2
  use test_utils
  implicit none

  integer, parameter :: NMAX = 8
  integer :: K, N, N1, LDQ, INFO, I, J
  double precision :: RHO
  double precision :: D(NMAX), Q(NMAX,NMAX), Z(NMAX)
  double precision :: DLAMBDA(NMAX), W(NMAX), Q2(NMAX*NMAX)
  integer :: INDXQ(NMAX), INDX(NMAX), INDXC(NMAX)
  integer :: INDXP(NMAX), COLTYP(NMAX)

  ! ---- Test 1: Basic merge, N=6, N1=3 ----
  N = 6
  N1 = 3
  LDQ = NMAX

  ! Eigenvalues of two subproblems (pre-sorted within each half)
  D(1) = 1.0D0
  D(2) = 3.0D0
  D(3) = 5.0D0
  D(4) = 2.0D0
  D(5) = 4.0D0
  D(6) = 6.0D0

  ! Q = identity (eigenvectors of subproblems)
  do J = 1, N
    do I = 1, N
      Q(I, J) = 0.0D0
    end do
    Q(J, J) = 1.0D0
  end do

  ! INDXQ: permutation that sorts each half (1-based, identity for pre-sorted)
  INDXQ(1) = 1
  INDXQ(2) = 2
  INDXQ(3) = 3
  INDXQ(4) = 1
  INDXQ(5) = 2
  INDXQ(6) = 3

  ! RHO: the off-diagonal element from the rank-1 cut
  RHO = 1.0D0

  ! Z: updating vector (last row of first eigenvector matrix,
  !    first row of second eigenvector matrix)
  Z(1) = 0.5D0
  Z(2) = 0.6D0
  Z(3) = 0.7D0
  Z(4) = 0.4D0
  Z(5) = 0.3D0
  Z(6) = 0.2D0

  call DLAED2(K, N, N1, D, Q, LDQ, INDXQ, RHO, Z, DLAMBDA, W,       &
              Q2, INDX, INDXC, INDXP, COLTYP, INFO)

  call begin_test('basic_n6')
  call print_int('N', N)
  call print_int('N1', N1)
  call print_int('K', K)
  call print_int('INFO', INFO)
  call print_scalar('RHO', RHO)
  call print_array('D', D, N)
  call print_array('Z', Z, N)
  call print_array('DLAMBDA', DLAMBDA, N)
  call print_array('W', W, N)
  call print_array('Q2', Q2, N*N)
  call print_matrix('Q', Q, LDQ, N, N)
  call print_int_array('INDXQ', INDXQ, N)
  call print_int_array('INDX', INDX, N)
  call print_int_array('INDXC', INDXC, N)
  call print_int_array('INDXP', INDXP, N)
  call print_int_array('COLTYP', COLTYP, 4)
  call end_test()

  ! ---- Test 2: N=0, quick return ----
  N = 0
  N1 = 0
  LDQ = 1
  RHO = 1.0D0
  INFO = -999

  call DLAED2(K, N, N1, D, Q, LDQ, INDXQ, RHO, Z, DLAMBDA, W,       &
              Q2, INDX, INDXC, INDXP, COLTYP, INFO)

  call begin_test('n0')
  call print_int('N', N)
  call print_int('INFO', INFO)
  call end_test()

  ! ---- Test 3: N=4, N1=2, negative RHO ----
  N = 4
  N1 = 2
  LDQ = NMAX

  D(1) = 1.0D0
  D(2) = 4.0D0
  D(3) = 2.0D0
  D(4) = 5.0D0

  do J = 1, N
    do I = 1, N
      Q(I, J) = 0.0D0
    end do
    Q(J, J) = 1.0D0
  end do

  INDXQ(1) = 1
  INDXQ(2) = 2
  INDXQ(3) = 1
  INDXQ(4) = 2

  RHO = -2.0D0

  Z(1) = 0.3D0
  Z(2) = 0.5D0
  Z(3) = 0.4D0
  Z(4) = 0.6D0

  call DLAED2(K, N, N1, D, Q, LDQ, INDXQ, RHO, Z, DLAMBDA, W,       &
              Q2, INDX, INDXC, INDXP, COLTYP, INFO)

  call begin_test('neg_rho')
  call print_int('N', N)
  call print_int('N1', N1)
  call print_int('K', K)
  call print_int('INFO', INFO)
  call print_scalar('RHO', RHO)
  call print_array('D', D, N)
  call print_array('Z', Z, N)
  call print_array('DLAMBDA', DLAMBDA, N)
  call print_array('W', W, N)
  call print_array('Q2', Q2, N1*N1+(N-N1)*(N-N1))
  call print_matrix('Q', Q, LDQ, N, N)
  call print_int_array('INDX', INDX, N)
  call print_int_array('INDXC', INDXC, N)
  call print_int_array('INDXP', INDXP, N)
  call print_int_array('COLTYP', COLTYP, 4)
  call end_test()

  ! ---- Test 4: Deflation via small z component ----
  N = 4
  N1 = 2
  LDQ = NMAX

  D(1) = 1.0D0
  D(2) = 3.0D0
  D(3) = 2.0D0
  D(4) = 5.0D0

  do J = 1, N
    do I = 1, N
      Q(I, J) = 0.0D0
    end do
    Q(J, J) = 1.0D0
  end do

  INDXQ(1) = 1
  INDXQ(2) = 2
  INDXQ(3) = 1
  INDXQ(4) = 2

  RHO = 1.0D0

  ! Make Z(2) tiny so eigenvalue 2 gets deflated
  Z(1) = 0.5D0
  Z(2) = 1.0D-20
  Z(3) = 0.4D0
  Z(4) = 0.3D0

  call DLAED2(K, N, N1, D, Q, LDQ, INDXQ, RHO, Z, DLAMBDA, W,       &
              Q2, INDX, INDXC, INDXP, COLTYP, INFO)

  call begin_test('small_z')
  call print_int('N', N)
  call print_int('N1', N1)
  call print_int('K', K)
  call print_int('INFO', INFO)
  call print_scalar('RHO', RHO)
  call print_array('D', D, N)
  call print_array('Z', Z, N)
  call print_array('DLAMBDA', DLAMBDA, N)
  call print_array('W', W, N)
  call print_int_array('INDX', INDX, N)
  call print_int_array('INDXC', INDXC, N)
  call print_int_array('INDXP', INDXP, N)
  call print_int_array('COLTYP', COLTYP, 4)
  call end_test()

  ! ---- Test 5: Deflation via close eigenvalues ----
  N = 4
  N1 = 2
  LDQ = NMAX

  ! Two eigenvalues very close
  D(1) = 1.0D0
  D(2) = 3.0D0
  D(3) = 1.0D0 + 1.0D-16
  D(4) = 5.0D0

  do J = 1, N
    do I = 1, N
      Q(I, J) = 0.0D0
    end do
    Q(J, J) = 1.0D0
  end do

  INDXQ(1) = 1
  INDXQ(2) = 2
  INDXQ(3) = 1
  INDXQ(4) = 2

  RHO = 1.0D0

  Z(1) = 0.5D0
  Z(2) = 0.6D0
  Z(3) = 0.4D0
  Z(4) = 0.3D0

  call DLAED2(K, N, N1, D, Q, LDQ, INDXQ, RHO, Z, DLAMBDA, W,       &
              Q2, INDX, INDXC, INDXP, COLTYP, INFO)

  call begin_test('close_eigenvalues')
  call print_int('N', N)
  call print_int('N1', N1)
  call print_int('K', K)
  call print_int('INFO', INFO)
  call print_scalar('RHO', RHO)
  call print_array('D', D, N)
  call print_array('Z', Z, N)
  call print_array('DLAMBDA', DLAMBDA, N)
  call print_array('W', W, N)
  call print_int_array('INDX', INDX, N)
  call print_int_array('INDXC', INDXC, N)
  call print_int_array('INDXP', INDXP, N)
  call print_int_array('COLTYP', COLTYP, 4)
  call end_test()

  ! ---- Test 6: All deflated (RHO*Z tiny) ----
  N = 4
  N1 = 2
  LDQ = NMAX

  D(1) = 1.0D0
  D(2) = 3.0D0
  D(3) = 2.0D0
  D(4) = 5.0D0

  do J = 1, N
    do I = 1, N
      Q(I, J) = 0.0D0
    end do
    Q(J, J) = 1.0D0
  end do

  INDXQ(1) = 1
  INDXQ(2) = 2
  INDXQ(3) = 1
  INDXQ(4) = 2

  RHO = 1.0D0

  ! All z components tiny
  Z(1) = 1.0D-20
  Z(2) = 1.0D-20
  Z(3) = 1.0D-20
  Z(4) = 1.0D-20

  call DLAED2(K, N, N1, D, Q, LDQ, INDXQ, RHO, Z, DLAMBDA, W,       &
              Q2, INDX, INDXC, INDXP, COLTYP, INFO)

  call begin_test('all_deflated')
  call print_int('N', N)
  call print_int('N1', N1)
  call print_int('K', K)
  call print_int('INFO', INFO)
  call print_scalar('RHO', RHO)
  call print_array('D', D, N)
  call print_array('DLAMBDA', DLAMBDA, N)
  call print_matrix('Q', Q, LDQ, N, N)
  call end_test()

  ! ---- Test 7: N=2, N1=1, minimal case ----
  N = 2
  N1 = 1
  LDQ = NMAX

  D(1) = 3.0D0
  D(2) = 1.0D0

  do J = 1, N
    do I = 1, N
      Q(I, J) = 0.0D0
    end do
    Q(J, J) = 1.0D0
  end do

  INDXQ(1) = 1
  INDXQ(2) = 1

  RHO = 0.5D0

  Z(1) = 0.7D0
  Z(2) = 0.7D0

  call DLAED2(K, N, N1, D, Q, LDQ, INDXQ, RHO, Z, DLAMBDA, W,       &
              Q2, INDX, INDXC, INDXP, COLTYP, INFO)

  call begin_test('n2')
  call print_int('N', N)
  call print_int('N1', N1)
  call print_int('K', K)
  call print_int('INFO', INFO)
  call print_scalar('RHO', RHO)
  call print_array('D', D, N)
  call print_array('Z', Z, N)
  call print_array('DLAMBDA', DLAMBDA, N)
  call print_array('W', W, N)
  call print_array('Q2', Q2, N1*N1+(N-N1)*(N-N1))
  call print_matrix('Q', Q, LDQ, N, N)
  call print_int_array('INDX', INDX, N)
  call print_int_array('INDXC', INDXC, N)
  call print_int_array('INDXP', INDXP, N)
  call print_int_array('COLTYP', COLTYP, 4)
  call end_test()

  ! ---- Test 8: N=8, larger case with mixed deflation ----
  N = 8
  N1 = 4
  LDQ = NMAX

  D(1) = 1.0D0
  D(2) = 3.0D0
  D(3) = 5.5D0
  D(4) = 7.0D0
  D(5) = 2.0D0
  D(6) = 4.0D0
  D(7) = 5.5D0 + 1.0D-16
  D(8) = 9.0D0

  do J = 1, N
    do I = 1, N
      Q(I, J) = 0.0D0
    end do
    Q(J, J) = 1.0D0
  end do

  INDXQ(1) = 1
  INDXQ(2) = 2
  INDXQ(3) = 3
  INDXQ(4) = 4
  INDXQ(5) = 1
  INDXQ(6) = 2
  INDXQ(7) = 3
  INDXQ(8) = 4

  RHO = 1.5D0

  Z(1) = 0.3D0
  Z(2) = 0.4D0
  Z(3) = 0.5D0
  Z(4) = 0.2D0
  Z(5) = 0.35D0
  Z(6) = 0.45D0
  Z(7) = 0.25D0
  Z(8) = 0.15D0

  call DLAED2(K, N, N1, D, Q, LDQ, INDXQ, RHO, Z, DLAMBDA, W,       &
              Q2, INDX, INDXC, INDXP, COLTYP, INFO)

  call begin_test('n8_mixed')
  call print_int('N', N)
  call print_int('N1', N1)
  call print_int('K', K)
  call print_int('INFO', INFO)
  call print_scalar('RHO', RHO)
  call print_array('D', D, N)
  call print_array('Z', Z, N)
  call print_array('DLAMBDA', DLAMBDA, N)
  call print_array('W', W, N)
  call print_int_array('INDX', INDX, N)
  call print_int_array('INDXC', INDXC, N)
  call print_int_array('INDXP', INDXP, N)
  call print_int_array('COLTYP', COLTYP, 4)
  call end_test()

end program
